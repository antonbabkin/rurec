# Warning: RAS segments of the code are outdated and left only for reference and potential future use.


# imports ----
library(units)
library(glue)
library(logger)
library(tidyverse)
library(lpSolve)

source("R/basic_utilities.R", local = (util <- new.env()))
source("R/geography.R", local = (geography <- new.env()))
source("R/place_io.R", local = (place_io <- new.env()))

# config ----

# set TRUE or FALSE to enable or disable caching of function outputs
use_cache <- TRUE

# set `debug_env` to not-NULL before calling a function to save it's internal variables for inspection
debug_env <- NULL

read_cache <- function(path) {
  if (use_cache && file.exists(path)) {
    x <- readRDS(path)
    log_debug("read from cache ", path)
  } else {
    x <- FALSE
  }
  x
}

write_cache <- function(x, path) {
  if (use_cache) {
    saveRDS(x, util$mkdir(path))
    log_debug("saved to cache ", path)
  }
}


debug_env_save <- function() {
  if (!is.null(debug_env)) {
    debug_env <<- rlang::caller_env()
  }
}

# data paths ----

ipath <- list(
)

opath <- list(
  old_sup = "data/trade_flows/demsup/demand_commodity_det_2012_infogroup.rds",
  old_dem = "data/trade_flows/demsup/supply_commodity_det_2012_infogroup.rds",
  old_flows_ = "data/trade_flows/flows_commodity_det_2012_infogroup/{ind_code}.rds",
  flows_ = "data/trade_flows/{bus_data}_{ilevel}_{year}/{ind_code}.rds",
  flows_all_ = "data/trade_flows/{bus_data}_{ilevel}_{year}.pq"
)


# helper functions ----



#' Distance matrix between county centroids in miles
distance_matrix <- function(year) {
  dmat <- geography$call_dist_mat(year = util$year2tiger(year), from = "center", cbsa = FALSE)
  rn <- rownames(dmat)
  cn <- colnames(dmat)
  stopifnot(all.equal(rn, cn))
  dmat <- dmat |>
    set_units(mi) |> # this operation destroys dim names
    drop_units()
  rownames(dmat) <- rn
  colnames(dmat) <- cn
  dmat
}


#' Return available industry codes
call_industry_codes <- function() {
  glue(opath$demand_, .envir = params_econ) |>
    readRDS() |>
    rownames()
}

#' Vector of commodity codes with positive aggregate excess supply and excess demand
call_traded_commodities <- function(year,
                               ilevel = c("det", "sum", "sec"),
                               bus_data = c("cbp_imp", "cbp_raw", "infogroup")) {
  place_io$outsupdem(year, ilevel, bus_data) %>%
    mutate(exsup = pmax(supply - demand, 0),
           exdem = pmax(demand - supply, 0)) %>%
    summarize(sup = sum(exsup), dem = sum(exsup), .by = com_code) %>%
    filter(sup > 0, dem > 0) %>%
    pull(com_code)
}

#' Combine estimated flows into a single dataframe
build_combined_df <- function(year, ilevel = c("det", "sum", "sec"), bus_data = c("cbp_imp", "cbp_raw", "infogroup")) {
  com_codes <- call_traded_commodities(year, ilevel, bus_data)
  df <- list()
  for (com in com_codes) {
    df[[com]] <- glue(opath$flows_, ind_code = com) %>%
      read_cache() %>%
      as_tibble(rownames = "from") %>%
      pivot_longer(!from, names_to = "to", values_to = "flow") %>%
      filter(flow > 0)
  }
  df <- bind_rows(df, .id = "com_code")
  save_to <- glue(opath$flows_all_)
  arrow::write_parquet(df, save_to)
  logger::log_info("build_combined_df() saved to", save_to)
}


# LP algorithm ----

solve_lp_trade_flows <- function(sup, dem, dmat) {
  time_start <- Sys.time()
  
  stopifnot(length(sup) == nrow(dmat))
  stopifnot(length(dem) == ncol(dmat))
  stopifnot(all.equal(sum(sup), sum(dem)))
  
  # reduce dimensions to only positive elements in demand and supply vectors
  spos <- (sup > 0)
  dpos <- (dem > 0)
  sup_ <- sup[spos]
  dem_ <- dem[dpos]
  dmat_ <- dmat[spos, dpos, drop = FALSE]

  log_debug("Solving LP problem with {nrow(dmat_)} rows and {ncol(dmat_)} columns...")
  
  # constraints to iterate over: pairs of (sup, dem) signs to satisfy
  cons <- list(
    c("=", "="),
    c("=", ">="),
    c("=", "<="),
    c(">=", "="),
    c("<=", "="),
    c(">=", "<="),
    c("<=", ">="),
    c(">=", ">=")
  )
  for (con in cons) {
    con_label <- paste0("supply ", con[1], ", demand ", con[2])
    log_debug("Attempting to solve with constraints {con_label}")
    sol <- lp.transport(
      cost.mat = dmat_,
      row.signs = rep(con[1], length(sup_)),
      row.rhs = sup_,
      col.signs = rep(con[2], length(dem_)),
      col.rhs = dem_,
      integers = NULL
    )
    if (sol$status == 0) {
      break
    }
  }
  if (sol$status > 0) {
    warning("NO SOLUTION\n")
  }
  
  # attach dim names
  rownames(sol$solution) <- names(sup_)
  colnames(sol$solution) <- names(dem_)
  
  # create full-size solution matrix with zeros where supply or demand are zero
  x <- matrix(0, length(sup), length(dem))
  x[spos, dpos] <- sol$solution
  rownames(x) <- names(sup)
  colnames(x) <- names(dem)
  attr(x, "status") <- sol$status
  attr(x, "constraints") <- con_label
  sol$solution_full <- x
  
  time_diff <- Sys.time() - time_start
  log_debug(paste("LP solved in", round(time_diff, 2), attr(time_diff, "units")))
  sol
}

#' Solve trade flows using LP algorithm
#' 
#' @param ind_code industry code, use "all_industries" to sum over all.
call_trade_flows <- function(ind_code,
                             year,
                             ilevel = c("det", "sum", "sec"),
                             bus_data = c("cbp_imp", "cbp_raw", "infogroup")) {
  ilevel <- match.arg(ilevel)
  bus_data <- match.arg(bus_data)
  
  cache_path <- glue(opath$flows_)
  x <- read_cache(cache_path)
  if (!isFALSE(x)) {
    return(x)
  }
  
  ind_codes <- call_traded_commodities(year, ilevel = ilevel, bus_data = bus_data)
  
  if (ind_code == "all_industries") {
    # add up every commodity with positive excess supply and demand
    y <- 0
    for (ind_code in ind_codes) {
      x <- call_trade_flows(ind_code, year = year, ilevel = ilevel, bus_data = bus_data)
      y <- y + x
    }
  } else {
    log_info("Solving trade flows for commodity {ind_code}")
    
    df <- place_io$call_outsupdem(year, ilevel = ilevel, bus_data = bus_data) %>%
      filter(indcode == ind_code)
    
    # abort if commodity has no aggregate excess supply or demand
    if (!(ind_code %in% ind_codes)) {
      stop(glue("Can not calculate trade flows, aggregate excess supply or demand is zero for commodity {ind_code}"))
    }
    
    dmat <- distance_matrix(year)
    
    df_feas <- inner_join(
      distribute_to_feasible_counties(df$supply, df$place, rownames(dmat)) %>% rename(supply = value),
      distribute_to_feasible_counties(df$demand, df$place, rownames(dmat)) %>% rename(demand = value),
      by = "place"
    ) %>%
      arrange(place) %>%
      mutate(exsup = pmax(supply - demand, 0),
             exdem = pmax(demand - supply, 0))
    
    places <- df_feas$place
    dmat <- dmat[places, places]
    sup <- df_feas$exsup
    names(sup) <- places
    dem <- df_feas$exdem
    names(dem) <- places
    
    
    sol <- solve_lp_trade_flows(sup, dem, dmat)
    if (sol$status > 0) {
      log_warn("No solution found for {ind_code}")
    }
    y <- sol$solution_full
  }
  
  write_cache(y, cache_path)
  debug_env_save()
  y
}


# TODO: replace call_industry_codes above 
call_industry_codes_custom <- function(file_directory = glue(opath$flows_, .envir = append(params_econ, list(ind_code = ""))) ){
  df <- file_directory %>%
    dirname() %>%
    list.files() %>%
    tools::file_path_sans_ext()
  return(df)
}


# industry_code: single code or all, or comma-separated list of codes, or regex
filter_industry_codes <- function(cluster_subset, industry_code_vector){
  cs <- cluster_subset
  icv <- industry_code_vector
  if (cs == "all_industries" || grepl("^[[:alnum:]]+$", cs)) { # single code or all
    if (cs %in% icv){
      df <- cs
      return(df)
    } else {stop("Selection not feasible")}
  } else if (grepl(",", cs)) { # comma-separated list of codes
    if (all(str_split_1(cs, ",") %in% icv)){
      df <- str_split_1(cs, ",")
      return(df)
    } else if (any(str_split_1(cs, ",") %in% icv)) {
      nf <- str_split_1(cs, ",") %>%
        {.[which(!. %in% icv)]}
      warning("Select codes: ", nf ," not feasible")
      df <- str_split_1(cs, ",") %>%
        {.[which(. %in% icv)]}
      return(df)
    } else {stop("Selection not feasible")}
  } else { # regex
    if (length(grep(cs, icv)) > 0){
      df <- grep(cs, icv, value = TRUE)
      return(df)
    } else {stop("Selection not feasible")}
  }
}

call_trade_flows_custom <- function(cluster_subset,
                                    file_directory = glue(opath$flows_, .envir = append(params_econ, list(ind_code = ""))) ){
  lp <- call_industry_codes_custom(file_directory = file_directory) %>%
    {filter_industry_codes(cluster_subset, .)}
  df <- 0
    for (i in lp) {
      df <- df + call_trade_flows(i)
    }
  return(df)
}


# RAS+Gravity algorithm ----

#' County-to-county impedance matrix
#' @param form Impedance form - "gaus" or "prox"
#' @param dist_par Distance parameter in miles
imped_mat <- function(form, dist_par) {
  # convert all units to meters and discard unit information
  dist_par <- set_units(dist_par, mi) |> set_units(m) |> drop_units()
  dist_mat <- readRDS(ipath$dist_mat) |> drop_units()
  if (form == "prox") {
    y <- (dist_mat < dist_par)
    mode(y) <- "integer"
  } else if (form == "gaus") {
    y <- exp(-.5 * (dist_mat / dist_par)^2)
  }
  rownames(y) <- colnames(y) <- rownames(dist_mat)
  return(y)
}

#' Gravity model predicted trade flows
gravity <- function(sup, dem, imped_mat) {
  x <- outer(sup, dem) * imped_mat
  # normalize to match total
  x <- x * sum(dem) / sum(x)
  x
}


#' Iteratively scale prior matrix to match target row and column sums using RAS algorithm
#'
#' @param x0 Prior matrix.
#' @param rs1 Vector of target row sums.
#' @param cs1 Vector of target column sums.
#' @param tol Numerical tolerance number, iterate until RMSE between steps falls below `tol`.
#' @param maxiter Maximum number of iterations.
#' @return List with result and iteration diagnostics.
#' 
#' @examples
#' ras_rescale(matrix(1, 3, 3), c(0,2,3), c(3,0,2))
ras_rescale <- function(x0, rs1, cs1, tol = 0, maxiter = 10) {
  
  # stalling condition tolerance
  maxad_stall_tol <- 0.01

  # verify if targets sum to same total within relative tolerance
  sum_tol <- 0.001
  sum_dif <- abs(sum(rs1) - sum(cs1)) / sum(cs1)
  if (sum_dif > sum_tol) stop("sum(rs1) != sum(cs1)")
  
  # mask away all-zero rows and columns
  # they will be added back after RAS
  rpos <- (rs1 > 0) & (rowSums(x0) > 0)
  cpos <- (cs1 > 0) & (colSums(x0) > 0)
  x <- x0[rpos, cpos, drop=F]
  nr <- nrow(x)
  nc <- ncol(x)
  rs <- rs1[rpos]
  cs <- cs1[cpos]
  
  
  # initialize convergence metrics
  rmsd <- Inf # root mean squared deviation
  mad <- Inf # mean absolute deviation
  maxad <- Inf # maximum absolute deviation
  converged <- FALSE
  
  # print matrix and required scaling factors
  print_scale_factors <- function(x1, rows) {
    y <- matrix(0, nrow(x0), ncol(x0), dimnames = list(rownames(x0), colnames(x0)))
    y[rpos, cpos] <- x1
    if (rows) {
      rs <- rowSums(y)
      y <- cbind(y, rs, rs1, rs1 / rs)
      colnames(y)[-(1:ncol(x0))] <- c("sum", "target", "adj")
    } else {
      cs <- colSums(y)
      y <- rbind(y, cs, cs1, cs1 / cs)
      rownames(y)[-(1:nrow(x0))] <- c("sum", "target", "adj")
    }
    y <- round(y, 3)
    print(glue("scale {what}", what = if (rows) "rows" else "cols"))
    print(y)
  }
  
  for (iter in 1:maxiter) {
    # scale rows
    # print_scale_factors(x, TRUE)
    radj <- rs / rowSums(x)
    x1 <- matrix(radj, nrow(x), ncol(x)) * x
    # scale cols
    # print_scale_factors(x1, FALSE)
    cadj <- cs / colSums(x1)
    x1 <- matrix(cadj, nrow(x), ncol(x), byrow = TRUE) * x1

    # convergence metrics
    # calculated from rows, since column sums match target exactly after scaling
    rs_dev <- rowSums(x1) - rs
    rmsd <- sqrt(mean(rs_dev^2))
    mad <- mean(abs(rs_dev))
    maxad1 <- max(abs(rs_dev))

    # new matrix for next iteration    
    x <- x1
    
    # check stalling condition
    maxad_rel_ch <- abs(maxad1 - maxad) / maxad1
    if (maxad_rel_ch < maxad_stall_tol) {
      log_warn("Convergence stalled at relative MaxAD difference {maxad_rel_ch}")
      break
    }
    maxad <- maxad1

    converged <- (maxad <= tol)
    
    log_debug("RAS iteration: {iter}, RMSD: {rmsd}, MAD: {mad}, MaxAD: {maxad}, converged: {converged}")
    if (converged) break
  }
  if (iter == maxiter) log_warn("Maximum number of iterations reached")

  # add back rows and cols of zeroes
  x_full <- matrix(0, nrow(x0), ncol(x0))
  x_full[rpos, cpos] <- x
  rownames(x_full) <- rownames(x0)
  colnames(x_full) <- colnames(x0)
  
  # last used row adjustment factors and unmatched zeroes
  radj_full <- rep(1, nrow(x0))
  radj_full[rpos] <- radj
  radj_full[(rs1 > 0) & (rowSums(x0) == 0)] <- Inf
  names(radj_full) <- rownames(x0)
  
  # last used column adjustment factors and unmatched zeroes
  cadj_full <- rep(1, ncol(x0))
  cadj_full[cpos] <- cadj
  cadj_full[(cs1 > 0) & (colSums(x0) == 0)] <- Inf
  names(cadj_full) <- colnames(x0)
  
  out <- list(matrix = x_full,
              nrow_pos = nrow(x), 
              ncol_pos = ncol(x),
              iterations = iter, 
              rmsd = rmsd,
              mad = mad, 
              maxad = maxad,
              converged = converged,
              radj = radj_full,
              cadj = cadj_full)
  return(out)
}



# tests ----

## viz ----

#' Viz matrix as a heat map using ggplot
plot_heatmap <- function(x, zero_na = TRUE, discrete = FALSE) {
  
  if (is.null(rownames(x))) rownames(x) <- 1:nrow(x)
  if (is.null(colnames(x))) colnames(x) <- 1:ncol(x)
  x |>
    as_tibble(rownames = "from") |>
    pivot_longer(!from, names_to = "to") |>
    mutate(
      from = ordered(from, levels = rev(rownames(x))),
      to = ordered(to, levels = colnames(x)),
      value = if (zero_na) na_if(value, 0) else value,
      value = if (discrete) ordered(value) else value
    ) |>
    ggplot() +
    scale_x_discrete(position = "top") +
    geom_tile(aes(to, from, fill = value)) +
    coord_fixed()
}

## impedance ----

test_map_gravity <- function(ind_code, imped_form, imped_par, place) {
  imp <- imped_mat(imped_form, imped_par)
  dem <- readRDS(glue(opath$demand_, .envir = params_econ))[ind_code, ]
  sup <- readRDS(glue(opath$supply_, .envir = params_econ))[ind_code, ]
  common_places <- base::intersect(rownames(imp), base::intersect(names(dem), names(sup)))
  place_n <- length(common_places)
  imp <- imp[common_places, common_places]
  dem <- dem[common_places]
  sup <- sup[common_places]
  
  tf <- gravity(sup, dem, imp)
  
  exports <- tf[place, ] |>
    as_tibble(rownames = "place") |> 
    rename(exports_to = value)
  imports <- tf[, place] |>
    as_tibble(rownames = "place") |> 
    rename(imports_from = value)
  df <- geography$call_geog() |> 
    filter(!(STATE_CODE %in% c('02', '15', '60', '66', '69', '72', '78')))
  df <- left_join(df, exports, "place") |>
    left_join(imports, "place")
  df
}

## RAS ----

test_ras <- function() {
  imp <- imped_mat("gaus", 500)
  dem <- readRDS(glue(opath$demand_, .envir = params_econ))[1, ]
  sup <- readRDS(glue(opath$supply_, .envir = params_econ))[1, ]
  common_places <- base::intersect(rownames(imp), base::intersect(names(dem), names(sup)))
  place_n <- length(common_places)
  imp <- imp[common_places, common_places]
  dem <- dem[common_places]
  sup <- sup[common_places]
  t <- ras_trade_flows(gravity(dem, sup, imp), sup, dem, 1e-5, 1, TRUE)
  tf <- t$trade_matrix
  rownames(tf) <- colnames(tf) <- common_places
  place <- "55025"
  exports <- tf[place, ] |>
    as_tibble(rownames = "place") |> 
    rename(exports = value)
  imports <- tf[, place] |>
    as_tibble(rownames = "place") |> 
    rename(imports = value)
  df <- geography$call_geog() |> 
    filter(!(STATE_CODE %in% c('02', '15', '60', '66', '69', '72', '78')))
  df <- left_join(df, exports, "place") |>
    left_join(imports, "place")
  
  tmap_mode("view")
  tm_shape(df) + tm_fill("imports")
}


LP_diagnostic <- function(){
  icode <- call_industry_codes_custom() %>% {.[-which(. == "all_industries")]}
  df <- data.frame()
  for (i in icode){
    df <- i %>% 
      {c(., sum(prep_lp_solver_inputs(.)$sup), sum(prep_lp_solver_inputs(.)$dem), sum(call_trade_flows(.)))} %>% 
      {rbind(df, .)}
  }
  colnames(df) <- c("ind", "sup", "dem", "tf")
  df$sup <- as.numeric(df$sup)
  df$dem <- as.numeric(df$dem)
  df$tf <- as.numeric(df$tf)
  df$diff <- (df$tf - df$dem)
  df$abs_diff <- (abs(df$tf - df$dem) / df$tf)
  return(df)
} 








