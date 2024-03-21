# Calculate county-to-county intermediate commodity trade flows.
# CLI usage:
# Rscript R/impedance_cost.R inputs # prepare inputs (demand, supply and distance), must run from project root
# Rscript impedance_cost.R compute <industry index> # calculate flows for 1 industry, runs from script dir, which must contain all inputs

# params ----
# whether execution happens in the Condor HTC environment or on a local machine
CONDOR <- FALSE


TEST_RUN <- FALSE

#parameters
imped_form <- "gaus" # gaus or prox
crosshaul <- FALSE
verbose <- TRUE

# economic data
params_econ <- list(
  year = 2012,
  ilevel = "det",
  class_sys = "commodity",
  bus_data = "infogroup"
)

# RAS parameters
params_ras <- list(
  tol = 1e-1,
  min_d = 25,
  max_d = 2000,
  step_d = if (TEST_RUN) 250 else 5,
  maxiter = if (TEST_RUN) 10 else 10000
)


# imports ----
library(units)
library(glue)
library(logger)




if (CONDOR) {
  library(magrittr)
} else {
  library(tidyverse)
  library(glue)
  library(lpSolve)
  
  source("R/geography.R", local = (geog <- new.env()))
  source("R/place_output.R", local = (place_output <- new.env()))
  source("R/basic_utilities.R", local = (util <- new.env()))
}

# data ----

ipath <- list(
  dist_mat = "data/geo/dist/center_2013_cbsaFALSE.rds"
)

opath <- list(
  demand_ = "data/trade_flows/demsup/demand_{class_sys}_{ilevel}_{year}_{bus_data}.rds",
  supply_ = "data/trade_flows/demsup/supply_{class_sys}_{ilevel}_{year}_{bus_data}.rds",
  flows_ = "data/trade_flows/flows_{class_sys}_{ilevel}_{year}_{bus_data}/{ind_code}.rds"
)


#' Prepare demand and supply matrices
#' Parameters are stored in global variables
prep_demsup_mat <- function() {
  dem_cache <- glue(opath$demand_, .envir = params_econ)
  if (file.exists(dem_cache)) {
    log_debug("demand matrix found at ", dem_cache)
  } else {
    d <- place_output$call_intermediate(
      params_econ$year, schedule = "demand", paradigm = "domestic",
      class_system = params_econ$class_sys,
      ilevel = params_econ$ilevel,
      bus_data = params_econ$bus_data)
    x <- pivot_wider(d, id_cols = "indcode", names_from = "place", values_from = "demand")
    ind_codes <- x$indcode
    x <- x |> select(!indcode) |> as.matrix()
    rownames(x) <- ind_codes
    saveRDS(x, util$mkdir(dem_cache))
    log_debug("demand matrix saved to ", dem_cache)
  }
  
  sup_cache <- glue(opath$supply_, .envir = params_econ)
  if (file.exists(sup_cache)) {
    log_debug("supply matrix found at ", sup_cache)
  } else {
    d <- place_output$call_intermediate(
      params_econ$year, schedule = "supply", paradigm = "domestic",
      class_system = params_econ$class_sys,
      ilevel = params_econ$ilevel,
      bus_data = params_econ$bus_data)
    x <- pivot_wider(d, id_cols = "indcode", names_from = "place", values_from = "supply")
    ind_codes <- x$indcode
    x <- x |> select(!indcode) |> as.matrix()
    rownames(x) <- ind_codes
    saveRDS(x, util$mkdir(sup_cache))
    log_debug("supply matrix saved to ", sup_cache)
  }
}

#' Return available industry codes
call_industry_codes <- function() {
  glue(opath$demand_, .envir = params_econ) |>
    readRDS() |>
    rownames()
}


# LP algorithm ----

#' Prepare data for LP solver
prep_lp_solver_inputs <- function(ind_code) {
  # load pre-calculated demand and supply vectors for selected industry
  sup_mat <- readRDS(glue(opath$supply_, .envir = params_econ))
  dem_mat <- readRDS(glue(opath$demand_, .envir = params_econ))
  stopifnot(isTRUE(all.equal(colnames(sup_mat), colnames(dem_mat))))
  
  # load pre-calculated distance matrix
  dmat <- readRDS(ipath$dist_mat)
  rn <- rownames(dmat)
  cn <- colnames(dmat)
  stopifnot(isTRUE(all.equal(rn, cn)))
  dmat <- dmat |>
    set_units(mi) |> # this operation destroys dim names
    drop_units()
  rownames(dmat) <- rn
  colnames(dmat) <- cn
  
  # select counties present both in sup/dem vectors and distance matrix
  common_places <- base::intersect(colnames(sup_mat), rownames(dmat)) |>
    sort()
  log_warn("Counties not in sup/dem: ", paste(setdiff(rownames(dmat), common_places), collapse = ","))
  log_warn("Counties not in dist mat: ", paste(setdiff(colnames(sup_mat), common_places), collapse = ","))
  
  # commonly available data
  sup <- sup_mat[ind_code, common_places]
  dem <- dem_mat[ind_code, common_places]
  dmat <- dmat[common_places, common_places]
  
  # only trade in excess of local demand/supply (no cross-hauling)
  exsup <- pmax(sup - dem, 0)
  exdem <- pmax(dem - sup, 0)
  
  # equalize sums of supply and demand:
  # scale up the smaller vector if sums do not match
  if (isTRUE(all.equal(sum(exsup), sum(exdem)))) {
    log_debug("sup == dem")
  } else if (sum(exsup) > sum(exdem)) {
    scale_factor <- sum(exsup) / sum(exdem)
    log_warn("sup > dem, scale dem by ", scale_factor)
    exdem <- exdem * scale_factor
  } else if (sum(exdem) > sum(exsup)) {
    scale_factor <- sum(exdem) / sum(exsup)
    log_warn("dem > sup, scale sup by ", scale_factor)
    exsup <- exsup * scale_factor
  }

  return(list(sup = exsup, dem = exdem, dmat = dmat))
}


solve_lp_trade_flows <- function(sup, dem, dmat) {
  time_start <- Sys.time()
  
  stopifnot(length(sup) == nrow(dmat))
  stopifnot(length(dem) == ncol(dmat))
  stopifnot(isTRUE(all.equal(sum(sup), sum(dem))))
  
  # reduce dimensions to only positive elements in demand and supply vectors
  spos <- (sup > 0)
  dpos <- (dem > 0)
  sup_ <- sup[spos]
  dem_ <- dem[dpos]
  dmat_ <- dmat[spos, dpos, drop = FALSE]

  log_debug("Solving LP problem with {nrow(dmat_)} rows and {ncol(dmat_)} columns...")

  sol <- lp.transport(
    cost.mat = dmat_,
    row.signs = rep("=", length(sup_)),
    row.rhs = sup_,
    col.signs = rep("=", length(dem_)),
    col.rhs = dem_,
    integers = NULL
  )
  
  # if there is no feasibile solution with exact equality constraints, try inequalities
  if (sol$status > 0) {
    log_debug("No feasible solution with equality constraints, trying inequality.")
    sol <- lp.transport(
      cost.mat = dmat_,
      row.signs = rep("<=", length(sup_)),
      row.rhs = sup_,
      col.signs = rep(">=", length(dem_)),
      col.rhs = dem_,
      integers = NULL
    )
  }
  
  # attach dim names
  rownames(sol$solution) <- names(sup_)
  colnames(sol$solution) <- names(dem_)
  
  # create full-size solution matrix with zeros where supply or demand are zero
  x <- matrix(0, length(sup), length(dem))
  x[spos, dpos] <- sol$solution
  rownames(x) <- names(sup)
  colnames(x) <- names(dem)
  sol$solution_full <- x
  
  time_diff <- Sys.time() - time_start
  log_debug(paste("LP solved in", round(time_diff, 2), attr(time_diff, "units")))
  return(sol)
}

#' Solve trade flows using LP algorithm
#' 
#' @param ind_code industry code, use "all_industries" to sum over all.
call_trade_flows <- function(ind_code) {
  cache_path <- glue(opath$flows_, .envir = append(params_econ, list(ind_code = ind_code)))
  if (file.exists(cache_path)) {
    log_debug("read from cache ", cache_path)
    y <- readRDS(cache_path)
  } else {
    if (ind_code == "all_industries") {
      # solve for every industry
      industry_codes <- call_industry_codes()
      y <- 0
      for (ind_code in industry_codes) {
        x <- call_trade_flows(ind_code)
        y <- y + x
      }
    } else {
      log_debug("Solving trade flows for industry {ind_code}")
      x <- prep_lp_solver_inputs(ind_code)
      sol <- solve_lp_trade_flows(x$sup, x$dem, x$dmat)
      y <- sol$solution_full
    }
    saveRDS(y, util$mkdir(cache_path))
    log_debug("saved to cache ", cache_path)
  }
  return(y)
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
  df <- geog$call_geog() |> 
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
  df <- geog$call_geog() |> 
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
  return(df)
} 








