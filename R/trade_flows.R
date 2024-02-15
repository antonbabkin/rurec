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
  supply_ = "data/trade_flows/demsup/supply_{class_sys}_{ilevel}_{year}_{bus_data}.rds"
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



#' Calculate trade flows for one industry
#' @param industry_index index of the industry list in demand and supply matrices
calc_trade_flows <- function(industry_index) {
  
  #input matricies files
  factor_supply <- readRDS("supply.rds")[industry_index, , drop=F]
  factor_demand <- readRDS("demand.rds")[industry_index, , drop=F]
  center2center_distmat <- readRDS("center2center_distmat.rds")
  
  

  # Convert miles to meters
  miles2meters <- function(miles){
    df <- as.integer(miles)*1609.344
    return(df)
  }
  
  # Produce Gaussian distance decay impedance matrix
  gaus_impedance_mat <- function(rms_width = miles2meters(1000),
                                 df = center2center_distmat,
                                 ...){
    df <- exp(-.5*(df/rms_width)^2) 
    return(df)
  }
  
  # Produce binary Distance Proximity Matrix from polygon center
  dprox_mat <- function(boundary_limit = miles2meters(1000),
                        df = center2center_distmat,
                        ...){
    df[df < boundary_limit & df > 0] <- 1
    df[df > boundary_limit] <- 0
    diag(df) <- 1
    return(df)
  }
  
  # Find minimum impedance distance that satisfies RAS algorithm
  min_imp_ras <- function(factor_supply, 
                          factor_demand,
                          center2center_distmat,
                          imp_funct,
                          crosshaul,
                          tradeflow_dir,
                          diag_location,
                          impedance_dir,
                          min_d,
                          max_d,
                          step_d,
                          tol,
                          verbose,
                          ...){
    i <- rownames(factor_demand)
    g <- center2center_distmat
    fs <- intersect(colnames(factor_supply), colnames(g)) %>% factor_supply[, ., drop=F]
    fd <- intersect(colnames(factor_demand), colnames(g)) %>% factor_demand[, ., drop=F]
    pl <- intersect(intersect(colnames(fs), colnames(fd)), intersect(colnames(g), rownames(g))) 
    
    if(isTRUE(crosshaul)){
      fsx <- fs
      fdx <- fd
    } else {
      fsx <- pmax(fs - fd, 0)
      fdx <- pmax(fd - fs, 0)
    }
    
    # pre-calculate impedance matrices, not useful if running single industry in isolated environment
    # imprf <- list.files(impedance_dir)
    # for(d in seq(min_d, max_d, by = step_d)){
    #   if(!d %in% imprf){
    #     temp_imp <- do.call(get(imp_funct), list(miles2meters(d)))
    #     saveRDS(temp_imp, file = file.path(impedance_dir, d))
    #   }
    # }
    
    df <- data.frame("sector" = c(),
                     "impedance" = c(),
                     "ras_supply_dim" = c(),
                     "ras_demand_dim" = c(),
                     "iterations" = c(),
                     "rmse" = c(),
                     "mad" = c())
      for(d in seq(min_d, max_d, by = step_d)){
        print(paste("Industry:", i, " Distance:", d))
        # impedance_mat <- readRDS(file.path(impedance_dir, d))
        impedance_mat <- do.call(get(imp_funct), list(miles2meters(d)))
        xs <- (t(fsx) %*%  fdx) * impedance_mat[pl, pl]
        tf <- ras_trade_flows(x0 = xs,
                              rs1 = fsx,
                              cs1 = fdx,
                              tol = tol,
                              maxiter = maxiter,
                              verbose = verbose)
        colnames(tf[[1]]) = colnames(fdx)
        rownames(tf[[1]]) = colnames(fsx)
        df <- rbind(df, data.frame("sector" = i, 
                                   "impedance" = d, 
                                   "ras_supply_dim" = tf[["ras_supply_dim"]], 
                                   "ras_demand_dim" = tf[["ras_demand_dim"]], 
                                   "iterations" = tf[["iterations"]], 
                                   "rmse" = tf[["rmse"]], 
                                   "mad" = tf[["mad"]]))
        saveRDS(df, file = paste0("diagnostic_", i))
        if (max(max(abs(rowSums(tf[[1]]) - fsx)) , max(abs(colSums(tf[[1]]) - fdx))) < tol) {
          break
        }
      }
      saveRDS(tf[[1]], file = paste0("trade_flows_", i))
      out <- list("synopsis" = df, 
                  "matrix" = tf[[1]])
      return(out) 
  }
  
  
  test_out <- min_imp_ras(factor_supply = factor_supply, 
                          factor_demand = factor_demand,
                          center2center_distmat = center2center_distmat,
                          imp_funct = imp_funct,
                          crosshaul = crosshaul,
                          min_d = min_d,
                          max_d = max_d,
                          step_d = step_d,
                          tol = tol,
                          verbose = verbose)


}

# tests ----

## impedance ----
# center <- "55025"
# imped_form <- "gaus"
# dist_par <- 700
# d <- imped_mat(imped_form, dist_par)[, center, drop=FALSE] |>
#   as_tibble(rownames = "place") |>
#   rename(all_of(c(impedance = center)))
# df <- left_join(
#   geog$call_geog() |> filter(!(STATE_CODE %in% c('02', '15', '60', '66', '69', '72', '78'))),
#   d, "place")
# tm_shape(df) + tm_fill("impedance", style = switch(imped_form, prox = "cat", gaus = "cont"))

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
# results ----


output_summary <- function(output_dir) {
  list.files(output_dir, "diagnostic_*") %>%
    map(\(x) file.path(output_dir, x)) %>%
    map(\(x) readRDS(x) %>% tail(1)) %>%
    bind_rows()
}

# d <- output_summary(rprojroot::find_rstudio_root_file("condor/output"))


# CLI ----
# running script from command line with arguments
args <- commandArgs(trailingOnly = TRUE)
print(c("Command line arguments:", args))
if (length(args) == 0) {
  print("functions defined")
} else if (args[1] == "inputs") {
  print("preparing inputs...")
  generate_inputs()
} else if (args[1] == "compute") {
  print("calculating trade flows...")
  calc_trade_flows(as.integer(args[2]))
}
