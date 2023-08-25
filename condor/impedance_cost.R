

library(magrittr)

TEST_RUN <- FALSE

#parameters
tol <- 1e-0
min_d <- 25
max_d <- 2000
step_d <- if (TEST_RUN) 250 else 25
maxiter <- if (TEST_RUN) 10 else 1000
imp_funct <- "gaus_impedance_mat"
crosshaul <- FALSE
verbose <- TRUE

generate_inputs <- function() {
  library(tidyverse)
  library(glue)

  year = 2012
  ilevel = "det"
  data_source = "infogroup"

  source(file.path(rprojroot::find_rstudio_root_file(), "nbs/r_visualization_functions.R"))
  
  industry_factor_demand_matrix(year = year, ilevel = ilevel, data_source = data_source) %>%
    saveRDS(file = c)
  industry_factor_supply_matrix(year = year, ilevel = ilevel, data_source = data_source) %>%
    saveRDS(file = rprojroot::find_rstudio_root_file("condor/supply.rds"))
  
  dist_matc() %>% 
    saveRDS(file = rprojroot::find_rstudio_root_file("condor/center2center_distmat.rds"))
  
}

output_summary <- function(output_dir) {
  list.files(output_dir, "diagnostic_*") %>%
    map(\(x) file.path(output_dir, x)) %>%
    map(\(x) readRDS(x) %>% tail(1)) %>%
    bind_rows()
}

# d <- output_summary(rprojroot::find_rstudio_root_file("condor/output"))


args <- commandArgs(trailingOnly = TRUE)
print(c("Command line arguments:", args))
if (args[1] == "inputs") {
  print("preparing inputs...")
  generate_inputs()
  quit("no")
} else if (args[1] == "compute") {
  print("calculating trade flows...")
  industry_index <- as.integer(args[2])
  # proceed through the rest of the script
} else {
  print("unkown argument, stopping")
  quit("no", 1)
}


#input matricies files
factor_supply <- readRDS("supply.rds")[industry_index, , drop=F]
factor_demand <- readRDS("demand.rds")[industry_index, , drop=F]
center2center_distmat <- readRDS("center2center_distmat.rds")




#Balance trade flow matrix using RAS algorithm. Iteratively update prior matrix until rows and columns sum to target vectors.
ras_trade_flows <- function (x0, rs1, cs1, tol, maxiter, verbose) {
  # test if targets sum to same total, within 0.1% tolerance
  sum_tol <- 0.001
  sum_dif <- abs(sum(rs1) - sum(cs1)) / sum(cs1)
  if (sum_dif > sum_tol) stop("sum(rs1) != sum(cs1)")
  # mask away all-zero rows and columns
  rpos <- (rs1 > 0) & sapply(rowSums(x0[, cs1 > 0, drop=F]), function(x){!isTRUE(all.equal(x, 0))} )
  cpos <- (cs1 > 0) & sapply(colSums(x0[rs1 > 0, ,drop=F]), function(x){!isTRUE(all.equal(x, 0))} )
  x <- x0[rpos, cpos, drop=F]
  nr <- nrow(x)
  nc <- ncol(x)
  rs <- rs1[rpos]
  cs <- cs1[cpos]
  mad <- -1
  rmse <- -1
  for (i in 1:maxiter) {
    # scale rows
    x1 <- matrix(rs / rowSums(x), nr, nc) * x
    # scale cols
    x1 <- matrix(cs / colSums(x1), nr, nc, byrow = TRUE) * x1
    if (mad == mean(abs(rowSums(x1) - rs))) {warning("\n\n  No convergence: infeasible\n")
      break}
    rmse <- max(max(abs(rowSums(x1) - rs)) , max(abs(colSums(x1) - cs)))
    mad <- mean(abs(rowSums(x1) - rs))
    x <- x1
    if (verbose) cat(paste("  Iteration:", i, "  RMSE:", rmse, " MAD:", mad, "\n"))
    if (rmse < tol) break
  }
  if (i == maxiter) warning("\n\n  No convergence. Maximum Number of iterations reached. Consider increasing the number of iterations.\n")
  if (verbose) cat(paste("Number of iterations:", i, "RMSE:", rmse, "\n"))
  # return zero rows and cols back
  xz <- matrix(0, nrow(x0), ncol(x0))
  xz[rpos, cpos] <- x
  out <- list("trade_matrix" = xz, "ras_supply_dim" =  dim(x)[1], "ras_demand_dim" =  dim(x)[2], "iterations" = i, "rmse" = rmse, "mad" = mad)
  return(out)
}

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




