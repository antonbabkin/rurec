

#' Balance trade flow matrix using RAS algorithm
#' 
#' Iteratively update prior matrix until rows and columns sum to target vectors.
#'
#' @param x0 Prior trade flows matrix.
#' @param rs1 Vector of target row sums.
#' @param cs1 Vector of target column sums.
#' @param tol Numerical tolerance number, iterate until RMSE between steps falls below `tol`.
#' @param maxiter Maximum number of iterations.
#' @param verbose Print iteration progress.
#' @return Balanced trade flow matrix.
#' 
#' @examples
#' ras_trade_flows(matrix(1, 3, 3), c(0,2,3), c(3,0,2))
ras_trade_flows <- function (x0, rs1, cs1, tol = 1e-2, maxiter = 1000, verbose = FALSE) 
{
  # test if targets sum to same total, within 0.1% tolerance
  sum_tol <- 0.001
  sum_dif <- abs(sum(rs1) - sum(cs1)) / sum(cs1)
  if (sum_dif > sum_tol) stop("sum(rs1) != sum(cs1)")

  # mask away all-zero rows and columns
  rpos <- (rs1 > 0) & (rowSums(x0) > 0)
  cpos <- (cs1 > 0) & (colSums(x0) > 0)
  x <- x0[rpos, cpos, drop=F]
  nr <- nrow(x)
  nc <- ncol(x)
  rs <- rs1[rpos]
  cs <- cs1[cpos]
  
  for (i in 1:maxiter) {
    # scale rows
    x1 <- matrix(rs / rowSums(x), nr, nc) * x
    # scale cols
    x1 <- matrix(cs / colSums(x1), nr, nc, byrow = TRUE) * x1
    rmse <- sqrt(mean((x1 - x)^2))
    if (verbose) cat(paste("  Iteration:", i, "  RMSE:", rmse, "\n"))
    if (rmse < tol) break
    x <- x1
  }
  if (i == maxiter) warning("\n\n  No convergence. Maximum Number of iterations reached. Consider increasing the number of iterations.\n")
  if (verbose) cat(paste("Number of iterations:", i, "RMSE:", rmse, "\n"))
  
  # return zero rows and cols back
  xz <- matrix(0, nrow(x0), ncol(x0))
  xz[rpos, cpos] <- x
  xz
}

