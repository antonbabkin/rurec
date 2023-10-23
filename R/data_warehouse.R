# The purpose of this script is to:
# 1. Maintain single registry of data objects.
# 2. Provide functions for packing and unpacking selected data objects as zip files for sharing.

library(rprojroot)
library(logger)


mkdir <- function(p) {
  d <- dirname(p)
  if (!dir.exists(d)) {
    logger::log_debug(paste("Creating directory", d))
    dir.create(d, recursive = TRUE)
  }
  return(p)
}

clear <- function(paths) {
  for (p in paths) {
    if (!file.exists(p)) next
    logger::log_info(paste("Removing", p))
    unlink(p, recursive = TRUE)
  }
}
    

paths <- list(
  dashboard_county_geography = "data/dashboard/county_geography.rds",
  dashboard_county_agg_stats = "data/dashboard/county_agg_stats.rds",
  dashboard_county_ind_outsupdem = "data/dashboard/county_ind_outsupdem.rds"
)


pack <- function(zipfile, files, overwrite = FALSE) {
  stopifnot(getwd() == rprojroot::find_rstudio_root_file())
  if (file.exists(zipfile)) {
    if (overwrite) {
      logger::log_info(paste("Replacing existing Zip file:", zipfile))
      file.remove(zipfile)
    }
    else stop("Zip file already exists: ", zipfile)
  }
  zip(mkdir(zipfile), as.character(files))
}


unpack <- function(zipfile, overwrite = FALSE) {
  stopifnot(getwd() == rprojroot::find_rstudio_root_file())
  stopifnot(file.exists(zipfile))
  unzip(zipfile, overwrite = overwrite)
}
