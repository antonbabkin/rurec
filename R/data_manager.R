# The purpose of this script is to:
# 1. Maintain single registry of data objects.
# 2. Provide functions for packing and unpacking selected data objects as zip files for sharing.

library(rprojroot)
library(logger)



# use forward slashes (/) for nested paths even on Windows
# use {x} for wildcard paths to be later filled with glue()
paths <- list(
  pydata_bea_io_dir = "data/pydata/bea_io",
  pydata_bea_io_naics_concord_ = "data/pydata/bea_io/naics_concord/{year}.rds",
  pydata_bea_io_sup_ = "data/pydata/bea_io/sup/{level}/{year}_{labels}.rds",
  pydata_bea_io_use_ = "data/pydata/bea_io/use/{level}/{year}_{labels}.rds",
  pydata_cbp_raw_dir = "data/pubdata/cbp/cbp_pq",
  pydata_cbp_raw_ = "data/pubdata/cbp/cbp_pq/{geo}/{year}.pq",
  pydata_cbp_efsy_dir = "data/pubdata/cbp/efsy_pq/years",
  pydata_cbp_efsy_ = "data/pubdata/cbp/efsy_pq/years/{year}.pq",
  dashboard_county_geography = "data/dashboard/county_geography.rds",
  dashboard_county_agg_stats = "data/dashboard/county_agg_stats.rds",
  dashboard_county_ind_outsupdem = "data/dashboard/county_ind_outsupdem.rds"
)


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
