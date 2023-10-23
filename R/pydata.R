# This script is a thin caching wrapper around data-generating Python modules.


params <- list(
  zip_pack_path = "tmp/pydata.zip"
)


library(logger)
log_threshold(DEBUG)
library(arrow)
library(tidyverse)
library(glue)

source("R/reseng.R", local = (reseng <- new.env()))

# data warehouse ----
source("R/data_warehouse.R", local = (dw <- new.env()))
paths <- names(dw$paths) %>%
  keep(\(x) str_starts(x, "pydata_")) %>%
  rlang::set_names() %>%
  map(\(x) dw$paths[[x]]) %>%
  rlang::set_names(str_remove(names(.), "pydata_"))

# pack data objects as zip
# pack_all(overwrite = TRUE)
pack_all <- function(zipfile = params$zip_pack_path, overwrite = FALSE) {
  ps <- c(
    paths$bea_io_dir,
    paths$cbp_raw_dir,
    paths$cbp_efsy_dir
  )
  log_info(paste("Saving pydata zip pack to", zipfile))
  log_debug(paste("Specified input paths:", str_flatten_comma(ps)))
  dw$pack(zipfile, ps, overwrite)
}

# unpack
# dw$unpack(params$zip_pack_path)


# Python env ----
# try to use Python environment and load modules
tryCatch({
  library(reticulate)
  use_condaenv("rurec")
  pymod <- new.env()
  pymod$cbp <- import("rurec.pubdata.cbp")
  pymod$bea_io <- import("rurec.pubdata.bea_io")
}, error = function (e) {
  print(e)
  log_info("Python environment not found, data will be read from cache.")
})

# Cache all ----
# Execute all data generating functions for the purpose of caching their results.
cache_all <- function() {
  for (year in c(2012L, 2017L)) {
    bea_io$get_naics_concord(year)
  }
  for (year in 1997L:2022L) {
    log_debug(year)
    for (level in c("sec", "sum", "det")) {
      if (level == "det" && !(year %in% c(2012L, 2017L))) next
      for (labels in c(FALSE, TRUE)) {
        bea_io$get_sup(year, level, labels)
        bea_io$get_use(year, level, labels)
      }
    }
  }
  
  cbp$get_cbp_df("us", 2012L)
  cbp$get_cbp_df("county", 2012L)
  cbp$get_efsy_year_df(2012L)
  
}
  

# BEA IO ----
bea_io <- new.env()

bea_io$get_naics_concord <- reseng$cache(
  \(year) pymod$bea_io$get_naics_concord(year),
  paths$bea_io_naics_concord_
)

bea_io$get_sup <- reseng$cache(
  \(year, level, labels) pymod$bea_io$get_sup(year, level, labels),
  paths$bea_io_sup_
)

bea_io$get_use <- reseng$cache(
  \(year, level, labels) pymod$bea_io$get_use(year, level, labels),
  paths$bea_io_use_
)


# CBP ----
cbp <- new.env()

cbp$get_cbp_df <- function(geo, year) {
  p <- glue(paths$cbp_raw_)
  if (file.exists(p)) {
    log_debug(paste("read from cache", p))
    return(read_parquet(p))
  }
  return(pymod$cbp$get_cbp_df(geo, year))
}

cbp$get_efsy_year_df <- function(year) {
  p <- glue(paths$cbp_efsy_)
  if (file.exists(p)) {
    log_debug(paste("read from cache", p))
    return(read_parquet(p))
  }
  return(pymod$cbp$get_efsy_year_df(year))
}

