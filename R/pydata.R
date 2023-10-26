# This script is a thin caching wrapper around data-generating Python modules.


params <- list(
  zip_pack_path = "tmp/pydata.zip"
)


library(logger)
log_threshold(DEBUG)
library(arrow)
library(tidyverse)
library(glue)


# data warehouse ----
source("R/data_manager.R", local = (dw <- new.env()))
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
  for (year in c(2012, 2017)) {
    bea_io$get_naics_concord(year)
  }
  for (year in 1997:2022) {
    log_debug(year)
    for (level in c("sec", "sum", "det")) {
      if (level == "det" && !(year %in% c(2007, 2012, 2017))) next
      for (labels in c(FALSE, TRUE)) {
        bea_io$get_sup(year, level, labels)
        bea_io$get_use(year, level, labels)
      }
    }
  }
  
  cbp$get_cbp_df("us", 2012)
  cbp$get_cbp_df("county", 2012)
  cbp$get_efsy_year_df(2012)
  
}

# Conversion helper functions ----

#' Un-list columns
#' Sometimes pandas df columns come as lists that need unlisting.
#' This is likely happening with string columns with missing values.
#' More investigation is desirable, maybe this corrections could be done by customizing reticulate.
#' For now, simply unlist() all columns that are of list class.
#' Usage: df <- col_list2vec(df)
col_list2vec <- function(df) {
  df %>% mutate(across(where(is.list), \(x) unlist(map_if(x, is.null, \(y) NA))))
}


# BEA IO ----
bea_io <- new.env()

bea_io$get_naics_concord <- function(year) {
  year <- as.integer(year)
  p <- glue(paths$bea_io_naics_concord_)
  if (file.exists(p)) {
    log_debug(paste("read from cache", p))
    return(readRDS(p))
  }
  x <- pymod$bea_io$get_naics_concord(year) %>%
    col_list2vec()
  log_debug(paste("save to cache", p))
  saveRDS(x, dw$mkdir(p))
  return(x)
}


bea_io$get_sup <- function(year, level, labels = FALSE) {
  match.arg(level, c("det", "sum", "sec"))
  year <- as.integer(year)
  p <- glue(paths$bea_io_sup_)
  if (file.exists(p)) {
    log_debug(paste("read from cache", p))
    return(readRDS(p))
  }
  
  x <- pymod$bea_io$get_sup(year, level, labels)

  # repair row names of sector supply tables
  # last row in spreadsheet is missing value in "T017" the code column
  # when dataframe is converted from pandas, entire row index is ignored because of a missing value
  if (level == "sec") {
    x$row_names[[length(x$row_names)]] <- c("T017", "Total industry supply")
    if (!labels) {
      rownames(x$table) <- map_chr(x$row_names, \(x) x[1])
    }
  }

  log_debug(paste("save to cache", p))
  saveRDS(x, dw$mkdir(p))
  return(x)
}


bea_io$get_use <- function(year, level, labels = FALSE) {
  match.arg(level, c("det", "sum", "sec"))
  year <- as.integer(year)
  p <- glue(paths$bea_io_use_)
  if (file.exists(p)) {
    log_debug(paste("read from cache", p))
    return(readRDS(p))
  }
  x <- pymod$bea_io$get_use(year, level, labels)
  log_debug(paste("save to cache", p))
  saveRDS(x, dw$mkdir(p))
  return(x)
}


# CBP ----
cbp <- new.env()

cbp$get_cbp_df <- function(geo, year) {
  year <- as.integer(year)
  p <- glue(paths$cbp_raw_)
  if (file.exists(p)) {
    log_debug(paste("read from cache", p))
    return(read_parquet(p))
  }
  return(pymod$cbp$get_cbp_df(geo, year))
}

cbp$get_efsy_year_df <- function(year) {
  year <- as.integer(year)
  p <- glue(paths$cbp_efsy_)
  if (file.exists(p)) {
    log_debug(paste("read from cache", p))
    return(read_parquet(p))
  }
  return(pymod$cbp$get_efsy_year_df(year))
}

