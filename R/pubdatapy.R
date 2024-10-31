# Wrapper around Python pubdata module.
# Use to cache Python outputs in R accessible format.


# R libraries ----
library(logger)
library(tidyverse)
library(arrow)
library(glue)


# R scripts ----
source("R/basic_utilities.R", local = (util <- new.env()))



# Data objects ----
ipath <- list(

)

opath <- list(
  naics_code_ = "data/pubdatapy/naics/code/{year}.pq"
)

clear_outputs <- function() {
  util$clear_paths(opath)
}


# Python modules ----
pymod <- new.env()
pymod$initialized <- FALSE

#' Initialize environment with necessary Python modules imported through reticulate
#' Running multiple times is safe and only imports once.
pymod$init <- function() {
  if (pymod$initialized) return()
  library(reticulate)
  use_condaenv("rurec")
  pymod$naics <- import("rurec.pubdata.naics")
  pymod$initialized <- TRUE
}


# NAICS ----


build_naics <- function(overwrite = FALSE) {
  log_info("building naics cache...")
  pymod$init()
  
  # 2-6 digit NAICS code structure
  for (year in seq(2002L, 2022L, 5L)) {
    p <- glue(opath$naics_code_)
    if (file.exists(p) && !overwrite) {
      log_info("file already exists {p}")
    } else {
      x <- pymod$naics$get_df(year = year, kind = "code") %>%
        rename_with(str_to_lower)
      arrow::write_parquet(x, util$mkdir(p))
      log_info("saved to cache {p}")
    }
  }
}