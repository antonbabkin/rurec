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
  naics_code_ = "data/pubdatapy/naics/code/{year}.pq",
  population = "data/pubdata/population/population.pq",
  price_index = "data/pubdata/bea_nipa/price_index.pq"
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
  pymod$population <- import("rurec.pubdata.population")
  pymod$bea_nipa <- import("rurec.pubdata.bea_nipa")
  pymod$initialized <- TRUE
}


# NAICS ----


build_naics <- function(overwrite = FALSE) {
  log_info("building naics cache...")
  pymod$init()
  
  # 2-6 digit NAICS code structure
  for (year in seq(2002L, 2022L, 5L)) {
    cache_path <- glue(opath$naics_code_)
    if (file.exists(cache_path) && !overwrite) {
      log_info("file already exists {cache_path}")
    } else {
      x <- pymod$naics$get_df(year = year, kind = "code") %>%
        rename_with(str_to_lower)
      arrow::write_parquet(x, util$mkdir(cache_path))
      log_info("saved to cache {cache_path}")
    }
  }
}


# Annual county population estimates
build_population <- function() {
  if (!file.exists(opath$population)) {
    pymod$init()
    pymod$population$get_df()
  }
}


# BEA price indexes from NIPA
build_price_index <- function() {
  cache_path <- opath$price_index
  if (!file.exists(cache_path)) {
    pymod$init()
    pymod$bea_nipa$get_price_index_df() %>%
      rownames_to_column("year") %>%
      mutate(year = as.integer(year)) %>%
      arrow::write_parquet(util$mkdir(cache_path))
  }
}


