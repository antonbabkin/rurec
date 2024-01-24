# Data preparation

# R libraries ----
library(logger)
log_threshold(DEBUG)
library(arrow)
library(tidyverse)
library(glue)


# R scripts ----
source("R/basic_utilities.R", local = (util <- new.env()))



# Data objects ----
ipath <- list(
)

opath <- list(
  population = "data/pubdata/population/population.pq"
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
  pymod$population <- import("rurec.pubdata.population")
  pymod$initialized <- TRUE
}


# Pubdata  ----
pubdata <- new.env()

pubdata$prep_population <- function() {
  if (!file.exists(opath$population)) {
    pymod$init()
    pymod$population$get_df()
  }
}


# Population ----

call_population <- function() {
  cache_path <- opath$population
  if (!file.exists(cache_path)) {
    pubdata$prep_population()
  }
  return(read_parquet(cache_path))
}



