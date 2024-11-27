
# This script generates circularity metrics

# R libraries ----
library(logger)
library(arrow)
library(tidyverse)
library(glue)

# R scripts ----
source("R/basic_utilities.R", local = (util <- new.env()))
source("R/place_io.R", local = (place_io <- new.env()))

# Data objects ----
# data dependencies
ipath <- list(
  
)

# data outputs
opath <- list(
  
)

clear_outputs <- function() {
  util$clear_paths(opath)
}


# circularity indicators ----

circularity_indicators <- function(year, bus_data = c("cbp_imp", "cbp_raw", "infogroup")) {
  bus_data <- match.arg(bus_data)

  df <- place_io$outsupdem(year, ilevel = "det", bus_data = bus_data) %>%
    mutate(exsup = pmax(0, supply - demand), exdem = pmax(0, demand - supply)) %>%
    summarize(across(c(output, supply, demand, exsup, exdem), sum), .by = "place") %>%
    mutate(
      production_capacity = supply / output,
      trade_capacity = exsup / output,
      retention = 1 - exsup / supply,
      production_dependency = demand / output,
      trade_dependency = exdem / output,
      autonomy = 1 - exdem / demand,
      trade_balance = (exsup - exdem) / output,
      trade_openness = (exsup + exdem) / output
    )
  df
}

