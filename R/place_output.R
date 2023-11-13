# This script calculates output by industry/commodity at local place level (county or CBSA)



# R libraries ----
library(logger)
log_threshold(DEBUG)
library(arrow)
library(tidyverse)
library(glue)


# R scripts ----
source("R/basic_utilities.R", local = (util <- new.env()))
source("R/dataprep_bea_io.R", local = (bea_io <- new.env()))
source("R/dataprep_cbp.R", local = (cbp <- new.env()))
source("R/dataprep_infogroup.R", local = (ig <- new.env()))
source("R/dataprep_agcensus.R", local = (agcen <- new.env()))




# Data objects ----
ipath <- list(
  ig_ = ig$opath$county_,
  cbp_ = cbp$opath$cbp_
  # data dependencies
)

opath <- list(
  output_ = "data/place_activity/output_{year}_{class_system}_{ilevel}_{bus_data}.pq"
)

clear_outputs <- function() {
  util$clear_paths(opath)
}


# Output ----



#
call_output <- function(year, 
                        class_system = c("industry", "commodity"), 
                        ilevel = c("det", "sum", "sec"),
                        bus_data = c("cbp_raw", "cbp_imp", "infogroup")) {
  class_system <- match.arg(class_system)
  ilevel <- match.arg(ilevel)
  bus_data <- match.arg(bus_data)
  
  cache_path <- glue(opath$output_)
  if (file.exists(cache_path)) {
    log_debug(paste("read from cache", cache_path))
    return(read_parquet(cache_path))
  }
  
  # class_system == "industry"
  # class_system == "commodity
  
  # ilevel == "det"
  # ilevel == "sum"
  # ilevel == "sec"
  
  # bus_data == "cbp_raw"
  # bus_data == "cbp_imp"
  # bus_data == "infogroup"
  
  
  
  log_debug(paste("save to cache", cache_path))
  write_parquet(df, util$mkdir(cache_path))
  return(df)  
}
