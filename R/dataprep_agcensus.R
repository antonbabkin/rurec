# Data preparation of Census of Agriculture

# R libraries ----
library(logger)
library(arrow)
library(tidyverse)
library(glue)


# R scripts ----
source("R/basic_utilities.R", local = (util <- new.env()))


# Python modules ----
pymod <- new.env()
pymod$initialized <- FALSE

#' Initialize environment with necessary Python modules imported through reticulate
#' Running multiple times is safe and only imports once.
pymod$init <- function() {
  if (pymod$initialized) return()
  library(reticulate)
  use_condaenv("rurec")
  pymod$ag_output <- import("rurec.ag_output")
  pymod$initialized <- TRUE
}


# Data objects ----
ipath <- list(
  agcensus_ = "data/pubdata/agcensus/{year}/part.pq"
)

opath <- list(
  farm_sales_bea_ = "data/agcensus/farm_sales_bea/{geo_level}_{year}.rds"
)

clear_outputs <- function() {
  util$clear_paths(opath)
}


# Python caching wrapper ----

get_farm_sales_by_bea_detail <- function(year, geo_level = c("county", "state", "national")) {
  geo_level <- match.arg(geo_level)
  year <- as.integer(year)
  
  p <- glue(opath$farm_sales_bea_)
  if (file.exists(p)) {
    log_debug(paste("read from cache", p))
    return(readRDS(p))
  }
  
  pymod$init()
  x <- pymod$ag_output$get_farm_sales_by_bea_detail(year, geo_level)
    # reticulate_unlist_cols()
  
  log_debug(paste("save to cache", p))
  saveRDS(x, util$mkdir(p))
  return(x)
}

# R funcs ----

##Note presence of county-level suppression too. Can we fix/lessen by way of imputation?
##Note county totals of 111900 and 112A00 are 50% larger than their national level counterparts
# Call up and clean Ag Output data ($1,000 of dollars)
call_agoutput <- function(year, geo_level = c("county", "state", "national")) {
  geo_level <- match.arg(geo_level)
  ag_year <- util$year2agcensus(year)
  df <- get_farm_sales_by_bea_detail(ag_year, geo_level) %>% as.data.frame()
  place <- c(place = rownames(df))
  df <- sapply(df, \(x) x / 1000) %>% as.data.frame()
  if (geo_level == "county" | geo_level == "state") {
    df <- cbind(place, df)
    rownames(df) <- 1:nrow(df)
  } else {
    df <- t(df)
  }
  return(df)
}

# Tests ----

test_all <- function() {
  for (y in c(2002, 2007, 2012, 2017)) {
    for (g in c("county", "state", "national")) {
      get_farm_sales_by_bea_detail(y, g)
      call_agoutput(y, g)
    }
  }
}
  

