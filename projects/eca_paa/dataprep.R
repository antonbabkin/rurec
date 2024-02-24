# Data preparation


# libraries and sources ----

library(tidyverse)
library(logger)
library(glue)
log_threshold(DEBUG)

source("R/basic_utilities.R", local = (util <- new.env()))
source("R/geography.R", local = (geography <- new.env()))
source("R/trade_flows.R", local = (trade_flows <- new.env()))
source("R/connectedness.R", local = (connectedness <- new.env()))
source("R/dataprep.R", local = (dataprep_misc <- new.env()))
source("R/dataprep_prosperity.R", local = (prosperity <- new.env()))


# paths ----

# many inputs are required indirectly through sourced scripts
ipath <- list(

)

opath <- list(
  county_shapes = "data/projects/eca_paa/county_shapes.rds",
  cbsa_delin = "data/projects/eca_paa/cbsa_delin.rds",
  eca = "data/projects/eca_paa/eca.rds",
  circ = "data/projects/eca_paa/dataset_circularity.rds",
  econ_dynam_ = "data/projects/eca_paa/econ_dynam_{year}.rds",
  unemp_rate = "data/projects/eca_paa/unemp_rate.rds",
  netmigration = "data/projects/eca_paa/netmigration.rds",
  education = "data/projects/eca_paa/education.rds",
  chrr = "data/projects/eca_paa/chrr.rds",
  lfpr = "data/projects/eca_paa/lfpr.rds",
  saipe = "data/projects/eca_paa/saipe_2004-2022.rds"
)

# County shapes ----

call_county_shapes <- function() {
  cache_path = opath$county_shapes
  if (file.exists(cache_path)) {
    df <- readRDS(cache_path)
    log_debug("read from cache {cache_path}")
  } else {
    df <- geography$call_geog(2013) |>
      rename_with(str_to_lower) |>
      rename(fips = place)
    d <- geography$pubdata$get_state_df(FALSE) |>
      rename_with(str_to_lower) |>
      select(code, contiguous, bea_region_name) |>
      rename(state_code = code)
    df <- left_join(df, d, "state_code")
    saveRDS(df, util$mkdir(cache_path))
    log_debug("save to cache {cache_path}")
  }    
  df
}

# CBSA ----

call_cbsa_delin <- function() {
  cache_path = opath$cbsa_delin
  if (file.exists(cache_path)) {
    df <- readRDS(cache_path)
    log_debug("read from cache {cache_path}")
  } else {
    df <- geography$pubdata$get_cbsa_delin_df(2013) |>
      rename_with(str_to_lower) |>
      mutate(fips = paste0(state_code, county_code), .before = 1)
    saveRDS(df, util$mkdir(cache_path))
    log_debug("save to cache {cache_path}")
  }    
  df
}

# ECA ----

#' County classification by ECA
call_eca_df <- function() {
  cache_path = opath$eca
  if (file.exists(cache_path)) {
    df <- readRDS(cache_path)
    log_debug("read from cache {cache_path}")
  } else {
    tf <- trade_flows$call_trade_flows("all_industries")
    tf_norm <- sweep(tf, 1, rowSums(tf), "/")
    tf_norm[is.na(tf_norm)] <- 0
    conn_metrics <- connectedness$apply_absorption_metrics(tf_norm)
    df <- connectedness$apply_absorption_algorithm(conn_metrics, threshold = 0) |>
      select(place, cluster_category, eca_membership, max_alpha) |>
      rename(fips = place, eca_cluster_category = cluster_category, max_trade_share = max_alpha)
    saveRDS(df, util$mkdir(cache_path))
    log_debug("save to cache {cache_path}")
  }
  df
}


# Econ dynamism ----

call_econ_dynam <- function(year) {
  cache_path <- glue(opath$econ_dynam_)
  if (file.exists(cache_path)) {
    df <- readRDS(cache_path)
    log_debug("read from cache {cache_path}")
  } else {
    df <- dataprep_misc$call_econ_dynam_ind(year) |>
      rename(fips = place)
    saveRDS(df, util$mkdir(cache_path))
    log_debug("save to cache {cache_path}")
  }    
  df
}

# Unemployment rate ----

call_unemp_rate <- function() {
  cache_path <- opath$unemp_rate
  if (file.exists(cache_path)) {
    df <- readRDS(cache_path)
    log_debug("read from cache {cache_path}")
  } else {
    df <- prosperity$call_unemployment_rate_df() |>
      mutate(fips = paste0(st, cty), .before = 1)
    saveRDS(df, util$mkdir(cache_path))
    log_debug("save to cache {cache_path}")
  }    
  df
}  


# Net migration ----

call_netmigration <- function() {
  cache_path <- opath$netmigration
  if (file.exists(cache_path)) {
    df <- readRDS(cache_path)
    log_debug("read from cache {cache_path}")
  } else {
    df <- prosperity$call_netmigration_df()
    saveRDS(df, util$mkdir(cache_path))
    log_debug("save to cache {cache_path}")
  }    
  df
}



# Education ----

call_education <- function() {
  cache_path <- opath$education
  if (file.exists(cache_path)) {
    df <- readRDS(cache_path)
    log_debug("read from cache {cache_path}")
  } else {
    df <- prosperity$call_education_df()
    saveRDS(df, util$mkdir(cache_path))
    log_debug("save to cache {cache_path}")
  }    
  df
}


# CHRR ----

call_chrr <- function() {
  cache_path <- opath$chrr
  if (file.exists(cache_path)) {
    df <- readRDS(cache_path)
    log_debug("read from cache {cache_path}")
  } else {
    df <- prosperity$call_CHRR_df()
    saveRDS(df, util$mkdir(cache_path))
    log_debug("save to cache {cache_path}")
  }    
  df
}


# LFPR ----

call_lfpr <- function() {
  cache_path <- opath$lfpr
  if (file.exists(cache_path)) {
    df <- readRDS(cache_path)
    log_debug("read from cache {cache_path}")
  } else {
    df <- prosperity$call_lfpr_df()
    saveRDS(df, util$mkdir(cache_path))
    log_debug("save to cache {cache_path}")
  }    
  df
}


# SAIPE ----


call_saipe <- function() {
  cache_path <- glue(opath$saipe)
  if (file.exists(cache_path)) {
    df <- readRDS(cache_path)
    log_debug("read from cache {cache_path}")
  } else {
    df <- prosperity$call_saipe_df() |>
      mutate(fips = paste0(state.fips, county.fips), .before = 1)
    saveRDS(df, util$mkdir(cache_path))
    log_debug("save to cache {cache_path}")
  }    
  df
}


# circularity  ----

#' NOTE: if first time accessing circularity RDS, ipath in "else" clause is reading in from unzipped file
call_circ_df <- function(x) {
  cache_path = opath$circ
  if (file.exists(cache_path)) {
    df <- readRDS(cache_path)
    log_debug("read from cache {cache_path}")
  } else {
    df <- readRDS("data/tmp/dataset_circularity_v240223/datasets/circularity/circularity.rds")
  }
  df %>%
    filter(year == x)
}
