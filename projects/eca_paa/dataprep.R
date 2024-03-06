# Data preparation


# libraries and sources ----

library(tidyverse)
library(logger)
library(glue)
library(readxl)
log_threshold(DEBUG)

source("R/basic_utilities.R", local = (util <- new.env()))
source("R/geography.R", local = (geography <- new.env()))
source("R/place_output.R", local = (place_output <- new.env()))
source("R/trade_flows.R", local = (trade_flows <- new.env()))
source("R/connectedness.R", local = (connectedness <- new.env()))
source("R/dataprep.R", local = (dataprep_misc <- new.env()))
source("R/dataprep_prosperity.R", local = (prosperity <- new.env()))



# paths ----

# many inputs are required indirectly through sourced scripts
ipath <- list(
  RUCC = "https://www.ers.usda.gov/webdocs/DataFiles/53251/ruralurbancodes2013.xls?v=8014.1"
)

opath <- list(
  geog_ = "data/projects/eca_paa/geog/{year}.rds",
  cbsa_conc_ = "data/projects/eca_paa/cbsa_conc/{year}.rds",
  cbsa_delin_ = "data/projects/eca_paa/cbsa_delin/{year}.rds",
  production_ = "data/projects/eca_paa/production/{bus_data}/{ilevel}/{class_system}/{year}.rds",
  population_ = "data/projects/eca_paa/population/{bus_data}/{year}.rds",
  laborforce_ = "data/projects/eca_paa/laborforce/{bus_data}/{year}.rds",
  employment_ = "data/projects/eca_paa/employment/{bus_data}/{year}.rds",
  laborforce_rate_ = "data/projects/eca_paa/laborforce_rate/{bus_data}/{year}.rds",
  highschool_attainment_rate_ = "data/projects/eca_paa/highschool_attainment_rate/{bus_data}/{year}.rds",
  poverty_ = "data/projects/eca_paa/poverty/{bus_data}/{year}.rds",
  ypll75_ = "data/projects/eca_paa/ypll75/{bus_data}/{year}.rds",
  establishments_ = "data/projects/eca_paa/establishments/{bus_data}/{year}.rds",
  entry_ = "data/projects/eca_paa/entry/{bus_data}/{year}.rds",
  exit_ = "data/projects/eca_paa/exit/{bus_data}/{year}.rds",
  entry_rate_ = "data/projects/eca_paa/entry_rate/{bus_data}/{year}.rds",
  exit_rate_ = "data/projects/eca_paa/exit_rate/{bus_data}/{year}.rds",
  county_shapes = "data/projects/eca_paa/county_shapes.rds",
  cbsa_delin = "data/projects/eca_paa/cbsa_delin.rds",
  eca_df = "data/projects/eca_paa/eca.rds",
  circ = "data/projects/eca_paa/dataset_circularity.rds",
  econ_dynam_ = "data/projects/eca_paa/econ_dynam_{year}.rds",
  unemp_rate = "data/projects/eca_paa/unemp_rate.rds",
  netmigration = "data/projects/eca_paa/netmigration.rds",
  education = "data/projects/eca_paa/education.rds",
  chrr = "data/projects/eca_paa/chrr.rds",
  lfpr = "data/projects/eca_paa/lfpr.rds",
  saipe = "data/projects/eca_paa/saipe_2004-2022.rds",
  RUCC = "data/projects/eca_paa/rucc2012.xlsx"
)


# Geog ----

call_geog <- function(year) {
  cache_path = glue(opath$geog_)
  if (file.exists(cache_path)) {
    df <- readRDS(cache_path)
    log_debug("read from cache {cache_path}")
  } else {
    df <- geography$call_geog(year) 
    saveRDS(df, util$mkdir(cache_path))
    log_debug("save to cache {cache_path}")
  }    
  return(df)
}

# CBSA concordance ----

call_cbsa_conc <- function(year) {
  cache_path = glue(opath$cbsa_conc_)
  if (file.exists(cache_path)) {
    df <- readRDS(cache_path)
    log_debug("read from cache {cache_path}")
  } else {
    df <- geography$call_cbsa_concord(year) 
    saveRDS(df, util$mkdir(cache_path))
    log_debug("save to cache {cache_path}")
  }    
  return(df)
}

# CBSA delineation ----

call_cbsa_delin_df <- function(year) {
  year = util$year2cbsa(year)
  cache_path = glue(opath$cbsa_delin_)
  if (file.exists(cache_path)) {
    df <- readRDS(cache_path)
    log_debug("read from cache {cache_path}")
  } else {
    df <- geography$pubdata$get_cbsa_delin_df(year) %>% 
      mutate(place = {paste0(.$STATE_CODE, .$COUNTY_CODE)}) %>% 
      {.[c("place", "METRO_MICRO", "CENTRAL_OUTLYING")]} 
    saveRDS(df, util$mkdir(cache_path))
    log_debug("save to cache {cache_path}")
  }    
  return(df)
}

# Production ----

call_production <- function(year, 
                            bus_data = "infogroup",
                            class_system = "commodity",
                            ilevel = "det") {
  cache_path = glue(opath$production_)
  if (file.exists(cache_path)) {
    df <- readRDS(cache_path)
    log_debug("read from cache {cache_path}")
  } else {
    df <- place_output$call_extraction_table(
      year = year,
      bus_data = bus_data,
      class_system = class_system,
      ilevel = ilevel,
      spatial = F) %>% 
      select(-extract)
    
    saveRDS(df, util$mkdir(cache_path))
    log_debug("save to cache {cache_path}")
  }    
  return(df)
}


# Population ----

call_population <- function(year, 
                            bus_data = "tidy_acs") {
  cache_path = glue(opath$population_)
  if (file.exists(cache_path)) {
    df <- readRDS(cache_path)
    log_debug("read from cache {cache_path}")
  } else {
    df <- dataprep_misc$call_county_population(
      year = year, 
      bus_data = bus_data)
    
    saveRDS(df, util$mkdir(cache_path))
    log_debug("save to cache {cache_path}")
  }    
  return(df)
}

# Laborforce ----

call_laborforce <- function(year, 
                            bus_data = "ers") {
  cache_path = glue(opath$laborforce_)
  if (file.exists(cache_path)) {
    df <- readRDS(cache_path)
    log_debug("read from cache {cache_path}")
  } else {
    df <- dataprep_misc$call_county_laborforce(
      year = year, 
      bus_data = bus_data)
    
    saveRDS(df, util$mkdir(cache_path))
    log_debug("save to cache {cache_path}")
  }    
  return(df)
}

# Employment ----

call_employment <- function(year, 
                            bus_data = "ers") {
  cache_path = glue(opath$employment_)
  if (file.exists(cache_path)) {
    df <- readRDS(cache_path)
    log_debug("read from cache {cache_path}")
  } else {
    df <- dataprep_misc$call_county_employment(
      year = year, 
      bus_data = bus_data)
    
    saveRDS(df, util$mkdir(cache_path))
    log_debug("save to cache {cache_path}")
  }    
  return(df)
}

# Labor Force rate ----

call_laborforce_rate <- function(year,
                                 bus_data = "tidy_acs") {
  cache_path = glue(opath$laborforce_rate_)
  if (file.exists(cache_path)) {
    df <- readRDS(cache_path)
    log_debug("read from cache {cache_path}")
  } else {
    df <- dataprep_misc$call_county_laborforce_rate(
      year = year, 
      bus_data = bus_data)
    
    saveRDS(df, util$mkdir(cache_path))
    log_debug("save to cache {cache_path}")
  }    
  return(df)
}


# Highschool Attainment rate ----

call_highschool_attainment_rate <- function(year,
                                            bus_data = "tidy_acs") {
  cache_path = glue(opath$highschool_attainment_rate_)
  if (file.exists(cache_path)) {
    df <- readRDS(cache_path)
    log_debug("read from cache {cache_path}")
  } else {
    df <- dataprep_misc$call_county_highschool_attainment_rate(
      year = year, 
      bus_data = bus_data)
    
    saveRDS(df, util$mkdir(cache_path))
    log_debug("save to cache {cache_path}")
  }    
  return(df)
}

# Poverty ----

call_poverty <- function(year,
                         bus_data = "saipe") {
  cache_path = glue(opath$poverty_)
  if (file.exists(cache_path)) {
    df <- readRDS(cache_path)
    log_debug("read from cache {cache_path}")
  } else {
    df <- dataprep_misc$call_county_poverty(
      year = year, 
      bus_data = bus_data)
    
    saveRDS(df, util$mkdir(cache_path))
    log_debug("save to cache {cache_path}")
  }    
  return(df)
}

# Years of Potential Life Lost (YPLL75) ----

call_ypll75 <- function(year,
                        bus_data = "chr") {
  cache_path = glue(opath$ypll75_)
  if (file.exists(cache_path)) {
    df <- readRDS(cache_path)
    log_debug("read from cache {cache_path}")
  } else {
    df <- dataprep_misc$call_county_ypll75(
      year = year, 
      bus_data = bus_data)
    
    saveRDS(df, util$mkdir(cache_path))
    log_debug("save to cache {cache_path}")
  }    
  return(df)
}

# Establishments ----

call_establishments <- function(year,
                                bus_data = "infogroup") {
  cache_path = glue(opath$establishments_)
  if (file.exists(cache_path)) {
    df <- readRDS(cache_path)
    log_debug("read from cache {cache_path}")
  } else {
    df <- dataprep_misc$call_county_establishments(
      year = year, 
      bus_data = bus_data)
    
    saveRDS(df, util$mkdir(cache_path))
    log_debug("save to cache {cache_path}")
  }    
  return(df)
}

# Entry ----

call_entry <- function(year,
                                bus_data = "infogroup") {
  cache_path = glue(opath$entry_)
  if (file.exists(cache_path)) {
    df <- readRDS(cache_path)
    log_debug("read from cache {cache_path}")
  } else {
    df <- dataprep_misc$call_county_entry(
      year = year, 
      bus_data = bus_data)
    
    saveRDS(df, util$mkdir(cache_path))
    log_debug("save to cache {cache_path}")
  }    
  return(df)
}

# Exit ----

call_exit <- function(year,
                      bus_data = "infogroup") {
  cache_path = glue(opath$exit_)
  if (file.exists(cache_path)) {
    df <- readRDS(cache_path)
    log_debug("read from cache {cache_path}")
  } else {
    df <- dataprep_misc$call_county_exit(
      year = year, 
      bus_data = bus_data)
    
    saveRDS(df, util$mkdir(cache_path))
    log_debug("save to cache {cache_path}")
  }    
  return(df)
}

# Entry rate ----

call_entry_rate <- function(year,
                            bus_data = "infogroup") {
  cache_path = glue(opath$entry_rate_)
  if (file.exists(cache_path)) {
    df <- readRDS(cache_path)
    log_debug("read from cache {cache_path}")
  } else {
    df <- dataprep_misc$call_county_entry_rate(
      year = year, 
      bus_data = bus_data)
    
    saveRDS(df, util$mkdir(cache_path))
    log_debug("save to cache {cache_path}")
  }    
  return(df)
}

# Exit rate ----

call_exit_rate <- function(year,
                           bus_data = "infogroup") {
  cache_path = glue(opath$exit_rate_)
  if (file.exists(cache_path)) {
    df <- readRDS(cache_path)
    log_debug("read from cache {cache_path}")
  } else {
    df <- dataprep_misc$call_county_exit_rate(
      year = year, 
      bus_data = bus_data)
    
    saveRDS(df, util$mkdir(cache_path))
    log_debug("save to cache {cache_path}")
  }    
  return(df)
}



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
  cache_path = opath$eca_df
  if (file.exists(cache_path)) {
    df <- readRDS(cache_path)
    log_debug("read from cache {cache_path}")
  } else {
    tf <- trade_flows$call_trade_flows("all_industries")
    tf_norm <- sweep(tf, 1, rowSums(tf), "/")
    tf_norm[is.na(tf_norm)] <- 0
    conn_metrics <- connectedness$apply_absorption_metrics(tf_norm)
    df <- connectedness$apply_absorption_algorithm(conn_metrics, threshold = 0) |>
      select(place, cluster_category, eca_membership, max_alpha, match) |>
      rename(fips = place, eca_cluster_category = cluster_category, max_trade_share = max_alpha, max_trade_place = match)
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

# TODO: fix this to read correctly the first time 
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


# RUCC  ----
call_RUCC <- function() {
  raw_path <- opath$RUCC
  
  # download raw data if needed
  if (file.exists(raw_path)) {
    log_debug("raw data found at {raw_path}")
  } else {
    # create parent directories
    raw_path <- util$mkdir(raw_path)
    download_status <- download.file(url = ipath$RUCC, destfile = raw_path, mode = "wb")
    stopifnot(download_status == 0)
    log_debug("raw data dowloaded to {raw_path}")
  }
  
  df <- read_xls(raw_path)
  
  return(df)
}

