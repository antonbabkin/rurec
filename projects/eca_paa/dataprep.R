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


# paths ----

# many inputs are required indirectly through sourced scripts
ipath <- list(
)

opath <- list(
  geog_ = "data/projects/eca_paa/geog/{year}.rds",
  cbsa_conc_ = "data/projects/eca_paa/cbsa_conc/{year}.rds",
  cbsa_delin_ = "data/projects/eca_paa/cbsa_delin/{year}.rds",
  production_ = "data/projects/eca_paa/production/{bus_data}/{ilevel}/{class_system}/{year}.rds",
  population_ = "data/projects/eca_paa/population/{bus_data}/{year}.rds",
  laborforce_ = "data/projects/eca_paa/laborforce/{bus_data}/{year}.rds",
  employment_ = "data/projects/eca_paa/employment/{bus_data}/{year}.rds",
  income_ = "data/projects/eca_paa/income/{bus_data}/{year}.rds",
  income_rate_ = "data/projects/eca_paa/income_rate/{bus_data}/{year}.rds",
  gdp_ = "data/projects/eca_paa/gdp/{bus_data}/{price_level}/{year}.rds",
  laborforce_rate_ = "data/projects/eca_paa/laborforce_rate/{bus_data}/{year}.rds",
  highschool_attainment_rate_ = "data/projects/eca_paa/highschool_attainment_rate/{bus_data}/{year}.rds",
  poverty_ = "data/projects/eca_paa/poverty/{bus_data}/{year}.rds",
  poverty_rate_ = "data/projects/eca_paa/poverty_rate/{bus_data}/{year}.rds",
  ypll75_ = "data/projects/eca_paa/ypll75/{bus_data}/{year}.rds",
  establishments_ = "data/projects/eca_paa/establishments/{bus_data}/{year}.rds",
  payroll_ = "data/projects/eca_paa/payroll/{bus_data}/{year}.rds",
  entry_ = "data/projects/eca_paa/entry/{bus_data}/{year}.rds",
  exit_ = "data/projects/eca_paa/exit/{bus_data}/{year}.rds",
  entry_rate_ = "data/projects/eca_paa/entry_rate/{bus_data}/{year}.rds",
  exit_rate_ = "data/projects/eca_paa/exit_rate/{bus_data}/{year}.rds",
  eca_df = "data/projects/eca_paa/eca.rds"
)


# Basic utility functions ----

growth_rate <- dataprep_misc$growth_rate


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
                            bus_data = "cbp_imp",
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

# Gross Output ----

call_gross_output <- function(
    year, 
    bus_data = "cbp_imp",
    class_system = "commodity",
    ilevel = "det") {
    df <- call_production(
      year = year, 
      bus_data = bus_data,
      class_system = class_system,
      ilevel = ilevel) %>% 
      select(place, gross_output)
  return(df)
}

# Intermediate Supply ----

call_intermediate_supply <- function(
    year, 
    bus_data = "cbp_imp",
    class_system = "commodity",
    ilevel = "det") {
  df <- call_production(
    year = year, 
    bus_data = bus_data,
    class_system = class_system,
    ilevel = ilevel) %>% 
    select(place, intermediate_supply)
  return(df)
}


# Intermediate Demand ----

call_intermediate_demand <- function(
    year, 
    bus_data = "cbp_imp",
    class_system = "commodity",
    ilevel = "det") {
  df <- call_production(
    year = year, 
    bus_data = bus_data,
    class_system = class_system,
    ilevel = ilevel) %>% 
    select(place, intermediate_demand)
  return(df)
}

# Net Supply ----

call_net_supply <- function(
    year, 
    bus_data = "cbp_imp",
    class_system = "commodity",
    ilevel = "det") {
  df <- call_production(
    year = year, 
    bus_data = bus_data,
    class_system = class_system,
    ilevel = ilevel) %>% 
    select(place, net_supply)
  return(df)
}

# Net Demand ----

call_net_demand <- function(
    year, 
    bus_data = "cbp_imp",
    class_system = "commodity",
    ilevel = "det") {
  df <- call_production(
    year = year, 
    bus_data = bus_data,
    class_system = class_system,
    ilevel = ilevel) %>% 
    select(place, net_demand)
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

# Labor force ----

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

# Jobs ----

call_jobs <- function(year,
                      bus_data = "cbp_imp") {
    df <- call_employment(
      year = year, 
      bus_data = bus_data) %>% 
      rename(jobs = employment)
  return(df)
}


# Income----

call_income <- function(
    year,
    bus_data = "bea_profile") {
  cache_path = glue(opath$income_)
  if (file.exists(cache_path)) {
    df <- readRDS(cache_path)
    log_debug("read from cache {cache_path}")
  } else {
    df <- dataprep_misc$call_county_income(
      year = year, 
      bus_data = bus_data)
    
    saveRDS(df, util$mkdir(cache_path))
    log_debug("save to cache {cache_path}")
  }    
  return(df)
}

# Income per capita ----

call_income_rate <- function(year,
                               bus_data = "bea_profile") {
  cache_path = glue(opath$income_rate_)
  if (file.exists(cache_path)) {
    df <- readRDS(cache_path)
    log_debug("read from cache {cache_path}")
  } else {
    df <- dataprep_misc$call_county_income_rate(
      year = year, 
      bus_data = bus_data)
    
    saveRDS(df, util$mkdir(cache_path))
    log_debug("save to cache {cache_path}")
  }    
  return(df)
}

# GDP ----

call_gdp <- function(
    year,
    bus_data = "bea_rea",
    price_level = "real") {
  cache_path = glue(opath$gdp_)
  if (file.exists(cache_path)) {
    df <- readRDS(cache_path)
    log_debug("read from cache {cache_path}")
  } else {
    df <- dataprep_misc$call_county_gdp(
      year = year, 
      bus_data = bus_data,
      price_level = price_level)
    
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
  stop("Use call_poverty_rate()")
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

# Poverty percent ----

call_poverty_rate <- function(year,
                         bus_data = "saipe") {
  cache_path = glue(opath$poverty_rate_)
  if (file.exists(cache_path)) {
    df <- readRDS(cache_path)
    log_debug("read from cache {cache_path}")
  } else {
    df <- dataprep_misc$call_county_poverty_rate(
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
                                bus_data = "cbp_imp") {
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

# Payroll ----

call_payroll <- function(year,
                         bus_data = "cbp_raw") {
  cache_path = glue(opath$payroll_)
  if (file.exists(cache_path)) {
    df <- readRDS(cache_path)
    log_debug("read from cache {cache_path}")
  } else {
    df <- dataprep_misc$call_county_payroll(
      year = year, 
      bus_data = bus_data)
    
    saveRDS(df, util$mkdir(cache_path))
    log_debug("save to cache {cache_path}")
  }    
  return(df)
}

# Wage ----

call_wage <- function(year,
                      bus_data = "cbp_raw") {
  pay <- call_payroll(year = year, bus_data = bus_data)
  emp <- call_employment(year = year, bus_data = bus_data)
  df <- full_join(pay, emp, "place") |>
    mutate(wage = 1000 * payroll / employment) |>
    mutate(wage = if_else(is.finite(wage), wage, NA)) |>
    select(place, wage) |>
    filter(!is.na(wage))
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


# Unemployment rate ----

call_unemp_rate <- function(year) {
  lf <- call_laborforce(year, bus_data = "ers")
  emp <- call_employment(year, bus_data = "ers")
  stop("FINISH THIS")
  # df <- full_join(pay, emp, "place") |>
  #   mutate(wage = 1000 * payroll / employment) |>
  #   mutate(wage = if_else(is.finite(wage), wage, NA)) |>
  #   select(place, wage) |>
  #   filter(!is.na(wage))
  # return(df)  
  
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






# RUCC  ----


