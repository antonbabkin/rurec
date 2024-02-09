# Indicators of prosperity



# R packages ----
library(logger)
log_threshold(DEBUG)
library(tidyverse)
library(readxl)


# R scripts ----
source("R/basic_utilities.R", local = (util <- new.env()))



# Data paths ----
# inputs
ipath <- list(
  population_county_2022 = "https://www2.census.gov/programs-surveys/popest/tables/2020-2022/counties/totals/co-est2022-pop.xlsx",
  unemployment_rate = "https://www.ers.usda.gov/webdocs/DataFiles/48747/Unemployment.xlsx?v=8720.6", 
  netmigration_rate = "https://netmigration.wisc.edu/documents/NME_1020_data_beta.zip"
)

# outputs
opath <- list(
  population_county_2022_raw = "data/population/raw/co-est2022-pop.xlsx",
  unemployment_rate_raw = "data/prosperity/raw/unemployment.xlsx", 
  netmigration_rate_raw = "data/prosperity/raw/NME_1020_data_beta.zip"
)



# Population ----
# population is used as an example here
# we actually already have it in pubdata

call_population_2022_df <- function() {
  raw_path <- opath$population_county_2022_raw
  
  # download raw data if needed
  if (file.exists(raw_path)) {
    log_debug("raw data found at {raw_path}")
  } else {
    # create parent directories
    raw_path <- util$mkdir(raw_path)
    download_status <- download.file(url = ipath$population_county_2022, destfile = raw_path, mode = "wb")
    stopifnot(download_status == 0)
    log_debug("raw data dowloaded to {raw_path}")
  }

  df <- read_xlsx(raw_path,
                   range = "A5:E3149",
                   col_names = c("place", "2020-04-01", "2020-07-01", "2021-07-01", "2022-07-01")) |>
    pivot_longer(!place, names_to = "date", values_to = "population") |>
    mutate(date = ymd(date), year = year(date))

  return(df)
}


call_unemployment_rate_df <- function() {
  raw_path <- opath$unemployment_rate_raw
  
  # download raw data if needed
  if (file.exists(raw_path)) {
    log_debug("raw data found at {raw_path}")
  } else {
    # create parent directories
    raw_path <- util$mkdir(raw_path)
    download_status <- download.file(url = ipath$unemployment_rate, destfile = raw_path, mode = "wb")
    stopifnot(download_status == 0)
    log_debug("raw data dowloaded to {raw_path}")
  }
  
  df <- read_xlsx(raw_path, , 
                  skip = 4) %>%
    mutate(st = substr(FIPS_Code,1,2), cty = substr(FIPS_Code,3,5)) %>%
    select(st, cty, starts_with("Unemployment_rate")) %>%
    pivot_longer(!c("st","cty"), names_to = "year", values_to = "unemp_rt") %>%
    mutate(year = substr(year,19,22), unemp_rt = unemp_rt / 100)
  
  return(df)
}


call_netmigration_df <- function() {
  raw_path <- opath$netmigration_rate_raw
  
  # download raw data if needed
  if (file.exists(raw_path)) {
    log_debug("raw data found at {raw_path}")
  } else {
    # create parent directories
    raw_path <- util$mkdir(raw_path)
    download_status <- download.file(url = ipath$netmigration_rate, destfile = raw_path, mode = "wb")
    stopifnot(download_status == 0)
    log_debug("raw data dowloaded to {raw_path}")
  }
  
  
  df <- read_csv(raw_path)
  

  return(df)
}

