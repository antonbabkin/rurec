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
  population_county_2022 = "https://www2.census.gov/programs-surveys/popest/tables/2020-2022/counties/totals/co-est2022-pop.xlsx"
)

# outputs
opath <- list(
  population_county_2022_raw = "data/population/raw/co-est2022-pop.xlsx"
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
    download_status <- download.file(url = ipath$population_county_2022, destfile = raw_path)
    stopifnot(download_status == 0)
    log_debug("raw data dowloaded to {raw_path}")
  }
  
  df <- read_excel(raw_path,
                   range = "A5:E3149",
                   col_names = c("place", "2020-04-01", "2020-07-01", "2021-07-01", "2022-07-01")) |>
    pivot_longer(!place, names_to = "date", values_to = "population") |>
    mutate(date = ymd(date), year = year(date))
  
  return(df)
}
