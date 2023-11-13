# Prepare county aggregates from InfoGroup



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
  raw_csv_ = "~/work/data/infogroup/{year}_Business_Academic_QCQ.csv.gz"
)

opath <- list(
  raw_pq_dir = "data/infogroup/raw_pq/",
  raw_pq_ = "data/infogroup/raw_pq/{year}/part.pq",
  enh_pq_dir = "data/infogroup/enh_pq/",
  enh_pq_ = "data/infogroup/enh_pq/{year}/part.pq",
  county_ = "data/infogroup/county_agg/{year}.pq"
)

clear_outputs <- function() {
  util$clear_paths(opath)
}


# Convert to parquet ----

## raw ----
# convert raw CSV to pq with minimal modification

get_raw_df <- function(year) {
  
  cache_path <- glue(opath$raw_pq_)
  if (file.exists(cache_path)) {
    log_debug(paste("read from cache", cache_path))
    return(read_parquet(cache_path))
  }
  
  state_codes <-
    c(
      "AL" ~ "01",
      "AK" ~ "02",
      "AZ" ~ "04",
      "AR" ~ "05",
      "CA" ~ "06",
      "CO" ~ "08",
      "CT" ~ "09",
      "DE" ~ "10",
      "DC" ~ "11",
      "FL" ~ "12",
      "GA" ~ "13",
      "HI" ~ "15",
      "ID" ~ "16",
      "IL" ~ "17",
      "IN" ~ "18",
      "IA" ~ "19",
      "KS" ~ "20",
      "KY" ~ "21",
      "LA" ~ "22",
      "ME" ~ "23",
      "MD" ~ "24",
      "MA" ~ "25",
      "MI" ~ "26",
      "MN" ~ "27",
      "MS" ~ "28",
      "MO" ~ "29",
      "MT" ~ "30",
      "NE" ~ "31",
      "NV" ~ "32",
      "NH" ~ "33",
      "NJ" ~ "34",
      "NM" ~ "35",
      "NY" ~ "36",
      "NC" ~ "37",
      "ND" ~ "38",
      "OH" ~ "39",
      "OK" ~ "40",
      "OR" ~ "41",
      "PA" ~ "42",
      "RI" ~ "44",
      "SC" ~ "45",
      "SD" ~ "46",
      "TN" ~ "47",
      "TX" ~ "48",
      "UT" ~ "49",
      "VT" ~ "50",
      "VA" ~ "51",
      "WA" ~ "53",
      "WV" ~ "54",
      "WI" ~ "55",
      "WY" ~ "56",
      "AS" ~ "60",
      "GU" ~ "66",
      "MP" ~ "69",
      "PR" ~ "72",
      "VI" ~ "78"
    )
  
  d <- read_csv(
    glue(ipath$raw_csv_),
    col_types = cols_only(
      ABI = "i",
      `State` = "c",
      `County Code` = "c", 
      `Primary NAICS Code` = "c", 
      `Employee Size (5) - Location` = "i", 
      `Sales Volume (9) - Location` = "i"
    )
  ) %>%
    rename(
      abi = ABI,
      st = `State`,
      cty = `County Code`, 
      naics = `Primary NAICS Code`, 
      emp = `Employee Size (5) - Location`, 
      sales = `Sales Volume (9) - Location`
    ) %>%
    mutate(naics = str_sub(naics, 1, 6)) %>%
    mutate(st = case_match(st, !!!state_codes)) %>%
    replace_na(list(emp = 0, sales = 0))
  
  log_debug(paste("save to cache", cache_path))
  write_parquet(d, util$mkdir(cache_path))
  return(d)
}


prep_raw_all_years <- function() {
  log_tictoc("start")
  for (y in 1997:2017) {
    get_raw_df(y)
    log_tictoc("finish ", y)
  }
}




## enhanced ----
# add entry and exit


prep_enh <- function() {
  
  log_tictoc("start")
  year_first <- 1997
  year_last <- 2017
  for (year in year_first:year_last) {
    if (year == year_first) {
      d0 <- get_raw_df(year) %>% mutate(entry = NA)
    } else {
      d0 <- d1 %>% mutate(entry = !(abi %in% d0$abi))
    }
    
    if (year == year_last) {
      d0 <- d0 %>% mutate(exit = NA)
    } else {
      d1 <- get_raw_df(year + 1)
      d0 <- d0 %>% mutate(exit = !(abi %in% d1$abi))
    }
    
    p <- glue(opath$enh_pq_)
    log_debug(paste("save to", p))
    write_parquet(d0, util$mkdir(p))
    log_tictoc("finish ", year)
  }

}


# County aggregation ----

#' Aggregate employment and sales by county-NAICS
prep_county_agg <- function(years) {
  ds <- open_dataset(opath$enh_pq_dir, partitioning = "year")
  for (year in years) {
    d <- ds %>% 
      filter(year == !!year) %>%
      group_by(st, cty, naics) %>%
      summarize(sales = sum(sales), emp = sum(emp), .groups = "drop") %>%
      collect()
    p <- glue(opath$county_)
    write_parquet(d, util$mkdir(p))
    log_debug(glue("County agg {year} saved to {p}"))
  }
}

