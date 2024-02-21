# Data preparation

# R libraries ----
library(logger)
log_threshold(DEBUG)
library(arrow)
library(tidyverse)
library(glue)


# R scripts ----
source("R/basic_utilities.R", local = (util <- new.env()))
source("R/dataprep_cbp.R", local = (cbp <- new.env()))
source("R/dataprep_infogroup.R", local = (ig <- new.env()))


# Data objects ----
ipath <- list(
  ig_ = ig$opath$county_,
  bea_econ_profile = "https://apps.bea.gov/regional/zip/CAINC30.zip"
)

opath <- list(
  population = "data/pubdata/population/population.pq",
  bea_econ_profile_raw = "data/bea/raw/CAINC30__ALL_AREAS_1969_2022.csv",
  bea_econ_profile = "data/bea/CAINC30__ALL_AREAS_1969_2022.pq"
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


# Surveys ----

# TODO: add Quarterly Census of Employment and Wages, American Community Survey, Current Population Survey, National Agricultural Workers Survey, Agricultural Resource Management Survey, Census of Agriculture, Farm Labor Survey
# e.g., "https://data.bls.gov/cew/data/files/2023/xls/2023_all_county_high_level.zip"

call_bea_econ_profile_raw <- function() {
  raw_path <- opath$bea_econ_profile_raw
  # download raw data if needed
  if (file.exists(raw_path)) {
    log_debug("raw data found at {raw_path}")
  } else {
    # create parent directories
    parent_path <- util$mkdir(file.path(dirname(raw_path), basename(ipath$bea_econ_profile)))
    download_status <- download.file(url = ipath$bea_econ_profile, 
                                     destfile = parent_path, 
                                     mode = "wb")
    stopifnot(download_status == 0)
    log_debug("zip data dowloaded to {parent_path}")
    df <- unzip(parent_path,
                files = basename(raw_path),
                exdir = dirname(raw_path))
  }
  df <- read.csv(raw_path)
  return(df)
}

call_bea_econ_profile <- function() {
  cache_path <- glue(opath$bea_econ_profile)
  if (file.exists(cache_path)) {
    log_debug(paste("read from cache", cache_path))
    return(read_parquet(cache_path))
  }
  df <- call_bea_econ_profile_raw()
  log_debug(paste("save to cache", cache_path))
  write_parquet(df, util$mkdir(cache_path))
  return(df)  
}

# Population ----

call_population <- function() {
  cache_path <- opath$population
  if (!file.exists(cache_path)) {
    pubdata$prep_population()
  }
  return(read_parquet(cache_path))
}

call_census_county_population <- function(year) {
  df <- call_population() %>% 
    mutate(place = {paste0(.$st, .$cty)}) %>% 
    {.[.$year == year, ]} %>% 
    {.[!grepl('(0)$', .$place), ]} %>% 
    {.[c("place", "pop")]} %>% 
    `colnames<-`(c("place", "population"))
  return(df)
}

call_bea_county_population <- function(year) {
  year <- util$year2bea_profile(year)
  df <- call_bea_econ_profile() %>%
    {.[.$LineCode == 100, ]} %>%
    {.[!grepl('(0)$', .$GeoFIPS), ]} %>%
    {.[c("GeoFIPS", paste0("X", year))]} %>%
    `colnames<-`(c("place", "population")) %>%
    `rownames<-`({1:nrow(.)})
return(df)
}

call_county_population <- function(
    year,
    bus_data = c("census", "bea_profile")
){
  bus_data <- match.arg(bus_data)
  if(bus_data == "census"){
    df <- call_census_county_population(year)
  }
  if(bus_data == "bea_profile"){
    df <- call_bea_county_population(year)
  }
  return(df)
}

# Employment ----

call_bea_county_employment <- function(year) {
  year <- util$year2bea_profile(year)
  df <- call_bea_econ_profile() %>%
    {.[.$LineCode == 240, ]} %>%
    {.[!grepl('(0)$', .$GeoFIPS), ]} %>%
    {.[c("GeoFIPS", paste0("X", year))]} %>%
    `colnames<-`(c("place", "employment")) %>%
    `rownames<-`({1:nrow(.)})
  return(df)
}

call_cbp_county_employment <- function(year) {
  df <- cbp$call_cbp(
    year = year,
    cbp_scale = "county",
    imputed = FALSE) %>% 
    {.[.$naics == "", ]} %>% 
    {.[c("place", "emp")]} %>% 
    `colnames<-`(c("place", "employment"))
  return(df)
}

call_cbp_county_employment_efcy <- function(year) {
  df <- cbp$call_cbp(
    year = year,
    cbp_scale = "county",
    imputed = TRUE) %>% 
    {.[.$naics == "", ]} %>% 
    {.[c("place", "emp")]} %>% 
    `colnames<-`(c("place", "employment"))
  return(df)
}

call_infogroup_county_employment <- function(year) {
  df <- glue(ig$opath$county_, .envir = list(year = year)) %>%
    open_dataset() %>%
    collect() %>% 
    na.omit() %>% 
    arrange(st, cty) %>% 
    mutate(place = {paste0(.$st, .$cty)}) %>% 
    {.[c("place", "emp")]} %>% 
    {aggregate(.$emp, list(.$place), FUN=sum)} %>% 
    `colnames<-`(c("place", "employment")) 
  return(df)
}

call_county_employment <- function(
    year,
    bus_data = c("cbp_imp", "cbp_raw", "infogroup", "bea_profile")
    ){
  bus_data <- match.arg(bus_data)
  if(bus_data == "cbp_imp"){
    df <- call_cbp_county_employment_efcy(year)
  }
  if(bus_data == "cbp_raw"){
    df <- call_cbp_county_employment(year)
  }
  if(bus_data == "infogroup"){
    df <- call_infogroup_county_employment(year)
  }
  if(bus_data == "bea_profile"){
    df <- call_bea_county_employment(year)
  }
  return(df)
}

# Establishments  ----

call_cbp_county_estab <- function(year) {
  df <- cbp$call_cbp(
    year = year,
    cbp_scale = "county",
    imputed = FALSE) %>% 
    {.[.$naics == "", ]} %>% 
    {.[c("place", "est")]} %>% 
    `colnames<-`(c("place", "establishments"))
  return(df)
}

call_cbp_county_estab_efcy <- function(year) {
  df <- cbp$call_cbp(
    year = year,
    cbp_scale = "county",
    imputed = TRUE) %>% 
    {.[.$naics == "", ]} %>% 
    {.[c("place", "est")]} %>% 
    `colnames<-`(c("place", "establishments"))
  return(df)
}

call_county_establishments <- function(
    year,
    bus_data = c("cbp_imp", "cbp_raw")
    ){
  bus_data <- match.arg(bus_data)
  if(bus_data == "cbp_imp"){
    df <- call_cbp_county_estab_efcy(year)
  }
  if(bus_data == "cbp_raw"){
    df <- call_cbp_county_estab(year)
  }
  return(df)
}


# GDP and Personal Income  ----

call_bea_county_income <- function(year) {
  #(thousands of dollars)
  year <- util$year2bea_profile(year)
  df <- call_bea_econ_profile() %>%
    {.[.$LineCode == 10, ]} %>%
    {.[!grepl('(0)$', .$GeoFIPS), ]} %>%
    {.[c("GeoFIPS", paste0("X", year))]} %>%
    `colnames<-`(c("place", "income")) %>%
    `rownames<-`({1:nrow(.)})
  return(df)
}

call_county_income <- function(
    year,
    bus_data = c("bea_profile")
){
  bus_data <- match.arg(bus_data)
  if(bus_data == "bea_profile"){
    df <- call_bea_county_income(year)
  }
  return(df)
}


