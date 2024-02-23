# Indicators of prosperity



# R packages ----
library(logger)
log_threshold(DEBUG)
library(tidyverse)
library(tidycensus)
library(readxl)


# R scripts ----
source("R/basic_utilities.R", local = (util <- new.env()))



# Data paths ----
# inputs
ipath <- list(
  unemployment_rate = "https://www.ers.usda.gov/webdocs/DataFiles/48747/Unemployment.xlsx?v=8720.6", 
  netmigration_rate = "https://netmigration.wisc.edu/documents/NME_1020_data_beta.zip", 
  CHRR_data_2012 = "https://www.countyhealthrankings.org/sites/default/files/analytic_data2012.csv",
  saipe_2022 = "https://www2.census.gov/programs-surveys/saipe/datasets/2022/2022-state-and-county/est22all.xls",
  saipe_2021 = "https://www2.census.gov/programs-surveys/saipe/datasets/2021/2021-state-and-county/est21all.xls",
  saipe_2020 = "https://www2.census.gov/programs-surveys/saipe/datasets/2020/2020-state-and-county/est20all.xls",
  saipe_2019 = "https://www2.census.gov/programs-surveys/saipe/datasets/2019/2019-state-and-county/est19all.xls",
  saipe_2018 = "https://www2.census.gov/programs-surveys/saipe/datasets/2018/2018-state-and-county/est18all.xls",
  saipe_2017 = "https://www2.census.gov/programs-surveys/saipe/datasets/2017/2017-state-and-county/est17all.xls",
  saipe_2016 = "https://www2.census.gov/programs-surveys/saipe/datasets/2016/2016-state-and-county/est16all.xls",
  saipe_2015 = "https://www2.census.gov/programs-surveys/saipe/datasets/2015/2015-state-and-county/est15all.xls",
  saipe_2014 = "https://www2.census.gov/programs-surveys/saipe/datasets/2014/2014-state-and-county/est14all.xls",
  saipe_2013 = "https://www2.census.gov/programs-surveys/saipe/datasets/2013/2013-state-and-county/est13all.xls",
  saipe_2012 = "https://www2.census.gov/programs-surveys/saipe/datasets/2012/2012-state-and-county/est12all.xls",
  saipe_2011 = "https://www2.census.gov/programs-surveys/saipe/datasets/2011/2011-state-and-county/est11all.xls",
  saipe_2010 = "https://www2.census.gov/programs-surveys/saipe/datasets/2010/2010-state-and-county/est10all.xls",
  saipe_2009 = "https://www2.census.gov/programs-surveys/saipe/datasets/2009/2009-state-and-county/est09all.xls",
  saipe_2008 = "https://www2.census.gov/programs-surveys/saipe/datasets/2008/2008-state-and-county/est08all.xls",
  saipe_2007 = "https://www2.census.gov/programs-surveys/saipe/datasets/2007/2007-state-and-county/est07all.xls",
  saipe_2006 = "https://www2.census.gov/programs-surveys/saipe/datasets/2006/2006-state-and-county/est06all.xls",
  saipe_2005 = "https://www2.census.gov/programs-surveys/saipe/datasets/2005/2005-state-and-county/est05all.xls",
  saipe_2004 = "https://www2.census.gov/programs-surveys/saipe/datasets/2004/2004-state-and-county/est04all.xls"
)

# outputs
opath <- list(
  unemployment_rate_raw = "data/prosperity/raw/unemployment.xlsx", 
  netmigration_rate_raw = "data/prosperity/raw/NME_1020_data_beta.zip",
  education_raw = "data/prosperity/raw/education.csv",
  lfpr_raw = "data/prosperity/raw/lfpr.csv",
  CHRR_data_2012_raw = "data/prosperity/raw/analytic_data2012.csv",
  saipe_2022_raw = "data/prosperity/raw/saipe22.xls",
  saipe_2021_raw = "data/prosperity/raw/saipe21.xls",
  saipe_2020_raw = "data/prosperity/raw/saipe20.xls",
  saipe_2019_raw = "data/prosperity/raw/saipe19.xls",
  saipe_2018_raw = "data/prosperity/raw/saipe18.xls",
  saipe_2017_raw = "data/prosperity/raw/saipe17.xls",
  saipe_2016_raw = "data/prosperity/raw/saipe16.xls",
  saipe_2015_raw = "data/prosperity/raw/saipe15.xls",
  saipe_2014_raw = "data/prosperity/raw/saipe14.xls",
  saipe_2013_raw = "data/prosperity/raw/saipe13.xls",
  saipe_2012_raw = "data/prosperity/raw/saipe12.xls",
  saipe_2011_raw = "data/prosperity/raw/saipe11.xls",
  saipe_2010_raw = "data/prosperity/raw/saipe10.xls",
  saipe_2009_raw = "data/prosperity/raw/saipe09.xls",
  saipe_2008_raw = "data/prosperity/raw/saipe08.xls",
  saipe_2007_raw = "data/prosperity/raw/saipe07.xls",
  saipe_2006_raw = "data/prosperity/raw/saipe06.xls",
  saipe_2005_raw = "data/prosperity/raw/saipe05.xls",
  saipe_2004_raw = "data/prosperity/raw/saipe04.xls",
  saipe_raw = "data/prosperity/raw/saipe.csv"
)



# Population ----
# population is used as an example here
# we actually already have it in pubdata

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

call_education_df <- function() {
  raw_path <- opath$education_raw
  
  # download raw data if needed
  if (file.exists(raw_path)) {
    log_debug("raw data found at {raw_path}")
  } else {
    years <- lst(2010,2012,2017,2022) 
    
    education <- map_dfr(
      years,
      ~ get_acs(
        geography = "county",
        variables = "S1501_C02_015E",
        year = .x,
        survey = "acs5",
        geometry = FALSE
      ),
      .id = "year"  # when combining results, add id var (name of list item)
    )
    write.csv(education, "data/prosperity/raw/education.csv")
    
    log_debug("raw data dowloaded to {raw_path}")
  }
  
  df <- read.csv(raw_path) %>%
    mutate(estimate = estimate/100,
           YEAR = if_else(year == 2010, "2006-2010",
                          ifelse(year == 2012, "2008-2012",
                                 ifelse(year == 2017, "2013-2017",
                                        ifelse(year == 2022, "2018-2022", NA))))) %>%
    select(-year, -X)
  return(df)
}

#US County Health Rankings and Roadmaps Data 2012 (Includes premature death)

call_CHRR_df <- function() {
  raw_path <- opath$CHRR_data_2012_raw
  # download raw data if needed
  if (file.exists(raw_path)) {
    log_debug("raw data found at {raw_path}")
  } else {
    # create parent directories
    raw_path <- util$mkdir(raw_path)
    download_status <- download.file(url = ipath$CHRR_data_2012, destfile = raw_path, mode = "wb")
    stopifnot(download_status == 0)
    log_debug("raw data dowloaded to {raw_path}")
  }
  df <- read_csv(raw_path, skip = 1)
  
  return(df)
}

call_lfpr_df <- function() {
  raw_path <- opath$lfpr_raw
  
  # download raw data if needed
  if (file.exists(raw_path)) {
    log_debug("raw data found at {raw_path}")
  } else {
    years <- lst(2010,2012,2017,2022) 
    
    lfpr <- map_dfr(
      years,
      ~ get_acs(
        geography = "county",
        variables = "S2301_C02_021E",
        year = .x,
        survey = "acs5",
        geometry = FALSE
      ),
      .id = "year"  # when combining results, add id var (name of list item)
    )
    write.csv(lfpr, "data/prosperity/raw/lfpr.csv")
    
    log_debug("raw data dowloaded to {raw_path}")
  }
  
  df <- read.csv(raw_path) %>%
    mutate(estimate = estimate/100,
           YEAR = if_else(year == 2010, "2006-2010",
                          ifelse(year == 2012, "2008-2012",
                                 ifelse(year == 2017, "2013-2017",
                                        ifelse(year == 2022, "2018-2022", NA))))) %>%
    select(-year, -X)
  return(df)
}


call_saipe_2022_df <- function() {
  raw_path <- opath$saipe_2022_raw
  
  # download raw data if needed
  if (file.exists(raw_path)) {
    log_debug("raw data found at {raw_path}")
  } else {
    # create parent directories
    raw_path <- util$mkdir(raw_path)
    download_status <- download.file(url = ipath$saipe_2022, destfile = raw_path, mode = "wb")
    stopifnot(download_status == 0)
    log_debug("raw data dowloaded to {raw_path}")
  }
  
  df <- read_xls(raw_path, skip = 3) %>%
    rename("pov"="Poverty Percent, All Ages", "state.fips"="State FIPS Code",
           "county.fips"="County FIPS Code", "state"="Postal Code") %>%
    mutate(pov = as.numeric(pov)/100, state.fips = sprintf("%02d", as.numeric(state.fips)), county.fips = sprintf("%03d", as.numeric(county.fips))) %>%
    select(state.fips, county.fips, state, pov) %>%
    mutate(year = 2022)
  
  return(df)
}

call_saipe_2021_df <- function() {
  raw_path <- opath$saipe_2021_raw
  
  # download raw data if needed
  if (file.exists(raw_path)) {
    log_debug("raw data found at {raw_path}")
  } else {
    # create parent directories
    raw_path <- util$mkdir(raw_path)
    download_status <- download.file(url = ipath$saipe_2021, destfile = raw_path, mode = "wb")
    stopifnot(download_status == 0)
    log_debug("raw data dowloaded to {raw_path}")
  }
  
  df <- read_xls(raw_path, skip = 3) %>%
    rename("pov"="Poverty Percent, All Ages", "state.fips"="State FIPS Code",
           "county.fips"="County FIPS Code", "state"="Postal Code") %>%
    mutate(pov = as.numeric(pov)/100, state.fips = sprintf("%02d", as.numeric(state.fips)), county.fips = sprintf("%03d", as.numeric(county.fips))) %>%
    select(state.fips, county.fips, state, pov) %>%
    mutate(year = 2021)
  
  return(df)
}

call_saipe_2020_df <- function() {
  raw_path <- opath$saipe_2020_raw
  
  # download raw data if needed
  if (file.exists(raw_path)) {
    log_debug("raw data found at {raw_path}")
  } else {
    # create parent directories
    raw_path <- util$mkdir(raw_path)
    download_status <- download.file(url = ipath$saipe_2020, destfile = raw_path, mode = "wb")
    stopifnot(download_status == 0)
    log_debug("raw data dowloaded to {raw_path}")
  }
  
  df <- read_xls(raw_path, skip = 3) %>%
    rename("pov"="Poverty Percent, All Ages", "state.fips"="State FIPS Code",
           "county.fips"="County FIPS Code", "state"="Postal Code") %>%
    mutate(pov = as.numeric(pov)/100, state.fips = sprintf("%02d", as.numeric(state.fips)), county.fips = sprintf("%03d", as.numeric(county.fips))) %>%
    select(state.fips, county.fips, state, pov) %>%
    mutate(year = 2020)
  
  return(df)
}


call_saipe_2018_df <- function() {
  raw_path <- opath$saipe_2018_raw
  
  # download raw data if needed
  if (file.exists(raw_path)) {
    log_debug("raw data found at {raw_path}")
  } else {
    # create parent directories
    raw_path <- util$mkdir(raw_path)
    download_status <- download.file(url = ipath$saipe_2018, destfile = raw_path, mode = "wb")
    stopifnot(download_status == 0)
    log_debug("raw data dowloaded to {raw_path}")
  }
  
  df <- read_xls(raw_path, skip = 3) %>%
    rename("pov"="Poverty Percent, All Ages", "state.fips"="State FIPS Code",
           "county.fips"="County FIPS Code", "state"="Postal Code") %>%
    mutate(pov = as.numeric(pov)/100, state.fips = sprintf("%02d", as.numeric(state.fips)), county.fips = sprintf("%03d", as.numeric(county.fips))) %>%
    select(state.fips, county.fips, state, pov) %>%
    mutate(year = 2019)
  
  return(df)
}
call_saipe_2019_df <- function() {
  raw_path <- opath$saipe_2019_raw
  
  # download raw data if needed
  if (file.exists(raw_path)) {
    log_debug("raw data found at {raw_path}")
  } else {
    # create parent directories
    raw_path <- util$mkdir(raw_path)
    download_status <- download.file(url = ipath$saipe_2019, destfile = raw_path, mode = "wb")
    stopifnot(download_status == 0)
    log_debug("raw data dowloaded to {raw_path}")
  }
  
  df <- read_xls(raw_path, skip = 3) %>%
    rename("pov"="Poverty Percent, All Ages", "state.fips"="State FIPS Code",
           "county.fips"="County FIPS Code", "state"="Postal Code") %>%
    mutate(pov = as.numeric(pov)/100, state.fips = sprintf("%02d", as.numeric(state.fips)), county.fips = sprintf("%03d", as.numeric(county.fips))) %>%
    select(state.fips, county.fips, state, pov) %>%
    mutate(year = 2018)
  
  return(df)
}
call_saipe_2017_df <- function() {
  raw_path <- opath$saipe_2017_raw
  
  # download raw data if needed
  if (file.exists(raw_path)) {
    log_debug("raw data found at {raw_path}")
  } else {
    # create parent directories
    raw_path <- util$mkdir(raw_path)
    download_status <- download.file(url = ipath$saipe_2017, destfile = raw_path, mode = "wb")
    stopifnot(download_status == 0)
    log_debug("raw data dowloaded to {raw_path}")
  }
  
  df <- read_xls(raw_path, skip = 3) %>%
    rename("pov"="Poverty Percent, All Ages", "state.fips"="State FIPS Code",
           "county.fips"="County FIPS Code", "state"="Postal Code") %>%
    mutate(pov = as.numeric(pov)/100, state.fips = sprintf("%02d", as.numeric(state.fips)), county.fips = sprintf("%03d", as.numeric(county.fips))) %>%
    select(state.fips, county.fips, state, pov) %>%
    mutate(year = 2017)
  
  return(df)
}
call_saipe_2016_df <- function() {
  raw_path <- opath$saipe_2016_raw
  
  # download raw data if needed
  if (file.exists(raw_path)) {
    log_debug("raw data found at {raw_path}")
  } else {
    # create parent directories
    raw_path <- util$mkdir(raw_path)
    download_status <- download.file(url = ipath$saipe_2016, destfile = raw_path, mode = "wb")
    stopifnot(download_status == 0)
    log_debug("raw data dowloaded to {raw_path}")
  }
  
  df <- read_xls(raw_path, skip = 3) %>%
    rename("pov"="Poverty Percent, All Ages", "state.fips"="State FIPS Code",
           "county.fips"="County FIPS Code", "state"="Postal Code") %>%
    mutate(pov = as.numeric(pov)/100, state.fips = sprintf("%02d", as.numeric(state.fips)), county.fips = sprintf("%03d", as.numeric(county.fips))) %>%
    select(state.fips, county.fips, state, pov) %>%
    mutate(year = 2016)
  
  return(df)
}
call_saipe_2015_df <- function() {
  raw_path <- opath$saipe_2015_raw
  
  # download raw data if needed
  if (file.exists(raw_path)) {
    log_debug("raw data found at {raw_path}")
  } else {
    # create parent directories
    raw_path <- util$mkdir(raw_path)
    download_status <- download.file(url = ipath$saipe_2015, destfile = raw_path, mode = "wb")
    stopifnot(download_status == 0)
    log_debug("raw data dowloaded to {raw_path}")
  }
  
  df <- read_xls(raw_path, skip = 3) %>%
    rename("pov"="Poverty Percent, All Ages", "state.fips"="State FIPS Code",
           "county.fips"="County FIPS Code", "state"="Postal Code") %>%
    mutate(pov = as.numeric(pov)/100, state.fips = sprintf("%02d", as.numeric(state.fips)), county.fips = sprintf("%03d", as.numeric(county.fips))) %>%
    select(state.fips, county.fips, state, pov) %>%
    mutate(year = 2015)
  
  return(df)
}
call_saipe_2014_df <- function() {
  raw_path <- opath$saipe_2014_raw
  
  # download raw data if needed
  if (file.exists(raw_path)) {
    log_debug("raw data found at {raw_path}")
  } else {
    # create parent directories
    raw_path <- util$mkdir(raw_path)
    download_status <- download.file(url = ipath$saipe_2014, destfile = raw_path, mode = "wb")
    stopifnot(download_status == 0)
    log_debug("raw data dowloaded to {raw_path}")
  }
  
  df <- read_xls(raw_path, skip = 3) %>%
    rename("pov"="Poverty Percent, All Ages", "state.fips"="State FIPS Code",
           "county.fips"="County FIPS Code", "state"="Postal Code") %>%
    mutate(pov = as.numeric(pov)/100, state.fips = sprintf("%02d", as.numeric(state.fips)), county.fips = sprintf("%03d", as.numeric(county.fips))) %>%
    select(state.fips, county.fips, state, pov) %>%
    mutate(year = 2014)
  
  return(df)
}
call_saipe_2013_df <- function() {
  raw_path <- opath$saipe_2013_raw
  
  # download raw data if needed
  if (file.exists(raw_path)) {
    log_debug("raw data found at {raw_path}")
  } else {
    # create parent directories
    raw_path <- util$mkdir(raw_path)
    download_status <- download.file(url = ipath$saipe_2013, destfile = raw_path, mode = "wb")
    stopifnot(download_status == 0)
    log_debug("raw data dowloaded to {raw_path}")
  }
  
  df <- read_xls(raw_path, skip = 3) %>%
    rename("pov"="Poverty Percent, All Ages", "state.fips"="State FIPS Code",
           "county.fips"="County FIPS Code", "state"="Postal Code") %>%
    mutate(pov = as.numeric(pov)/100, state.fips = sprintf("%02d", as.numeric(state.fips)), county.fips = sprintf("%03d", as.numeric(county.fips))) %>%
    select(state.fips, county.fips, state, pov) %>%
    mutate(year = 2013)
  
  return(df)
}
call_saipe_2012_df <- function() {
  raw_path <- opath$saipe_2012_raw
  
  # download raw data if needed
  if (file.exists(raw_path)) {
    log_debug("raw data found at {raw_path}")
  } else {
    # create parent directories
    raw_path <- util$mkdir(raw_path)
    download_status <- download.file(url = ipath$saipe_2012, destfile = raw_path, mode = "wb")
    stopifnot(download_status == 0)
    log_debug("raw data dowloaded to {raw_path}")
  }
  
  df <- read_xls(raw_path, skip = 2) %>%
    rename("pov"="Poverty Percent, All Ages", "state.fips"="State FIPS Code",
           "county.fips"="County FIPS Code", "state"="Postal Code") %>%
    mutate(pov = as.numeric(pov)/100, state.fips = sprintf("%02d", as.numeric(state.fips)), county.fips = sprintf("%03d", as.numeric(county.fips))) %>%
    select(state.fips, county.fips, state, pov) %>%
    mutate(year = 2012)
  
  return(df)
}
call_saipe_2011_df <- function() {
  raw_path <- opath$saipe_2011_raw
  
  # download raw data if needed
  if (file.exists(raw_path)) {
    log_debug("raw data found at {raw_path}")
  } else {
    # create parent directories
    raw_path <- util$mkdir(raw_path)
    download_status <- download.file(url = ipath$saipe_2011, destfile = raw_path, mode = "wb")
    stopifnot(download_status == 0)
    log_debug("raw data dowloaded to {raw_path}")
  }
  
  df <- read_xls(raw_path, skip = 2) %>%
    rename("pov"="Poverty Percent All Ages", "state.fips"="State FIPS",
           "county.fips"="County FIPS", "state"="Postal") %>%
    mutate(pov = as.numeric(pov)/100, state.fips = sprintf("%02d", as.numeric(state.fips)), county.fips = sprintf("%03d", as.numeric(county.fips))) %>%
    select(state.fips, county.fips, state, pov) %>%
    mutate(year = 2011)
  
  return(df)
}
call_saipe_2010_df <- function() {
  raw_path <- opath$saipe_2010_raw
  
  # download raw data if needed
  if (file.exists(raw_path)) {
    log_debug("raw data found at {raw_path}")
  } else {
    # create parent directories
    raw_path <- util$mkdir(raw_path)
    download_status <- download.file(url = ipath$saipe_2010, destfile = raw_path, mode = "wb")
    stopifnot(download_status == 0)
    log_debug("raw data dowloaded to {raw_path}")
  }
  
  df <- read_xls(raw_path, skip = 2) %>%
    rename("pov"="Poverty Percent All Ages", "state.fips"="State FIPS",
           "county.fips"="County FIPS", "state"="Postal") %>%
    mutate(pov = as.numeric(pov)/100, state.fips = sprintf("%02d", as.numeric(state.fips)), county.fips = sprintf("%03d", as.numeric(county.fips))) %>%
    select(state.fips, county.fips, state, pov) %>%
    mutate(year = 2010)
  
  return(df)
}
call_saipe_2009_df <- function() {
  raw_path <- opath$saipe_2009_raw
  
  # download raw data if needed
  if (file.exists(raw_path)) {
    log_debug("raw data found at {raw_path}")
  } else {
    # create parent directories
    raw_path <- util$mkdir(raw_path)
    download_status <- download.file(url = ipath$saipe_2009, destfile = raw_path, mode = "wb")
    stopifnot(download_status == 0)
    log_debug("raw data dowloaded to {raw_path}")
  }
  
  df <- read_xls(raw_path, skip = 2) %>%
    rename("pov"="Poverty Percent All Ages", "state.fips"="State FIPS",
           "county.fips"="County FIPS", "state"="Postal") %>%
    mutate(pov = as.numeric(pov)/100, state.fips = sprintf("%02d", as.numeric(state.fips)), county.fips = sprintf("%03d", as.numeric(county.fips))) %>%
    select(state.fips, county.fips, state, pov) %>%
    mutate(year = 2009)
  
  return(df)
}
call_saipe_2008_df <- function() {
  raw_path <- opath$saipe_2008_raw
  
  # download raw data if needed
  if (file.exists(raw_path)) {
    log_debug("raw data found at {raw_path}")
  } else {
    # create parent directories
    raw_path <- util$mkdir(raw_path)
    download_status <- download.file(url = ipath$saipe_2008, destfile = raw_path, mode = "wb")
    stopifnot(download_status == 0)
    log_debug("raw data dowloaded to {raw_path}")
  }
  
  df <- read_xls(raw_path, skip = 2) %>%
    rename("pov"="Poverty Percent All Ages", "state.fips"="State FIPS",
           "county.fips"="County FIPS", "state"="Postal") %>%
    mutate(pov = as.numeric(pov)/100, state.fips = sprintf("%02d", as.numeric(state.fips)), county.fips = sprintf("%03d", as.numeric(county.fips))) %>%
    select(state.fips, county.fips, state, pov) %>%
    mutate(year = 2008)
  
  return(df)
}
call_saipe_2007_df <- function() {
  raw_path <- opath$saipe_2007_raw
  
  # download raw data if needed
  if (file.exists(raw_path)) {
    log_debug("raw data found at {raw_path}")
  } else {
    # create parent directories
    raw_path <- util$mkdir(raw_path)
    download_status <- download.file(url = ipath$saipe_2007, destfile = raw_path, mode = "wb")
    stopifnot(download_status == 0)
    log_debug("raw data dowloaded to {raw_path}")
  }
  
  df <- read_xls(raw_path, skip = 2) %>%
    rename("pov"="Poverty Percent All Ages", "state.fips"="State FIPS",
           "county.fips"="County FIPS", "state"="Postal") %>%
    mutate(pov = as.numeric(pov)/100, state.fips = sprintf("%02d", as.numeric(state.fips)), county.fips = sprintf("%03d", as.numeric(county.fips))) %>%
    select(state.fips, county.fips, state, pov) %>%
    mutate(year = 2007)
  
  return(df)
}
call_saipe_2006_df <- function() {
  raw_path <- opath$saipe_2006_raw
  
  # download raw data if needed
  if (file.exists(raw_path)) {
    log_debug("raw data found at {raw_path}")
  } else {
    # create parent directories
    raw_path <- util$mkdir(raw_path)
    download_status <- download.file(url = ipath$saipe_2006, destfile = raw_path, mode = "wb")
    stopifnot(download_status == 0)
    log_debug("raw data dowloaded to {raw_path}")
  }
  
  df <- read_xls(raw_path, skip = 2) %>%
    rename("pov"="Poverty Percent All Ages", "state.fips"="State FIPS",
           "county.fips"="County FIPS", "state"="Postal") %>%
    mutate(pov = as.numeric(pov)/100, state.fips = sprintf("%02d", as.numeric(state.fips)), county.fips = sprintf("%03d", as.numeric(county.fips))) %>%
    select(state.fips, county.fips, state, pov) %>%
    mutate(year = 2006)
  
  return(df)
}
call_saipe_2005_df <- function() {
  raw_path <- opath$saipe_2005_raw
  
  # download raw data if needed
  if (file.exists(raw_path)) {
    log_debug("raw data found at {raw_path}")
  } else {
    # create parent directories
    raw_path <- util$mkdir(raw_path)
    download_status <- download.file(url = ipath$saipe_2005, destfile = raw_path, mode = "wb")
    stopifnot(download_status == 0)
    log_debug("raw data dowloaded to {raw_path}")
  }
  
  df <- read_xls(raw_path, skip = 2) %>%
    rename("pov"="Poverty Percent All Ages", "state.fips"="State FIPS",
           "county.fips"="County FIPS", "state"="Postal") %>%
    mutate(pov = as.numeric(pov)/100, state.fips = sprintf("%02d", as.numeric(state.fips)), county.fips = sprintf("%03d", as.numeric(county.fips))) %>%
    select(state.fips, county.fips, state, pov) %>%
    mutate(year = 2005)
  
  return(df)
}
call_saipe_2004_df <- function() {
  raw_path <- opath$saipe_2004_raw
  
  # download raw data if needed
  if (file.exists(raw_path)) {
    log_debug("raw data found at {raw_path}")
  } else {
    # create parent directories
    raw_path <- util$mkdir(raw_path)
    download_status <- download.file(url = ipath$saipe_2004, destfile = raw_path, mode = "wb")
    stopifnot(download_status == 0)
    log_debug("raw data dowloaded to {raw_path}")
  }
  
  df <- read_xls(raw_path, skip = 1) %>%
    rename("pov"="Poverty Percent All Ages", "state.fips"="State FIPS",
           "county.fips"="County FIPS", "state"="Postal") %>%
    mutate(pov = as.numeric(pov)/100, state.fips = sprintf("%02d", as.numeric(state.fips)), county.fips = sprintf("%03d", as.numeric(county.fips))) %>%
    select(state.fips, county.fips, state, pov) %>%
    mutate(year = 2004)
  
  return(df)
}

call_saipe_df <- function() {
  raw_path <- opath$saipe_raw

    # download raw data if needed
  if (file.exists(raw_path)) {
    log_debug("raw data found at {raw_path}")
  } else {
    saipe22 = prosperity$call_saipe_2022_df()
    saipe21 = prosperity$call_saipe_2021_df()
    saipe20 = prosperity$call_saipe_2020_df()
    saipe19 = prosperity$call_saipe_2019_df()
    saipe18 = prosperity$call_saipe_2018_df()
    saipe17 = prosperity$call_saipe_2017_df()
    saipe16 = prosperity$call_saipe_2016_df()
    saipe15 = prosperity$call_saipe_2015_df()
    saipe14 = prosperity$call_saipe_2014_df()
    saipe13 = prosperity$call_saipe_2013_df()
    saipe12 = prosperity$call_saipe_2012_df()
    saipe11 = prosperity$call_saipe_2011_df()
    saipe10 = prosperity$call_saipe_2010_df()
    saipe09 = prosperity$call_saipe_2009_df()
    saipe08 = prosperity$call_saipe_2008_df()
    saipe07 = prosperity$call_saipe_2007_df()
    saipe06 = prosperity$call_saipe_2006_df()
    saipe05 = prosperity$call_saipe_2005_df()
    saipe04 = prosperity$call_saipe_2004_df()
    
    saipe = rbind(saipe22,saipe21,saipe20,saipe19,saipe18,saipe17,saipe16,saipe15,saipe14,saipe13,saipe12,saipe11,saipe10,saipe09,saipe08,saipe07,saipe06,saipe05,saipe04)
    
    write.csv(saipe, "data/prosperity/raw/saipe.csv")
    
  }
  
  df <- read.csv(raw_path) %>%
    mutate(state.fips = sprintf("%02d", as.numeric(state.fips)), county.fips = sprintf("%03d", as.numeric(county.fips))) %>%
    select(-X)
  
  return(df)
}





