
# Use rurec.pubdata modules to make local versions of selected data
# Load and attach necessary packages
library(rprojroot)
library(arrow)
library(magrittr)

#simple independent utility functions in base R for cohesion, conversion, or manipulation
source(file.path(find_rstudio_root_file(), "nbs", "basic_utilities.R"))

# S3 methods for automatic reticulate conversion of GeoDataFrame and GeoSeries
source(file.path(find_rstudio_root_file(), "rurec", "reticulate_extras.R"))

# Load conda environment "rurec"
use_condaenv('rurec')

# Import pubdata Python modules
bea_io <- import("rurec.pubdata.bea_io")
cbp <- import("rurec.pubdata.cbp")
ers_rurality <- import("rurec.pubdata.ers_rurality")
geography <- import("rurec.pubdata.geography")
#naics <- import("rurec.pubdata.naics")
geography_cbsa <- import("rurec.pubdata.geography_cbsa")
ag_output <- import("rurec.ag_output")

# save to disk select bea_io python module data with function that mirrors original

  #create warehouse directory
dwh <- file.path(find_rstudio_root_file(), "data", "warehouse")
  if (!file.exists(dwh)) {
    dir.create(dwh, recursive = T)
  }
  
  #make concordance table
fn <- "get_naics_df"
  fp <- file.path(dwh, fn)
  if (!file.exists(fp)) {
    dir.create(fp, recursive = T)
    bea_io$get_naics_df() %>% saveRDS(file = file.path(fp, fn))
  }
  
  ### select years and levels to store
  year = 2012
  ilevel = "det"
  # year = 2021:1997
  # ilevel = c("det", "sum", "sec")
  
fn <- "get_sup"
  for (i in ilevel){
    fp <- file.path(dwh, fn, i)
    if (!file.exists(fp)) {
      dir.create(fp, recursive = T)
    }
    for (y in year){
      bea_year <- year2bea(y, i)
      if (!file.exists(file.path(fp, bea_year))) {
        bea_io$get_sup(bea_year, i) %>% saveRDS(file = file.path(fp, bea_year))
      }
    }
  }
  
  ### select years and levels to store
  year = 2012
  ilevel = "det"
  # year = 2021:1997
  # ilevel = c("det", "sum", "sec")
  
fn <- "get_use"
  for (i in ilevel){
    fp <- file.path(dwh, fn, i)
    if (!file.exists(fp)) {
      dir.create(fp, recursive = T)
    }
    for (y in year){
      bea_year <- year2bea(y, i)
      if (!file.exists(file.path(fp, bea_year))) {
        bea_io$get_use(bea_year, i) %>% saveRDS(file = file.path(fp, bea_year))
      }
    }
  }
  
  
  ### select years and scales to store
  year = 2012
  geometry = TRUE
  scale = "20m"
  # year = 2022:1990
  # scale = c("20m", "500k", "5m")
  
fn <- "get_county_df"
  for (g in geometry){
    for (i in scale){
      fp <- file.path(dwh, fn, g, i)
      if (!file.exists(fp)) {
        dir.create(fp, recursive = T)
      }
      for (y in year){
        if (!file.exists(file.path(fp, year2tiger(y)))) {
          geography$get_county_df(year2tiger(y), g, i) %>% saveRDS(file = file.path(fp, year2tiger(y)))
        }
      }
    }
  }
  
  
  ### select geometry and scales to store
  geometry = FALSE
  
fn <- "get_state_df"
  fp <- file.path(dwh, fn)
  if (!file.exists(fp)) {
    dir.create(fp, recursive = T)
  }
  for (g in geometry){
    geography$get_state_df(g) %>% saveRDS(file = file.path(fp, g))
  }
  
  
  ### select years to store
  year = 2012
  # year = 2020:2003
  
fn <- "get_cbsa_delin_df"
  fp <- file.path(dwh, fn)
  if (!file.exists(fp)) {
    dir.create(fp, recursive = T)
  }
  for (y in year){
    if (!file.exists(file.path(fp, year2cbsa(y)))) {
      geography_cbsa$get_cbsa_delin_df(year2cbsa(y)) %>% saveRDS(file = file.path(fp, year2cbsa(y)))
    }
  }
  
  
  ### select years to store
  year = 2012
  # year = 1986:2021
  
fn <- "get_cbp_year_pq"
  fp <- file.path(dwh, fn)
  if (!file.exists(fp)) {
    dir.create(fp, recursive = T)
  }
  for (y in year){
    if (!file.exists(file.path(fp, year2cbp(y)))) {
      file.copy(as.character(cbp$get_cbp_year_pq(year2cbp(y))), file.path(fp, year2cbp(y)))
    }
  }
  
  
  ### select years to store
  year = 2012
  geo = "county"
  # year = 1986:2021
  # geo = c("county", "state", "us")
  
fn <- "get_df"
  for (g in geo){
    fp <- file.path(dwh, fn, g)
    if (!file.exists(fp)) {
      dir.create(fp, recursive = T)
    }
    for (y in year){
      if (!file.exists(file.path(fp, year2cbp(y)))) {
        cbp$get_df(g, year2cbp(y)) %>% saveRDS(file = file.path(fp, year2cbp(y)))
      }
    }
  }
  
fn <- "get_ruc_df"
  fp <- file.path(dwh, fn)
  if (!file.exists(fp)) {
    dir.create(fp, recursive = T)
    ers_rurality$get_ruc_df() %>% saveRDS(file = file.path(fp, fn))
  }
  
  ### select years to store
  year = 2012
  geo_level =  c("county", "national")
  # year = c(2002, 2007, 2012)
  #geo_level = c("county","state", "national")
  
fn <- "get_farm_sales_by_bea_detail"
  for (g in geo_level){
      fp <- file.path(dwh, fn, g)
    if (!file.exists(fp)) {
      dir.create(fp, recursive = T)
    }
    for (y in year){
      if (!file.exists(file.path(fp, year2agcensus(y)))) {
        ag_output$get_farm_sales_by_bea_detail(year2agcensus(y), g) %>% saveRDS(file = file.path(fp, year2agcensus(y)))
      }
    }
  }

  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  

