
# A place to call functions from rurec.pubdata

# Load and attach necessary packages
library(rprojroot)

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


#### Set of data generation functions called from rurec.pubdata used in downstream R notebooks####
get_sup <- function(year, 
                    level, 
                    labels = FALSE){
  df <- bea_io$get_sup(year, level, labels)
  return(df)
}
get_use <- function(year, 
                    level, 
                    labels = FALSE){
  df <- bea_io$get_use(year, level, labels)
  return(df)
}
get_naics_df <- function(){
  df <- bea_io$get_naics_df() 
  return(df)
}
get_cbp_year_pq <- function(year){
  df <- cbp$get_cbp_year_pq(year)
  return(df)
}
get_df <- function(geo, 
                   year){
  df <- cbp$get_df(geo, year)
  return(df)
}
get_ruc_df <- function(){
  df <- ers_rurality$get_ruc_df()
  return(df)
}
get_county_df <- function(year = 2020L, 
                          geometry = TRUE, 
                          scale = "20m"){
  df <- geography$get_county_df(year, geometry, scale)
  return(df)
}
get_state_df <- function(geometry = TRUE, 
                         scale = "5m"){
  df <- geography$get_state_df(geometry, scale)
  return(df)
}
get_cbsa_delin_df <- function(year){
  df <- geography_cbsa$get_cbsa_delin_df(year)
  return(df)
}
get_farm_sales_by_bea_detail <- function(year, 
                                         geo_level){
  df <- ag_output$get_farm_sales_by_bea_detail(year, geo_level)
  return(df)
}


