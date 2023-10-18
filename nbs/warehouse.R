# call from disk select bea_io python module data as function that mirrors original

# Load and attach necessary packages
library(rprojroot)
library(sf)

# ensure data exists where expected
if(!file.exists(file.path(find_rstudio_root_file(), "data", "warehouse"))){warning("warehouse directory not found")}


# bea to naics concordance 
get_naics_df <- function(fp = file.path(find_rstudio_root_file(), "data", "warehouse", "get_naics_df", "get_naics_df")){
  readRDS(fp)
}

# bea supply table
get_sup <- function(year, 
                    ilevel, 
                    fp = file.path(find_rstudio_root_file(), "data", "warehouse", "get_sup")){
  readRDS(file.path(fp, ilevel, year))
}

# bea use table
get_use <- function(year, 
                    ilevel, 
                    fp = file.path(find_rstudio_root_file(), "data", "warehouse", "get_use")){
  readRDS(file.path(fp, ilevel, year))
}

# census county shapefiles
get_county_df <- function(year,
                          geometry,
                          scale,
                          fp = file.path(find_rstudio_root_file(), "data", "warehouse", "get_county_df")){
  readRDS(file.path(fp, geometry, scale, year)) %>% st_as_sf()
}

# census state shapefiles
get_state_df <- function(geometry,
                         fp = file.path(find_rstudio_root_file(), "data", "warehouse", "get_state_df")){
  readRDS(file.path(fp, geometry))
}

# census cbsa delineations
get_cbsa_delin_df <- function(year,
                              fp = file.path(find_rstudio_root_file(), "data", "warehouse", "get_cbsa_delin_df")){
  readRDS(file.path(fp, year))
}

# efsy census cbp
get_cbp_year_pq <- function(year,
                            fp = file.path(find_rstudio_root_file(), "data", "warehouse", "get_cbp_year_pq")){
  file.path(fp, year)
}

# census cbp
get_df <- function(geo,
                   year,
                   fp = file.path(find_rstudio_root_file(), "data", "warehouse", "get_df")){
  readRDS(file.path(fp, geo, year))
}

# census rucc
get_ruc_df <- function(fp = file.path(find_rstudio_root_file(), "data", "warehouse", "get_ruc_df", "get_ruc_df")){
  readRDS(file.path(fp))
}

# ag census bea farm sales
get_farm_sales_by_bea_detail <- function(year, 
                                         geo_level,
                                         fp = file.path(find_rstudio_root_file(), "data", "warehouse", "get_farm_sales_by_bea_detail")){
  readRDS(file.path(fp, geo_level, year))
}


#example and test of functions
run_test = FALSE
if (run_test){
  df <- get_naics_df()
  str(df, max.level = 1)
  
  df <- get_sup(2012, "det")
  dim(df)
  
  df <- get_use(2012, "det")
  dim(df)
  
  df <- get_county_df(2013, TRUE, "20m")
  str(df)
  
  df <- get_state_df(FALSE)
  str(df, max.level = 1)
  
  df <- get_cbsa_delin_df(2013)
  str(df, max.level = 1)
  
  get_cbp_year_pq(2012)
  
  df <- get_df("county", 2012)
  str(df)
  
  df <- get_ruc_df()
  str(df, max.level = 1)
  
  df <- get_farm_sales_by_bea_detail(2012, "county")
  str(df)
}







