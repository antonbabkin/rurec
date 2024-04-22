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
source("R/visualization.R", local = (visualization <- new.env()))
source("R/dataprep_bea_io.R", local = (bea_io <- new.env()))

# paths ----

# many inputs are required indirectly through sourced scripts
ipath <- list(
)

opath <- list(
  geog_ = "data/projects/industry_health/geog/{year}.rds",
  ruc_ = "data/projects/industry_health/ers_ruc/{year}.rds",
  cbsa_conc_ = "data/projects/industry_health/cbsa_conc/{year}.rds",
  cbsa_delin_ = "data/projects/industry_health/cbsa_delin/{year}.rds",
  cbsa_ = "data/projects/industry_health/cbsa/{year}.rds",
  eca_df = "data/projects/industry_health/eca.rds",
  production_ = "data/projects/industry_health/production/{bus_data}/{ilevel}/{class_system}/{year}.rds",
  population_ = "data/projects/industry_health/population/{bus_data}/{year}.rds",
  laborforce_ = "data/projects/industry_health/laborforce/{bus_data}/{year}.rds",
  employment_ = "data/projects/industry_health/employment/{bus_data}/{year}.rds",
  income_ = "data/projects/industry_health/income/{bus_data}/{year}.rds",
  income_rate_ = "data/projects/industry_health/income_rate/{bus_data}/{year}.rds",
  gdp_ = "data/projects/industry_health/gdp/{bus_data}/{price_level}/{year}.rds",
  laborforce_rate_ = "data/projects/industry_health/laborforce_rate/{bus_data}/{year}.rds",
  highschool_attainment_rate_ = "data/projects/industry_health/highschool_attainment_rate/{bus_data}/{year}.rds",
  poverty_ = "data/projects/industry_healthpoverty/{bus_data}/{year}.rds",
  poverty_rate_ = "data/projects/industry_health/poverty_rate/{bus_data}/{year}.rds",
  ypll75_ = "data/projects/industry_health/ypll75/{bus_data}/{year}.rds",
  PAAM_ = "data/projects/industry_health/PAAM/{bus_data}/{year}.rds",
  establishments_ = "data/projects/industry_health/establishments/{bus_data}/{year}.rds",
  payroll_ = "data/projects/industry_health/payroll/{bus_data}/{year}.rds",
  entry_ = "data/projects/industry_health/entry/{bus_data}/{year}.rds",
  exit_ = "data/projects/industry_health/exit/{bus_data}/{year}.rds",
  entry_rate_ = "data/projects/industry_health/entry_rate/{bus_data}/{year}.rds",
  exit_rate_ = "data/projects/industry_health/exit_rate/{bus_data}/{year}.rds",
  industry_structure_ = "data/projects/industry_health/industry_structure/{year}.rds"
)


# Utility functions ----

#' (x1 - x0) / (0.5 * (x1 + x0))
growth_rate <- dataprep_misc$growth_rate

#' Call every data-generating function for each applicable parameter value
#' for the purpose of caching returned values
create_complete_cache <- function() {
  for (year in c(2010, 2013:2020)) {
    call_geog(year)
  }
  for (year in c(2013, 2015, 2017, 2018, 2020, 2023)) {
    # call_cbsa_conc(year)
    # call_cbsa_delin_df(year)
    call_cbsa(year)
  }
  call_eca_df()
  for (year in c(2003, 2013)) {
    call_ruc(year)
  }
  for (year in 2009:2019) {
    call_population(year, bus_data = "tidy_acs")
  }
  for (year in 2002:2022) {
    call_employment(year, bus_data = "ers")
    call_unemp_rate(year, bus_data = "ers")
  }
  for (year in 2012:2022) {
    call_laborforce_rate(year, bus_data = "tidy_acs")
  }
  for (year in 2002:2021) {
    call_employment(year, bus_data = "cbp_raw")
    call_establishments(year, bus_data = "cbp_raw")
    call_payroll(year, bus_data = "cbp_raw")
    call_wage(year, bus_data = "cbp_raw")
  }
  for (year in 2002:2021) {
    call_exit_rate(year, bus_data = "bds")
    call_entry_rate(year, bus_data = "bds")
  }  
  for (year in 2002:2022) {
    call_income_rate(year, bus_data = "bea_profile")
  }
  for (year in 2017:2022) {
    call_gdp(year, price_level = "nominal")
  }
  for (year in 2002:2022) {
    call_poverty_rate(year, bus_data = "saipe")
  }
  for (year in 2011:2023) {
    call_ypll75(year, bus_data = "chr")
  }
  for (year in 2010:2022) {
    call_highschool_attainment_rate(year, bus_data = "tidy_acs")
  }
}

#' #' clean up a df for better time with visualizations
#' strip_dataframe <- function(df){
#'   df <- df %>% 
#'     st_drop_geometry() %>% 
#'     select(where(is.numeric)) %>% 
#'     na.omit()
#'   return(df)
#' }


# Data Viz functions ----

viz <- new.env()
viz$diverge_choro_map <- visualization$diverge_choro_map
viz$density_dist_plot <- visualization$density_dist_plot
viz$cat_choro_map <- visualization$cat_choro_map
viz$nominal_choro_map <- visualization$nominal_choro_map
viz$normal_choro_map <- visualization$normal_choro_map


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






# RUC  ----
call_ruc <- function(year) {
  cache_path <- glue(opath$ruc_)
  if (file.exists(cache_path)) {
    df <- readRDS(cache_path)
    log_debug("read from cache {cache_path}")
  } else {
    df <- dataprep_misc$pubdata$ers_ruc() |>
      filter(ruc_year == util$year2rucc(year)) |>
      select(fips, ruc_code)
    saveRDS(df, util$mkdir(cache_path))
    log_debug("save to cache {cache_path}")
  }    
  df
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



# County Health Rankings ----

call_chr <- function(year) {
  
  df <- dataprep_misc$call_chr_raw(year = year) %>%
    `colnames<-`({.[1,]}) %>%
    .[-1, ] %>%
    rename(place = fipscode) %>%
    {.[!grepl('(0)$', .$place), ]} %>%
    select(place,
           paam = v127_rawvalue,
           ypll75 = v001_rawvalue,
           pcp = v004_other_data_1,
           uninsured = v085_rawvalue,
           dentists = v088_other_data_1,
           mhp = v062_other_data_1) %>%
    mutate(across(!place, as.numeric))
  
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



# Unemployment rate ----

call_unemp_rate <- function(year, bus_data = "ers") {
  lf <- call_laborforce(year, bus_data = bus_data)
  emp <- call_employment(year, bus_data = bus_data)
  df <- inner_join(lf, emp, "place") |>
    mutate(unemp_rate = 100 * (1 - employment / laborforce)) |>
    select(place, unemp_rate) |>
    filter(is.finite(unemp_rate))
  df
}  


# Industry structure ----

call_industry_structure <- function(year) {
  if (year != 2012) stop("calculation not verified for years other than 2012")

  cache_path = glue(opath$industry_structure_)
  if (file.exists(cache_path)) {
    df <- readRDS(cache_path)
    log_debug("read from cache {cache_path}")
  } else {
      
    
    x1 <- place_output$call_output(year, class_system = "industry", ilevel = "sec", bus_data = "infogroup")
    x2 <- bea_io$call_bea_use_table(year, ilevel = "sec")[c("V001", "VABAS", "T018"), ] %>%
      t() %>%
      as_tibble(rownames = "indcode") %>%
      rename(emp_comp = V001, value_added = VABAS, total_output = T018) %>%
      mutate(emp_to_va = emp_comp / value_added,
             emp_to_output = emp_comp / total_output,
             va_to_output = value_added / total_output,
             .keep = "unused")
    x3 <- left_join(x1, x2, "indcode")
    
    x4 <- x3 %>%
      mutate(va = va_to_output * output) %>%
      group_by(place) %>%
      summarize(emp_to_va = sum(emp_to_va * va) / sum(va),
                emp_to_output = sum(emp_to_output * output) / sum(output),
                va_to_output = sum(va) / sum(output))
    
    x5 <- x3 %>%
      select(place, indcode, output) %>%
      left_join(summarize(x3, tot_output = sum(output), .by = place), "place") %>%
      mutate(value = output / tot_output, indcode = str_to_lower(indcode)) %>%
      pivot_wider(id_cols = place, names_from = indcode, names_prefix = "output_share_ind_")
    
    df <- inner_join(x4, x5, "place")
    saveRDS(df, util$mkdir(cache_path))
    log_debug("save to cache {cache_path}")
  }    
  return(df)
}


# Data merge  ----


## Space  ----

call_space_df <- function(year){
  df_geo <- call_geog(year)
  df_ruc <- call_ruc(year)
  df <- left_join(df_geo, df_ruc, by = join_by(place == fips)) %>% 
  return(df)
}

## Covariates  ----

call_covariates <- function(year){
  dl <- list(
    call_population(year = year, bus_data = "tidy_acs"),
    call_laborforce_rate(year = year, bus_data = "tidy_acs"),
    call_unemp_rate(year = year, bus_data = "ers"),
    call_income_rate(year = year, bus_data = "bea_profile"),
    call_poverty_rate(year = year, bus_data = "saipe"),
    call_highschool_attainment_rate(year = year, bus_data = "tidy_acs"),
    call_chr(year = year, bus_data = "chr")
  )
  df = dl[[1]]
  for(i in 2:length(dl)){
    df <- full_join(df, dl[[i]], by = "place")
  }
  return(df)
}



## ALL  ----

#' For analytical consistency a single place to call a dataframe for a given year 
call_proj_df <- function(year){
  df <- left_join(call_space_df(year), call_covariates(year),  by = "place")  %>% 
return(df)
}








