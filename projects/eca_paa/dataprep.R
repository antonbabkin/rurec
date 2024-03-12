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

# paths ----

# many inputs are required indirectly through sourced scripts
ipath <- list(
)

opath <- list(
  geog_ = "data/projects/eca_paa/geog/{year}.rds",
  ruc_ = "data/projects/eca_paa/ers_ruc/{year}.rds",
  cbsa_conc_ = "data/projects/eca_paa/cbsa_conc/{year}.rds",
  cbsa_delin_ = "data/projects/eca_paa/cbsa_delin/{year}.rds",
  cbsa_ = "data/projects/eca_paa/cbsa/{year}.rds",
  eca_df = "data/projects/eca_paa/eca.rds",
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
  net_migration = "data/projects/eca_paa/net_migration.rds"
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
  call_net_migration()
}

#' clean up a df for better time with visualizations
strip_dataframe <- function(df){
  df <- df %>% 
    st_drop_geometry() %>% 
    select(where(is.numeric)) %>% 
    na.omit()
  return(df)
}

round_stats <- function(reg_object,
                        stat_list = c("coefficients", "std.error", "statistic", "p.value", "conf.low", "conf.high"),
                        round_level = 6){
  round_level = 6
  l = c("coefficients", "std.error", "statistic", "p.value", "conf.low", "conf.high")
  for (i in stat_list){
    reg_object[[i]] <- round(reg_object[[i]], round_level)
  }
  return(reg_object)
}

temporal_permutations <- function(year_range){
  df <- c(year_range) %>% 
    expand_grid(., .) %>% 
    .[.[1] < .[2], ]
  return(df)
}


# Data Viz functions ----

viz <- new.env()
viz$diverge_choro_map <- visualization$diverge_choro_map
viz$density_dist_plot <- visualization$density_dist_plot
viz$cat_choro_map <- visualization$cat_choro_map
viz$nominal_choro_map <- visualization$nominal_choro_map
viz$normal_choro_map <- visualization$normal_choro_map
viz$absorption_density_plot <- visualization$absorption_density_plot


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
  stop("Use call_cbsa()")
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
  stop("Use call_cbsa()")
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


# CBSA  ----

call_cbsa <- function(year) {
  year = util$year2cbsa(year)
  cache_path = glue(opath$cbsa_)
  if (file.exists(cache_path)) {
    df <- readRDS(cache_path)
    log_debug("read from cache {cache_path}")
  } else {
    df <- geography$pubdata$get_cbsa_delin_df(year) %>% 
      mutate(place = {paste0(.$STATE_CODE, .$COUNTY_CODE)}) %>% 
      mutate(CBSA_TITLE = {paste(str_split_i(.$CBSA_TITLE, ",", 1), "CBSA")}) %>% 
      {.[c("place", "CBSA_CODE", "CBSA_TITLE", "METRO_MICRO", "CENTRAL_OUTLYING")]} 
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

call_wage <- function(year, bus_data = "cbp_raw") {
  pay <- call_payroll(year = year, bus_data = bus_data)
  emp <- call_employment(year = year, bus_data = bus_data)
  df <- inner_join(pay, emp, "place") |>
    mutate(wage = 1000 * payroll / employment) |>
    select(place, wage) |>
    filter(is.finite(wage))
  df
}



# Establishment entry & exit ----

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


call_entry_rate <- function(year,
                            bus_data = c("bds", "infogroup")) {
  bus_data <- match.arg(bus_data)
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

call_exit_rate <- function(year,
                           bus_data = c("bds", "infogroup")) {
  bus_data <- match.arg(bus_data)
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


# Net migration ----

call_net_migration <- function() {
  cache_path <- opath$net_migration
  if (file.exists(cache_path)) {
    df <- readRDS(cache_path)
    log_debug("read from cache {cache_path}")
  } else {
    df <- dataprep_misc$call_net_migration()
    saveRDS(df, util$mkdir(cache_path))
    log_debug("save to cache {cache_path}")
  }    
  df
}


# Data merge  ----


## Space  ----

call_space_df <- function(year){
  df_geo <- call_geog(year)
  df_cbsa <- call_cbsa(year)
  df_ruc <- call_ruc(year)
  df <- left_join(df_geo, df_cbsa, by = "place") %>% 
    replace_na(list(CBSA_CODE = "rural", 
                    CBSA_TITLE = "rural", 
                    CENTRAL_OUTLYING = "rural", 
                    METRO_MICRO = "rural")) %>% 
    mutate(cbsa_rural_category = factor(CBSA_CODE == "rural", labels = c("nonrural", "rural")) ) %>% 
    {left_join(., df_ruc, by = join_by(place == fips))} %>% 
    group_by(CBSA_CODE) %>% mutate(cbsa_members_count = n(), .after = CBSA_CODE) %>% ungroup() 
  return(df)
}

## Prosperity  ----

call_prosperity_outcomes <- function(year){
  dl <- list(
    call_population(year = year, bus_data = "tidy_acs"),
    call_employment(year = year, bus_data = "ers"),
    call_jobs(year = year, bus_data = "cbp_imp"),
    call_laborforce_rate(year = year, bus_data = "tidy_acs"),
    call_unemp_rate(year = year, bus_data = "ers"),
    call_payroll(year = year, bus_data = "cbp_raw"),
    call_wage(year = year, bus_data = "cbp_raw"),
    call_income_rate(year = year, bus_data = "bea_profile"),
    call_establishments(year = year, bus_data = "cbp_raw"),
    call_entry_rate(year = year, bus_data = "bds"),
    call_exit_rate(year = year, bus_data = "bds"),
    call_gdp(year = year, bus_data = "bea_rea"),
    call_gross_output(year = year, bus_data = "cbp_imp"),
    call_intermediate_supply(year = year, bus_data = "cbp_imp"),
    call_intermediate_demand(year = year, bus_data = "cbp_imp"),
    call_net_supply(year = year, bus_data = "cbp_imp"),
    call_net_demand(year = year, bus_data = "cbp_imp"),
    call_poverty_rate(year = year, bus_data = "saipe"),
    call_highschool_attainment_rate(year = year, bus_data = "tidy_acs"),
    call_ypll75(year = year, bus_data = "chr")
  )
  df = dl[[1]]
  for(i in 2:length(dl)){
    df <- full_join(df, dl[[i]], by = "place")
  }
  return(df)
}

## Spatial ECA  ----

call_eca_space_df <- function(year){
  df_space <- call_space_df(year)
  df_eca <- call_eca_df() %>% 
    rename(place = fips)
  df <- left_join(df_space, df_eca,  by = "place")  %>%
    na.omit() %>% 
    group_by(eca_membership) %>% 
    mutate(cluster_members_count = n(), .after = eca_membership) %>% 
    ungroup() 
  df <- df %>% 
    {.[which(.$place  == .$eca_membership), ]} %>% 
    select(place, CBSA_CODE) %>% 
    st_drop_geometry() %>% 
    `colnames<-`(c("eca_membership", "cbsa_of_eca")) %>% 
    {left_join(df, . , by = "eca_membership")} %>% 
    relocate(cbsa_of_eca, .after = eca_membership)
  return(df)
}

## ALL  ----

#' For analytical consistency a single place to call a dataframe for a given year 
call_proj_df <- function(year){
  df <- left_join(call_eca_space_df(year), call_prosperity_outcomes(year),  by = "place")  %>% 
    mutate(eca_cbsa_xtab = str_to_title(paste(cbsa_rural_category, str_split_i(eca_cluster_category, " ", -1))), .after = eca_cluster_category) %>% 
    mutate(eca_central_out_xtab = str_to_title(paste(CENTRAL_OUTLYING, str_split_i(eca_cluster_category, " ", -1))), .after = eca_cluster_category) %>% 
    mutate(sink_filter = ifelse(cluster_members_count == 1, "sink/source",
                                ifelse(place %in% max_trade_place, "sink/sink",
                                       "source/source")), .before = eca_membership) %>% 
    mutate(cbsa_by_eca = ifelse(eca_central_out_xtab == "Central Source", "only CBSA", 
                                ifelse(eca_central_out_xtab == "Central Sink", "ECA and CBSA", 
                                       ifelse(eca_central_out_xtab %in% c("Rural Sink", "Outlying Sink"), "only ECA", 
                                              NA))), .before = eca_membership)
return(df)
}

# TODO: optional spatial toggle 
#' For analytical consistency a single place to call a dataframe for a set of years
call_temporal_range_df <- function(
    set_of_years, # 2002:2023 or c(2012, 2015, 2017)
    bind = TRUE){
  df <- util$temp_fun_recur_list(set_of_years, call_proj_df)
  if(bind){
    df <- bind_rows(df, .id = "id_year")
  }
  return(df)
}








