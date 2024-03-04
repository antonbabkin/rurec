# Data preparation

# R libraries ----
library(logger)
log_threshold(DEBUG)
library(arrow)
library(tidyverse)
library(glue)

library(tidycensus)
library(readxl)


# R scripts ----
source("R/basic_utilities.R", local = (util <- new.env()))
source("R/dataprep_cbp.R", local = (cbp <- new.env()))
source("R/dataprep_infogroup.R", local = (ig <- new.env()))
source("R/place_output.R", local = (place_output <- new.env()))

# Data objects ----
ipath <- list(
  ig_ = ig$opath$county_,
  bea_econ_profile = "https://apps.bea.gov/regional/zip/CAINC30.zip",
  ers_labor_stats_raw = "https://www.ers.usda.gov/webdocs/DataFiles/48747/Unemployment.csv",
  saipe_raw_ = "https://www2.census.gov/programs-surveys/saipe/datasets/{year}/{year}-state-and-county/est{substr(year, 3, 4)}all.{ext}"
)

opath <- list(
  population = "data/pubdata/population/population.pq",
  bea_econ_profile_raw = "data/bea/raw/CAINC30__ALL_AREAS_1969_2022.csv",
  bea_econ_profile = "data/bea/CAINC30__ALL_AREAS_1969_2022.pq",
  econ_dynam_ind_ = "data/econ_dynam/econ_dynam_ind_{year}.rds",
  ers_labor_stats_raw = "data/ers/unemployemnt.pq",
  tidy_acs_stats_raw_ = "data/tidy_acs/{survey}/{geography}/{variables}/{year}.pq",
  saipe_raw_ = "data/saipe/{year}.pq"
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

# Utility functions----

# find simple growth rates between two years 
# takes functions with year input parameter
# anticipated output from input function: data.frame("place", "level_variable") 
# optional function parameters can be applied in the "..." space
growth_rate <- function(
    start_year, 
    end_year, 
    function_name, 
    ...){
  df <- util$temp_fun_recur_list(
    set_of_years = c(start_year, end_year),
    function_name = function_name, 
    ...) %>% 
    {inner_join(.[[1]], .[[2]], by = "place")} %>% 
    na.omit() %>% 
    mutate(gr = {((.[[3]] - .[[2]])/.[[2]])*100})  %>%
    {.[c(1, 4)]} %>%
    `colnames<-`(c("place", "grow_rate"))
  return(df)
}

entry_rate <- function(entry_t, est_t, est_l){
  df <- (2*entry_t/(est_l + est_t))*100
  return(df)
}

exit_rate <- function(exit_l, est_t, est_l){
  df <- (2*exit_l/(est_l + est_t))*100
  return(df)
}

# Surveys ----

# TODO: add Quarterly Census of Employment and Wages, American Community Survey, Current Population Survey, National Agricultural Workers Survey, Agricultural Resource Management Survey, Census of Agriculture, Farm Labor Survey
# e.g., "https://data.bls.gov/cew/data/files/2023/xls/2023_all_county_high_level.zip"

call_bea_econ_profile_raw <- function() {
  raw_path <- opath$bea_econ_profile_raw
  # download raw data if needed
  if (file.exists(raw_path)) {
    log_debug("raw data found at {raw_path}")
    return(read.csv(raw_path))
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


# Civilian_labor_force, Employed, Unemployed, and Unemployment_rate from 2000:2022 
# Plus Median_Household_Income_2021, Med_HH_Income_Percent_of_State_Total_2021, Rural_Urban_Continuum_Code_2013, Urban_Influence_Code_2013, and Metro_2013
call_ers_labor_stats_raw <- function() {
  cache_path <- glue(opath$ers_labor_stats_raw)
  if (file.exists(cache_path)) {
    log_debug(paste("read from cache", cache_path))
    return(read_parquet(cache_path))
  }
  
  parent_path <- util$mkdir(file.path(dirname(cache_path), basename(ipath$ers_labor_stats_raw)))
  download_status <- download.file(url = ipath$ers_labor_stats_raw, 
                                   destfile = parent_path, 
                                   mode = "wb")
  stopifnot(download_status == 0)
  log_debug("data dowloaded to {parent_path}")
  df <- read_csv(parent_path, show_col_types = FALSE) %>% 
    mutate(FIPS_Code = sprintf("%05d", FIPS_Code))
  
  log_debug(paste("save to cache", cache_path))
  write_parquet(df, util$mkdir(cache_path))
  return(df)
}

call_ers_county_stats <- function(year,
                                  metric = c("Civilian_labor_force", "Employed", "Unemployed", "Unemployment_rate")){
  gyear <- util$year2ers_labor(year) %>% 
    {paste0("(", ., ")$")}
  gmetric <- match.arg(metric) %>% 
    {paste0("(^", ., ")")}
  df <- call_ers_labor_stats_raw() %>% 
    rename(place = FIPS_Code) %>% 
    {.[!grepl('(0)$', .$place), ]} %>% 
    {.[grepl(gyear, .$Attribute), ]} %>% 
    {.[grepl(gmetric, .$Attribute), ]}
  return(df)
}

call_tidy_acs_county_stats <- function(
    year,
    variables,
    geography = "county",
    survey = c("acs5", "acs1")){
  survey <- match.arg(survey)
  if (survey == "acs5"){
    year <- util$year2tidy_acs5(year)
  }
  if (survey == "acs1"){
    year <- util$year2tidy_acs1(year)
  }
  cache_path <- glue(opath$tidy_acs_stats_raw_)
  if (file.exists(cache_path)) {
    log_debug(paste("read from cache", cache_path))
    return(read_parquet(cache_path))
  }
  
  df <- get_acs(geography = geography,
                variables = variables,
                year = year,
                survey = survey)
  
  log_debug(paste("save to cache", cache_path))
  write_parquet(df, util$mkdir(cache_path))
  
  return(df)
}


call_saipe_raw <- function(year) {
  year <- util$year2saipe(year)
  if (year > 2002){ext = "xls"} else {ext = "dat"}
  cache_path <- glue(opath$saipe_raw_)
  if (file.exists(cache_path)) {
    log_debug(paste("read from cache", cache_path))
    return(read_parquet(cache_path))
  }
  tf <- tempfile()
  download_status <- download.file(url = glue(ipath$saipe_raw_), 
                                   destfile = tf, 
                                   mode = "wb")
  stopifnot(download_status == 0)
  if (ext == "xls"){
    df <- read_excel(tf) %>% suppressMessages()
  } else if (ext == "dat"){
    df <- read.table(tf, fill = TRUE)
  } 
  log_debug(paste("save to cache", cache_path))
  write_parquet(df, util$mkdir(cache_path))
  return(df)  
}

call_saipe_county <- function(year) {
  df <- call_saipe_raw(year = year)
    if (grep("^0", df[[1]])[1] != 1 ) {
      df <- df %>%
        .[(grep("^0", .[[1]])[1]-1):nrow(.),] %>% 
        `colnames<-`(.[1,]) %>% 
        .[-1, ]
    }
    if (any(is.na(df[[1]]))){
      df <- df %>% 
        .[(1:which(is.na(.[[1]]))-1),]
    }
  df[[1]] = sprintf("%02s", df[[1]])
  df[[2]] = sprintf("%03s", df[[2]])
  df <- df %>% 
    {.[!grepl('(0)$', .[[2]]), ]} %>% 
    {.[, 1:25]}
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

call_ers_county_employment <- function(year) {
  df <- call_ers_county_stats(
    year = year,
    metric = "Employed") %>% 
    {.[c("place", "Value")]} %>% 
    `colnames<-`(c("place", "employment"))
  return(df)
}

call_county_employment <- function(
    year,
    bus_data = c("cbp_imp", "cbp_raw", "infogroup", "bea_profile", "ers")
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
  if(bus_data == "ers"){
    df <- call_ers_county_employment(year)
  }
  return(df)
}

# Unemployment ----

call_ers_county_unemployment <- function(year) {
  df <- call_ers_county_stats(
    year = year,
    metric = "Unemployed") 
  {.[c("place", "Value")]} %>% 
    `colnames<-`(c("place", "unemployment"))
  return(df)
}

call_county_unemployment <- function(
    year,
    bus_data = c("ers")
){
  if(bus_data == "ers"){
    df <- call_ers_county_unemployment(year)
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

call_infogroup_county_estab <- function(year) {
  df <- glue(ig$opath$county_, .envir = list(year = year)) %>%
    open_dataset() %>%
    collect() %>% 
    na.omit() %>% 
    arrange(st, cty) %>% 
    mutate(place = {paste0(.$st, .$cty)}) %>% 
    {.[c("place", "est")]} %>% 
    {aggregate(.$est, list(.$place), FUN=sum)} %>% 
    `colnames<-`(c("place", "establishments")) 
  return(df)
}

call_county_establishments <- function(
    year,
    bus_data = c("cbp_imp", "cbp_raw", "infogroup")
    ){
  bus_data <- match.arg(bus_data)
  if(bus_data == "cbp_imp"){
    df <- call_cbp_county_estab_efcy(year)
  }
  if(bus_data == "cbp_raw"){
    df <- call_cbp_county_estab(year)
  }
  if(bus_data == "infogroup"){
    df <- call_infogroup_county_estab(year)
  }
  return(df)
}

# Labor force  ----

call_ers_county_laborforce <- function(year) {
  df <- call_ers_county_stats(
    year = year,
    metric = "Civilian_labor_force") %>% 
  {.[c("place", "Value")]} %>% 
    `colnames<-`(c("place", "laborforce"))
  return(df)
}

call_county_laborforce <- function(
    year,
    bus_data = c("ers") ){
  bus_data <- match.arg(bus_data)
  if(bus_data == "ers"){
    df <- call_ers_county_laborforce(year)
  }
  return(df)
}

# Labor force participation rate   ----

call_tidy_acs_county_laborforce_rate <- function(year) {
  df <- call_tidy_acs_county_stats(
    year = year,
    variables = "S2301_C02_001E",
    geography = "county",
    survey = "acs5") %>% 
    {.[c("GEOID", "estimate")]} %>% 
    `colnames<-`(c("place", "laborforce_part_rate"))
  return(df)
}

call_county_laborforce_rate <- function(
    year,
    bus_data = c("tidy_acs") ){
  bus_data <- match.arg(bus_data)
  if(bus_data == "tidy_acs"){
    df <- call_tidy_acs_county_laborforce_rate(year)
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
    mutate_at(2, as.numeric) %>% 
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

# Poverty ----

call_saipe_county_poverty <- function(year) {
  df <- call_saipe_county(year = year) 
  if (year > 2002){
    df <- df[, c(1, 2, 5)]
  } else {
    df <- df[, 1:3]
  }
  df <- df %>% 
    `colnames<-`(c("st", "cty", "poverty")) %>% 
    mutate(place = {paste0(.$st, .$cty)}) %>% 
    {.[c("place", "poverty")]}
  return(df)
}

call_county_poverty <- function(
    year,
    bus_data = c("saipe") ){
  bus_data <- match.arg(bus_data)
  if(bus_data == "saipe"){
    df <- call_saipe_county_poverty(year)
  }
  return(df)
}

# Output  ----

call_county_output <- function(
    year, 
    class_system = c("industry", "commodity"), 
    ilevel = c("det", "sum", "sec"),
    bus_data = c("cbp_imp", "cbp_raw", "infogroup")){
  df <- place_output$call_output(
    year = year, 
    class_system = class_system, 
    ilevel = ilevel,
    bus_data = bus_data) %>% 
    {aggregate(.$output, list(.$place), FUN=sum)} %>% 
    `colnames<-`(c("place", "output"))
  return(df)
}

# Dynamism ----

call_infogroup_county_entry <- function(year) {
  df <- glue(ig$opath$county_, .envir = list(year = year)) %>%
    open_dataset() %>%
    collect() %>% 
    na.omit() %>% 
    arrange(st, cty) %>% 
    mutate(place = {paste0(.$st, .$cty)}) %>% 
    {.[c("place", "entry")]} %>% 
    {aggregate(.$entry, list(.$place), FUN=sum)} %>% 
    `colnames<-`(c("place", "entry")) 
  return(df)
}

call_infogroup_county_exit <- function(year) {
  df <- glue(ig$opath$county_, .envir = list(year = year)) %>%
    open_dataset() %>%
    collect() %>% 
    na.omit() %>% 
    arrange(st, cty) %>% 
    mutate(place = {paste0(.$st, .$cty)}) %>% 
    {.[c("place", "exit")]} %>% 
    {aggregate(.$exit, list(.$place), FUN=sum)} %>% 
    `colnames<-`(c("place", "exit")) 
  return(df)
}

call_county_entry <- function(
    year,
    bus_data = c("infogroup")){
  bus_data <- match.arg(bus_data)
  if(bus_data == "infogroup"){
    df <- call_infogroup_county_entry(year)
  }
  return(df)
}

call_county_exit <- function(
    year,
    bus_data = c("infogroup")){
  bus_data <- match.arg(bus_data)
  if(bus_data == "infogroup"){
    df <- call_infogroup_county_exit(year)
  }
  return(df)
}

call_infogroup_county_entry_rate <- function(year){
  entry_t <- call_infogroup_county_entry(year = year)
  est_t <- call_infogroup_county_estab(year = year)
  est_l <- call_infogroup_county_estab(year = (year-1))
  df <- inner_join(est_t, est_l, by = "place") %>% 
    inner_join(., entry_t, by = "place") %>% 
    mutate(entry_rate = {entry_rate(entry_t = .[[4]], est_t = .[[2]], est_l = .[[3]])}) %>% 
    {.[c("place", "entry_rate")]}
  return(df)
}


call_infogroup_county_exit_rate <- function(year){
  exit_l <- call_infogroup_county_exit(year = (year-1))
  est_t <- call_infogroup_county_estab(year = year)
  est_l <- call_infogroup_county_estab(year = (year-1))
  df <- inner_join(est_t, est_l, by = "place") %>% 
    inner_join(., exit_l, by = "place") %>% 
    mutate(exit_rate = {exit_rate(exit_l = .[[4]], est_t = .[[2]], est_l = .[[3]])}) %>% 
    {.[c("place", "exit_rate")]}
  return(df)
}

call_county_entry_rate <- function(
    year,
    bus_data = c("infogroup")){
  bus_data <- match.arg(bus_data)
  if(bus_data == "infogroup"){
    df <- call_infogroup_county_entry_rate(year)
  }
  return(df)
}

call_county_exit_rate <- function(
    year,
    bus_data = c("infogroup")){
  bus_data <- match.arg(bus_data)
  if(bus_data == "infogroup"){
    df <- call_infogroup_county_exit_rate(year)
  }
  return(df)
}



# Growth Rates  ----

# test <- growth_rate(start_year = 2010, end_year = 2015, function_name = call_county_population,  bus_data = "census")
# 
# test <- growth_rate(start_year = 2010, end_year = 2015, function_name = call_county_employment,  bus_data = "infogroup")
# 
# test <- growth_rate(start_year = 2010, end_year = 2015, function_name = call_county_establishments,  bus_data = "infogroup")
# 
# test <- growth_rate(start_year = 2010, end_year = 2015, function_name = call_county_income,  bus_data = "bea_profile")
# 
# test <- growth_rate(start_year = 2010, end_year = 2015, function_name = call_county_output,  bus_data = "infogroup", class_system = "commodity", ilevel = "det")


# All indicators  ----


# call_econ_dynam_ind <- function(
    #     year){
#   cache_path <- glue(opath$econ_dynam_ind_)
#   if (file.exists(cache_path)) {
#     log_debug(paste("read from cache", cache_path))
#     return(readRDS(cache_path))
#   }
#   tmp <- list(
#     call_county_population(year = year, bus_data = "census"),
#     call_county_employment(year = year, bus_data = "infogroup"),
#     call_county_establishments(year = year, bus_data = "infogroup"),
#     call_county_output(year = year, bus_data = "infogroup", class_system = "commodity", ilevel = "det"),
#     call_county_entry(year = year, bus_data = "infogroup"),
#     call_county_exit(year = year, bus_data = "infogroup"),
#     call_county_entry_rate(year = year, bus_data = "infogroup"),
#     call_county_exit_rate(year = year, bus_data = "infogroup")
#     )
#   df = tmp[[1]]
#   for(i in 2:length(tmp)){
#     df <- full_join(df, tmp[[i]], by = "place")
#   }
#   log_debug(paste("save to cache", cache_path))
#   saveRDS(df, util$mkdir(cache_path))
#   return(df)
# }


# TODO: refactor growth rate indicator naming process
call_econ_dynam_ind <- function(
    year,
    growth_rate_start_year = year,
    growth_rate_end_year = (year-5)){
  cache_path <- glue(opath$econ_dynam_ind_)
  if (file.exists(cache_path)) {
    log_debug(paste("read from cache", cache_path))
    return(readRDS(cache_path))
  }
  tmp <- list(
    call_county_population(year = year, bus_data = "census"),
    call_county_laborforce(year = year, bus_data = "ers"),
    call_county_employment(year = year, bus_data = "infogroup"),
    call_county_establishments(year = year, bus_data = "infogroup"),
    call_county_output(year = year, bus_data = "infogroup", class_system = "commodity", ilevel = "det"),
    call_county_entry(year = year, bus_data = "infogroup"),
    call_county_exit(year = year, bus_data = "infogroup"),
    call_county_entry_rate(year = year, bus_data = "infogroup"),
    call_county_exit_rate(year = year, bus_data = "infogroup"),
    growth_rate(start_year = growth_rate_start_year, end_year = growth_rate_end_year, function_name = call_county_population,  bus_data = "census") %>% `colnames<-`(c("place", paste0(tail(strsplit(deparse(substitute(call_county_population)), split = "_")[[1]], 1), "_grow_rate"))),
    growth_rate(start_year = growth_rate_start_year, end_year = growth_rate_end_year, function_name = call_county_employment,  bus_data = "infogroup")  %>% `colnames<-`(c("place", paste0(tail(strsplit(deparse(substitute(call_county_employment)), split = "_")[[1]], 1), "_grow_rate"))),
    growth_rate(start_year = growth_rate_start_year, end_year = growth_rate_end_year, function_name = call_county_establishments,  bus_data = "infogroup") %>% `colnames<-`(c("place", paste0(tail(strsplit(deparse(substitute(call_county_establishments)), split = "_")[[1]], 1), "_grow_rate"))),
    growth_rate(start_year = growth_rate_start_year, end_year = growth_rate_end_year, function_name = call_county_output,  bus_data = "infogroup", class_system = "commodity", ilevel = "det") %>% `colnames<-`(c("place", paste0(tail(strsplit(deparse(substitute(call_county_output)), split = "_")[[1]], 1), "_grow_rate")))
  )
  df = tmp[[1]]
  for(i in 2:length(tmp)){
    df <- full_join(df, tmp[[i]], by = "place")
  }
  log_debug(paste("save to cache", cache_path))
  saveRDS(df, util$mkdir(cache_path))
  return(df)
}








