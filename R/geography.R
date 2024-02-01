# Geographic dataframes and impedance matrices

# R libraries ----
library(logger)
log_threshold(DEBUG)
library(arrow)
library(tidyverse)
library(glue)
library(sf)
library(units)
# library(geosphere)
library(spdep)

#turn off S2 spherical geometry
sf_use_s2(FALSE)



# R scripts ----
source("R/basic_utilities.R", local = (util <- new.env()))

# S3 methods for automatic reticulate conversion of GeoDataFrame and GeoSeries
source("R/reticulate_extras.R")



# Python modules ----
pymod <- new.env()
pymod$initialized <- FALSE

#' Initialize environment with necessary Python modules imported through reticulate
#' Running multiple times is safe and only imports once.
pymod$init <- function() {
  if (pymod$initialized) return()
  library(reticulate)
  use_condaenv("rurec")
  pymod$geo <- import("rurec.pubdata.geography")
  pymod$cbsa <- import("rurec.pubdata.geography_cbsa")
  pymod$initialized <- TRUE
}


# Data objects ----
ipath <- list(
  county_raw_ = "data/pubdata/source/geo/county/{year}_{scale}.zip",
  county_ = "data/pubdata/geo/county.pq/{year}/{scale}.pq",
  state_raw_ = "data/pubdata/source/geo/state/{filename}.zip",
  state_ = "data/pubdata/geo/state/{scale}.pq",
  cbsa_delin_ = "data/pubdata/source/geography_cbsa/delin/{year}.{ext}"
)

opath <- list(
  county_ = "data/geo/county/{year}_{scale}_{geometry}.rds",
  state_ = "data/geo/state/{scale}_{geometry}.rds",
  cbsa_delin_ = "data/geo/cbsa/delin_{year}.rds",
  cbsa_shapes_ = "data/geo/cbsa/shape_{year}.rds",
  dist_mat_ = "data/geo/dist/{from}_{year}_cbsa{cbsa}.rds",
  bprox_mat_ = "data/geo/dist/bprox_{year}_cbsa{cbsa}_queen{queen}.rds",
  dist_neighbors_ = "data/geo/dist/neighbors/{central_place}.rds"
)

clear_outputs <- function() {
  util$clear_paths(opath)
}


# Pubdata ----
pubdata <- new.env()

pubdata$get_county_df <- function(year, geometry, scale) {
  year <- as.integer(year)
  p <- glue(opath$county_)
  if (file.exists(p)) {
    log_debug(paste("read from cache", p))
    return(readRDS(p))
  }
  pymod$init()
  x <- pymod$geo$get_county_df(year, geometry, scale)
  log_debug(paste("save to cache", p))
  # files suspiciously small, watch out for issues
  saveRDS(x, util$mkdir(p))
  return(x)
}

pubdata$get_state_df <- function(geometry,
                                 scale = c("20m", "5m", "500k", "tiger")) {
  scale <- match.arg(scale)
  p <- glue(opath$state_)
  if (file.exists(p)) {
    log_debug(paste("read from cache", p))
    return(readRDS(p))
  }
  pymod$init()
  x <- pymod$geo$get_state_df(geometry, scale)
  log_debug(paste("save to cache", p))
  saveRDS(x, util$mkdir(p))
  return(x)
}

pubdata$get_cbsa_delin_df <- function(year) {
  year <- as.integer(year)
  p <- glue(opath$cbsa_delin_)
  if (file.exists(p)) {
    log_debug(paste("read from cache", p))
    return(readRDS(p))
  }
  pymod$init()
  x <- pymod$cbsa$get_cbsa_delin_df(year) %>%
    util$reticulate_unlist_cols()
  log_debug(paste("save to cache", p))
  saveRDS(x, util$mkdir(p))
  return(x)
}

# R functions ----

## shapes functions----

# Spatial union each CBSA member in a cluster
cbsa_spatial_cluster <- function(spatial_dataframe,
                                 cbsa_concordance,
                                 verbose = FALSE){
  sdf <- spatial_dataframe
  cbsa_conc <- cbsa_concordance
  j <- cbsa_conc %>% 
    {.[.$place %in% intersect(.$place, sdf$place), ]} %>% 
    {data.frame(CBSA_CODE = c(.$CBSA_CODE, setdiff(sdf$place, .$place)), 
                place = c(.$place, setdiff(sdf$place, .$place)),
                CBSA_TITLE = c(.$CBSA_TITLE, sdf$COUNTY[sdf$place %in% setdiff(sdf$place, .$place)])) }%>% 
    {.[order(.$CBSA_CODE), ]} %>% 
    `rownames<-`(1:nrow(.))  %>% 
    {inner_join(., sdf, by = "place", copy = TRUE)} 
  df <- j %>% 
    select(CBSA_CODE, CBSA_TITLE) %>% 
    distinct(CBSA_CODE, .keep_all = TRUE) %>% 
    rename(place = CBSA_CODE)
  df$geometry <- df$geometry %>% st_transform("EPSG:26911")
  x <- unique(cbsa_conc$CBSA_CODE) 
  for (i in x){
    if(verbose){print(paste("start cluster: ", i, which(i == x), "of", length(x), Sys.time()))}
    df[df$place == i, ]$geometry <- j[j$CBSA_CODE == i, ] %>% st_transform("EPSG:26911") %>% st_union()
    if(verbose){print(paste("end cluster: ", i, which(i == x), "of", length(x), Sys.time()))}
  }
  df$geometry <- df$geometry %>% st_transform(st_crs(sdf)[[1]]) 
  df$center <- df$geometry %>% st_transform("EPSG:26911") %>% st_centroid() %>% st_transform(st_crs(sdf)[[1]]) 
  return(df)
}

## shapes files----

# Call up and clean TIGER data
# shapefile formats are available for 2020:2013, 2010, 2000, 1990
call_tiger <- function(year = 2013,
                       scale = c("20m", "500k", "5m"),
                       geometry = TRUE) {
  scale <- match.arg(scale)
  df <- pubdata$get_county_df(util$year2tiger(year), 
                              geometry, 
                              scale) %>%
    rename(place = CODE)
  df$COUNTY <- paste(df$NAME, "County")
  if(isTRUE(geometry)){
    #df$center <- st_centroid(df$geometry)
    df$center <- df$geometry %>% st_transform("EPSG:26911") %>% st_centroid() %>% st_transform(st_crs(df)[[1]]) 
  }
  st <- pubdata$get_state_df(geometry = FALSE) %>%
    select(1:3) 
  names(st) <- c("STATE_CODE", "STATE_NAME", "STATE")
  df <- left_join(df, st, by = "STATE_CODE") %>%
    arrange(place)
  return(df)
}


# Call up and clean CBSA concordance codes (Delineations available for years 2003:2009, 2013, 2015, 2017, 2018, 2020)
call_cbsa_concord <- function(year){
  cbsa_year <- util$year2cbsa(year)  
  df <- pubdata$get_cbsa_delin_df(cbsa_year)
  df$CBSA_TITLE <- sapply(strsplit(df$CBSA_TITLE, ","), "[", 1)
  df$CBSA_TITLE <- paste(df$CBSA_TITLE, rep("CBSA", length(df$CBSA_TITLE)))
  df$place <- paste0(df$STATE_CODE, df$COUNTY_CODE)
  df <- df %>% select(CBSA_CODE, place, CBSA_TITLE)
  return(df)
}

# Call geography and aggregate spatial features of each CBSA member in a cluster
# (low priority todo) can be rewritten to use pymod$cbsa$get_cbsa_shape_df()
call_cbsa_spatial_cluster <- function(year = 2013,
                                      scale = c("20m", "5m", "500k"),
                                      verbose = FALSE) {
  scale <- match.arg(scale)
  
  cache_path <- glue(opath$cbsa_shapes_)
  if (file.exists(cache_path)) {
    log_debug(paste("read from cache", cache_path))
    return(readRDS(cache_path))
  }
  sdf <- call_tiger(year = year, 
                    scale = scale)
  cbsa_conc <- call_cbsa_concord(year = year)
  df <- cbsa_spatial_cluster(spatial_dataframe = sdf, 
                             cbsa_concordance = cbsa_conc, 
                             verbose = verbose)
  
  log_debug(paste("save to cache", cache_path))
  saveRDS(df, util$mkdir(cache_path))
  return(df)
}

### Call geographic features
call_geog <- function(year = 2013,
                      scale = c("20m", "5m", "500k"),
                      cbsa = FALSE,
                      verbose = FALSE){
  scale <- match.arg(scale)
  if (cbsa){
    df <- call_cbsa_spatial_cluster(year = year, 
                                    scale = scale, 
                                    verbose = verbose)
  } else {
    df <- call_tiger(year = year, 
                     scale = scale)
  }
  return(df)
}

## place codes/concordance ----

# Convert a fips code into a cbsa code 
fips2cbsa <- function(fips,
                      year = 2013){
  cb <- call_cbsa_concord(year)
  counties <- util$year2tiger(year) %>% 
    {pubdata$get_county_df(., FALSE, "20m")}
  if (isFALSE(fips %in% counties$CODE)) {
    warning("FIPS entry [",fips,"] not found in ANSI FIPS records\n See: https://www2.census.gov/geo/docs/reference/codes/national_county.txt")
  }
  if (isTRUE(fips %in% cb$place)) return(cb$CBSA_CODE[fips == cb$place])
  else return(fips)
}

# Get county name from FIPS code
fips2name <- function(fips,
                      year = 2013,
                      long = FALSE){
  df <- call_tiger(year = year, scale = "20m", geometry = F)
  if (long) {
    df <- paste0(df$COUNTY[df$place == fips],", ", df$STATE_NAME[df$place == fips])
  } else {
    df <- df$NAME[df$place == fips]
  }
  return(df)
}


## distance functions----

# generate a distance matrix
dist_mat <- function(spatial_dataframe, 
                     from = c("center", "border")) {
  from <- match.arg(from)

  df <- spatial_dataframe
  if (from == "center") x <- df$center
  else if (from == "border") x <- df$geometry
  m <- st_distance(x)
  rownames(m) <- colnames(m) <- df$place

  return(m)
}

# generate a shared border proximity matrix
bprox_mat <- function(spatial_dataframe, 
                      queen = TRUE) {
  sd <- spatial_dataframe 
  df <- sd$geometry %>% 
    poly2nb(queen = queen) %>%
    nb2mat(style = "B", zero.policy = TRUE)
  diag(df) <- 1
  rownames(df) <- colnames(df) <- sd$place
  return(df)
}

## distance matrices----

#' Produce Distance Matrix
#' "border" takes 6+ hours to calculate.
call_dist_mat <- function(year = 2013, 
                          from = c("center", "border"),
                          cbsa = FALSE) {
  from <- match.arg(from)
  
  cache_path <- glue(opath$dist_mat_)
  if (file.exists(cache_path)) {
    log_debug(paste("read from cache", cache_path))
    return(readRDS(cache_path))
  }
  df <- call_geog(year = year, cbsa = cbsa) %>% 
    {dist_mat(spatial_dataframe = ., from = from)}

  log_debug(paste("save to cache", cache_path))
  saveRDS(df, util$mkdir(cache_path))  
  return(df)
}

# # TODO: depreciate
# call_dist_matc <- function(year = 2013, 
#                            cbsa = FALSE) {
#   return(call_dist_mat(year = year, from = "center", cbsa = cbsa))
# }
# 
# # TODO: depreciate
# call_dist_matb <- function(year = 2013, 
#                            cbsa = FALSE) {
#   return(call_dist_mat(year = year, from = "border", cbsa = cbsa))
# }


# Produce Shared Border Matrix
call_bprox_mat <- function(year = 2013, 
                           cbsa = FALSE, 
                           queen = TRUE) {
  cache_path <- glue(opath$bprox_mat_)
  if (file.exists(cache_path)) {
    log_debug(paste("read from cache", cache_path))
    return(readRDS(cache_path))
  }
  
  df <- call_geog(year = year, cbsa = cbsa) %>% 
    {bprox_mat(spatial_dataframe = ., queen = queen)}
  
  log_debug(paste("save to cache", cache_path))
  saveRDS(df, util$mkdir(cache_path))  
  return(df)
}


#TODO: fix with functional form and update more efficient/flexible algorithm 
############ Generate neighbors of neighbors hierarchical vector for a place ad nauseam
#Watch out for Nantucket, MA; Staten Island,NY; and San Juan, WA
# this is slow and can probably be improved by using existing shortest path algorithm
call_neighbor_of_neighbor <- function(central_place,
                                 quiet = TRUE,
                                 ...){
  cache_path <- glue(opath$dist_neighbors_)
  if (file.exists(cache_path)) {
    log_debug(paste("read from cache", cache_path))
    return(readRDS(cache_path))
  }
  
  t <- call_geog(...) %>% 
    .[!grepl('^(02|15|72)', .$place), ]
  df <- data.frame("place" = t$place, 
                   "nn" = vector(mode = "character", length = length(t$place)))
  df$nn[df$place %in% central_place] <- "n0"
  nn <- central_place
  l <- 1
  x <- 0
  while(sum(df$nn=="") != x){
    x = sum(df$nn=="")
    tp <- st_touches(t$geometry, 
                     t[t$place %in% nn, ]$geometry)
    tp <- +as.matrix(tp)
    rownames(tp) <- t$place
    colnames(tp) <- c(nn)
    nx <- setdiff(rownames(tp)[apply(tp, 1, function(x){any(x==1)})], nn)
    df$nn[df$place %in% nx] <- paste0("n",l)
    nn <- c(nn, nx)
    l = l + 1
    if (!quiet) {
      print(paste("Interval level:", l))
      print(paste("Places remaining:", sum(df$nn=="")))
    }
  }
  log_debug(paste("save to cache", cache_path))
  saveRDS(df, util$mkdir(cache_path))  
  return(df)
}

## impedance functions----

# generate a binary proximity distance matrix
prox_impedance_mat <- function(distance_matrix, 
                               radius = 1000){
  x <- distance_matrix
  m <- (x < set_units(radius, mi))
  mode(m) <- "integer"
  rownames(m) <- colnames(m) <- rownames(x)
  return(m)
}

# generate a inverse power decay impedance matrix
power_impedance_mat <- function(distance_matrix,
                                decay_power = 2) {
  x <- distance_matrix |>
    drop_units()
  x <- 1 / (x^decay_power)
  x[is.infinite(x)] <- 1
  return(x)
}

# generate an exponential decay impedance matrix
expo_impedance_mat <- function(distance_matrix,
                               decay_constant = 10000) {
  x <- distance_matrix |>
    drop_units()
  x <- exp(- x / decay_constant)
  return(x)
}

# generate a Gaussian decay impedance matrix
gaus_impedance_mat <- function(distance_matrix,
                               rms_width = 1000) {
  x <- distance_matrix |>
    drop_units()
  rms_width <- set_units(rms_width, mi) |> 
    set_units(m) |> 
    drop_units()
  x <- exp(-.5 * (x / rms_width)^2)
  return(x)
}

# generate a hyperbolic secant decay impedance matrix
hyper_impedance_mat <- function(distance_matrix,
                                decay_constant = 200) {
  x <- distance_matrix |>
    drop_units()
  decay_constant <- set_units(decay_constant, mi) |> 
    set_units(m) |>
    drop_units()
  x <- ((2/(exp(-(x/decay_constant)) + exp(x/decay_constant))))
  return(x)
}

# generate a bi-square decay impedance matrix
bisquare_impedance_mat <- function(distance_matrix,
                                   decay_zero = 1000) {
  x <- distance_matrix |>
    drop_units()
  decay_zero <- set_units(decay_zero, mi) |> 
    set_units(m) |> 
    drop_units()
  m <- (1-(x/decay_zero)^2)^2
  m[x > decay_zero] <- 0
  return(m)
}

# TODO: implement neighbors decay option
# Note: power decay with constant of 1 is functionally the inverse distance decay
# generate family of impedance matrices from single function 
impedance_mat <- function(spatial_dataframe,
                          from = c("center", "border"),
                          functional_form = c("bisquare",
                                              "secant",
                                              "gaussian",
                                              "exponential",
                                              "power",
                                              "distance",
                                              "queen",
                                              "rook",
                                              "neighbor"),
                          scalar_constant,
                          meta = FALSE){
  from <- match.arg(from)
  functional_form <- match.arg(functional_form)
  if(functional_form == "bisquare"){
    if(is.null(scalar_constant)){scalar_constant = 1000}
    dm <- dist_mat(spatial_dataframe = spatial_dataframe, from = from)
    df <- bisquare_impedance_mat(distance_matrix = dm, decay_zero = scalar_constant)
    caption <- paste0("Bi-square Decay: ", scalar_constant," mile limit")
  }
  if(functional_form == "secant"){
    if(is.null(scalar_constant)){scalar_constant = 200}
    dm <- dist_mat(spatial_dataframe = spatial_dataframe, from = from)
    df <- hyper_impedance_mat(distance_matrix = dm, decay_constant = scalar_constant)
    caption <- paste0("Hyperbolic Secant Decay: ", scalar_constant," mile scewness scalar")
  }
  if(functional_form == "gaussian"){
    if(is.null(scalar_constant)){scalar_constant = 1000}
    dm <- dist_mat(spatial_dataframe = spatial_dataframe, from = from)
    df <- gaus_impedance_mat(distance_matrix = dm, rms_width = scalar_constant)
    caption <- paste0("Gaussian Decay: ", scalar_constant," mile RMS scalar")
  }
  if(functional_form == "exponential"){
    if(is.null(scalar_constant)){scalar_constant = 10000}
    dm <- dist_mat(spatial_dataframe = spatial_dataframe, from = from)
    df <- expo_impedance_mat(distance_matrix = dm, decay_constant = scalar_constant)
    caption <- paste0("Exponential Decay: ", scalar_constant," mile disintegration scalar")
  }
  if(functional_form == "power"){
    if(is.null(scalar_constant)){scalar_constant = 2}
    dm <- dist_mat(spatial_dataframe = spatial_dataframe, from = from)
    df <- power_impedance_mat(distance_matrix = dm, decay_power = scalar_constant)
    caption <- paste0("Inverse Power Decay: decay power of ", scalar_constant)
  }
  if(functional_form == "distance"){
    if(is.null(scalar_constant)){scalar_constant = 500}
    dm <- dist_mat(spatial_dataframe = spatial_dataframe, from = from)
    df <- prox_impedance_mat(distance_matrix = dm, radius = scalar_constant)
    caption <- paste0("Proximity Radius: ", scalar_constant," mile limit")
  }
  if(functional_form == "queen"){
    df <- bprox_mat(spatial_dataframe = spatial_dataframe, queen = TRUE)
    caption <- paste0("Queen Adjacent Borders")
  }
  if(functional_form == "rook"){
    df <- bprox_mat(spatial_dataframe = spatial_dataframe, queen = FALSE)
    caption <- paste0("Rook Adjacent Borders")
  }
  if(functional_form == "neighbor"){
    stop("dummy error")
    if(missing(scalar_constant)){scalar_constant = 2}
    df <- neighbor_of_neighbor()
    caption <- paste0("Neighbor Adjacency Hierarchy:", scalar_constant, "degrees of seperation")
  }
  
  if(meta){
    return(list("imatrix" = df, "caption" = caption, "scalar" = scalar_constant))
  } else {
    return(df)
  }
  
}

## impedance matrices----

#' Binary proximity matrix (1 = within radius)
#' @param radius distance in miles
call_prox_impedance_mat <- function(radius = 1000, from = c("center", "border"), year = 2013, cbsa = FALSE) {
  from <- match.arg(from)
  df <- call_dist_mat(year = year, from = from, cbsa = cbsa) %>% 
    {prox_impedance_mat(distance_matrix = ., radius = radius)}
  return(df)
}

# Produce inverse power distance decay impedance matrix
call_power_impedance_mat <- function(decay_power = 2, from = c("center", "border"), year = 2013, cbsa = FALSE) {
  from <- match.arg(from)
  df <- call_dist_mat(year = year, from = from, cbsa = cbsa) %>% 
    {power_impedance_mat(distance_matrix = ., decay_power = decay_power)}
  return(df)
}

# Produce exponential distance decay impedance matrix
call_expo_impedance_mat <- function(decay_constant = 10000, from = c("center", "border"), year = 2013, cbsa = FALSE) {
  from <- match.arg(from)
  df <- call_dist_mat(year = year, from = from, cbsa = cbsa) %>% 
    {expo_impedance_mat(distance_matrix = ., decay_constant = decay_constant)}
  return(df)
}

#' Produce Gaussian distance decay impedance matrix
#' @param rms_width distance in miles
call_gaus_impedance_mat <- function(rms_width = 1000, from = c("center", "border"), year = 2013, cbsa = FALSE) {
  from <- match.arg(from)
  df <- call_dist_mat(year = year, from = from, cbsa = cbsa) %>% 
    {gaus_impedance_mat(distance_matrix = ., rms_width = rms_width)}
  return(df)
}

#' Produce hyperbolic secant distance decay impedance matrix
#' @param decay_constant distance in miles
call_hyper_impedance_mat <- function(decay_constant = 200, from = c("center", "border"), year = 2013, cbsa = FALSE) {
  from <- match.arg(from)
  df <- call_dist_mat(year = year, from = from, cbsa = cbsa) %>% 
    {hyper_impedance_mat(distance_matrix = ., decay_constant = decay_constant)}
  return(df)
}

#' Produce bi-square distance decay
#' @param decay_zero distance in miles
call_bisquare_impedance_mat <- function(decay_zero = 1000, from = c("center", "border"), year = 2013, cbsa = FALSE) {
  from <- match.arg(from)
  df <- call_dist_mat(year = year, from = from, cbsa = cbsa) %>% 
    {bisquare_impedance_mat(distance_matrix = ., decay_zero = decay_zero)}
  return(df)
}

# call family of impedance matrices from single function 
call_impedance_mat <- function(year = 2013,
                               cbsa = FALSE,
                               from = c("center", "border"),
                               functional_form = c("bisquare",
                                                   "secant",
                                                   "gaussian",
                                                   "exponential",
                                                   "power",
                                                   "distance",
                                                   "queen",
                                                   "rook",
                                                   "neighbor"),
                               scalar_constant = NULL,
                               meta = FALSE){
  df <- call_geog(year = year, cbsa = cbsa)
  df <-  impedance_mat(spatial_dataframe = df, 
                       from = from, 
                       functional_form = functional_form,
                       scalar_constant = scalar_constant, 
                       meta = meta)
  return(df)
}

# call tidy table of place specific impedance
call_impedance_distribution_table <- function(central_place,
                                              year = 2013,
                                              cbsa = FALSE,
                                              from = c("center", "border"),
                                              functional_form = c("bisquare",
                                                                  "secant",
                                                                  "gaussian",
                                                                  "exponential",
                                                                  "power",
                                                                  "distance",
                                                                  "queen",
                                                                  "rook",
                                                                  "neighbor"),
                                              scalar_constant = NULL){
  if(cbsa){
    central_place <- fips2cbsa(fips = central_place, 
                                    year = year)
  }
  geot <- call_geog(year = year,
                        cbsa = cbsa)
  imat <- call_impedance_mat(year = year,
                                  cbsa = cbsa,
                                  from = from,
                                  functional_form = functional_form,
                                  scalar_constant = scalar_constant,
                                  meta = TRUE) 
  meta <-  imat[[2]]
  imat <- imat[[1]] %>% 
    as.data.frame.table() %>% 
    `colnames<-`(c("place", "central_place", "impedance"))
  df <- imat %>% 
    {.[.$central_place == central_place, , drop = FALSE]} %>% 
    {inner_join(geot, ., by = "place")}
  return(list(df, meta))
}


# tests ----
test_pubdata <- function() {
  
  for (y in c(2010, 2013:2020)) {
    for (geom in c(TRUE, FALSE)) {
      # not testing smaller scales
      pubdata$get_county_df(y, geom, "20m")
    }
  }
  
  for (scale in c("20m", "5m", "500k", "tiger")) {
    for (geometry in c(TRUE, FALSE)) {
      pubdata$get_state_df(geometry, scale)
    }
  }
  
  for (y in c(2023, 2020, 2018, 2017, 2015, 2013, 2009:2003, 1993)) {
    pubdata$get_cbsa_delin_df(y)
  }
  
  
}
  

test_dataprep <- function() {
  
  for (y in c(2010, 2013:2020)) {
    for (geom in c(TRUE, FALSE)) {
      # not testing smaller scales
      call_tiger(y, "20m", geom)
    }
  }
  
  # does not work for 1993
  for (y in c(2023, 2020, 2018, 2017, 2015, 2013, 2009:2003)) {
    call_cbsa_concord(y)
  }
  
  fips2cbsa("55025", 2020) == "31540"
  
  fips2name("55025") == "Dane"
  fips2name("55025", long = TRUE) == "Dane County, Wisconsin"
  
  # call_cbsa_spatial_cluster(2013) #slow

  # call_dist_matc()
  # # call_dist_matb() #slow, many hours
  # 
  # call_bprox_mat()
  # # neighbor_of_neighbor("55025", quiet = FALSE) #slow
  
  x <- call_prox_impedance_mat()
  x <- call_power_impedance_mat()
  x <- call_expo_impedance_mat()
  x <- call_gaus_impedance_mat()
  x <- call_hyper_impedance_mat()
  x <- call_bisquare_impedance_mat()
}
