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


## shapes ----

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

#Convert a fips code into a cbsa code 
fips2cbsa <- function(fips,
                      year){
  cb <- call_cbsa_concord(year)
  counties <- pubdata$get_county_df(year, FALSE, "20m")
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
  df <- call_tiger(util$year2tiger(year), geometry = F)
  if (long) {
    df <- paste0(df$COUNTY[df$place == fips],", ", df$STATE_NAME[df$place == fips])
  } else {
    df <- df$NAME[df$place == fips]
  }
  return(df)
}


# Call geography and aggregate spatial features of each CBSA member in a cluster
# (low priority todo) can be rewritten to use pymod$cbsa$get_cbsa_shape_df()
cbsa_spatial_cluster <- function(year = 2013) {
  cache_path <- glue(opath$cbsa_shapes_)
  if (file.exists(cache_path)) {
    log_debug(paste("read from cache", cache_path))
    return(readRDS(cache_path))
  }
  
  t <- call_tiger(year)
  c <- call_cbsa_concord(year)
  c <- c[c$place %in% intersect(c$place, t$place),]
  c <- data.frame(CBSA_CODE = c(c$CBSA_CODE, setdiff(t$place, c$place)), 
                  place = c(c$place, setdiff(t$place, c$place)),
                  CBSA_TITLE = c(c$CBSA_TITLE, t$COUNTY[t$place %in% setdiff(t$place, c$place)]))
  c <- c[order(c$CBSA_CODE), ]
  rownames(c) <- 1:nrow(c)
  j <- inner_join(t, c, by = "place", copy = TRUE)
  x <- j$CBSA_CODE %>% unique() %>% .[order(.)]
  df <- j %>% distinct(CBSA_CODE, .keep_all = TRUE) %>% select(CBSA_CODE, CBSA_TITLE)
  for (i in x){
    #print(paste(list_names, "start cluster: ", i, which(i == x), "of", length(x), Sys.time()))
    df$geometry[df$CBSA_CODE == i] <- j %>% filter(j$CBSA_CODE == i) %>% st_union()
    #print(paste(list_names, "  end cluster: ", i, which(i == x), "of", length(x), Sys.time()))
  }
  df$center <- st_centroid(df$geometry)
  
  log_debug(paste("save to cache", cache_path))
  saveRDS(df, util$mkdir(cache_path))
  return(df)
}

### Call geographic features
call_geog <- function(year = 2013,
                      cbsa_clust = FALSE,
                      scale = c("20m", "5m", "500k"),
                      ...){
  scale <- match.arg(scale)
  if (cbsa_clust) {
    df <- cbsa_spatial_cluster(year = year, scale = scale)
  } else {
    df <- call_tiger(year = year, scale = scale)
  }
  return(df)
}


## distance ----

#' Produce Distance Matrix
#' "border" takes 6+ hours to calculate.
dist_mat <- function(from = c("center", "border"), year = 2013, cbsa = FALSE) {
  from <- match.arg(from)
  cache_path <- glue(opath$dist_mat_)
  if (file.exists(cache_path)) {
    log_debug(paste("read from cache", cache_path))
    return(readRDS(cache_path))
  }
  
  df <- call_geog(year, cbsa)
  if (from == "center") x <- df$center
  else if (from == "border") x <- df$geometry
  m <- st_distance(x)
  rownames(m) <- colnames(m) <- df$place
  
  log_debug(paste("save to cache", cache_path))
  saveRDS(m, util$mkdir(cache_path))  
  return(m)
}

dist_matc <- function(year = 2013, cbsa = FALSE) {
  return(dist_mat("center", year, cbsa))
}

dist_matb <- function(year = 2013, cbsa = FALSE) {
  return(dist_mat("border", year, cbsa))
}



# Produce Shared Border Matrix
bprox_mat <- function(queen = TRUE, year = 2013, cbsa = FALSE) {
  cache_path <- glue(opath$bprox_mat_)
  if (file.exists(cache_path)) {
    log_debug(paste("read from cache", cache_path))
    return(readRDS(cache_path))
  }
  
  t <- call_geog(year, cbsa)
  df <- t$geometry %>% 
    poly2nb(queen = queen) %>%
    nb2mat(style = "B", zero.policy = TRUE)
  diag(df) <- 1
  rownames(df) <- colnames(df) <- t$place
  
  log_debug(paste("save to cache", cache_path))
  saveRDS(df, util$mkdir(cache_path))  
  return(df)
}


############ Generate neighbors of neighbors hierarchical vector for a place ad nauseam
#Watch out for Nantucket, MA; Staten Island,NY; and San Juan, WA
# this is slow and can probably be improved by using existing shortest path algorithm
neighbor_of_neighbor <- function(central_place,
                                 quiet = TRUE,
                                 ...){
  cache_path <- glue(opath$neighbor_rings_)
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

## impedance ----

#' Binary proximity matrix (1 = within radius)
#' @param radius distance in miles
prox_impedance_mat <- function(radius = 1000, from = c("center", "border"), year = 2013, cbsa = FALSE) {
  from <- match.arg(from)
  x <- dist_mat(from, year, cbsa)
  m <- (x < set_units(radius, mi))
  mode(m) <- "integer"
  rownames(m) <- colnames(m) <- rownames(x)
  return(m)
}


# Produce inverse power distance decay impedance matrix
power_impedance_mat <- function(decay_power = 2, from = c("center", "border"), year = 2013, cbsa = FALSE) {
  from <- match.arg(from)
  x <- dist_mat(from, year, cbsa) |>
    drop_units()
  x <- 1 / (x^decay_power)
  x[is.infinite(x)] <- 1
  return(x)
}

# Produce exponential distance decay impedance matrix
expo_impedance_mat <- function(decay_constant = 10000, from = c("center", "border"), year = 2013, cbsa = FALSE) {
  from <- match.arg(from)
  x <- dist_mat(from, year, cbsa) |>
    drop_units()
  x <- exp(- x / decay_constant)
  return(x)
}

#' Produce Gaussian distance decay impedance matrix
#' @param rms_width distance in miles
gaus_impedance_mat <- function(rms_width = 1000, from = c("center", "border"), year = 2013, cbsa = FALSE) {
  from <- match.arg(from)
  x <- dist_mat(from, year, cbsa) |>
    drop_units()
  rms_width <- set_units(rms_width, mi) |> set_units(m) |> drop_units()
  x <- exp(-.5 * (x / rms_width)^2)
  return(x)
}



# Produce hyperbolic secant distance decay impedance matrix
hyper_impedance_mat <- function(decay_constant = 1000000, from = c("center", "border"), year = 2013, cbsa = FALSE) {
  from <- match.arg(from)
  x <- dist_mat(from, year, cbsa) |>
    drop_units()
  x <- ((2/(exp(-(x/decay_constant)) + exp(x/decay_constant))))
  return(x)
}

#' Produce bi-square distance decay
#' @param decay_zero distance in miles
bisquare_impedance_mat <- function(decay_zero = 1000, from = c("center", "border"), year = 2013, cbsa = FALSE) {
  from <- match.arg(from)
  x <- dist_mat(from, year, cbsa) |>
    drop_units()
  decay_zero <- set_units(decay_zero, mi) |> set_units(m) |> drop_units()
  m <- (1-(x/decay_zero)^2)^2
  m[x > decay_zero] <- 0
  return(m)
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
  
  # cbsa_spatial_cluster(2013) #slow

  dist_matc()
  # dist_matb() #slow, many hours
  
  bprox_mat()
  # neighbor_of_neighbor("55025", quiet = FALSE) #slow
  
  x <- prox_impedance_mat()
  x <- power_impedance_mat()
  x <- expo_impedance_mat()
  x <- gaus_impedance_mat()
  x <- hyper_impedance_mat()
  x <- bisquare_impedance_mat()
}
