# Function repository

# Load and attach necessary packages
library(rprojroot)
library(fs)
library(rlog)
library(reticulate)
library(tidyverse)
library(magrittr)
library(sf)
library(geosphere)
library(spdep)
library(reshape2)
library(viridis)
library(REAT)

#Load vis libraries
library(ggiraph)
library(glue)
library(ggnewscale)
library(cowplot)


# library(readxl)
# library(openxlsx)
# library(tidyr)
# library(tools)
# library(dplyr)
# 
# library(arrow)
# 
# library(ggplot2)
# library(RColorBrewer)
# library(scales)
# 
# library(magick)
# 
# library(tmaptools)
# 
# library(parallel)
# library(igraph)
# 
# library(ggridges)


# Display start time
log_info("Define functions start")

# S3 methods for automatic reticulate conversion of GeoDataFrame and GeoSeries
source(file.path(rprojroot::find_rstudio_root_file(), "rurec", "reticulate_extras.R"))

# Load conda environment "rurec"
use_condaenv('rurec')

# Import pubdata Python modules
bea_io <- import("rurec.pubdata.bea_io")
cbp <- import("rurec.pubdata.cbp")
#ers_rurality <- import("rurec.pubdata.ers_rurality")
geography <- import("rurec.pubdata.geography")
naics <- import("rurec.pubdata.naics")
geography_cbsa <- import("rurec.pubdata.geography_cbsa")
ag_output <- import("rurec.ag_output")

if (!file.exists(file.path(find_rstudio_root_file(), "data", "robjs"))) {
  dir.create(file.path(find_rstudio_root_file(), "data", "robjs"))
}

#### Add specified rows and columns of a vector matrix
vector_collapse <- function(vector, 
                            collapse_names, 
                            new_name){
  vec <- vector
  cl <- collapse_names
  nn <- new_name
  vec <- cbind(rowSums(vec[, which(colnames(vec) %in% cl), drop=F]), vec[, which(!colnames(vec) %in% cl), drop = F] )
  colnames(vec)[1] <- nn
  cn <- colnames(vector)[!colnames(vector) %in% cl] %>% append(nn, after = (min(which(colnames(vector) %in% cl)) - 1))
  vec <- vec[,cn, drop=F] 
  return(vec)
}

#### Add specified rows and columns of a matrix
matrix_collapse <- function(matrix, 
                            collapse_names, 
                            new_name){
  mat <- matrix
  cl <- collapse_names
  nn <- new_name
  mat <- rbind(colSums(mat[which(rownames(mat) %in% cl), ]), mat[which(!rownames(mat) %in% cl), ] )
  mat <- cbind(rowSums(mat[, which(colnames(mat) %in% cl)]), mat[, which(!colnames(mat) %in% cl) ] )
  rownames(mat)[1] <- nn
  colnames(mat)[1] <- nn
  rn <- rownames(matrix)[!rownames(matrix) %in% cl] %>% append(nn, after = (min(which(rownames(matrix) %in% cl)) - 1))
  cn <- colnames(matrix)[!colnames(matrix) %in% cl] %>% append(nn, after = (min(which(colnames(matrix) %in% cl)) - 1))
  mat <- mat[rn, cn] 
  return(mat)
}

### Create edgelist from a matrix
matrix2edgelist <- function(x){
  df <- do.call(cbind, 
                lapply(list("row_index" = row(x), 
                            "col_index" = col(x), 
                            "value" = x), 
                       as.vector))
}

### Create matrix from an edgelist
edgelist2matrix <- function(x){
  df <- matrix(x[,3], 
               nrow = length(unique(x[,1])), 
               ncol = length(unique(x[,2])), 
               dimnames = list(unique(x[,1]), 
                               unique(x[,2])))
  return(df)
}

# Convert miles to meters
miles2meters <- function(miles){
  df <- as.integer(miles)*1609.344
  return(df)
}

# Convert miles to meters
meters2miles <- function(meters){
  df <- as.integer(meters)/1609.344
  return(df)
}

year2tiger <- function(year){
  tiger_year = c(2022:2013, 2010, 2000, 1990)
  if(year %in% tiger_year){
    x <- year
  }else if(year > max(tiger_year)){
    x <- max(tiger_year)
  }else if(year > 2011){
    x <- 2013
  }else if(year > 2005){
    x <- 2010
  }else if(year > 1995){
    x <- 2000
  }else if(year < 1996){
    x <- min(tiger_year)
  }
  if(!year %in% tiger_year){
    warning("Shapefile years do not contain [",year,"] using [", x,"]")
  }
  return(as.integer(x))
}

year2cbsa <- function(year){
  cbsa_year = c(2020, 2018, 2017, 2015, 2013, 2009:2003)
  if(year %in% cbsa_year){
    x <- year
  }else if(year > 2018){
    x <- 2020
  }else if(year == 2016){
    x <- 2017
  }else if(year == 2014){
    x <- 2015
  }else if(year %in% 2012:2010){
    x <- 2013
  }else if(year < 2004){
    x <- 2003
  }
  if(!year %in% cbsa_year){
    warning("CBSA concordance years do not contain [",year,"] using [", x,"]")
  }
  return(as.integer(x))
}

year2rucc <- function(year){
  rucc_year = c(2013, 2003, 1993, 1983, 1974)
  if(year %in% rucc_year){
    x <- year
  }else if(year > max(rucc_year)){
    x <- max(rucc_year)
  }else if(year > 2008){
    x <- 2013
  }else if(year > 1998){
    x <- 2003
  }else if(year > 1988){
    x <- 1993
  }else if(year > 1978){
    x <- 1983
  }else if(year < 1979){
    x <- min(rucc_year)
  }
  if(!year %in% rucc_year){
    warning("RUCC years do not contain [",year,"] using [", x,"]")
  }
  return(as.integer(x))
}

year2cbp <- function(year){
  cbp_year = c(2021:1986)
  if(year %in% cbp_year){
    x <- year
  }else if(year > max(cbp_year)){
    x <- max(cbp_year)
  }else if(year < min(cbp_year)){
    x <- min(cbp_year)
  }
  if(!year %in% cbp_year){
    warning("CBP years do not contain [",year,"] using [", x,"]")
  }
  return(as.integer(x))
}

year2bea <- function(year,
                     ilevel = c("det", "sum", "sec"), 
                     ...){
  ilevel <- match.arg(ilevel)
  if(ilevel != "det"){
    bea_year = c(2021:1997)
    if(year %in% bea_year){
      x <- year
    }else if(year > max(bea_year)){
      x <- max(bea_year)
    }else if(year < min(bea_year)){
      x <- min(bea_year)
    }
    if(!year %in% bea_year){
      warning("BEA years do not contain [",year,"] using [", x,"]")
    }
  }
  if(ilevel == "det"){
    bea_year = c(2012, 2007)
    if(year %in% bea_year){
      x <- year
    }else if(year > 2007){
      x <- 2012
    }else if(year < 2007){
      x <- 2007
    }
    if(!year %in% bea_year){
      warning("Detail level BEA years do not contain [",year,"] using [", x,"]")
    }
  }
  return(as.integer(x))
}

year2agcensus <- function(year){
  ag_year = c(2017, 2012, 2007, 2002)
  if(year %in% ag_year){
    x <- year
  }else if(year > 2014){
    x <- 2017
  }else if(year > 2009){
    x <- 2012
  }else if(year > 2004){
    x <- 2007
  }else if(year < 2005){
    x <- 2002
  }
  if(!year %in% ag_year){
    warning("AgCensus years do not contain [",year,"] using [", x,"]")
  }
  return(as.integer(x))
}

beacode2description <- function(code, 
                                year = 2012,
                                ...){
  #Note: year is necessary but arbitrary selection
  bea_year <- year2bea(year, ...)
  sec <- data.frame("code" = names(bea_io$get_sup(bea_year, "sec", FALSE)), 
                    "description" = names(bea_io$get_sup(bea_year, "sec", TRUE))
  )
  sum <- data.frame("code" = names(bea_io$get_sup(bea_year, "sum", FALSE)), 
                    "description" = names(bea_io$get_sup(bea_year, "sum", TRUE))
  )
  det <- data.frame("code" = names(bea_io$get_sup(bea_year, "det", FALSE)), 
                    "description" = names(bea_io$get_sup(bea_year, "det", TRUE))
  )
  x <- rbind(sec, sum, det)
  df <- x["description"][x["code"] == code]
  return(df)
}

###### Call and tidy NAICS to BEA industry concordance table
call_industry_concordance <- function(...){
  df <- bea_io$get_naics_df() %>% filter(NAICS != "n.a.") %>% filter(NAICS != "NaN") 
  for(i in names(df)){
    df[[i]] <- unlist(df[[i]], use.names = FALSE) 
  }
  df %<>% add_row(SECTOR = "23", 
                  SUMMARY = "23", 
                  U_SUMMARY = "23", 
                  DETAIL = "23", 
                  DESCRIPTION = "Construction", 
                  NAICS = "23", 
                  .before = which(df$SECTOR == '23')[1])
  df %<>% filter(NAICS != "23*")
  df %<>% filter(DETAIL != "531HST")
  df <- df[order(df$NAICS), ]
  rownames(df) <- 1:nrow(df)
  b <- c("11", "21", "22", "23", "31G", "31G", "31G", "42", "44RT", "44RT", "48TW", "48TW", "51", "FIRE", "FIRE", "PROF", "PROF", "PROF", "6", "6", "7", "7", "81", "G")
  n <- c("11", "21", "22", "23", "31", "32", "33", "42", "44", "45", "48", "49", "51", "52", "53", "54", "55", "56", "61", "62", "71", "72", "81", "92")
  for(i in 1:length(n)){
    df$SECTOR[substr(df$NAICS, 1,2) %in% n[i]] <- b[i]
  }
  return(df)
}

###### Get specific industry NAICS to BEA concordance
ilevel_concord <- function(ilevel = c("det", "sum", "sec"), 
                           ...){
  ilevel <- match.arg(ilevel)
  x <- call_industry_concordance()
  if(ilevel == "det"){
    df <- x %>% select(DETAIL, NAICS)
  }
  if(ilevel == "sum"){
    df <- x %>% select(SUMMARY, NAICS) 
    df$NAICS <- substr(df$NAICS, 1,3)
    df$SUMMARY[df$NAICS == "336"] = "336"
    df$SUMMARY[df$NAICS == "541"] = "541"
    df <- df[!duplicated(df), ]
    rownames(df) <- 1:nrow(df)
  }
  if(ilevel == "sec"){
    df <- x %>% select(SECTOR, NAICS) 
    df$NAICS <- substr(df$NAICS, 1,2)
    df <- df[!duplicated(df), ]
    rownames(df) <- 1:nrow(df)
  }
  return(df)
}

# Call up and clean CBSA concordance codes (Delineations available for years 2003:2009, 2013, 2015, 2017, 2018, 2020)
call_cbsa_concord <- function(year, 
                              ...){
  cbsa_year <- year2cbsa(year)  
  df <- geography_cbsa$get_cbsa_delin_df(cbsa_year)
  df$CBSA_TITLE <- sapply(strsplit(df$CBSA_TITLE, ","), "[", 1)
  df$CBSA_TITLE <- paste(df$CBSA_TITLE, rep("CBSA", length(df$CBSA_TITLE)))
  df$place <- paste0(df$STATE_CODE, df$COUNTY_CODE)
  df <- df %>% select(CBSA_CODE, place, CBSA_TITLE)
  return(df)
}

#Convert a fips code into a cbsa code 
fips2cbsa <- function(fips,
                      year, 
                      ...){
  cb <- call_cbsa_concord(year)
  if(isFALSE(fips %in% paste0(dataRetrieval::countyCd$STATE, dataRetrieval::countyCd$COUNTY))){
    warning("FIPS entry [",fips,"] not found in ANSI FIPS records\n See: https://www2.census.gov/geo/docs/reference/codes/national_county.txt")
  }
  if(isTRUE(fips %in% cb$place)){cb$CBSA_CODE[fips == cb$place]}else{fips}
}

#error in "rurec.pubdata.ers_rurality" does not work
# # Call up and clean RUCC data available for years 2013, 2003, 1993, 1983, 1974
# call_rucc <- function(year){
#   df <- ers_rurality$get_ruc_df() %>% filter(RUC_YEAR == year2rucc(year))
#   df$place <- df$FIPS
#   return(df)
# }

# Call up and clean TIGER data
# shapefile formats are available for 2020:2013, 2010, 2000, 1990
call_tiger <- function(year = 2013,
                       scale = c("20m", "500k", "5m"),
                       geometry = TRUE,
                       ...){
  scale <- match.arg(scale)
  df <- geography$get_county_df(year2tiger(year), 
                                geometry, 
                                scale)
  df %<>% rename(place = CODE)
  df$COUNTY <- paste(df$NAME, "County")
  if(isTRUE(geometry)){
    df$center <- st_centroid(df$geometry)
  }
  st <- geography$get_state_df(geometry = FALSE) %>% select(c(1:3)) 
  names(st) <- c("STATE_CODE", "STATE_NAME", "STATE")
  df <- left_join(df, st, by = "STATE_CODE")
  df %<>% arrange(place)
  return(df)
}

# Call geography and aggregate spatial features of each CBSA member in a cluster
cbsa_spatial_cluster <- function(year = 2013, 
                                 ...){
  t <- call_tiger(year, ...)
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
  return(df)
}

### Call geographic features
call_geog <- function(year,
                      cbsa_clust = FALSE, 
                      ...){
  if(isFALSE(cbsa_clust)){
    df <- call_tiger(...) 
  } else {
    df <- cbsa_spatial_cluster(...)
  }
  return(df)
}

############ Generate neighbors of neighbors hierarchal vector for a place ad nauseam
#Watch out for Nantucket, MA; Staten Island,NY; and San Juan, WA
neighbor_of_neighbor <- function(central_place,
                                 quiet = TRUE,
                                 ...){
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
    if(!quiet == TRUE){
      print(paste("Interval level:", l))
      print(paste("Places remaining:", sum(df$nn=="")))
    }
  }
  return(df)
}

# Produce Center to Center Distance Matrix
dist_matc <- function(...){
  t <- call_geog(...)
  df <- t$center %>% 
    as_Spatial() %>% 
    distm()
  rownames(df) = colnames(df) <- t$place
  return(df)
}

# Produce Shared Border Matrix
bprox_mat <- function(queen = TRUE, 
                      ...){
  t <- call_geog(...)
  df <- t$geometry %>% 
    poly2nb(queen = queen) %>%
    nb2mat(style = "B", zero.policy = TRUE)
  diag(df) <- 1
  rownames(df) = colnames(df) <- t$place
  return(df)
}

# Produce binary Distance Matrix from polygon edge with variable distance
dist_matb <- function(boundary_limit = miles2meters(1000), 
                      ...){
  t <- call_geog(...)
  df = st_is_within_distance(t$geometry, 
                             dist = boundary_limit)
  df <- +as.matrix(df)
  diag(df) <- 1
  rownames(df) = colnames(df) <- t$place
  return(df)
}

# Produce binary Distance Proximity Matrix from polygon center
# distance in meters
dprox_mat <- function(boundary_limit = miles2meters(1000),
                      ...){
  df <- dist_matc(...)
  df[df < boundary_limit & df > 0] <- 1
  df[df > boundary_limit] <- 0
  diag(df) <- 1
  return(df)
}

# Produce inverse power distance decay impedance matrix
power_impedance_mat <- function(decay_power = 2, 
                                ...){
  df <- dist_matc(...)
  df <- (1/(df)^decay_power)
  df[is.infinite(df)] = 1
  return(df)
}

# Produce exponential distance decay impedance matrix
expo_impedance_mat <- function(decay_constant = 10000,
                               ...){
  df <- dist_matc(...)
  df <- exp(-(df/decay_constant)) 
  return(df)
}

# Produce Gaussian distance decay impedance matrix
gaus_impedance_mat <- function(rms_width = miles2meters(1000),
                               ...){
  df <- dist_matc(...)
  df <- exp(-.5*(df/rms_width)^2) 
  return(df)
}

# Produce hyperbolic secant distance decay impedance matrix
hyper_impedance_mat <- function(decay_constant = 1000000,
                                ...){
  df <- dist_matc(...)
  df <- ((2/(exp(-(df/decay_constant)) + exp(df/decay_constant))))
  return(df)
}

# Produce bi-square distance decay 
bisquare_impedance_mat <- function(decay_zero = miles2meters(1000),
                                   ...){
  dis <- dist_matc(...)
  df <- (1-(dis/decay_zero)^2)^2
  df[dis > decay_zero] <- 0
  return(df)
}

############ Call and clean pubdata BEA IO Use table
call_use_table <- function(year,
                           ilevel = c("det", "sum", "sec"), 
                           ...){
  ilevel <- match.arg(ilevel)
  df <-year2bea(year, ilevel) %>% bea_io$get_use(., ilevel) %>% as.matrix()
  df[is.na(df)] = 0
  return(df)
}

############ Call and clean pubdata BEA IO Supply table
call_supply_table <- function(year,
                              ilevel = c("det", "sum", "sec"), 
                              ...){
  ilevel <- match.arg(ilevel)
  df <- year2bea(year, ilevel) %>% bea_io$get_sup(., ilevel) %>% as.matrix()
  df[is.na(df)] = 0
  return(df)
}

#### Commodities-by-Industries parallel to ordinary technical coefficients matrix 
b_matrix <- function(year,
                     ilevel = c("det", "sum", "sec"),
                     ...){
  ilevel <- match.arg(ilevel)
  df <- call_use_table(year, ilevel, ...) %>% as.data.frame()
  u <- df[1:(which(rownames(df) == "T005")-1), 1:(which(colnames(df) == "T001")-1)] %>% as.matrix()
  x <- df["T018", 1:(which(colnames(df) == "T001")-1)]
  if(ilevel == "sum"){ 
    u <- matrix_collapse(u, grep("^336", colnames(u), value = TRUE), "336") %>% matrix_collapse(., grep("^541", colnames(.), value = TRUE), "541")
    x <- vector_collapse(x, grep("^336", colnames(x), value = TRUE), "336") %>% vector_collapse(., grep("^541", colnames(.), value = TRUE), "541")
  }
  if(ilevel == "det"){ 
    u <- matrix_collapse(u, grep("^23", colnames(u), value = TRUE), "23") 
    com_names <- rownames(u)[!rownames(u) %in% c("4200ID")]
    ind_names <- colnames(u)[!colnames(u) %in% c("4200ID")]
    u <- u[com_names, ind_names] 
    x <- vector_collapse(x, grep("^23", colnames(x), value = TRUE), "23")
    x <- x[ind_names] 
  }
  df <- u %*% diag(1/x)
  colnames(df) <- colnames(x)
  return(df)
}

####Commodity Composition of Industry Outputs
c_matrix <- function(year,
                     ilevel = c("det", "sum", "sec"),
                     ...){
  ilevel <- match.arg(ilevel)
  df <- call_supply_table(year, ilevel, ...) %>% as.data.frame()
  ind_supply <- df[nrow(df), 1:(which(colnames(df) == "T007")-1), drop=F]
  supply_mat <- as.matrix(df[1:(nrow(df)-1), 1:(which(colnames(df) == "T007")-1)])
  if(ilevel == "sum"){ 
    supply_mat <- matrix_collapse(supply_mat, grep("^336", colnames(supply_mat), value = TRUE), "336") %>% matrix_collapse(., grep("^541", colnames(.), value = TRUE), "541")
    ind_supply <- vector_collapse(ind_supply, grep("^336", colnames(ind_supply), value = TRUE), "336") %>% vector_collapse(., grep("^541", colnames(.), value = TRUE), "541")
  }
  if(ilevel == "det"){ 
    supply_mat <- matrix_collapse(supply_mat, grep("^23", colnames(supply_mat), value = TRUE), "23") 
    com_names <- rownames(supply_mat)[!rownames(supply_mat) %in% c("4200ID")]
    ind_names <- colnames(supply_mat)[!colnames(supply_mat) %in% c("4200ID")]
    supply_mat <- supply_mat[com_names, ind_names] 
    ind_supply <- vector_collapse(ind_supply, grep("^23", colnames(ind_supply), value = TRUE), "23")
    ind_supply <- ind_supply[ind_names] 
  }
  df <- supply_mat %*% diag(1/ind_supply)
  colnames(df) <- colnames(supply_mat)
  #temp fix needs correction in rurec.pubdata.bea_io module
  if (ilevel == "sec") {
    rownames(df) <- c(colnames(supply_mat), "Used", "Other")
  }
  return(df)
}

#### Industry Source of Commodity Outputs
d_matrix <- function(year,
                     ilevel = c("det", "sum", "sec"),
                     ...){
  ilevel <- match.arg(ilevel)
  df <- call_supply_table(year, ilevel, ...) %>% as.data.frame()
  com_supply <- df[1:(nrow(df)-1), "T007", drop=F] %>% as.matrix()
  supply_mat <- as.matrix(df[1:(nrow(df)-1), 1:(which(colnames(df) == "T007")-1)])
  if(ilevel == "sum"){ 
    supply_mat <- matrix_collapse(supply_mat, grep("^336", colnames(supply_mat), value = TRUE), "336") %>% matrix_collapse(., grep("^541", colnames(.), value = TRUE), "541")
    com_supply <- vector_collapse(t(com_supply), grep("^336", colnames(t(com_supply)), value = TRUE), "336") %>% vector_collapse(., grep("^541", colnames(.), value = TRUE), "541")  %>% t()
  }
  if(ilevel == "det"){ 
    supply_mat <- matrix_collapse(supply_mat, grep("^23", colnames(supply_mat), value = TRUE), "23") 
    com_names <- rownames(supply_mat)[!rownames(supply_mat) %in% c("4200ID")]
    ind_names <- colnames(supply_mat)[!colnames(supply_mat) %in% c("4200ID")]
    supply_mat <- supply_mat[com_names, ind_names] 
    com_supply <- t(vector_collapse(t(com_supply), grep("^23", colnames(t(com_supply)), value = TRUE), "23"))
    com_supply <- com_supply[com_names, , drop=F] 
  }
  df <- t(supply_mat) %*% diag(as.vector(1/com_supply))
  #temp fix needs correction in rurec.pubdata.bea_io module
  if (ilevel == "sec") {
    colnames(df) <- c(colnames(supply_mat), "Used", "Other")
  } else {
    colnames(df) <- rownames(supply_mat)
  }
  return(df)
}

# Call up and clean CBP ($1,000 of dollars) available for years 1986:2021
call_cbp <- function(year,
                     cbp_scale = c("county", "state", "us"),
                     imputed = TRUE, 
                     national_wages = TRUE, 
                     ...){
  cbp_year <- year2cbp(year)
  cbp_scale <- match.arg(cbp_scale)
  if(imputed){
    df <- cbp$get_cbp_year(cbp_year)
    df$place <- paste0(df$fipstate, df$fipscty)
    # stopgap for getting imputed payrolls when suppression exists even at the state-level
    if (national_wages){
      nat_ind_emp_sum <- df %>% {aggregate(.$emp, list(.$industry), FUN=sum)}
      colnames(nat_ind_emp_sum) <- c("industry", "nat_emp")
      nat_ind_ap_sum <- cbp$get_df(geo = "us", year = cbp_year) %>% .[.$lfo != "-", ] %>% {aggregate(.$ap, list(.$industry), FUN=sum)}
      colnames(nat_ind_ap_sum) <- c("industry", "natsub_ap")
      nat_sums <- inner_join(nat_ind_emp_sum, nat_ind_ap_sum, by = "industry")
      nat_ind_wage <- cbp$get_df(geo = "us", year = cbp_year) %>% .[.$lfo == "-", ] %>% inner_join(., nat_sums, by = "industry")
      nat_ind_wage$nat_wage = nat_ind_wage$qp1 / nat_ind_wage$emp * 4
      x <- nat_ind_wage$qp1 == 0
      nat_ind_wage[x, ]$nat_wage = nat_ind_wage[x, ]$ap / nat_ind_wage[x, ]$emp 
      x <- nat_ind_wage$emp == 0
      nat_ind_wage[x, ]$nat_wage = nat_ind_wage[x, ]$qp1 / nat_ind_wage[x, ]$nat_emp * 4
      x <- nat_ind_wage$emp == 0 & nat_ind_wage$qp1 == 0
      nat_ind_wage[x, ]$nat_wage = nat_ind_wage[x, ]$ap / nat_ind_wage[x, ]$nat_emp 
      x <- nat_ind_wage$emp == 0 & nat_ind_wage$qp1 == 0  & nat_ind_wage$ap == 0
      nat_ind_wage[x, ]$nat_wage = nat_ind_wage[x, ]$natsub_ap / nat_ind_wage[x, ]$nat_emp 
      nat_ind_wage <- nat_ind_wage %>% subset(select = c("industry", "nat_wage"))
      df <- left_join(df, nat_ind_wage, by = "industry")
      x <- df$emp != 0 & !is.finite(df$ap) & is.finite(df$nat_wage)
      df[x, ]$ap <- df[x, ]$emp * df[x, ]$nat_wage
    }
  } else {
    df <- cbp$get_df(geo = cbp_scale, 
                     year = cbp_year)
    if(cbp_scale == "county"){
      df$place <- paste0(df$fipstate, df$fipscty)
    }
    if(cbp_scale == "state"){
      df$place <- df$fipstate
    }
    if(cbp_scale == "us"){
      df$place <- "usa"
    }
  }
  n <- c("lfo", "fipstate", "fipscty", "place", "industry", "emp", "qp1", "ap", "est")
  df <- df[names(df) %in% n] %>% 
    rename(NAICS = industry)
  df$ap[is.nan(df$ap) | is.infinite(df$ap)] <- 0
  return(df)
}

# 
# ##test the percent improvement in capturing total employment and payrolls across 3 iterations (raw-cbp, state-level Eckert imputation, and national and state-level Eckert imputation)
# {
# naics_code = "311224"
# year = 2012
# national_cbp_ap = call_cbp(year = year, cbp_scale = "us", imputed = F) %>% .[.$lfo == "-", ] %>% {.[.$NAICS == naics_code, ]$ap}
# raw_cbp_ap = call_cbp(year = year, imputed = F) %>% {.[.$NAICS == naics_code, ]$ap} %>% sum()
# onelevel_cbp_ap = call_cbp(year = year, national_wages = F) %>% {.[.$NAICS == naics_code, ]$ap} %>% sum()
# twolevel_cbp_ap = call_cbp(year = year) %>% {.[.$NAICS == naics_code, ]$ap} %>% sum()
# 
# raw_cbp_ap/national_cbp_ap
# onelevel_cbp_ap/national_cbp_ap
# twolevel_cbp_ap/national_cbp_ap
# 
# df <- call_cbp(year = year, cbp_scale = "us", imputed = F) %>% .[.$lfo == "-", ]
# x <- call_cbp(year = year, imputed = F) %>% {aggregate(.$ap, list(.$NAICS), FUN=sum)}
# colnames(x) <- c("NAICS", "raw_ap")
# df <- left_join(df, x, by = "NAICS")
# x <- call_cbp(year = year, national_wages = F) %>% {aggregate(.$ap, list(.$NAICS), FUN=sum)}
# colnames(x) <- c("NAICS", "onelevel_ap")
# df <- left_join(df, x, by = "NAICS")
# x <- call_cbp(year = year) %>% {aggregate(.$ap, list(.$NAICS), FUN=sum)}
# colnames(x) <- c("NAICS", "twolevel_ap")
# df <- left_join(df, x, by = "NAICS")
# df$raw2nat <- df$raw_ap / df$ap 
# df$onelevel2nat <- df$onelevel_ap / df$ap 
# df$twolevel2nat <- df$twolevel_ap / df$ap 
# 
# temp <- df %>% pivot_longer(c(raw2nat, onelevel2nat, twolevel2nat), names_to = "key", values_to = "value")
# ggplot(temp, aes(x = value, color = key)) + geom_density()
# ggplot(temp[temp$NAICS %in% ilevel_concord(ilevel = "det")$NAICS, ], aes(x = NAICS, y = (value), color = key)) + geom_hline(yintercept=1) + geom_point(position=position_dodge(width=0.4), alpha = 1/2) + theme_classic() + theme(axis.text.x=element_blank(), axis.ticks.x=element_blank()) 
# ggplot(temp[temp$NAICS %in% ilevel_concord(ilevel = "sum")$NAICS, ], aes(x = NAICS, y = (value), color = key)) + geom_hline(yintercept=1) + geom_point(aes(shape=key, color=key), position=position_dodge(width=0.4)) + theme_classic() + theme(axis.text.x=element_blank(), axis.ticks.x=element_blank()) 
# ggplot(temp[temp$NAICS %in% ilevel_concord(ilevel = "sec")$NAICS, ], aes(x = NAICS, y = (value), color = key)) + geom_hline(yintercept=1) + geom_point(aes(shape=key, color=key), position=position_dodge(width=0.4)) + theme_classic() 
# # ggplot(temp[temp$NAICS %in% ilevel_concord(ilevel = "det")$NAICS, ], aes(x = NAICS, y = value, color = key)) + geom_point(alpha = 1/2) + theme_classic() + theme(axis.text.x=element_blank(), axis.ticks.x=element_blank())
# 
# temp <- df %>% pivot_longer(c(raw_ap, onelevel_ap, twolevel_ap), names_to = "key", values_to = "value")
# ggplot(temp, aes(x=log10(ap), y=log10(value), color = key), position="dodge") + 
#   geom_abline() + geom_point(shape=23) + theme_minimal() 
# 
# ggplot(df, aes(x=log10(ap), y=log10(raw_ap)), position="dodge") + 
#   scale_x_continuous(limits = c(0, NA)) + scale_y_continuous(limits = c(0, NA)) + 
#   geom_abline() + geom_point(shape=23, color = "#00BA38") + theme_minimal() 
# 
# ggplot(df, aes(x=log10(ap), y=log10(onelevel_ap)), position="dodge") + 
#   scale_x_continuous(limits = c(0, NA)) + scale_y_continuous(limits = c(0, NA)) + 
#   geom_abline() + geom_point(shape=23, color = "#F8766D") + theme_minimal() 
# 
# ggplot(df, aes(x=log10(ap), y=log10(twolevel_ap)), position="dodge") + 
#   scale_x_continuous(limits = c(0, NA)) + scale_y_continuous(limits = c(0, NA)) + 
#   geom_abline() + geom_point(shape=23, color = "#619CFF") + theme_minimal() 
# }


#Agglomerate NAICS and BEA concordance by year and industry specificity (sector, summary, or detail)
place_industry_economy_long <- function(year, 
                                        cbp_scale = c("county", "state", "us"), 
                                        imputed = TRUE,
                                        ...){
  cbp_scale <- match.arg(cbp_scale)
  conc <- ilevel_concord(...)
  n <- names(conc)[1]
  cbp_dat <- call_cbp(year = year, imputed = imputed, cbp_scale = cbp_scale, ...)
  if (cbp_scale != "county" & isFALSE(imputed)){
    cbp_dat <- cbp_dat %>% filter(.[["lfo"]] == "-")
  }
  x <- left_join(cbp_dat, conc, by = "NAICS") 
  x <- x %>%
    filter(.[dim(x)[2]] != "NULL") %>%
    group_by(place, .[dim(x)[2]]) %>%
    summarise(across(where(is.numeric), sum), .groups = 'drop') %>%
    as.data.frame()
  if (cbp_scale != "us"){
    x <- x %>%
      group_by(place) %>%
      arrange(factor(x[[2]], levels = unique(conc[[1]])), .by_group = TRUE) %>%
      as.data.frame()
    df <- x %>% pivot_wider(id_cols = n, names_from = "place", values_from = c("emp", "qp1", "ap", "est"), names_sep = ".", values_fill = 0)
    df <- df %>% pivot_longer(-n, names_to = c(".value", "place"), names_pattern = "([^\\.]*)\\.*(\\d+)") %>% as.data.frame()
    df <- df %>% group_by(df[[2]]) %>% arrange(factor(df[[1]], levels = unique(conc[[1]])), .by_group = TRUE)
    names(df)[1] <- "indcode"
  } else {
    df <- x %>% arrange(factor(x[[2]], levels = unique(conc[[1]])), .by_group = TRUE)
    names(df)[2] <- "indcode"
  }
  df <- df[1:6]
  return(df)
}

#Generate industry output ("ap", "emp", "qp1", or "est") by county from CBP in terms of BEA industry codes ("det", "sum", or "sec") for any available year
industry_output_by_place <- function(year, 
                                     output_metric = c("ap", "emp", "qp1", "est"), 
                                     ...){
  output_metric <- match.arg(output_metric)
  df <- place_industry_economy_long(year = year, ...) %>% 
    .[, c("indcode", "place", output_metric)] %>% 
    pivot_wider(id_cols = "indcode", names_from = "place", values_from = output_metric) %>% 
    as.data.frame()
  rownames(df) <- df$indcode
  df <- df[, !colnames(df) %in% c("indcode","place"), drop=F] %>% 
    as.matrix()
  return(df)
}

############ Derive the national level, industry specific, payroll share of gross output by year and industry scale
payroll_share <- function(year,
                          ilevel = c("det", "sum", "sec"), 
                          county_totals = TRUE,
                          ...){
  ilevel <- match.arg(ilevel)
  indout <- call_use_table(year, ilevel) %>% 
    .["T018", !colnames(.) %in% grep("^(F|T)[0-9]", colnames(.), value = TRUE), drop = FALSE]*1000000
  if(ilevel == "sum"){
    indout <-  vector_collapse(indout, grep("^336", colnames(indout), value = TRUE), "336") %>% vector_collapse(., grep("^541", colnames(.), value = TRUE), "541")
  }
  if(ilevel == "det"){
    indout <-  vector_collapse(indout, grep("^23", colnames(indout), value = TRUE), "23")
  }
  conc <- ilevel_concord(ilevel)
  if (county_totals){
    cbp_dat <- call_cbp(year = year, ...) %>% {aggregate(.$ap, list(.$NAICS), FUN=sum)} %>% `colnames<-`(c("NAICS", "ap"))
    ap <- left_join(cbp_dat, conc, by = "NAICS") %>% .[!is.na(.[[dim(.)[2]]]), ]
  } else {
    cbp_dat <- call_cbp(year = year, cbp_scale = "us", imputed = FALSE)
    ap <- left_join(cbp_dat, conc, by = "NAICS") %>% .[!is.na(.[[dim(.)[2]]]) & .$lfo == "-", ]
  }
  n <- names(ap)[dim(ap)[2]]
  ap <- ap %>% 
    group_by(.[dim(.)[2]]) %>%
    summarise(across(where(is.numeric), sum), .groups = 'drop') %>% 
    .[c("ap", n )] %>% 
    pivot_wider(names_from = n, values_from = c("ap"), names_sep = ".", values_fill = 0) %>% 
    as.matrix()*1000
  rownames(ap) <- "ap"
  intind <- intersect(colnames(ap), colnames(indout))
  df <- (ap["ap", intind , drop = FALSE] / indout["T018", intind , drop = FALSE]) 
  rownames(df) <- "psogo"
  return(df)
}

##Note presence of county-level suppression too. Can we fix/lessen by way of imputation?
##Note county totals of 111900 and 112A00 are 50% larger than their national level counterparts
# Call up and clean Ag Output data ($1,000 of dollars)
call_agoutput <- function(year, 
                          geo_level = c("county", "state", "national"), 
                          ...){
  geo_level <- match.arg(geo_level)
  ag_year <- year2agcensus(year)
  df <- ag_output$get_farm_sales_by_bea_detail(ag_year, geo_level) %>% as.data.frame()
  place <- c(place = rownames(df))
  df <- sapply(df, function(x)x/1000) %>% as.data.frame()
  if(geo_level == "county" | geo_level == "state"){
    df <- cbind(place, df)
    rownames(df) <- 1:nrow(df)
  } else {
    df <- t(df)
  }
  return(df)
}

############ Derive adjusted national ag gross industry output (in thousands of dollars)
total_ag_industry_output <- function (year,
                                      ...){
  ilevel = "det"
  farm_sales <- call_agoutput(year, geo_level = "national")
  indout <- call_use_table(year, ilevel)
  indsup <- call_supply_table(year, ilevel)
  agtax <- indout["T00OTOP", colnames(farm_sales)]
  agsub <- indout["T00SUB", colnames(farm_sales)]
  aginv <- indout[colnames(farm_sales), "F03000"]
  df <- farm_sales
  for(i in colnames(farm_sales)){
    commod_indust_ratio <- indsup[i, i] / indsup["T017", i]
    indust_commod_ratio <- indsup[i, i] / indsup[i, "T007"]
    adj_agtax <- commod_indust_ratio * agtax[i]
    adj_agsub <- commod_indust_ratio * agsub[i] 
    adj_aginv <- indust_commod_ratio * aginv[i] 
    df[, i] <- farm_sales[, i] + (adj_agtax + adj_agsub + adj_aginv)*1000
  }
  return(df)
}
# #error between ag census and bea industry output adjusting for tax, subsidy and inventory 
# total_ag_industry_output(year) / (call_supply_table(year, ilevel)["T017", colnames(farm_sales)]*1000)


############ Derive national level AgCensus sales share of gross output by year
agsales_share <- function(year,
                          county_totals = TRUE,
                          ...){
  if (county_totals){
    farm_sales <- call_agoutput(year, geo_level = "county") %>% .[, sapply(., is.numeric)] %>% colSums() %>% t()
  } else {
    farm_sales <- call_agoutput(year, geo_level = "national")
  }
  indout <- call_use_table(year, ilevel = "det")["T018", colnames(farm_sales)]*1000 
  df <- farm_sales/indout
  rownames(df) <- "asogo"
  return(df)
}

############ Derive the Total Industry Output Matrix (in thousands of dollars)
total_industry_output <- function (year,
                                   ilevel = c("det", "sum", "sec"), 
                                   ...){
  ilevel <- match.arg(ilevel)
  farm_sales <- call_agoutput(year, ...)
  fn <- colnames(farm_sales)[-c(1)]
  as <- agsales_share(year, ...)
  agout <- farm_sales %>% .[sapply(., is.numeric)] %>% {apply(., 1, function (x) {x / as})} %>% t() %>% as.data.frame()
  colnames(agout) <- fn
  agout$place <- farm_sales$place
  iout <- industry_output_by_place(year = year, ilevel = ilevel, ...)
  ps <- payroll_share(year = year, ilevel = ilevel, ...) %>% .[, rownames(iout)[rownames(iout) %in% colnames(.)], drop = FALSE] 
  df <- apply(iout, 2, function (x) {x / ps})
  df[is.infinite(df)] = 0
  rownames(df) <- rownames(iout)
  df <- t(df) %>% as.data.frame()
  df$place <- rownames(df)
  df <- left_join(df, agout, by = "place")
  if (ilevel == "sec") {
    df[["11"]] <- rowSums(df[, c("11", fn)], na.rm = T)
    df <- df %>% select(!all_of(c(fn)))
  } else if (ilevel == "sum") {
    df[["111CA"]] <- rowSums(df[, c(fn)], na.rm = T)
    df <- df %>% select(!all_of(c(fn))) %>% select("111CA", everything())
  } else if (ilevel == "det") {
    df <- df %>% select(all_of(fn), everything())
  }
  rownames(df) <- df$place
  df$place <- NULL
  df <- t(df)
  df[is.na(df)] = 0
  df[is.infinite(df)] = 0
  return(df)
}

# Aggregate industry output of the CBSA members in a cluster
cbsa_aggregate_industry_output <- function(year,
                                           ...){
  o <- total_industry_output(year, ...)
  c <- call_cbsa_concord(year, ...)
  c <- c[c$place %in% intersect(c$place, colnames(o)),]
  c <- data.frame(CBSA_CODE = c(c$CBSA_CODE, setdiff(colnames(o), c$place)), 
                  place = c(c$place, setdiff(colnames(o), c$place)))
  c <- c[order(c$CBSA_CODE), ]
  rownames(c) <- 1:nrow(c)
  x <- c$CBSA_CODE %>% unique()
  df <- data.frame(row.names = rownames(o))
  for(i in x){
    df[, i] <- rowSums(o[, c$place[c$CBSA_CODE == i], drop = FALSE])
  } 
  df <- as.matrix(df)
  df[is.na(df)] = 0
  return(df)
}

############ Call Gross Industry Output 
industry_output_matrix <- function(year,
                                   cbsa_clust = FALSE, 
                                   ...){
  if(isFALSE(cbsa_clust)){
    df <- total_industry_output(year, ...)
  } else {
    df <- cbsa_aggregate_industry_output(year, ...)
  }
  df <- df[(which(!is.na(rowSums(df)) & !rowSums(df) == 0)), ]
  return(df)
}

############ Call Gross Commodity Output
commodity_output_matrix <- function(year,
                                    ...){
  cm <- c_matrix(year, ...)
  o <- industry_output_matrix(year, ...)
  df <- cm[, rownames(o)] %*% o
  return(df)
}

############ Call Industry Factor Demand
industry_factor_demand_matrix <- function(year,
                                          ...){
  o <- industry_output_matrix(year, ...)
  df <- call_supply_table(year, ...)[,"T007", drop=F]
  dm <- d_matrix(year, ...)
  bm <- b_matrix(year, ...)
  pos_com <- setdiff(colnames(dm), rownames(df)[df == 0])
  tech_coef <- (dm[, pos_com] %*% bm[pos_com,])
  df <- tech_coef[rownames(o), rownames(o)] %*% o
  return(df)
}

############ Call Commodity Factor Demand
commodity_factor_demand_matrix <- function(year, 
                                           ...){
  o <- industry_output_matrix(year, ...)
  tech_coef <- b_matrix(year, ...)[, rownames(o)]
  df <- tech_coef %*% o
  return(df)
}

############ Call the National Factor Ratio of Industry Factor Demand to Gross Industry Output
industry_region_factor_ratio <- function(year,
                                         ...){
  fd <- industry_factor_demand_matrix(year, ...)
  io <- industry_output_matrix(year, ...)
  df <- rowSums(fd)/rowSums(io) %>% as.matrix()
  colnames(df) <- "factor_ratio"
  return(df)
}

############ Call the National Factor Ratio of Commodity Factor Demand to Gross Commodity Output
commodity_region_factor_ratio <- function(year,
                                          ...){
  fd <- commodity_factor_demand_matrix(year, ...)
  co <- commodity_output_matrix(year, ...)
  df <- rowSums(fd)/rowSums(co) %>% as.matrix()
  colnames(df) <- "factor_ratio"
  return(df)
}

############ Call Industry Factor Supply
industry_factor_supply_matrix <- function(year,
                                          ...){
  io <- industry_output_matrix(year, ...)
  fr <- industry_region_factor_ratio(year, ...)
  df <- diag(as.vector(fr)) %*% io
  rownames(df) <- rownames(fr)
  return(df)
}

############ Call Commodity Factor Supply
commodity_factor_supply_matrix <- function(year,
                                           ...){
  co <- commodity_output_matrix(year, ...)
  fr <- commodity_region_factor_ratio(year, ...)
  df <- diag(as.vector(fr)) %*% co
  rownames(df) <- rownames(fr)
  return(df)
}

############ Call list of IO products
io_yeild_list <- function(year,
                          flow_class = c("industry", "commodity"),
                          net_class = c("factor", "gross"),
                          ...){
  flow_class <- match.arg(flow_class)
  net_class <- match.arg(net_class)
  if(flow_class == "industry"){
    gross_ouput <- industry_output_matrix(year, ...)
    factor_supply <- industry_factor_supply_matrix(year, ...)
    factor_demand <- industry_factor_demand_matrix(year, ...)
  } 
  if(flow_class == "commodity"){
    gross_ouput <- commodity_output_matrix(year, ...)
    factor_supply <- commodity_factor_supply_matrix(year, ...)
    factor_demand <- commodity_factor_demand_matrix(year, ...)
  }
  if(net_class == "factor"){
    net_demand <- pmax(factor_demand - factor_supply, 0)
    net_supply <- pmax(factor_supply - factor_demand, 0)
  }
  if(net_class == "gross"){
    net_demand <- pmax(factor_demand - gross_ouput, 0)
    net_supply <- pmax(gross_ouput - factor_demand, 0)
  }
  return(list("gross_ouput" = gross_ouput, 
              "factor_supply" = factor_supply, 
              "factor_demand" = factor_demand,  
              "net_demand" = net_demand, 
              "net_supply" = net_supply))
}

#need fix for concordance with commodities
#does it work with sum and sec level inputs?
############ Derive data frame of various detail level IO metrics across space with sector level grouping
io_yeild_distribution <- function(year,
                                  ...){
  bea_year <- year2bea(year)
  des <- data.frame("code" = names(bea_io$get_sup(bea_year, "sec", FALSE)), 
                    "description" = names(bea_io$get_sup(bea_year, "sec", TRUE)) ) %>% 
    .[.$code %in% unique(ilevel_concord("sec")[[1]]), ] %>% 
    `colnames<-`(c("SECTOR", "description")) %>% 
    cbind(. , "color" = viridis(length(unique(.$description)))) %>% 
    inner_join(., call_industry_concordance()[, c("SECTOR", "DETAIL")], by = "SECTOR") %>% 
    .[!duplicated(.), ]
  df <- io_yeild_list(year)
  for (i in 1:length(df)){
    df[[i]] <- melt(df[[i]])
    names(df[[i]]) <- c("DETAIL", "place", names(df)[i])
    df[[i]]$place <- df[[i]]$place %>% formatC(width = 5, format = "d", flag = "0")
  }
  df <- lapply(df[-1], function(x){x[setdiff(names(x), Reduce(intersect, lapply(df, names)) )]} ) %>% 
    do.call(cbind, .) %>% 
    cbind(df[[1]], .) %>% 
    left_join(., des, by = "DETAIL")
  return(df)
}





#RAS trade matrix
ras_trade_lists <- function(factor_supply,
                            factor_demand, 
                            impedance_mat = NULL,
                            core_count = 10,
                            data_dir = file.path("data", "robjs"),
                            ...){
  
  df <- setequal(rownames(factor_supply), rownames(factor_demand))
  stopifnot("Commodity/Industry names do not match" = df == TRUE)
  
  if(!is.null(impedance_mat)){ 
    rn <- colnames(factor_supply)
    cn <- colnames(factor_demand)
    df <- all(
      setequal(rn, cn),
      setequal(rn, rownames(impedance_mat[rn, cn])), 
      setequal(cn, colnames(impedance_mat[rn, cn]))
    )
    stopifnot("Place names do not match" = df == TRUE)
  }
  
  #Starting position of trade matrix 
  x <- list()
  y <- intersect(
    names(which(!is.na(rowSums(factor_demand)) & 
                  !rowSums(factor_demand) == 0 )), 
    names(which(!is.na(rowSums(factor_supply)) & 
                  !rowSums(factor_supply) == 0 ))
  )
  for (i in y){
    if(!is.null(impedance_mat)){
      x[[i]] <- (t(factor_supply[i, , drop=F]) %*% factor_demand[i, , drop=F]) * (impedance_mat[rn, cn])
    } else {
      x[[i]] <- (t(factor_supply[i, , drop=F]) %*% factor_demand[i, , drop=F])
    }
  }
  unlink(file.path(find_rstudio_root_file(), data_dir, "temp"), recursive = TRUE)
  dir.create(file.path(find_rstudio_root_file(), data_dir, "temp"))
  
  mclapply(names(x), function(i) {
    x0 = x[[i]]
    r1 = factor_supply[i, , drop=F]
    c1 = factor_demand[i, , drop=F]
    tf <- ras_trade_flows(x0 = x0,
                          rs1 = r1,
                          cs1 = c1,
                          ...)
    colnames(tf) = colnames(c1)
    rownames(tf) = colnames(r1)
    saveRDS(tf, file = file.path(find_rstudio_root_file(), data_dir, "temp", i))
    rm(x0,r1,c1,tf)
    return(invisible(NULL))
  }, mc.cores = core_count)
  return(names(x))
}

ras_trade_listsx <- function(factor_supply,
                             factor_demand,
                             impedance_mat = NULL,
                             data_dir = file.path("data", "robjs"),
                             ...){
  x <- ras_trade_lists(factor_supply = factor_supply,
                       factor_demand = factor_demand,
                       impedance_mat = impedance_mat,
                       ...)
  df <- lapply(file.path(find_rstudio_root_file(), data_dir, "temp", x), readRDS)
  names(df) <- x
  return(df)
}


############ Call matrices of commodity or industry imputed trade flows from RAS procedure
call_imputed_tradeflows <- function(year,
                                    industryflow = TRUE,
                                    impedance_mat = NULL,
                                    subsectors = NULL,
                                    crosshaul = TRUE,
                                    ...){
  if(!"ras_trade_fls" %in% c(lsf.str())){
    source(file.path(find_rstudio_root_file(), "nbs", "io_analysis.R"))
  }
  if(isTRUE(industryflow)){
    fs <- industry_factor_supply_matrix(year, ...)
    fd <- industry_factor_demand_matrix(year, ...)
  } else {
    fs <- commodity_factor_supply_matrix(year, ...)
    fd <- commodity_factor_demand_matrix(year, ...)
  }
  df <- setequal(rownames(fs), rownames(fd))
  stopifnot("Commodity/Industry names do not match" = df == TRUE)
  if(!is.null(subsectors)){
    fs <- fs[subsectors, , drop = FALSE]
    fd <- fd[subsectors, , drop = FALSE]
  }
  if(isTRUE(crosshaul)){
    fsx <- fs
    fdx <- fd
  } else {
    fsx <- pmax(fs - fd, 0)
    fdx <- pmax(fd - fs, 0)
  }
  df <- ras_trade_listsx(factor_supply = fsx,
                         factor_demand = fdx, 
                         impedance_mat = impedance_mat, 
                         ...)
  return(df)
}

load_tradeflows <- function(year,
                            ilevel,
                            industryflow = TRUE,
                            impedance_mat = NULL,
                            subsectors = NULL,
                            crosshaul = TRUE,
                            data_dir = file.path("data", "robjs"),
                            impedance_call = NULL, 
                            ...){
  
  if(!is.null(impedance_mat) & 
     is.null(impedance_call)
  ){
    warning("Impedance is applied without a specific description to save/load")
  }
  rasf <- paste0("ras", "_", ilevel,"class", "_", cbp_year, "cbp", "_", if(isTRUE(industryflow)){"industries"}else{"commodities"}, "_", if(isTRUE(crosshaul)){"xhaul"}else{"nohaul"}, "_", if(is.null(subsectors)){"all"}else{subsectors}, "sectors", "_", if(is.null(impedance_mat)){"NA"}else{impedance_call}, "impedance")
  if (file.exists(file.path(find_rstudio_root_file(), data_dir, rasf) ) ){ 
    df <- readRDS(file.path(find_rstudio_root_file(), data_dir, rasf))
  } else {
    df <- call_imputed_tradeflows(year,
                                  industryflow,
                                  impedance_mat,
                                  subsectors,
                                  crosshaul,
                                  ...)
    saveRDS(df,  file = file.path(find_rstudio_root_file(), data_dir, rasf) )
  }
  return(df)
}

### Aggregate list of sector specific trade flow matrices into single matrix
aggregate_sector_list <- function(sector_list){
  df <- Reduce('+', sector_list)
  return(df)
}

### Normalize trade flow matrix by total county outbound factor supply (rows) or inbound factor demand (cols)
normalize_tradeflow <- function(tradeflow_matrix, 
                                byrow = TRUE){
  if(isTRUE(byrow)){
    df <- tradeflow_matrix %>% sweep(1, rowSums(.), FUN="/")
  } else {
    df <- tradeflow_matrix %>% sweep(2, colSums(.), FUN="/")
  }
  return(df)
}

### Summarize data from trade flow matrix
inbound2outbound <- function(trade_matrix){
  df <- data.frame("place" = rownames(trade_matrix),
                   "inbound" = colSums(trade_matrix), 
                   "outbound" = rowSums(trade_matrix), 
                   "out2in" = rowSums(trade_matrix)/colSums(trade_matrix), 
                   "out_less_in" = rowSums(trade_matrix)-colSums(trade_matrix)
  )
  return(df)
}





############ Stacked Absorption Share 
stacked_absorption_share <- function(nis_matrix, 
                                     nid_matrix,
                                     list_names = NULL){
  s <- nis_matrix
  d <- nid_matrix
  x <- rep(c(1), each=nrow(s))
  ## Check counties match between nis_matrix and nid_matrix
  df <- identical(colnames(d), colnames(s))
  stopifnot(df)
  print(paste(list_names, "Absorption calculation started", Sys.time() ))
  df <-  matrix(0, nrow = ncol(s), 
                ncol = ncol(s) )
  rownames(df) = colnames(df) <- colnames(s)
  for (i in 1:ncol(s)){
    for (j in 1:ncol(s)){
      df[i,j] <- 
        (x %*% pmin(s[,i], d[,j]))
    }
  }
  print(paste(list_names, "Absorption calculation finished", Sys.time() ))
  invisible(df)
}

############ Normalized Absorption Share
normalized_absorption_share <- function(sas_matrix, 
                                        nis_matrix){
  s <- sas_matrix
  n <- nis_matrix
  df <- s / colSums(n)
  df[is.na(df)] = 0
  return(df)
}

############ Call absorption matrix for any available year and industry scale
absorption_matrix <- function(year,
                              ilevel = c("det", "sum", "sec"),
                              cbsa_clust = FALSE,
                              flow_class = c("industry", "commodity"),
                              net_class = c("factor", "gross"),
                              normalized = TRUE,
                              impedance = NULL,
                              data_dir = file.path("data", "robjs"),
                              ...){
  ilevel <- match.arg(ilevel)
  flow_class <- match.arg(flow_class)
  net_class <- match.arg(net_class)
  yl <- io_yeild_list(year, 
                      ilevel = ilevel,
                      cbsa_clust = cbsa_clust,
                      flow_class = flow_class, 
                      net_class = net_class, 
                      ...)
  y <-  yl[[1]] %>% {names(which(!is.na(rowSums(.)) & !rowSums(.) == 0))}
  net_demand <- yl[[4]][y, , drop=F]
  net_supply <- yl[[5]][y, , drop=F]
  sasf <- paste0("sas", "_", ilevel,"level", "_", year, "year", "_", flow_class, "class", "_", "cbsa", if(isTRUE(cbsa_clust)){"clust"}else{"NA"}, "_", net_class, "class")
  if (file.exists(file.path(find_rstudio_root_file(), data_dir, sasf) ) ){ 
    df <- readRDS(file.path(find_rstudio_root_file(), data_dir, sasf))
  } else {
    df <- stacked_absorption_share(net_supply, net_demand)
    saveRDS(df, file = file.path(find_rstudio_root_file(), data_dir, sasf) )
  }
  if(isTRUE(normalized)){
    df <- normalized_absorption_share(df, net_supply)
  }
  if(!is.null(impedance)){
    df <- df * impedance[colnames(df), rownames(df)]
  }
  return(df)
}

### Generate vector of maximum non-impedance absorption values using impedance scaled absorption matrix
noimpedance_absorption_maximum <- function(absorption_matrix, 
                                           impedance_mat, 
                                           row_max_match = TRUE){
  a <- absorption_matrix
  df <- c()
  x <- a * impedance_mat[rownames(a), colnames(a)]
  if(isTRUE(row_max_match)){
    x <- apply(x, 1, which.max)
    for(i in 1:nrow(a)){
      df <- cbind(df, a[i, x[i]])
    }
  } else {
    x <- apply(x, 2, which.max)
    for(i in 1:ncol(a)){
      df <- cbind(df, a[x[i], i])
    }
  }
  df <- as.data.frame(t(df))
  rownames(df) <- rownames(a)
  colnames(df) <- "ab_max"
  df$id <- deparse(substitute(absorption_matrix))
  return(df)
}


#need to address HI and AK matching
############ Row-wise Absorption Potential Maximum and Match
absorption_maximum_match <- function(absorption_matrix, 
                                     threshold = .05,
                                     ...){
  a <- absorption_matrix
  df <- cbind(place = rownames(a), 
              match = colnames(a)[apply(a, 1, which.max)],
              max_absorption_alpha = apply(a, 1, max), 
              max_absorption_count = apply(a, 1, function(x) {sum(max(x) == x)}),
              second_max_absorption_alpha = apply(a, 1, function(x){max(x[x != max(x), drop = FALSE])}), 
              absorption_alpha_gini = apply(a, 1, gini),
              absorption_alpha_total = apply(a, 1, sum),
              absorption_alpha_mean = apply(a, 1, mean),
              absorption_alpha_sd = apply(a, 1, sd),
              adsorption_match = colnames(a)[apply(a, 2, which.max)],
              max_adsorption_alpha = apply(a, 2, max), 
              max_adsorption_count = apply(a, 2, function(x) {sum(max(x) == x)}),
              second_max_adsorption_alpha = apply(a, 2, function(x){max(x[x != max(x), drop = FALSE])}), 
              adsorption_alpha_gini = apply(a, 2, gini),
              adsorption_alpha_total = apply(a, 2, sum),
              adsorption_alpha_mean = apply(a, 2, mean),
              adsorption_alpha_sd = apply(a, 2, sd) ) %>% 
    as.data.frame()
  df[, 3:ncol(df)] <- lapply(3:ncol(df), function(x) as.numeric(df[[x]]))
  
  ### cluster_class reevaluates the maximum absorption match to account for an isolation threshold and ECA isolated corner cases (i.e., no one imports your excess so you are isolated, but you are max import sink for someone else)
  df$cluster_class <- df$match
  df$cluster_class[df$max_absorption_alpha < threshold] <- "Isolated"
  df$cluster_class[df$place %in% unique(df$match) & df$cluster_class == "Isolated"] <- "ECA Isolated"
  
  ### eca_class reevaluates the maximum absorption match and returns the corrected self-match locations for "ECA Isolated" and "Cluster Core" locations 
  df$eca_class <- df$cluster_class
  df$eca_class[df$cluster_class == "ECA Isolated"] <- df$place[df$cluster_class == "ECA Isolated"]
  df$eca_class[df$place %in% unique(df$cluster_class)] <- df$place[df$place %in% unique(df$cluster_class)]
  
  ### cluster_category gives the categorical classification of each location as one of: "Isolated", "Isolated, Cluster Sink", "Cluster Sink", or "Cluster Source"
  df$cluster_category <- df$cluster_class
  df$cluster_category[df$place %in% unique(df$cluster_class)] <- "Cluster Sink"
  df$cluster_category[df$eca_class != df$place] <- "Cluster Source"
  df$cluster_category[df$cluster_class == "Isolated"] <- "Isolated"
  df$cluster_category[df$cluster_class == "ECA Isolated"] <- "Isolated, Cluster Sink"
  
  ### eca_membership gives all places their ECA corrected matching location explicitly
  df$eca_membership <- df$eca_class
  df$eca_membership[df$eca_class == "Isolated"] <- df$place[df$eca_class == "Isolated"]
  
  ### cluster_members_count is a tally of the number of places belonging to a cluster
  df <- df %>% group_by(eca_membership) %>% mutate(cluster_members_count = n())
  
  return(df)
}

############ Join spatial and other location specific information to ECA classification data tables
join_space_with_connectedness <- function(connectedness_table,
                                          space_data, 
                                          join_variable = place){
  df <- inner_join(space_data, connectedness_table, by = deparse(substitute(join_variable)), copy = TRUE)
}

#need to drop Alaska and Hawaii when?
############ Call connectedness table
connectedness <- function (year,
                           central_place = NULL,
                           cbsa_clust = FALSE, 
                           spatial = TRUE,
                           ...){
  df <- absorption_matrix(year, cbsa_clust = cbsa_clust, ...)
  if(is.null(central_place)){
    df <- absorption_maximum_match(df, ...)
  } else {
    if(isTRUE(cbsa_clust)){
      central_place <- fips2cbsa(central_place, year)
    }
    df <- cbind(export_absorption = c(t(df[central_place, , drop = FALSE])),
                import_absorption = c(df[, central_place, drop = FALSE]),
                place = rownames(df)) 
    df[, 2:3] <- lapply(2:3, function(x) as.numeric(df[[x]]))
  }
  if(isTRUE(spatial)){
    df <- call_geog(...) %>% join_space_with_connectedness(df, .)
  }
  return(df)
}

############ Absorption matching outcomes over time
absorption_match_overtime <- function(years = 2000:2020,
                                      ...){
  dis <- vector("list", length(years))
  names(dis) <- years
  for (y in years){
    dis[[y]] <- connectedness(year = y, ...)
  }
  df <- bind_rows(dis, .id = "id")
  return(df)
}







###Need to make adding html a separate operation
#need to drop Alaska and Hawaii when?
############ Call connectedness table with spatial component
spatial_connectedness <- function(year,
                                  ...){
  c <- connectedness(year, ...)
  s <- call_geog(...)
  df <- join_space_with_connectedness(c, s)
  
  # data_dir = file.path("data", "robjs")
  # if(isTRUE(add_html)){
  #   hp <- paste0("htmlplots", "_", industry_aggregate_class, "class", "_", cbp_year, "cbp", "_",  ag_year, "ag", "_", tiger_year, "tiger", "_", if(isTRUE(cbsa_clust)){cbsa_year}else{"NA"}, "cbsa")
  #   if (file.exists(file.path(find_rstudio_root_file(), data_dir, hp))){ 
  #     h <- readRDS(file.path(find_rstudio_root_file(), data_dir, hp))
  #   } else {
  #     h <- html_industry_dist_plots(cbp_year,
  #                                   ...)
  #     saveRDS(h, file = file.path(find_rstudio_root_file(), data_dir, hp))
  #   }
  #   indicator_type = names(h)
  #   for(i in indicator_type){
  #     df[[paste0("html_", i) ]] <- h[[i]]
  #   }
  # } 
  
  # ilevel <- match.arg(ilevel)
  # flow_class <- match.arg(flow_class)
  # net_class <- match.arg(net_class)
  
  # sasf <- paste0("sas", "_", ilevel,"level", "_", year, "year", "_", flow_class, "class", "_", "cbsa", if(isTRUE(cbsa_clust)){"clust"}else{"NA"}, "_", net_class, "class")
  # if (file.exists(file.path(find_rstudio_root_file(), data_dir, sasf) ) ){ 
  #   df <- readRDS(file.path(find_rstudio_root_file(), data_dir, sasf))
  # } else {
  #   df <- stacked_absorption_share(net_supply, net_demand)
  #   saveRDS(df, file = file.path(find_rstudio_root_file(), data_dir, sasf) )
  # }
  
  return(df)
}

#double check operation
############ Aggregate economic industry output of each ECA member in a cluster, keep all non source places as ECA core unit label
aggregate_industry_output <- function(industry_output_matrix, 
                                      connectedness_table,
                                      ...){
  df <- industry_output_matrix
  c <- connectedness_table
  x <- c$eca_membership %>% unique() %>% .[order(.)]
  for(i in x){
    df[, i] <- rowSums(df[, c$place[c$eca_membership == i], drop = FALSE])
  } 
  df <- df[, x]
}

#double check operation
############ Spatial union each ECA member in a cluster
spatial_cluster <- function(spatial_connectedness_table,
                            quiet = TRUE,
                            ...){
  df <-  spatial_connectedness_table %>% select(names(.)[!(names(.) %in% c("center", "html_output", "html_input", "html_nis", "html_nid"))])
  x <- df$eca_membership %>% unique() %>% .[order(.)]
  for (i in x){
    if(!quiet == TRUE){print(paste("start cluster: ", i, which(i == x), "of", length(x), Sys.time()))}
    df[df$place == i,]$geometry <- df %>% filter(df$eca_membership == i) %>% st_union()
    if(!quiet == TRUE){print(paste("end cluster: ", i, which(i == x), "of", length(x), Sys.time()))}
  }
  df <- df %>% .[.$place %in% .$eca_membership, ]
}

# may be redundant and unneecessary
# ############ Call spatial connectedness for any available year and industry scale aggregating eca clusters across space
# cluster_spatial_connectedness <- function (year,
#                                            list_names = NULL,
#                                            ...){
#   df <- spatial_connectedness(year, ...)
#   x <- df$eca_membership %>% unique() %>% .[order(.)]
#   for (i in x){
#     print(paste(list_names, "start cluster: ", i, which(i == x), "of", length(x), Sys.time()))
#     df[df$place == i,]$geometry <- df %>% filter(df$eca_membership == i) %>% st_union()
#     print(paste(list_names, "  end cluster: ", i, which(i == x), "of", length(x), Sys.time()))
#   }
#   df <- df %>% .[.$place %in% .$eca_membership, ]
#   return(df)
# }


#Superseded need to phase out
############ Call and clean the total requirements matrix 
call_total_requirements <- function(bea_year,
                                    ilevel = c("det", "sum", "sec"), 
                                    ...){
  ilevel <- match.arg(ilevel)
  if(ilevel == "det"){
    x <- bea_year %in% c("2007", "2012")
    stopifnot("BEA detail level tables only exist for years 2007 and 2012" = x == TRUE)
  }
  df <- bea_io$get_ixi(strtoi(bea_year), ilevel) %>% .[1:ncol(.), ] %>% as.matrix()
  rownames(df) <- colnames(df)
  return(df)
}

#Superseded need to phase out
### Need better methods for matrix inversion in tests inv() gave more accurate results but solve() was MUCH faster: inv() will also reach memory limit
############ Derive the direct requirements matrix (Technical Coefficients) 
call_direct_requirements <- function(...){
  df <- call_total_requirements(...)
  df <- diag(ncol(df)) - solve(df)
  df[df < 0] = 0
  return(df)
}


############ Industry Input Needs 
### Derive the commodity-by-county matrix of input needs DY
industry_input <- function(technical_coefficients_matrix, 
                           industry_output_matrix){
  ## Check industry level specificity match between industry_output_matrix and technical_coefficients_matrix
  df <- setequal(colnames(technical_coefficients_matrix), rownames(industry_output_matrix))
  stopifnot("Industry names do not match" = df == TRUE)
  o <- industry_output_matrix
  d <- technical_coefficients_matrix
  df <- d %*% o
}

############ Net Input Demand
### Derive the industry-by-county matrix of net input demand 
net_input_demand <- function(industry_output_matrix, 
                             industry_input_matrix){
  i <- industry_input_matrix
  o <- industry_output_matrix
  df <- pmax(i - o, 0)
}

############ Net Input Supply
### Derive the industry-by-county matrix of net input supply 
net_input_supply <- function(industry_output_matrix, 
                             industry_input_matrix){
  i <- industry_input_matrix
  o <- industry_output_matrix
  df <- pmax(o - i, 0)
}

### need to update with C_matrix B_matrix capability and adjust for factor supply /demand /RAS
############ Single function of nested functions to derive a hierarchies of connectedness tables and resulting output matrices from a base single output matrix and single direct requirements matrix
one_hierarchical_connectedness <- function(cbp_year,
                                           ilevel = c("det", "sum", "sec"),
                                           cbsa_clust = FALSE,
                                           normalized = TRUE,
                                           impedance = NULL,
                                           data_dir = file.path("data", "robjs"),
                                           queen = TRUE,
                                           ...){
  ilevel <- match.arg(ilevel)
  cbsa_year <- year2cbsa(cbp_year, ...)  %>% suppressWarnings() 
  ag_year <- year2agcensus(cbp_year, ...)
  bea_year <- year2bea(cbp_year, ilevel, ...)
  tiger_year <- year2tiger(cbp_year, ...) %>% suppressWarnings()
  sasf <- paste0("hier", "_", ilevel,"class", "_", cbp_year, "cbp", "_", if(isTRUE(cbsa_clust)){cbsa_year}else{"NA"}, "cbsa", "_", if(is.null(impedance)){"NA_impedance"}else if(is.numeric(impedance)){paste0(impedance,"_impedance")}else{if(queen==TRUE){"queenborder_impedance"}else{"rookborder_impedance"}} )
  if (file.exists(file.path(find_rstudio_root_file(), data_dir, sasf) ) ){ 
    df <- readRDS(file.path(find_rstudio_root_file(), data_dir, sasf))
  } else {
    o <- industry_output_tidy_matrix(cbp_year = cbp_year, ilevel = ilevel, cbsa_clust = cbsa_clust, ...)
    d <- call_direct_requirements(bea_year, ilevel, ...)
    if(ilevel == "det"){ 
      con <- grep("^23", colnames(d), value = TRUE)
      cm <- matrix(sum(d[con,con])/length(con), 
                   dimnames = list(c("23"), c("23")))
      cr <- t(matrix(colMeans(d[con, ]), 
                     dimnames = list(colnames(d), c("23")) ))
      cc <- matrix(rowMeans(d[, con]), 
                   dimnames = list(colnames(d), c("23"))) 
      d <- cbind( rbind(cr, d ), rbind(cm, cc )) 
      d <- d[colnames(d)[!colnames(d) %in% con] , colnames(d)[!colnames(d) %in% con]]
      d <- d[rownames(o)[rownames(o) %in% rownames(d)], rownames(o)[rownames(o) %in% colnames(d)]] 
    }
    hct <- list()
    hsct <- list()
    hsct$level_0 <- spatial_connectedness(cbp_year, ...)
    hom <- list()
    hom$level_0 <- o
    n = 1
    i = FALSE
    while(i == FALSE){
      print(paste("level", n))
      ii <- industry_input(d, o)
      nis <- net_input_supply(o, ii)
      nid <- net_input_demand(o, ii)
      df <- stacked_absorption_share(nis, nid)
      if(isTRUE(normalized)){
        df <- normalized_absorption_share(df, nis)
      }
      if(is.null(impedance)){
        df <- df
      } else if(is.numeric(impedance)){
        impd = st_is_within_distance(hsct[[paste0("level_", n-1)]]$geometry, dist = miles2meters(impedance))
        impd <- +as.matrix(impd)
        diag(impd) <- 0
        rownames(impd) = colnames(impd) <- hsct[[paste0("level_", n-1)]]$place
        df <- df * impd[colnames(df), rownames(df)]
      } else {
        impd = hsct[[paste0("level_", n-1)]]$geometry %>% 
          poly2nb(queen = queen) %>%
          nb2mat(style = "B", zero.policy = TRUE)
        rownames(impd) = colnames(impd) <- hsct[[paste0("level_", n-1)]]$place
        df <- df * impd[colnames(df), rownames(df)]
      }
      c <- absorption_maximum_match(absorption_matrix = df, ...)  
      s <-  hsct[[paste0("level_", n-1)]] %>% select(c("place", "NAME", "STATE_CODE", "COUNTY_CODE", "COUNTY", "STATE_NAME", "STATE", "geometry" ))
      hct[[paste0("level_", n)]] <- join_space_with_connectedness(c, s)
      i <- all(c$place %in% c$eca_membership) 
      if (i == TRUE){next}
      hsct[[paste0("level_", n)]] <- hct[[paste0("level_", n)]] %>% spatial_cluster()
      o <- aggregate_industry_output(o, c)
      hom[[paste0("level_", n)]] <- o
      n = n + 1
    }
    df <- list("Hierarchical_Connectedness_table" = hct,
               "Hierarchical_Spatial_Cluster_table" = hsct,
               "Hierarchical_Output_mat" = hom)
    saveRDS(df,  file = file.path(find_rstudio_root_file(), data_dir, sasf) )
  }
  return(df)
}

# update n vector and place_centric_connect inputs
############ Change in connectedness over time for a county 
place_connect_delta <- function(central_place, 
                                sample_years, # vector of years
                                ...){
  n <- c("export_absorption", "import_absorption", "html_output", "html_input", "html_nis", "html_nid")
  place_connect <- vector("list", length(sample_years))
  names(place_connect) <- sample_years
  for (i in sample_years){
    place_connect[[i]] <- place_centric_connect(central_place = central_place,
                                                year = i,
                                                ...)
    x <- n[n %in% names(place_connect[[i]])]
    names(place_connect[[i]])[names(place_connect[[i]]) %in% x] <- paste(x, i, sep=".")
  }
  df <- place_connect[[1]]
  for(i in 2:length(sample_years)){
    df <- st_set_geometry(place_connect[[i]], NULL) %>% .[c(grep("*[0-9]", colnames(.), value = TRUE), "place")] %>% inner_join(df, ., by = "place")
  }
  com <- combn(sample_years, 2)
  for (x in 1:ncol(com)){
    df[[paste0("export_absorption_delta_", substr(com[2, x], 3, 4), substr(com[1, x], 3, 4))]] <- df[[paste0("export_absorption.", com[1, x])]] - df[[paste0("export_absorption.", com[2, x])]]
    df[[paste0("import_absorption_delta_", substr(com[2, x], 3, 4), substr(com[1, x], 3, 4))]] <- df[[paste0("import_absorption.", com[1, x])]] - df[[paste0("import_absorption.", com[2, x])]]
  }
  ###Percent Change option
  # for (x in 1:ncol(com)){
  #   df[[paste0("export_absorption_delta_", substr(com[2, x], 3, 4), substr(com[1, x], 3, 4))]] <- (df[[paste0("export_absorption.", com[1, x])]] - df[[paste0("export_absorption.", com[2, x])]])/df[[paste0("export_absorption.", com[1, x])]]*100
  #   df[[paste0("import_absorption_delta_", substr(com[2, x], 3, 4), substr(com[1, x], 3, 4))]] <- (df[[paste0("import_absorption.", com[1, x])]] - df[[paste0("import_absorption.", com[2, x])]])/df[[paste0("import_absorption.", com[1, x])]]*100
  # }
  return(df)
}

# Display end time
log_info("Define functions end")


# Display start time
log_info("Define viz functions start")


############ Bar charts of industry distributions by county
industry_distribution_barcharts <- function(data,
                                            interact = TRUE,
                                            short = TRUE,
                                            ...){
  ft <- c("gross_ouput", "factor_supply", "factor_demand", "net_demand", "net_supply")
  sn <- unique(distinct(data, DETAIL, SECTOR)[order(distinct(data, DETAIL, SECTOR)$DETAIL),]$SECTOR)
  pn <- unique(data$place)
  df <- c()
  for (j in ft){
    for (i in pn){
      class2title <- function(x){str_split(x, "_") %>% unlist() %>% toString() %>% gsub(",", "", .) %>% str_to_title()}
      d <- data[data$place == i & data[[j]] > 0, setdiff(names(data), setdiff(ft, j) )]
      df[[j]][[i]] <- ggplot(d, aes(
        if(isTRUE(short)){
          x = factor(SECTOR, levels = sn)
        }else{
          x = factor(description, levels = unique(description))
        },
        y = .data[[j]],
        fill = DETAIL))
      if(isTRUE(interact)){
        df[[j]][[i]] <- df[[j]][[i]] +
          geom_col_interactive(aes(tooltip = paste0("Detail Sector: ", .data[["DETAIL"]], "\n Value: ", format(round(.data[[j]], 0), big.mark = ",", scientific = FALSE) ),
                                   data_id = DETAIL), 
                               color = NA)
      } else {
        df[[j]][[i]] <- df[[j]][[i]] +
          geom_col(color = NA)
      }
      df[[j]][[i]] <- df[[j]][[i]] +
        scale_fill_manual(values = d$color) +
        scale_y_continuous(labels = scales::comma) +
        theme_minimal() +
        theme(axis.text.x = element_text(size = rel(.75), angle = 300, hjust = 0),
              axis.text.y = element_text(size = rel(.75)),
              plot.subtitle = element_text(size = rel(.75), vjust = -2), 
              plot.margin = margin(t = 0, r = 50, b = 0, l = 0, unit = "pt")) +
        labs(x = element_blank(),
             y = element_blank(),
             subtitle = paste0(class2title(j), " ($1,000)"),
             title = element_blank()) +
        guides(fill = "none")
    }
  }
  return(df)
}


############ html plots of bar charts of industry distributions by county [used in mapping interactive tooltips]
html_industry_dist_plots <- function(year,
                                     ilevel = c("det", "sum", "sec"),
                                     cbsa_clust = FALSE,
                                     flow_class = c("industry", "commodity"),
                                     net_class = c("factor", "gross"),
                                     data_dir = file.path("data", "robjs"),
                                     ...){
  ilevel <- match.arg(ilevel)
  flow_class <- match.arg(flow_class)
  net_class <- match.arg(net_class)
  b <- io_yeild_distribution(year, 
                             ilevel = ilevel, 
                             cbsa_clust = cbsa_clust, 
                             flow_class = flow_class, 
                             net_class = net_class, 
                             ...) %>% 
    industry_distribution_barcharts(., interact = FALSE)
  df <- vector("list", length = length(b))
  names(df) <- names(b)
  for (i in names(df)){
    print(i)
    print(Sys.time())
    df[[i]] <- vector("list", length = length(b[[i]]))
    for (p in 1:length(df[[i]])){
      df[[i]][p] <- htmltools::plotTag(b[[i]][p], alt = "") %>% as.character()
    }
    names(df[[i]]) <- names(b[[i]])
  }
  hp <- paste0("htmlplots", "_", ilevel,"level", "_", year, "year", "_", flow_class, "class", "_", "cbsa", if(isTRUE(cbsa_clust)){"clust"}else{"NA"}, "_", net_class, "class")
  saveRDS(h, file = file.path(find_rstudio_root_file(), data_dir, hp))
  return(df)
}

############ Multi plot of bar charts of industry distributions for a given county
industry_dist_plots <- function(central_place,
                                year,
                                cbsa_clust = FALSE,
                                ...){
  if(isTRUE(cbsa_clust)){central_place <- fips2cbsa(central_place)}
  idis <- io_yeild_distribution(year = year, 
                                cbsa_clust = cbsa_clust, 
                                ...)
  bd <- idis[idis$place == central_place, ]  %>% industry_distribution_barcharts(., ...)
  pobj <- plot_grid(bd$factor_supply[[central_place]],
                    bd$factor_demand[[central_place]],
                    bd$net_demand[[central_place]],
                    bd$net_supply[[central_place]])
  girafe(ggobj = pobj, 
         options = list(
           opts_hover(
             css = girafe_css(
               css = "stroke:gray;r:8pt;",
               text = "stroke:none;fill:black;fill-opacity:1;" ) ),
           opts_tooltip(css = "font-family:sans-serif;
                                             background-color:gray;
                                             color:white;
                                             padding:10px;
                                             border-radius:5px;") ))
}



#status unknown
############ hierarchy GIF generator
national_hierarchy_gif <- function(df,
                                   folder,
                                   map_function, 
                                   caption = NULL,
                                   ...){
  r <- file.path(find_rstudio_root_file(), "data", folder)
  if(!dir.exists(r)){
    dir.create(r)
  }
  fn <- list()
  lev <- list(names(df))
  for(sn in names(df)){
    fp <- file.path(r, paste0(folder, "_", sn, ".png"))
    if(!file.exists(fp)){ 
      argList <- list()
      argList$df <- df[[sn]]
      argList$caption <- glue("Level {which(names(df) == sn)} Hierarchy\n 5% Isolation Threshold")
      do.call(map_function, argList)
      ggsave(fp)
    }
    fn[[which(names(df) == sn)]] <- fp
  }
  anim <- fn %>% unlist() %>% lapply(image_read) %>% image_join() %>% image_animate(fps = 1)
  image_write(image = anim,
              path = file.path(r, paste0(folder, ".gif")))
}

#status unknown
############ Distance impedance map GIF generator
national_progressiveimpedance_gif <- function(year,
                                              dist,
                                              impd,
                                              folder,
                                              map_function,
                                              ...){
  r <- file.path(find_rstudio_root_file(), "data", folder)
  if(!dir.exists(r)){
    dir.create(r)
  }
  fn <- list()
  for(d in dist){
    fp <- file.path(r, paste0(folder, "_", d, ".png"))
    if(!file.exists(fp)){
      if(impd == "B"){
        impedance = dist_matb(dist = miles2meters(d), ...)
      }
      if(impd == "C"){
        impedance = dprox_mat(boundary_limit = miles2meters(d), ...)
      }
      df <- spatial_connectedness(cbp_year = year,
                                  impedance = impedance,
                                  ...)
      argList <- list()
      argList$df <- df
      argList$caption <- glue("{d} Mile Impedance \n 5% Isolation Threshold")
      g <- do.call(map_function, argList)
      ggsave(fp)
    }
    fn[[which(dist == d)]] <- fp
  }
  anim <- fn %>% unlist() %>% lapply(image_read) %>% image_join() %>% image_animate(fps = 1)
  image_write(image = anim,
              path = file.path(r, paste0(folder, ".gif")))
}



############ Map arbitrary qualitative data (four color theorem) 
aqual_map <- function(df,
                      fcmt = FALSE,
                      tooltip = glue("Place: {NAME}\nECA: {eca_membership}"),
                      data_id = eca_membership,
                      ncols = 8,
                      minimize = FALSE,
                      palette = "Set2",
                      caption = NULL,
                      ...){
  if(fcmt == FALSE){
    g <- ggplot(df) + 
      geom_sf_interactive(aes(fill = factor(max_absorption_alpha), 
                              tooltip = {{tooltip}}, 
                              data_id = {{data_id}})) + 
      theme_void() +
      theme(legend.position = "none") + 
      labs(caption = caption)
  } else {
    stopifnot("dataframe needs sf geometry" = "sf" %in% class(df) == TRUE) 
    if(!"Ncol" %in% names(df)){
      df[["Ncol"]] <- map_coloring(df$geometry,
                                   ncols = ncols,
                                   minimize = minimize)
    }
    g <- ggplot(df) + 
      geom_sf_interactive(aes(fill = factor(Ncol), 
                              tooltip = {{tooltip}}, 
                              data_id = {{data_id}})) + 
      theme(legend.position = "none") + 
      scale_fill_brewer(palette = palette) + 
      labs(caption = caption)
  }
  girafe(ggobj = g,
         options = list(
           opts_hover(
             css = girafe_css(
               css = "stroke:gray;r:8pt;fill:orange",
               text = "stroke:none;fill:black;fill-opacity:1;" ) ),
           opts_zoom(max = 5),
           opts_selection(type = "multiple", 
                          only_shiny = FALSE,
                          css = "fill:black"),
           opts_sizing = opts_sizing(rescale = TRUE),
           opts_tooltip(offx = 20, offy = 20,
                        css = "font-family:sans-serif;
                               background-color:gray;
                               color:white;
                               padding:10px;
                               border-radius:5px;",
                        use_cursor_pos = TRUE) ))
}


############ Map change in connectedness over time for a county 
absorption_delta_map <- function(central_place,
                                 df, 
                                 fill,
                                 delta_min,
                                 delta_max){
  g <- ggplot(df) +
    geom_sf_interactive(aes(fill = .data[[fill]], 
                            tooltip = if("STATE" %in% names(df)){
                              glue("{NAME}, {STATE}\nFIPS: {place}\nAbsorption Change: {round(.data[[fill]], 5)}")
                            }else{
                              glue("{NAME}\nFIPS: {place}\nAbsorption Change: {round(.data[[fill]], 5)}")
                            },
                            data_id = place
    ), 
    color = NA
    ) + 
    guides(alpha = "none") +
    coord_sf() +
    theme_void() +
    scale_fill_gradientn(colors = c("#d7191c", "#ffffbf", "#2b83ba"), values=rescale(c(delta_min, 0, delta_max)), limits=c(delta_min, delta_max)) +
    geom_sf_interactive(data = df[df$place==central_place,], fill = "#A020F0", color = NA) +
    labs(fill = if("STATE" %in% names(df)){
      glue("{df[df$place==central_place,]$NAME}, {df[df$place==central_place,]$STATE} \nAggregate: {round(sum(df[[fill]]), 3)} \nAbsorption Change")
    }else{
      glue("{df[df$place==central_place,]$NAME} \nAggregate: {round(sum(df[[fill]]), 3)} \nAbsorption Change")
    } )  +
    theme(plot.margin = margin(t = 1, r = 1, b = 10, l = 1, unit = "pt"),
          legend.key.size = unit(.2, "cm"),
          legend.title = element_text(size = rel(0.5), color = "white"), 
          legend.text = element_text(size = rel(0.5), color = "white"),
          legend.position = c(0.9, 0.3),
          panel.background = element_rect(fill = "black")) 
  
  
  girafe(ggobj = g, 
         options = list(
           opts_hover(
             css = girafe_css(
               css = "stroke:gray;r:8pt;",
               text = "stroke:none;fill:black;fill-opacity:1;" ) ),
           opts_zoom(max = 5),
           opts_sizing = opts_sizing(rescale = TRUE),
           opts_tooltip(css = "font-family:sans-serif;
                                           background-color:gray;
                                           color:white;
                                           padding:10px;
                                           border-radius:5px;",
                        use_cursor_pos = TRUE) ))
}


############ Map absorption metrics
absorption_map <- function(df, 
                           add_html = FALSE,
                           fill = "max_absorption_alpha", 
                           fill_lab = "Max Absorption",
                           unit_scale = TRUE,
                           caption = paste0(5,"% Isolation Threshold")){
  if(!isTRUE(add_html)){
    g <- ggplot(df) +
      geom_sf_interactive(aes(fill = .data[[fill]], 
                              tooltip = if("STATE" %in% names(df)){
                                glue("{NAME}, {STATE}\nFIPS: {place}\nMatch: {eca_membership}\nAlpha: {round(.data[[fill]], 5)}")
                              }else{
                                glue("{NAME}\nFIPS: {place}\nMatch: {eca_membership}\nAlpha: {round(.data[[fill]], 5)}")
                              },
                              data_id = place), 
                          color = NA) + 
      guides(alpha = "none") +
      coord_sf() +
      theme_void() +
      theme(plot.margin = margin(t = 1, r = 1, b = 10, l = 1, unit = "pt"),
            legend.key.size = unit(.2, "cm"),
            legend.title = element_text(size = rel(0.5)), 
            legend.text = element_text(size = rel(0.5)),
            legend.position = c(0.9, 0.2)) + {
              if(isTRUE(unit_scale)){
                scale_fill_viridis(direction = -1, limits=c(floor(0), ceiling(1)))
              } else {
                scale_fill_viridis(direction = -1)
              } } + 
      labs(fill = fill_lab,
           caption = caption)
  } else {
    g <- ggplot(df) +
      geom_sf_interactive(aes(fill = .data[[fill]], 
                              tooltip = if("STATE" %in% names(df)){
                                glue("{NAME}, {STATE}\nFIPS: {place}\nMatch: {eca_membership}\nAlpha: {round(.data[[fill]], 5)}\nIndustry Output\n{html_output}")
                              }else{
                                glue("{NAME}\nFIPS: {place}\nMatch: {eca_membership}\nAlpha: {round(.data[[fill]], 5)}\nIndustry Output\n{html_output}")
                              },
                              data_id = place), 
                          color = NA) + 
      guides(alpha = "none") +
      coord_sf() +
      theme_void() +
      theme(plot.margin = margin(t = 1, r = 1, b = 10, l = 1, unit = "pt"),
            legend.key.size = unit(.2, "cm"),
            legend.title = element_text(size = rel(0.5)), 
            legend.text = element_text(size = rel(0.5)),
            legend.position = c(0.9, 0.2)) + {
              if(isTRUE(unit_scale)){
                scale_fill_viridis(direction = -1, limits=c(floor(0), ceiling(1)))
              } else {
                scale_fill_viridis(direction = -1)
              } } + 
      labs(fill = fill_lab,
           caption = caption)
  }
  girafe(ggobj = g, 
         options = list(
           opts_hover(
             css = girafe_css(
               css = "stroke:gray;r:8pt;",
               text = "stroke:none;fill:black;fill-opacity:1;" ) ),
           opts_zoom(max = 5),
           opts_sizing = opts_sizing(rescale = TRUE),
           opts_tooltip(css = "font-family:sans-serif;
                                           background-color:gray;
                                           color:white;
                                           padding:10px;
                                           border-radius:5px;",
                        use_cursor_pos = FALSE,
                        offx = 0, offy = 330) ))
  
}

############ Hierarchical Map absorption metrics
hier_ab_map <- function(df, 
                        central_place,
                        threshold = .05){
  g <- ggplot() +
    geom_sf_interactive(aes(fill = eca_membership, 
                            tooltip = if("STATE" %in% names(df)){
                              glue("{NAME}, {STATE}\nFIPS: {place}\nCluster Count: {cluster_members_count}")
                            }else{
                              glue("{NAME}\nFIPS: {place}\nCluster Count: {cluster_members_count}")
                            },
                            data_id = place),
                        data = subset(df, eca_membership == df$eca_membership[df$place == central_place]),
                        color = NA) +
    scale_fill_manual(values = "#feb24c",
                      guide = guide_legend(order = 2),
                      labels = "Economic Catchment Area",
                      name = NULL) +
    #guides(fill=guide_legend(title=NULL)) +
    new_scale_fill() +
    geom_sf_interactive(aes(fill = eca_membership, 
                            tooltip = if("STATE" %in% names(df)){
                              glue("{NAME}, {STATE}\nFIPS: {place}\nCluster Count: {cluster_members_count}")
                            }else{
                              glue("{NAME}\nFIPS: {place}\nCluster Count: {cluster_members_count}")
                            },
                            data_id = place), 
                        data = subset(df, eca_membership != df$eca_membership[df$place == central_place]),
                        color = NA) +
    scale_fill_grey(start = 0.5,
                    end = 0.5,
                    guide = "none") +
    new_scale_fill() +
    geom_sf_interactive(data = df[df$place==central_place,], aes(fill = place), color = NA) +
    scale_fill_manual(values = "#56B1F7",
                      guide = guide_legend(order = 1),
                      labels = "Place of Interest",
                      name = NULL) +
    #guides(fill=guide_legend(title=NULL)) +
    new_scale_fill() +
    geom_sf_interactive(data = df[df$place == df$eca_membership[df$place == central_place],], aes(fill = place), color = NA) +
    scale_fill_manual(values = "#f03b20",
                      guide = guide_legend(order = 3),
                      labels = "Catchment Sink",
                      name = NULL) +
    #guides(fill=guide_legend(title=NULL)) +
    guides(alpha = "none") +
    coord_sf() +
    theme_void() +
    theme(plot.margin = margin(t = 1, r = 1, b = 10, l = 1, unit = "pt"),
          legend.key.size = unit(.2, "cm"),
          legend.title = element_text(size = rel(0.5)), 
          legend.text = element_text(size = rel(0.5)),
          legend.position = c(0.9, 0.2)) +
    {if(!is.null(threshold)){labs(caption = paste0(threshold*100,"% Isolation Threshold")) } }
  
  girafe(ggobj = g, 
         options = list(
           opts_hover(
             css = girafe_css(
               css = "stroke:gray;r:8pt;",
               text = "stroke:none;fill:black;fill-opacity:1;" ) ),
           opts_zoom(max = 5),
           opts_sizing = opts_sizing(rescale = TRUE),
           opts_tooltip(css = "font-family:sans-serif;
                                           background-color:gray;
                                           color:white;
                                           padding:10px;
                                           border-radius:5px;") ))
}

############ Map cluster membership counts by absorption 
clustmember_map <- function(df, 
                            alpha = "max_absorption_alpha",
                            add_html = FALSE,
                            caption = paste0(5,"% Isolation Threshold")){
  if(!isTRUE(add_html)){
    g <- ggplot() +
      geom_sf_interactive(aes(fill = cluster_members_count,
                              tooltip = if("STATE" %in% names(df)){
                                glue("{NAME}, {STATE}\nFIPS: {place}\nMatch: {eca_membership}\nAlpha: {round(.data[[alpha]], 5)}")
                              }else{
                                glue("{NAME}\nFIPS: {place}\nMatch: {eca_membership}\nAlpha: {round(.data[[alpha]], 5)}")
                              },
                              data_id = place),
                          data = subset(df, cluster_category == "Cluster Sink" | cluster_category == "Isolated, Cluster Sink" ),
                          color = NA) + 
      labs(fill = "Cluster Sink") +
      scale_fill_gradient(low = "#feb24c", high = "#f03b20", guide = guide_colorbar(order = 2)) +
      new_scale_fill() +
      geom_sf_interactive(aes(fill = cluster_members_count,
                              tooltip = if("STATE" %in% names(df)){
                                glue("{NAME}, {STATE}\nFIPS: {place}\nMatch: {eca_membership}\nAlpha: {round(.data[[alpha]], 5)}")
                              }else{
                                glue("{NAME}\nFIPS: {place}\nMatch: {eca_membership}\nAlpha: {round(.data[[alpha]], 5)}")
                              },
                              data_id = place),
                          data = subset(df, cluster_category == "Cluster Source"),
                          color = NA) + 
      labs(fill = "Cluster Source") +
      scale_fill_gradient(low = "#56B1F7", high = "#132B43", guide = guide_colorbar(order = 1)) +
      new_scale_fill() +
      geom_sf_interactive(aes(fill = cluster_category,
                              tooltip = if("STATE" %in% names(df)){
                                glue("{NAME}, {STATE}\nFIPS: {place}\nMatch: {eca_membership}\nAlpha: {round(.data[[alpha]], 5)}")
                              }else{
                                glue("{NAME}\nFIPS: {place}\nMatch: {eca_membership}\nAlpha: {round(.data[[alpha]], 5)}")
                              },
                              data_id = place),
                          data = subset(df, cluster_category == "Isolated"),
                          color = NA) + 
      guides(fill=guide_legend(title=NULL)) +
      scale_fill_manual(values = "#b2df8a",
                        guide = guide_legend(order = 3)) +
      coord_sf() +
      theme_void() +
      theme(plot.margin = margin(t = 1, r = 1, b = 10, l = 1, unit = "pt"),
            legend.key.size = unit(.2, "cm"),
            legend.title = element_text(size = rel(0.5)), 
            legend.text = element_text(size = rel(0.5)),
            legend.position = c(0.9, 0.2)) +
      labs(caption = caption) 
  } else {
    g <- ggplot() +
      geom_sf_interactive(aes(fill = cluster_members_count,
                              tooltip = if("STATE" %in% names(df)){
                                glue("{NAME}, {STATE}\nFIPS: {place}\nMatch: {eca_membership}\nAlpha: {round(.data[[alpha]], 5)}\nIndustry Output\n{html_output}")
                              }else{
                                glue("{NAME}\nFIPS: {place}\nMatch: {eca_membership}\nAlpha: {round(.data[[alpha]], 5)}\nIndustry Output\n{html_output}")
                              },
                              data_id = place),
                          data = subset(df, cluster_category == "Cluster Sink" | cluster_category == "Isolated, Cluster Sink"),
                          color = NA) + 
      labs(fill = "Cluster Matches") +
      scale_fill_gradient(low = "#feb24c", high = "#f03b20", guide = guide_colorbar(order = 2)) +
      new_scale_fill() +
      geom_sf_interactive(aes(fill = cluster_members_count,
                              tooltip = if("STATE" %in% names(df)){
                                glue("{NAME}, {STATE}\nFIPS: {place}\nMatch: {eca_membership}\nAlpha: {round(.data[[alpha]], 5)}\nIndustry Output\n{html_output}")
                              }else{
                                glue("{NAME}\nFIPS: {place}\nMatch: {eca_membership}\nAlpha: {round(.data[[alpha]], 5)}\nIndustry Output\n{html_output}")
                              },
                              data_id = place),
                          data = subset(df, cluster_category == "Cluster Source"),
                          color = NA) + 
      labs(fill = "Cluster Source") +
      scale_fill_gradient(low = "#56B1F7", high = "#132B43", guide = guide_colorbar(order = 1)) +
      new_scale_fill() +
      geom_sf_interactive(aes(fill = cluster_category,
                              tooltip = if("STATE" %in% names(df)){
                                glue("{NAME}, {STATE}\nFIPS: {place}\nMatch: {eca_membership}\nAlpha: {round(.data[[alpha]], 5)}\nIndustry Output\n{html_output}")
                              }else{
                                glue("{NAME}\nFIPS: {place}\nMatch: {eca_membership}\nAlpha: {round(.data[[alpha]], 5)}\nIndustry Output\n{html_output}")
                              },
                              data_id = place),
                          data = subset(df, cluster_category == "Isolated"),
                          color = NA) + 
      guides(fill=guide_legend(title=NULL)) +
      scale_fill_manual(values = "#b2df8a",
                        guide = guide_legend(order = 3)) +
      coord_sf() +
      theme_void() +
      theme(plot.margin = margin(t = 1, r = 1, b = 10, l = 1, unit = "pt"),
            legend.key.size = unit(.2, "cm"),
            legend.title = element_text(size = rel(0.5)), 
            legend.text = element_text(size = rel(0.5)),
            legend.position = c(0.9, 0.2)) +
      labs(caption = caption) 
  }
  girafe(ggobj = g, 
         options = list(
           opts_hover(
             css = girafe_css(
               css = "stroke:gray;r:8pt;",
               text = "stroke:none;fill:black;fill-opacity:1;" ) ),
           opts_zoom(max = 5),
           opts_sizing = opts_sizing(rescale = TRUE),
           opts_tooltip(css = "font-family:sans-serif;
                                           background-color:gray;
                                           color:white;
                                           padding:10px;
                                           border-radius:5px;",
                        use_cursor_pos = FALSE,
                        offx = 0, offy = 330),
           opts_toolbar = opts_toolbar(position = "top", saveaspng = FALSE) ))
}

############ Map place-centric connectedness for a county 
place_absorption_map <- function(central_place,
                                 df, 
                                 fill, 
                                 add_html = FALSE,
                                 unit_scale = TRUE){
  
  # if(fill == "export_absorption"){x <- "Export"}else{x <- "Import"}
  g <- ggplot(df) + {
    if(!isTRUE(add_html)){
      geom_sf_interactive(aes(fill = (round(.data[[fill]], 5)), 
                              tooltip = if("STATE" %in% names(df)){
                                glue("{NAME}, {STATE}\nFIPS: {place}\nAbsorption: {round(.data[[fill]], 4)}")
                              }else{
                                glue("{NAME}\nFIPS: {place}\nAbsorption: {round(.data[[fill]], 4)}")
                              },
                              data_id = place ), color = NA ) } else {
                                nis <- paste0("html_nis.", substr(fill, nchar(fill) - 3, nchar(fill)) )
                                nid <- paste0("html_nid.", substr(fill, nchar(fill) - 3, nchar(fill)) )
                                geom_sf_interactive(aes(fill = (round(.data[[fill]], 5)), 
                                                        tooltip = if("STATE" %in% names(df)){
                                                          glue("{NAME}, {STATE}\nFIPS: {place}\nAbsorption: {round(.data[[fill]], 4)}\nOutput Excess and Input Needs\n{.data[[nis]]}{.data[[nid]]}")
                                                        }else{
                                                          glue("{NAME}\nFIPS: {place}\nAbsorption: {round(.data[[fill]], 4)}\nOutput Excess and Input Needs\n{.data[[nis]]}{.data[[nid]]}")
                                                        },
                                                        data_id = place ), color = NA ) } 
  } + 
    guides(alpha = "none") +
    coord_sf() +
    theme_void() + {
      if(isTRUE(unit_scale)){
        scale_fill_viridis(direction = -1, limits=c(floor(0), ceiling(1)))
      } else {
        scale_fill_viridis(direction = -1)
      } } +
    geom_sf_interactive(data = df[df$place==central_place,], fill = "#8b0000", color = NA) +
    labs(fill = if("STATE" %in% names(df)){
      glue("{df[df$place==central_place,]$NAME}, {df[df$place==central_place,]$STATE} \nAbsorption")
    }else{
      glue("{df[df$place==central_place,]$NAME} \nAbsorption")
    } )  +
    theme(plot.margin = margin(t = 1, r = 1, b = 10, l = 1, unit = "pt"),
          legend.key.size = unit(.2, "cm"),
          legend.title = element_text(size = rel(0.5)), 
          legend.text = element_text(size = rel(0.5)),
          legend.position = c(0.9, 0.3)) 
  
  girafe(ggobj = g, 
         options = list(
           opts_hover(
             css = girafe_css(
               css = "stroke:gray;r:8pt;",
               text = "stroke:none;fill:black;fill-opacity:1;" ) ),
           opts_zoom(max = 5),
           opts_sizing = opts_sizing(rescale = TRUE),
           opts_tooltip(css = "font-family:sans-serif;
                                           background-color:gray;
                                           color:white;
                                           padding:10px;
                                           border-radius:5px;",
                        use_cursor_pos = FALSE,
                        offx = 0, offy = 330) ))
}


############ Map place-centric trade flows for a county (takes county-to-county matrix or sector list of county-to-county matrices as data inputs)
place_trade_map <- function(df,
                            central_place,
                            sector = NULL,
                            export_flows = TRUE, 
                            geog_year = "2013",
                            censor_scale_lowerbound = 0,
                            ...){
  df <- if(is.null(sector)){
    if(isTRUE(export_flows)){
      t(df[central_place, , drop = F] )
    } else {
      df[, central_place, drop = F]
    }
  } else {
    if(isTRUE(export_flows)){
      t(df[[sector]][central_place, , drop = F] )
    } else {
      df[[sector]][, central_place, drop = F]
    }
  } 
  df <- df %>% 
    as_tibble(rownames = "place") %>%
    rename("trade" = central_place)
  geog <- call_geog(geog_year)
  df <- join_space_with_connectedness(df, geog, ...)
  tfl <- if(isTRUE(export_flows)){
    "Outbound Trade"
  } else {
    "Inbound Trade"
  }
  g <- ggplot(df) + 
    geom_sf_interactive(aes(fill = (round(trade, 2)), 
                            tooltip = if("STATE" %in% names(df)){
                              glue("{NAME}, {STATE}\nFIPS: {place}\nQuantity: {round(trade, 2)}")
                            }else{
                              glue("{NAME}\nFIPS: {place}\nQuantity: {round(trade, 2)}")
                            }, 
                            data_id = place),
                        color = NA) + 
    guides(alpha = "none") +
    coord_sf() +
    theme_void() + 
    scale_fill_viridis(direction = -1, 
                       limits = c(floor(censor_scale_lowerbound), 
                                  ceiling(max(df$trade)))) +
    geom_sf_interactive(data = df[df$place==central_place,], fill = "#8b0000", color = NA) +
    labs(fill = if("STATE" %in% names(df)){
      glue("{df[df$place==central_place,]$NAME}, {df[df$place==central_place,]$STATE} \n{tfl}")
    }else{
      glue("{df[df$place==central_place,]$NAME} \n{tfl}")
    } )  +
    theme(plot.margin = margin(t = 1, r = 1, b = 10, l = 1, unit = "pt"),
          legend.key.size = unit(.2, "cm"),
          legend.title = element_text(size = rel(0.5)), 
          legend.text = element_text(size = rel(0.5)),
          legend.position = c(0.9, 0.3),
          plot.caption = element_text(hjust = 0.9, size = rel(0.5)) ) +
    {
      if(is.null(sector)){
        labs(caption = "Aggregate Trade Flows") 
      } else { 
        labs(caption = glue(sector, ": ", beacode2description(code = sector), " trade flow")) 
      }
    }
  
  girafe(ggobj = g, 
         options = list(
           opts_hover(
             css = girafe_css(
               css = "stroke:gray;r:8pt;",
               text = "stroke:none;fill:black;fill-opacity:1;" ) ),
           opts_zoom(max = 5),
           opts_sizing = opts_sizing(rescale = TRUE),
           opts_tooltip(css = "font-family:sans-serif;
                                           background-color:gray;
                                           color:white;
                                           padding:10px;
                                           border-radius:5px;",
                        use_cursor_pos = FALSE,
                        offx = 0, offy = 330) ))
}



############ Map Outbound to Inbound trade flows (takes county-to-county matrix or sector list of county-to-county matrices as data inputs)
inbound2outbound_map <- function(df,
                                 fill = c("out_less_in", "out2in", "inbound", "outbound"), 
                                 sector = NULL,
                                 geog_year = "2013",
                                 ...){
  fill <- match.arg(fill)
  df <- if(is.null(sector)){
    inbound2outbound(df)
  } else { 
    inbound2outbound(df[[sector]])
  }
  geog <- call_geog(geog_year)
  df <- join_space_with_connectedness(df, geog, ...)
  
  g <- ggplot() + {
    if(fill == "out_less_in" | fill == "out2in"){
      geom_sf_interactive(aes(fill = round(if(fill == "out_less_in"){log(-.data[[fill]])}else{.data[[fill]]}, 2), 
                              tooltip = if("STATE" %in% names(df)){
                                glue("{NAME}, {STATE}\nFIPS: {place}\nInbound: {round(inbound, 2)}\nOutbound: {round(outbound, 2)}\nRatio: {round(out2in, 2)}\nNet: {round(out_less_in, 2)}")
                              }else{
                                glue("{NAME}\nFIPS: {place}\nInbound: {round(inbound, 2)}\nOutbound: {round(outbound, 2)}\nRatio: {round(out2in, 2)}\nNet: {round(out_less_in, 2)}")
                              },
                              data_id = place), 
                          color = NA,
                          data = subset(df, out_less_in < 0)) 
    } else {
      geom_sf_interactive(aes(fill = round(log(.data[[fill]]), 2), 
                              tooltip = if("STATE" %in% names(df)){
                                glue("{NAME}, {STATE}\nFIPS: {place}\nInbound: {round(inbound, 2)}\nOutbound: {round(outbound, 2)}\nRatio: {round(out2in, 2)}\nNet: {round(out_less_in, 2)}")
                              }else{
                                glue("{NAME}\nFIPS: {place}\nInbound: {round(inbound, 2)}\nOutbound: {round(outbound, 2)}\nRatio: {round(out2in, 2)}\nNet: {round(out_less_in, 2)}")
                              },
                              data_id = place), 
                          color = NA,
                          data = df) 
    }
  } + {
    if(fill == "out_less_in"){
      scale_fill_gradient(low = "#56B1F7", high = "#132B43") 
    } else if(fill == "out2in"){
      scale_fill_gradient(low = "#132B43", high = "#56B1F7") 
    } else {
      scale_fill_viridis(direction = -1)
    } 
  } +
    labs(fill = if(fill == "out_less_in"){"Net Sink (log)"} else if(fill == "out2in"){"Sink Ratio"} else if(fill == "inbound"){"Inbound Volume (log)"} else if(fill == "outbound"){"Outbound Volume (log)"}) + 
    new_scale_fill() + {
      if(fill == "out_less_in" | fill == "out2in"){
        geom_sf_interactive(aes(fill = round(if(fill == "out_less_in"){log(.data[[fill]])}else{.data[[fill]]}, 2), 
                                tooltip = if("STATE" %in% names(df)){
                                  glue("{NAME}, {STATE}\nFIPS: {place}\nInbound: {round(inbound, 2)}\nOutbound: {round(outbound, 2)}\nRatio: {round(out2in, 2)}\nNet: {round(out_less_in, 2)}")
                                }else{
                                  glue("{NAME}\nFIPS: {place}\nInbound: {round(inbound, 2)}\nOutbound: {round(outbound, 2)}\nRatio: {round(out2in, 2)}\nNet: {round(out_less_in, 2)}")
                                },
                                data_id = place), 
                            color = NA,
                            data = subset(df, out_less_in > 0))
      }
    } + {
      if(fill == "out_less_in" | fill == "out2in"){ scale_fill_gradient(low = "#feb24c", high = "#f03b20") }
    } + {
      if(fill == "out_less_in" | fill == "out2in"){ labs(fill = if(fill == "out_less_in"){"Net Source (log)"} else if(fill == "out2in"){"Source Ratio"}) }
    } +
    coord_sf() +
    theme_void() +
    theme(plot.margin = margin(t = 1, r = 1, b = 10, l = 1, unit = "pt"),
          legend.key.size = unit(.2, "cm"),
          legend.title = element_text(size = rel(0.5)), 
          legend.text = element_text(size = rel(0.5)),
          legend.position = c(0.9, 0.2),
          plot.caption = element_text(hjust = 0.9, size = rel(0.5))) +
    {
      if(is.null(sector)){
        labs(caption = "Aggregate Sector Provenance") 
      } else { 
        labs(caption = glue(sector, ": ", beacode2description(code = sector), " provenance")) 
      }
    }
  
  girafe(ggobj = g, 
         options = list(
           opts_hover(
             css = girafe_css(
               css = "stroke:gray;r:8pt;",
               text = "stroke:none;fill:black;fill-opacity:1;" ) ),
           opts_zoom(max = 5),
           opts_sizing = opts_sizing(rescale = TRUE),
           opts_tooltip(css = "font-family:sans-serif;
                                           background-color:gray;
                                           color:white;
                                           padding:10px;
                                           border-radius:5px;",
                        use_cursor_pos = FALSE,
                        offx = 0, offy = 330),
           opts_toolbar = opts_toolbar(position = "top", saveaspng = FALSE) ))
}


############ Map Net Supply and Demand of industry/commodity goods
net_sd_map <- function(df,
                       fill,
                       ...){
  
  g <- ggplot() + 
    geom_sf_interactive(aes(fill = round((.data[[fill]]), 2), 
                            tooltip = if("STATE" %in% names(df)){
                              glue("{NAME}, {STATE}\nFIPS: {place}\nValue: {round({.data[[fill]]}, 2)}")
                            }else{
                              glue("{NAME}\nFIPS: {place}\nValue: {round({.data[[fill]]}, 2)}")
                            },
                            data_id = place), 
                        color = NA,
                        data = df[df[[fill]] > 0,]) +
    scale_fill_gradient(low = "#feb24c", high = "#f03b20") + 
    labs(fill = "Net Supply") +
    new_scale_fill() +
    geom_sf_interactive(aes(fill = round(abs(.data[[fill]]), 2), 
                            tooltip = if("STATE" %in% names(df)){
                              glue("{NAME}, {STATE}\nFIPS: {place}\nValue: {round(abs({.data[[fill]]}), 2)}")
                            }else{
                              glue("{NAME}\nFIPS: {place}\nValue: {round(abs({.data[[fill]]}), 2)}")
                            },
                            data_id = place), 
                        color = NA,
                        data = df[df[[fill]] < 0,]) +
    scale_fill_gradient(low = "#56B1F7", high = "#132B43") + 
    labs(fill = "Net Demand") +
    new_scale_fill() +
    geom_sf_interactive(aes(fill = round(abs(.data[[fill]]), 2),
                            tooltip = if("STATE" %in% names(df)){
                              glue("{NAME}, {STATE}\nFIPS: {place}\nValue: {round({.data[[fill]]}, 2)}")
                            }else{
                              glue("{NAME}\nFIPS: {place}\nValue: {round({.data[[fill]]}, 2)}")
                            },
                            data_id = place),
                        color = NA,
                        data = df[df[[fill]] == 0,]) +
    scale_fill_gradient(low = "grey80", high = "grey80", guide = "none") + 
    coord_sf() +
    theme_void() +
    theme(plot.margin = margin(t = 1, r = 1, b = 10, l = 1, unit = "pt"),
          legend.key.size = unit(.2, "cm"),
          legend.title = element_text(size = rel(0.5)), 
          legend.text = element_text(size = rel(0.5)),
          legend.position = c(0.9, 0.2),
          plot.caption = element_text(hjust = 0.9, size = rel(0.5))) +
    labs(caption = glue(fill, ": ", beacode2description(code = fill) ))
  
  girafe(ggobj = g, 
         options = list(
           opts_hover(
             css = girafe_css(
               css = "stroke:gray;r:8pt;",
               text = "stroke:none;fill:black;fill-opacity:1;" ) ),
           opts_zoom(max = 5),
           opts_sizing = opts_sizing(rescale = TRUE),
           opts_tooltip(css = "font-family:sans-serif;
                                           background-color:gray;
                                           color:white;
                                           padding:10px;
                                           border-radius:5px;",
                        use_cursor_pos = FALSE,
                        offx = 0, offy = 330),
           opts_toolbar = opts_toolbar(position = "top", saveaspng = FALSE) ))
}

############ Map ratio of NIS to NID
nis2nid_map <- function(df){
  g <- ggplot(df) +
    geom_sf_interactive(aes(fill = (round(nis2nid, 5)), 
                            tooltip = if("STATE" %in% names(df)){
                              glue("{NAME}, {STATE}\nFIPS: {place}\nTotal Output: {round(total_output, 0)}\nTotal Input: {round(total_input, 0)}\nNet Export Supply: {round(total_nis, 0)}\nNet Import Demand: {round(total_nid, 0)}\nRatio: {round(nis2nid, 2)} ")
                            }else{
                              glue("{NAME}\nFIPS: {place}\nTotal Output: {round(total_output, 0)}\nTotal Input: {round(total_input, 0)}\nNet Export Supply: {round(total_nis, 0)}\nNet Import Demand: {round(total_nid, 0)}\nRatio: {round(nis2nid, 2)} ")
                            },
                            data_id = place
    ), 
    color = NA
    ) + 
    guides(alpha = "none") +
    coord_sf() +
    theme_void() +
    scale_fill_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0) +
    labs(fill = glue("Export Supply to\n Import Demand Ratio"))  +
    theme(plot.margin = margin(t = 1, r = 1, b = 10, l = 1, unit = "pt"),
          legend.key.size = unit(.2, "cm"),
          legend.title = element_text(size = rel(0.5)), 
          legend.text = element_text(size = rel(0.5)),
          legend.position = c(0.9, 0.3)) 
  
  girafe(ggobj = g, 
         options = list(
           opts_hover(
             css = girafe_css(
               css = "stroke:gray;r:8pt;",
               text = "stroke:none;fill:black;fill-opacity:1;" ) ),
           opts_zoom(max = 5),
           opts_sizing = opts_sizing(rescale = TRUE),
           opts_tooltip(css = "font-family:sans-serif;
                                             background-color:gray;
                                             color:white;
                                             padding:10px;
                                             border-radius:5px;",
                        use_cursor_pos = FALSE,
                        offx = 0, offy = 330) ))
  
}

absorption_density_plot <- function(df,
                                    fill,
                                    fill_lab,
                                    normalized = TRUE,
                                    trans = NULL,
                                    #colorbreaks = c("#440154FF" = 1:7 , "#1E9C89FF" = 8:17 , "#D64B40FF" = 18, "#FDE725FF" =  19:21)){
                                    colorbreaks = c("#440154FF" = 2000:2006, "#1E9C89FF" = 2007:2016 , "#D64B40FF" = 2017, "#FDE725FF" =  2018:2020)){
  
  g <- ggplot(df) + 
    geom_density_interactive(aes(x = (.data[[fill]]), 
                                 tooltip = id,
                                 data_id = id,
                                 `data-id` = id,
                                 color = id),
                             extra_interactive_params = "data-id"
                             #position = position_fill(reverse = TRUE)
    ) + { 
      if(!is.null(colorbreaks)){
        scale_color_manual_interactive(values = substr(names(colorbreaks), 1, 9),
                                       extra_interactive_params = "data-id",
                                       `data-id` = function(x) x, 
                                       data_id = function(x) x) 
      }     
    } + { 
      if(!is.null(trans)){
        scale_x_continuous(trans = trans)
      }     
    } + 
    labs(color = "CBP Year", x = paste(if(isTRUE(normalized)){"Normalized"}else{"Nominal"}, fill_lab), y = "Density")
  
  girafe(ggobj = g, 
         options = list(
           opts_hover(
             css = girafe_css(
               css = "stroke:orange;r:12pt;",
               text = "stroke:none;fill:black;fill-opacity:1;" ) ),
           opts_zoom(max = 5),
           opts_sizing = opts_sizing(rescale = TRUE),
           opts_tooltip(css = "font-family:sans-serif;
                                           background-color:gray;
                                           color:white;
                                           padding:10px;
                                           border-radius:5px;") )
  )
}

############ Map various spatial impedance functions
impedance_distribution_map <- function(central_place = "20183", 
                                       decay_function = c("bisquare", "hyper", "gaus", "expo", "power", "dprox", "eprox", "bprox"),
                                       boundary_limit = 200,
                                       rms_width = 200,
                                       hyper_decay_constant = 200,
                                       expo_decay_constant = 200,
                                       decay_power = 1/8,
                                       queen = TRUE,
                                       ...){
  decay_function <- match.arg(decay_function)
  if(decay_function == "bisquare"){
    sf <- bisquare_impedance_mat(decay_zero = miles2meters(boundary_limit), ...)
    caption <- paste0("Bi-square Decay: ", boundary_limit," mile limit")}
  if(decay_function == "hyper"){
    sf <- hyper_impedance_mat(decay_constant = miles2meters(hyper_decay_constant), ...)
    caption <- paste0("Hyperbolic Secant Decay: ", hyper_decay_constant," mile scewness scalar")}
  if(decay_function == "gaus"){
    sf <- gaus_impedance_mat(rms_width = miles2meters(rms_width), ...)
    caption <- paste0("Gaussian Decay: ", rms_width," mile RMS scalar")}
  if(decay_function == "expo"){
    sf <- expo_impedance_mat(decay_constant = miles2meters(expo_decay_constant), ...)
    caption <- paste0("Exponential Decay: ", expo_decay_constant," mile disintegration scalar")}
  if(decay_function == "power"){
    sf <- power_impedance_mat(decay_power = decay_power, ...)
    caption <- paste0("Inverse Power Decay: decay power of ", decay_power)}
  if(decay_function == "dprox"){
    sf <- dprox_mat(boundary_limit = miles2meters(boundary_limit), ...)
    caption <- paste0("Uniform Center Proximity: ", boundary_limit," mile limit")}
  if(decay_function == "eprox"){
    sf <- dist_matb(boundary_limit = miles2meters(boundary_limit), ...)
    caption <- paste0("Uniform Edge Proximity: ", boundary_limit," mile limit")}
  if(decay_function == "bprox"){
    sf <- bprox_mat(queen = queen, ...)
    caption <- if(isTRUE(queen)){
      paste0("Queen Adjacent Borders")
    }else{
      paste0("Rook Adjacent Borders")}}
  sf <- data.frame(place = rownames(sf), 
                   imped = c(sf[, central_place, drop = FALSE]))
  df <- call_geog(...) %>% .[!grepl('^(02|15|72)', .$place), ] %>% left_join(., sf, by = "place")
  g <- ggplot() + 
    geom_sf_interactive(aes(fill = imped,
                            tooltip = paste0("Value: ", round(.data[["imped"]], 4), "\nCounty: ", .data[["NAME"]], "\nFIPS: ", .data[["place"]]), 
                            data_id = place), 
                        data = df, 
                        color = NA) + 
    guides(fill = guide_legend(title = "Impedance", 
                               reverse = TRUE)) + 
    theme_void() + 
    scale_fill_viridis(direction = -1, 
                       limits=c(floor(0), 
                                ceiling(1))) +
    theme(plot.margin = margin(t = 1, r = 1, b = 10, l = 1, unit = "pt"),
          legend.key.size = unit(.2, "cm"),
          legend.title = element_text(size = rel(0.5)), 
          legend.text = element_text(size = rel(0.5)),
          legend.position = c(0.9, 0.3),
          plot.caption = element_text(hjust = 0.9, size = rel(0.5))) + 
    labs(caption = caption)
  girafe(ggobj = g, 
         options = list(
           opts_hover(
             css = girafe_css(
               css = "stroke:orange;r:12pt;",
               text = "stroke:none;fill:black;fill-opacity:1;" ) ),
           opts_zoom(max = 5),
           opts_sizing = opts_sizing(rescale = TRUE),
           opts_tooltip(css = "font-family:sans-serif;
                                             background-color:gray;
                                             color:white;
                                             padding:10px;
                                             border-radius:5px;") ))
}

############ Map hierarchy of neighbors from central place
hierarchy_of_neighbors_map <- function(central_place = "20183", 
                                       ...){
  df <- inner_join(call_geog(...), neighbor_of_neighbor(central_place, ...), by = "place")
  g <- ggplot() + 
    geom_sf_interactive(aes(fill = nn, 
                            tooltip = paste0("Interval level: ", .data[["nn"]], "\nCounty: ", .data[["NAME"]], "\nFIPS: ", .data[["place"]]), 
                            data_id = place), 
                        data = df, 
                        color = NA) + 
    guides(fill = guide_legend(title = "Neighbors", 
                               reverse = TRUE)) + 
    theme_void() +
    theme(plot.margin = margin(t = 1, r = 1, b = 10, l = 1, unit = "pt"),
          legend.key.size = unit(.2, "cm"),
          legend.title = element_text(size = rel(0.75)), 
          legend.text = element_text(size = rel(0.75)),
          legend.position = "none") + 
    labs(caption = paste0(df$COUNTY[df$nn=="n0"], ", ", df$STATE[df$nn=="n0"], " Hierarchy of Neighbors"))
  girafe(ggobj = g, 
         options = list(
           opts_hover(
             css = girafe_css(
               css = "stroke:orange;r:12pt;",
               text = "stroke:none;fill:black;fill-opacity:1;" ) ),
           opts_zoom(max = 5),
           opts_sizing = opts_sizing(rescale = TRUE),
           opts_tooltip(css = "font-family:sans-serif;
                                               background-color:gray;
                                               color:white;
                                               padding:10px;
                                               border-radius:5px;") ))
}

############ Map SNA metrics from county-by-county absorption matrix
sna_value_map <- function(df,
                          metric = c("eigen_centrality", 
                                     "alpha_centrality", 
                                     "closeness", 
                                     "harmonic_centrality", 
                                     "hub_score", 
                                     "authority_score", 
                                     "page_rank", 
                                     "strength"),
                          normalize = c("none", "row", "column"),
                          scale = FALSE,
                          mode = c("total", "in", "out", "all"),
                          geog_year = "2013",
                          ...){
  geog <- call_geog(geog_year)
  metric <- match.arg(metric)
  mode <- match.arg(mode)
  normalize <- match.arg(normalize)
  if(normalize == "row"){
    df <- df %>% apply(1, rescale) %>% t()
  }
  if(normalize == "column"){
    df <- df %>% apply(2, rescale)
  }
  df <- df %>% graph_from_adjacency_matrix(weighted = TRUE,
                                           mode = "directed")
  if(metric == "eigen_centrality"){
    df <- df %>% eigen_centrality(directed = TRUE, scale = scale) %>% .[[1]]
  }
  if(metric == "alpha_centrality"){
    df <- df %>% alpha_centrality(loops = TRUE)
  }
  if(metric == "closeness"){
    df <- df %>% closeness(mode = mode)
  }
  if(metric == "harmonic_centrality"){
    df <- df %>% harmonic_centrality()
  }
  if(metric == "hub_score"){
    df <- df %>% hub_score(scale = scale) %>% .[[1]]
  }
  if(metric == "authority_score"){
    df <- df %>% authority_score(scale = scale) %>% .[[1]]
  }
  if(metric == "page_rank"){
    df <- df %>% page_rank(directed = TRUE) %>% .[[1]]
  }
  if(metric == "strength"){
    df <- df %>% strength(mode = mode)
  }
  df <- as_tibble(df, rownames = "place") %>% 
    join_space_with_connectedness(., geog, ...)
  g <- ggplot(df) +
    geom_sf_interactive(aes(fill = (round(value, 5)),
                            tooltip = if("STATE" %in% names(df)){
                              glue("{NAME}, {STATE}\nFIPS: {place}\nValue: {round(value, 5)}")
                            }else{
                              glue("{NAME}\nFIPS: {place}\nValue: {round(value, 5)}")
                            },
                            data_id = place),
                        color = NA) +
    guides(alpha = "none") +
    coord_sf() +
    theme_void() +
    scale_fill_viridis(direction = -1) +
    labs(fill = 
           if(metric == "eigen_centrality"){"Eigen Centrality"} else 
             if(metric == "alpha_centrality"){"Alpha Centrality"} else 
               if(metric == "closeness"){"Closeness"} else 
                 if(metric == "harmonic_centrality"){"Harmonic Centrality"} else 
                   if(metric == "hub_score"){"Hub Score"} else 
                     if(metric == "authority_score"){"Authority Score"} else 
                       if(metric == "page_rank"){"Page Rank"} else 
                         if(metric == "strength"){"Strength"}
    ) +
    theme(plot.margin = margin(t = 1, r = 1, b = 10, l = 1, unit = "pt"),
          legend.key.size = unit(.2, "cm"),
          legend.title = element_text(size = rel(0.5)),
          legend.text = element_text(size = rel(0.5)),
          legend.position = c(0.9, 0.3),
          plot.caption = element_text(hjust = 0.9, size = rel(0.5)) )
  
  girafe(ggobj = g,
         options = list(
           opts_hover(
             css = girafe_css(
               css = "stroke:gray;r:8pt;",
               text = "stroke:none;fill:black;fill-opacity:1;" ) ),
           opts_zoom(max = 5),
           opts_sizing = opts_sizing(rescale = TRUE),
           opts_tooltip(css = "font-family:sans-serif;
                                             background-color:gray;
                                             color:white;
                                             padding:10px;
                                             border-radius:5px;",
                        use_cursor_pos = FALSE,
                        offx = 0, offy = 330),
           opts_toolbar = opts_toolbar(position = "top", saveaspng = FALSE) ))
}


# Display end time
log_info("Define viz functions end")






