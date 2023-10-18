
#A pruned congruent branch of backend functions using a data warehouse to play with BEA, NAICS and IO infrastructure
 
# Load and attach necessary packages
library(rprojroot)
library(tidyverse)
library(magrittr)
library(geosphere)
library(arrow)

#libraries used elsewhere
# library(fs)
# library(rlog)
# library(reticulate)
# library(sf)
# library(spdep)
# library(reshape2)
# library(parallel)
# library(viridis)
# library(REAT)


# rombi node warehouse to store and call data
if (!file.exists(file.path(find_rstudio_root_file(), "data", "warehouse"))) {
  dir.create(file.path(find_rstudio_root_file(), "data", "warehouse"), recursive = T)
}

# using data from disk in warehouse built from pubdata python modules
source(file.path(find_rstudio_root_file(), "nbs", "warehouse.R"))

#Utility functions##############################################################

#simple independent utility functions in base R for cohesion, conversion, or manipulation
source(file.path(find_rstudio_root_file(), "nbs", "basic_utilities.R"))

#BEA NAICS concordance##########################################################

# Call and tidy NAICS to BEA industry concordance table
call_industry_concordance <- function(){
  df <- get_naics_df() %>% filter(NAICS != "n.a.") %>% filter(NAICS != "NaN") 
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
  df %<>% add_row(SECTOR = "53",
                  SUMMARY = "531",
                  U_SUMMARY = "531",
                  DETAIL = "531",
                  DESCRIPTION = "Housing",
                  NAICS = "531",
                  .before = which(df$SECTOR == '53')[1])
  df %<>% filter(DETAIL != "531HST")
  df %<>% filter(DETAIL != "531ORE")
  df <- df[order(df$NAICS), ]
  rownames(df) <- 1:nrow(df)
  b <- c("11", "21", "22", "23", "31G", "31G", "31G", "42", "44RT", "44RT", "48TW", "48TW", "51", "FIRE", "FIRE", "PROF", "PROF", "PROF", "6", "6", "7", "7", "81", "G")
  n <- c("11", "21", "22", "23", "31", "32", "33", "42", "44", "45", "48", "49", "51", "52", "53", "54", "55", "56", "61", "62", "71", "72", "81", "92")
  for(i in 1:length(n)){
    df$SECTOR[substr(df$NAICS, 1,2) %in% n[i]] <- b[i]
  }
  return(df)
}

# Get specific industry NAICS to BEA concordance
ilevel_concord <- function(ilevel = c("det", "sum", "sec")){
  ilevel <- match.arg(ilevel)
  x <- call_industry_concordance()
  if(ilevel == "det"){
    df <- x %>% select(DETAIL, NAICS)
  }
  if(ilevel == "sum"){
    df <- x %>% select(SUMMARY, NAICS) 
    df$NAICS <- substr(df$NAICS, 1,3)
    # 3-digit NAICS maps to more then one summary for these, we collapse them into one
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

# Join ag classes with BEA industries
ag2bea <- function(afbd = file.path(find_rstudio_root_file(),"data","raw","afbd.csv")
                   ){
  afbd <- read.csv(afbd)
  afbd$NAICS <- NA
  afbd$naics <- afbd[[1]]
  conc <- ilevel_concord(ilevel = "det")
  n <- names(conc)[1]
  for(i in unique(conc$NAICS)){
    x <- paste0("^", i) %>% 
      {afbd[grepl(., afbd$naics), "naics"]} %>% 
      unique() %>% 
      unlist()
    if(length(x)<1) {next}
    afbd[afbd$naics %in% x, ]$NAICS <- i
  }
  df <- left_join(afbd, conc, by = "NAICS")
}

#CBSA tables####################################################################

# Call up and clean CBSA concordance codes
call_cbsa_concord <- function(year, 
                              ...){
  cbsa_year <- year2cbsa(year)  
  df <- get_cbsa_delin_df(cbsa_year)
  df$CBSA_TITLE <- sapply(strsplit(df$CBSA_TITLE, ","), "[", 1)
  df$CBSA_TITLE <- paste(df$CBSA_TITLE, rep("CBSA", length(df$CBSA_TITLE)))
  df$place <- paste0(df$STATE_CODE, df$COUNTY_CODE)
  df <- df %>% select(CBSA_CODE, place, CBSA_TITLE)
  return(df)
}

# Convert a fips code into a cbsa code 
fips2cbsa <- function(fips,
                      year, 
                      ...){
  cb <- call_cbsa_concord(year)
  if(isFALSE(fips %in% paste0(dataRetrieval::countyCd$STATE, dataRetrieval::countyCd$COUNTY))){
    warning("FIPS entry [",fips,"] not found in ANSI FIPS records\n See: https://www2.census.gov/geo/docs/reference/codes/national_county.txt")
  }
  if(isTRUE(fips %in% cb$place)){cb$CBSA_CODE[fips == cb$place]}else{fips}
}

#RUCC tables####################################################################

# Call up RUCC data
call_rucc <- function(year){
  df <- get_ruc_df() %>% filter(RUC_YEAR == year2rucc(year))
  df$place <- df$FIPS
  return(df)
}

#Geography tables###############################################################

# Call up and clean census shapefiles
call_tiger <- function(year = 2013,
                       scale = c("20m", "500k", "5m"),
                       geometry = TRUE,
                       ...){
  scale <- match.arg(scale)
  df <- get_county_df(year2tiger(year), 
                                geometry, 
                                scale)
  df %<>% rename(place = CODE)
  df$COUNTY <- paste(df$NAME, "County")
  if(isTRUE(geometry)){
    #df$center <- st_centroid(df$geometry)
    df$center <- df$geometry %>% st_transform("EPSG:26911") %>% st_centroid() %>% st_transform(st_crs(df)[[1]]) 
  }
  st <- get_state_df(geometry = FALSE) %>% select(1:3) 
  names(st) <- c("STATE_CODE", "STATE_NAME", "STATE")
  df <- left_join(df, st, by = "STATE_CODE")
  df %<>% arrange(place)
  return(df)
}

# Get county name from FIPS code
fips2name <- function(fips,
                      year = 2012,
                      long = FALSE,
                      ...){
  df <- call_tiger(year2tiger(year), geometry = F)
  if(long){
    df <- paste0(df$COUNTY[df$place == fips],", ", df$STATE_NAME[df$place == fips])
  } else {
    df <- df$NAME[df$place == fips]
  }
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

# Call geographic features
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

# Generate neighbors of neighbors hierarchical vector for a place ad nauseam
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

#BEA tables#####################################################################
# Call and clean pubdata BEA IO Use table
call_use_table <- function(year,
                           ilevel = c("det", "sum", "sec"), 
                           ...){
  ilevel <- match.arg(ilevel)
  df <- year2bea(year, ilevel) %>% 
    get_use(., ilevel) %>% 
    as.matrix()
  df[is.na(df)] = 0
  return(df)
}

# Call and clean pubdata BEA IO Supply table
call_supply_table <- function(year,
                              ilevel = c("det", "sum", "sec"), 
                              ...){
  ilevel <- match.arg(ilevel)
  df <- year2bea(year, ilevel) %>% 
    get_sup(., ilevel) %>% 
    as.matrix()
  df[is.na(df)] = 0
  if (ilevel == "sec"){
    #temp fix needs correction in rurec.pubdata.bea_io module
    rownames(df) <- c(colnames(df)[1:15], "Used", "Other", "T017")
  }
  return(df)
}

# Aggregate and tidy a commodity-by-industry BEA matrix
condense_bea_matrix <- function(matrix,
                                ilevel){
  if(ilevel == "det"){
    df <- matrix %>% 
      matrix_collapse(., grep("^23", colnames(.), value = TRUE), "23") %>% 
      matrix_collapse(., grep("^531", colnames(.), value = TRUE), "531") %>% 
      .[!grepl("4200ID|S00402|S00300", rownames(.)), !grepl("4200ID", colnames(.)), drop=F]
  }
  if(ilevel == "sum"){ 
    df <- matrix %>% 
      matrix_collapse(., grep("^336", colnames(.), value = TRUE), "336") %>% 
      matrix_collapse(., grep("^541", colnames(.), value = TRUE), "541") %>% 
      matrix_collapse(., grep("^(HS|ORE)", colnames(.), value = TRUE), "531")
  }
  if (ilevel == "sec"){
    df <- matrix
  }
  return(df)
}

# Aggregate and tidy a BEA row-vector
condense_bea_vector <- function(vector,
                                ilevel){
  if(ilevel == "det"){
    df <- vector %>% 
      vector_collapse(., grep("^23", colnames(.), value = TRUE), "23") %>% 
      vector_collapse(., grep("^531", colnames(.), value = TRUE), "531") %>% 
      .[,!grepl("4200ID|S00402|S00300", colnames(.)), drop=F]
  }
  if(ilevel == "sum"){ 
    df <- vector %>% 
      vector_collapse(., grep("^336", colnames(.), value = TRUE), "336") %>% 
      vector_collapse(., grep("^541", colnames(.), value = TRUE), "541") %>% 
      vector_collapse(., grep("^(HS|ORE)", colnames(.), value = TRUE), "531")
  }
  if (ilevel == "sec"){
    df <- vector
  }
  return(df)
}

#needs more adjustments so that all consolidating for CBP is done beforehand (e.g., government)?
# Get Use matrix and tidy structure for use with NAICS adjacent processes
use_matrix <- function(year,
                       ilevel = c("det", "sum", "sec"),
                       ...){
  ilevel <- match.arg(ilevel)
  df <- call_use_table(year, ilevel, ...) %>% 
    .[1:(which(rownames(.) == "T005")-1), 1:(which(colnames(.) == "T001")-1)] %>% 
    condense_bea_matrix(., ilevel)
  return(df)
}
### Get Supply matrix and tidy structure for use with NAICS adjacent processes
supply_matrix <- function(year,
                          ilevel = c("det", "sum", "sec"),
                          ...){
  ilevel <- match.arg(ilevel)
  df <- call_supply_table(year, ilevel, ...) %>% 
    .[1:(nrow(.)-1), 1:(which(colnames(.) == "T007")-1)] %>% 
    condense_bea_matrix(., ilevel)
  return(df)
}

# Get National BEA Total Industry Output Vector and tidy structure for use with NAICS adjacent processes
industry_output <- function(year,
                            ilevel = c("det", "sum", "sec"),
                            ...){
  ilevel <- match.arg(ilevel)
  df <- call_supply_table(year, ilevel, ...) %>% 
    .[nrow(.), 1:(which(colnames(.) == "T007")-1), drop=F] %>% 
    condense_bea_vector(., ilevel) %>% 
    t() %>% 
    `colnames<-`("T017")
  return(df)
}
# Get National BEA Total Commodity Output Vector and tidy structure for use with NAICS adjacent processes
commodity_output <- function(year,
                             ilevel = c("det", "sum", "sec"),
                             ...){
  ilevel <- match.arg(ilevel)
  df <- call_supply_table(year, ilevel, ...) %>% 
    .[1:(nrow(.)-1), "T007", drop=F] %>% 
    t() %>% 
    condense_bea_vector(., ilevel) %>% 
    t() %>% 
    `colnames<-`("T007")
  return(df)
}

# Commodities-by-Industries parallel to ordinary technical coefficients matrix 
b_matrix <- function(year,
                     ilevel = c("det", "sum", "sec"),
                     ...){
  ilevel <- match.arg(ilevel)
  u_mat <- use_matrix(year, ilevel, ...)
  x <- industry_output(year, ilevel, ...)
  df <- u_mat %*% diag(1/as.vector(x))
  colnames(df) <- colnames(u_mat)
  return(df)
}

# Commodity Composition of Industry Outputs
c_matrix <- function(year,
                     ilevel = c("det", "sum", "sec"),
                     ...){
  ilevel <- match.arg(ilevel)
  supply_mat <- supply_matrix(year, ilevel, ...)
  x <- industry_output(year, ilevel, ...)
  df <- supply_mat %*% diag(1/as.vector(x))
  colnames(df) <- colnames(supply_mat)
  return(df)
}

# Industry Source of Commodity Outputs
d_matrix <- function(year,
                     ilevel = c("det", "sum", "sec"),
                     ...){
  ilevel <- match.arg(ilevel)
  supply_mat <- supply_matrix(year, ilevel, ...)
  q <- commodity_output(year, ilevel, ...)
  df <- t(supply_mat) %*% diag(1/as.vector(q))
  colnames(df) <- rownames(supply_mat)
  return(df)
}


#CBP tables#####################################################################

# Call up and clean CBP ($1,000 of dollars)
call_cbp <- function(year,
                     cbp_scale = c("county", "state", "us"),
                     imputed = TRUE, 
                     national_wages = FALSE, 
                     ...){
  cbp_year <- year2cbp(year)
  cbp_scale <- match.arg(cbp_scale)
  #If TRUE derive county-level annual payroll from county-level EFSY imputed employment and CBP quarterly payroll
  if(imputed){
    df <- get_cbp_year_pq(cbp_year) %>% as.character() %>% open_dataset() %>% collect() %>% as.data.frame()
    df$place <- paste0(df$fipstate, df$fipscty)
    # stopgap for getting imputed payrolls when suppression exists even at the state-level
    if (national_wages){
      #national employment derived from sum of county employment
      nat_ind_emp_sum <- df %>% {aggregate(.$emp, list(.$industry), FUN=sum)}
      colnames(nat_ind_emp_sum) <- c("industry", "nat_emp")
      #national annual payroll derived from sum of sub-establishment type payroll
      nat_ind_ap_sum <- get_df(geo = "us", year = cbp_year) %>% .[.$lfo != "-", ] %>% {aggregate(.$ap, list(.$industry), FUN=sum)}
      colnames(nat_ind_ap_sum) <- c("industry", "natsub_ap")
      nat_sums <- inner_join(nat_ind_emp_sum, nat_ind_ap_sum, by = "industry")
      nat_ind_wage <- get_df(geo = "us", year = cbp_year) %>% .[.$lfo == "-", ] %>% inner_join(., nat_sums, by = "industry")
      nat_ind_wage$nat_wage = nat_ind_wage$qp1 / nat_ind_wage$emp * 4
      #If national quarterly payroll is zero use national annual payroll
      x <- nat_ind_wage$qp1 == 0
      nat_ind_wage[x, ]$nat_wage = nat_ind_wage[x, ]$ap / nat_ind_wage[x, ]$emp 
      #If CBP/EFSY employment is zero use sum of county employment
      x <- nat_ind_wage$emp == 0
      nat_ind_wage[x, ]$nat_wage = nat_ind_wage[x, ]$qp1 / nat_ind_wage[x, ]$nat_emp * 4
      #If CBP/EFSY employment is zero AND national quarterly payroll is zero use national annual payroll and sum of county employment
      x <- nat_ind_wage$emp == 0 & nat_ind_wage$qp1 == 0
      nat_ind_wage[x, ]$nat_wage = nat_ind_wage[x, ]$ap / nat_ind_wage[x, ]$nat_emp 
      #If CBP/EFSY employment is zero AND national quarterly payroll is zero AND national annual payroll is zero use sum of subsector payroll and sum of county employment
      x <- nat_ind_wage$emp == 0 & nat_ind_wage$qp1 == 0  & nat_ind_wage$ap == 0
      nat_ind_wage[x, ]$nat_wage = nat_ind_wage[x, ]$natsub_ap / nat_ind_wage[x, ]$nat_emp 
      nat_ind_wage <- nat_ind_wage %>% subset(select = c("industry", "nat_wage"))
      df <- left_join(df, nat_ind_wage, by = "industry")
      x <- df$emp != 0 & !is.finite(df$ap) & is.finite(df$nat_wage)
      df[x, ]$ap <- df[x, ]$emp * df[x, ]$nat_wage
    }
  } else {
    df <- get_df(geo = cbp_scale, 
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
  df$ap[!is.finite(df$ap)] <- 0
  return(df)
}

# Agglomerate NAICS and BEA concordance by year and industry specificity (sector, summary, or detail)
place_industry_economy_long <- function(year, 
                                        cbp_scale = c("county", "state", "us"), 
                                        imputed = TRUE,
                                        ...){
  cbp_scale <- match.arg(cbp_scale)
  conc <- ilevel_concord(ilevel = list(...)$ilevel)
  n <- names(conc)[1]
  cbp_dat <- call_cbp(year = year, imputed = imputed, cbp_scale = cbp_scale, ...)
  if (cbp_scale != "county" & isFALSE(imputed)){
    cbp_dat <- cbp_dat[cbp_dat$lfo == "-", ]
  }
  x <- left_join(cbp_dat, conc, by = "NAICS") 
  x <- x %>% .[!is.na(.[dim(.)[2]] ),] %>% 
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

# Generate industry activity ("ap", "emp", "qp1", or "est") by county from CBP in terms of BEA industry codes
industry_activity_by_place <- function(year, 
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


#Ag Census######################################################################

##Note county totals of 111900 and 112A00 are 50% larger than their national level counterparts
# Call up and clean Ag Output data ($1,000 of dollars)
call_agoutput <- function(year, 
                          geo_level = c("county", "state", "national"), 
                          ...){
  geo_level <- match.arg(geo_level)
  ag_year <- year2agcensus(year)
  df <- get_farm_sales_by_bea_detail(ag_year, geo_level) %>% as.data.frame()
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

# Derive adjusted national ag gross industry output (in thousands of dollars)
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
















