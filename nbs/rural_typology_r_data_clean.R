# Clean imported outside data

# Load and attach necessary packages
library(rprojroot)
library(dplyr)
library(geosphere)
library(spdep)
library(rlog)
library(reticulate)

# Display start time
log_info("Define data clean start")

# Connect and parse code from another file 
source(file.path(find_rstudio_root_file(), "nbs", "rural_typology_r_data_import.R"))

data_dir = file.path(find_rstudio_root_file(), "data", "robjs")

# Import and cleanup "pubdata" County Business Patterns data
if (!file.exists(file.path(data_dir, "CBP_2019p"))){
  CBP_2019p <- cbp$get_df("county", 2019L)
  CBP_2019p %<>% rename(NAICS = industry)
  CBP_2019p$place <- paste0(CBP_2019p$fipstate, CBP_2019p$fipscty)
  CBP_2019p %<>% select(fipstate, fipscty, place, NAICS, emp, qp1, ap, est)
  saver(CBP_2019p)
  rm(CBP_2019p) 
}
log_info("Import 'pubdata' CBP complete")


# Specific Detail level concordance
if (!file.exists(file.path(data_dir, "det_cord"))){
  x <- bea_io$get_naics_df() %>% filter(NAICS != "n.a.") %>% filter(NAICS != "NaN") 
  det_cord <- c()
  det_cord$DETAIL <- unlist(x$DETAIL, use.names = FALSE) 
  det_cord$NAICS <- unlist(x$NAICS, use.names = FALSE) 
  det_cord <- as.data.frame(det_cord)
  det_cord$NAICS <- ifelse(det_cord$NAICS == "531" & det_cord$DETAIL == "531HST", "531*",  det_cord$NAICS)
  det_cord %<>% add_row(DETAIL = "23", NAICS = "23", .after = 39)
  det_cord %<>% filter(NAICS != "23*")
  det_cord %<>% filter(NAICS != "531*")
  det_cord <- det_cord[order(det_cord$NAICS), ]
  rownames(det_cord) <- 1:nrow(det_cord)
  write.csv(det_cord, file.path(find_rstudio_root_file(), "data", "det_cord"), row.names = FALSE)
  saver(det_cord)
  rm(det_cord, x)
}

# Specific Summary level concordance
if (!file.exists(file.path(data_dir, "sum_cord"))){
  x <- bea_io$get_naics_df() %>% filter(NAICS != "n.a.") %>% filter(NAICS != "NaN") 
  sum_cord <- c()
  sum_cord$SUMMARY <- unlist(x$SUMMARY, use.names = FALSE) 
  sum_cord$NAICS <- unlist(x$NAICS, use.names = FALSE) 
  sum_cord <- as.data.frame(sum_cord)
  sum_cord$NAICS <- ifelse(sum_cord$NAICS == "531" & sum_cord$SUMMARY == "HS", "531*",  sum_cord$NAICS)
  sum_cord %<>% add_row(SUMMARY = "23", NAICS = "23", .after = 39)
  sum_cord %<>% filter(NAICS != "23*")
  sum_cord %<>% filter(NAICS != "531*")
  sum_cord <- sum_cord[order(sum_cord$NAICS), ]
  rownames(sum_cord) <- 1:nrow(sum_cord)
  sum_cord$SUM <- substr(sum_cord$NAICS, 1, 3)
  sum_cord <- distinct(sum_cord, SUM, .keep_all = TRUE)
  write.csv(sum_cord, file.path(find_rstudio_root_file(), "data", "sum_cord"), row.names = FALSE)
  saver(sum_cord)
  rm(sum_cord, x)
}

# Specific Sector level concordance
if (!file.exists(file.path(data_dir, "sec_cord"))){
  sec_cord <- c()
  sec_cord$SECTOR <- c("11", "21", "22", "23", "31G", "42", "44RT", "48TW", "51", "FIRE", "FIRE", "PROF", "PROF", "PROF", "6", "6", "7", "7", "81")
  sec_cord$NAICS <- c("11", "21", "22", "23", "31", "42", "44", "48", "51", "52", "53", "54", "55", "56", "61", "62", "71", "72", "81")
  sec_cord <- as.data.frame(sec_cord)
  write.csv(sec_cord, file.path(find_rstudio_root_file(), "data", "sec_cord"), row.names = FALSE)
  saver(sec_cord)
  rm(sec_cord)
}


# Generate full economic industry/county table consolidating NAICS to BEA codes at the detail level 
if (!file.exists(file.path(data_dir, "CBP_2019p_Concord_Detail_XBEA"))){
  CBP_2019p_Concord_Detail <- concordr("det_cord")
  reshaper(CBP_2019p_Concord_Detail, "det_cord")
  saver(CBP_2019p_Concord_Detail_XBEA)
  rm(CBP_2019p_Concord_Detail_XBEA, CBP_2019p_Concord_Detail)
}
log_info("Detail level CBP/BEA crosswalk complete")

# Generate full economic industry/county table consolidating NAICS to BEA codes at the Summary level 
if (!file.exists(file.path(data_dir, "CBP_2019p_Concord_Summary_XBEA"))){
  CBP_2019p_Concord_Summary <- concordr("sum_cord")
  reshaper(CBP_2019p_Concord_Summary, "sum_cord")
  saver(CBP_2019p_Concord_Summary_XBEA)
  rm(CBP_2019p_Concord_Summary_XBEA, CBP_2019p_Concord_Summary)
}
log_info("Summary level CBP/BEA crosswalk complete")

# Generate full economic industry/county table consolidating NAICS to BEA codes at the Sector level 
if (!file.exists(file.path(data_dir, "CBP_2019p_Concord_Sector_XBEA"))){
  CBP_2019p_Concord_Sector <- concordr("sec_cord")
  reshaper(CBP_2019p_Concord_Sector, "sec_cord")
  saver(CBP_2019p_Concord_Sector_XBEA)
  rm(CBP_2019p_Concord_Sector_XBEA, CBP_2019p_Concord_Sector)
}
log_info("Sector level CBP/BEA crosswalk complete")


# Import and cleanup "pubdata" TIGER data
if (!file.exists(file.path(data_dir, "TIGERDatap"))){
  TIGERDatap <- geography$get_county_df(year=2020L, geometry=TRUE, scale="500k")
  TIGERDatap %<>% rename(place = CODE)
  TIGERDatap$center <- st_centroid(TIGERDatap$geometry)
  TIGERDatap %<>% arrange(place)
  saver(TIGERDatap)
  rm(TIGERDatap) 
}
log_info("Import 'pubdata' TIGER complete")


# Produce  Distance  Matrix
if (!file.exists(file.path(data_dir, "Dist_mat"))){
  Dist_mat <- file.path(data_dir, "TIGERDatap") %>% readRDS() %>% .$center %>% as_Spatial() %>% distm()
  rownames(Dist_mat) = colnames(Dist_mat) <- file.path(data_dir, "TIGERDatap") %>% readRDS() %>%  .$place
saver(Dist_mat)
rm(Dist_mat) 
}
log_info("Distance Matrix complete")


## Produce Proximity  Matrix
if (!file.exists(file.path(data_dir, "Prox_mat"))){
  Prox_mat <-  file.path(data_dir, "TIGERDatap") %>% readRDS() %>% poly2nb(queen = TRUE) %>% nb2mat(style = "B", zero.policy = TRUE)
  colnames(Prox_mat) <- rownames(Prox_mat)
saver(Prox_mat)
rm(Prox_mat)
}
log_info("Proximity Matrix complete")



# Import and cleanup "pubdata" RUCC data
if (!file.exists(file.path(data_dir, "RUCCDatap"))){
  RUCCDatap <- ers_rurality$get_ruc_df()
  RUCCDatap %<>% filter(RUC_YEAR=="2013")
  RUCCDatap$place <- RUCCDatap$FIPS
  saver(RUCCDatap)
  rm(RUCCDatap) 
}
log_info("Import 'pubdata' RUCC complete")


# TIGER  and RUCC
if (!file.exists(file.path(data_dir, "TIGER_RUCC"))){
  TIGER_RUCC <- inner_join(readRDS(file.path(data_dir, "TIGERDatap")), readRDS(file.path(data_dir, "RUCCDatap")), by = "place")
  ### Four non overlapping counties from each ("02063" "02066" "02158" "46102") and ("02261" "02270" "46113" "51515")
  ##setdiff(TIGERDatap$place, RUCCDatap$place)
  ##setdiff(RUCCDatap$place, TIGERDatap$place)
  TIGER_RUCC <- TIGER_RUCC[order(TIGER_RUCC$place), ]
  rownames(TIGER_RUCC) <- TIGER_RUCC$place
  TIGER_RUCC <- transform(TIGER_RUCC, H3 = ifelse(RUC_CODE %in% 1:3, 1, ifelse(RUC_CODE %in% 4:6, 2, ifelse(RUC_CODE%in% 7:9, 3, 0)  ) ) )
saver(TIGER_RUCC)
rm(TIGER_RUCC)
}
log_info("TIGER/RUCC merge complete")



# Remove clutter
rm(data_dir) 

# Display end time
log_info("Define data clean end")




