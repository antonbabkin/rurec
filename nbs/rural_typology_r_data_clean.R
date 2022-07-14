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


# # Import, cleanup, and save BEA/NAICS concordance table
# if (!file.exists(file.path(data_dir, "Concord"))){
#   Concord <- bea_io$get_naics_df()
#   Concord$SUMMARY <- unlist(Concord$SUMMARY)
#   Concord$U_SUMMARY <- unlist(Concord$U_SUMMARY)
#   Concord$DETAIL <- unlist(Concord$DETAIL)
#   Concord$NAICS <- unlist(Concord$NAICS)
#   Concord %<>% filter(NAICS != "NaN") 
#   Concord %<>% filter(NAICS != "n.a.")
#   Concord$NAICS %<>% as.character()
#   Concord$SECTOR <- ifelse(Concord$SECTOR == "33DG" | Concord$SECTOR == "31ND" , "31G",  Concord$SECTOR)
#   Concord$SECTOR <- ifelse(Concord$SECTOR == "52" | Concord$SECTOR == "53" , "FIRE",  Concord$SECTOR)
#   Concord$SECTOR <- ifelse(Concord$SECTOR == "54" | Concord$SECTOR == "55" | Concord$SECTOR == "56" , "PROF",  Concord$SECTOR)
#   Concord$SECTOR <- ifelse(Concord$SECTOR == "61" | Concord$SECTOR == "62" , "6",  Concord$SECTOR)
#   Concord$SECTOR <- ifelse(Concord$SECTOR == "71" | Concord$SECTOR == "72" , "7",  Concord$SECTOR) 
#   Concord$NAICS <- ifelse(Concord$NAICS == "531", "531*",  Concord$NAICS)
#   Concord %<>% add_row( SECTOR = "23", SUMMARY = "23", U_SUMMARY = "23", DETAIL = "23", NAICS = "23", .after = 	39)
#   Concord %<>% add_row( SECTOR = "FIRE", SUMMARY = "531", U_SUMMARY = "531", DETAIL = "531", NAICS = "531", .after = 	390)
#   Concord %<>% filter(NAICS != "23*")
#   Concord %<>% filter(NAICS != "531*")
#   saver(Concord)
# }
# 
# CBP_2019p_Concord <- left_join(readRDS(file.path(data_dir, "CBP_2019p")), readRDS(file.path(data_dir, "Concord")), by = "NAICS")
# rm(CBP_2019p, Concord) %>% suppressWarnings()



# Specific Detail level concordance
if (!file.exists(file.path(data_dir, "det_cord"))){
  x <- bea_io$get_naics_df() %>% filter(NAICS != "n.a.") %>% filter(NAICS != "NaN") 
  det_cord <- c()
  det_cord$DETAIL <- unlist(x$DETAIL, use.names = FALSE) 
  det_cord$NAICS <- unlist(x$NAICS, use.names = FALSE) 
  det_cord <- as.data.frame(det_cord)
  det_cord$NAICS <- ifelse(det_cord$NAICS == "531" & det_cord$DETAIL == "531HST", "531*",  det_cord$NAICS)
  det_cord %<>% add_row(DETAIL = "23", NAICS = "23", .after = 39)
  #det_cord %<>% add_row(DETAIL = "531XXX", NAICS = "531", .after = 390)
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
  #sum_cord %<>% add_row(SUMMARY = "531XXX", NAICS = "531", .after = 390)
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
  CBP_2019p_Concord_Detail <- left_join(readRDS(file.path(data_dir, "CBP_2019p")), readRDS(file.path(data_dir, "det_cord")), by = "NAICS") %>% filter(DETAIL != "NULL") %>% group_by(place, DETAIL) %>%  summarise(across(where(is.numeric), sum), .groups = 'drop')  %>% as.data.frame() %>% subset(select = c(place, DETAIL, emp, qp1, ap, est))
  reshaper(CBP_2019p_Concord_Detail)
  saver(CBP_2019p_Concord_Detail_XBEA)
  rm(CBP_2019p_Concord_Detail_XBEA, CBP_2019p_Concord_Detail)
}
log_info("Detail level CBP/BEA crosswalk complete")

# Generate full economic industry/county table consolidating NAICS to BEA codes at the Summary level 
if (!file.exists(file.path(data_dir, "CBP_2019p_Concord_Summary_XBEA"))){
  CBP_2019p_Concord_Summary <- left_join(readRDS(file.path(data_dir, "CBP_2019p")), readRDS(file.path(data_dir, "sum_cord")), by = "NAICS") %>% filter(SUMMARY != "NULL") %>% group_by(place, SUMMARY) %>%  summarise(across(where(is.numeric), sum), .groups = 'drop') %>% as.data.frame() %>% subset(select = c(place, SUMMARY, emp, qp1, ap, est))
  reshaper(CBP_2019p_Concord_Summary)
  saver(CBP_2019p_Concord_Summary_XBEA)
  rm(CBP_2019p_Concord_Summary_XBEA, CBP_2019p_Concord_Summary)
}
log_info("Summary level CBP/BEA crosswalk complete")

# Generate full economic industry/county table consolidating NAICS to BEA codes at the Sector level 
if (!file.exists(file.path(data_dir, "CBP_2019p_Concord_Sector_XBEA"))){
  CBP_2019p_Concord_Sector <- left_join(readRDS(file.path(data_dir, "CBP_2019p")), readRDS(file.path(data_dir, "sec_cord")), by = "NAICS") %>% filter(SECTOR != "NULL") %>% group_by(place, SECTOR) %>%  summarise(across(where(is.numeric), sum), .groups = 'drop') %>% as.data.frame() %>% subset(select = c(place, SECTOR, emp, qp1, ap, est))
  reshaper(CBP_2019p_Concord_Sector)
  saver(CBP_2019p_Concord_Sector_XBEA)
  rm(CBP_2019p_Concord_Sector_XBEA, CBP_2019p_Concord_Sector)
}
log_info("Sector level CBP/BEA crosswalk complete")



# Produce  Distance  Matrix
if (!file.exists(file.path(data_dir, "Dist_mat"))){
  Dist_mat <- file.path(data_dir, "TIGERData") %>% readRDS() %>% .$center %>% as_Spatial() %>% distm()
  rownames(Dist_mat) = colnames(Dist_mat) <- file.path(data_dir, "TIGERData") %>% readRDS() %>%  .$place
saver(Dist_mat)
}
log_info("Distance Matrix complete")

# Produce  Decay/Impedance  Matrix
if (!file.exists(file.path(data_dir, "QI_mat"))){
  #QI_mat <- (1/(Dist_mat/1)^2)
  #QI_mat <- exp(-(Dist_mat/10000))
  ### hyperbolic secant function
  QI_mat <- 2/(exp(-(Dist_mat/1000000)) + exp(Dist_mat/1000000))
saver(QI_mat)
}
rm(Dist_mat, QI_mat) %>% suppressWarnings()
log_info("Impedance Matrix complete")

## Produce Proximity  Matrix
if (!file.exists(file.path(data_dir, "Prox_mat"))){
  Prox_mat <-  file.path(data_dir, "TIGERData") %>% readRDS() %>% poly2nb(queen = TRUE) %>% nb2mat(style = "B", zero.policy = TRUE)
  colnames(Prox_mat) <- rownames(Prox_mat)
saver(Prox_mat)
rm(Prox_mat)
}
log_info("Proximity Matrix complete")

# TIGER  and RUCC
if (!file.exists(file.path(data_dir, "TIGER_RUCC"))){
  TIGER_RUCC <- inner_join(readRDS(file.path(data_dir, "TIGERData")), readRDS(file.path(data_dir, "RUCCData")), by = "place")
  ### Four non overlapping counties from each ("02063" "02066" "02158" "46102") and ("02261" "02270" "46113" "51515")
  TIGER_RUCC <- TIGER_RUCC[order(TIGER_RUCC$place), ]
  rownames(TIGER_RUCC) <- TIGER_RUCC$place
  TIGER_RUCC <- transform(TIGER_RUCC, H3 = ifelse(RUCC_2013 %in% 1:3, 1, ifelse(RUCC_2013 %in% 4:6, 2, ifelse(RUCC_2013%in% 7:9, 3, 0)  ) ) )
saver(TIGER_RUCC)
rm(TIGER_RUCC)
}
log_info("TIGER/RUCC merge complete")



# Remove clutter
rm(data_dir) 

# Display end time
log_info("Define data clean end")




