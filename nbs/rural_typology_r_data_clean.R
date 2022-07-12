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
}
log_info("import 'pubdata' CBP complete")

# Import, cleanup, and save BEA/NAICS concordance table
if (!file.exists(file.path(data_dir, "Concord"))){
  Concord <- bea_io$get_naics_df()
  Concord$SUMMARY <- unlist(Concord$SUMMARY)
  Concord$U_SUMMARY <- unlist(Concord$U_SUMMARY)
  Concord$DETAIL <- unlist(Concord$DETAIL)
  Concord$NAICS <- unlist(Concord$NAICS)
  Concord %<>% filter(NAICS != "NaN") 
  Concord %<>% filter(NAICS != "n.a.")
  Concord$NAICS %<>% as.character()
  Concord$SECTOR <- ifelse(Concord$SECTOR == "33DG" | Concord$SECTOR == "31ND" , "31G",  Concord$SECTOR)
  Concord$SECTOR <- ifelse(Concord$SECTOR == "52" | Concord$SECTOR == "53" , "FIRE",  Concord$SECTOR)
  Concord$SECTOR <- ifelse(Concord$SECTOR == "54" | Concord$SECTOR == "55" | Concord$SECTOR == "56" , "PROF",  Concord$SECTOR)
  Concord$SECTOR <- ifelse(Concord$SECTOR == "61" | Concord$SECTOR == "62" , "6",  Concord$SECTOR)
  Concord$SECTOR <- ifelse(Concord$SECTOR == "71" | Concord$SECTOR == "72" , "7",  Concord$SECTOR) 
  Concord$NAICS <- ifelse(Concord$NAICS == "531", "531*",  Concord$NAICS)
  Concord %<>% add_row( SECTOR = "23", SUMMARY = "23", U_SUMMARY = "23", DETAIL = "23", NAICS = "23", .after = 	39)
  Concord %<>% add_row( SECTOR = "FIRE", SUMMARY = "531", U_SUMMARY = "531", DETAIL = "531", NAICS = "531", .after = 	390)
  Concord %<>% filter(NAICS != "23*")
  Concord %<>% filter(NAICS != "531*")
  saver(Concord)
}
log_info("import 'pubdata' concordance complete")



CBP_2019p_Concord <- left_join(readRDS(file.path(data_dir, "CBP_2019p")), readRDS(file.path(data_dir, "Concord")), by = "NAICS")
rm(CBP_2019p, Concord) %>% suppressWarnings()

# Generate full economic industry/county table consolidating NAICS to BEA codes at the detail level 
if (!file.exists(file.path(data_dir, "CBP_2019p_Concord_Detail_XBEA"))){
  CBP_2019p_Concord_Detail <- filter(CBP_2019p_Concord, DETAIL != "NULL")  %>% subset(select = c(place, DETAIL, emp, qp1, ap, est))
  reshaper(CBP_2019p_Concord_Detail)
  saver(CBP_2019p_Concord_Detail_XBEA)
  rm(CBP_2019p_Concord_Detail_XBEA)
}
log_info("Detail level CBP/BEA crosswalk complete")

# Generate full economic industry/county table consolidating NAICS to BEA codes at the Summary level 
if (!file.exists(file.path(data_dir, "CBP_2019p_Concord_Summary_XBEA"))){
  CBP_2019p_Concord_Summary <- filter(CBP_2019p_Concord, DETAIL != "NULL") %>% group_by(place, SUMMARY) %>%  summarise(across(where(is.numeric), sum), .groups = 'drop') %>% as.data.frame()
  reshaper(CBP_2019p_Concord_Summary)
  saver(CBP_2019p_Concord_Summary_XBEA)
  rm(CBP_2019p_Concord_Summary_XBEA)
}
log_info("Summary level CBP/BEA crosswalk complete")

# Generate full economic industry/county table consolidating NAICS to BEA codes at the Sector level 
if (!file.exists(file.path(data_dir, "CBP_2019p_Concord_Sector_XBEA"))){
  CBP_2019p_Concord_Sector  <- filter(CBP_2019p_Concord, DETAIL != "NULL") %>% group_by(place, SECTOR) %>%  summarise(across(where(is.numeric), sum), .groups = 'drop') %>% as.data.frame()
  reshaper(CBP_2019p_Concord_Sector)
  saver(CBP_2019p_Concord_Sector_XBEA)
  rm(CBP_2019p_Concord_Sector_XBEA)
}

rm(CBP_2019p_Concord, CBP_2019p_Concord_Detail, CBP_2019p_Concord_Summary, CBP_2019p_Concord_Sector) %>% suppressWarnings()
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



#Delete
# # Parse  TIGER  and  CBP  2019  county  overlap
# if (!file.exists(file.path(data_dir, "TIGER_CBP"))){
#   TIGER_CBP <- inner_join(readRDS(file.path(data_dir, "TIGERData")), readRDS(file.path(data_dir, "CBP_2019_places_Sector")), by = "place")
#   TIGER_CBP  <- TIGER_CBP[order(TIGER_CBP$place), ]
#   rownames(TIGER_CBP) <- TIGER_CBP$place
# saver(TIGER_CBP)
# rm(TIGER_CBP)
# }
# log_info("TIGER/CBP 2019 merge complete")
# 
# 
# #Parse  TIGER/CBP  and  RUCC  crosswalk
# if (!file.exists(file.path(data_dir, "TIGER_CBP_RUCC"))){
#   TIGER_CBP_RUCC <- inner_join(readRDS(file.path(data_dir, "TIGER_CBP")), readRDS(file.path(data_dir, "RUCCData")), by = "place")
#   rownames(TIGER_CBP_RUCC) <- TIGER_CBP_RUCC$place
#   # add augmented hierarchical classification 
#   TIGER_CBP_RUCC <- transform(TIGER_CBP_RUCC, H3 = ifelse(RUCC_2013 %in% 1:3, 1, ifelse(RUCC_2013 %in% 4:6, 2, ifelse(RUCC_2013%in% 7:9, 3, 0)  ) ) ) 
# saver(TIGER_CBP_RUCC)
# rm(TIGER_CBP_RUCC)
# }
# log_info("TIGER/CBP/RUCC merge complete")
# 
# 
# # Parse  TIGER  and  QCED  crosswalk
# if (!file.exists(file.path(data_dir, "QCEW_2020_places_Summary"))){
#   QCEW_2020_places_Summary <- file.path(data_dir, "QCEW_2020_Sum_XBEA") %>% readRDS() %>% .$place %>% unique() %>% as.data.frame()
#   names(QCEW_2020_places_Summary) <- "place"
# saver(QCEW_2020_places_Summary)
# rm(QCEW_2020_places_Summary)
# }
# if (!file.exists(file.path(data_dir, "TIGER_QCEW"))){
#   TIGER_QCEW <- inner_join(readRDS(file.path(data_dir, "TIGERData")), readRDS(file.path(data_dir, "QCEW_2020_places_Summary")), by = "place")
#   TIGER_QCEW  <- TIGER_QCEW[order(TIGER_QCEW$place), ]
#   rownames(TIGER_QCEW) <- TIGER_QCEW$place
# saver(TIGER_QCEW)
# rm(TIGER_QCEW)
# }
# log_info("TIGER/QCED merge complete")
# 
# #Parse  TIGER/QCEW  and  RUCC  crosswalk
# if (!file.exists(file.path(data_dir, "TIGER_QCEW_RUCC"))){
#   TIGER_QCEW_RUCC <- inner_join(readRDS(file.path(data_dir, "TIGER_QCEW")),  readRDS(file.path(data_dir, "RUCCData")), by = "place")
#   rownames(TIGER_QCEW_RUCC) <- TIGER_QCEW_RUCC$place
#   # add  augmented  hierarchical  classification 
#   TIGER_QCEW_RUCC <- transform(TIGER_QCEW_RUCC, H3 = ifelse(RUCC_2013 %in% 1:3, 1, ifelse(RUCC_2013 %in% 4:6, 2, ifelse(RUCC_2013%in% 7:9, 3, 0)  ) ) )
# saver(TIGER_QCEW_RUCC)
# rm(TIGER_QCEW_RUCC)
# }
# log_info("TIGER/QCEW/RUCC merge complete")




# Remove clutter
rm(data_dir) 

# Display end time
log_info("Define data clean end")




