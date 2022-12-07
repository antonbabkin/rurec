# Clean imported outside data and generate new data products

# Load and attach necessary packages
library(rprojroot)

# Connect and parse code from "functions" file 
source(file.path(find_rstudio_root_file(), "nbs", "rural_typology_r_functions.R"))

# Display start time
log_info("Define data start")

# Turn Off Scientific Notation 
options(scipen = 999)

# Define data directory path
data_dir = file.path(find_rstudio_root_file(), "data", "robjs")


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
  #write.csv(det_cord, file.path(find_rstudio_root_file(), "data", "det_cord"), row.names = FALSE)
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
  #write.csv(sum_cord, file.path(find_rstudio_root_file(), "data", "sum_cord"), row.names = FALSE)
  saver(sum_cord)
  rm(sum_cord, x)
}

# Specific Sector level concordance
if (!file.exists(file.path(data_dir, "sec_cord"))){
  sec_cord <- c()
  sec_cord$SECTOR <- c("11", "21", "22", "23", "31G", "42", "44RT", "48TW", "51", "FIRE", "FIRE", "PROF", "PROF", "PROF", "6", "6", "7", "7", "81")
  sec_cord$NAICS <- c("11", "21", "22", "23", "31", "42", "44", "48", "51", "52", "53", "54", "55", "56", "61", "62", "71", "72", "81")
  sec_cord <- as.data.frame(sec_cord)
  #write.csv(sec_cord, file.path(find_rstudio_root_file(), "data", "sec_cord"), row.names = FALSE)
  saver(sec_cord)
  rm(sec_cord)
}

# # Added option but not needed elsewhere
# # Generate full economic industry/county list consolidating NAICS to BEA codes at the all levels 
# if (!file.exists(file.path(data_dir, "CBP_Concord_XBEA"))){
#   CBP_Concord_XBEA <- list()
#   CBP_Concord_XBEA[["Sector"]] <- place_industry_economy_long("2020", "sec_cord")
#   CBP_Concord_XBEA[["Summary"]] <- place_industry_economy_long("2020", "sum_cord")
#   CBP_Concord_XBEA[["Detail"]] <- place_industry_economy_long("2012", "det_cord")
#   saver(CBP_Concord_XBEA)
#   rm(CBP_Concord_XBEA)
# }
# log_info("Sector level CBP/BEA crosswalk complete")

# # Added option but not needed elsewhere
# ### Total Requirements matrix
# if (file.exists(file.path(data_dir, "Total_mat"))) {
#   Total_mat <- list()
#   Total_mat[["Sector"]] <- total_requirements("2020", "sec")
#   Total_mat[["Summary"]] <- total_requirements("2020", "sum")
#   Total_mat[["Detail"]] <- total_requirements("2012", "det")
#   saver(Total_mat)
#   rm(Total_mat)
#   }
# log_info("Total requirements matrix complete")


# # Added option but not needed elsewhere
# ### Annual payroll matrix
# if (file.exists(file.path(data_dir, "Xpay_mat"))) {
#   Xpay_mat <- list()
#   Xpay_mat[["Sector"]] <- industry_output_by_place("2020",  ilevel_concord = "sec_cord")
#   Xpay_mat[["Summary"]] <- industry_output_by_place("2020",  ilevel_concord = "sum_cord")
#   Xpay_mat[["Detail"]] <- industry_output_by_place("2012",  ilevel_concord = "det_cord")
#   saver(Xpay_mat)
#   rm(Xpay_mat)
#   }
# log_info("Total requirements matrix complete")



### Test example of censoring/noise-infusion data loss
# sum(place_industry_economy_long("2012")$ap) / sum(filter(cbp("2012"), NAICS == '-')$ap)
# sum(place_industry_economy_long("2012")$emp) / sum(filter(cbp("2012"), NAICS == '-')$emp)



### Download and clean farmssales data 
  ### Note: only temporary until pubdata.agcensus.md complete 
  ###download file from agcensus_sales_by_bea_v220909.csv google sharedrive rurec/Data (https://drive.google.com/file/d/1gDufjSB3vXaloBtlYOjfo8IuSTgebpT3/view?usp=share_link)
if (file.exists(file.path(data_dir, "farm_sales"))) {
  log_info("farm_sales table already exists")
} else {
    download.file(url="https://drive.google.com/uc?export=download&id=1gDufjSB3vXaloBtlYOjfo8IuSTgebpT3", 
                  destfile=file.path(find_rstudio_root_file(), "data", "agcensus_sales_by_bea_v220909.csv"))
    farm_sales_file <- file.path(find_rstudio_root_file(), "data", "agcensus_sales_by_bea_v220909.csv")
    
    df <- read.csv(farm_sales_file, colClasses = "character")
    df$SALES <- df$SALES %>% as.numeric / 1000
    df <- df %>% filter(AGG_LEVEL_DESC == "COUNTY") %>% select(!AGG_LEVEL_DESC)
    df <- reshape(df, direction = "wide", idvar = "STCTY", timevar = "BEA_INDUSTRY_DETAIL")
    colnames(df) <- gsub("SALES.", "", colnames(df))
    farm_sales <- df
   saver(farm_sales)
}
log_info("farms sales download complete")



#### Total Output Matrix in thousands of dollars
if (!file.exists(file.path(data_dir, "Output_mat"))){
  Output_mat <- list()
  Output_mat[["Sector"]] <- total_output("2020", "sec")
  Output_mat[["Summary"]] <- total_output("2020", "sum")
  Output_mat[["Detail"]] <- total_output("2012", "det", labor_share_year = "2012")
  saver(Output_mat)
  rm(Output_mat)
}
log_info("Total Output Matrix complete")

#### Direct requirements matrices (Technical Coefficients)
if (!file.exists(file.path(data_dir, "Direct_mat"))){
  Direct_mat <- list()
  Direct_mat[["Sector"]] <- direct_requirements("2020", "sec")
  Direct_mat[["Summary"]] <- direct_requirements("2020", "sum")
  Direct_mat[["Detail"]] <- direct_requirements("2012", "det")
  saver(Direct_mat)
  rm(Direct_mat)
}
log_info("Direct Requirements Matrix complete")

# TIGER  and RUCC
if (!file.exists(file.path(data_dir, "TIGER_RUCC"))){
  TIGER_RUCC <- tiger_rucc("2020")
  #TIGER_RUCC <- transform(TIGER_RUCC, H3 = ifelse(RUC_CODE %in% 1:3, 1, ifelse(RUC_CODE %in% 4:6, 2, ifelse(RUC_CODE%in% 7:9, 3, 0)  ) ) )
  saver(TIGER_RUCC)
  rm(TIGER_RUCC)
}
log_info("TIGER/RUCC merge complete")



### Test of missing CBP coverage in Census coverage
# setdiff(unique(tiger("2020")$place), unique(place_industry_economy_long("2012", "det_cord")$place) )


### Test of nonoverlapping TIGER and RUCC coverage
#setdiff(tiger("2020")$place, rucc()$place)
#setdiff(rucc()$place, tiger("2020")$place)
## Note: Four non overlapping counties from each ("02063" "02066" "02158" "46102") and ("02261" "02270" "46113" "51515") for 2020



# Remove clutter
rm(data_dir, l) %>% suppressWarnings()


# Display end time
log_info("Define data end")










