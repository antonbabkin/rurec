# Import all downloaded outside data

# Load and attach necessary packages
library(rprojroot)
library(magrittr)
library(sf)
library(rlog)

# Display start time
log_info("Define data import start")

# Connect and parse code from another file 
source(file.path(find_rstudio_root_file(), "nbs", "rural_typology_r_data_sources.R"))

# Create r object only data folder if not already done
data_dir = file.path(find_rstudio_root_file(), "data", "robjs")
if (!file.exists(data_dir)) {
  dir.create(data_dir)
}


# Import IO tables 
if (!file.exists(file.path(data_dir, "IO_tables"))){
  excel_importr(
    TableName = "IO_tables",
    FileDir = file.path(find_rstudio_root_file(), "data", "AllTablesSUP") 
  )
}
saver(IO_tables)
log_info("IO tables import complete")


# Import CBP data 2019
if (!file.exists(file.path(data_dir, "CBP_2019"))){
  if (!exists("CBP_2019")){
    CBP_2019 <- file.path(find_rstudio_root_file(), "data", "cbp19co.txt") %>% read.csv(header = TRUE)
    
    CBP_2019$fipstate %<>% formatC(width = 2, format = "d", flag = "0")
    CBP_2019$fipscty  %<>% formatC(width = 3, format = "d", flag = "0")
    CBP_2019$place <- paste0(CBP_2019$fipstate, CBP_2019$fipscty)
  }
}
saver(CBP_2019)
log_info("CBP 2019 import complete")

# Import CBP data 2012 
if (!file.exists(file.path(data_dir, "CBP_2012"))){
  if (!exists("CBP_2012")){
    CBP_2012 <- file.path(find_rstudio_root_file(), "data", "cbp12co.txt") %>% read.csv(header = TRUE)
    
    CBP_2012$fipstate %<>% formatC(width = 2, format = "d", flag = "0")
    CBP_2012$fipscty  %<>% formatC(width = 3, format = "d", flag = "0")
    CBP_2012$place <- paste0(CBP_2012$fipstate, CBP_2012$fipscty)
  }
}
saver(CBP_2012)
log_info("CBP 2012 import complete")

#Import 2020 BLS Quarterly Census of Employment and Wages (QCEW) 
if (!file.exists(file.path(data_dir, "QCEW_2020"))){
  if (!exists("QCEW_2020")){
    QCEW_2020 <- file.path(find_rstudio_root_file(), "data", "2020.annual.singlefile.csv") %>% read.csv(header = TRUE)
  }
}
saver(QCEW_2020)
log_info("QCWEW 2020 import complete")

# Import TIGER data 
if (!file.exists(file.path(data_dir, "TIGERData"))){
  if (!exists("TIGERData")){
    TIGERData <- file.path(find_rstudio_root_file(), "data", "cb_2021_us_county_500k.shp") %>% st_read(stringsAsFactors = FALSE)
    TIGERData$place <- paste0(TIGERData$STATEFP, TIGERData$COUNTYFP)
    TIGERData$center <- st_centroid(TIGERData$geometry)
    TIGERData %<>% arrange(place)
  }
}
saver(TIGERData)
log_info("TIGER import complete")

# Import RUCA tables 
if (!file.exists(file.path(data_dir, "RUCAData"))){
  if (!exists("RUCAData")){
    RUCAData <- file.path(find_rstudio_root_file(), "data", "ruca2010revised.xlsx") %>% read.xlsx(startRow = 2)
  }
}
saver(RUCAData)
log_info("RUCA import complete")

# Import RUCC tables 
if (!file.exists(file.path(data_dir, "RUCCData"))){
  if (!exists("RUCCData")){
    RUCCData <- file.path(find_rstudio_root_file(), "data", "ruralurbancodes2013.xls") %>% read_xls()
  }
RUCCData$place <- RUCCData$FIPS
}
####  Note:  2 counties (02158, 46102) are not common to TIGER_CBP in RUCCData
saver(RUCCData)
log_info("RUCC import complete")

# Import UIC tables 
if (!file.exists(file.path(data_dir, "UICData"))){
  if (!exists("UICData")){
    UICData <- file.path(find_rstudio_root_file(), "data", "UrbanInfluenceCodes2013.xls") %>% read_xls()
  }
UICData$place <- UICData$FIPS
}
saver(UICData)
log_info("UIC import complete")

# Import CBSA crosswalk 
if (!file.exists(file.path(data_dir, "CBSAcross"))){
  if (!exists("CBSAcross")){
    CBSAcross <- file.path(find_rstudio_root_file(), "data", "qcew-county-msa-csa-crosswalk-xlsx.xlsx") %>% read_xlsx()
  }
  CBSAcross$place <- CBSAcross$"County Code"
}
saver(CBSAcross)
log_info("CBSAcross import complete")


# Remove clutter
rm(CBP_2012, CBP_2019, IO_tables, QCEW_2020, TIGERData, RUCAData, RUCCData, UICData, CBSAcross, data_dir) %>% suppressWarnings()

# Display end time
log_info("Define data import end")





