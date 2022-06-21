# Import all downloaded outside data

# Load and attach necessary packages
library(rprojroot)
library(magrittr)
library(sf)

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

#Import 2020 BLS Quarterly Census of Employment and Wages (QCEW) 
if (!file.exists(file.path(data_dir, "QCEW_2020"))){
  if (!exists("QCEW_2020")){
    QCEW_2020 <- file.path(find_rstudio_root_file(), "data", "2020.annual.singlefile.csv") %>% read.csv(header = TRUE)
  }
}
saver(QCEW_2020)

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

# Import RUCA tables 
if (!file.exists(file.path(data_dir, "RUCAData"))){
  if (!exists("RUCAData")){
    RUCAData <- file.path(find_rstudio_root_file(), "data", "ruca2010revised.xlsx") %>% read.xlsx(startRow = 2)
  }
}
saver(RUCAData)

# Import RUCC tables 
if (!file.exists(file.path(data_dir, "RUCCData"))){
  if (!exists("RUCCData")){
    RUCCData <- file.path(find_rstudio_root_file(), "data", "ruralurbancodes2013.xls") %>% read_xls()
  }
RUCCData$place <- RUCCData$FIPS
}
####  Note:  2 counties (02158, 46102) are not common to TIGER_CBP in RUCCData
saver(RUCCData)

# Import UIC tables 
if (!file.exists(file.path(data_dir, "UICData"))){
  if (!exists("UICData")){
    UICData <- file.path(find_rstudio_root_file(), "data", "UrbanInfluenceCodes2013.xls") %>% read_xls()
  }
UICData$place <- UICData$FIPS
}
saver(UICData)

# Remove clutter
rm(CBP_2012, CBP_2019, IO_tables, QCEW_2020, TIGERData, RUCAData, RUCCData, UICData, data_dir)







