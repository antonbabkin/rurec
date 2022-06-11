

# Establish working directory relative to location of this file
script_path() %>% setwd() 

# Connect and parse code from another file 
source("Data_sources.R")


# Import IO tables 
excel_importr(
  TableName = "IO_tables",
  FileDir = path(getwd(), "data", "AllTablesSUP") 
)


# Import CBP data 
if (!exists("CBP_2019")){
  CBP_2019 <- path(getwd(), "data", "cbp19co.txt") %>% read.csv(header = TRUE)
  
  CBP_2019$fipstate %<>% formatC(width = 2, format = "d", flag = "0")
  CBP_2019$fipscty  %<>% formatC(width = 3, format = "d", flag = "0")
  CBP_2019$place <- paste0(CBP_2019$fipstate, CBP_2019$fipscty)
}


# Import CBP data 2012 
if (!exists("CBP_2012")){
  CBP_2012 <- path(getwd(), "data", "cbp12co.txt") %>% read.csv(header = TRUE)
  
  CBP_2012$fipstate %<>% formatC(width = 2, format = "d", flag = "0")
  CBP_2012$fipscty  %<>% formatC(width = 3, format = "d", flag = "0")
  CBP_2012$place <- paste0(CBP_2012$fipstate, CBP_2012$fipscty)
}


#Import 2020 BLS Quarterly Census of Employment and Wages (QCEW) 
if (!exists("QCEW_2020")){
  QCEW_2020 <- path(getwd(), "data", "2020.annual.singlefile.csv") %>% read.csv(header = TRUE)
}

# Import TIGER data 
if (!exists("TIGERData")){
  TIGERData <- path(getwd(), "data", "cb_2021_us_county_500k.shp") %>% st_read(stringsAsFactors = FALSE)
  TIGERData$place <- paste0(TIGERData$STATEFP, TIGERData$COUNTYFP)
  TIGERData$center <- st_centroid(TIGERData$geometry)
  TIGERData %<>% arrange(place)
}


# Import RUCA tables 
if (!exists("RUCAData")){
  RUCAData <- path(getwd(), "data", "ruca2010revised.xlsx") %>% read.xlsx(startRow = 2)
}

# Import RUCC tables 
if (!exists("RUCCData")){
  RUCCData <- path(getwd(), "data", "ruralurbancodes2013.xls") %>% read_xls()
}
RUCCData$place <- RUCCData$FIPS
####  Note:  2 counties (02158, 46102) are not common to TIGER_CBP in RUCCData


# Import UIC tables 
if (!exists("UICData")){
  UICData <- path(getwd(), "data", "UrbanInfluenceCodes2013.xls") %>% read_xls()
}
UICData$place <- UICData$FIPS










