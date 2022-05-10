
# List packages needed for this exercise
packages <- c("dlm",
              "fs",
              "geosphere",
              "gtools",
              "knitr",
              "magrittr",
              "Matrix",
              "openxlsx",
              "RColorBrewer",
              "readxl",
              "reticulate",
              "sf",
              "spdep",
              "tidyverse",
              "tmap",
              "tmaptools",
              "tools")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages], dependencies = TRUE)
}

# Load packages as necessary
invisible(lapply(packages, library, character.only = TRUE))

# Establish working directory relative to location of this file
script_path() %>% setwd() 

# Push working directory up a level to establish parallel data files
setwd("..")

# Create "data" a file if not already done
if (!file.exists("data")) {
  "data" %>% dir.create()
}

# Function to download  data
data_getr <- function(FileURL,
                      DestDir){
  local({
    destfile <- FileURL %>% basename() %>% path(DestDir, .)
    if (!file.exists(destfile)) {
      download.file(url=FileURL, destfile=destfile, quiet=TRUE, overwrite = TRUE)
    }
  }) 
}


# Function to download and unzip data
data_zipr <- function(ZipURL,
                      DestDir,
                      FileExt = ""){
  local({
    DestFile <- ZipURL %>% basename() %>% file_path_sans_ext() %>% path(DestDir, ., ext = FileExt)
    temp <- tempfile()
    if (!file.exists(DestFile)) {
      download.file(url=ZipURL, destfile=temp, quiet=TRUE)
      if (file.info(temp)$size > 0){
        unzip(zipfile = temp, exdir = DestDir, overwrite = FALSE)
      }
    }
  })
}


# Download and unzip BEA IO data.
data_zipr(
  ZipURL = "https://apps.bea.gov/industry/iTables%20Static%20Files/AllTablesSUP.zip",
  DestDir = getwd() %>% path("data")
)

# Download and unzip Census CBP data.
data_zipr(
  ZipURL = "https://www2.census.gov/programs-surveys/cbp/datasets/2019/cbp19co.zip",
  DestDir =  getwd() %>% path("data"),
  FileExt = "txt"
)

# Download and unzip Census county TIGER  data.
data_zipr(
  ZipURL = "https://www2.census.gov/geo/tiger/TIGER2021/COUNTY/tl_2021_us_county.zip",
  DestDir = getwd() %>% path("data")
)
data_zipr(
  ZipURL = "https://www2.census.gov/geo/tiger/GENZ2021/shp/cb_2021_us_county_500k.zip",
  DestDir = getwd() %>% path("data")
)

# Download RUCA codes.
data_getr(
  FileURL = "https://www.ers.usda.gov/webdocs/DataFiles/53241/ruca2010revised.xlsx",
  DestDir = getwd() %>% path("data")
)

# Download RUCC codes.
data_getr(
  FileURL = "https://www.ers.usda.gov/webdocs/DataFiles/53251/ruralurbancodes2013.xls",
  DestDir = getwd() %>% path("data")
)

# Download Urban Influence codes.
data_getr(
  FileURL = "https://www.ers.usda.gov/webdocs/DataFiles/53797/UrbanInfluenceCodes2013.xls",
  DestDir = getwd() %>% path("data")
)



# Function to import a file of excel data tables
excel_importr <- function(TableName,
                          FileDir,
                          RegExType = "*.xlsx"){
  if (!exists(TableName)){
    local({ 
      temp <- FileDir %>% list.files(pattern = RegExType, full.names = TRUE)
      IO_tables <<- vector("list", length(temp))
      for (i in 1:length(temp)){
        DataSheets <- temp[i] %>% excel_sheets()
        SheetList <- lapply(DataSheets, read.xlsx, xlsxFile=temp[i])
        names(SheetList) <- DataSheets
        IO_tables[[i]] <<- SheetList
      }
      names(IO_tables) <<- temp %>% basename() %>% file_path_sans_ext()
    })
  }
}


# Function(s) to clean non-finite values in lists of matrix 
finiter <- function(x){
  if (!is.finite(x)){
    x <- 0
  }
  else {
    x=x
  }
} 

finiterer <- function(x){ 
  lapply(1:length(x), function(i) apply(x[[i]], c(1,2), finiter))
}


# Function(s) to truncate values in lists of matrix  
oner <- function(x, t=1, l=1){
  if (x > l){
    x <- t
  }
  else {
    x=x
  }
}
onerer <- function(x){ 
  lapply(1:length(x), function(i) apply(x[[i]], c(1,2), oner))
}



# Import IO tables into R 
excel_importr(
  TableName = "IO_tables",
  FileDir = path(getwd(), "data", "AllTablesSUP") 
)


# Import CBP data into R 
if (!exists("RegionalData")){
  RegionalData <- path(getwd(), "data", "cbp19co.txt") %>% read.csv(header = TRUE)
  
  RegionalData$fipstate %<>% formatC(width = 2, format = "d", flag = "0")
  RegionalData$fipscty  %<>% formatC(width = 3, format = "d", flag = "0")
}

# Import TIGER data into R 
if (!exists("TIGERData")){
  TIGERData <- path(getwd(), "data", "cb_2021_us_county_500k.shp") %>% st_read(stringsAsFactors = FALSE)
  TIGERData$place <- paste0(TIGERData$STATEFP, TIGERData$COUNTYFP)
  TIGERData$center <- st_centroid(TIGERData$geometry)
}



# Process and parse hierarchical structure  of CBP data
RegionalData$place <- paste0(RegionalData$fipstate, RegionalData$fipscty)

RegionalData_C <- filter(RegionalData, naics == "------")
Regions <- RegionalData_C[, 1:2]
Regions$place <- paste0(Regions$fipstate, Regions$fipscty)
RegionalData_Sector <- RegionalData %>% filter(grepl('*----', naics) & naics != '------' )
####  Note:  Six counties (30069, 31007, 31117, 32009, 48033, 48301)  do not have Sector level naicscodes  only top level "------"
Regions_Sector <- as.data.frame(unique(RegionalData_Sector$place)) 
names(Regions_Sector) <- "place"
RegionalData_Subsector <- RegionalData %>% filter(grepl('///', naics))
RegionalData_IndustryGroup <- RegionalData %>% filter(grepl('//', naics) & !grepl('///', naics))
RegionalData_NAICSIndustry <- RegionalData %>% filter(grepl('/', naics) & !grepl('///', naics)  & !grepl('//', naics))
RegionalData_USNAICS <- RegionalData %>% filter(!grepl('/', naics) & !grepl('-', naics))

# Parse TIGER and CBP crosswalk
TIGER_CBP <- inner_join(TIGERData, Regions_Sector, by = "place")
TIGER_CBP  <- TIGER_CBP[order(TIGER_CBP$place), ]
rownames(TIGER_CBP) <- TIGER_CBP$place


# Generate a cross walk to transform NAICS sectors used by CBP into BEA sectors 
CBP_table <- RegionalData_Sector
CBP_table$place <- paste0(CBP_table$fipstate, CBP_table$fipscty)
CBP_table %<>% subset(select = c(place, naics, emp, qp1, ap, est))
CBP_table$naics %<>% substr(0,2) %>% as.numeric()

CBP_table %<>% reshape(idvar = "naics", timevar = "place", direction = "wide")
CBP_table[is.na(CBP_table)] <- 0
CBP_table %<>%  as.data.frame() %>% dplyr::arrange(naics)

CBP_table %<>% t() %>%  as.data.frame()
CBP_table %<>% mutate("11" = V1)
CBP_table %<>% mutate("21" = V2)
CBP_table %<>% mutate("22" = V3)
CBP_table %<>% mutate("23" = V4)
CBP_table %<>% mutate("31G" = V5)
CBP_table %<>% mutate("42" = V6)
CBP_table %<>% mutate("44RT" = V7)
CBP_table %<>% mutate("48WT" = V8)
CBP_table %<>% mutate("51" = V9)
CBP_table %<>% mutate("FIRE" = V10 + V11)
CBP_table %<>% mutate("PROF" = V12 + V13 + V14)
CBP_table %<>% mutate("6" = V15 + V16)
CBP_table %<>% mutate("7" = V17 + V18)
CBP_table %<>% mutate("81" = V19)
CBP_table %<>% mutate("G" = V20)
CBP_table %<>% subset(select = -c(V1:V20)) %>% slice(-c(1)) %>% t()
BEA_Sectors <- row.names(CBP_table)
CBP_table <- cbind(BEA_Sectors, CBP_table) %>% as.data.frame()
CBP_table %<>% reshape(idvar = "BEA_Sectors", varying = c(colnames(CBP_table)[-1]), direction = "long")
rownames(CBP_table) <- 1:nrow(CBP_table)
names(CBP_table)[names(CBP_table)=="time"] <- "place"
CBP_table$place  %<>% formatC(width = 5, format = "d", flag = "0")
CBP_table$emp <-  as.numeric(CBP_table$emp)
CBP_table$qp1 <-  as.numeric(CBP_table$qp1)
CBP_table$ap <-  as.numeric(CBP_table$ap)
CBP_table$est <-  as.numeric(CBP_table$est)


# Import RUCA tables into R 
if (!exists("RUCAData")){
  RUCAData <- path(getwd(), "data", "ruca2010revised.xlsx") %>% read.xlsx(startRow = 2)
}

# Import RUCC tables into R 
if (!exists("RUCCData")){
  RUCCData <- path(getwd(), "data", "ruralurbancodes2013.xls") %>% read_xls()
}

RUCCData$place <- RUCCData$FIPS
####  Note:  2 counties (02158, 46102) are not common to TIGER_CBP in RUCCData


# Import UIC tables into R 
if (!exists("UICData")){
  UICData <- path(getwd(), "data", "UrbanInfluenceCodes2013.xls") %>% read_xls()
}
UICData$place <- UICData$FIPS




#Parse TIGER/CBP and RUCC crosswalk
TIGER_CBP_RUCC <- inner_join(TIGER_CBP, RUCCData, by = "place")
rownames(TIGER_CBP_RUCC) <- TIGER_CBP_RUCC$place
# add augmented hierarchical classification 
TIGER_CBP_RUCC <- transform(TIGER_CBP_RUCC, H3 = ifelse(RUCC_2013 %in% 1:3, 1, ifelse(RUCC_2013 %in% 4:6, 2, ifelse(RUCC_2013%in% 7:9, 3, 0)  ) ) )


# Produce Distance Matrix
Dist_mat <- TIGER_CBP_RUCC$center %>% as_Spatial() %>% distm()
rownames(Dist_mat) = colnames(Dist_mat) <- TIGER_CBP_RUCC$place


## Proximity Matrix
Prox_mat <- poly2nb(TIGER_CBP_RUCC, queen = TRUE) %>% nb2mat(style = "B", zero.policy = TRUE)
colnames(Prox_mat) <- rownames(Prox_mat)


# Import CBP data using separate Python pre-processing
# use_condaenv('rurec')
# cbp <- import('rurec.cbp')
# regional_data_py <- cbp$get_df("state", 2019L)
# head(regional_data_py)





