
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

# Download and unzip Census CBP data 2012.
data_zipr(
  ZipURL = "https://www2.census.gov/programs-surveys/cbp/datasets/2012/cbp12co.zip",
  DestDir =  getwd() %>% path("data"),
  FileExt = "txt"
)


# Download and unzip 2020 BLS Quarterly Census of Employment and Wages (QCEW).
data_zipr(
  ZipURL = "https://data.bls.gov/cew/data/files/2020/csv/2020_annual_singlefile.zip",
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

# Import CBP data 2012 into R 
if (!exists("RegionalData12")){
  RegionalData12 <- path(getwd(), "data", "cbp12co.txt") %>% read.csv(header = TRUE)
  
  RegionalData12$fipstate %<>% formatC(width = 2, format = "d", flag = "0")
  RegionalData12$fipscty  %<>% formatC(width = 3, format = "d", flag = "0")
}


#Import 2020 BLS Quarterly Census of Employment and Wages (QCEW) into R 
if (!exists("QCEW_2020")){
  QCEW_2020 <- path(getwd(), "data", "2020.annual.singlefile.csv") %>% read.csv(header = TRUE)
}
QCEW_2020_Sum <- filter(QCEW_2020, agglvl_code == 75)
QCEW_2020_Sum %<>% filter(own_code == 5)
QCEW_2020_Sum %<>% filter(industry_code != 999)
QCEW_2020_Sum %<>% filter(disclosure_code != "N")
QCEW_2020_Sum %<>% subset(select = c(area_fips, industry_code, annual_avg_estabs, annual_avg_emplvl, total_annual_wages, avg_annual_pay))
QCEW_2020_Sum %<>% rename(place = area_fips, 
                          naics = industry_code)

QCEW_2020_miss <- c("13265", "17151", "31005", "31009", "31091", "31113", "31115", "31171", "35021", "38065", "38085", "41069", "46017", "46095", "46137")
for (i in 1:length(QCEW_2020_miss)){
  QCEW_2020_Sum %<>% add_row(place = QCEW_2020_miss[i], naics = "111" )
}

Naics_Sum <- sort(unique(QCEW_2020_Sum$naics))



QCEW_2020_Sum %<>% reshape(idvar = "naics", timevar = "place", direction = "wide")
QCEW_2020_Sum %<>% arrange(naics)
QCEW_2020_Sum[is.na(QCEW_2020_Sum)] <- 0
QCEW_2020_Sum %<>% as.data.frame() %>% dplyr::arrange(naics)
QCEW_2020_Sum %<>% t() %>%  as.data.frame()
QCEW_2020_Sum[] <- lapply(QCEW_2020_Sum, function(x) as.numeric(as.character(x)))

# Generate a cross walk to transform 3-level NAICS  used by CBP into BEA Summary level industries 
QCEW_2020_Sum %<>% mutate("111CA" = V1 + V2)
QCEW_2020_Sum %<>% mutate("113FF" = V3 + V4 + V5)
QCEW_2020_Sum %<>% mutate("211" = V6)
QCEW_2020_Sum %<>% mutate("212" = V7)
QCEW_2020_Sum %<>% mutate("213" = V8)
QCEW_2020_Sum %<>% mutate("22" = V9)
QCEW_2020_Sum %<>% mutate("23" = V10 + V11 + V12)
QCEW_2020_Sum %<>% mutate("311FT" = V13 + V14)
QCEW_2020_Sum %<>% mutate("313TT" = V15 + V16)
QCEW_2020_Sum %<>% mutate("315AL" = V17 + V18)
QCEW_2020_Sum %<>% mutate("321" = V19)
QCEW_2020_Sum %<>% mutate("322" = V20)
QCEW_2020_Sum %<>% mutate("323" = V21)
QCEW_2020_Sum %<>% mutate("324" = V22)
QCEW_2020_Sum %<>% mutate("325" = V23)
QCEW_2020_Sum %<>% mutate("326" = V24)
QCEW_2020_Sum %<>% mutate("327" = V25)
QCEW_2020_Sum %<>% mutate("331" = V26)
QCEW_2020_Sum %<>% mutate("332" = V27)
QCEW_2020_Sum %<>% mutate("333" = V28)
QCEW_2020_Sum %<>% mutate("334" = V29)
QCEW_2020_Sum %<>% mutate("335" = V30)
QCEW_2020_Sum %<>% mutate("336XX" = V31)
QCEW_2020_Sum %<>% mutate("337" = V32)
QCEW_2020_Sum %<>% mutate("339" = V33)
QCEW_2020_Sum %<>% mutate("42" = V34 + V35 + V36)
QCEW_2020_Sum %<>% mutate("441" = V37)
QCEW_2020_Sum %<>% mutate("445" = V41)
QCEW_2020_Sum %<>% mutate("452" = V46)
QCEW_2020_Sum %<>% mutate("4A0" = V38 + V39 + V40 + V42 + V43 + V44 + V45 + V47 + V48)
QCEW_2020_Sum %<>% mutate("481" = V49)
QCEW_2020_Sum %<>% mutate("482" = V50)
QCEW_2020_Sum %<>% mutate("483" = V51)
QCEW_2020_Sum %<>% mutate("484" = V52)
QCEW_2020_Sum %<>% mutate("485" = V53)
QCEW_2020_Sum %<>% mutate("486" = V54)
QCEW_2020_Sum %<>% mutate("487OS" = V55 + V56 + V58)
QCEW_2020_Sum %<>% mutate("493" = V59)
QCEW_2020_Sum %<>% mutate("511" = V60)
QCEW_2020_Sum %<>% mutate("512" = V61)
QCEW_2020_Sum %<>% mutate("513" = V62 + V63)
QCEW_2020_Sum %<>% mutate("514" = V64 + V65)
QCEW_2020_Sum %<>% mutate("521CI" = V66 + V67)
QCEW_2020_Sum %<>% mutate("523" = V68)
QCEW_2020_Sum %<>% mutate("524" = V69)
QCEW_2020_Sum %<>% mutate("525" = V70)
QCEW_2020_Sum %<>% mutate("HSOREXX" = V71)
QCEW_2020_Sum %<>% mutate("532RL" = V72 + V73)
QCEW_2020_Sum %<>% mutate("541XX" = V74)
QCEW_2020_Sum %<>% mutate("55" = V75)
QCEW_2020_Sum %<>% mutate("561" = V76)
QCEW_2020_Sum %<>% mutate("562" = V77)
QCEW_2020_Sum %<>% mutate("61" = V78)
QCEW_2020_Sum %<>% mutate("621" = V79)
QCEW_2020_Sum %<>% mutate("622" = V80)
QCEW_2020_Sum %<>% mutate("623" = V81)
QCEW_2020_Sum %<>% mutate("624" = V82)
QCEW_2020_Sum %<>% mutate("711AS" = V83 + V84)
QCEW_2020_Sum %<>% mutate("713" = V85)
QCEW_2020_Sum %<>% mutate("721" = V86)
QCEW_2020_Sum %<>% mutate("722" = V87)
QCEW_2020_Sum %<>% mutate("81" = V88 + V89 + V90 + V91)


QCEW_2020_Sum %<>% subset(select = -c(V1:V91)) %>% slice(-c(1)) %>% t()

BEA_Summary <- row.names(QCEW_2020_Sum)

QCEW_2020_Sum <- cbind(BEA_Summary, QCEW_2020_Sum) %>% as.data.frame()
QCEW_2020_Sum %<>% reshape(idvar = "BEA_Summary", varying = c(colnames(QCEW_2020_Sum)[-1]), direction = "long")
rownames(QCEW_2020_Sum) <- 1:nrow(QCEW_2020_Sum)
names(QCEW_2020_Sum)[names(QCEW_2020_Sum)=="time"] <- "place"
QCEW_2020_Sum$place  %<>% formatC(width = 5, format = "d", flag = "0")
QCEW_2020_Sum$annual_avg_estabs <-  as.numeric(QCEW_2020_Sum$annual_avg_estabs)
QCEW_2020_Sum$annual_avg_emplvl <-  as.numeric(QCEW_2020_Sum$annual_avg_emplvl)
QCEW_2020_Sum$total_annual_wages <-  as.numeric(QCEW_2020_Sum$total_annual_wages)
QCEW_2020_Sum$avg_annual_pay <-  as.numeric(QCEW_2020_Sum$avg_annual_pay)




# Import TIGER data into R 
if (!exists("TIGERData")){
  TIGERData <- path(getwd(), "data", "cb_2021_us_county_500k.shp") %>% st_read(stringsAsFactors = FALSE)
  TIGERData$place <- paste0(TIGERData$STATEFP, TIGERData$COUNTYFP)
  TIGERData$center <- st_centroid(TIGERData$geometry)
}



# Process and parse hierarchical structure  of CBP data
RegionalData$place <- paste0(RegionalData$fipstate, RegionalData$fipscty)
RegionalData %<>% subset(select = c(fipstate, fipscty, place, naics, emp, qp1, ap, est))
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
CBP_sector <- RegionalData_Sector
CBP_sector$place <- paste0(CBP_sector$fipstate, CBP_sector$fipscty)
CBP_sector %<>% subset(select = c(place, naics, emp, qp1, ap, est))
CBP_sector$naics %<>% substr(0,2) %>% as.numeric()

CBP_sector %<>% reshape(idvar = "naics", timevar = "place", direction = "wide")
CBP_sector[is.na(CBP_sector)] <- 0
CBP_sector %<>%  as.data.frame() %>% dplyr::arrange(naics)

CBP_sector %<>% t() %>%  as.data.frame()
CBP_sector %<>% mutate("11" = V1)
CBP_sector %<>% mutate("21" = V2)
CBP_sector %<>% mutate("22" = V3)
CBP_sector %<>% mutate("23" = V4)
CBP_sector %<>% mutate("31G" = V5)
CBP_sector %<>% mutate("42" = V6)
CBP_sector %<>% mutate("44RT" = V7)
CBP_sector %<>% mutate("48WT" = V8)
CBP_sector %<>% mutate("51" = V9)
CBP_sector %<>% mutate("FIRE" = V10 + V11)
CBP_sector %<>% mutate("PROF" = V12 + V13 + V14)
CBP_sector %<>% mutate("6" = V15 + V16)
CBP_sector %<>% mutate("7" = V17 + V18)
CBP_sector %<>% mutate("81" = V19)
CBP_sector %<>% mutate("G" = V20)
CBP_sector %<>% subset(select = -c(V1:V20)) %>% slice(-c(1)) %>% t()
BEA_Sectors <- row.names(CBP_sector)
CBP_sector <- cbind(BEA_Sectors, CBP_sector) %>% as.data.frame()
CBP_sector %<>% reshape(idvar = "BEA_Sectors", varying = c(colnames(CBP_sector)[-1]), direction = "long")
rownames(CBP_sector) <- 1:nrow(CBP_sector)
names(CBP_sector)[names(CBP_sector)=="time"] <- "place"
CBP_sector$place  %<>% formatC(width = 5, format = "d", flag = "0")
CBP_sector$emp <-  as.numeric(CBP_sector$emp)
CBP_sector$qp1 <-  as.numeric(CBP_sector$qp1)
CBP_sector$ap <-  as.numeric(CBP_sector$ap)
CBP_sector$est <-  as.numeric(CBP_sector$est)


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


# Import CBP data using separate Python pre-processing (If trouble restart R, load reticulate, load conda env "rurec")
# library(reticulate)
# use_condaenv('rurec')
# cbp <- import('rurec.cbp')
# regional_data_py <- cbp$get_df("state", 2019L)
# head(regional_data_py)
# 
# CBP <- cbp$get_df("county", 2019L)
# CBP %<>% filter(industry != "-")

## Detail level CBP processing 
# CBP_detail <- filter(RegionalData, naics != "------") 
# CBP_detail$naics <- as.numeric(regmatches(CBP_detail$naics, gregexpr("[0-9.]+", CBP_detail$naics)))
# AllNaics <- unique(CBP_detail$naics) %>% as.vector() %>% sort()
# CBP_detail <- filter(CBP_detail, naics %in% unique(Naics_D))
# 
# CBP_detail %<>% reshape(idvar = "naics", timevar = "place", direction = "wide")
# CBP_detail[is.na(CBP_detail)] <- 0
# CBP_detail %<>%  as.data.frame() %>% dplyr::arrange(naics) 
# CBP_detail %<>% t() %>%  as.data.frame()
# CBP_detail[1,] <- as.numeric(CBP_detail[1,] )




# Parse TIGER and QCED crosswalk
Regions_Summary <- as.data.frame(unique(QCEW_2020_Sum$place)) 
names(Regions_Summary) <- "place"
TIGER_QCEW <- inner_join(TIGERData, Regions_Summary, by = "place")
TIGER_QCEW  <- TIGER_CBP[order(TIGER_CBP$place), ]
rownames(TIGER_QCEW) <- TIGER_CBP$place


#Parse TIGER/CBP and RUCC crosswalk
TIGER_QCEW_RUCC <- inner_join(TIGER_QCEW, RUCCData, by = "place")
rownames(TIGER_QCEW_RUCC) <- TIGER_QCEW_RUCC$place
# add augmented hierarchical classification 
TIGER_QCEW_RUCC <- transform(TIGER_QCEW_RUCC, H3 = ifelse(RUCC_2013 %in% 1:3, 1, ifelse(RUCC_2013 %in% 4:6, 2, ifelse(RUCC_2013%in% 7:9, 3, 0)  ) ) )











