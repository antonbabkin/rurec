
# Establish working directory relative to location of this file
script_path() %>% setwd() 

# Connect and parse code from another file 
source("Functions.R")

# Push working directory up a level to establish parallel data files
setwd("..")

# Create "data" a file if not already done
if (!file.exists("data")) {
  "data" %>% dir.create()
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









