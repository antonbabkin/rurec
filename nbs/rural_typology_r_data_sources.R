# Download all used outside data

# Load and attach necessary packages
library(fs)
library(rprojroot)
library(tidyr)
library(tools)
library(rlog)

# Display start time
log_info("Define data sources start")

# Connect and parse code from another file
source(file.path(find_rstudio_root_file(), "nbs", "rural_typology_r_functions.R"))

# Create "data" folder if not already done
  data_dir = file.path(find_rstudio_root_file(), "data")
  if (!file.exists(data_dir)) {
     dir.create(data_dir)
  }

# Increase download timeout for slower connections
download_timeout_old <- getOption("timeout")
options(timeout = 300)

# Download and unzip BEA IO data.
data_zipr(
  ZipURL = "https://apps.bea.gov/industry/iTables%20Static%20Files/AllTablesSUP.zip",
  DestDir = data_dir
)
log_info("BEA IO data download complete")

# Download and unzip Census CBP data 2019.
data_zipr(
  ZipURL = "https://www2.census.gov/programs-surveys/cbp/datasets/2019/cbp19co.zip",
  DestDir = data_dir,
  FileExt = "txt"
)
log_info("CBP data 2019 download complete")

# Download and unzip Census CBP data 2012.
data_zipr(
  ZipURL = "https://www2.census.gov/programs-surveys/cbp/datasets/2012/cbp12co.zip",
  DestDir = data_dir,
  FileExt = "txt"
)
log_info("CBP data 2012 download complete")

##Note: More robustness and  refinement is needed. Zip name has underscored but unzipped name which it is checked against changes to dot 
# Download and unzip 2020 BLS Quarterly Census of Employment and Wages (QCEW).
data_zipr(
  ZipURL = "https://data.bls.gov/cew/data/files/2020/csv/2020_annual_singlefile.zip",
  DestDir = data_dir
) %>% suppressWarnings()
log_info("QCEW 2020 download complete")



# Download and unzip Census county TIGER  data.
data_zipr(
  ZipURL = "https://www2.census.gov/geo/tiger/TIGER2021/COUNTY/tl_2021_us_county.zip",
  DestDir = data_dir
) %>% suppressWarnings()
log_info("TIGER line data download complete")

data_zipr(
  ZipURL = "https://www2.census.gov/geo/tiger/GENZ2021/shp/cb_2021_us_county_500k.zip",
  DestDir = data_dir
) %>% suppressWarnings()
log_info("TIGER 500k county data download complete")

# Download RUCA codes.
data_getr(
  FileURL = "https://www.ers.usda.gov/webdocs/DataFiles/53241/ruca2010revised.xlsx",
  DestDir = data_dir
)
log_info("RUCA data download complete")


# Download RUCC codes.
data_getr(
  FileURL = "https://www.ers.usda.gov/webdocs/DataFiles/53251/ruralurbancodes2013.xls",
  DestDir = data_dir
)
log_info("RUCC data download complete")

# Download Urban Influence codes.
data_getr(
  FileURL = "https://www.ers.usda.gov/webdocs/DataFiles/53797/UrbanInfluenceCodes2013.xls",
  DestDir = data_dir
)
log_info("Urban Influence Codes download complete")

# Revert to previously set timeout
options(timeout = download_timeout_old)

# Remove clutter
rm(data_dir, zip_dir, download_timeout_old) %>% suppressWarnings()

# Display end time
log_info("Define data sources end")


