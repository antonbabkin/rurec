root_dir <- rprojroot::find_rstudio_root_file()

# Connect and parse code from another file 
source(file.path(root_dir, "nbs", "rural_typology_r_functions.R"))

# Create "data" folder if not already done
data_dir = file.path(root_dir, "data")
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

# Download and unzip Census CBP data.
data_zipr(
  ZipURL = "https://www2.census.gov/programs-surveys/cbp/datasets/2019/cbp19co.zip",
  DestDir = data_dir,
  FileExt = "txt"
)

# Download and unzip Census CBP data 2012.
data_zipr(
  ZipURL = "https://www2.census.gov/programs-surveys/cbp/datasets/2012/cbp12co.zip",
  DestDir = data_dir,
  FileExt = "txt"
)


# Download and unzip 2020 BLS Quarterly Census of Employment and Wages (QCEW).
data_zipr(
  ZipURL = "https://data.bls.gov/cew/data/files/2020/csv/2020_annual_singlefile.zip",
  DestDir = data_dir,
  FileExt = "txt"
)


# Download and unzip Census county TIGER  data.
data_zipr(
  ZipURL = "https://www2.census.gov/geo/tiger/TIGER2021/COUNTY/tl_2021_us_county.zip",
  DestDir = data_dir
)
data_zipr(
  ZipURL = "https://www2.census.gov/geo/tiger/GENZ2021/shp/cb_2021_us_county_500k.zip",
  DestDir = data_dir
)

# Download RUCA codes.
data_getr(
  FileURL = "https://www.ers.usda.gov/webdocs/DataFiles/53241/ruca2010revised.xlsx",
  DestDir = data_dir
)

# Download RUCC codes.
data_getr(
  FileURL = "https://www.ers.usda.gov/webdocs/DataFiles/53251/ruralurbancodes2013.xls",
  DestDir = data_dir
)

# Download Urban Influence codes.
data_getr(
  FileURL = "https://www.ers.usda.gov/webdocs/DataFiles/53797/UrbanInfluenceCodes2013.xls",
  DestDir = data_dir
)



# Revert to previusly set timeout
options(timeout = download_timeout_old)
rm(download_timeout_old)




