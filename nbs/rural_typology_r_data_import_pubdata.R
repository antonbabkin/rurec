# Import "pubdata" preprocessed datafiles

# Load and attach necessary packages
library(reticulate)
library(rprojroot)
library(dplyr)
library(rlog)

# Display start time
log_info("Import pubdata start")

# Connect  and  parse  code  from  another  file 
source(file.path(find_rstudio_root_file(), "nbs", "rural_typology_r_functions.R"))

# Data folder directory pathname
data_dir = file.path(find_rstudio_root_file(), "data", "rpyobjs")


# Load conda environment "rurec"
use_condaenv('rurec')

# Import pubdata Python modules
bds <- import('rurec.pubdata.bds')
bea_io <- import('rurec.pubdata.bea_io')
cbp <- import('rurec.pubdata.cbp')
ers_rurality <- import('rurec.pubdata.ers_rurality')
geography <- import('rurec.pubdata.geography')
naics <- import('rurec.pubdata.naics')
population <- import('rurec.pubdata.population')


# Import CBP data 2019
if (!file.exists(file.path(data_dir, "CBP_2019"))){
  if (!exists("CBP_2019")){
    CBP_2019 <- cbp$get_df("county", 2019L)
    CBP_2019$place <- paste0(CBP_2019$fipstate, CBP_2019$fipscty)
  }
}
pysaver(CBP_2019)


#tCBP_2019 <- file.path(data_dir, "CBP_2019") %>% readRDS()



# Display end time
log_info("Import pubdata end")


