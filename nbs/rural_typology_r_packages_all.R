# Download all R packages as necessary for project

# Load packages
library(renv)
library(rprojroot)

# Run if new packages are added/updated
#snapshot()

# Check differences between the project lockfile and the current library
status()

# Sync system and project package versions
restore(lockfile = file.path(find_rstudio_root_file(), "renv.lock") ) 


# # If missing renv.lock file
# # Install packages not yet installed
# local({
#   packages <- c("cowplot",
#                 "dash",
#                 "dlm",
#                 "dplyr",
#                 "DT",
#                 "fs",
#                 "geosphere",
#                 "ggiraph",
#                 "glue",
#                 "gtools",
#                 "knitr",
#                 "magrittr",
#                 "manipulateWidget",
#                 "Matrix",
#                 "openxlsx",
#                 "plotly",
#                 "RColorBrewer",
#                 "renv",
#                 "reshape2",
#                 "readxl",
#                 "reticulate",
#                 "rprojroot",
#                 "scales",
#                 "sf",
#                 "shinydashboard",
#                 "spdep",
#                 "tidyr",
#                 "tidyverse",
#                 "tmap",
#                 "tmaptools",
#                 "tools"
#   )
# 
#   installed_packages <- packages %in% rownames(installed.packages())
#   if (any(installed_packages == FALSE)) {
#     install.packages(packages[!installed_packages], dependencies = TRUE)
#   }
# })


# Alternative for version control 
#writeLines(capture.output(sessionInfo()), paste0("session_info_", basename(file_path_sans_ext(script_file())), ".txt"))


