# Download all R packages as necessary for project


# List packages needed for this exercise
packages <- c("cowplot", 
              "dash",
              "dlm",
              "DT",
              "fs",
              "geosphere",
              "ggiraph",
              "glue",
              "gtools",
              "knitr",
              "magrittr",
              "Matrix",
              "openxlsx",
              "plotly",
              "RColorBrewer",
              "reshape2",
              "readxl",
              "reticulate",
              "scales",
              "sf",
              "shinydashboard",
              "spdep",
              "tidyr",
              "tidyverse",
              "tmap",
              "tmaptools",
              "tools"
              )


# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages], dependencies = TRUE)
}