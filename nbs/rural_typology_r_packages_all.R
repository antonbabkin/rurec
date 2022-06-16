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
              "manipulateWidget",
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

invisible(lapply(packages, library, character.only = TRUE))

writeLines(capture.output(sessionInfo()), paste0("session_info_", basename(file_path_sans_ext(script_file())), ".txt"))

## invisible(lapply(paste0('package:', names(sessionInfo()$otherPkgs)), detach, character.only=TRUE, unload=TRUE))

