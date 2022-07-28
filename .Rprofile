source("renv/activate.R")

library(reticulate)

# If conda can not be found in default locations, look at specified alternative paths
tryCatch(
  conda_binary(),
  error = function(e) {
    if (e$message != "Unable to find conda binary. Is Anaconda installed?") stop(e)

    alternative_conda_paths <- c(
      file.path(Sys.getenv("LOCALAPPDATA"), "mambaforge/condabin/conda.bat"),
      file.path(Sys.getenv("USERPROFILE"), "mambaforge/condabin/conda.bat")
    )
    for (conda_path in alternative_conda_paths) {
      if (file.exists(conda_path)) {
        options(reticulate.conda_binary = conda_path)
        print(c("Using conda found at:", conda_binary()))
        return()
      }
    }
    stop("Could not find conda.")
  }
)

use_condaenv("rurec")
