# General purpose tools and utilities

root_dir <- rprojroot::find_rstudio_root_file()

simplecache <- function(f, filename) {
  wrapper <- function(...) {
    if (file.exists(filename)) {
      print(paste("read from cache", filename))
      res <- readRDS(filename)
    } else {
      res <- f(...)
      dir.create(dirname(filename), showWarnings = FALSE, recursive = TRUE)
      saveRDS(res, filename)
      print(paste("saved to cache", filename))
    }
    return(res)
  }
  return(wrapper)
}
