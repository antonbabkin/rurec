# General purpose tools and utilities

root_dir <- rprojroot::find_rstudio_root_file()

#' Cache function output to a file
#' 
#' filename may include references to function arguments using glue syntax.
#' Limitation: fn <- cache(fn, ...) creates infinite recursive reference to fn.
#' To avoid, either use distinct names: fn <- cache(.fn, ...)
#' Or pass anonymous function: fn <- cache(function(x) {x}, ...)
#'
#' @param f Function to be cached.
#' @param filename Where to save cache.
#' @return Function with automatic caching.
#' 
#' @examples
#' slow_fun <- function(t) {Sys.sleep(t); t}
#' fast_fun <- cache(slow_fun, "cache/slow_fun.rds")
cache <- function(f, filename) {
  wrapper <- function(...) {
    
    # PROBLEM! does not work if optional argument is not given
    # try (cache(function(x, y=1) {}))(1)
    
    # create unevaluated function call
    c0 <- rlang::call2(f, ...)
    # call with arguments specified by their full names
    c1 <- rlang::call_match(c0, f)
    # list of named arguments
    e <- rlang::call_args(c1)
    # substitute call args into filename template
    filename <- glue::glue(filename, .envir = e)
    
    
    if (file.exists(filename)) {
      logger::log_debug(paste("read from cache", filename))
      res <- readRDS(filename)
    } else {
      res <- f(...)
      dir.create(dirname(filename), showWarnings = FALSE, recursive = TRUE)
      saveRDS(res, filename)
      logger::log_debug(paste("saved to cache", filename))
    }
    return(res)
  }
  return(wrapper)
}
