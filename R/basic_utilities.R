
#simple independent utility functions in base R for cohesion, conversion, or manipulation

# file management ----
mkdir <- function(p) {
  d <- dirname(p)
  if (!dir.exists(d)) {
    logger::log_debug(paste("Creating directory", d))
    dir.create(d, recursive = TRUE)
  }
  return(p)
}


clear_paths <- function(paths) {
  for (ps in paths) {
    ps <- ps |>
      as.character() |>
      gsub("\\{.*?\\}", "*", x = _) |>
      Sys.glob()
    for (p in ps) {
      if (file.exists(p)) {
        log_debug("Removing file ", p)
        unlink(p)
      }
    }
  }
}


zip_pack <- function(zipfile, files, overwrite = FALSE) {
  stopifnot(getwd() == rprojroot::find_rstudio_root_file())
  if (file.exists(zipfile)) {
    if (overwrite) {
      logger::log_info(paste("Replacing existing Zip file:", zipfile))
      file.remove(zipfile)
    }
    else stop("Zip file already exists: ", zipfile)
  }
  # turn all "{...}" into "*" and expand resulting wildcard file names
  files <- files |>
    as.character() |>
    gsub("\\{.*?\\}", "*", x = _) |>
    Sys.glob()
  zip(mkdir(zipfile), files)
}



zip_unpack <- function(zipfile, overwrite = FALSE) {
  stopifnot(getwd() == rprojroot::find_rstudio_root_file())
  stopifnot(file.exists(zipfile))
  unzip(zipfile, overwrite = overwrite)
}

#' Un-list columns in pandas dataframes
#' Sometimes pandas df columns come through reticulate as lists that need unlisting.
#' This is likely happening with string columns with missing values.
#' More investigation is desirable, maybe this corrections could be done by customizing reticulate.
#' For now, simply unlist() all columns that are of list class.
#' Usage: df <- reticulate_unlist_cols(df)
reticulate_unlist_cols <- function(df) {
  df %>% mutate(across(
    where(is.list), 
    \(col) unlist(map_if(col, \(el) is.null(el) || is.nan(el), \(y) NA))
  ))
}

#### Add specified rows and columns of a vector matrix
vector_collapse <- function(vector, 
                            collapse_names, 
                            new_name){
  vec <- vector
  cl <- collapse_names
  nn <- new_name
  vec <- cbind(rowSums(vec[, which(colnames(vec) %in% cl), drop=F]), vec[, which(!colnames(vec) %in% cl), drop = F] )
  colnames(vec)[1] <- nn
  cn <- colnames(vector)[!colnames(vector) %in% cl] %>% append(nn, after = (min(which(colnames(vector) %in% cl)) - 1))
  vec <- vec[,cn, drop=F] 
  return(vec)
}

#### Add specified rows and columns of a matrix
matrix_collapse <- function(matrix, 
                            collapse_names, 
                            new_name){
  mat <- matrix
  cl <- collapse_names
  nn <- new_name
  mat <- rbind(colSums(mat[which(rownames(mat) %in% cl), ]), mat[which(!rownames(mat) %in% cl), ] )
  mat <- cbind(rowSums(mat[, which(colnames(mat) %in% cl)]), mat[, which(!colnames(mat) %in% cl) ] )
  rownames(mat)[1] <- nn
  colnames(mat)[1] <- nn
  rn <- rownames(matrix)[!rownames(matrix) %in% cl] %>% append(nn, after = (min(which(rownames(matrix) %in% cl)) - 1))
  cn <- colnames(matrix)[!colnames(matrix) %in% cl] %>% append(nn, after = (min(which(colnames(matrix) %in% cl)) - 1))
  mat <- mat[rn, cn] 
  return(mat)
}

### Create edgelist from a matrix
matrix2edgelist <- function(x){
  df <- do.call(cbind, 
                lapply(list("row_index" = row(x), 
                            "col_index" = col(x), 
                            "value" = x), 
                       as.vector))
}

### Create matrix from an edgelist
edgelist2matrix <- function(x){
  df <- matrix(x[,3], 
               nrow = length(unique(x[,1])), 
               ncol = length(unique(x[,2])), 
               dimnames = list(unique(x[,1]), 
                               unique(x[,2])))
  return(df)
}

# Convert miles to meters
miles2meters <- function(miles){
  df <- as.integer(miles)*1609.344
  return(df)
}

# Convert miles to meters
meters2miles <- function(meters){
  df <- as.integer(meters)/1609.344
  return(df)
}

year2tiger <- function(year){
  tiger_year = c(2022:2013, 2010, 2000, 1990)
  if(year %in% tiger_year){
    x <- year
  }else if(year > max(tiger_year)){
    x <- max(tiger_year)
  }else if(year > 2011){
    x <- 2013
  }else if(year > 2005){
    x <- 2010
  }else if(year > 1995){
    x <- 2000
  }else if(year < 1996){
    x <- min(tiger_year)
  }
  if(!year %in% tiger_year){
    warning("Shapefile years do not contain [",year,"] using [", x,"]")
  }
  return(as.integer(x))
}

nearest_point <- function(x, grid) {
  if (x <= min(grid)) return(min(grid))
  if (x >= max(grid)) return(max(grid))
  grid <- sort(grid)
  for (i in 1:(length(grid)-1)) {
    lo <- grid[i]
    hi <- grid[i+1]
    if (x > hi) next
    if (x - lo < hi - x) return(lo)
    else return(hi)
  }
  stop("something is wrong! ", x, grid)
}

test_nearest_point <- function() {
  nearest_point(1993, c(2023, 2020, 2018, 2017, 2015, 2013, 2009:2003)) == 2003 # below min
  nearest_point(2050, c(2023, 2020, 2018, 2017, 2015, 2013, 2009:2003)) == 2023 # above max
  nearest_point(2005, c(2023, 2020, 2018, 2017, 2015, 2013, 2009:2003)) == 2005 # exact match
  nearest_point(2021, c(2023, 2020, 2018, 2017, 2015, 2013, 2009:2003)) == 2020 # lower bound
  nearest_point(2022, c(2023, 2020, 2018, 2017, 2015, 2013, 2009:2003)) == 2023 # upper bound
  nearest_point(2016, c(2023, 2020, 2018, 2017, 2015, 2013, 2009:2003)) == 2017 # tie
}


year2cbsa <- function(year){
  cbsa_year = c(2023, 2020, 2018, 2017, 2015, 2013, 2009:2003, 1993)
  x <- nearest_point(year, cbsa_year)
  if(!year %in% cbsa_year){
    warning("CBSA concordance years do not contain [",year,"] using [", x,"]")
  }
  return(as.integer(x))
}


year2rucc <- function(year){
  rucc_year = c(2013, 2003, 1993, 1983, 1974)
  x <- nearest_point(year, rucc_year)
  if(!year %in% rucc_year){
    warning("RUCC years do not contain [",year,"] using [", x,"]")
  }
  return(as.integer(x))
}

year2cbp <- function(year){
  cbp_year = c(2021:1986)
  x <- nearest_point(year, cbp_year)
  if(!year %in% cbp_year){
    warning("CBP years do not contain [",year,"] using [", x,"]")
  }
  return(as.integer(x))
}

year2bea <- function(year,
                     ilevel = c("det", "sum", "sec")){
  ilevel <- match.arg(ilevel)
  if(ilevel != "det"){
    bea_year = 2022:1997
    x <- nearest_point(year, bea_year)
    if(!year %in% bea_year){
      warning("BEA years do not contain [",year,"] using [", x,"]")
    }
  }
  if(ilevel == "det"){
    bea_year = c(2017, 2012, 2007)
    x <- nearest_point(year, bea_year)
    if(!year %in% bea_year){
      warning("Detail level BEA years do not contain [",year,"] using [", x,"]")
    }
  }
  return(as.integer(x))
}

year2agcensus <- function(year){
  ag_year = c(2017, 2012, 2007, 2002)
  x <- nearest_point(year, ag_year)
  if(!year %in% ag_year){
    warning("AgCensus years do not contain [",year,"] using [", x,"]")
  }
  return(as.integer(x))
}

year2infogroup <- function(year){
  info_year = c(2017:1997)
  x <- nearest_point(year, info_year)
  if(!year %in% info_year){
    warning("InfoGroup years do not contain [",year,"] using [", x,"]")
  }
  return(as.integer(x))
}
