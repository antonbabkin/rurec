
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

# Set manipulation ----

# Set operation to find the symmetric difference
symm_diff <- function(x, y){
  setdiff(union(x, y) , intersect(x, y))
}


temporal_permutations <- function(year_range){
  df <- c(year_range) %>% 
    expand_grid(., .) %>% 
    .[.[1] < .[2], ]
  return(df)
}


sequential_pairs <- function(year_range){
  embed(year_range, 2)
}


# matrix manipulation ----

# get matrix output format from long format data used with parquet storage
long2matrix <- function(df, 
                        values_from = names(df)[3], 
                        names_from = names(df)[2], 
                        id_cols = names(df)[1]){
  df <- df %>% 
    pivot_wider(id_cols = all_of(id_cols), 
                names_from = all_of(names_from), 
                values_from = all_of(values_from)) %>% 
    as.data.frame() 
  rownames(df) <- df[[id_cols]]
  df <- df[-1] %>% 
    as.matrix() 
  return(df)
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

# spatial concordance ----

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

# temporal concordance ----

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

# test_nearest_point <- function() {
#   nearest_point(1993, c(2023, 2020, 2018, 2017, 2015, 2013, 2009:2003)) == 2003 # below min
#   nearest_point(2050, c(2023, 2020, 2018, 2017, 2015, 2013, 2009:2003)) == 2023 # above max
#   nearest_point(2005, c(2023, 2020, 2018, 2017, 2015, 2013, 2009:2003)) == 2005 # exact match
#   nearest_point(2021, c(2023, 2020, 2018, 2017, 2015, 2013, 2009:2003)) == 2020 # lower bound
#   nearest_point(2022, c(2023, 2020, 2018, 2017, 2015, 2013, 2009:2003)) == 2023 # upper bound
#   nearest_point(2016, c(2023, 2020, 2018, 2017, 2015, 2013, 2009:2003)) == 2017 # tie
# }

year2tiger <- function(year){
  tiger_year = c(2022:2013, 2010, 2000, 1990)
  x <- nearest_point(year, tiger_year)
  if(!year %in% tiger_year){
    warning("Shapefile years do not contain [",year,"] using [", x,"]")
  }
  return(as.integer(x))
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

year2bea_rea <- function(year){
  bea_rea = c(2022:2017)
  x <- nearest_point(year, bea_rea)
  if(!year %in% bea_rea){
    warning("BEA-REA data years do not contain [",year,"] using [", x,"]")
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

year2bea_concord <- function(year){
  concord_year = c(2017,2012)
  x <- nearest_point(year, concord_year)
  if(!year %in% concord_year){
    warning("Concordance years do not contain [",year,"] using [", x,"]")
  }
  return(as.integer(x))
}


year2bea_profile <- function(year){
    bea_year = 2022:1969
    x <- nearest_point(year, bea_year)
    if(!year %in% bea_year){
      warning("BEA profile years do not contain [",year,"] using [", x,"]")
    }
  return(as.integer(x))
}

year2ers_labor <- function(year){
  ers_year = 2022:2000
  x <- nearest_point(year, ers_year)
  if(!year %in% ers_year){
    warning("ERS labor stats years do not contain [",year,"] using [", x,"]")
  }
  return(as.integer(x))
}

year2tidy_acs5 <- function(year){
  tidy_acs_year = 2022:2009
  x <- nearest_point(year, tidy_acs_year)
  if(!year %in% tidy_acs_year){
    warning("tidycensus ACS-5 data years do not contain [",year,"] using [", x,"]")
  }
  return(as.integer(x))
}

year2tidy_acs1 <- function(year){
  tidy_acs_year = c(2022:2021, 2019:2005)
  x <- nearest_point(year, tidy_acs_year)
  if(!year %in% tidy_acs_year){
    warning("tidycensus ACS-1 data years do not contain [",year,"] using [", x,"]")
  }
  return(as.integer(x))
}

year2saipe <- function(year){
  saipe_year = c(2022:1995, 1993, 1989)
  x <- nearest_point(year, saipe_year)
  if(!year %in% saipe_year){
    warning("SAIPE data years do not contain [",year,"] using [", x,"]")
  }
  return(as.integer(x))
}

year2chr <- function(year){
  chr_year = c(2023:2011)
  x <- nearest_point(year, chr_year)
  if(!year %in% chr_year){
    warning("CHR data years do not contain [",year,"] using [", x,"]")
  }
  return(as.integer(x))
}


# temporal recursion ----

# return a list of results from a function with a year argument, optional function arguments are permitted
temp_fun_recur_list <- function(set_of_years = 2000:2020, 
                                function_name, 
                                ...){
  df <- vector("list", length(set_of_years))
  names(df) <- set_of_years
  x <- list(set_of_years = set_of_years)
  opargs <- list(...)
  if (length(opargs) > 0){
    for(i in 1:length(opargs)){
      assign(paste(names(opargs[i])), opargs[[i]][1])
    }
  }
  for (y in 1:length(set_of_years)){
        year <- x$set_of_years[[y]]
        df[[y]] <- do.call(deparse(substitute(function_name)), c(list(year = year, ... )) )
  }
  return(df)
}


# Tests ----





