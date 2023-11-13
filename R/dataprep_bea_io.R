# Data preparation of BEA I-O tables

# R libraries ----
library(logger)
log_threshold(DEBUG)
library(arrow)
library(tidyverse)
library(glue)


# R scripts ----
source("R/basic_utilities.R", local = (util <- new.env()))


# Python modules ----
pymod <- new.env()
pymod$initialized <- FALSE

#' Initialize environment with necessary Python modules imported through reticulate
#' Running multiple times is safe and only imports once.
pymod$init <- function() {
  if (pymod$initialized) return()
  library(reticulate)
  use_condaenv("rurec")
  pymod$bea_io <- import("rurec.pubdata.bea_io")
  pymod$initialized <- TRUE
}


# Data objects ----
ipath <- list(
  raw_2022 = "data/pubdata/bea_io/src/AllTablesSUP_2022q2.zip",
  raw_2023 = "data/pubdata/bea_io/src/AllTablesSUP_2023.zip"
)

opath <- list(
  naics_concord_ = "data/bea_io/naics_concord/{year}.rds",
  sup_ = "data/bea_io/sup/{level}/{year}_{labels}.rds",
  use_ = "data/bea_io/use/{level}/{year}_{labels}.rds"
)

clear_outputs <- function() {
  util$clear_paths(opath)
}


# Pubdata ----
pubdata <- new.env()

pubdata$get_naics_concord <- function(year) {
  year <- as.integer(year)
  p <- glue(opath$naics_concord_)
  if (file.exists(p)) {
    log_debug(paste("read from cache", p))
    return(readRDS(p))
  }
  pymod$init()
  x <- pymod$bea_io$get_naics_concord(year) %>%
    util$reticulate_unlist_cols()
  
  log_debug(paste("save to cache", p))
  saveRDS(x, util$mkdir(p))
  return(x)
}

pubdata$get_sup <- function(year, level, labels = FALSE) {
  match.arg(level, c("det", "sum", "sec"))
  year <- as.integer(year)
  p <- glue(opath$sup_)
  if (file.exists(p)) {
    log_debug(paste("read from cache", p))
    return(readRDS(p))
  }
  
  pymod$init()
  x <- pymod$bea_io$get_sup(year, level, labels)
  
  # repair row names of sector supply tables
  # last row in spreadsheet is missing value in "T017" the code column
  # when dataframe is converted from pandas, entire row index is ignored because of a missing value
  if (level == "sec") {
    x$row_names[[length(x$row_names)]] <- c("T017", "Total industry supply")
    if (!labels) {
      rownames(x$table) <- map_chr(x$row_names, \(x) x[1])
    }
  }
  
  log_debug(paste("save to cache", p))
  saveRDS(x, util$mkdir(p))
  return(x)
}


pubdata$get_use <- function(year, level, labels = FALSE) {
  match.arg(level, c("det", "sum", "sec"))
  year <- as.integer(year)
  p <- glue(opath$use_)
  if (file.exists(p)) {
    log_debug(paste("read from cache", p))
    return(readRDS(p))
  }
  pymod$init()
  x <- pymod$bea_io$get_use(year, level, labels)
  log_debug(paste("save to cache", p))
  saveRDS(x, util$mkdir(p))
  return(x)
}



# IO tables functions ----

beacode2description <- function(code, 
                                year = 2012,
                                ...){
  #Note: year is necessary but arbitrary selection
  bea_year <- util$year2bea(year, ...)
  x <- pubdata$get_sup(bea_year, "sec", FALSE)
  sec <- do.call(rbind, x$col_names)
  x <- pubdata$get_sup(bea_year, "sum", FALSE)
  sum <- do.call(rbind, x$col_names)
  x <- pubdata$get_sup(bea_year, "det", FALSE)
  det <- do.call(rbind, x$col_names)
  x <- rbind(sec, sum, det) %>% as.data.frame()
  colnames(x) <- c("code", "description")
  df <- x["description"][x["code"] == code]
  return(df)
}


###### Call and tidy NAICS to BEA industry concordance table
call_industry_concordance <- function(year = 2012) {
  df <- pubdata$get_naics_concord(year) %>%
    rename_with(str_to_upper) %>%
    filter(NAICS != "n.a.", NAICS != "NaN")
  df <- df %>%
    add_row(SECTOR = "23", 
            SUMMARY = "23", 
            U_SUMMARY = "23", 
            DETAIL = "23", 
            DESCRIPTION = "Construction", 
            NAICS = "23", 
            .before = which(df$SECTOR == '23')[1]) %>%
    filter(NAICS != "23*")
  df <- df %>%
    add_row(SECTOR = "53",
            SUMMARY = "531",
            U_SUMMARY = "531",
            DETAIL = "531",
            DESCRIPTION = "Housing",
            NAICS = "531",
            .before = which(df$SECTOR == '53')[1]) %>%
    filter(DETAIL != "531HST", DETAIL != "531ORE") %>%
    arrange(NAICS)
  
  rownames(df) <- 1:nrow(df)
  b <- c("11", "21", "22", "23", "31G", "31G", "31G", "42", "44RT", "44RT", "48TW", "48TW", "51", "FIRE", "FIRE", "PROF", "PROF", "PROF", "6", "6", "7", "7", "81", "G")
  n <- c("11", "21", "22", "23", "31", "32", "33", "42", "44", "45", "48", "49", "51", "52", "53", "54", "55", "56", "61", "62", "71", "72", "81", "92")
  for(i in 1:length(n)){
    df$SECTOR[substr(df$NAICS, 1,2) %in% n[i]] <- b[i]
  }
  return(df)
}


###### Get specific industry NAICS to BEA concordance
ilevel_concord <- function(ilevel = c("det", "sum", "sec"), year = 2012) {
  ilevel <- match.arg(ilevel)
  x <- call_industry_concordance(year)
  if(ilevel == "det"){
    df <- x %>% select(DETAIL, NAICS)
  }
  if(ilevel == "sum"){
    df <- x %>% select(SUMMARY, NAICS) 
    df$NAICS <- substr(df$NAICS, 1,3)
    # 3-digit NAICS maps to more then one summary for these, we collapse them into one
    df$SUMMARY[df$NAICS == "336"] = "336"
    df$SUMMARY[df$NAICS == "541"] = "541"
    df <- df[!duplicated(df), ]
    rownames(df) <- 1:nrow(df)
  }
  if(ilevel == "sec"){
    df <- x %>% select(SECTOR, NAICS) 
    df$NAICS <- substr(df$NAICS, 1,2)
    df <- df[!duplicated(df), ]
    rownames(df) <- 1:nrow(df)
  }
  return(df)
}


############ Call and clean pubdata BEA IO Use table
call_use_table <- function(year,
                           ilevel = c("det", "sum", "sec"), 
                           ...){
  ilevel <- match.arg(ilevel)
  df <- pubdata$get_use(util$year2bea(year, ilevel), ilevel)$table %>% 
    as.matrix()
  df[is.na(df)] = 0
  return(df)
}

############ Call and clean pubdata BEA IO Supply table
call_supply_table <- function(year,
                              ilevel = c("det", "sum", "sec"), 
                              ...){
  ilevel <- match.arg(ilevel)
  df <- pubdata$get_sup(util$year2bea(year, ilevel), ilevel)$table %>% 
    as.matrix()
  df[is.na(df)] = 0
  return(df)
}


### Aggregate and tidy a commodity-by-industry BEA matrix
condense_bea_matrix <- function(matrix,
                                ilevel){
  if(ilevel == "det"){
    df <- matrix %>% 
      util$matrix_collapse(., grep("^23", colnames(.), value = TRUE), "23") %>% 
      util$matrix_collapse(., grep("^531", colnames(.), value = TRUE), "531") %>% 
      .[!grepl("4200ID|S00402|S00300", rownames(.)), !grepl("4200ID", colnames(.)), drop=F]
  }
  if(ilevel == "sum"){ 
    df <- matrix %>% 
      util$matrix_collapse(., grep("^336", colnames(.), value = TRUE), "336") %>% 
      util$matrix_collapse(., grep("^541", colnames(.), value = TRUE), "541") %>% 
      util$matrix_collapse(., grep("^(HS|ORE)", colnames(.), value = TRUE), "531")
  }
  if (ilevel == "sec"){
    df <- matrix
  }
  return(df)
}

### Aggregate and tidy a BEA row-vector
condense_bea_vector <- function(vector,
                                ilevel){
  if(ilevel == "det"){
    df <- vector %>% 
      util$vector_collapse(., grep("^23", colnames(.), value = TRUE), "23") %>% 
      util$vector_collapse(., grep("^531", colnames(.), value = TRUE), "531") %>% 
      .[,!grepl("4200ID|S00402|S00300", colnames(.)), drop=F]
  }
  if(ilevel == "sum"){ 
    df <- vector %>% 
      util$vector_collapse(., grep("^336", colnames(.), value = TRUE), "336") %>% 
      util$vector_collapse(., grep("^541", colnames(.), value = TRUE), "541") %>% 
      util$vector_collapse(., grep("^(HS|ORE)", colnames(.), value = TRUE), "531")
  }
  if (ilevel == "sec"){
    df <- vector
  }
  return(df)
}

#needs more adjustments so that all consolidating for CBP is done beforehand (e.g., government)?
### Get Use matrix and tidy structure for use with NAICS adjacent processes
use_matrix <- function(year,
                       ilevel = c("det", "sum", "sec"),
                       ...){
  ilevel <- match.arg(ilevel)
  df <- call_use_table(year, ilevel, ...) %>% 
    .[1:(which(rownames(.) == "T005")-1), 1:(which(colnames(.) == "T001")-1)] %>% 
    condense_bea_matrix(., ilevel)
  return(df)
}
### Get Supply matrix and tidy structure for use with NAICS adjacent processes
supply_matrix <- function(year,
                          ilevel = c("det", "sum", "sec"),
                          ...){
  ilevel <- match.arg(ilevel)
  df <- call_supply_table(year, ilevel, ...) %>% 
    .[1:(nrow(.)-1), 1:(which(colnames(.) == "T007")-1)] %>% 
    condense_bea_matrix(., ilevel)
  return(df)
}

### Get National BEA Total Industry Output Vector and tidy structure for use with NAICS adjacent processes
industry_output <- function(year,
                            ilevel = c("det", "sum", "sec"),
                            ...){
  ilevel <- match.arg(ilevel)
  df <- call_supply_table(year, ilevel, ...) %>% 
    .[nrow(.), 1:(which(colnames(.) == "T007")-1), drop=F] %>% 
    condense_bea_vector(., ilevel) %>% 
    t() %>% 
    `colnames<-`("T017")
  return(df)
}
### Get National BEA Total Commodity Output Vector and tidy structure for use with NAICS adjacent processes
commodity_output <- function(year,
                             ilevel = c("det", "sum", "sec"),
                             ...){
  ilevel <- match.arg(ilevel)
  df <- call_supply_table(year, ilevel, ...) %>% 
    .[1:(nrow(.)-1), "T007", drop=F] %>% 
    t() %>% 
    condense_bea_vector(., ilevel) %>% 
    t() %>% 
    `colnames<-`("T007")
  return(df)
}

#### Commodities-by-Industries parallel to ordinary technical coefficients matrix 
b_matrix <- function(year,
                     ilevel = c("det", "sum", "sec"),
                     ...){
  ilevel <- match.arg(ilevel)
  u_mat <- use_matrix(year, ilevel, ...)
  x <- industry_output(year, ilevel, ...)
  df <- u_mat %*% diag(1/as.vector(x))
  colnames(df) <- colnames(u_mat)
  return(df)
}

####Commodity Composition of Industry Outputs
c_matrix <- function(year,
                     ilevel = c("det", "sum", "sec"),
                     ...){
  ilevel <- match.arg(ilevel)
  supply_mat <- supply_matrix(year, ilevel, ...)
  x <- industry_output(year, ilevel, ...)
  df <- supply_mat %*% diag(1/as.vector(x))
  colnames(df) <- colnames(supply_mat)
  return(df)
}

#### Industry Source of Commodity Outputs
d_matrix <- function(year,
                     ilevel = c("det", "sum", "sec"),
                     ...){
  ilevel <- match.arg(ilevel)
  supply_mat <- supply_matrix(year, ilevel, ...)
  q <- commodity_output(year, ilevel, ...)
  df <- t(supply_mat) %*% diag(1/as.vector(q))
  colnames(df) <- rownames(supply_mat)
  return(df)
}

# Toys ----
toy <- new.env()

#' Create a toy use table from given data or randomly generated
#' @param core commodity-by-industry core matrix of the use table
#' @param va payroll + taxes + surplus vector of the use table
#' @param final consumption + export + gov expenses vector of the use table
#'
#' @example toy$use_table(matrix(1:9, 3, 3), 0:2, 1:3)
toy$use_table <- function(core, va = NA, final = NA) {
  nc <- nrow(core)
  ni <- ncol(core)
  
  if (all(is.na(core))) {
    core <- matrix(runif(nc * ni), nc, ni)
    core[runif(nc * ni) < 0.5] <- 0
  }
  if (all(is.na(va))) {
    va <- runif(ni)
  }
  if (all(is.na(final))) {
    final <- runif(nc)
  }
  
  c_names <- if (is.null(rownames(core))) paste0("c_", 1:nc) else rownames(core)
  i_names <- if (is.null(colnames(core))) paste0("i_", 1:ni) else colnames(core)
  
  
  t <- matrix(NA, nc + 3, ni + 3,
              dimnames = list(
                commodity = c(paste0("c_", 1:nc), "i_tot_use", "val_add", "i_tot_out"),
                industry = c(paste0("i_", 1:ni), "c_int_use", "final_cons", "c_tot_use")
              ))
  t[1:nc, 1:ni] <- core
  t[1:nc, "c_int_use"] <- rowSums(core)
  t[1:nc, "final_cons"] <- final
  t[1:nc, "c_tot_use"] <- t[1:nc, "c_int_use"] + final
  t["i_tot_use", ] <- colSums(t[1:nc, ])
  t["val_add", 1:ni] <- va
  t["i_tot_out", 1:ni] <- t["i_tot_use", 1:ni] + va
  t[c("val_add", "i_tot_out"), "c_int_use"] <- rowSums(t[c("val_add", "i_tot_out"), 1:ni])
  return(t) 
}


#' Create a toy supply table from given data or randomly generated
#' @param core commodity-by-industry core matrix of the supply table
#' @param wedge imports + margins + taxes vector of the supply table
#'
#' @example toy$sup_table(matrix(1:9, 3, 3), 0:2)
toy$sup_table <- function(core, wedge = NA) {
  nc <- nrow(core)
  ni <- ncol(core)
  
  if (all(is.na(core))) {
    core[1:nc, 1:ni] <- runif(nc * ni)
    core[runif(nc * ni) < 0.5] <- 0
  }
  if (all(is.na(wedge))) {
    wedge[1:nc] <- runif(nc)
  }

  c_names <- if (is.null(rownames(core))) paste0("c_", 1:nc) else rownames(core)
  i_names <- if (is.null(colnames(core))) paste0("i_", 1:ni) else colnames(core)
  t <- matrix(NA, nc + 1, ni + 3,
              dimnames = list(
                commodity = c(c_names, "i_tot_out"),
                industry = c(i_names, "c_tot_out", "wedge", "c_tot_sup")
              ))
  t[1:nc, 1:ni] <- core
  t[1:nc, "c_tot_out"] <- rowSums(core)
  t[1:nc, "wedge"] <- wedge
  t[1:nc, "c_tot_sup"] <- t[1:nc, "c_tot_out"] + wedge
  t["i_tot_out", ] <- colSums(t[1:nc, ])
  return(t) 
}


#### Commodities-by-Industries parallel to ordinary technical coefficients matrix 
b_matrix <- function(year,
                     ilevel = c("det", "sum", "sec"),
                     ...){
  ilevel <- match.arg(ilevel)
  u_mat <- use_matrix(year, ilevel, ...)
  x <- industry_output(year, ilevel, ...)
  df <- u_mat %*% diag(1/as.vector(x))
  colnames(df) <- colnames(u_mat)
  return(df)
}

#' Commodities-by-Industries parallel to ordinary technical coefficients matrix
toy$b_matrix <- function(use_table) {
  nc <- nrow(use_table) - 3
  ni <- ncol(use_table) - 3
  ind_out <- use_table["i_tot_out", 1:ni]
  m <- use_table[1:nc, 1:ni] %*% diag(1 / ind_out)
  colnames(m) <- colnames(use_table)[1:ni]
  return(m)
}

#' Commodity Composition of Industry Outputs
toy$c_matrix <- function(sup_table) {
  nc <- nrow(sup_table) - 1
  ni <- ncol(sup_table) - 3
  ind_out <- sup_table["i_tot_out", 1:ni]
  m <- sup_table[1:nc, 1:ni] %*% diag(1 / ind_out)
  m <- matrix(m, nc, ni, dimnames = list(
    commodity = rownames(sup_table)[1:nc],
    industry = colnames(sup_table)[1:ni]))
  return(m)
}

#' Industry Source of Commodity Outputs
toy$d_matrix <- function(sup_table) {
  nc <- nrow(sup_table) - 1
  ni <- ncol(sup_table) - 3
  com_out <- sup_table[1:nc, "c_tot_out"]
  m <- t(sup_table[1:nc, 1:ni]) %*% diag(1 / com_out)
  m <- matrix(m, nc, ni, dimnames = list(
    industry = colnames(sup_table)[1:ni],
    commodity = rownames(sup_table)[1:nc]))
  return(m)
}





# Tests ----

test_pubdata <- function() {
  for (year in c(2012, 2017)) {
    pubdata$get_naics_concord(year)
  }
  for (year in 1997:2022) {
    log_debug(year)
    for (level in c("sec", "sum", "det")) {
      if (level == "det" && !(year %in% c(2007, 2012, 2017))) next
      for (labels in c(FALSE, TRUE)) {
        pubdata$get_sup(year, level, labels)
        pubdata$get_use(year, level, labels)
      }
    }
  }
}

test_dataprep <- function() {

  beacode2description("532100")
  beacode2description("T016")
  call_industry_concordance(2012)
  call_industry_concordance(2017)
  for (year in c(2012, 2017)) {
    for (ilevel in c("det", "sum", "sec")) {
      ilevel_concord(ilevel, year)
    }
  }
  
  for (year in 1997:2022) {
    for (ilevel in c("det", "sum", "sec")) {
      if (ilevel == "det" && !(year %in% c(2007, 2012, 2017))) next
      call_use_table(year, ilevel)
      call_supply_table(year, ilevel)
      use_matrix(year, ilevel)
      supply_matrix(year, ilevel)
      industry_output(year, ilevel)
      commodity_output(year, ilevel)
      b_matrix(year, ilevel)
      c_matrix(year, ilevel)
      d_matrix(year, ilevel)
    }
  }
}

