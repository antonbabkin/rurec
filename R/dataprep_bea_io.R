# Data preparation of BEA I-O tables

library(logger)
log_threshold(DEBUG)
library(arrow)
library(tidyverse)
library(glue)

source("R/basic_utilities.R")
source("R/pydata.R", local = (pydata <- new.env()))


beacode2description <- function(code, 
                                year = 2012,
                                ...){
  #Note: year is necessary but arbitrary selection
  bea_year <- year2bea(year, ...)
  x <- pydata$bea_io$get_sup(bea_year, "sec", FALSE)
  sec <- do.call(rbind, x$col_names)
  x <- pydata$bea_io$get_sup(bea_year, "sum", FALSE)
  sum <- do.call(rbind, x$col_names)
  x <- pydata$bea_io$get_sup(bea_year, "det", FALSE)
  det <- do.call(rbind, x$col_names)
  x <- rbind(sec, sum, det) %>% as.data.frame()
  colnames(x) <- c("code", "description")
  df <- x["description"][x["code"] == code]
  return(df)
}


###### Call and tidy NAICS to BEA industry concordance table
call_industry_concordance <- function(year = 2012) {
  df <- pydata$bea_io$get_naics_concord(year) %>%
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

df <- call_industry_concordance(2012)

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
  df <- pydata$bea_io$get_use(year2bea(year, ilevel), ilevel)$table %>% 
    as.matrix()
  df[is.na(df)] = 0
  return(df)
}

############ Call and clean pubdata BEA IO Supply table
call_supply_table <- function(year,
                              ilevel = c("det", "sum", "sec"), 
                              ...){
  ilevel <- match.arg(ilevel)
  df <- pydata$bea_io$get_sup(year2bea(year, ilevel), ilevel)$table %>% 
    as.matrix()
  df[is.na(df)] = 0
  return(df)
}


### Aggregate and tidy a commodity-by-industry BEA matrix
condense_bea_matrix <- function(matrix,
                                ilevel){
  if(ilevel == "det"){
    df <- matrix %>% 
      matrix_collapse(., grep("^23", colnames(.), value = TRUE), "23") %>% 
      matrix_collapse(., grep("^531", colnames(.), value = TRUE), "531") %>% 
      .[!grepl("4200ID|S00402|S00300", rownames(.)), !grepl("4200ID", colnames(.)), drop=F]
  }
  if(ilevel == "sum"){ 
    df <- matrix %>% 
      matrix_collapse(., grep("^336", colnames(.), value = TRUE), "336") %>% 
      matrix_collapse(., grep("^541", colnames(.), value = TRUE), "541") %>% 
      matrix_collapse(., grep("^(HS|ORE)", colnames(.), value = TRUE), "531")
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
      vector_collapse(., grep("^23", colnames(.), value = TRUE), "23") %>% 
      vector_collapse(., grep("^531", colnames(.), value = TRUE), "531") %>% 
      .[,!grepl("4200ID|S00402|S00300", colnames(.)), drop=F]
  }
  if(ilevel == "sum"){ 
    df <- vector %>% 
      vector_collapse(., grep("^336", colnames(.), value = TRUE), "336") %>% 
      vector_collapse(., grep("^541", colnames(.), value = TRUE), "541") %>% 
      vector_collapse(., grep("^(HS|ORE)", colnames(.), value = TRUE), "531")
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


test_dataprep_bea_io <- function() {
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

