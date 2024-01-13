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
  use_ = "data/bea_io/use/{level}/{year}_{labels}.rds",
  ixi_ = "data/bea_io/ixi/{level}/{year}_{labels}.rds",
  ixc_ = "data/bea_io/ixc/{level}/{year}_{labels}.rds",
  cxc_ = "data/bea_io/cxc/{level}/{year}_{labels}.rds"
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

pubdata$get_ixi <- function(year, level, labels = FALSE) {
  match.arg(level, c("det", "sum", "sec"))
  year <- as.integer(year)
  p <- glue(opath$ixi_)
  if (file.exists(p)) {
    log_debug(paste("read from cache", p))
    return(readRDS(p))
  }
  pymod$init()
  x <- pymod$bea_io$get_ixi(year, level, labels)
  log_debug(paste("save to cache", p))
  saveRDS(x, util$mkdir(p))
  return(x)
}

pubdata$get_ixc <- function(year, level, labels = FALSE) {
  match.arg(level, c("det", "sum", "sec"))
  year <- as.integer(year)
  p <- glue(opath$ixc_)
  if (file.exists(p)) {
    log_debug(paste("read from cache", p))
    return(readRDS(p))
  }
  pymod$init()
  x <- pymod$bea_io$get_ixc(year, level, labels)
  log_debug(paste("save to cache", p))
  saveRDS(x, util$mkdir(p))
  return(x)
}

pubdata$get_cxc <- function(year, level, labels = FALSE) {
  match.arg(level, c("det", "sum", "sec"))
  year <- as.integer(year)
  p <- glue(opath$cxc_)
  if (file.exists(p)) {
    log_debug(paste("read from cache", p))
    return(readRDS(p))
  }
  pymod$init()
  x <- pymod$bea_io$get_cxc(year, level, labels)
  log_debug(paste("save to cache", p))
  saveRDS(x, util$mkdir(p))
  return(x)
}

# IO code/concordance ----

#TODO: check if unnecessaryredundant with new table/label dataframe scheme
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
ilevel_concord <- function(ilevel = c("det", "sum", "sec"), 
                           year = 2012) {
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


### Aggregate and tidy a commodity-by-industry BEA matrix
condense_bea_matrix <- function(matrix,
                                ilevel= c("det", "sum", "sec")){
  ilevel <- match.arg(ilevel)
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
                                ilevel= c("det", "sum", "sec")){
  ilevel <- match.arg(ilevel)
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


# IO functions core----

industry_output <- function(io_supply_matrix){
  df <- io_supply_matrix %>% 
    colSums() %>% 
    as.matrix() %>% 
    `colnames<-`("T017")
  return(df)
}

commodity_output <- function(io_supply_matrix){
  df <- io_supply_matrix %>% 
    rowSums() %>% 
    as.matrix() %>% 
    `colnames<-`("T007")
  return(df)
}

b_matrix <- function(io_use_matrix, 
                     io_supply_matrix){
  x <- industry_output(io_supply_matrix)
  inames <- rownames(x)[x > 0]
  df <- io_use_matrix %*% diag(1/as.vector(x))
  colnames(df) <- colnames(io_supply_matrix)
  df <- df[, inames, drop=F]
  return(df)
}

c_matrix <- function(io_supply_matrix){
  x <- industry_output(io_supply_matrix)
  inames <- rownames(x)[x > 0]
  df <- io_supply_matrix %*% diag(1/as.vector(x))
  colnames(df) <- colnames(io_supply_matrix)
  df <- df[, inames, drop=F]
  return(df)
}

d_matrix <- function(io_supply_matrix){
  x <- commodity_output(io_supply_matrix)
  cnames <- rownames(x)[x > 0]
  df <- t(io_supply_matrix) %*% diag(1/as.vector(x))
  colnames(df) <- rownames(io_supply_matrix)
  df <- df[, cnames, drop=F]
  return(df)
}

# technical coefficients
direct_requirements <- function(io_use_matrix,
                                io_supply_matrix,
                                technology = c("industry", "commodity"),
                                dimensions = c("industry", "commodity")){
  technology <- match.arg(technology)
  dimensions <- match.arg(dimensions)
  if(technology == "industry" & dimensions == "commodity"){
    bmat <- b_matrix(io_use_matrix, io_supply_matrix)
    dmat <- d_matrix(io_supply_matrix)
    inames <- intersect(colnames(bmat), rownames(dmat))
    cnames <- intersect(rownames(bmat), colnames(dmat))
    df <- bmat[cnames, inames] %*% dmat[inames, cnames]
  }
  if(technology == "industry" & dimensions == "industry"){
    bmat <- b_matrix(io_use_matrix, io_supply_matrix)
    dmat <- d_matrix(io_supply_matrix)
    inames <- intersect(colnames(bmat), rownames(dmat))
    cnames <- intersect(rownames(bmat), colnames(dmat))
    df <- dmat[inames, cnames] %*% bmat[cnames, inames]
  }
  if(technology == "commodity" & dimensions == "commodity"){
    bmat <- b_matrix(io_use_matrix, io_supply_matrix)
    cmat <- c_matrix(io_supply_matrix)
    inames <- intersect(colnames(bmat), colnames(cmat))
    cnames <- intersect(rownames(bmat), rownames(cmat))
    df <- bmat[cnames, inames] %*% solve(cmat[cnames, inames])
  }
  if(technology == "commodity" & dimensions == "industry"){
    bmat <- b_matrix(io_use_matrix, io_supply_matrix)
    cmat <- c_matrix(io_supply_matrix)
    inames <- intersect(colnames(bmat), colnames(cmat))
    cnames <- intersect(rownames(bmat), rownames(cmat))
    df <- solve(cmat[cnames, inames]) %*% bmat[cnames, inames]
  }
  return(df)
}

# Leontief inverse
total_requirements <- function(io_use_matrix,
                               io_supply_matrix,
                               technology = c("industry", "commodity"),
                               dimensions = c("industry", "commodity")){
  technology <- match.arg(technology)
  dimensions <- match.arg(dimensions)
  dr <- direct_requirements(io_use_matrix, io_supply_matrix, technology, dimensions)
  df <- solve(diag(ncol(dr)) - dr)
  return(df)
}

# IO functions extension----


# domestic production shares of total national commodity supply
production_shares <- function(commodity_output_vector,
                              commodity_supply_vector){
  df <- commodity_output_vector / commodity_supply_vector 
  colnames(df) <- "production_share"
  rownames(df) <- rownames(commodity_supply_vector) 
  return(df)
}

# domestic intermediate commodity use shares of total national commodity supply
commodity_use_shares <- function(intermediate_commodity_use_vector,
                                 commodity_supply_vector){
  df <- intermediate_commodity_use_vector / commodity_supply_vector 
  colnames(df) <- "com_use_share"
  rownames(df) <- rownames(commodity_supply_vector) 
  return(df)
}

# domestic intermediate commodity use shares of total national commodity supply
industry_use_shares <- function(intermediate_industry_use_vector,
                                industry_output_vector){
  df <- intermediate_industry_use_vector / industry_output_vector 
  colnames(df) <- "ind_use_share"
  rownames(df) <- rownames(industry_output_vector) 
  return(df)
}

# formula to recover commodity supply from commodity output and Phi
commodity_supply <- function(commodity_output_vector,
                             phi){
  df <- commodity_output_vector / phi 
  return(df)
}

# toy artificial output by place diagnostic matrix for n randomly divided arbitrary "places"
out_by_place_diag <- function(output_vector,
                              place_count = 5){
  ov <- output_vector
  df <- ov %>% {(diag(as.vector(.)) %*% t(replicate(length(.), diff(c(0, sort(runif(place_count-1)), 1)))) ) } %>% 
    `rownames<-`(rownames(ov)) 
  return(df)
}


# IO tables ----

############ Call and clean pubdata BEA IO Use table
call_bea_use_table <- function(year,
                               ilevel = c("det", "sum", "sec")){
  ilevel <- match.arg(ilevel)
  df <- util$year2bea(year, ilevel) %>% 
    {pubdata$get_use(., ilevel)$table} %>% 
    as.matrix()
  df[is.na(df)] = 0
  return(df)
}

############ Call and clean pubdata BEA IO Supply table
call_bea_supply_table <- function(year,
                                  ilevel = c("det", "sum", "sec")){
  ilevel <- match.arg(ilevel)
  df <- util$year2bea(year, ilevel) %>% 
    {pubdata$get_sup(., ilevel)$table} %>% 
    as.matrix()
  df[is.na(df)] = 0
  return(df)
}

############ Call and clean BEA IO total requirements
call_bea_total_req <- function(year,
                                ilevel = c("det", "sum", "sec"),
                                dim_class = c("ixi", "ixc", "cxc")){
  ilevel <- match.arg(ilevel)
  dim_class <- match.arg(dim_class)
  y <- util$year2bea(year, ilevel) 
  if(dim_class == "ixi"){df <- pubdata$get_ixi(y, ilevel)$table} 
  if(dim_class == "ixc"){df <- pubdata$get_ixc(y, ilevel)$table} 
  if(dim_class == "cxc"){df <- pubdata$get_cxc(y, ilevel)$table} 
  df <- df %>% 
    as.matrix() %>% 
    {.[-nrow(.),]}
  df[is.na(df)] = 0
  return(df)
}

### Get Use matrix and tidy structure for use with NAICS adjacent processes
call_use_matrix <- function(year,
                             ilevel = c("det", "sum", "sec"),
                             condense = TRUE){
  df <- call_bea_use_table(year, ilevel) %>% 
    .[1:(which(rownames(.) == "T005")-1), 1:(which(colnames(.) == "T001")-1)] 
  if(condense){
    df <- condense_bea_matrix(df, ilevel)
  }
  return(df)
}

### Get Supply matrix and tidy structure for use with NAICS adjacent processes
call_supply_matrix <- function(year,
                                ilevel = c("det", "sum", "sec"),
                                condense = TRUE){
  df <- call_bea_supply_table(year, ilevel) %>% 
    .[1:(nrow(.)-1), 1:(which(colnames(.) == "T007")-1)]   
  if(condense){
      df <- condense_bea_matrix(df, ilevel)
    }
  return(df)
}

### Get National BEA Total Industry Output Vector and tidy structure for use with NAICS adjacent processes
call_industry_output <- function(year,
                                  ilevel = c("det", "sum", "sec"),
                                  condense = TRUE){
  df <- call_supply_matrix(year, ilevel, condense) %>% 
    industry_output()
  return(df)
}

### Get National BEA Total Commodity Output Vector and tidy structure for use with NAICS adjacent processes
call_commodity_output <- function(year,
                                   ilevel = c("det", "sum", "sec"),
                                   condense = TRUE){
  df <- call_supply_matrix(year, ilevel, condense) %>% 
    commodity_output()
  return(df)
}

### Get National BEA Total Commodity Product Supply Vector and tidy structure for use with NAICS adjacent processes
# TODO has not been tested
call_commodity_supply <- function(year,
                                  ilevel = c("det", "sum", "sec"),
                                  condense = TRUE){
  df <- call_bea_supply_table(year, ilevel) %>% 
    .[1:(nrow(.)-1), "T016", drop=F]
  if(condense){
    df <- df %>% 
      t() %>% 
      condense_bea_vector(., ilevel) %>% 
      t() 
  }
  return(df)
}

#### Commodities-by-Industries parallel to ordinary technical coefficients matrix 
call_b_matrix <- function(year,
                           ilevel = c("det", "sum", "sec"),
                           condense = TRUE){
  u_mat <- call_use_matrix(year, ilevel, condense)
  s_mat <- call_supply_matrix(year, ilevel, condense)
  df <- b_matrix(u_mat, s_mat)
  return(df)
}

####Commodity Composition of Industry Outputs
call_c_matrix <- function(year,
                           ilevel = c("det", "sum", "sec"),
                           condense = TRUE){
  df <- call_supply_matrix(year, ilevel, condense) %>% 
    c_matrix()
  return(df)
}

#### Industry Source of Commodity Outputs
call_d_matrix <- function(year,
                           ilevel = c("det", "sum", "sec"),
                           condense = TRUE){
  df <- call_supply_matrix(year, ilevel, condense) %>% 
    d_matrix()
  return(df)
}

call_direct_requirements <- function(year,
                                     ilevel = c("det", "sum", "sec"),
                                     condense = TRUE,
                                     technology = c("industry", "commodity"),
                                     dimensions = c("industry", "commodity")){
  u_mat <- call_use_matrix(year, ilevel, condense)
  s_mat <- call_supply_matrix(year, ilevel, condense)
  df <- direct_requirements(u_mat, s_mat, technology, dimensions)
  return(df)
}

call_total_requirements <- function(year,
                                     ilevel = c("det", "sum", "sec"),
                                     condense = TRUE,
                                     technology = c("industry", "commodity"),
                                     dimensions = c("industry", "commodity")){
  u_mat <- call_use_matrix(year, ilevel, condense)
  s_mat <- call_supply_matrix(year, ilevel, condense)
  df <- total_requirements(u_mat, s_mat, technology, dimensions)
  return(df)
}



# IO tables extension----

### Get total commodity output's share of total product supply: Phi
# TODO has not been tested
call_commodity_share_factor <- function(year,
                                   ilevel = c("det", "sum", "sec"),
                                   condense = TRUE){
  df <- call_commodity_output(year, ilevel, condense)/call_commodity_supply(year, ilevel, condense)
  return(df)
}

# call national domestic production shares of total national commodity supply
call_production_shares <- function(year,
                              ilevel = c("det", "sum", "sec"),
                              condense = TRUE){
  x <- call_industry_output(year, ilevel, condense)
  cmat <- call_c_matrix(year, ilevel, condense)
  cs <- call_commodity_supply(year, ilevel, condense)
  co <- (cmat %*% x)
  #cs <- ((cmat %*% x) + (cs-(cmat %*% x)))
  df <- production_shares(commodity_output_vector = co,
                          commodity_supply_vector = cs)
  return(df)
}

# call national domestic intermediate commodity use shares of total national commodity supply
call_commodity_use_shares <- function(year,
                                 ilevel = c("det", "sum", "sec"),
                                 condense = TRUE){
  x <- call_industry_output(year, ilevel, condense)
  bmat <- call_b_matrix(year, ilevel, condense)
  cs <- call_commodity_supply(year, ilevel, condense)
  icu <- (bmat %*% x)
  #cs <- ((bmat %*% x) + (cs - (bmat %*% x)))
  df <- commodity_use_shares(intermediate_commodity_use_vector = icu,
                             commodity_supply_vector = cs)
  return(df)
}

# call national domestic intermediate industry use shares of total national industry use
call_industry_use_shares <- function(year,
                                ilevel = c("det", "sum", "sec"),
                                condense = TRUE){
  x <- call_industry_output(year, ilevel, condense)
  bmat <- call_b_matrix(year, ilevel, condense)
  iiu <- (diag(as.vector(colSums(bmat))) %*% x)
  df <- industry_use_shares(intermediate_industry_use_vector = iiu,
                            industry_output_vector = x)
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
        pubdata$get_ixi(year, level, labels)
        pubdata$get_ixc(year, level, labels)
        pubdata$get_cxc(year, level, labels)
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
      call_bea_use_table(year, ilevel)
      call_bea_supply_table(year, ilevel)
      call_use_matrix(year, ilevel)
      call_supply_matrix(year, ilevel)
      call_industry_output(year, ilevel)
      call_commodity_output(year, ilevel)
      call_b_matrix(year, ilevel)
      call_c_matrix(year, ilevel)
      call_d_matrix(year, ilevel)
    }
  }
}

