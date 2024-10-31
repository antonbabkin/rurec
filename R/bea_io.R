# Data preparation of BEA I-O tables - new pubdata edition

# R libraries ----
library(logger)
library(arrow)
library(tidyverse)
library(glue)


# R scripts ----
source("R/basic_utilities.R", local = (util <- new.env()))



# Data and cache ----
ipath <- list(
  naics_code_ = "data/pubdatapy/naics/code/{year}.pq"
)

opath <- list(
  concordance = "data/bea_io/concordance.pq",
  use_table_ = "data/bea_io/use_table/{year}_{ilevel}.pq",
  supply_table_ = "data/bea_io/supply_table/{year}_{ilevel}.pq"
)

# cache management
cache <- new.env()
with(cache, {
  # use cache when calling functions
  enabled <- TRUE
  
  # create cache files by calling all functions that save to cache
  build <- function() {
    concordance()
    for (year in c(2012)) {
      for (ilevel in c("sec", "sum", "det")) {
        use_table(year, ilevel)
        supply_table(year, ilevel)
      }
    }
  }
  
  # remove all cache files
  clear <- function() {
    util$clear_paths(opath)
  }
  
  # pack all cache files to zip archive
  pack <- function(path, overwrite = FALSE) {
    util$zip_pack(path, files = opath, overwrite = overwrite)  
  }
  
  # pack all cache files to zip archive
  unpack <- function(path, overwrite = FALSE) {
    util$zip_unpack(path, overwrite = overwrite)  
  }
  
})


# Parameters ----

#' Industry codes that are combined together
ind_code_comb <- tribble(
  ~ilevel, ~code, ~new_code, ~new_title,
  "sector", "33DG", "31G", "Manufacturing",
  "sector", "31ND", "31G", "Manufacturing",
  "sector", "52", "FIRE", "Finance, insurance, real estate, rental, and leasing",
  "sector", "53", "FIRE", "Finance, insurance, real estate, rental, and leasing",
  "sector", "54", "PROF", "Professional and business services",
  "sector", "55", "PROF", "Professional and business services",
  "sector", "56", "PROF", "Professional and business services",
  "sector", "61", "6", "Educational services, health care, and social assistance",
  "sector", "62", "6", "Educational services, health care, and social assistance",
  "sector", "71", "7", "Arts, entertainment, recreation, accommodation, and food services",
  "sector", "72", "7", "Arts, entertainment, recreation, accommodation, and food services",
  "u_summary", "23EH", "23", "Construction",
  "u_summary", "23OC", "23", "Construction",
  "u_summary", "23PC", "23", "Construction",
  "u_summary", "23TH", "23", "Construction",
  "u_summary", "23OT", "23", "Construction",
  "u_summary", "23SF", "23", "Construction",
  "u_summary", "23OR", "23", "Construction",
  "u_summary", "23MR", "23", "Construction",
  "detail", "233210", "23", "Construction",
  "detail", "233262", "23", "Construction",
  "detail", "2332A0", "23", "Construction",
  "detail", "233240", "23", "Construction",
  "detail", "2332C0", "23", "Construction",
  "detail", "233230", "23", "Construction",
  "detail", "2332D0", "23", "Construction",
  "detail", "233411", "23", "Construction",
  "detail", "233412", "23", "Construction",
  "detail", "2334A0", "23", "Construction",
  "detail", "230301", "23", "Construction",
  "detail", "230302", "23", "Construction",
  "summary", "HS", "531", "Owner-occupied housing, tenant-occupied housing, and other real estate",
  "summary", "ORE", "531", "Owner-occupied housing, tenant-occupied housing, and other real estate",
  "u_summary", "HSO", "531", "Owner-occupied housing, tenant-occupied housing, and other real estate",
  "u_summary", "HST", "531", "Owner-occupied housing, tenant-occupied housing, and other real estate",
  "u_summary", "ORE", "531", "Owner-occupied housing, tenant-occupied housing, and other real estate",
  "detail", "531HSO", "531", "Owner-occupied housing, tenant-occupied housing, and other real estate",
  "detail", "531HST", "531", "Owner-occupied housing, tenant-occupied housing, and other real estate",
  "detail", "531ORE", "531", "Owner-occupied housing, tenant-occupied housing, and other real estate"
)



# Helper functions ----


# Concordance ----

#' concordance between BEA industry levels and NAICS codes
concordance <- function() {
  bea_rev <- 2022
  naics_rev <- 2012
  
  cache_path <- glue(opath$concordance)
  if (cache$enabled & file.exists(cache_path)) {
    log_debug(paste("read from cache", cache_path))
    return(read_parquet(cache_path))
  }
  
  
  x0 <- pubdata::bea_io_get(glue("{bea_rev}_naics"))
  
  # de-uppercase sector titles and add column that indicates industry level
  x1 <- x0 %>%
    mutate(title = str_to_sentence(title)) %>%
    mutate(
      ilevel = case_when(
        is.na(summary) ~ "sector",
        is.na(u_summary) ~ "summary",
        is.na(detail) ~ "u_summary",
        .default = "detail"
      ),
      .before = 1
    )
  
  # combine codes in accordance with specification in "ind_code_comb" table
  x2 <- x1
  for (ilv in unique(ind_code_comb$ilevel)) {
    comb <- ind_code_comb %>%
      filter(ilevel == ilv) %>%
      select(code, new_code, new_title)
    x2$code <- x2[[ilv]]
    x2 <- left_join(x2, comb, by = "code")
    x2[[ilv]] <- with(x2, if_else(is.na(new_code), code, new_code))
    x2$title <- with(x2, if_else(ilevel == ilv & !is.na(new_code), new_title, title))
    x2 <- select(x2, !c(code, new_code, new_title))
  }
  # drop duplicates created through combination of codes
  # special treatment for "531": 531HSO and 531HST are combined, but 531HSO has no NAICS code
  x2 <- x2 %>%
    distinct() %>%
    filter(is.na(detail) | detail != "531" | !is.na(naics))
  
  # all NAICS observations must be unique
  stopifnot(!(x2$naics %>% na.omit() %>% duplicated() %>% any()))
  
  # expand rows with 6-digit NAICS codes
  df_naics <- arrow::read_parquet(glue(ipath$naics_code_, year = naics_rev)) %>%
    filter(digits == 6)
  x3 <- mutate(x2, naics6 = if_else(str_length(naics) == 6, naics, NA))
  for (digits in 2:5) {
    merge_col <- paste0("code_", digits)
    x3[[merge_col]] <- with(x3, if_else(str_length(naics) == digits, naics, NA))
    x3 <- x3 %>%
      left_join(df_naics[c(merge_col, "code_6")], by = merge_col) %>%
      mutate(naics6 = if_else(is.na(code_6), naics6, code_6)) %>%
      select(!code_6)
  }
  # this process reveals invalid naics codes, created during list expansion in pubdata
  # until pubdata fixes this, here is a solution
  # remove rows where naics6 was not created: naics was not found in lookup table, so it is not valid
  # also remove temporary columns
  x3 <- x3 %>%
    filter(is.na(naics) | !is.na(naics6)) %>%
    select(!c(code_2, code_3, code_4, code_5))
  
  # tests
  naics_bea <- x3$naics6 %>% na.omit() %>% as.character()
  stopifnot(!any(duplicated(naics_bea)))
  # NAICS is fully covered at 6-digit level with exception of "92" (Public Administration)
  stopifnot(
    base::setdiff(df_naics$code_6, naics_bea) %>%
      str_detect("^92....$") %>%
      all()
  )
  
  # industry/commodity indicators
  # every code is both commodity and industry with a few exceptions
  x4 <- x3 %>%
    mutate(
      industry = case_when(
        sector %in% c("Used", "Other") ~ FALSE,
        .default = TRUE
      ),
      commodity = case_when(
        detail %in% c("331314", "S00101", "S00201", "S00202") ~ FALSE,
        .default = TRUE
      )
    )
  
  if (cache$enabled) {
    log_debug(paste("save to cache", cache_path))
    write_parquet(x4, util$mkdir(cache_path))
  }
  
  x4
}


# Tables ----

use_table <- function(year, ilevel = c("det", "sum", "sec")) {
  ilevel <- match.arg(ilevel)
  ilevel_long <- switch(ilevel, det = "detail", sum = "summary", sec = "sector")
  bea_rev <- 2022
  
  cache_path <- glue(opath$use_table_)
  if (cache$enabled & file.exists(cache_path)) {
    log_debug(paste("read from cache", cache_path))
    return(read_parquet(cache_path))
  }
  
  df_comb <- ind_code_comb %>%
    filter(ilevel == ilevel_long) %>%
    select(code, new_code, new_title)
  df_tab <- pubdata::bea_io_get(glue("{bea_rev}_use_{ilevel}_{year}"))
  
  # combined codes for rows
  x <- df_tab %>%
    mutate(code = row_code) %>%
    left_join(df_comb, by = "code") %>%
    mutate(
      row_code = if_else(is.na(new_code), row_code, new_code),
      row_name = if_else(is.na(new_title), row_name, new_title),
    ) %>%
    select(!c(code, new_code, new_title))
  # combined codes for columns
  x <- x %>%
    mutate(code = col_code) %>%
    left_join(df_comb, by = "code") %>%
    mutate(
      col_code = if_else(is.na(new_code), col_code, new_code),
      col_name = if_else(is.na(new_title), col_name, new_title),
    ) %>%    
    select(!c(code, new_code, new_title))
  # add up values in combined rows and columns
  x <- x %>%
    summarize(
      value = sum(value, na.rm = TRUE),
      across(!value, first),
      .by = c("row_code", "col_code")
    )
  
  x <- x %>% 
    relocate(row_code, row_name, col_code, col_name, value, core_matrix)
  
  if (cache$enabled) {
    log_debug(paste("save to cache", cache_path))
    write_parquet(x, util$mkdir(cache_path))
  }
  
  x
}


supply_table <- function(year, ilevel = c("det", "sum", "sec")) {
  ilevel <- match.arg(ilevel)
  ilevel_long <- switch(ilevel, det = "detail", sum = "summary", sec = "sector")
  bea_rev <- 2022
  
  cache_path <- glue(opath$supply_table_)
  if (cache$enabled & file.exists(cache_path)) {
    log_debug(paste("read from cache", cache_path))
    return(read_parquet(cache_path))
  }
  
  df_comb <- ind_code_comb %>%
    filter(ilevel == ilevel_long) %>%
    select(code, new_code, new_title)
  df_tab <- pubdata::bea_io_get(glue("{bea_rev}_sup_{ilevel}_{year}"))
  if (ilevel == "sec") {
    # code is missing in source tables
    df_tab <- df_tab %>%
      mutate(row_code = if_else(row_name == "Total industry supply", "T017", row_code))
  }
  # combined codes for rows
  x <- df_tab %>%
    mutate(code = row_code) %>%
    left_join(df_comb, by = "code") %>%
    mutate(
      row_code = if_else(is.na(new_code), row_code, new_code),
      row_name = if_else(is.na(new_title), row_name, new_title),
      ) %>%
    select(!c(code, new_code, new_title))
  # combined codes for columns
  x <- x %>%
    mutate(code = col_code) %>%
    left_join(df_comb, by = "code") %>%
    mutate(
      col_code = if_else(is.na(new_code), col_code, new_code),
      col_name = if_else(is.na(new_title), col_name, new_title),
    ) %>%    
    select(!c(code, new_code, new_title))
  # add up values in combined rows and columns
  x <- x %>%
    summarize(
      value = sum(value, na.rm = TRUE),
      across(!value, first),
      .by = c("row_code", "col_code")
    )
  
  x <- x %>%
    relocate(row_code, row_name, col_code, col_name, value, core_matrix)
  
  if (cache$enabled) {
    log_debug(paste("save to cache", cache_path))
    write_parquet(x, util$mkdir(cache_path))
  }
  
  x
}



# Matrices ----

#' Calculate B-matrix from Use table
b_matrix <- function(year, ilevel = c("det", "sum", "sec")) {
  ilevel <- match.arg(ilevel)
  tab <- use_table(year, ilevel)
  ind_codes <- tab %>% filter(core_matrix) %>% distinct(col_code) %>% pull()
  tot_name <- switch(
    ilevel,
    det = "Total industry output (basic value)",
    sum = "Total industry output (basic prices)",
    sec = "Total industry output (basic prices)"
  )
  output <- tab %>%
    filter(row_name == tot_name, col_code %in% ind_codes) %>%
    pull(value, name = col_code)
  umat <- tab %>%
    filter(core_matrix) %>%
    util$tab2mat("row_code", "col_code")
  stopifnot(all(colnames(umat) == names(output)))
  
  bmat <- sweep(umat, 2, output, "/")
  # industry 4200ID (Customs duties) has zero total industry output
  bmat[, colSums(umat) == 0] <- 0
  bmat
}


#' Calculate C-matrix from Supply table
#' 
#' Industry total output is not exactly equal to supply matrix column sums because of rounding.
#' We normalize by column sums instead of industry total to guarantee that C-matrix columns sum to 1.
c_matrix <- function(year, ilevel = c("det", "sum", "sec")) {
  ilevel <- match.arg(ilevel)
  smat <- supply_table(year, ilevel) %>%
    filter(core_matrix) %>%
    util$tab2mat("row_code", "col_code")
  cmat <- sweep(smat, 2, colSums(smat), "/")
  # industry 4200ID (Customs duties) has zero total industry supply
  cmat[, colSums(smat) == 0] <- 0
  cmat
}



