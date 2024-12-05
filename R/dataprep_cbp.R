# Data preparation of CBP

# R libraries ----
library(logger)
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
  pymod$cbp <- import("rurec.pubdata.cbp")
  pymod$initialized <- TRUE
}


# Data objects ----
ipath <- list(
  cbp_ = "data/pubdata/cbp/cbp_pq/{geo}/{year}.pq",
  efsy_ = "data/pubdata/cbp/efsy_pq/years/{year}.pq"
)

opath <- list(
  cbp_ = "data/cbp/{geo}_{year}_imp{imputed}.pq"
)

clear_outputs <- function() {
  util$clear_paths(opath)
}



# Pubdata ----
pubdata <- new.env()

pubdata$prep_cbp <- function(geo = c("us", "state", "county"), year) {
  geo <- match.arg(geo)
  year <- as.integer(year)
  p <- glue(ipath$cbp_)
  if (!file.exists(p)) {
    pymod$init()
    pymod$cbp$get_cbp_df(geo, year)
  }
}


pubdata$prep_efsy <- function(year) {
  year <- as.integer(year)
  p <- glue(ipath$efsy_)
  if (!file.exists(p)) {
    pymod$init()
    pymod$cbp$get_efsy_year_df(year)
  }
}


# Imputed CBP  ----

#' National CBP with missing employment and payroll imputed
#' 
#' Imputation is done by linear interpolation of emp and pay in suppressed industries 
#' from higher digit in proportion to number of establishments.
impute_national <- function(year) {
  
  df <- ipath$cbp_ %>%
    glue(year = year, geo = "us") %>%
    read_parquet() %>%
    {
      if ("lfo" %in% names(.)) filter(., lfo == "-")
      else .
    } %>%
    select(naics, est, emp, ap) %>%
    mutate(
      naics = str_remove_all(naics, "[-/]"),
      digits = if_else(naics == "", 1, str_length(naics))
    )
  
  # skip years with issues (assertions fail) - investigate if years are needed in future
  if (year %in% c(2001, 2009)) {
    warning("CBP imputation might have issues in ", year)
    return(select(df, !digits))
  }
  
  
  # Iterate from higher to lower levels of aggregation,
  # imputing missing values on the level below at every step.
  for (dig in 1:5) {
    # total emp and pay at "dig" level
    d_tot <- df %>%
      filter(digits == dig) %>%
      rename(naics_tot = naics) %>%
      select(!digits)
    # industries for imputation at "dig+1" level
    d_imp <- df %>%
      filter(digits == dig + 1) %>%
      {
        if (dig == 1) mutate(., naics_tot = "")
        else mutate(., naics_tot = str_sub(naics, 1, dig))
      }
    # some special cases at 2-digit level
    if (dig == 2) {
      # "95" and "99" are missing industry codes and does not go below 2-digit level
      d_tot <- filter(d_tot, !(naics_tot %in% c("95", "99")))
      # over-ride first 2 digits of 3-digit industries for merge
      # "31" consists of "31x", "32x" and "33x"
      # "44" consists of "44x" and "45x"
      # "48" consists of "48x" and "49x"
      d_imp <- d_imp %>%
        mutate(naics_tot = case_match(naics_tot, "32" ~ "31", "33" ~ "31", "45" ~ "44", "49" ~ "48", .default = naics_tot))
    }
    # sum at "dig+1" level
    # if somethig is suppressed, total at "dig" > sum at "dig+1"
    d_sum <- d_imp %>%
      summarize(across(c(est, emp, ap), sum), .by = "naics_tot")
    
    # verify 1-to-1 merge
    stopifnot(all(d_tot$naics_tot == d_sum$naics_tot))
    # no suppression in est:
    stopifnot(all(d_tot$est == d_sum$est))
    
    # interpolation
    d_imp <- d_imp %>%
      left_join(d_tot, by = "naics_tot", suffix = c("",  "_tot")) %>%
      left_join(d_sum, by = "naics_tot", suffix = c("",  "_sum")) %>%
      # combined emp and pay in all suppressed "dig+1" industries within single "dig" industry
      mutate(
        emp_sup = emp_tot - emp_sum,
        ap_sup = ap_tot - ap_sum
      ) %>%
      # combined est in "dig+1" industries where emp or pay is suppressed
      mutate(
        est_sup_emp = sum(if_else(emp == 0, est, 0)),
        est_sup_ap = sum(if_else(ap == 0, est, 0)),
        .by = "naics_tot"
      ) %>%
      # emp(pay) is distributed among suppressed "dig+1" industries within single "dig" industry
      # in proportion with est
      mutate(
        emp = if_else(emp == 0, emp_sup * est / est_sup_emp, emp),
        ap = if_else(ap == 0, ap_sup * est / est_sup_ap, ap)
      )
    
    # take interpolated values back into main df
    df <- left_join(df, select(d_imp, naics, emp_imp = emp, ap_imp = ap), "naics") %>%
      mutate(
        emp = if_else(is.na(emp_imp), emp, emp_imp),
        ap = if_else(is.na(ap_imp), ap, ap_imp)
      ) %>%
      select(naics, est, emp, ap, digits)
  }
  
  df %>% select(!digits)
}


# Call up and clean CBP ($1,000 of dollars) available for years 1986:2021
#' @param imputed if TRUE derive county-level annual payroll from county-level EFSY imputed employment and CBP annual payroll
call_cbp <- function(year,
                     cbp_scale = c("county", "state", "us"),
                     imputed = TRUE) {
  cbp_scale <- match.arg(cbp_scale)
  

  if (imputed && (year > 2016)) {
    stop(glue("Imputed CBP not available for year {year}"))
  }
  
  if (imputed && (cbp_scale != "county")) {
    stop(glue("Imputed CBP not available for '{cbp_scale}' scale"))
  }
  
  if (imputed && year %in% c(2001, 2009)) {
    # national imputation is skipped for these years
    warning("CBP imputation might have issues in ", year)
  }

  cache_path <- glue(opath$cbp_, geo = cbp_scale)
  if (file.exists(cache_path)) {
    log_debug(paste("read from cache", cache_path))
    return(read_parquet(cache_path))
  }

  if (!imputed) {
    pubdata$prep_cbp(cbp_scale, year)
    p <- glue(ipath$cbp_, geo = cbp_scale)
    df <- open_dataset(p) %>%
      select(any_of(c("fipstate", "fipscty", "lfo")), naics, est, emp, ap, qp1) %>%
      collect() %>%
      mutate(naics = str_remove_all(naics, "[-/]"))
    
    if (cbp_scale == "county") {
      df$place <- paste0(df$fipstate, df$fipscty)
    }
    if (cbp_scale == "state") {
      df$place <- df$fipstate
    }
    if (cbp_scale == "us") {
      df$place <- "usa"
    }
  } else { # imputed == TRUE

    pubdata$prep_cbp("county", year)
    pubdata$prep_cbp("us", year)
    pubdata$prep_efsy(year)
    
    # CBP raw county data
    df <- glue(ipath$cbp_, geo = "county") %>%
      open_dataset() %>%
      select(fipstate, fipscty, naics, est, emp, ap) %>%
      collect() %>%
      mutate(place = paste0(fipstate, fipscty),
             naics = str_remove_all(naics, "[-/]"))

    # merge EFSY county employment
    d <- glue(ipath$efsy_) %>%
      open_dataset() %>%
      mutate(efsy_emp = (lb + ub) / 2,
             naics = str_remove_all(naics, "[-/]")) %>%
      select(fipstate, fipscty, naics, efsy_emp) %>%
      collect()
    if (year == 1999) {
      # 6 pairs of duplicate rows exist in this year
      d <- distinct(d)
    }      
    df <- left_join(df, d, join_by(fipstate, fipscty, naics))

    ## calculate and merge total suppressed emp and pay
    # unsuppressed total emp and pay by naics
    d <- df %>%
      group_by(naics) %>%
      summarize(emp_unsup = sum(emp), ap_unsup = sum(ap))
    # national emp and pay by naics
    dsup <- impute_national(year) %>%
      select(naics, emp_nat = emp, ap_nat = ap) %>%
      left_join(d, "naics") %>%
      mutate(emp_sup = emp_nat - emp_unsup, ap_sup = ap_nat - ap_unsup)
    
    # sanity check
    d_sus <- dsup %>%
      filter(((emp_nat > 0) & (emp_nat < emp_unsup))
        | ((ap_nat > 0) & (ap_nat < ap_unsup)))
    if (nrow(d_sus) > 0) {
      log_warn("Imputed emp/ap greater than national!!!")
      print(d_sus)
    }

    df <- left_join(df, dsup, join_by(naics))
    
    # fill missing with imputed
    df <- df %>%
      mutate(imputed = (emp == 0 & efsy_emp > 0)) %>%
      mutate(emp = if_else(imputed, efsy_emp, emp)) %>%
      mutate(ap = if_else(imputed & (emp_nat > 0), emp / emp_sup * ap_sup, ap))
    
    # drop temporary columns
    df <- df %>%
      select(place, naics, est, emp, ap, imputed)
  }

  
  log_debug(paste("save to cache", cache_path))
  write_parquet(df, util$mkdir(cache_path))

  return(df)
}


# Tests ----

test_naics_years <- function() {
  for (y in 1998:2021) {
    if (y < 2017) 
      call_cbp(y, "county", TRUE)
    for (scale in c("county", "state", "us"))
      call_cbp(y, scale, FALSE)
  }
}
  
  