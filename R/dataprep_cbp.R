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
      collect()
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
    p <- glue(ipath$cbp_, geo = "county")
    df <- open_dataset(p) %>%
      select(fipstate, fipscty, naics, est, emp, ap, qp1) %>%
      collect()
    df$place <- paste0(df$fipstate, df$fipscty)

    # merge EFSY county employment
    p <- glue(ipath$efsy_)
    d <- open_dataset(p) %>%
      mutate(efsy_emp = (lb + ub) / 2) %>%
      select(fipstate, fipscty, naics, efsy_emp) %>%
      collect()
    if (year == 1999) {
      # 6 pairs of duplicate rows exist in this year
      d <- distinct(d)
    }      
    df <- left_join(df, d, join_by(fipstate, fipscty, naics))

    # calculate and merge total suppressed emp and pay
    p <- glue(ipath$cbp_, geo = "us")
    dsup <- open_dataset(p)
    if ("lfo" %in% names(dsup)) {
      dsup <- filter(dsup, lfo == "-")
    }
    dsup <- dsup |>
      select(naics, emp, ap) |>
      rename(emp_nat = emp, ap_nat = ap) |>
      collect()

    d <- df |>
      group_by(naics) |>
      summarize(emp_unsup = sum(emp), ap_unsup = sum(ap))
    dsup <- left_join(dsup, d, join_by(naics)) |>
      mutate(emp_sup = emp_nat - emp_unsup, ap_sup = ap_nat - ap_unsup)
    
    # non-blocking sanity check
    d_sus <- dsup |>
      filter(((emp_nat > 0) & (emp_nat < emp_unsup))
        | ((ap_nat > 0) & (ap_nat < ap_unsup)))
    if (nrow(d_sus) > 0) {
      log_warn("Imputed emp/ap greater than national!!!")
      print(d_sus)
    }

    df <- left_join(df, dsup, join_by(naics))
    
    # fill missing with imputed
    df <- df |>
      mutate(imputed = (emp == 0 & efsy_emp > 0)) |>
      mutate(emp = if_else(imputed, efsy_emp, emp)) |>
      mutate(ap = if_else(imputed & (emp_nat > 0), emp / emp_sup * ap_sup, ap))
    
    # drop temporary columns
    df <- df |>
      select(place, naics, est, emp, ap, qp1, imputed, fipstate, fipscty)
  }
  
  df$naics <- str_remove_all(df$naics, "[-/]")
  
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
  
  