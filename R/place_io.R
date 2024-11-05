# This script calculates output by industry/commodity at local place level (county or CBSA)


# R libraries ----
library(logger)
library(arrow)
library(tidyverse)
library(glue)


# R scripts ----
source("R/basic_utilities.R", local = (util <- new.env()))
source("R/bea_io.R", local = (bea_io <- new.env()))
source("R/dataprep_cbp.R", local = (cbp <- new.env()))
source("R/dataprep_infogroup.R", local = (infogroup <- new.env()))
source("R/dataprep_agcensus.R", local = (agcen <- new.env()))


# Data and cache ----

ipath <- list(
)

opath <- list(
  output_ = "data/place_io/output/{year}_{classification}_{ilevel}_{bus_data}.pq",
  outsupdem_ = "data/place_io/outsupdem/{year}_{ilevel}_{bus_data}.pq"
)

# cache management
cache <- new.env()
with(cache, {
  # use cache when calling functions
  enabled <- TRUE
  
  # create cache files by calling all functions that save to cache
  build <- function() {
    for (year in c(2012)) {
      for (classification in c("industry", "commodity")) {
        for (ilevel in c("sec", "sum", "det")) {
          for (bus_data in c("cbp_imp", "cbp_raw", "infogroup")) {
            output(year, classification, ilevel, bus_data)
            outsupdem(year, ilevel, bus_data)
          }
        }
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

# Helper functions ----


#' Drop infeasible counties and distribute their values to remaining feasible counties
#' 
#' @param values Vector of values to modify.
#' @param counties Vector of county codes, aligned with `values`.
#' @param feasible_counties Vector of feasible county codes.
#' 
#' @returns Dataframe with new "value" for each "place".  
distribute_to_feasible_counties <- function(values, counties, feasible_counties) {
  # validate inputs
  stopifnot(length(values) == length(counties))
  stopifnot(all(!is.na(values)))
  stopifnot(all(!is.na(counties)))
  stopifnot(all(!is.na(feasible_counties)))
  
  # for later check
  sum_before <- sum(values)
  
  # organize values and counties into a dataframe
  df <- tibble(place = counties, old_value = values) %>%
    mutate(state = str_sub(place, 1, 2))
  
  # drop infeasible states and increase value in all feasible states to keep the total unchanged
  feasible_states <- unique(str_sub(feasible_counties, 1, 2))
  df <- df %>%
    mutate(national_value = sum(old_value)) %>%
    filter(state %in% feasible_states) %>%
    mutate(national_mult = national_value / sum(old_value),
           value = old_value * national_mult)
  
  # drop infeasible counties and increase value in remaining counties to keep state-by-state totals unchanged
  df <- df %>%
    mutate(state_value = sum(value), .by = "state") %>%
    filter(place %in% feasible_counties) %>%
    mutate(state_mult = if_else(sum(value) == 0, 0, first(state_value) / sum(value)), .by = "state") %>%
    mutate(value = value * state_mult) %>%
    # rare special case when all non-state-wide counties have zero value
    mutate(value = if_else(state_mult == 0, state_value / n(), value), .by = "state")
  
  # check that aggregate value remains unchanged
  stopifnot(all.equal(sum(df$value), sum_before))
  
  # verify that only feasible counties remain
  stopifnot(all(df$place %in% feasible_counties))
  
  select(df, place, value)  
}


# Output ----


#' County output by industry or commodity
output <- function(year,
                   classification = c("industry", "commodity"),
                   ilevel = c("det", "sum", "sec"),
                   bus_data = c("cbp_imp", "cbp_raw", "infogroup")) {
  
  stopifnot(year %in% c(2012))
  classification <- match.arg(classification)
  ilevel <- match.arg(ilevel)
  ilevel_long <- switch(ilevel, det = "detail", sum = "summary", sec = "sector")
  bus_data <- match.arg(bus_data)
   
  cache_path <- glue(opath$output_)
  if (cache$enabled & file.exists(cache_path)) {
    log_debug(paste("read from cache", cache_path))
    return(read_parquet(cache_path))
  }  
  
  # industry concordance table
  df_conc <- bea_io$concordance() %>%
    filter(industry, ilevel == "detail")
  
  # agricultural industry value is already calculated at detail level
  df_ag_val <- agcen$get_farm_sales_by_bea_detail(year = year, geo_level = "county") %>%
    util$mat2tab("place", "detail")
  
  # non-agricultural industry value at detail level
  if (bus_data %in% c("cbp_imp", "cbp_raw")) {
    # CBP: join with multi-level naics-bea concordance 
    # to always use higher level CBP aggregation because it is less subject to supression
    # payroll is used as county-industry value
    x1 <- df_conc %>%
      filter(summary != "111CA") %>%
      distinct(detail, naics)
    x2 <- cbp$call_cbp(
      year = year,
      cbp_scale = "county",
      imputed = (bus_data == "cbp_imp")
    ) %>%
      select(place, naics, value = ap)
    # merge and aggregate at detail level
    # drop observations with missing county, these are industries not available in CBP
    df_nag_val <- left_join(x1, x2, by = "naics") %>%
      summarize(value = sum(value, na.rm = TRUE), .by = c("place", "detail")) %>%
      drop_na(place)
  } else if (bus_data == "infogroup") {
    # infogroup: join with 6-digit naics-bea concordance
    x1 <- df_conc %>%
      filter(summary != "111CA") %>%
      distinct(detail, naics6)
    x2 <- glue(infogroup$opath$county_) %>%
      open_dataset() %>%
      collect() %>%
      drop_na(naics) %>%
      replace_na(list(st = "99", cty = "999")) %>%
      mutate(place = paste0(st, cty)) %>%
      select(place, naics6 = naics, value = emp)
    # merge and aggregate at detail level
    # drop observations with missing county, these are industries not available in infogroup
    df_nag_val <- left_join(x1, x2, by = "naics6") %>%
      summarize(value = sum(value, na.rm = TRUE), .by = c("place", "detail")) %>%
      drop_na(place)
  }
  
  
  # relocate values to only feasible counties
  feas_counties <- cbp$call_cbp(year = year, cbp_scale = "county", imputed = FALSE) %>%
    distinct(place) %>%
    filter(!str_ends(place, "999")) %>%
    pull()
  # skip detail industries with no value
  x <- bind_rows(df_ag_val, df_nag_val) %>%
    filter(sum(value, na.rm = TRUE) > 0, .by = "detail")
  df_feas <- list()
  for (ind_code in unique(x$detail)) {
    x1 <- x %>%
      filter(detail == ind_code)
    df_feas[[ind_code]] <- distribute_to_feasible_counties(x1$value, x1$place, feas_counties)
  }
  df_feas <- bind_rows(df_feas, .id = "detail")
  stopifnot(all.equal(
    x %>% summarize(value = sum(value), .by = "detail") %>% pull(value),
    df_feas %>% summarize(value = sum(value), .by = "detail") %>% pull(value)
  ))
  
  # national industry output by detail
  x <- bea_io$supply_table(year, ilevel = "det")
  ind_codes <- x %>% 
    filter(core_matrix) %>% 
    distinct(col_code) %>% 
    pull()
  nat_out <- x %>%
    filter(row_name == "Total industry supply", col_code %in% ind_codes) %>%
    select(detail = col_code, nat_out = value)
  
  # calc place shares by detail, calc place output in $1000s by detail
  df_out_det <- df_feas %>%
    mutate(place_sh = value / sum(value, na.rm = TRUE), .by = "detail") %>%
    left_join(nat_out, by = "detail") %>%
    mutate(output = place_sh * 1000 * nat_out)
  
  # add missing industries, aggregate to requested ilevel, complete to full panel
  if (ilevel == "det") {
    df_ind <- df_conc %>%
      distinct(detail) %>%
      left_join(select(df_out_det, detail, place, output), by = "detail") %>%
      rename(indcode = detail)
  } else {
    df_ind <- df_conc %>%
      select(indcode = {{ilevel_long}}, detail) %>%
      distinct() %>%
      left_join(select(df_out_det, detail, place, output), by = "detail") %>%
      summarize(output = sum(output, na.rm = TRUE), .by = c("place", "indcode"))
  }
  df_ind <- df_ind %>%
    complete(place, indcode, fill = list(output = 0)) %>%
    filter(!is.na(place))

  # convert to commodity if requested
  if (classification == "industry") {
    df <- df_ind
  } else { # commodity
    cmat <- bea_io$c_matrix(year, ilevel = ilevel)
    output_mat <- df_ind %>%
      util$tab2mat("indcode", "place", "output")
    stopifnot(base::setequal(rownames(output_mat), colnames(cmat)))
    output_mat <- output_mat[colnames(cmat), ]
    df <- (cmat %*% output_mat) %>%
      as_tibble(rownames = "comcode") %>%
      pivot_longer(!comcode, names_to = "place", values_to = "output") %>%
      relocate(place, comcode, output) %>%
      arrange(place, comcode)
  }

  if (cache$enabled) {
    log_debug(paste("save to cache", cache_path))
    write_parquet(df, util$mkdir(cache_path))
  }
  
  df
}


# Supply and demand ----


#' County-commodity output, supply and demand in $1000s
#' 
#' WARNING. While sector and summary will technically work, the calculations are not correct:
#' I-O tables assume complete industries, but with business data, some detail 
#' industries have zero output, and sector aggregates are incomplete.
#' Accurate solution would be to create custom summary and sector I-O tables, aggregating from detail and taking missing industry columns outside of core.
#' This is already done at detail level.
outsupdem <- function(year,
                      ilevel = c("det", "sum", "sec"),
                      bus_data = c("cbp_imp", "cbp_raw", "infogroup")) {
  ilevel <- match.arg(ilevel)
  bus_data <- match.arg(bus_data)

  cache_path <- glue(opath$outsupdem_)
  if (cache$enabled & file.exists(cache_path)) {
    log_debug(paste("read from cache", cache_path))
    return(read_parquet(cache_path))
  }
  
  # I-O tables
  sup_tab <- bea_io$supply_table(year, ilevel = ilevel)
  use_tab <- bea_io$use_table(year, ilevel = ilevel)
  bmat <- bea_io$b_matrix(year, ilevel = ilevel)
  com_codes <- use_tab %>% filter(core_matrix) %>% distinct(row_code) %>% pull()
  ind_codes <- use_tab %>% filter(core_matrix) %>% distinct(col_code) %>% pull()
  
  #### output
  # county-industry output
  output_ind <- output(year, classification = "industry", ilevel = ilevel, bus_data = bus_data)
  # county-commodity output
  output_com <- output(year, classification = "commodity", ilevel = ilevel, bus_data = bus_data)
  
  # outputs in matrix form for multiplication
  # rearrange rows to align with I-O tables
  output_ind_mat <- util$tab2mat(output_ind, row = "indcode", col = "place", val = "output")
  stopifnot(base::setequal(rownames(output_ind_mat), ind_codes))
  output_ind_mat <- output_ind_mat[ind_codes, ]
  output_com_mat <- util$tab2mat(output_com, row = "comcode", col = "place", val = "output")
  stopifnot(base::setequal(rownames(output_com_mat), com_codes))
  output_com_mat <- output_com_mat[com_codes, ]
  
  # non-zero industries
  pos_ind_codes <- rownames(output_ind_mat)[rowSums(output_ind_mat) > 0]
  zero_ind_codes <- base::setdiff(rownames(output_ind_mat), pos_ind_codes)
  if (length(zero_ind_codes) > 0) {
    log_debug(glue("{length(zero_ind_codes)} industries have zero output: {str_c(zero_ind_codes, collapse = ',')}"))
  }
  
  #### supply
  # domestic intermediate use share of total use
  total_use <- use_tab %>%
    filter(col_name == "Total use of products", row_code %in% com_codes) %>%
    pull(value, name = "row_code")
  # in the use table only add up columns with positive industry output
  dom_int_use <- use_tab %>%
    filter(core_matrix, col_code %in% pos_ind_codes) %>%
    util$tab2mat(row = "row_code", col = "col_code") %>%
    rowSums()
  stopifnot(all(names(total_use) == names(dom_int_use)))
  dom_int_use_share <- dom_int_use / total_use
  dom_int_use_share[dom_int_use == 0 | total_use == 0] <- 0
  # tibble(comcode = names(total_use), dom_int_use, total_use, dom_int_use_share) %>% View()
  
  # county-commodity supply = commodity_output * use_share
  stopifnot(all(rownames(output_com_mat) == names(dom_int_use_share)))
  supply_mat <- sweep(output_com_mat, 1, dom_int_use_share, "*")
  
  #### demand
  # producer price domestic output share of total supply
  # total_use == total_supply, I-O table accounting identity
  total_supply <- total_use
  # in the supply table only add up columns with positive industry output
  dom_pp_out <- sup_tab %>%
    filter(core_matrix, col_code %in% pos_ind_codes) %>%
    util$tab2mat("row_code", "col_code") %>%
    rowSums()
  stopifnot(all(names(total_supply) == names(dom_pp_out)))
  dom_pp_out_share <- dom_pp_out / total_supply
  dom_pp_out_share[dom_pp_out == 0 | total_supply == 0] <- 0
  # tibble(comcode = names(total_supply), dom_pp_out, total_supply, dom_pp_out_share) %>% View()

  # county-commodity demand = B-mat * industry_output * dom_pp_share
  stopifnot(all(colnames(bmat) == rownames(output_ind_mat)))
  demand_mat <- bmat %*% output_ind_mat
  stopifnot(all(rownames(demand_mat) == names(dom_pp_out_share)))
  demand_mat <- sweep(demand_mat, 1, dom_pp_out_share, "*")
  
  
  #### bind output, supply and demand together into single dataframe
  df_supply <- supply_mat %>%
    as_tibble(rownames = "comcode") %>%
    pivot_longer(!comcode, names_to = "place", values_to = "supply")
  df_demand <- demand_mat %>%
    as_tibble(rownames = "comcode") %>%
    pivot_longer(!comcode, names_to = "place", values_to = "demand")
  stopifnot(nrow(output_com) == nrow(df_supply))
  stopifnot(nrow(output_com) == nrow(df_demand))
  df <- inner_join(output_com, df_supply, join_by(comcode, place), relationship = "one-to-one") %>%
    inner_join(df_demand, join_by(comcode, place), relationship = "one-to-one")

  if (cache$enabled) {
    log_debug(paste("save to cache", cache_path))
    write_parquet(df, util$mkdir(cache_path))
  }
  
  df
}



