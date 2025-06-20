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
  ind_output_ = "data/place_io/ind_output/{year}_{ilevel}_{bus_data}.pq",
  outsupdem_ = "data/place_io/outsupdem/{year}_{ilevel}_{bus_data}.pq"
)

# cache management
cache <- new.env()
with(cache, {
  # use cache when calling functions
  enabled <- TRUE
  
  read <- function(path) {
    if (enabled && file.exists(path)) {
      log_debug(paste("read from cache", path))
      return(read_parquet(path))
    }
  }
  
  write <- function(df, path) {
    if (enabled) {
      log_debug(paste("save to cache", path))
      write_parquet(df, util$mkdir(path))
    }
  }

  # create cache files by calling all functions that save to cache
  build <- function() {
    for (year in c(2012)) {
      for (ilevel in c("sum", "det")) {
        for (bus_data in c("cbp_imp", "cbp_raw", "infogroup")) {
          ind_output(year, ilevel, bus_data)
          outsupdem(year, ilevel, bus_data)
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



call_agoutput <- function(year, geo_level = c("county", "state", "national")) {
  if (year != 2012) stop("Census of Agriculture was only imputed for 2012, use call_output_old() if you want other years without imputation")
  ipath$agcensus_imp_2012 %>%
    arrow::read_parquet() %>%
    separate_wider_delim(id, "_", names_sep = "") %>%
    mutate(place = paste0(id1, id2), commodity = paste(id3, id4, id5, sep = "_")) %>%
    {switch(
      geo_level,
      national = filter(., place == "00000"),
      state = filter(., place != "00000", id2 == "000"),
      county = filter(., id2 != "000")
    )} %>%
    mutate(value = if_else(is.na(sale), sol, sale)) %>%
    pivot_wider(id_cols = place, names_from = commodity, values_fill = 0) %>%
    mutate(
      "1111A0" = `1c_1gr_3soy`,
      "1111B0" = `1c_1gr_0000` - `1c_1gr_3soy`,
      "111200" = `1c_4ve_0000`,
      "111300" = `1c_5fr_0000`,
      "111400" = `1c_6ho_0000` + `1c_7tr_0000`,
      "111900" = `1c_2to_0000` + `1c_3co_0000` + `1c_8ot_0000`,
      "1121A0" = `2a_2ca_0000`,
      "112120" = `2a_3mi_0000`,
      "112300" = `2a_1po_0000`,
      "112A00" = `2a_4hg_0000` + `2a_5sh_0000` + `2a_6hr_0000` + `2a_7aq_0000` + `2a_8ot_0000`
    ) %>%
    select(place, starts_with("11"))
}



#' County output by industry
ind_output <- function(year,
                       ilevel = c("det", "sum", "sec"),
                       bus_data = c("cbp_imp", "cbp_raw", "infogroup")) {
  
  if (year != 2012) stop("Not implemented")
  ilevel <- match.arg(ilevel)
  bus_data <- match.arg(bus_data)
   
  cache_path <- glue(opath$ind_output_)
  if (!is.null(x <- cache$read(cache_path))) return(x)
  
  # industry concordance table
  df_conc <- bea_io$concordance() %>%
    filter(industry, ilevel == "detail")
  
  # agricultural industry value is already calculated at detail level
  df_ag_val <- agcen$get_farm_sales_by_bea_detail(year = year, geo_level = "county") %>%
    util$mat2tab("place", "detail")
  
  # non-agricultural, non-government industry value at detail level
  if (bus_data %in% c("cbp_imp", "cbp_raw")) {
    # CBP: join with multi-level naics-bea concordance 
    # to always use higher level CBP aggregation because it is less subject to supression
    # payroll is used as county-industry value
    x1 <- df_conc %>%
      filter(summary != "111CA", sector != "G") %>%
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
      filter(summary != "111CA", sector != "G") %>%
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
  nat_out <- bea_io$ind_totals(year, "det") %>%
    select(detail = ind_code, nat_out = make)
  
  # calc place shares by detail, calc place output in $1000s by detail
  df_out_det <- df_feas %>%
    mutate(place_sh = value / sum(value, na.rm = TRUE), .by = "detail") %>%
    left_join(nat_out, by = "detail") %>%
    mutate(output = place_sh * 1000 * nat_out)
  
  # add missing industries, aggregate to requested ilevel
  if (ilevel == "det") {
    df <- df_conc %>%
      distinct(detail) %>%
      left_join(select(df_out_det, detail, place, output), by = "detail") %>%
      rename(ind_code = detail)
  } else {
    ilevel_long <- switch(ilevel, det = "detail", sum = "summary", sec = "sector")
    df <- df_conc %>%
      select(ind_code = {{ilevel_long}}, detail) %>%
      distinct() %>%
      left_join(select(df_out_det, detail, place, output), by = "detail") %>%
      summarize(output = sum(output, na.rm = TRUE), .by = c("place", "ind_code"))
  }
  # complete to full panel, add industry names
  x <- bea_io$ind_totals(year, ilevel) %>%
    select(ind_code, ind_name)
  df <- df %>%
    complete(place, ind_code, fill = list(output = 0)) %>%
    filter(!is.na(place)) %>%
    left_join(x, by = "ind_code") %>%
    relocate(place, ind_code, ind_name, output)

  cache$write(df, cache_path)
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
  if (!is.null(x <- cache$read(cache_path))) return(x)
  
  #### I-O national tables
  # make table and matrix
  mak_tab <- bea_io$make_table(year, ilevel)
  mak_mat <- mak_tab %>%
    filter(core_matrix) %>%
    util$tab2mat("row_code", "col_code")
  # C-matrix is commodity-by-industry, make matrix is industry-by-commodity
  c_mat <- sweep(mak_mat, 1, rowSums(mak_mat), "/") %>%
    t()
  stopifnot(all.equal(colSums(c_mat), rep(1, ncol(c_mat)), check.names = FALSE))
  # use table
  use_tab <- bea_io$domuse_table(year, ilevel)
  use_mat <- use_tab %>%
    filter(core_matrix) %>%
    util$tab2mat("row_code", "col_code")
  # commodity and industry codes and totals
  ind_tot <- bea_io$ind_totals(year, ilevel)
  com_tot <- bea_io$com_totals(year, ilevel)
  # (domestic) B-matrix, commodity-by-industry
  stopifnot(all(colnames(use_mat) == ind_tot$ind_code))
  b_mat <- sweep(use_mat, 2, ind_tot$make, "/")
  # verify alignment
  stopifnot(all(rownames(c_mat) == com_tot$com_code))
  stopifnot(all(colnames(c_mat) == ind_tot$ind_code))
  
  #### output by industry
  # county-industry output table
  output_ind <- ind_output(year, ilevel = ilevel, bus_data = bus_data)
  # industry-by-county output in matrix form
  # rearrange rows to align with I-O tables
  output_ind_mat <- util$tab2mat(output_ind, row = "ind_code", col = "place", val = "output")
  stopifnot(base::setequal(rownames(output_ind_mat), ind_tot$ind_code))
  output_ind_mat <- output_ind_mat[ind_tot$ind_code, ]
  
  #### adjustment factors from national totals
  adj <- com_tot
  # industries with zero output are moved off-core in I-O matrices
  pos_ind_codes <- rownames(output_ind_mat)[rowSums(output_ind_mat) > 0]
  # national commodity output of observed industries
  pos_ind_make <- mak_mat[pos_ind_codes, ] %>%
    colSums()
  stopifnot(all(names(pos_ind_make) == adj$com_code))
  adj$make_pos <- pos_ind_make
  # national commodity demand from observed industries
  pos_ind_use <- use_mat[, pos_ind_codes] %>%
    rowSums()
  stopifnot(all(names(pos_ind_use) == adj$com_code))
  adj$int_use_pos <- pos_ind_use
  # adjustment factors for supply and demand
  adj <- adj %>%
    mutate(
      pos_int_use_sh = case_when(
        int_use_pos > 0 & use > 0 ~ int_use_pos / use,
        .default = 0),
      pos_make_sh = case_when(
        make_pos > 0 & make > 0 ~ make_pos / make,
        .default = 0)
      )
  
  #### supply and demand
  # commodity-by-county output matrix
  stopifnot(all(colnames(c_mat) == rownames(output_ind_mat)))
  output_com_mat <- c_mat %*% output_ind_mat
  # commodity-by-county supply = commodity_output * pos_int_use_sh
  stopifnot(all(rownames(output_com_mat) == adj$com_code))
  supply_mat <- sweep(output_com_mat, 1, adj$pos_int_use_sh, "*")
  # commodity-by-county demand = B-mat * industry_output * pos_make_sh
  stopifnot(all(colnames(b_mat) == rownames(output_ind_mat)))
  demand_mat <- b_mat %*% output_ind_mat
  stopifnot(all(rownames(demand_mat) == adj$com_code))
  demand_mat <- sweep(demand_mat, 1, adj$pos_make_sh, "*")
  
  #### bind output, supply and demand together into single dataframe
  df_output <- output_com_mat %>%
    as_tibble(rownames = "com_code") %>%
    pivot_longer(!com_code, names_to = "place", values_to = "output")    
  df_supply <- supply_mat %>%
    as_tibble(rownames = "com_code") %>%
    pivot_longer(!com_code, names_to = "place", values_to = "supply")
  df_demand <- demand_mat %>%
    as_tibble(rownames = "com_code") %>%
    pivot_longer(!com_code, names_to = "place", values_to = "demand")
  df <- com_tot %>%
    select(com_code, com_name) %>%
    full_join(df_output, "com_code", relationship = "one-to-many") %>%
    full_join(df_supply, c("com_code", "place"), relationship = "one-to-one") %>%
    full_join(df_demand, c("com_code", "place"), relationship = "one-to-one") %>%
    arrange(place, com_code) %>%
    relocate(place, com_code, com_name, output, supply, demand)
  
  cache$write(df, cache_path)
  df
}



