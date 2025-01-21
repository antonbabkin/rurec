# Circularity paper - data preparation

library(tidyverse)

gdp_by_sector <- function() {

  x = 
    pubdata::bea_reg_get("2022_cagdp2") %>%
    filter(year == 2012) %>%
    select(geo_fips, geo_name, line_code, ind_code, ind_desc, value, value_f) %>%
    mutate(
      geo_st = str_sub(geo_fips, 1, 2),
      geo = case_when(
        geo_fips == "00000" ~ "nat",
        as.numeric(geo_st) %in% 91:98 ~ "reg",
        str_sub(geo_fips, 3, 5) == "000" ~ "st",
        .default = "cty"),
      line_code = as.character(line_code)
    ) %>%
    filter(geo == "cty") %>%
    filter(!(value_f %in% c("(NA)"))) %>%
    select(place = geo_fips, ind_code, ind_desc, value) %>%
    mutate(
      sector = case_when(
        ind_desc == "All industry total" ~ "tot",
        ind_code == "11" ~ "ag",
        ind_code == "21" ~ "mine",
        ind_code == "31-33" ~ "mfg",
        ind_code == "52,53" ~ "fire",
        ind_code == "54,55,56" ~ "prof")
    ) %>%
    filter(!is.na(sector))
  
  x1 =
    x %>%
    mutate(
      suppressed = is.na(value),
      value = replace_na(value, 0)
    ) %>%
    mutate(share = value / first(if_else(sector == "tot", value, NA), na_rm = TRUE), .by = "place") %>%
    arrange(place) %>%
    select(place, sector, gdp = value, share, suppressed) %>%
    mutate(
      sector_desc = case_match(
        sector,
        "tot" ~ "Total",
        "ag" ~ "Agriculture, forestry, fishing and hunting",
        "mine" ~ "Mining, quarrying, and oil and gas extraction",
        "mfg" ~ "Manufacturing",
        "fire" ~ "Finance, insurance, real estate, rental, and leasing",
        "prof" ~ "Professional and business services"
      )
    )
  
  x1
}