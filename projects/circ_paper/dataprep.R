# Circularity paper - data preparation

library(tidyverse)

gdp_by_sector <- function() {
  x = pubdata::bea_reg_get("2022_cagdp2") %>%
    filter(year == 2012) %>%
    filter(!str_ends(geo_fips, "000")) %>%
    filter(!(value_f %in% c("(NA)"))) %>%
    select(place = geo_fips, ind_code, ind_desc, value) %>%
    mutate(
      sector = case_match(
        ind_code,
        "11,21" ~ "afm",
        "42,44-45" ~ "tr",
        "22,48-49" ~ "tr", 
        "31-33,51" ~ "mfg",
        "52,53" ~ "svc",
        "54,55,56" ~ "svc", 
        "92" ~ "gov")
    )
  
  x1 = bind_rows(
    x %>%
      filter(ind_desc == "All industry total") %>%
      mutate(sector = "tot") %>%
      select(place, sector, value),
    x %>%
      filter(!is.na(sector)) %>%
      summarize(value = sum(value, na.rm = TRUE), .by = c("place", "sector"))
  ) %>%
    mutate(share = value / first(if_else(sector == "tot", value, NA), na_rm = TRUE), .by = "place") %>%
    arrange(place) %>%
    select(place, sector, gdp = value, share) %>%
    mutate(
      sector_desc = case_match(
        sector,
        "tot" ~ "Total",
        "afm" ~ "Natural resources and mining",
        "tr" ~ "Trade, transportation and utilities",
        "mfg" ~ "Manufacturing and information",
        "svc" ~ "FIRE, professional and business services",
        "gov" ~ "Government and government enterprises"
      )
    )
  
  x1
}