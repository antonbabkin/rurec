# This script compiles the circularity dataset


# libraries and sources ----

library(tidyverse)
library(logger)

source("R/basic_utilities.R", local = (util <- new.env()))
source("R/circularity.R", local = (circularity_source <- new.env()))

# paths ----

# many inputs are required indirectly through sourced scripts
ipath <- list(
  
)

opath <- list(
  circularity = "datasets/circularity/circularity.rds"
)

# dataset ----

get_circularity <- function() {
  cache_path = opath$circularity
  if (file.exists(cache_path)) {
    df <- readRDS(cache_path)
    log_debug("read from cache {cache_path}")
  } else {
    df <- list()
    for (d in c("infogroup", "cbp_imp")) {
      for (l in c("det", "sum", "sec")) {
        for (year in c(2007, 2012, 2017)) {
          if (year == 2017 && d == "cbp_imp") {
            log_warn("Imputed CBP is not available in 2017. Circularity indicators for 'cbp_imp' not calculated.")
            next
          }
          df[[length(df) + 1]] <- circularity_source$call_circularity_metrics(
            year = year,
            ilevel = l,
            class_system = "commodity",
            paradigm = "domestic",
            bus_data = d,
            spatial = FALSE
          ) |>
            select(
              place,
              gross_output,
              intermediate_supply,
              intermediate_demand,
              net_supply,
              net_demand,
              production_capacity,
              trade_capacity,
              retention,
              production_dependency,
              trade_dependency,
              autonomy,
              trade_balance,
              trade_openness
            ) |>
            rename(excess_supply = net_supply,
                   excess_demand = net_demand) |>
            mutate(bus_data = d, .before = 1) |>
            mutate(ilevel = l, .before = 1) |>
            mutate(year = year, .before = 1)
        }
      }
    }
    df <- bind_rows(df)
    
    saveRDS(df, util$mkdir(cache_path))
    log_debug("save to cache {cache_path}")
  }
  df
}
