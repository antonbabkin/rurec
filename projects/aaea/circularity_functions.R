
# This script generates circularity metrics

# R libraries ----
library(logger)
log_threshold(DEBUG)
library(arrow)
library(tidyverse)
library(glue)

# R scripts ----
source("R/basic_utilities.R", local = (util <- new.env()))
source("R/place_output.R", local = (place_output <- new.env()))
source("R/geography.R", local = (geog <- new.env()))

# Data objects ----
ipath <- list(
  # data dependencies
)

opath <- list(
  # data products
  circularity_ = "data/circularity/circularity_{year}_{ilevel}_{class_system}_{paradigm}_{bus_data}_{cbsa}_{cluster_subset}_{spatial}.rds"
)

clear_outputs <- function() {
  util$clear_paths(opath)
}

#--- Function that creates circularity inputs

call_extraction_table <- function(year,
                                    bus_data = c("cbp_imp", "cbp_raw", "infogroup"),
                                    ilevel = "det", # c("det", "sum", "sec")
                                    class_system = "commodity", # c("industry", "commodity")
                                    paradigm = "domestic", # c("factor", "domestic", "capital"),
                                    cbsa = FALSE,
                                    cluster_subset = NULL,
                                    from_cache = FALSE,
                                    to_cache = FALSE,
                                    overwrite = FALSE,
                                    spatial = TRUE){

  cache_path <- glue(opath$circularity_)

  if (from_cache == TRUE){
    if (file.exists(cache_path)) {
      log_debug(paste("read from cache", cache_path))
      return(readRDS(cache_path))
    } else {
      log_debug("WARNING: Cached version does not exist")
      from_cache = FALSE
    }
  } 

  if (from_cache == FALSE){
  
    factor_list <- place_output$call_factor_list(year = year,
                                                 class_system = class_system,
                                                 paradigm = paradigm,
                                                 ilevel = ilevel,
                                                 bus_data = bus_data,
                                                 cbsa = cbsa)
    total_output <- factor_list |>
      group_by(place) |>
      summarize(
        total_output = sum(gross_output)
      )
    
    if (!is.null(cluster_subset)){
      factor_list <- factor_list[grepl(cluster_subset, factor_list$indcode), ]
    }
    
    df <- factor_list |> 
      group_by(place) |> 
      summarize(
        across(where(is.numeric), sum)
      )
    
    df <- inner_join(df, total_output)
    
    if (spatial){
      geot <- geog$call_geog(year = year, cbsa = cbsa)
      df <- inner_join(geot, df, by = "place", copy = TRUE)
    }
     
    if (to_cache == TRUE){
      if (file.exists(cache_path)) {
        if (overwrite == FALSE){
          log_debug(paste("file", cache_path, "already exists. Set overwrite = TRUE to replace existing file"))
        } 
        else if (overwrite == TRUE){
          clear_outputs()
        }
      } 
      else {
        clear_outputs()
        log_debug(paste("save to cache", cache_path))
        saveRDS(df, util$mkdir(cache_path))
      }
    }

    return(df)
  }
}

calculate_circularity_metrics <- function(df,
                                          cluster_subset = NULL
                                          ){
  df <- df |> 
  mutate(
    production_capacity = intermediate_supply/total_output,
    trade_capacity = net_supply/total_output,
    retention = 1 - net_supply/intermediate_supply,
    production_dependency = intermediate_demand/total_output,
    trade_dependency = net_demand/total_output,
    autonomy = 1 - net_demand/intermediate_demand,
    trade_balance = (net_supply-net_demand)/total_output,
    trade_openness = (net_supply+net_demand)/total_output
  )

  if (!is.null(cluster_subset)){
    df <- df |> 
      mutate(
        subset_production_capacity = intermediate_supply/gross_output,
        subset_trade_capacity = net_supply/gross_output,
        subset_production_dependency = intermediate_demand/gross_output,
        subset_trade_depenency = net_demand/gross_output,
        subset_trade_balance = (net_supply-net_demand)/gross_output,
        subset_trade_openness = (net_supply+net_demand)/gross_output
      )
  }
  
  return(df)  
}


