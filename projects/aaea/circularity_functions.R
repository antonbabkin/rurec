
# This script generates circularity metrics

# R libraries ----
library(logger)
log_threshold(DEBUG)
library(arrow)
library(tidyverse)
library(glue)

# R scripts ----
source("R/basic_utilities.R", local = (util <- new.env()))
source("R/place_io.R", local = (place_io <- new.env()))
source("R/geography.R", local = (geog <- new.env()))


# Data objects ----
ipath <- list(
  # data dependencies
)

opath <- list(
  # data products
  circularity_ = "data/projects/aaea/circularity/circularity_{year}_{ilevel}_{bus_data}_{cluster_subset}_{spatial}.rds"
)

clear_outputs <- function() {
  util$clear_paths(opath)
}

#--- Function that creates circularity inputs

call_extraction_table <- function(year,
                                  bus_data = c("cbp_imp", "cbp_raw", "infogroup"),
                                  ilevel = c("det", "sum", "sec"),
                                  cluster_subset = NULL,
                                  from_cache = FALSE,
                                  to_cache = FALSE,
                                  overwrite = FALSE,
                                  spatial = TRUE) {

  bus_data <- match.arg(bus_data)
  ilevel <- match.arg(ilevel)
  
  cache_path <- glue(opath$circularity_)

  if (from_cache == TRUE){
    if (file.exists(cache_path)) {
      log_debug(paste("read from cache", cache_path))
      return(readRDS(cache_path))
    } else {
      log_warn("Cached version does not exist")
      from_cache = FALSE
    }
  } 

  if (from_cache == FALSE){
  
    factor_list <- place_io$call_outsupdem(year = year,
                                           ilevel = ilevel,
                                           bus_data = bus_data) |>
      mutate(net_supply = pmax(supply - demand, 0),
             net_demand = pmax(demand - supply, 0)) |>
      rename(gross_output = output, intermediate_supply = supply, intermediate_demand = demand)
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
        across(c(gross_output, intermediate_supply, intermediate_demand, net_supply, net_demand), sum)
      )
    
    df <- inner_join(df, total_output, by = "place")
    
    if (spatial){
      geot <- geog$call_geog(year = year)
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


