
# This script generates circularity metrics

# R libraries ----
library(logger)
log_threshold(DEBUG)
library(arrow)
library(tidyverse)
library(glue)
library(REAT)

# R scripts ----
source("R/basic_utilities.R", local = (util <- new.env()))
source("R/place_output.R", local = (place_output <- new.env()))

# Data objects ----
ipath <- list(
  # data dependencies
)

opath <- list(
  # data products
  circularity_ = "data/circularity/circularity_{year}_{ilevel}_{class_system}_{paradigm}_{bus_data}_{cluster_level}_{cbsa}_{clust_sub}_{spatial}.rds"
)

clear_outputs <- function() {
  util$clear_paths(opath)
}


# utility ----

dfcol2matrix <- function(data_frame, 
                         indicator_name, 
                         place_name = "place"){
  df <- data_frame[[indicator_name]] %>% 
    as.numeric() %>% 
    matrix(nrow = 1) %>% 
    `rownames<-`(indicator_name) %>%
    `colnames<-`(data_frame[[place_name]])
  return(df)
}

# functions ----

# importance of intermediate production relative to the size of local economy
production_capacity <- function(gross_output_matrix,
                                intermediate_supply_matrix){
  df <-  rowSums(t(intermediate_supply_matrix))%*%diag(1/as.vector(rowSums(t(gross_output_matrix)))) %>% 
    `colnames<-`(colnames(gross_output_matrix)) %>% 
    {as.data.frame.table(.)} %>%
    {subset(., select = 2:3)} %>%
    `colnames<-`(c("place", "production_capacity"))
  return(df)
}

# importance of intermediate exports relative to the size of local economy
trade_capacity <- function(gross_output_matrix,
                           net_supply_matrix){
  df <-   rowSums(t(net_supply_matrix))%*%diag(1/as.vector(rowSums(t(gross_output_matrix)))) %>%
    `colnames<-`(colnames(gross_output_matrix)) %>% 
    {as.data.frame.table(.)} %>%
    {subset(., select = 2:3)} %>%
    `colnames<-`(c("place", "trade_capacity"))
  return(df)
}

# share of intermediate production is used domestically
retention <- function(gross_output_matrix,
                      intermediate_supply_matrix,
                      net_supply_matrix){
  pc <- production_capacity(gross_output_matrix, intermediate_supply_matrix)
  tc <- trade_capacity(gross_output_matrix, net_supply_matrix)
  df <- inner_join(pc, tc, by = "place")
  df$retention <- (1 - (df$trade_capacity/df$production_capacity))
  df <- subset(df, select = c("place", "retention"))
  return(df)
}

# importance of intermediate input needs relative to the total size of local economy
production_dependency <- function(gross_output_matrix,
                                  intermediate_demand_matrix){
  df <-  rowSums(t(intermediate_demand_matrix))%*%diag(1/as.vector(rowSums(t(gross_output_matrix)))) %>% 
    `colnames<-`(colnames(gross_output_matrix)) %>% 
    {as.data.frame.table(.)} %>%
    {subset(., select = 2:3)} %>%
    `colnames<-`(c("place", "production_dependency"))
  return(df)
}

# importance of intermediate imports relative to the size of local economy
trade_dependency <- function(gross_output_matrix,
                             net_demand_matrix){
  df <-   rowSums(t(net_demand_matrix))%*%diag(1/as.vector(rowSums(t(gross_output_matrix)))) %>%
    `colnames<-`(colnames(gross_output_matrix)) %>% 
    {as.data.frame.table(.)} %>%
    {subset(., select = 2:3)} %>%
    `colnames<-`(c("place", "trade_dependency"))
  return(df)
}

# share of intermediate input needs are satisfied domestically
autonomy <- function(gross_output_matrix,
                     intermediate_demand_matrix,
                     net_demand_matrix){
  pd <- production_dependency(gross_output_matrix, intermediate_demand_matrix)
  td <- trade_dependency(gross_output_matrix, net_demand_matrix)
  df <- inner_join(pd, td, by = "place")
  df$autonomy <- (1 - (df$trade_dependency/df$production_dependency))
  df <- subset(df, select = c("place", "autonomy"))
  return(df)
}

# net flow of intermediate trade relative to the size of local economy
trade_balance <- function(gross_output_matrix,
                          intermediate_supply_matrix,
                          intermediate_demand_matrix){
  net_supply <- pmax(intermediate_supply_matrix - intermediate_demand_matrix, 0)
  net_demand <- pmax(intermediate_demand_matrix - intermediate_supply_matrix, 0)
  df <-  (rowSums(t(net_supply))-rowSums(t(net_demand)))%*%diag(1/as.vector(rowSums(t(gross_output_matrix)))) %>% 
    `colnames<-`(colnames(gross_output_matrix)) %>% 
    {as.data.frame.table(.)} %>%
    {subset(., select = 2:3)} %>%
    `colnames<-`(c("place", "trade_balance"))
  return(df)
}

# importance of trade in intermediates relative to the size of local economy
trade_openness <- function(gross_output_matrix,
                           intermediate_supply_matrix,
                           intermediate_demand_matrix){
  net_supply <- pmax(intermediate_supply_matrix - intermediate_demand_matrix, 0)
  net_demand <- pmax(intermediate_demand_matrix - intermediate_supply_matrix, 0)
  df <-  (rowSums(t(net_supply))+rowSums(t(net_demand)))%*%diag(1/as.vector(rowSums(t(gross_output_matrix)))) %>% 
    `colnames<-`(colnames(gross_output_matrix)) %>% 
    {as.data.frame.table(.)} %>%
    {subset(., select = 2:3)} %>%
    `colnames<-`(c("place", "trade_openness"))
  return(df)
}

circularity_metrics <- function(gross_output_matrix,
                                intermediate_supply_matrix,
                                intermediate_demand_matrix,
                                net_demand_matrix,
                                net_supply_matrix){
  pc <- production_capacity(gross_output_matrix, intermediate_supply_matrix)
  tc <- trade_capacity(gross_output_matrix, net_supply_matrix)
  rt <- retention(gross_output_matrix, intermediate_supply_matrix, net_supply_matrix)
  pd <- production_dependency(gross_output_matrix, intermediate_demand_matrix)
  td <- trade_dependency(gross_output_matrix, net_demand_matrix)
  at <- autonomy(gross_output_matrix, intermediate_demand_matrix, net_demand_matrix)
  tb <- trade_balance(gross_output_matrix, intermediate_supply_matrix, intermediate_demand_matrix)
  to <- trade_openness(gross_output_matrix, intermediate_supply_matrix, intermediate_demand_matrix)
  df <- bind_cols(pc, tc, rt, pd, td, at, tb, to, .name_repair = "minimal") %>% 
    {.[!duplicated(as.list(.))]}
  return(df)
}

# tables ----

# call dataframe table of circularity
call_circularity_metrics <- function(year,
                                     ilevel = c("det", "sum", "sec"),
                                     class_system = c("industry", "commodity"),
                                     paradigm = c("factor", "domestic", "capital"),
                                     bus_data = c("cbp_imp", "cbp_raw", "infogroup"),
                                     verbose = FALSE,
                                     cluster_level = c("sec", "sum", "det"),
                                     cbsa = FALSE,
                                     cluster_subset = NULL,
                                     spatial = TRUE){

  ilevel <- match.arg(ilevel)
  class_system <- match.arg(class_system)
  paradigm <- match.arg(paradigm)
  bus_data <- match.arg(bus_data)
  cluster_level <- match.arg(cluster_level)

  if (is.null(cluster_subset)){
    clust_sub = "NULL"
  } else {
    clust_sub = cluster_subset
  }
  
  cache_path <- glue(opath$circularity_)
  if (file.exists(cache_path)) {
    log_debug(paste("read from cache", cache_path))
    return(readRDS(cache_path))
  }
  
  fl <- place_output$call_factor_list(year = year,
                                      class_system = class_system,
                                      paradigm = paradigm,
                                      ilevel = ilevel,
                                      bus_data = bus_data,
                                      cbsa = cbsa,
                                      verbose = verbose) %>% 
    left_join(., bea_io$call_intra_level_concordance(year = year, cluster_level = cluster_level), by = "indcode")
  if (!is.null(cluster_subset)){
    fl <- fl[grepl(cluster_subset, fl[[place_output$short2long(cluster_level)]]), ]
  }

  cm <- circularity_metrics(gross_output_matrix = fl[c("indcode", "place", "gross_output")] %>% util$long2matrix(), 
                            intermediate_supply_matrix = fl[c("indcode", "place", "intermediate_supply")] %>% util$long2matrix(), 
                            intermediate_demand_matrix = fl[c("indcode", "place", "intermediate_demand")] %>% util$long2matrix(), 
                            net_demand_matrix = fl[c("indcode", "place", "net_supply")] %>% util$long2matrix(), 
                            net_supply_matrix = fl[c("indcode", "place", "net_demand")] %>% util$long2matrix())
  
  df <- fl[, 2:7] %>% 
    {aggregate(.[sapply(.,is.numeric)], list(.[["place"]]), FUN=sum)} %>% 
    `colnames<-`(c("place", names(.)[-1])) %>% 
    inner_join(., cm, by = "place", copy = TRUE)

  if (spatial){
    geot <- geog$call_geog(year = year, cbsa = cbsa)
    df <- inner_join(geot, df, by = "place", copy = TRUE)
  }
  
  log_debug(paste("save to cache", cache_path))
  saveRDS(df, util$mkdir(cache_path))

  return(df)
}

# Tests ----

test_circ <- function(){
  go <- place_output$call_output(2012, class_system = "commodity") %>% util$long2matrix() 
  is <- place_output$call_intermediate(2012, schedule = "supply", paradigm = "domestic", class_system = "commodity") %>% util$long2matrix() %>% {.[1,,drop=F]}
  id <- place_output$call_intermediate(2012, schedule = "demand", paradigm = "domestic", class_system = "commodity") %>% util$long2matrix() %>% {.[1,,drop=F]}
  test <- circ$circularity_metrics(gross_output_matrix = go, intermediate_supply_matrix = is, intermediate_demand_matrix = id)
  test <- call_circularity_metrics(year = 2012, cluster_subset = "^11", class_system = "commodity", paradigm = "domestic", ilevel = "det")
  test <- call_circularity_metrics(year = 2012, class_system = "commodity", paradigm = "domestic", ilevel = "det", cluster_level = "det", cluster_subset = NULL)
  # cor(test[,-1])
  # summary(st_drop_geometry(test[,8:21]))
  # st_drop_geometry(test[,8:21]) %>% cor(use = "complete.obs")
}








