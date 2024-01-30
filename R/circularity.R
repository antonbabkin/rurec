
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
                           intermediate_supply_matrix,
                           intermediate_demand_matrix){
  net_supply <- pmax(intermediate_supply_matrix - intermediate_demand_matrix, 0)
  df <-  rowSums(t(net_supply))%*%diag(1/as.vector(rowSums(t(gross_output_matrix)))) %>% 
    `colnames<-`(colnames(gross_output_matrix)) %>% 
    {as.data.frame.table(.)} %>%
    {subset(., select = 2:3)} %>%
    `colnames<-`(c("place", "trade_capacity"))
  return(df)
}

# share of intermediate production is used domestically
retention <- function(gross_output_matrix,
                      intermediate_supply_matrix,
                      intermediate_demand_matrix){
  pc <- production_capacity(gross_output_matrix, intermediate_supply_matrix)
  tc <- trade_capacity(gross_output_matrix, intermediate_supply_matrix, intermediate_demand_matrix)
  df <- inner_join(pc, tc, by = "place")
  df$retention <- df$trade_capacity/df$production_capacity
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
                             intermediate_supply_matrix,
                             intermediate_demand_matrix){
  net_demand <- pmax(intermediate_demand_matrix - intermediate_supply_matrix, 0)
  df <-  rowSums(t(net_demand))%*%diag(1/as.vector(rowSums(t(gross_output_matrix)))) %>% 
    `colnames<-`(colnames(gross_output_matrix)) %>% 
    {as.data.frame.table(.)} %>%
    {subset(., select = 2:3)} %>%
    `colnames<-`(c("place", "trade_dependency"))
  return(df)
}

# share of intermediate input needs are satisfied domestically
autonomy <- function(gross_output_matrix,
                     intermediate_supply_matrix,
                     intermediate_demand_matrix){
  pd <- production_dependency(gross_output_matrix, intermediate_demand_matrix)
  td <- trade_dependency(gross_output_matrix, intermediate_supply_matrix, intermediate_demand_matrix)
  df <- inner_join(pd, td, by = "place")
  df$autonomy <- df$trade_dependency/df$production_dependency
  df <- subset(df, select = c("place", "autonomy"))
  return(df)
}

# net flow of intermediate trade relative to the size of local economy
trade_balance <- function(gross_output_matrix,
                          intermediate_supply_matrix,
                          intermediate_demand_matrix){
  net_supply <- pmax(intermediate_supply_matrix - intermediate_demand_matrix, 0)
  net_demand <- pmax(intermediate_demand_matrix - intermediate_supply_matrix, 0)
  df <-  (rowSums(t(net_demand))-rowSums(t(net_supply)))%*%diag(1/as.vector(rowSums(t(gross_output_matrix)))) %>% 
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
  df <-  (rowSums(t(net_demand))+rowSums(t(net_supply)))%*%diag(1/as.vector(rowSums(t(gross_output_matrix)))) %>% 
    `colnames<-`(colnames(gross_output_matrix)) %>% 
    {as.data.frame.table(.)} %>%
    {subset(., select = 2:3)} %>%
    `colnames<-`(c("place", "trade_openness"))
  return(df)
}

circularity_metrics <- function(gross_output_matrix,
                                intermediate_supply_matrix,
                                intermediate_demand_matrix){
  pc <- production_capacity(gross_output_matrix, intermediate_supply_matrix)
  tc <- trade_capacity(gross_output_matrix, intermediate_supply_matrix, intermediate_demand_matrix)
  rt <- retention(gross_output_matrix, intermediate_supply_matrix, intermediate_demand_matrix)
  pd <- production_dependency(gross_output_matrix, intermediate_demand_matrix)
  td <- trade_dependency(gross_output_matrix, intermediate_supply_matrix, intermediate_demand_matrix)
  at <- autonomy(gross_output_matrix, intermediate_supply_matrix, intermediate_demand_matrix)
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
                                     trim = "^(60|66|69|78)|(999)$", 
                                     spatial = TRUE){
  df <- place_output$call_extraction_table(year = year,
                                           ilevel = ilevel,
                                           class_system = class_system,
                                           paradigm = paradigm,
                                           bus_data = bus_data,
                                           verbose = verbose,
                                           cluster_level = cluster_level,
                                           cbsa = cbsa,
                                           cluster_subset = cluster_subset,
                                           trim = trim,
                                           spatial = spatial)
  cm <- circularity_metrics(gross_output_matrix = dfcol2matrix(df, "gross_output"), 
                            intermediate_supply_matrix = dfcol2matrix(df, "intermediate_supply"), 
                            intermediate_demand_matrix = dfcol2matrix(df, "intermediate_demand"))
  df <- inner_join(df, cm, by = "place")
  return(df)
}

# Tests ----

test_circ <- function(){
  go <- place_output$call_output(2012, class_system = "commodity") %>% util$long2matrix() 
  is <- place_output$call_intermediate(2012, schedule = "supply", paradigm = "domestic", class_system = "commodity") %>% util$long2matrix()
  id <- place_output$call_intermediate(2012, schedule = "demand", paradigm = "domestic", class_system = "commodity") %>% util$long2matrix()
  circularity_metrics(gross_output_matrix = go, intermediate_supply_matrix = is, intermediate_demand_matrix = id)
  test <- call_circularity_metrics(year = 2012, cluster_subset = "^11", class_system = "commodity", paradigm = "domestic", ilevel = "det")
  # cor(test[,-1])
  # st_drop_geometry(test[,8:21]) %>% cor(use = "complete.obs")
}








