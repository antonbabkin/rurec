# Function repository

# Load and attach necessary packages
library(rprojroot)
library(fs)
library(rlog)
library(reticulate)
library(tidyverse)
library(magrittr)
library(sf)
library(geosphere)
library(spdep)
library(reshape2)
library(parallel)
library(viridis)
library(REAT)
library(arrow)


# Display start time
log_info("Define functions start")

# Directory to put new data outputs from R functions
if (!file.exists(file.path(find_rstudio_root_file(), "data", "robjs"))) {
  dir.create(file.path(find_rstudio_root_file(), "data", "robjs"))
}

#simple independent utility functions in base R for cohesion, conversion, or manipulation
source(file.path(find_rstudio_root_file(), "R", "basic_utilities.R"))

#IO RAS process
source(file.path(find_rstudio_root_file(), "nbs", "io_analysis.R"))

#select if using data from disk in warehouse or built from pubdata python modules
warehouse = F
if (warehouse){
  source(file.path(find_rstudio_root_file(), "nbs", "warehouse.R"))
} else{
  source(file.path(find_rstudio_root_file(), "rurec", "pubdata.R"))
}

#turn off S2 spherical geometry
sf_use_s2(FALSE)

# BEA IO tables
source("R/dataprep_bea_io.R", local = (bea_io <- new.env()))

### join ag classes with BEA industries
ag2bea <- function(afbd = file.path(find_rstudio_root_file(),"data","raw","afbd.csv")
){
  afbd <- read.csv(afbd)
  afbd$NAICS <- NA
  afbd$naics <- afbd[[1]]
  conc <- ilevel_concord(ilevel = "det")
  n <- names(conc)[1]
  for(i in unique(conc$NAICS)){
    x <- paste0("^", i) %>% 
      {afbd[grepl(., afbd$naics), "naics"]} %>% 
      unique() %>% 
      unlist()
    if(length(x)<1) {next}
    afbd[afbd$naics %in% x, ]$NAICS <- i
  }
  df <- left_join(afbd, conc, by = "NAICS")
}


# Call up and clean RUCC data available for years 2013, 2003, 1993, 1983, 1974
call_rucc <- function(year){
  df <- get_ruc_df() %>% filter(RUC_YEAR == year2rucc(year))
  df$place <- df$FIPS
  return(df)
}



#Agglomerate NAICS and BEA concordance by year and industry specificity (sector, summary, or detail)
place_industry_economy_long <- function(year, 
                                        cbp_scale = c("county", "state", "us"), 
                                        imputed = TRUE,
                                        ...){
  cbp_scale <- match.arg(cbp_scale)
  conc <- ilevel_concord(ilevel = list(...)$ilevel)
  n <- names(conc)[1]
  cbp_dat <- call_cbp(year = year, imputed = imputed, cbp_scale = cbp_scale, ...)
  if (cbp_scale != "county" & isFALSE(imputed)){
    cbp_dat <- cbp_dat[cbp_dat$lfo == "-", ]
  }
  x <- left_join(cbp_dat, conc, by = "NAICS") 
  x <- x %>% .[!is.na(.[dim(.)[2]] ),] %>% 
    group_by(place, .[dim(x)[2]]) %>%
    summarise(across(where(is.numeric), sum), .groups = 'drop') %>%
    as.data.frame()
  if (cbp_scale != "us"){
    x <- x %>%
      group_by(place) %>%
      arrange(factor(x[[2]], levels = unique(conc[[1]])), .by_group = TRUE) %>%
      as.data.frame()
      df <- x %>% pivot_wider(id_cols = n, names_from = "place", values_from = c("emp", "qp1", "ap", "est"), names_sep = ".", values_fill = 0)
      df <- df %>% pivot_longer(-n, names_to = c(".value", "place"), names_pattern = "([^\\.]*)\\.*(\\d+)") %>% as.data.frame()
      df <- df %>% group_by(df[[2]]) %>% arrange(factor(df[[1]], levels = unique(conc[[1]])), .by_group = TRUE)
    names(df)[1] <- "indcode"
  } else {
    df <- x %>% arrange(factor(x[[2]], levels = unique(conc[[1]])), .by_group = TRUE)
    names(df)[2] <- "indcode"
  }
  df <- df[1:6]
  return(df)
}

#Generate industry activity ("ap", "emp", "qp1", or "est") by county from CBP in terms of BEA industry codes ("det", "sum", or "sec") for any available year
industry_activity_by_place <- function(year, 
                                     output_metric = c("ap", "emp", "qp1", "est"), 
                                     ...){
  output_metric <- match.arg(output_metric)
  df <- place_industry_economy_long(year = year, ...) %>% 
    .[, c("indcode", "place", output_metric)] %>% 
    pivot_wider(id_cols = "indcode", names_from = "place", values_from = output_metric) %>% 
    as.data.frame()
  rownames(df) <- df$indcode
  df <- df[, !colnames(df) %in% c("indcode","place"), drop=F] %>% 
    as.matrix()
  return(df)
}

############ Derive the national level, industry specific, payroll share of gross output by year and industry scale
payroll_share <- function(year,
                          ilevel = c("det", "sum", "sec"), 
                          county_totals = TRUE,
                          ...){
  ilevel <- match.arg(ilevel)
  indout <- call_use_table(year, ilevel) %>% 
    .["T018", !colnames(.) %in% grep("^(F|T)[0-9]", colnames(.), value = TRUE), drop = FALSE]*1000000
  if(ilevel == "sum"){
    indout <-  vector_collapse(indout, grep("^336", colnames(indout), value = TRUE), "336") %>% 
      vector_collapse(., grep("^541", colnames(.), value = TRUE), "541") %>%
      vector_collapse(., grep("^(HS|ORE)", colnames(.), value = TRUE), "531") 
  }
  if(ilevel == "det"){
    indout <-  vector_collapse(indout, grep("^23", colnames(indout), value = TRUE), "23") %>%
      vector_collapse(., grep("^531", colnames(.), value = TRUE), "531") 
  }
  conc <- ilevel_concord(ilevel)
  if (county_totals){
    cbp_dat <- call_cbp(year = year, ...) %>% {aggregate(.$ap, list(.$NAICS), FUN=sum)} %>% `colnames<-`(c("NAICS", "ap"))
    ap <- left_join(cbp_dat, conc, by = "NAICS") %>% .[!is.na(.[[dim(.)[2]]]), ]
  } else {
    cbp_dat <- call_cbp(year = year, cbp_scale = "us", imputed = FALSE)
    ap <- left_join(cbp_dat, conc, by = "NAICS") %>% .[!is.na(.[[dim(.)[2]]]) & .$lfo == "-", ]
  }
  n <- names(ap)[dim(ap)[2]]
  ap <- ap %>% 
    group_by(.[dim(.)[2]]) %>%
    summarise(across(where(is.numeric), sum), .groups = 'drop') %>% 
    .[c("ap", n )] %>% 
    pivot_wider(names_from = n, values_from = c("ap"), names_sep = ".", values_fill = 0) %>% 
    as.matrix()*1000
  rownames(ap) <- "ap"
  intind <- intersect(colnames(ap), colnames(indout))
  df <- (ap["ap", intind , drop = FALSE] / indout["T018", intind , drop = FALSE]) 
  rownames(df) <- "psogo"
  return(df)
}

#add example of what payroll share does (this is not a truth correction but instead a data correction)
#We only see payroll at county level in CBP and what we want is output by county
#For each industry, there exist a ratio of annual payroll for a given level of output at the national level
#In order to scale CBP payroll correctly (given known suppression) we aggregate county level payroll by industry and divide by the BEA's level of total industry output
#Assuming a constant proportion of wage to output across space, this national level ratio is applied to each observed county-level industry payroll  
#For example, suppose BEA total output in mining was 10 billion and CBP had 4 counties with mining payroll with 1 billion, 1 billion, 1 billion, and 2 billion in annual payroll respectively, then the payroll share for mining would be (1+1+1+2)/10 = 0.5  


# Call up InfoGroup county level 6 digit NAICS employment and sales ($1,000 of dollars) available for years 1997:2017
call_infogroup <- function(year, 
                           data_dir = file.path("data", "infogroup"),
                           ...){
  year <- year2infogroup(year)
  fp <- file.path(find_rstudio_root_file(), data_dir)
  df <- fp %>% {file.path(., grep(year, list.files(.), value = TRUE))} %>% readRDS() %>% .[!is.na(.$st) & !is.na(.$cty) & !is.na(.$naics) & !is.na(.$sales), ]
  df$place <- paste0(df$st, df$cty)
  return(df)
}

#Agglomerate NAICS and BEA concordance (sector, summary, or detail) by year for InfoGroup
infogroup_bea_long <- function(year,
                               ...){
  info_dat <- call_infogroup(year, ...)
  info_dat$NAICS <- NA
  conc <- ilevel_concord(ilevel = list(...)$ilevel)
  n <- names(conc)[1]
  for(i in unique(conc$NAICS)){
    x <- paste0("^", i) %>% 
      {info_dat[grepl(., info_dat$naics), "naics"]} %>% 
      unique() %>% 
      unlist()
    info_dat[info_dat$naics %in% x, ]$NAICS <- i
  }
  df <- left_join(info_dat, conc, by = "NAICS") %>% 
    {.[!is.na(.[n]),]} %>% 
    group_by(place, .[n]) %>%
    summarise(across(where(is.numeric), sum), .groups = 'drop') %>%
    pivot_wider(id_cols = n, 
                names_from = "place", 
                values_from = c("emp", "sales"), 
                names_sep = ".", 
                values_fill = 0) %>% 
    pivot_longer(cols = -n, 
                 names_to = c(".value", "place"), 
                 names_pattern = "([^\\.]*)\\.*(\\d+)") %>% 
    group_by(place) %>% 
    arrange(factor(.[[n]], levels = unique(conc[[n]])), .by_group = TRUE) %>% 
    as.data.frame() %>% 
    .[,1:4]
  names(df)[1] <- "indcode"
  return(df)
}

#Generate industry sales or employment by county from InfoGroup in BEA industry codes ("det", "sum", or "sec") for any available year
infogroup_by_place <- function(year,
                               output_metric = c("sales", "emp"),
                               ...){
  output_metric <- match.arg(output_metric)
  df <- infogroup_bea_long(year = year, ...) %>% 
    .[, c("indcode", "place", output_metric)] %>% 
    pivot_wider(id_cols = "indcode", names_from = "place", values_from = output_metric) %>% 
    as.data.frame()
  rownames(df) <- df$indcode
  df <- df[, !colnames(df) %in% c("indcode","place"), drop=F] %>% 
    as.matrix()
  return(df)
}

############ Derive national level sales share of gross output by year
infogroup_sales_share <- function(year,
                                  ilevel = c("det", "sum", "sec"), 
                                  ...){
  ilevel <- match.arg(ilevel)
  indout <- call_use_table(year, ilevel) %>% 
    .["T018", !colnames(.) %in% grep("^(F|T)[0-9]", colnames(.), value = TRUE), drop = FALSE]*1000000
  if(ilevel == "sum"){
    indout <-  vector_collapse(indout, grep("^336", colnames(indout), value = TRUE), "336") %>% 
      vector_collapse(., grep("^541", colnames(.), value = TRUE), "541") %>%
      vector_collapse(., grep("^(HS|ORE)", colnames(.), value = TRUE), "531") 
  }
  if(ilevel == "det"){
    indout <- vector_collapse(indout, grep("^23", colnames(indout), value = TRUE), "23") %>%
      vector_collapse(., grep("^531", colnames(.), value = TRUE), "531") 
  }
  info_dat <- call_infogroup(year = year, ilevel = ilevel, ...) %>% .[!.$st %in% c("78"),] %>%
    {aggregate(.$sales, list(.$naics), FUN=sum)} %>% 
    `colnames<-`(c("naics", "sales")) %>% 
    as_tibble()
  info_dat$NAICS <- NA
  conc <- ilevel_concord(ilevel = ilevel)
  n <- names(conc)[1]
  for(i in unique(conc$NAICS)){
    x <- paste0("^", i) %>% 
      {info_dat[grepl(., info_dat$naics), "naics"]} %>% 
      unique() %>% 
      unlist()
    info_dat[info_dat$naics %in% x, ]$NAICS <- i
  }
  df <- left_join(info_dat, conc, by = "NAICS") %>% 
    {.[!is.na(.[n]),]} %>% 
    group_by(.[n]) %>%
    summarise(across(where(is.numeric), sum), .groups = 'drop') %>%
    pivot_wider(names_from = n, 
                values_from = c("sales"), 
                names_sep = ".", 
                values_fill = 0) %>% 
    as.matrix()*1000
  rownames(df) <- "sales"
  intind <- intersect(colnames(df), colnames(indout)) %>% intersect(unique(conc[[n]]), .)
  df <- (df["sales", intind, drop = FALSE] / indout["T018", intind, drop = FALSE]) 
  rownames(df) <- "ssogo"
  return(df)
}


############ Derive adjusted national ag gross industry output (in thousands of dollars)
total_ag_industry_output <- function (year,
                                      ...){
  ilevel = "det"
  farm_sales <- call_agoutput(year, geo_level = "national")
  indout <- call_use_table(year, ilevel)
  indsup <- call_supply_table(year, ilevel)
  agtax <- indout["T00OTOP", colnames(farm_sales)]
  agsub <- indout["T00SUB", colnames(farm_sales)]
  aginv <- indout[colnames(farm_sales), "F03000"]
  df <- farm_sales
  for(i in colnames(farm_sales)){
    commod_indust_ratio <- indsup[i, i] / indsup["T017", i]
    indust_commod_ratio <- indsup[i, i] / indsup[i, "T007"]
    adj_agtax <- commod_indust_ratio * agtax[i]
    adj_agsub <- commod_indust_ratio * agsub[i] 
    adj_aginv <- indust_commod_ratio * aginv[i] 
    df[, i] <- farm_sales[, i] + (adj_agtax + adj_agsub + adj_aginv)*1000
  }
  return(df)
}
# #error between ag census and bea industry output adjusting for tax, subsidy and inventory 
# total_ag_industry_output(year) / (call_supply_table(year, ilevel)["T017", colnames(call_agoutput(year, geo_level = "national"))]*1000)


############ Derive national level AgCensus sales share of gross output by year
agsales_share <- function(year,
                          county_totals = TRUE,
                          ...){
  if (county_totals){
    farm_sales <- call_agoutput(year, geo_level = "county") %>% .[, sapply(., is.numeric)] %>% colSums() %>% t()
  } else {
    farm_sales <- call_agoutput(year, geo_level = "national")
  }
  indout <- call_use_table(year, ilevel = "det")["T018", colnames(farm_sales)]*1000 
  df <- farm_sales/indout
  rownames(df) <- "asogo"
  return(df)
}

############ Derive the Total Industry Output Matrix (in thousands of dollars)
total_industry_output <- function (year,
                                   ilevel = c("det", "sum", "sec"), 
                                   data_source = c("cbp", "infogroup"),
                                   ...){
  ilevel <- match.arg(ilevel)
  data_source <- match.arg(data_source)
  farm_sales <- call_agoutput(year, ...)
  fn <- colnames(farm_sales)[-c(1)]
  as <- agsales_share(year, ...)
  agout <- farm_sales %>% .[sapply(., is.numeric)] %>% {apply(., 1, function (x) {x / as})} %>% t() %>% as.data.frame()
  colnames(agout) <- fn
  agout$place <- farm_sales$place
  if(data_source == "cbp"){
    iout <- industry_activity_by_place(year = year, ilevel = ilevel, ...)
    ps <- payroll_share(year = year, ilevel = ilevel, ...) %>% .[, rownames(iout)[rownames(iout) %in% colnames(.)], drop = FALSE] 
    df <- apply(iout, 2, function (x) {x / ps})
    df[is.infinite(df)] = 0
    rownames(df) <- rownames(iout)
    df <- t(df) %>% as.data.frame()
    df$place <- rownames(df)
  }
  if(data_source == "infogroup"){
    df <- infogroup_by_place(year, ilevel = ilevel, ...) %>% {.[,!colnames(.) %in% grep("^(78)", colnames(.), value = TRUE)]}
    #To get Infogroup sales adjusted by BEA totals
    ss <- infogroup_sales_share(year, ilevel = ilevel, ...)
    df <- apply(df, 2, function (x) {x / ss})
    rownames(df) <- colnames(ss)
    df <- t(df) %>% as.data.frame()
    df$place <- rownames(df)
    df <- df %>% {.[,!colnames(.) %in% c(fn, "111CA")]}
  }
  df <- left_join(df, agout, by = "place")
  #note  methodological issues of double counting remain with mixing Agcensus and InfoGroup at sector level for non 113FF ag industries 
  if (ilevel == "sec") {
    df[["11"]] <- rowSums(df[, c("11", fn)], na.rm = T)
    df <- df %>% select(!all_of(c(fn)))
  } else if (ilevel == "sum") {
    df[["111CA"]] <- rowSums(df[, c(fn)], na.rm = T)
    df <- df %>% select(!all_of(c(fn))) %>% select("111CA", everything())
  } else if (ilevel == "det") {
    df <- df %>% select(all_of(fn), everything())
  }
  rownames(df) <- df$place
  df$place <- NULL
  df <- t(df)
  df[is.na(df)] = 0
  df[is.infinite(df)] = 0
  return(df)
}

# # BEA and InfoGroup industry coverage comparison
# conc <- ilevel_concord()
# info_dat <- call_infogroup(year) 
# info_dat$NAICS <- NA
# for(i in unique(conc$NAICS)){
#   x <- paste0("^", i) %>% {info_dat[grepl(., info_dat$naics), "naics"]} %>% unique() %>% unlist()
#   info_dat[info_dat$naics %in% x, ]$NAICS <- i
# }
# info_dat <- left_join(info_dat, conc, by = "NAICS")
# # InfoGroup industries with no BEA coverage
# info_dat[is.na(info_dat$NAICS), ]$naics %>% unique()
# # BEA industries with no InfoGroup coverage 
# setdiff(colnames(bea_ind), unique(info_dat$DETAIL))

# Aggregate industry output of the CBSA members in a cluster
cbsa_aggregate_industry_output <- function(year,
                                           ...){
  o <- total_industry_output(year, ...)
  c <- call_cbsa_concord(year, ...)
  c <- c[c$place %in% intersect(c$place, colnames(o)),]
  c <- data.frame(CBSA_CODE = c(c$CBSA_CODE, setdiff(colnames(o), c$place)), 
                  place = c(c$place, setdiff(colnames(o), c$place)))
  c <- c[order(c$CBSA_CODE), ]
  rownames(c) <- 1:nrow(c)
  x <- c$CBSA_CODE %>% unique()
  df <- data.frame(row.names = rownames(o))
  for(i in x){
    df[, i] <- rowSums(o[, c$place[c$CBSA_CODE == i], drop = FALSE])
  } 
  df <- as.matrix(df)
  df[is.na(df)] = 0
  return(df)
}





############ Call Gross Industry Output 
industry_output_matrix <- function(year,
                                   cbsa_clust = FALSE, 
                                   ...){
  if(isFALSE(cbsa_clust)){
    df <- total_industry_output(year, ...)
  } else {
    df <- cbsa_aggregate_industry_output(year, ...)
  }
  df <- df[(which(!is.na(rowSums(df)) & !rowSums(df) == 0)), ]
  return(df)
}

############ Call Gross Commodity Output
commodity_output_matrix <- function(year,
                                    ...){
  cm <- c_matrix(year, ...)
  o <- industry_output_matrix(year, ...)
  df <- cm[, rownames(o)] %*% o
  return(df)
}

#NFG!
############ Call Industry Factor Demand
industry_factor_demand_matrix <- function(year,
                                          ...){
  o <- industry_output_matrix(year, ...)
  dm <- d_matrix(year, ...)
  bm <- b_matrix(year, ...)
  tech_coef <- diag(as.vector(colSums(dm %*% bm))) %>% 
    `colnames<-`(colnames(bm)) %>% 
    `rownames<-`(colnames(bm)) 
  df <- tech_coef[rownames(o), rownames(o)] %*% o
  return(df)
}

############ Call Commodity Factor Demand
commodity_factor_demand_matrix <- function(year, 
                                           ...){
  o <- industry_output_matrix(year, ...)
  tech_coef <- b_matrix(year, ...)
  df <- tech_coef[, rownames(o)] %*% o
  return(df)
}

#NFG!
############ Call the National Factor Ratio of Industry Factor Demand to Gross Industry Output
industry_region_factor_ratio <- function(year,
                                         ...){
  fd <- industry_factor_demand_matrix(year, ...)
  io <- industry_output_matrix(year, ...)
  df <- rowSums(fd)/rowSums(io) %>% as.matrix()
  colnames(df) <- "factor_ratio"
  return(df)
}
#note: IFR can be derived from just B matrix and D matrix
# o <- industry_output_matrix(year, ...)
# dm <- d_matrix(year, ...)
# bm <- b_matrix(year, ...)
# tech_coef <- diag(as.vector(colSums(dm %*% bm))) %>% 
#   `colnames<-`(colnames(bm)) %>% 
#   `rownames<-`(colnames(bm)) 
# df <- tech_coef[,rownames(o)] %>% colSums() %>% as.matrix()


############ Call the National Factor Ratio of Commodity Factor Demand to Gross Commodity Output
commodity_region_factor_ratio <- function(year,
                                          ...){
  fd <- commodity_factor_demand_matrix(year, ...)
  co <- commodity_output_matrix(year, ...)
  df <- rowSums(fd)/rowSums(co) %>% as.matrix()
  colnames(df) <- "factor_ratio"
  return(df)
}

#NFG!
############ Call Industry Factor Supply
industry_factor_supply_matrix <- function(year,
                                          ...){
  io <- industry_output_matrix(year, ...)
  fr <- industry_region_factor_ratio(year, ...)
  df <- diag(as.vector(fr)) %*% io
  rownames(df) <- rownames(fr)
  return(df)
}

############ Call Commodity Factor Supply
commodity_factor_supply_matrix <- function(year,
                                           ...){
  co <- commodity_output_matrix(year, ...)
  fr <- commodity_region_factor_ratio(year, ...)
  df <- diag(as.vector(fr)) %*% co
  rownames(df) <- rownames(fr)
  return(df)
}

############ Call list of IO products
io_yeild_list <- function(year,
                          flow_class = c("industry", "commodity"),
                          net_class = c("factor", "gross"),
                          ...){
  flow_class <- match.arg(flow_class)
  net_class <- match.arg(net_class)
  if(flow_class == "industry"){
    gross_ouput <- industry_output_matrix(year, ...)
    factor_supply <- industry_factor_supply_matrix(year, ...)
    factor_demand <- industry_factor_demand_matrix(year, ...)
  } 
  if(flow_class == "commodity"){
    gross_ouput <- commodity_output_matrix(year, ...)
    factor_supply <- commodity_factor_supply_matrix(year, ...)
    factor_demand <- commodity_factor_demand_matrix(year, ...)
  }
  if(net_class == "factor"){
    net_demand <- pmax(factor_demand - factor_supply, 0)
    net_supply <- pmax(factor_supply - factor_demand, 0)
  }
  if(net_class == "gross"){
    net_demand <- pmax(factor_demand - gross_ouput, 0)
    net_supply <- pmax(gross_ouput - factor_demand, 0)
  }
  return(list("gross_ouput" = gross_ouput, 
              "factor_supply" = factor_supply, 
              "factor_demand" = factor_demand,  
              "net_demand" = net_demand, 
              "net_supply" = net_supply))
}

#need fix for concordance with commodities
#does it work with sum and sec level inputs?
#drop fips ending in 999?
############ Derive data frame of various detail level IO metrics across space with sector level grouping
io_yeild_distribution <- function(year,
                                  ...){
  bea_year <- year2bea(year)
  des <- data.frame("code" = names(get_sup(bea_year, "sec", FALSE)), 
                    "description" = names(get_sup(bea_year, "sec", TRUE)) ) %>% 
    .[.$code %in% unique(ilevel_concord("sec")[[1]]), ] %>% 
    `colnames<-`(c("SECTOR", "description")) %>% 
    cbind(. , "color" = viridis(length(unique(.$description)))) %>% 
    inner_join(., call_industry_concordance()[, c("SECTOR", "DETAIL")], by = "SECTOR") %>% 
    .[!duplicated(.), ]
  df <- io_yeild_list(year, ...)
  for (i in 1:length(df)){
    df[[i]] <- melt(df[[i]])
    names(df[[i]]) <- c("DETAIL", "place", names(df)[i])
    df[[i]]$place <- df[[i]]$place %>% formatC(width = 5, format = "d", flag = "0")
  }
  df <- lapply(df[-1], function(x){x[setdiff(names(x), Reduce(intersect, lapply(df, names)) )]} ) %>% 
    do.call(cbind, .) %>% 
    cbind(df[[1]], .) %>% 
    left_join(., des, by = "DETAIL")
  return(df)
}


#RAS trade matrix from factor supply and demand
ras_trade_lists <- function(factor_supply,
                            factor_demand, 
                            crosshaul = TRUE,
                            impedance_mat = NULL,
                            core_count = 10,
                            data_dir = file.path("data", "robjs"),
                            ...){
  
  pl <- intersect(colnames(factor_supply), colnames(factor_demand))
  if(!is.null(impedance_mat)){ 
    pl <- intersect(intersect(colnames(factor_supply), colnames(factor_demand)), intersect(colnames(impedance_mat), rownames(impedance_mat))) 
    impedance_mat <- impedance_mat[pl, pl]
  }
  fs <- factor_supply[, pl, drop=F]
  fd <- factor_demand[, pl, drop=F]
  if(isTRUE(crosshaul)){
    fsx <- fs
    fdx <- fd
  } else {
    fsx <- pmax(fs - fd, 0)
    fdx <- pmax(fd - fs, 0)
  }
  #Starting position of trade matrix 
  x <- list()
  y <- intersect(
    names(which(!is.na(rowSums(fdx)) & 
                  !rowSums(fdx) == 0 )), 
    names(which(!is.na(rowSums(fsx)) & 
                  !rowSums(fsx) == 0 )) )
  for (i in y){
    if(!is.null(impedance_mat)){
      x[[i]] <- (t(fsx[i, , drop=F]) %*% fdx[i, , drop=F]) * (impedance_mat)
    } else {
      x[[i]] <- (t(fsx[i, , drop=F]) %*% fdx[i, , drop=F])
    }
  }
  
  unlink(file.path(find_rstudio_root_file(), data_dir, "temp"), recursive = TRUE)
  dir.create(file.path(find_rstudio_root_file(), data_dir, "temp"))
  
  mclapply(names(x), function(i) {
    tf <- ras_trade_flows(x0 = x[[i]],
                          rs1 = fsx[i, , drop=F],
                          cs1 = fdx[i, , drop=F],
                          ...)$trade_matrix
    colnames(tf) = colnames(fdx[i, , drop=F])
    rownames(tf) = colnames(fsx[i, , drop=F])
    saveRDS(tf, file = file.path(find_rstudio_root_file(), data_dir, "temp", i))
    return(invisible(NULL))
  }, mc.cores = core_count)
  df <- lapply(file.path(find_rstudio_root_file(), data_dir, "temp", names(x)), readRDS)
  names(df) <- names(x)
  return(df)
}

############ Call matrices of commodity or industry imputed trade flows from RAS procedure (note: impedance is fixed)
call_imputed_tradeflows <- function(year,
                                    flow_class = c("industry", "commodity"),
                                    ilevel = c("det", "sum", "sec"),
                                    cbsa_clust = FALSE,
                                    impedance_mat = NULL,
                                    ...){
  if(!"ras_trade_flows" %in% c(lsf.str())){
    source(file.path(find_rstudio_root_file(), "nbs", "io_analysis.R"))
  }
  ilevel <- match.arg(ilevel)
  flow_class <- match.arg(flow_class)
  
  if(flow_class == "industry"){
    fs <- industry_factor_supply_matrix(year = year, ilevel = ilevel, cbsa_clust = cbsa_clust, ...)
    fd <- industry_factor_demand_matrix(year = year, ilevel = ilevel, cbsa_clust = cbsa_clust, ...)
  }
  if(flow_class == "commodity"){
    fs <- commodity_factor_supply_matrix(year = year, ilevel = ilevel, cbsa_clust = cbsa_clust, ...)
    fd <- commodity_factor_demand_matrix(year = year, ilevel = ilevel, cbsa_clust = cbsa_clust, ...)
  }
  df <- ras_trade_lists(factor_supply = fs,
                        factor_demand = fd, 
                        impedance_mat = impedance_mat, 
                        ...)
  return(df)
}


#needs refined
# load_tradeflows <- function(year,
#                             ilevel,
#                             industryflow = TRUE,
#                             impedance_mat = NULL,
#                             crosshaul = TRUE,
#                             data_dir = file.path("data", "robjs"),
#                             impedance_call = NULL, 
#                             ...){
#   
#   if(!is.null(impedance_mat) & 
#      is.null(impedance_call)
#   ){
#     warning("Impedance is applied without a specific description to save/load")
#   }
#   rasf <- paste0("ras", "_", ilevel,"class", "_", cbp_year, "cbp", "_", if(isTRUE(industryflow)){"industries"}else{"commodities"}, "_", if(isTRUE(crosshaul)){"xhaul"}else{"nohaul"}, "_", if(is.null(subsectors)){"all"}else{subsectors}, "sectors", "_", if(is.null(impedance_mat)){"NA"}else{impedance_call}, "impedance")
#   if (file.exists(file.path(find_rstudio_root_file(), data_dir, rasf) ) ){ 
#     df <- readRDS(file.path(find_rstudio_root_file(), data_dir, rasf))
#   } else {
#     df <- call_imputed_tradeflows(year,
#                                   industryflow,
#                                   impedance_mat,
#                                   subsectors,
#                                   crosshaul,
#                                   ...)
#     saveRDS(df,  file = file.path(find_rstudio_root_file(), data_dir, rasf) )
#   }
#   return(df)
# }


#develop better cache and directory system
### Find minimal distance impedance trade flows 
min_imp_ras <- function(factor_supply, 
                        factor_demand,
                        imp_funct = "gaus_impedance_mat",
                        crosshaul = FALSE,
                        dir_location = file.path(find_rstudio_root_file(), "data", "robjs", "trade_base", "tester"),
                        min_d = 25,
                        max_d = 2000,
                        step_d = 25,
                        tol = 1e-0,
                        verbose = TRUE,
                        ...){
  g <- dist_matc(...)
  fs <- intersect(colnames(factor_supply), colnames(g)) %>% factor_supply[, ., drop=F]
  fd <- intersect(colnames(factor_demand), colnames(g)) %>% factor_demand[, ., drop=F]
  pl <- intersect(intersect(colnames(fs), colnames(fd)), intersect(colnames(g), rownames(g))) 
  
  if(isTRUE(crosshaul)){
    fsx <- fs
    fdx <- fd
  } else {
    fsx <- pmax(fs - fd, 0)
    fdx <- pmax(fd - fs, 0)
  }
  df <- data.frame("sector" = c(),
                   "impedance" = c(),
                   "ras_supply_dim" = c(),
                   "ras_demand_dim" = c(),
                   "iterations" = c(),
                   "rmse" = c(),
                   "mad" = c())
  y <- intersect(
    names(which(!is.na(rowSums(fdx)) & 
                  !rowSums(fdx) == 0 )), 
    names(which(!is.na(rowSums(fsx)) & 
                  !rowSums(fsx) == 0 )) )
  # unlink(dir_location, recursive = TRUE)
  if(!dir.exists(dir_location)){
    dir.create(dir_location)
  } 
  imprd <- file.path(find_rstudio_root_file(), "data", "robjs", paste0(imp_funct, "_range"))
  if(!dir.exists(imprd)){
    dir.create(imprd)
  } 
  imprf <- list.files(imprd)
  for(d in seq(min_d, max_d, by = step_d)){
    if(!d %in% imprf){
      temp_imp <- do.call(get(imp_funct), list(miles2meters(d)))
      saveRDS(temp_imp, file = file.path(imprd, d))
    }
  }
  # y <- setdiff(y, list.files(dir_location))
  for(i in y){
    for(d in seq(min_d, max_d, by = step_d)){
      print(paste("Industry:", i, " Distance:", d))
      impedance_mat <- readRDS(file.path(imprd, d))
      xs <- (t(fsx[i, , drop=F]) %*%  fdx[i, , drop=F]) * impedance_mat[pl, pl]
      tf <- ras_trade_flows(x0 = xs,
                            rs1 = fsx[i, , drop=F],
                            cs1 = fdx[i, , drop=F],
                            tol = tol, 
                            verbose = verbose)
      colnames(tf[[1]]) = colnames(fdx[i, , drop=F])
      rownames(tf[[1]]) = colnames(fsx[i, , drop=F])
      if (max(max(abs(rowSums(tf[[1]]) - fsx[i, , drop=F])) , max(abs(colSums(tf[[1]]) - fdx[i, , drop=F]))) < tol) {
        break
      }
    }
    saveRDS(tf[[1]], file = file.path(dir_location, i))
    df <- rbind(df, data.frame("sector" = i, 
                               "impedance" = d, 
                               "ras_supply_dim" = tf[["ras_supply_dim"]], 
                               "ras_demand_dim" = tf[["ras_demand_dim"]], 
                               "iterations" = tf[["iterations"]], 
                               "rmse" = tf[["rmse"]], 
                               "mad" = tf[["mad"]]))
    saveRDS(df, file = file.path(find_rstudio_root_file(), "data", "robjs", "trade_base", "sec_imp_list"))
  }
  dfl <- lapply(file.path(dir_location, y), readRDS)
  names(dfl) <- y
  out <- list("impedance_synopsis" = df, 
              "balanced_matrices" = dfl)
  return(out) 
}


############ Call matrices of commodity or industry imputed trade flows from RAS procedure (note: impedance type is fixed and not specified in file name scheme)
call_min_imp_ras <- function(year,
                             data_source = c("cbp", "infogroup"),
                             flow_class = c("industry", "commodity"),
                             ilevel = c("det", "sum", "sec"),
                             cbsa_clust = FALSE,
                             crosshaul = FALSE,
                             dir_location = file.path(find_rstudio_root_file(), "data", "robjs", "trade_base"),
                             step_d = 25,
                             tol = 1e-0,
                             ...){
  data_source <- match.arg(data_source)
  ilevel <- match.arg(ilevel)
  flow_class <- match.arg(flow_class)
  
  nm <- paste0("tally", "_", ilevel,"level", "_", year, "year", "_", flow_class, "class", "_", if(isTRUE(crosshaul)){"xhaul"}else{"nohaul"}, "_", "cbsa", if(isTRUE(cbsa_clust)){"clust"}else{"NA"}, "_", "by", step_d, "_", "tol", tol, "_", data_source)
  if(file.exists(file.path(dir_location, nm))){
    df <- readRDS(file.path(dir_location, nm))
  } else {
    if(!"ras_trade_flows" %in% c(lsf.str())){
      source(file.path(find_rstudio_root_file(), "nbs", "io_analysis.R"))
    }
    if(flow_class == "industry"){
      fs <- industry_factor_supply_matrix(year = year, ilevel = ilevel, cbsa_clust = cbsa_clust, data_source = data_source, ...)
      fd <- industry_factor_demand_matrix(year = year, ilevel = ilevel, cbsa_clust = cbsa_clust, data_source = data_source, ...)
    }
    if(flow_class == "commodity"){
      fs <- commodity_factor_supply_matrix(year = year, ilevel = ilevel, cbsa_clust = cbsa_clust, data_source = data_source, ...)
      fd <- commodity_factor_demand_matrix(year = year, ilevel = ilevel, cbsa_clust = cbsa_clust, data_source = data_source, ...)
    }
    df <- min_imp_ras(factor_supply = fs,
                      factor_demand = fd,
                      crosshaul = crosshaul,
                      step_d = step_d,
                      tol = tol,
                      ...)
    #need to add to tallyname
    saveRDS(df, file = file.path(dir_location, nm))
  }
  return(df)
}


### Aggregate list of sector specific trade flow matrices into single matrix
aggregate_sector_list <- function(sector_list){
  df <- Reduce('+', sector_list)
  return(df)
}

### Normalize trade flow matrix by total county outbound factor supply (rows) or inbound factor demand (cols)
normalize_tradeflow <- function(tradeflow_matrix, 
                                byrow = TRUE){
  if(isTRUE(byrow)){
    df <- tradeflow_matrix %>% sweep(1, rowSums(.), FUN="/")
  } else {
    df <- tradeflow_matrix %>% sweep(2, colSums(.), FUN="/")
  }
  return(df)
}

### Summarize data from trade flow matrix
inbound2outbound <- function(trade_matrix){
  df <- data.frame("place" = rownames(trade_matrix),
                   "inbound" = colSums(trade_matrix), 
                   "outbound" = rowSums(trade_matrix), 
                   "out2in" = rowSums(trade_matrix)/colSums(trade_matrix), 
                   "out_less_in" = rowSums(trade_matrix)-colSums(trade_matrix)
  )
  return(df)
}



############ Stacked Absorption Share 
stacked_absorption_share <- function(nis_matrix, 
                                     nid_matrix,
                                     list_names = NULL){
  s <- nis_matrix
  d <- nid_matrix
  x <- rep(c(1), each=nrow(s))
  ## Check counties match between nis_matrix and nid_matrix
  df <- identical(colnames(d), colnames(s))
  stopifnot(df)
  print(paste(list_names, "Absorption calculation started", Sys.time() ))
  df <-  matrix(0, nrow = ncol(s), 
                ncol = ncol(s) )
  rownames(df) = colnames(df) <- colnames(s)
  for (i in 1:ncol(s)){
    for (j in 1:ncol(s)){
      df[i,j] <- 
        (x %*% pmin(s[,i], d[,j]))
    }
  }
  print(paste(list_names, "Absorption calculation finished", Sys.time() ))
  invisible(df)
}

############ Normalized Absorption Share
normalized_absorption_share <- function(sas_matrix, 
                                        nis_matrix){
  s <- sas_matrix
  n <- nis_matrix
  df <- s / colSums(n)
  df[is.na(df)] = 0
  return(df)
}

############ Call absorption matrix for any available year and industry scale
absorption_matrix <- function(year,
                              ilevel = c("det", "sum", "sec"),
                              cbsa_clust = FALSE,
                              flow_class = c("industry", "commodity"),
                              net_class = c("factor", "gross"),
                              normalized = TRUE,
                              impedance = NULL,
                              data_dir = file.path("data", "robjs"),
                              ...){
  ilevel <- match.arg(ilevel)
  flow_class <- match.arg(flow_class)
  net_class <- match.arg(net_class)
  yl <- io_yeild_list(year, 
                      ilevel = ilevel,
                      cbsa_clust = cbsa_clust,
                      flow_class = flow_class, 
                      net_class = net_class, 
                      ...)
  y <-  yl[[1]] %>% {names(which(!is.na(rowSums(.)) & !rowSums(.) == 0))}
  net_demand <- yl[[4]][y, , drop=F]
  net_supply <- yl[[5]][y, , drop=F]
  sasf <- paste0("sas", "_", ilevel,"level", "_", year, "year", "_", flow_class, "class", "_", "cbsa", if(isTRUE(cbsa_clust)){"clust"}else{"NA"}, "_", net_class, "class")
  if (file.exists(file.path(find_rstudio_root_file(), data_dir, sasf) ) ){ 
    df <- readRDS(file.path(find_rstudio_root_file(), data_dir, sasf))
  } else {
    df <- stacked_absorption_share(net_supply, net_demand)
    saveRDS(df, file = file.path(find_rstudio_root_file(), data_dir, sasf) )
  }
  if(isTRUE(normalized)){
    df <- normalized_absorption_share(df, net_supply)
  }
  if(!is.null(impedance)){
    df <- df * impedance[colnames(df), rownames(df)]
  }
  return(df)
}

### Generate vector of maximum non-impedance absorption values using impedance scaled absorption matrix
noimpedance_absorption_maximum <- function(absorption_matrix, 
                                           impedance_mat, 
                                           row_max_match = TRUE){
  a <- absorption_matrix
  df <- c()
  x <- a * impedance_mat[rownames(a), colnames(a)]
  if(isTRUE(row_max_match)){
    x <- apply(x, 1, which.max)
    for(i in 1:nrow(a)){
      df <- cbind(df, a[i, x[i]])
    }
  } else {
    x <- apply(x, 2, which.max)
    for(i in 1:ncol(a)){
      df <- cbind(df, a[x[i], i])
    }
  }
  df <- as.data.frame(t(df))
  rownames(df) <- rownames(a)
  colnames(df) <- "ab_max"
  df$id <- deparse(substitute(absorption_matrix))
  return(df)
}


#need to address HI and AK matching
############ Row-wise Absorption Potential Maximum and Match
absorption_maximum_match <- function(absorption_matrix, 
                                     threshold = .05,
                                     ...){
  a <- absorption_matrix
  df <- cbind(place = rownames(a), 
               match = colnames(a)[apply(a, 1, which.max)],
               max_absorption_alpha = apply(a, 1, max), 
               max_absorption_count = apply(a, 1, function(x) {sum(max(x) == x)}),
               second_max_absorption_alpha = apply(a, 1, function(x){max(x[x != max(x), drop = FALSE])}), 
               absorption_alpha_gini = apply(a, 1, gini),
               absorption_alpha_total = apply(a, 1, sum),
               absorption_alpha_mean = apply(a, 1, mean),
               absorption_alpha_sd = apply(a, 1, sd),
               adsorption_match = colnames(a)[apply(a, 2, which.max)],
               max_adsorption_alpha = apply(a, 2, max), 
               max_adsorption_count = apply(a, 2, function(x) {sum(max(x) == x)}),
               second_max_adsorption_alpha = apply(a, 2, function(x){max(x[x != max(x), drop = FALSE])}), 
               adsorption_alpha_gini = apply(a, 2, gini),
               adsorption_alpha_total = apply(a, 2, sum),
               adsorption_alpha_mean = apply(a, 2, mean),
               adsorption_alpha_sd = apply(a, 2, sd) ) %>% 
    as.data.frame()
  df[, 3:ncol(df)] <- lapply(3:ncol(df), function(x) as.numeric(df[[x]]))

  ### cluster_class reevaluates the maximum absorption match to account for an isolation threshold and ECA isolated corner cases (i.e., no one imports your excess so you are isolated, but you are max import sink for someone else)
  df$cluster_class <- df$match
  df$cluster_class[df$max_absorption_alpha < threshold] <- "Isolated"
  df$cluster_class[df$place %in% unique(df$match) & df$cluster_class == "Isolated"] <- "ECA Isolated"
  
  ### eca_class reevaluates the maximum absorption match and returns the corrected self-match locations for "ECA Isolated" and "Cluster Core" locations 
  df$eca_class <- df$cluster_class
  df$eca_class[df$cluster_class == "ECA Isolated"] <- df$place[df$cluster_class == "ECA Isolated"]
  df$eca_class[df$place %in% unique(df$cluster_class)] <- df$place[df$place %in% unique(df$cluster_class)]
  
  ### cluster_category gives the categorical classification of each location as one of: "Isolated", "Isolated, Cluster Sink", "Cluster Sink", or "Cluster Source"
  df$cluster_category <- df$cluster_class
  df$cluster_category[df$place %in% unique(df$cluster_class)] <- "Cluster Sink"
  df$cluster_category[df$eca_class != df$place] <- "Cluster Source"
  df$cluster_category[df$cluster_class == "Isolated"] <- "Isolated"
  df$cluster_category[df$cluster_class == "ECA Isolated"] <- "Isolated, Cluster Sink"
  
  ### eca_membership gives all places their ECA corrected matching location explicitly
  df$eca_membership <- df$eca_class
  df$eca_membership[df$eca_class == "Isolated"] <- df$place[df$eca_class == "Isolated"]
  
  ### cluster_members_count is a tally of the number of places belonging to a cluster
  df <- df %>% group_by(eca_membership) %>% mutate(cluster_members_count = n())
  
  return(df)
}

############ Join spatial and other location specific information to ECA classification data tables
join_space_with_connectedness <- function(connectedness_table,
                                          space_data, 
                                          join_variable = place){
  df <- inner_join(space_data, connectedness_table, by = deparse(substitute(join_variable)), copy = TRUE)
}

#need to drop Alaska and Hawaii when?
############ Call connectedness table
connectedness <- function (year,
                           central_place = NULL,
                           cbsa_clust = FALSE, 
                           spatial = TRUE,
                           ...){
  df <- absorption_matrix(year, cbsa_clust = cbsa_clust, ...)
  if(is.null(central_place)){
    df <- absorption_maximum_match(df, ...)
  } else {
    if(isTRUE(cbsa_clust)){
      central_place <- fips2cbsa(central_place, year)
      }
    df <- cbind(export_absorption = c(t(df[central_place, , drop = FALSE])),
                import_absorption = c(df[, central_place, drop = FALSE]),
                place = rownames(df)) 
    df[, 2:3] <- lapply(2:3, function(x) as.numeric(df[[x]]))
  }
  if(isTRUE(spatial)){
    df <- call_geog(...) %>% join_space_with_connectedness(df, .)
  }
  return(df)
}
    
############ Absorption matching outcomes over time
absorption_match_overtime <- function(years = 2000:2020,
                                      ...){
  dis <- vector("list", length(years))
  names(dis) <- years
  for (y in years){
    dis[[y]] <- connectedness(year = y, ...)
  }
  df <- bind_rows(dis, .id = "id")
  return(df)
}







###Need to make adding html a separate operation
#need to drop Alaska and Hawaii when?
############ Call connectedness table with spatial component
spatial_connectedness <- function(year,
                                  ...){
  c <- connectedness(year, ...)
  s <- call_geog(...)
  df <- join_space_with_connectedness(c, s)
  
  # data_dir = file.path("data", "robjs")
  # if(isTRUE(add_html)){
  #   hp <- paste0("htmlplots", "_", industry_aggregate_class, "class", "_", cbp_year, "cbp", "_",  ag_year, "ag", "_", tiger_year, "tiger", "_", if(isTRUE(cbsa_clust)){cbsa_year}else{"NA"}, "cbsa")
  #   if (file.exists(file.path(find_rstudio_root_file(), data_dir, hp))){ 
  #     h <- readRDS(file.path(find_rstudio_root_file(), data_dir, hp))
  #   } else {
  #     h <- html_industry_dist_plots(cbp_year,
  #                                   ...)
  #     saveRDS(h, file = file.path(find_rstudio_root_file(), data_dir, hp))
  #   }
  #   indicator_type = names(h)
  #   for(i in indicator_type){
  #     df[[paste0("html_", i) ]] <- h[[i]]
  #   }
  # } 
  
  # ilevel <- match.arg(ilevel)
  # flow_class <- match.arg(flow_class)
  # net_class <- match.arg(net_class)
  
  # sasf <- paste0("sas", "_", ilevel,"level", "_", year, "year", "_", flow_class, "class", "_", "cbsa", if(isTRUE(cbsa_clust)){"clust"}else{"NA"}, "_", net_class, "class")
  # if (file.exists(file.path(find_rstudio_root_file(), data_dir, sasf) ) ){ 
  #   df <- readRDS(file.path(find_rstudio_root_file(), data_dir, sasf))
  # } else {
  #   df <- stacked_absorption_share(net_supply, net_demand)
  #   saveRDS(df, file = file.path(find_rstudio_root_file(), data_dir, sasf) )
  # }
  
  return(df)
}

#double check operation
############ Aggregate economic industry output of each ECA member in a cluster, keep all non source places as ECA core unit label
aggregate_industry_output <- function(industry_output_matrix, 
                                      connectedness_table,
                                      ...){
  df <- industry_output_matrix
  c <- connectedness_table
  x <- c$eca_membership %>% unique() %>% .[order(.)]
  for(i in x){
    df[, i] <- rowSums(df[, c$place[c$eca_membership == i], drop = FALSE])
  } 
  df <- df[, x]
}

#double check operation
############ Spatial union each ECA member in a cluster
spatial_cluster <- function(spatial_connectedness_table,
                            quiet = TRUE,
                            ...){
  df <-  spatial_connectedness_table %>% select(names(.)[!(names(.) %in% c("center", "html_output", "html_input", "html_nis", "html_nid"))])
  x <- df$eca_membership %>% unique() %>% .[order(.)]
  for (i in x){
    if(!quiet == TRUE){print(paste("start cluster: ", i, which(i == x), "of", length(x), Sys.time()))}
    df[df$place == i,]$geometry <- df %>% filter(df$eca_membership == i) %>% st_union()
    if(!quiet == TRUE){print(paste("end cluster: ", i, which(i == x), "of", length(x), Sys.time()))}
  }
  df <- df %>% .[.$place %in% .$eca_membership, ]
}

# may be redundant and unneecessary
# ############ Call spatial connectedness for any available year and industry scale aggregating eca clusters across space
# cluster_spatial_connectedness <- function (year,
#                                            list_names = NULL,
#                                            ...){
#   df <- spatial_connectedness(year, ...)
#   x <- df$eca_membership %>% unique() %>% .[order(.)]
#   for (i in x){
#     print(paste(list_names, "start cluster: ", i, which(i == x), "of", length(x), Sys.time()))
#     df[df$place == i,]$geometry <- df %>% filter(df$eca_membership == i) %>% st_union()
#     print(paste(list_names, "  end cluster: ", i, which(i == x), "of", length(x), Sys.time()))
#   }
#   df <- df %>% .[.$place %in% .$eca_membership, ]
#   return(df)
# }


#Superseded need to phase out
############ Call and clean the total requirements matrix 
call_total_requirements <- function(bea_year,
                                    ilevel = c("det", "sum", "sec"), 
                                    ...){
  ilevel <- match.arg(ilevel)
  if(ilevel == "det"){
    x <- bea_year %in% c("2007", "2012")
    stopifnot("BEA detail level tables only exist for years 2007 and 2012" = x == TRUE)
  }
  df <- bea_io$get_ixi(strtoi(bea_year), ilevel) %>% .[1:ncol(.), ] %>% as.matrix()
  rownames(df) <- colnames(df)
  return(df)
}

#Superseded need to phase out
### Need better methods for matrix inversion in tests inv() gave more accurate results but solve() was MUCH faster: inv() will also reach memory limit
############ Derive the direct requirements matrix (Technical Coefficients) 
call_direct_requirements <- function(...){
  df <- call_total_requirements(...)
  df <- diag(ncol(df)) - solve(df)
  df[df < 0] = 0
  return(df)
}


############ Industry Input Needs 
### Derive the commodity-by-county matrix of input needs DY
industry_input <- function(technical_coefficients_matrix, 
                           industry_output_matrix){
  ## Check industry level specificity match between industry_output_matrix and technical_coefficients_matrix
  df <- setequal(colnames(technical_coefficients_matrix), rownames(industry_output_matrix))
  stopifnot("Industry names do not match" = df == TRUE)
  o <- industry_output_matrix
  d <- technical_coefficients_matrix
  df <- d %*% o
}

############ Net Input Demand
### Derive the industry-by-county matrix of net input demand 
net_input_demand <- function(industry_output_matrix, 
                             industry_input_matrix){
  i <- industry_input_matrix
  o <- industry_output_matrix
  df <- pmax(i - o, 0)
}

############ Net Input Supply
### Derive the industry-by-county matrix of net input supply 
net_input_supply <- function(industry_output_matrix, 
                             industry_input_matrix){
  i <- industry_input_matrix
  o <- industry_output_matrix
  df <- pmax(o - i, 0)
}

### need to update with C_matrix B_matrix capability and adjust for factor supply /demand /RAS
############ Single function of nested functions to derive a hierarchies of connectedness tables and resulting output matrices from a base single output matrix and single direct requirements matrix
one_hierarchical_connectedness <- function(cbp_year,
                                           ilevel = c("det", "sum", "sec"),
                                           cbsa_clust = FALSE,
                                           normalized = TRUE,
                                           impedance = NULL,
                                           data_dir = file.path("data", "robjs"),
                                           queen = TRUE,
                                           ...){
  ilevel <- match.arg(ilevel)
  cbsa_year <- year2cbsa(cbp_year, ...)  %>% suppressWarnings() 
  ag_year <- year2agcensus(cbp_year, ...)
  bea_year <- year2bea(cbp_year, ilevel, ...)
  tiger_year <- year2tiger(cbp_year, ...) %>% suppressWarnings()
  sasf <- paste0("hier", "_", ilevel,"class", "_", cbp_year, "cbp", "_", if(isTRUE(cbsa_clust)){cbsa_year}else{"NA"}, "cbsa", "_", if(is.null(impedance)){"NA_impedance"}else if(is.numeric(impedance)){paste0(impedance,"_impedance")}else{if(queen==TRUE){"queenborder_impedance"}else{"rookborder_impedance"}} )
  if (file.exists(file.path(find_rstudio_root_file(), data_dir, sasf) ) ){ 
    df <- readRDS(file.path(find_rstudio_root_file(), data_dir, sasf))
  } else {
    o <- industry_output_tidy_matrix(cbp_year = cbp_year, ilevel = ilevel, cbsa_clust = cbsa_clust, ...)
    d <- call_direct_requirements(bea_year, ilevel, ...)
    if(ilevel == "det"){ 
      con <- grep("^23", colnames(d), value = TRUE)
      cm <- matrix(sum(d[con,con])/length(con), 
                   dimnames = list(c("23"), c("23")))
      cr <- t(matrix(colMeans(d[con, ]), 
                     dimnames = list(colnames(d), c("23")) ))
      cc <- matrix(rowMeans(d[, con]), 
                   dimnames = list(colnames(d), c("23"))) 
      d <- cbind( rbind(cr, d ), rbind(cm, cc )) 
      d <- d[colnames(d)[!colnames(d) %in% con] , colnames(d)[!colnames(d) %in% con]]
      d <- d[rownames(o)[rownames(o) %in% rownames(d)], rownames(o)[rownames(o) %in% colnames(d)]] 
    }
    hct <- list()
    hsct <- list()
    hsct$level_0 <- spatial_connectedness(cbp_year, ...)
    hom <- list()
    hom$level_0 <- o
    n = 1
    i = FALSE
    while(i == FALSE){
      print(paste("level", n))
      ii <- industry_input(d, o)
      nis <- net_input_supply(o, ii)
      nid <- net_input_demand(o, ii)
      df <- stacked_absorption_share(nis, nid)
      if(isTRUE(normalized)){
        df <- normalized_absorption_share(df, nis)
      }
      if(is.null(impedance)){
        df <- df
        } else if(is.numeric(impedance)){
          impd = st_is_within_distance(hsct[[paste0("level_", n-1)]]$geometry, dist = miles2meters(impedance))
          impd <- +as.matrix(impd)
          diag(impd) <- 0
          rownames(impd) = colnames(impd) <- hsct[[paste0("level_", n-1)]]$place
          df <- df * impd[colnames(df), rownames(df)]
        } else {
        impd = hsct[[paste0("level_", n-1)]]$geometry %>% 
          poly2nb(queen = queen) %>%
          nb2mat(style = "B", zero.policy = TRUE)
        rownames(impd) = colnames(impd) <- hsct[[paste0("level_", n-1)]]$place
        df <- df * impd[colnames(df), rownames(df)]
      }
      c <- absorption_maximum_match(absorption_matrix = df, ...)  
      s <-  hsct[[paste0("level_", n-1)]] %>% select(c("place", "NAME", "STATE_CODE", "COUNTY_CODE", "COUNTY", "STATE_NAME", "STATE", "geometry" ))
      hct[[paste0("level_", n)]] <- join_space_with_connectedness(c, s)
      i <- all(c$place %in% c$eca_membership) 
      if (i == TRUE){next}
      hsct[[paste0("level_", n)]] <- hct[[paste0("level_", n)]] %>% spatial_cluster()
      o <- aggregate_industry_output(o, c)
      hom[[paste0("level_", n)]] <- o
      n = n + 1
    }
    df <- list("Hierarchical_Connectedness_table" = hct,
               "Hierarchical_Spatial_Cluster_table" = hsct,
               "Hierarchical_Output_mat" = hom)
    saveRDS(df,  file = file.path(find_rstudio_root_file(), data_dir, sasf) )
  }
  return(df)
}

# update n vector and place_centric_connect inputs
############ Change in connectedness over time for a county 
place_connect_delta <- function(central_place, 
                                sample_years, # vector of years
                                ...){
  n <- c("export_absorption", "import_absorption", "html_output", "html_input", "html_nis", "html_nid")
  place_connect <- vector("list", length(sample_years))
  names(place_connect) <- sample_years
  for (i in sample_years){
    place_connect[[i]] <- place_centric_connect(central_place = central_place,
                                                year = i,
                                                ...)
    x <- n[n %in% names(place_connect[[i]])]
    names(place_connect[[i]])[names(place_connect[[i]]) %in% x] <- paste(x, i, sep=".")
  }
  df <- place_connect[[1]]
  for(i in 2:length(sample_years)){
    df <- st_set_geometry(place_connect[[i]], NULL) %>% .[c(grep("*[0-9]", colnames(.), value = TRUE), "place")] %>% inner_join(df, ., by = "place")
  }
  com <- combn(sample_years, 2)
  for (x in 1:ncol(com)){
    df[[paste0("export_absorption_delta_", substr(com[2, x], 3, 4), substr(com[1, x], 3, 4))]] <- df[[paste0("export_absorption.", com[1, x])]] - df[[paste0("export_absorption.", com[2, x])]]
    df[[paste0("import_absorption_delta_", substr(com[2, x], 3, 4), substr(com[1, x], 3, 4))]] <- df[[paste0("import_absorption.", com[1, x])]] - df[[paste0("import_absorption.", com[2, x])]]
  }
  ###Percent Change option
  # for (x in 1:ncol(com)){
  #   df[[paste0("export_absorption_delta_", substr(com[2, x], 3, 4), substr(com[1, x], 3, 4))]] <- (df[[paste0("export_absorption.", com[1, x])]] - df[[paste0("export_absorption.", com[2, x])]])/df[[paste0("export_absorption.", com[1, x])]]*100
  #   df[[paste0("import_absorption_delta_", substr(com[2, x], 3, 4), substr(com[1, x], 3, 4))]] <- (df[[paste0("import_absorption.", com[1, x])]] - df[[paste0("import_absorption.", com[2, x])]])/df[[paste0("import_absorption.", com[1, x])]]*100
  # }
return(df)
}


# Display end time
log_info("Define functions end")





