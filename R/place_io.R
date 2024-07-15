# This script calculates output by industry/commodity at local place level (county or CBSA)



# R libraries ----
library(logger)
library(arrow)
library(tidyverse)
library(glue)



# R scripts ----
source("R/basic_utilities.R", local = (util <- new.env()))
source("R/dataprep_bea_io.R", local = (bea_io <- new.env()))
source("R/dataprep_cbp.R", local = (cbp <- new.env()))
source("R/dataprep_infogroup.R", local = (ig <- new.env()))
source("R/dataprep_agcensus.R", local = (agcen <- new.env()))
source("R/geography.R", local = (geog <- new.env()))


# Data objects ----

ipath <- list(
  # data dependencies
)

opath <- list(
  output_ = "data/place_io/output_{year}_{class_system}_{ilevel}_{bus_data}.pq"
)

clear_outputs <- function() {
  util$clear_paths(opath)
}


# utility functions----



# return commodity output matrix using industry output matrix and supply matrix
industry2commodity <- function(industry_output_matrix,
                               io_supply_matrix){
  #industry-by-place gross output matrix (1,000's of dollars)
  df <- industry_output_matrix
  
  #national industry aggregate of derived gross industry output by place
  x <- rowSums(df)/1000
  
  #BEA supply matrix 
  smat <- io_supply_matrix
  
  #alternate scaled "C" matrix using derived values 
  cmat_alt <- (smat[, names(x)[x!=0], drop=FALSE] %*% diag(1/as.vector(x[names(x)[x!=0]]))) %>%
    `colnames<-`(names(x)[x!=0]) 
  
  #commodity-by-place gross output matrix (1,000's of dollars)
  df <- cmat_alt%*%df[names(x)[x!=0], ,drop = F]
  
  return(df)
}


# Output ----

#' County output in $1000s
call_output <- function(year, 
                        class_system = c("industry", "commodity"), 
                        ilevel = c("det", "sum", "sec"),
                        bus_data = c("cbp_imp", "cbp_raw", "infogroup"),
                        verbose = FALSE){
  
  class_system <- match.arg(class_system)
  ilevel <- match.arg(ilevel)
  bus_data <- match.arg(bus_data)
  
  cache_path <- glue(opath$output_)
  if (file.exists(cache_path)) {
    log_debug(paste("read from cache", cache_path))
    return(read_parquet(cache_path))
  }
  
  #NAICS to BEA industry concordance
  conc <- bea_io$call_ilevel_concord(ilevel = ilevel, year = year)
  
  #BEA national gross industry output
  indout <- bea_io$call_industry_output(year = year, ilevel = ilevel, condense = TRUE) %>% 
    as.data.frame() %>% 
    mutate(indcode = rownames(.))
  
  #test: list of BEA (collapsed) industries with no NAICS concordance match
  if (verbose){
    cat(paste("No concordance match: \n"))
    setdiff(indout$indcode, conc[[1]]) %>% 
      {cat(paste(.,"\n"))}
  }
  
  #test: list of BEA industries with no conceivable NAICS concordance match or only many-to-one NAICS-to-BEA or both
  if (verbose){
    cat(paste("No conceivable match: \n"))
    bea_io$call_industry_output(year = year, ilevel = ilevel, condense = FALSE) %>%
      as.data.frame() %>%
      mutate(indcode = rownames(.)) %>%
      {setdiff(.$indcode, conc[[1]])} %>% 
      {cat(paste(.,"\n"))}
  }
  
  #test: list of condensed BEA industries with no conceivable NAICS concordance match
  if (verbose){
    cat(paste("No condensed match: \n"))
    setdiff(indout$indcode, conc[[1]]) %>% 
      {cat(paste(.,"\n"))}
  }
  
  if(bus_data == "cbp_imp"){
    #EFSY imputed county business patterns annual payroll across NAICS industry hierarchy by place
    df <- cbp$call_cbp(year = year, 
                       cbp_scale = "county",
                       imputed = TRUE) %>% 
      select(naics, place, ap)
  }
  
  if(bus_data == "cbp_raw"){
    #raw county business patterns annual payroll across NAICS industry hierarchy by place
    df <- cbp$call_cbp(year = year, 
                       cbp_scale = "county",
                       imputed = FALSE) %>% 
      select(naics, place, ap)
  }
  
  if(bus_data == "infogroup"){
    #infogroup 6 digit NAICS industry sales by place
    df <- glue(ig$opath$county_, .envir = list(year = year)) %>%
      open_dataset() %>%
      collect() %>% 
      {.[c(1:6)]} %>% 
      na.omit() %>% 
      arrange(st, cty) 
    df$place <- paste0(df$st, df$cty)
    df <- df %>% 
      select(naics, place, sales) 
    
    #6 digit NAICS to BEA equivalent NAICS concordance
    n <- df$naics %>% unique() %>% sort()
    cn <- conc$NAICS %>% unique()
    cnc <- data.frame("naics" = c(), "NAICS" = c())
    for(i in cn){
      x <- paste0("^", i) %>% grep(., n, value = T)
      if (!is_empty(x)){
        cnc <- rbind(cnc, data.frame("naics" = x, "NAICS" = i))
      } 
    }
    
    #test: list of 6-digit NAICS industries with available underlying micro data but no matching BEA industry concordance
    if (verbose){
      cat(paste("No BEA 6-digit match: \n"))
      setdiff(n, cnc$naics) %>% 
        {cat(paste(.,"\n"))}
    }

    # BEA industry equivalent industry sales by place
    df <- right_join(cnc, df, by = "naics") %>% 
      select(-naics) %>% 
      na.omit()
  }
  
  #test: list of condensed BEA industries with a conceivable NAICS concordance value but no available underlying micro data
  if (verbose){
    cat(paste("No BEA condensed match: \n"))
    df %>%
      `colnames<-`(c("naics", "place", "value")) %>%
      full_join(conc, ., by = c("NAICS" = "naics")) %>%
      select(-place) %>%
      {.[is.na(.$value), ]} %>%
      .[[1]] %>%
      unique() %>% 
      {cat(paste(.,"\n"))}
  }
  
  # non-farm BEA equivalent industry "value" by place
  df <- df %>% 
    `colnames<-`(c("naics", "place", "value")) %>%
    left_join(conc, ., by = c("NAICS" = "naics")) %>% 
    select(-NAICS) %>% 
    `colnames<-`(c("indcode", "place", "value")) %>% 
    left_join(indout[, "indcode", drop=F], ., by = "indcode") %>% 
    mutate(value = ifelse(is.na(value), 0, value)) %>% 
    .[!(duplicated(.) & .$value==0), ] %>% 
    complete(indcode, place, fill = list(value = 0)) %>% 
    na.omit() %>%
    {aggregate(.$value, list(.$indcode, .$place), FUN=sum)} %>% # TODO: highest single point of computation time consider possible alternatives
    `colnames<-`(c("indcode", "place", "value")) %>% 
    {.[!grepl("^(11)[1-2]", .$indcode), ]} %>% 
    arrange(place)
  
  #test: check for completeness and uniqueness
  if (verbose){
    cat(paste("Check completeness and uniqueness: "))
    (length(unique(df$place))*length(unique(df$indcode))==length(df$value)) %>% 
      {cat(paste(.,"\n"))}
  }
  
  #test: list of condensed BEA non-farm industries with no available underlying micro data
  if (verbose){
    cat(paste("No micro for BEA non-farm: \n"))
    df %>%
      select(-place) %>%
      {.[is.na(.$value), ]} %>%
      .$indcode %>%
      unique() %>% 
      {cat(paste(.,"\n"))}
  }
  
  #Ag census farm sales at BEA detail level by place
  fs <- agcen$call_agoutput(year = year, geo_level = "county")
  
  #nationally adjusted BEA detail level farm industry gross output by place (millions of dollars) 
  ag <- fs %>% 
    pivot_longer(!place, names_to = "indcode", values_to = "value") %>% 
    select(indcode, place, value)
  
  #nationally adjusted farm and non-farm industry gross output by place (1,000's of dollars)  (NaN's as Zero's)
  df <- rbind(ag, df) %>%
    complete(indcode, place, fill = list(value = 0)) %>%
    replace(is.na(.), 0) 
  
  if (ilevel == "sec"){
    # adjusted "indcode" output aggregation consistent with sector ilevel
    df <- df %>% 
      {.[grepl("^(11)", .$indcode), ]} %>% 
      {aggregate(.$value, list(.$place), FUN=sum)} %>% 
      mutate(indcode = "11", .before = 1) %>% 
      `colnames<-`(c("indcode", "place", "value")) %>% 
      rbind(., df[!grepl("^(11)", df$indcode), ])
  }
  
  if (ilevel == "sum"){
    # adjusted "indcode" output aggregation consistent with summary ilevel
    df <- df %>% 
      {.[grepl("^(11)[1-2]", .$indcode), ]} %>% 
      {aggregate(.$value, list(.$place), FUN=sum)} %>% 
      mutate(indcode = "111CA", .before = 1) %>% 
      `colnames<-`(c("indcode", "place", "value")) %>% 
      rbind(., df[!grepl("^(11)[1-2]", df$indcode), ])
  }
  
  #national aggregate industry "value" to BEA industry gross output ratio
  ps <- df %>% 
    select(-place) %>% 
    {aggregate(.$value, list(.$indcode), FUN=sum)} %>%
    `colnames<-`(c("indcode", "value")) %>% 
    inner_join(., indout, by = "indcode") %>% 
    mutate(share = value / T017 ) %>%
    select(indcode, share)
  
  #nationally adjusted industry gross output by place (1,000's of dollars) 
  df <- df %>% 
    inner_join(., ps, by = "indcode") %>% 
    mutate(output = value/(share/1000)) %>% 
    select(indcode, place, output) %>%
    replace(is.na(.), 0) %>% 
    {arrange(., match(.$indcode, indout$indcode))} %>%
    arrange(place)
  
  #test: check aggregation consistency
  #(note: disaggregate industries with outputs of zero not tested)
  if (verbose){
    cat(paste("Check overall aggregation consistency: "))
    df %>%
      select(-place) %>%
      {aggregate(.$output, list(.$indcode), FUN=sum)} %>%
      `colnames<-`(c("indcode", "output")) %>%
      mutate_at(vars(output), \(x)(round(x/1000, 6))) %>%
      full_join(., indout, by = "indcode") %>%
      .[!(.$output==0), ] %>%
      {all.equal(.$output, .$T017, tolerance = 1)} %>% 
      {cat(paste(.,"\n"))}
  }
  
  if (class_system == "commodity"){
    #test: list of condensed BEA industries with zero total aggregate gross output derived from micro data
    if (verbose){
      #national industry aggregate of derived gross industry output by place
      x <- df %>% 
        util$long2matrix() %>% 
        rowSums()
      cat(paste("No microdata gross output BEA industries: \n"))
      x %>% 
        {names(.)[.==0]} %>%
        {cat(paste(.,"\n"))}
    }
    
    #BEA supply matrix 
    smat <- bea_io$call_supply_matrix(year = year, 
                                      ilevel = ilevel, 
                                      condense = TRUE)
    
    #industry-by-place gross output matrix (1,000's of dollars)
    df <- df %>% 
      util$long2matrix() %>% 
      {industry2commodity(industry_output_matrix = ., 
                          io_supply_matrix = smat)}
    
    #nationally adjusted farm and non-farm commodity gross output by place (1,000's of dollars)  (NaN's as Zero's)
    df <- df %>% 
      as.data.frame.table() %>% 
      `colnames<-`(c("indcode", "place", "output"))
    
    #test: check aggregation consistency and alternate C matrix scalar precision at disaggregate level
    if (verbose){
      #BEA national gross commodity output
      comout <- bea_io$call_commodity_output(year = year, ilevel = ilevel, condense = TRUE) %>%
        as.data.frame() %>%
        mutate(indcode = rownames(.))
      cat(paste("Check check aggregation consistency and alternate C matrix scalar precision: "))
      df %>%
        select(-place) %>%
        {aggregate(.$output, list(.$indcode), FUN=sum)} %>%
        `colnames<-`(c("indcode", "output")) %>%
        mutate_at(vars(output), \(x)(round(x/1000, 6))) %>%
        full_join(., comout, by = "indcode") %>%
        full_join(., data.frame("indcode" = rownames(smat), "altcomout" = rowSums(smat[, names(x)[x!=0]])), by = "indcode") %>%
        {.$output == .$altcomout} %>%
        all() %>%
        {cat(paste(.,"\n"))}
    }
    
    #test: list of effected condensed BEA commodities for which an adjusted C matrix corrected aggregation inconsistencies for commodity gross output derived from micro data
    if (verbose){
      cat(paste("Missing micro effected BEA commodities: \n"))
      df %>%
        select(-place) %>%
        {aggregate(.$output, list(.$indcode), FUN=sum)} %>%
        `colnames<-`(c("indcode", "output")) %>%
        mutate_at(vars(output), \(x)(round(x/1000, 6))) %>%
        {full_join(., comout, by = "indcode")} %>%
        {.[.$output!=.$T007, ]} %>%
        {.$indcode} %>%
        {cat(paste(.,"\n"))}
    }
  }
  
  log_debug(paste("save to cache", cache_path))
  write_parquet(df, util$mkdir(cache_path))
  return(df)  
}




# Supply and demand ----


#' County-commodity output, supply and demand in $1000s
call_outsupdem <- function(year,
                           ilevel = c("det", "sum", "sec"),
                           bus_data = c("cbp_imp", "cbp_raw", "infogroup")) {
  ilevel <- match.arg(ilevel)
  bus_data <- match.arg(bus_data)
  
  # county-industry output
  output_ind <- call_output(
    year = year,
    class_system = "industry",
    ilevel = ilevel,
    bus_data = bus_data
  )
  # county-commodity output
  output_com <- call_output(
    year = year,
    class_system = "commodity",
    ilevel = ilevel,
    bus_data = bus_data
  )
  
  # outputs in matrix form for multiplication
  output_ind_mat <- util$long2matrix(output_ind)
  output_com_mat <- util$long2matrix(output_com)
  
  # non-zero industries
  pos_ind_names <- rownames(output_ind_mat)[rowSums(output_ind_mat) > 0]
  zero_ind_names <- base::setdiff(rownames(output_ind_mat), pos_ind_names)
  if (length(zero_ind_names) > 0) {
    log_info(glue("{length(zero_ind_names)} industries have zero output: {str_c(zero_ind_names, collapse = ',')}"))
  }
  
  # domestic intermediate use share of total use
  # total_use == total_supply, I-O table accounting identity
  total_use <- bea_io$call_commodity_supply(year = year, ilevel = ilevel, condense = TRUE)[, 1]
  # in the use table only add up columns with positive industry output
  dom_int_use <- bea_io$call_use_matrix(year = year, ilevel = ilevel, condense = TRUE)[, pos_ind_names] %>%
    rowSums()
  stopifnot(all(names(total_use) == names(dom_int_use)))
  dom_int_use_share <- dom_int_use / total_use
  dom_int_use_share[dom_int_use == 0 | total_use == 0] <- 0
  # tibble(indcode = names(total_use), dom_int_use, total_use, dom_int_use_share) %>% View()
  
  # county-commodity supply = commodity_output * use_share
  stopifnot(all(rownames(output_com_mat) == names(dom_int_use_share)))
  supply_mat <- sweep(output_com_mat, 1, dom_int_use_share, "*")
  
  # producer price domestic output share of total supply
  total_supply <- total_use
  # in the supply table only add up columns with positive industry output
  dom_pp_out <- bea_io$call_supply_matrix(year = year, ilevel = ilevel, condense = TRUE)[, pos_ind_names] %>%
    rowSums()
  dom_pp_out_share <- dom_pp_out / total_supply
  dom_pp_out_share[dom_pp_out == 0 | total_supply == 0] <- 0
  # tibble(indcode = names(total_supply), dom_pp_out, total_supply, dom_pp_out_share) %>% View()

  # county-commodity demand = B-mat * industry_output * dom_pp_share
  bmat <- bea_io$call_b_matrix(year = year, ilevel = ilevel, condense = TRUE)
  stopifnot(all(colnames(bmat) == rownames(output_ind_mat)))
  demand_mat <- bmat %*% output_ind_mat
  stopifnot(all(rownames(demand_mat) == names(dom_pp_out_share)))
  demand_mat <- sweep(demand_mat, 1, dom_pp_out_share, "*")
  
  # bind output, supply and demand together into single dataframe
  df_supply <- supply_mat %>%
    as_tibble(rownames = "indcode") %>%
    pivot_longer(!indcode, names_to = "place", values_to = "supply")
  df_demand <- demand_mat %>%
    as_tibble(rownames = "indcode") %>%
    pivot_longer(!indcode, names_to = "place", values_to = "demand")
  stopifnot(nrow(output_com) == nrow(df_supply))
  stopifnot(nrow(output_com) == nrow(df_demand))
  df <- inner_join(output_com, df_supply, join_by(indcode, place), relationship = "one-to-one") %>%
    inner_join(df_demand, join_by(indcode, place), relationship = "one-to-one")

  df
}



  
  
