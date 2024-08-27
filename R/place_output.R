# This script calculates output by industry/commodity at local place level (county or CBSA)



# R libraries ----
library(logger)
log_threshold(DEBUG)
library(arrow)
library(tidyverse)
library(glue)
library(REAT)


# R scripts ----
source("R/basic_utilities.R", local = (util <- new.env()))
source("R/dataprep_bea_io.R", local = (bea_io <- new.env()))
source("R/dataprep_cbp.R", local = (cbp <- new.env()))
source("R/dataprep_infogroup.R", local = (ig <- new.env()))
source("R/dataprep_agcensus.R", local = (agcen <- new.env()))
source("R/geography.R", local = (geog <- new.env()))


# Data objects ----
ipath <- list(
  ig_ = ig$opath$county_,
  cbp_ = cbp$opath$cbp_
  # data dependencies
)

opath <- list(
  output_ = "data/place_activity/output_{year}_{class_system}_{ilevel}_{bus_data}.pq",
  iofactor_ = "data/place_activity/iofactor_{year}_{class_system}_{ilevel}_{bus_data}_{cbsa}_{paradigm}.pq"
)

clear_outputs <- function() {
  util$clear_paths(opath)
}


# utility functions----

# get long format industry name
short2long <- function(short_name){
  switch(short_name, sec = {"sector"}, sum = {"summary"}, det = {"detail"})
}

# Output functions----

# Aggregate "output" of the CBSA members if in a cluster
cbsa_aggregate_output <- function(df,
                                  year = 2012){
  df <- geog$call_cbsa_concord(year) %>% 
    {left_join(df, ., by = "place")} %>% 
    mutate(CBSA_CODE = ifelse(is.na(CBSA_CODE), place, CBSA_CODE)) %>% 
    {aggregate(.[[3]], list(.$indcode, .$CBSA_CODE), FUN=sum)} %>% 
    `colnames<-`(names(df))
  return(df)  
}

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


# Output tables----

# gross (industry or commodity) output by county across 3 levels ("det", "sum", "sec") and sources of micro data
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


# Call intertemporal gross output by county table
call_temporal_output <- function(set_of_years, 
                                 names_prefix = "y.",
                                 class_system = c("industry", "commodity"), 
                                 ilevel = c("det", "sum", "sec"),
                                 bus_data = c("cbp_imp", "cbp_raw", "infogroup"),
                                 verbose = FALSE){
  df <- util$temp_fun_recur_list(set_of_years = set_of_years, 
                                 call_output, 
                                 class_system = class_system, 
                                 ilevel = ilevel, 
                                 bus_data = bus_data,
                                 verbose = verbose) %>%
    bind_rows(.id = "id") %>%
    pivot_wider(names_from = id, values_from = output, names_prefix = names_prefix)
  return(df)
}


# Intermediate functions----

#return "factor" paradigm intermediate industry/commodity demand/supply matrix 
intermediate_activity_factor <- function(industry_output_matrix,
                                          io_b_matrix,
                                          io_supply_matrix,
                                          schedule = c("demand", "supply"),
                                          class_system = c("industry", "commodity") ){
  schedule <- match.arg(schedule)
  class_system <- match.arg(class_system)

  df <- industry_output_matrix
  bmat <- io_b_matrix
  smat <- io_supply_matrix

  # commodity output matrix
  co <- industry2commodity(df, smat)

  #subset of industry names where aggregate output is not zero
  inames <- df %>%
    rowSums() %>%
    {names(.)[.!=0]}

  #subset of commodity names where aggregate output is not zero
  cnames <- co %>%
    rowSums() %>%
    {names(.)[.!=0]}

  #alternate scaled "D" matrix using only industries available in the micro data
  dmat_alt <- co %>%
    rowSums() %>%
    {./1000} %>%
    as.matrix() %>%
    {(t(smat[, inames, drop=FALSE]) %*% diag(1/as.vector(.)))} %>%
    `colnames<-`(rownames(smat))

  if (schedule == "demand" & class_system == "industry"){
    df <- diag(as.vector(colSums(dmat_alt[inames, cnames, drop = F]%*%bmat[cnames, inames, drop = F])))%*%df[inames, , drop = F] %>%
      `rownames<-`(inames)
  }
  if (schedule == "supply" & class_system == "industry"){
    df <- (dmat_alt[inames, cnames, drop = F]%*%bmat[cnames, inames, drop = F])%*%df[inames, , drop = F] %>%
      `rownames<-`(inames)
  }
  if (schedule == "demand" & class_system == "commodity"){
    df <- bmat[cnames, inames, drop = F]%*%df[inames, , drop = F] %>%
      `rownames<-`(cnames)
  }
  if (schedule == "supply" & class_system == "commodity"){
    df <- diag(as.vector(bmat[cnames, inames, drop = F]%*%rowSums(df[inames, , drop = F])))%*%t(dmat_alt[inames, cnames, drop = F])%*%diag(1/as.vector(rowSums(df[inames, , drop = F])))%*%df[inames, , drop = F] %>%
      `rownames<-`(cnames)
  }

  return(df)
}

#return "domestic" paradigm intermediate industry/commodity demand/supply matrix 
intermediate_activity_domestic <- function(industry_output_matrix,
                                            io_b_matrix,
                                            io_supply_matrix,
                                            phi,
                                            schedule = c("demand", "supply"),
                                            class_system = c("industry", "commodity") ){
  schedule <- match.arg(schedule)
  class_system <- match.arg(class_system)

  df <- industry_output_matrix
  bmat <- io_b_matrix
  smat <- io_supply_matrix
  phi <- phi

  # commodity output matrix
  co <- industry2commodity(df, smat)

  #subset of commodity names where aggregate output is not zero
  cnames <- co %>%
    rowSums() %>%
    {names(.)[.!=0]}
  
  #sub-subset of commodity names where phi is defined
  cnames <- phi %>%
    {rownames(.)[is.finite(.)]} %>% 
    {intersect(. , cnames)}

  #aggregate gross commodity supply
  cs <- co %>%
    rowSums() %>%
    {bea_io$commodity_supply(commodity_output_vector = ., phi = phi)}

  if (schedule == "demand" & class_system == "industry"){
    stop("dummy error")
  }
  if (schedule == "supply" & class_system == "industry"){
    stop("dummy error")
  }
  if (schedule == "demand" & class_system == "commodity"){
    #censored production shares
    ps <- co %>%
      rowSums() %>%
      {bea_io$production_shares(commodity_output_vector = ., commodity_supply_vector = cs)}

    #county demand for domestically produced intermediate commodities in producer prices
    df <- diag(as.vector(ps[cnames, , drop = F])) %*% (bmat %*% df)[cnames, , drop = F] %>%
      `rownames<-`(cnames)
  }
  if (schedule == "supply" & class_system == "commodity"){
    #censored commodity use shares
    cus <- (bmat %*% df)[, , drop = F] %>%
      rowSums() %>%
      {bea_io$commodity_use_shares(intermediate_commodity_use_vector = ., commodity_supply_vector = cs)}

    #county supply of domestically produced intermediate commodities in producer prices
    df <- diag(as.vector(cus[cnames, , drop = F])) %*% co[cnames, , drop = F]  %>%
      `rownames<-`(cnames)
  }

  return(df)
}


#return "capital" paradigm intermediate industry/commodity demand/supply matrix 
intermediate_activity_capital <- function(schedule = c("demand", "supply"),
                                          class_system = c("industry", "commodity") ){
  schedule <- match.arg(schedule)
  class_system <- match.arg(class_system)

  if (schedule == "demand" & class_system == "industry"){
    stop("dummy error")
  }
  if (schedule == "supply" & class_system == "industry"){
    stop("dummy error")
  }
  if (schedule == "demand" & class_system == "commodity"){
    stop("dummy error")
  }
  if (schedule == "supply" & class_system == "commodity"){
    stop("dummy error")
  }

  return(df)
}

# return intermediate industry/commodity demand/supply matrix under either factor/domestic/capital paradigm
intermediate_activity <- function(industry_output_matrix,
                                  io_b_matrix,
                                  io_supply_matrix,
                                  phi,
                                  schedule = c("demand", "supply"),
                                  paradigm = c("factor", "domestic", "capital"),
                                  class_system = c("industry", "commodity") ){
  schedule <- match.arg(schedule)
  paradigm <- match.arg(paradigm)
  class_system <- match.arg(class_system)
  if(paradigm == "factor"){
    df <- intermediate_activity_factor(industry_output_matrix = industry_output_matrix,
                                       io_b_matrix = io_b_matrix,
                                       io_supply_matrix = io_supply_matrix,
                                       schedule = schedule,
                                       class_system = class_system)
  }
  
  if(paradigm == "domestic"){
    df <- intermediate_activity_domestic(industry_output_matrix = industry_output_matrix,
                                         io_b_matrix = io_b_matrix,
                                         io_supply_matrix = io_supply_matrix,
                                         phi,
                                         schedule = schedule,
                                         class_system = class_system)
  }
  
  if(paradigm == "capital"){
    df <- intermediate_activity_capital(schedule = schedule,
                                        class_system = class_system)
  }
  
  return(df)  
}

# Intermediate tables----

# regional intermediate industry demand or supply (in 1,000's of dollars)
call_intermediate <- function(year,
                              schedule = c("demand", "supply"),
                              paradigm = c("factor", "domestic", "capital"),
                              class_system = c("industry", "commodity"), 
                              ilevel = c("det", "sum", "sec"),
                              bus_data = c("cbp_imp", "cbp_raw", "infogroup"),
                              verbose = FALSE){
  
  schedule <- match.arg(schedule)
  paradigm <- match.arg(paradigm)
  class_system <- match.arg(class_system)
  ilevel <- match.arg(ilevel)
  bus_data <- match.arg(bus_data)
  
  #derived subnational industry output
  iout <- call_output(year = year, 
                      class_system = "industry", 
                      ilevel = ilevel, 
                      bus_data = bus_data,
                      verbose = verbose) %>% 
    util$long2matrix()
  
  #BEA-IO B matrix 
  bmat <- bea_io$call_b_matrix(year = year, 
                               ilevel = ilevel, 
                               condense = TRUE) 
  #BEA supply matrix 
  smat <- bea_io$call_supply_matrix(year = year, 
                                    ilevel = ilevel, 
                                    condense = TRUE)
  #total commodity output's share of total product supply
  phi <- bea_io$call_commodity_share_factor(year = year, 
                                             ilevel = ilevel, 
                                             condense = TRUE)

  df <- intermediate_activity(industry_output_matrix = iout,
                              io_b_matrix = bmat,
                              io_supply_matrix = smat,
                              phi = phi,
                              paradigm = paradigm,
                              schedule = schedule,
                              class_system = class_system) %>% 
    as.data.frame.table() %>% 
    `colnames<-`(c("indcode", "place", schedule)) %>%
    mutate(place = as.character(place)) # place as factor works incorrectly when using as matrix index
  
  return(df)  
}

# Call list factor supply and demand 
call_factor_list <- function(year,
                             class_system = c("industry", "commodity"),
                             paradigm = c("factor", "domestic", "capital"),
                             ilevel = c("det", "sum", "sec"),
                             bus_data = c("cbp_imp", "cbp_raw", "infogroup"),
                             cbsa = FALSE,
                             verbose = FALSE){
  
  class_system <- match.arg(class_system)
  paradigm <- match.arg(paradigm)
  ilevel <- match.arg(ilevel)
  bus_data <- match.arg(bus_data)

  cache_path <- glue(opath$iofactor_)
  if (file.exists(cache_path)) {
    log_debug(paste("read from cache", cache_path))
    return(read_parquet(cache_path))
  }
  
  to <- call_output(year = year, 
                    class_system = class_system, 
                    ilevel = ilevel, 
                    bus_data = bus_data,
                    verbose = verbose) 
  
  fs <- call_intermediate(year = year,
                          schedule = "supply",
                          paradigm = paradigm,
                          class_system = class_system, 
                          ilevel = ilevel,
                          bus_data = bus_data,
                          verbose = verbose)
  fd <- call_intermediate(year = year,
                          schedule = "demand",
                          paradigm = paradigm,
                          class_system = class_system, 
                          ilevel = ilevel,
                          bus_data = bus_data,
                          verbose = verbose)
  if(cbsa){
    to <- cbsa_aggregate_output(to, year = year)
    fs <- cbsa_aggregate_output(fs, year = year)
    fd <- cbsa_aggregate_output(fd, year = year)
  }
  
  df <- inner_join(fs, fd, by = join_by(indcode, place)) %>% 
    left_join(to, ., by = join_by(indcode, place)) %>% 
    `colnames<-`(c("indcode", "place", "gross_output", "intermediate_supply", "intermediate_demand"))
  df[is.na(df)]=0
  df$net_supply <- pmax(df$intermediate_supply - df$intermediate_demand, 0)
  df$net_demand <- pmax(df$intermediate_demand - df$intermediate_supply, 0)
  
  log_debug(paste("save to cache", cache_path))
  write_parquet(df, util$mkdir(cache_path))
  return(df)  
  
}
  
# Call intertemporal list factor supply and demand 
call_temporal_factor_list <- function(set_of_years,
                                      class_system = c("industry", "commodity"),
                                      paradigm = c("factor", "domestic", "capital"),
                                      ilevel = c("det", "sum", "sec"),
                                      bus_data = c("cbp_imp", "cbp_raw", "infogroup"),
                                      cbsa = FALSE,
                                      verbose = FALSE){
  df <- util$temp_fun_recur_list(set_of_years = set_of_years, 
                                 call_factor_list, 
                                 paradigm = paradigm,
                                 class_system = class_system, 
                                 ilevel = ilevel,
                                 bus_data = bus_data,
                                 verbose = verbose) %>%
    bind_rows(.id = "id_year")
  return(df) 
}

# trade flow potential ----

# tidy a long factor list into a place indexed table of economic activity (trade flow potential) for a set/sets of sectors
extraction_table <- function(intra_level_concordance,
                             io_factor_list,
                             cluster_level = c("sec", "sum", "det"),
                             cbsa = FALSE,
                             cluster_subset = NULL) {
  cluster_level <- match.arg(cluster_level)
  ilc <- intra_level_concordance
  iol <- io_factor_list
  df <- left_join(iol, ilc, by = "indcode")
  if (!is.null(cluster_subset)){
    df <- df[grepl(cluster_subset, df[[short2long(cluster_level)]]), ]
  }
  df <- df %>%
    {aggregate(.[sapply(.,is.numeric)], list(.[["place"]]), FUN=sum)} %>% 
    `colnames<-`(c("place", names(.)[-1])) 
  return(df)
}

# generate a table of trade flow potential with spatial components
call_extraction_table <- function(year,
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
  ilevel <- match.arg(ilevel)
  cluster_level <- match.arg(cluster_level)
  bea_io$cluster_logic(ilevel, cluster_level)
  ilc <- bea_io$call_intra_level_concordance(year = year, 
                                             cluster_level = cluster_level)
  iol <- call_factor_list(year = year,
                          class_system = class_system,
                          paradigm = paradigm,
                          ilevel = ilevel,
                          bus_data = bus_data,
                          cbsa = cbsa,
                          verbose = verbose)
  df <- extraction_table(intra_level_concordance = ilc,
                         io_factor_list = iol,
                         cluster_level = cluster_level,
                         cbsa = cbsa,
                         cluster_subset = cluster_subset) %>% 
    mutate(extract = intermediate_demand - intermediate_supply) 
  if (spatial){
    geot <- geog$call_geog(year = year, cbsa = cbsa)
    df <- inner_join(geot, df, by = "place", copy = TRUE)
  }
  if (!is.null(trim)){
    df <- df[!grepl(trim, df$place), ]
  }
  return(df)
}




# Tests ----

test_all <- function() {
  for (y in c(2021:1986)) {
    for (s in c("industry", "commodity")) {
      for (i in c("det", "sum", "sec")) {
        for (b in c("cbp_imp", "cbp_raw", "infogroup")) {
          if (b == "infogroup" && y > 2017) next
          call_output(y, s, i, b)
        }
      }
    }
  }
}



# year = 2012
# class_system = c("industry", "commodity")[1]
# ilevel = c("det", "sum", "sec")[3]
# bus_data = c("cbp_imp", "cbp_raw", "infogroup")[1]
# 
# test <- call_output(year = year,
#                     class_system = class_system,
#                     ilevel = ilevel,
#                     bus_data = bus_data)

  
######### notes and other sundry

#Note BEA commodities c("482000", "814000", "S00500", "S00600", "491000", "S00102", "GSLGE",  "GSLGH",  "GSLGO",  "S00900") are drooped/censored 
# either due to either zero overall domestic intermediate usage c("814000", "S00500", "S00600", "GSLGE", "GSLGH", "GSLGO", "S00900") 
# and/or have an industry supply source(s) not derivable from the micro data c("482000", "491000", "S00102") 

#Note BEA industries c("335224", "311230", "482000", "814000", "S00500", "S00600", "491000", "S00101", "S00102", "GSLGE", "GSLGH", "GSLGO", "S00201", "S00202", "S00203") are drooped/censored 
#due to lack of equivalent NIACS concordance c("S00500", "S00600", "S00101", "S00102", "GSLGE", "GSLGH", "GSLGO", "S00201", "S00202", "S00203") and/or no coverage in micro data c("335224", "311230", "482000", "814000", "491000")

#Note: issue stemming from some BEA industries do not exist in underlying data and/or do not have equivalent NAICS codes resulting in NaN's
#as such going from industry to commodity using the C matrix does not work for commodities which have corresponding non-zero elements in the C matrix 
#(rownames(cmat)[rowSums(cmat[,names(rowSums(df))[rowSums(df)==0]])>0], where df is the industry-by-place output matrix) 
#to solve we construct an adjusted C matrix using the derived industry-by-place output totals 
#additional related issue, because industries with known non-zero BEA commodity supply/ industry output are suppressed, when recovering commodity output from an adjusted C matrix all commodity output from the suppressed industry is absent when comparing to BEA commodity output
#to solve we do nothing but take note and adjust expectation of national commodity output downward by removing underiveable industries columns from supply table when constructing the "known" commodity output values

###watch for PR, 999's in CBP (state), and 99990 in infogroup (industry)
  
  # load business data (non-ag by industry)
  # 3 paths (cbp, imputed , infogroup)
  # translate NAISCS to BEA (concordance plus collapse if level) 
  # if cbp payroll -> output
###what to do with 814000, 491000, 482000, 335224, 311230 or "311221" "311230" "322110" "331313" "335110" "335222" "335224" "335228" "335912" "336112" "336120" "336414" "336992" "482000" "491000" "814000"?
### TODO check concordance/cbp validity for place:42007 naics:443 and naics:453 both have ap:4575 AND place:53029 naics:8134 and naics:8139 both have ap:692 Are we double counting?
  # payroll shares from Use table (bea_io$call_bea_use_table()[V00100,]/bea_io$call_industry_output())

  # output = payroll/share
  # if infogroup sales == output
### note some sales do not have BEA equivalents e.g., "921120" "924120" "999990" "922160" "928110" "922110" "926130" "924110" "921130" "923130" "926120" "922190" "926150" "922130" "921190" "922120" "923120" "922150" "922140" "925120" "921110" "923140" "926110" "926140" "923110" "925110" "928120" "927110"
  # drop ag industries
  # rescale output to match national total 
### note scaling is sensitive to selection of places in include/exclude e.g. XX999 FIPS's or PR
### what to do with "S00500" "S00600" "S00101" "S00102" "GSLGE"  "GSLGH"  "GSLGO"  "S00201" "S00202" "S00203" and "325110", "326140", "331314", "334418", "814000"?
  # non-ag columns by industry
  
  # load census data (ag by industry)  
  #agcen$call_agoutput()
  #rescale county's farm industry output proportional to county's share in national ag commodity output
  #agcen$call_agoutput()/bea_io$call_industry_output()
  
  # combine ag and non-ag
  #if class_system == "industry" then concatenate (rowbind)
  #if class_system == "commodity" then apply C matrix?



  
  
