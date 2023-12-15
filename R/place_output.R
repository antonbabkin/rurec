# This script calculates output by industry/commodity at local place level (county or CBSA)



# R libraries ----
library(logger)
log_threshold(DEBUG)
library(arrow)
library(tidyverse)
library(glue)


# R scripts ----
source("R/basic_utilities.R", local = (util <- new.env()))
source("R/dataprep_bea_io.R", local = (bea_io <- new.env()))
source("R/dataprep_cbp.R", local = (cbp <- new.env()))
source("R/dataprep_infogroup.R", local = (ig <- new.env()))
source("R/dataprep_agcensus.R", local = (agcen <- new.env()))


# Data objects ----
ipath <- list(
  ig_ = ig$opath$county_,
  cbp_ = cbp$opath$cbp_
  # data dependencies
)

opath <- list(
  output_ = "data/place_activity/output_{year}_{class_system}_{ilevel}_{bus_data}.pq"
)

clear_outputs <- function() {
  util$clear_paths(opath)
}


# Output ----

call_output <- function(year, 
                        class_system = c("industry", "commodity"), 
                        ilevel = c("det", "sum", "sec"),
                        bus_data = c("cbp_imp", "cbp_raw", "infogroup") ){
                          
  class_system <- match.arg(class_system)
  ilevel <- match.arg(ilevel)
  bus_data <- match.arg(bus_data)

  cache_path <- glue(opath$output_)
  if (file.exists(cache_path)) {
    log_debug(paste("read from cache", cache_path))
    return(read_parquet(cache_path))
  }
  
  #NAICS to BEA industry concordance
  conc <- bea_io$ilevel_concord(ilevel = ilevel, year = util$year2bea_concord(year))
  
  #BEA national gross industry output
  indout <- bea_io$industry_output(year = year, ilevel = ilevel, condense = TRUE) %>% 
    as.data.frame() %>% 
    mutate(indcode = rownames(.))
  
  #BEA national gross commodity output
  comout <- bea_io$commodity_output(year = year, ilevel = ilevel, condense = TRUE) %>% 
    as.data.frame() %>% 
    mutate(indcode = rownames(.))
  
  # #test: list of BEA industries with no conceivable NAICS concordance match or only many-to-one NAICS-to-BEA or both
  # bea_io$industry_output(year = year, ilevel = ilevel, condense = FALSE) %>%
  #   as.data.frame() %>%
  #   mutate(indcode = rownames(.)) %>%
  #   {setdiff(.$indcode, conc$DETAIL)}
  
  # #test: list of condensed BEA industries with no conceivable NAICS concordance match
  # setdiff(indout$indcode, conc$DETAIL)
  
  if(bus_data == "cbp_imp"){
    #EFCY imputed county business patterns annual payroll across NAICS industry hierarchy by place
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
      
      # #test: list of 6-digit NAICS industries with available underlying micro data but no matching BEA industry concordance
      # setdiff(n, cnc$naics)
      
      # BEA industry equivalent industry sales by place
      df <- right_join(cnc, df, by = "naics") %>% 
        select(-naics) %>% 
        na.omit()
  }
  
  # #test: list of condensed BEA industries with a conceivable NAICS concordance value but no available underlying micro data
  # df %>%
  #   `colnames<-`(c("naics", "place", "value")) %>%
  #   full_join(conc, ., by = c("NAICS" = "naics")) %>%
  #   select(-place) %>%
  #   {.[is.na(.$value), ]} %>%
  #   .[[1]] %>%
  #   unique()
  
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
    {aggregate(.$value, list(.$indcode, .$place), FUN=sum)} %>% #TODO: highest single point of computation time consider possible alternatives
    `colnames<-`(c("indcode", "place", "value")) %>% 
    {.[!grepl("^(11)[1-2]", .$indcode), ]} %>% 
    arrange(place)

  #national aggregate non-farm industry "value" to BEA industry gross output ratio
  ps <- df %>% 
    select(-place) %>% 
    {aggregate(.$value, list(.$indcode), FUN=sum)} %>%
    `colnames<-`(c("indcode", "value")) %>% 
    inner_join(., indout, by = "indcode") %>% 
    mutate(share = value / T017 ) %>%
    select(indcode, share)
  
  #nationally adjusted non-farm industry gross output by place (1,000's of dollars) 
  df <- df %>% 
    inner_join(., ps, by = "indcode") %>% 
    mutate(output = value/(share/1000)) %>% 
    select(indcode, place, output)
  
  # #test: check for completeness and uniqueness 
  # length(unique(df$place))*length(unique(df$indcode))==length(df$output)

  # #test: list of condensed BEA non-farm industries with no available underlying micro data
  # df %>%
  #   select(-place) %>%
  #   {.[is.na(.$output), ]} %>%
  #   .$indcode %>%
  #   unique()

  # #test: check aggregation consistency
  # #(note: some BEA industries do not exist in underlying data and/or do not have equivalent NAICS codes resulting in NaN's)
  # df %>%
  #   select(-place) %>%
  #   {aggregate(.$output, list(.$indcode), FUN=sum)} %>%
  #   {all.equal(.[,2]/1000, indout[.[,1], "T017"])}
  
  #Ag census farm sales at BEA detail level by place
  fs <- agcen$call_agoutput(year = year, geo_level = "county")
  
  #BEA-IO C matrix detail-level
  cmat <- bea_io$c_matrix(year = year, ilevel = "det", condense = TRUE)
  
  #BEA-IO D matrix detail-level
  dmat <- bea_io$d_matrix(year = year, ilevel = "det", condense = TRUE)
  
  #Ag census to BEA gross industry output scalar 
  as <- fs %>% 
    select(-place) %>% 
    colSums() %>% 
    as.data.frame() %>%
    `colnames<-`(c("scalar")) %>% 
    {(.)/1000} %>% 
    {(.)/comout[rownames(.), "T007"]} %>% 
    {(.)*diag(cmat[rownames(.), rownames(.)])} %>% 
    {diag(dmat[rownames(.), rownames(.)])/(.)} %>% 
    mutate(indcode = rownames(.))
  
  # #test: check scalar precision at aggregate level 
  # fs %>%
  #   select(-place) %>%
  #   colSums() %>%
  #   {all.equal(as.numeric((.)*as$scalar/1000), indout[as$indcode, "T017"])}
  
  #nationally adjusted BEA detail level farm industry gross output by place (millions of dollars) 
  ag <- fs %>% 
    pivot_longer(!place, names_to = "indcode", values_to = "value") %>% 
    select(indcode, place, value) %>% 
    inner_join(., as, by = "indcode") %>% 
    mutate(output = value*scalar) %>% 
    select(indcode, place, output)
  
  # #test: check aggregation consistency and scalar precision at disaggregate level 
  # ag %>%
  #   select(-place) %>%
  #   {aggregate(.$output, list(.$indcode), FUN=sum)} %>%
  #   {all.equal(.[,2]/1000, indout[.[,1], "T017"])}
  
  #nationally adjusted farm and non-farm industry gross output by place (1,000's of dollars)  (NaN's as Zero's)
  df <- rbind(ag, df) %>% 
    complete(indcode, place, fill = list(value = 0)) %>% 
    replace(is.na(.), 0) %>% 
    arrange(place)
  
  if (ilevel == "sec"){
    # adjusted "indcode" output aggregation consistent with sector ilevel
    df <- df %>% 
      {.[grepl("^(11)", .$indcode), ]} %>% 
      {aggregate(.$output, list(.$place), FUN=sum)} %>% 
      mutate(indcode = "11", .before = 1) %>% 
      `colnames<-`(c("indcode", "place", "output")) %>% 
      rbind(., df[!grepl("^(11)", df$indcode), ]) %>% 
      arrange(place)
  }
  
  if (ilevel == "sum"){
    # adjusted "indcode" output aggregation consistent with summary ilevel
    df <- df %>% 
      {.[grepl("^(11)[1-2]", .$indcode), ]} %>% 
      {aggregate(.$output, list(.$place), FUN=sum)} %>% 
      mutate(indcode = "111CA", .before = 1) %>% 
      `colnames<-`(c("indcode", "place", "output")) %>% 
      rbind(., df[!grepl("^(11)[1-2]", df$indcode), ]) %>% 
      arrange(place)
  }
  
  # #test: check aggregation consistency
  # #(note: disaggregate industries with outputs of zero not tested)
  # df %>%
  #   select(-place) %>%
  #   {aggregate(.$output, list(.$indcode), FUN=sum)} %>%
  #   `colnames<-`(c("indcode", "output")) %>%
  #   mutate_at(vars(output), \(x)(round(x/1000, 6))) %>%
  #   full_join(., indout, by = "indcode") %>%
  #   .[!(.$output==0), ] %>%
  #   {.$output == .$T017} %>%
  #   all()
  
  if (class_system == "commodity"){
    #industry-by-place gross output matrix (1,000's of dollars)
    df <- df %>% 
      pivot_wider(id_cols = "indcode", names_from = "place", values_from = "output") %>% 
      as.data.frame()  %>% 
      `rownames<-`(.$indcode) %>% 
      select(-indcode) %>% 
      as.matrix() 
    
    #BEA supply matrix 
    s_mat <- bea_io$supply_matrix(year = year, ilevel = ilevel, condense = TRUE)
    
    #national industry aggregate of derived gross industry output by place
    x <- rowSums(df)/1000
    
    #alternate scaled "C" matrix using derived values 
    cmat_alt <- (s_mat[, names(x)[x!=0], drop=FALSE] %*% diag(1/as.vector(x[names(x)[x!=0]]))) %>%
      `colnames<-`(names(x)[x!=0]) 

    #commodity-by-place gross output matrix (1,000's of dollars)
    df <- cmat_alt%*%df[names(x)[x!=0], ]
    
    #nationally adjusted farm and non-farm commodity gross output by place (1,000's of dollars)  (NaN's as Zero's)
    df <- df %>% 
      as.data.frame.table() %>% 
      `colnames<-`(c("indcode", "place", "output"))
    
    # #test: list of condensed BEA industries with zero total aggregate gross output derived from micro data
    # names(x)[x==0]
    
    # #test: check aggregation consistency and alternate C matrix scalar precision at disaggregate level 
    # df %>%
    #   select(-place) %>%
    #   {aggregate(.$output, list(.$indcode), FUN=sum)} %>%
    #   `colnames<-`(c("indcode", "output")) %>%
    #   mutate_at(vars(output), \(x)(round(x/1000, 6))) %>%
    #   full_join(., comout, by = "indcode") %>%
    #   full_join(., data.frame("indcode" = rownames(s_mat), "altcomout" = rowSums(s_mat[, names(x)[x!=0]])), by = "indcode") %>%
    #   {.$output == .$altcomout} %>%
    #   all()
    
    # #test: list of effected condensed BEA commodities for which an adjusted C matrix corrected aggregation inconsistencies for commodity gross output derived from micro data
    # df %>%
    #   select(-place) %>%
    #   {aggregate(.$output, list(.$indcode), FUN=sum)} %>%
    #   `colnames<-`(c("indcode", "output")) %>%
    #   mutate_at(vars(output), \(x)(round(x/1000, 6))) %>%
    #   full_join(., comout, by = "indcode") %>% .[.$output!=.$T007, ] %>% .$indcode
    
  }

  log_debug(paste("save to cache", cache_path))
  write_parquet(df, util$mkdir(cache_path))
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

  #dmatrix  "what percent of total commodity output is produced by the column industry?" 
  #cmatrix "what percent of an industries total output does the column commodity account for?"
  # Assume ag census is equivalent to BEA T007 total commodity output. 
  # 1) get national aggregate and construct ratio to scale place values accordingly as with CBP/infogroup industry figures.
  # 2) observe from BEA supply that some fraction of total commodity output is produced by some non-farm industries
  # 3) use the D matrix to find the "Industry Source of Commodity Outputs" i.e., the fraction of total commodity output that is produced by an industry
  # 4) derive national aggregate and construct ratio to scale place values accordingly as with CBP/infogroup industry figures
  # 5) alternately derive single combined scale factor at the national level: (diag(beacom)^-1)*(agcen*i)=share, D*(diag(agcen*i)*diag(share)^-1)=NWQ, (diag(beaind)^-1)*diag_e(NWQ)=comb_ratio
  ## (diag(beaind)^-1)*diag_e(D*(diag(agcen*i)*diag((diag(beacom)^-1)*(agcen*i))))=comb_ratio
  ## simplify if assuming NWQ of D and in particular C is strictly square and diagonal (as it is for 2007, 2012 and 2017)
  #agc*((D/C)/share)=farm_industry_output
  
  
###watch for PR, 999's in CBP (state), and 99990 in infogroup (industry)
  
  
  # load business data (non-ag by industry)
  # 3 paths (cbp, imputed , infogroup)
  # translate NAISCS to BEA (concordance plus collapse if level) 
  # if cbp payroll -> output
###what to do with 814000, 491000, 482000, 335224, 311230 or "311221" "311230" "322110" "331313" "335110" "335222" "335224" "335228" "335912" "336112" "336120" "336414" "336992" "482000" "491000" "814000"?
### TODO check concordance/cbp validity for place:42007 naics:443 and naics:453 both have ap:4575 AND place:53029 naics:8134 and naics:8139 both have ap:692 Are we double counting?
  # payroll shares from Use table (bea_io$call_use_table()[V00100,]/bea_io$industry_output())

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
  #agcen$call_agoutput()/bea_io$industry_output()
  
  # combine ag and non-ag
  #if class_system == "industry" then concatenate (rowbind)
  #if class_system == "commodity" then apply C matrix?

### issue stemming from some BEA industries do not exist in underlying data and/or do not have equivalent NAICS codes resulting in NaN's
### as such going from industry to commodity using the C matrix does not work for commodities which have corresponding non-zero elements in the C matrix 
### (rownames(cmat)[rowSums(cmat[,names(rowSums(df))[rowSums(df)==0]])>0], where df is the industry-by-place output matrix) 
### to solve we construct an adjusted C matrix using the derived industry-by-place output totals 
### additional related issue, because industries with known non-zero BEA commodity supply/ industry output are suppressed, when recovering commodity output from an adjusted C matrix all commodity output from the suppressed industry is absent when comparing to BEA commodity output
### to solve we do nothing but take note and adjust expectation of national commodity output downward by removing underiveable industries columns from supply table when constructing the "known" commodity output values
  

  
  
