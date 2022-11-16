# Clean imported outside data and generate new data products

# Display start time
log_info("Define data start")

# Connect and parse code from "functions" file 
source(file.path(find_rstudio_root_file(), "nbs", "rural_typology_r_functions.R"))

# Turn Off Scientific Notation 
options(scipen = 999)

# Define data directory path
data_dir = file.path(find_rstudio_root_file(), "data", "robjs")

# Import and cleanup pubdata Industry by Industry Total Requirements tables
if (!file.exists(file.path(data_dir, "TR_matp"))){
  TR_matp <- list()
  TR_matp[["Sector"]] <- bea_io$get_ixi(2020L, "sec")
  TR_matp[["Summary"]] <- bea_io$get_ixi(2020L, "sum")
  TR_matp[["Detail"]] <- bea_io$get_ixi(2012L, "det")
  saver(TR_matp)
  rm(TR_matp) 
}
log_info("Import 'pubdata' TR matrix complete")

# Import and cleanup pubdata BEA Use tables
if (!file.exists(file.path(data_dir, "SUT_matp"))){
  SUT_matp <- list()
  SUT_matp[["Sector"]] <- bea_io$get_use(2020L, "sec")
  SUT_matp[["Summary"]] <- bea_io$get_use(2020L, "sum")
  SUT_matp[["Detail"]] <- bea_io$get_use(2012L, "det")
  saver(SUT_matp)
  rm(SUT_matp) 
}
log_info("Import 'pubdata' SUT matrix complete")

# Import and cleanup pubdata County Business Patterns data
if (!file.exists(file.path(data_dir, "CBP"))){
  CBP <- cbp$get_df("county", 2020L)
  CBP %<>% rename(NAICS = industry)
  CBP$place <- paste0(CBP$fipstate, CBP$fipscty)
  CBP %<>% select(fipstate, fipscty, place, NAICS, emp, qp1, ap, est)
  saver(CBP)
  rm(CBP) 
}
log_info("Import 'pubdata' CBP complete")


# Specific Detail level concordance
if (!file.exists(file.path(data_dir, "det_cord"))){
  x <- bea_io$get_naics_df() %>% filter(NAICS != "n.a.") %>% filter(NAICS != "NaN") 
  det_cord <- c()
  det_cord$DETAIL <- unlist(x$DETAIL, use.names = FALSE) 
  det_cord$NAICS <- unlist(x$NAICS, use.names = FALSE) 
  det_cord <- as.data.frame(det_cord)
  det_cord$NAICS <- ifelse(det_cord$NAICS == "531" & det_cord$DETAIL == "531HST", "531*",  det_cord$NAICS)
  det_cord %<>% add_row(DETAIL = "23", NAICS = "23", .after = 39)
  det_cord %<>% filter(NAICS != "23*")
  det_cord %<>% filter(NAICS != "531*")
  det_cord <- det_cord[order(det_cord$NAICS), ]
  rownames(det_cord) <- 1:nrow(det_cord)
  write.csv(det_cord, file.path(find_rstudio_root_file(), "data", "det_cord"), row.names = FALSE)
  saver(det_cord)
  rm(det_cord, x)
}

# Specific Summary level concordance
if (!file.exists(file.path(data_dir, "sum_cord"))){
  x <- bea_io$get_naics_df() %>% filter(NAICS != "n.a.") %>% filter(NAICS != "NaN") 
  sum_cord <- c()
  sum_cord$SUMMARY <- unlist(x$SUMMARY, use.names = FALSE) 
  sum_cord$NAICS <- unlist(x$NAICS, use.names = FALSE) 
  sum_cord <- as.data.frame(sum_cord)
  sum_cord$NAICS <- ifelse(sum_cord$NAICS == "531" & sum_cord$SUMMARY == "HS", "531*",  sum_cord$NAICS)
  sum_cord %<>% add_row(SUMMARY = "23", NAICS = "23", .after = 39)
  sum_cord %<>% filter(NAICS != "23*")
  sum_cord %<>% filter(NAICS != "531*")
  sum_cord <- sum_cord[order(sum_cord$NAICS), ]
  rownames(sum_cord) <- 1:nrow(sum_cord)
  sum_cord$SUM <- substr(sum_cord$NAICS, 1, 3)
  sum_cord <- distinct(sum_cord, SUM, .keep_all = TRUE)
  write.csv(sum_cord, file.path(find_rstudio_root_file(), "data", "sum_cord"), row.names = FALSE)
  saver(sum_cord)
  rm(sum_cord, x)
}

# Specific Sector level concordance
if (!file.exists(file.path(data_dir, "sec_cord"))){
  sec_cord <- c()
  sec_cord$SECTOR <- c("11", "21", "22", "23", "31G", "42", "44RT", "48TW", "51", "FIRE", "FIRE", "PROF", "PROF", "PROF", "6", "6", "7", "7", "81")
  sec_cord$NAICS <- c("11", "21", "22", "23", "31", "42", "44", "48", "51", "52", "53", "54", "55", "56", "61", "62", "71", "72", "81")
  sec_cord <- as.data.frame(sec_cord)
  write.csv(sec_cord, file.path(find_rstudio_root_file(), "data", "sec_cord"), row.names = FALSE)
  saver(sec_cord)
  rm(sec_cord)
}

# Generate full economic industry/county table consolidating NAICS to BEA codes at the detail level 
if (!file.exists(file.path(data_dir, "CBP_Concord_Detail_XBEA"))){
  CBP_Concord_Detail <- concordr("det_cord", "CBP")
  reshaper(CBP_Concord_Detail, "det_cord")
  saver(CBP_Concord_Detail_XBEA)
  rm(CBP_Concord_Detail_XBEA, CBP_Concord_Detail)
}
log_info("Detail level CBP/BEA crosswalk complete")

# Generate full economic industry/county table consolidating NAICS to BEA codes at the Summary level 
if (!file.exists(file.path(data_dir, "CBP_Concord_Summary_XBEA"))){
  CBP_Concord_Summary <- concordr("sum_cord", "CBP")
  reshaper(CBP_Concord_Summary, "sum_cord")
  saver(CBP_Concord_Summary_XBEA)
  rm(CBP_Concord_Summary_XBEA, CBP_Concord_Summary)
}
log_info("Summary level CBP/BEA crosswalk complete")

# Generate full economic industry/county table consolidating NAICS to BEA codes at the Sector level 
if (!file.exists(file.path(data_dir, "CBP_Concord_Sector_XBEA"))){
  CBP_Concord_Sector <- concordr("sec_cord", "CBP")
  reshaper(CBP_Concord_Sector, "sec_cord")
  saver(CBP_Concord_Sector_XBEA)
  rm(CBP_Concord_Sector_XBEA, CBP_Concord_Sector)
}
log_info("Sector level CBP/BEA crosswalk complete")

# Generate full economic industry/county list consolidating NAICS to BEA codes at the all levels 
if (!file.exists(file.path(data_dir, "CBP_Concord_XBEA"))){
  CBP_Concord_XBEA <- list()
  CBP_Concord_XBEA[["Sector"]] <- file.path(data_dir, "CBP_Concord_Sector_XBEA") %>% readRDS()
  CBP_Concord_XBEA[["Summary"]] <- file.path(data_dir, "CBP_Concord_Summary_XBEA") %>% readRDS()
  CBP_Concord_XBEA[["Detail"]] <- file.path(data_dir, "CBP_Concord_Detail_XBEA") %>% readRDS()
  saver(CBP_Concord_XBEA)
  rm(CBP_Concord_XBEA)
}
log_info("Sector level CBP/BEA crosswalk complete")


### Test example of censoring/noise-infusion data loss
# sum(CBP_Concord_Detail_XBEA$ap) / sum(filter(CBP, NAICS == '-')$ap)
# sum(CBP_Concord_Detail_XBEA$emp) / sum(filter(CBP, NAICS == '-')$emp)

# Import and cleanup "pubdata" TIGER data
if (!file.exists(file.path(data_dir, "TIGERDatap"))){
  TIGERDatap <- geography$get_county_df(year=2020L, geometry=TRUE, scale="500k")
  TIGERDatap %<>% rename(place = CODE)
  TIGERDatap$center <- st_centroid(TIGERDatap$geometry)
  TIGERDatap %<>% arrange(place)
  saver(TIGERDatap)
  rm(TIGERDatap) 
}
log_info("Import 'pubdata' TIGER complete")

### Test example of missing CBP coverage 
# setdiff(unique(TIGERDatap$place), unique(CBP_Concord_Detail_XBEA$place) )

# Produce  Distance  Matrix
if (!file.exists(file.path(data_dir, "Dist_mat"))){
  Dist_mat <- file.path(data_dir, "TIGERDatap") %>% readRDS() %>% .$center %>% as_Spatial() %>% distm()
  rownames(Dist_mat) = colnames(Dist_mat) <- file.path(data_dir, "TIGERDatap") %>% readRDS() %>%  .$place
  saver(Dist_mat)
  rm(Dist_mat) 
}
log_info("Distance Matrix complete")

## Produce Proximity  Matrix
if (!file.exists(file.path(data_dir, "Prox_mat"))){
  Prox_mat <-  file.path(data_dir, "TIGERDatap") %>% readRDS() %>% poly2nb(queen = TRUE) %>% nb2mat(style = "B", zero.policy = TRUE)
  colnames(Prox_mat) <- rownames(Prox_mat)
  saver(Prox_mat)
  rm(Prox_mat)
}
log_info("Proximity Matrix complete")

# Import and cleanup pubdata RUCC data
if (!file.exists(file.path(data_dir, "RUCCDatap"))){
  RUCCDatap <- ers_rurality$get_ruc_df()
  RUCCDatap %<>% filter(RUC_YEAR=="2013")
  RUCCDatap$place <- RUCCDatap$FIPS
  saver(RUCCDatap)
  rm(RUCCDatap) 
}
log_info("Import 'pubdata' RUCC complete")

# TIGER  and RUCC
if (!file.exists(file.path(data_dir, "TIGER_RUCC"))){
  TIGER_RUCC <- inner_join(readRDS(file.path(data_dir, "TIGERDatap")), readRDS(file.path(data_dir, "RUCCDatap")), by = "place")
  ### Note: Four non overlapping counties from each ("02063" "02066" "02158" "46102") and ("02261" "02270" "46113" "51515")
  ##setdiff(TIGERDatap$place, RUCCDatap$place)
  ##setdiff(RUCCDatap$place, TIGERDatap$place)
  TIGER_RUCC <- TIGER_RUCC[order(TIGER_RUCC$place), ]
  rownames(TIGER_RUCC) <- TIGER_RUCC$place
  TIGER_RUCC <- transform(TIGER_RUCC, H3 = ifelse(RUC_CODE %in% 1:3, 1, ifelse(RUC_CODE %in% 4:6, 2, ifelse(RUC_CODE%in% 7:9, 3, 0)  ) ) )
  saver(TIGER_RUCC)
  rm(TIGER_RUCC)
}
log_info("TIGER/RUCC merge complete")



### Total Requirements matrix
if (file.exists(file.path(data_dir, "Total_mat"))) {
  log_info("Total requirements matrix already exists")
} else {
  local({
    Total_mat <- list()
    for (i in c("Sector", "Summary", "Detail")){
      df <-   file.path(data_dir, "TR_matp")  %>% readRDS() %>% .[[i]] %>% .[1:ncol(.), ] %>% as.matrix()
      rownames(df) <- colnames(df)
      Total_mat[[i]] <- df
    }
    
    ### Detail level
    # Collapse ambiguous industry 23* codes
    # >     rownames(tmat)[25:36]
    # [1] "233210" "233262" "230301" "230302" "2332A0" "233412" "2334A0" "233230" "2332D0" "233240" "233411" "2332C0"
    # TODO: double-check this calculation
    tmat <- Total_mat$Detail
    d <- sum(tmat[25:36, 25:36]) / 12
    tmat[, 25] <- rowMeans(tmat[, 25:36])
    tmat <- tmat[, -c(26:36)]
    tmat[25, ] <- colMeans(tmat[25:36, ])
    tmat <- tmat[-c(26:36), ]
    tmat[25, 25] <- d
    colnames(tmat)[25] <- rownames(tmat)[25] <- "23"
    Total_mat$Detail <- tmat
    saver(Total_mat)
  })
  log_info("Total requirements matrix complete")
}


### Labor share
if (file.exists(file.path(data_dir, "Labor_mat"))) {
  log_info("Labor matrix already exists")
} else {
  local({
    Labor_mat <- list()
    for (i in c("Sector", "Summary")){
      df <- file.path(data_dir, "SUT_matp")  %>% readRDS() %>% .[[i]] %>% as.matrix()
      Labor_mat[[i]] <- df["V001", , drop = FALSE]/df["T018", , drop = FALSE]
      rownames( Labor_mat[[i]] ) <- "labor_share"
    }
    
    ### Detail level
    # Collapse ambiguous industry 23* codes
    # add up all construction columns together
    df <- file.path(data_dir, "SUT_matp")  %>% readRDS() %>% .[["Detail"]] %>% as.matrix()
    l <- 1:24
    c <- 25:36
    r <- 37:405
    df <- cbind(df[, l], rowSums(df[, c], na.rm=TRUE), df[, r])
    colnames(df)[25] <- "23"
    Labor_mat[["Detail"]] <- df["V00100", , drop = FALSE]/df["T018", , drop = FALSE]
    rownames( Labor_mat[["Detail"]] ) <- "labor_share"
    saver(Labor_mat)
  })
  log_info("Labor matrix complete")
}


#Extract county level industry payroll data and reshape to industry-by-county matrix
if (!file.exists(file.path(data_dir, "Xpay_mat"))){
  importr(CBP_Concord_XBEA)
  Xpay_mat <- lapply(CBP_Concord_XBEA, reshape_output_long_wide)
  saver(Xpay_mat)
  rm(Xpay_mat, CBP_Concord_XBEA)
}  
log_info("Payroll matrix complete")



#Total output Matrix in thousands of dollars
if (file.exists(file.path(data_dir, "Output_mat"))) {
  log_info("Total output matrix already exists")
} else {
  local({
    importr(Xpay_mat)
    importr(Labor_mat)
    
    # prepare farm sales
    ### Note: only temporary until pubdata.agcensus.md complete 
    ###download file from agcensus_sales_by_bea_v220909.csv google sharedrive rurec/Data (https://drive.google.com/file/d/1gDufjSB3vXaloBtlYOjfo8IuSTgebpT3/view?usp=share_link)
    download.file(url="https://drive.google.com/uc?export=download&id=1gDufjSB3vXaloBtlYOjfo8IuSTgebpT3", 
                  destfile=file.path(find_rstudio_root_file(), "data", "agcensus_sales_by_bea_v220909.csv"))
    farm_sales_file <- file.path(find_rstudio_root_file(), "data", "agcensus_sales_by_bea_v220909.csv")
    
    df <- read.csv(farm_sales_file, colClasses = "character")
    df$SALES <- df$SALES %>% as.numeric / 1000
    df <- df %>% filter(AGG_LEVEL_DESC == "COUNTY") %>% select(!AGG_LEVEL_DESC)
    df <- reshape(df, direction = "wide", idvar = "STCTY", timevar = "BEA_INDUSTRY_DETAIL")
    colnames(df) <- gsub("SALES.", "", colnames(df))
    farm_sales <- df
    farm_ind_detail <- colnames(farm_sales)[-c(1)]
    
    Output_mat <- list()
    for (l in names(Xpay_mat)) {
      payroll <- Xpay_mat[[l]]
      # labor shares only for industries present in payroll table
      labor_share <- Labor_mat[[l]][, rownames(payroll)] %>% unlist()
      output <- apply(payroll, 2, function (x) {x / labor_share})
      output <- t(output) %>% as.data.frame()
      output$STCTY <- rownames(output)
      output <- left_join(output, farm_sales, by="STCTY")
      
      if (l == "Sector") {
        output[["11"]] <- rowSums(output[, c("11", farm_ind_detail)], na.rm=T)
        output <- output %>% select(!c(farm_ind_detail))
      } else if (l == "Summary") {
        output[["111CA"]] <- rowSums(output[, c(farm_ind_detail)], na.rm=T)
        output <- output %>% select(!c(farm_ind_detail)) %>% select("111CA", everything())
      } else if (l == "Detail") {
        output <- output %>% select(farm_ind_detail, everything())
      }
      
      rownames(output) <- output$STCTY
      output$STCTY <- NULL
      output <- t(output)
      Output_mat[[l]] <- output
    }
    saver(Output_mat)
  })
  rm(Xpay_mat, Labor_mat)
  log_info("Total output matrix complete")
}


#### Direct requirements matrices (Technical Coefficients) 
if (!file.exists(file.path(data_dir, "Direct_mat"))){
  Total_mat <- readRDS(file.path(data_dir, "Total_mat"))
  Direct_mat <- Total_mat
  for (l in 1:length(Total_mat)){
    Direct_mat[[l]] <- diag(ncol(Total_mat[[l]])) - solve(Total_mat[[l]])
  }
  saver(Direct_mat)
  rm(Direct_mat, Total_mat)
} 
log_info("Direct requirements matrix complete")


# ###Note: Get impedance to work with Dist_mat and develop more flexible functionality
# #Specify spatial impedance structure
# if (!file.exists(file.path(data_dir, "Impede_mat"))){
#   importr(D_mat) 
#   Impede_mat <- vector(mode='list', length = 3)
#   
#   ### inverse square function
#   Impede_mat[[1]] <- D_mat
#   for (i in 1:length(D_mat)){
#     Impede_mat[[1]][[i]] <- ((1/(D_mat[[i]])^2))
#   }
#   ### exponential decay function
#   Impede_mat[[2]] <- D_mat
#   for (i in 1:length(D_mat)){
#     Impede_mat[[2]][[i]] <- (exp(-(D_mat[[i]]/10000)) )
#   }
#   ### hyperbolic secant function
#   Impede_mat[[3]] <- D_mat
#   for (i in 1:length(D_mat)){
#     Impede_mat[[3]][[i]] <-  ((2/(exp(-(D_mat[[i]]/1000000)) + exp(D_mat[[i]]/1000000))))
#   }
#   ### hyperbolic secant function 2
#   Impede_mat[[4]] <- D_mat
#   for (i in 1:length(D_mat)){
#     Impede_mat[[4]][[i]] <-  ((2/(exp(-(D_mat[[i]]/10000000)) + exp(D_mat[[i]]/10000000))))
#   }
#   saver(Impede_mat)
#   rm(D_mat, Impede_mat)
# }
# log_info("Spatial impedance structure complete")


# Remove clutter
rm(data_dir, l) %>% suppressWarnings()


# Display end time
log_info("Define data end")

##### prox test
#test <- Sim_mat_imp_rel*Prox_mat
#test <- replace(test, test<=0, Inf)








