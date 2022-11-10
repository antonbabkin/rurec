# Generate new data products

# Load and attach necessary packages
library(rprojroot)
library(rlog)

# Display start time
log_info("Define data generation start")

# Connect  and  parse  code  from  another  file 
source(file.path(find_rstudio_root_file(), "nbs", "rural_typology_r_data_clean.R"))

options(scipen = 999)

data_dir = file.path(find_rstudio_root_file(), "data", "robjs")
industry_levels  = c("Sector", "Summary", "Detail")


### Total Requirements matrix
if (file.exists(file.path(data_dir, "Total_mat"))) {
  log_info("Total requirements matrix already exists")
} else {
  local({
    Total_mat <- list()
    
    ### Sector level
    raw_table <- readRDS(file.path(data_dir, "IO_tables"))[["IxI_TR_1997-2020_PRO_SEC"]][["2020"]]
    tmat <- raw_table[6:20, 3:17] %>% lapply(as.numeric) %>% as.data.frame() %>% as.matrix()
    rownames(tmat) <- colnames(tmat) <- raw_table[4, 3:17]
    Total_mat[["Sector"]] <- tmat
    
    ### Summary level
    raw_table <- readRDS(file.path(data_dir, "IO_tables"))[["IxI_TR_1997-2020_PRO_SUM"]][["2020"]]
    tmat <- raw_table[6:76, 3:73] %>% lapply(as.numeric) %>% as.data.frame() %>% as.matrix()
    rownames(tmat) <- colnames(tmat) <- raw_table[4, 3:73]
    Total_mat[["Summary"]] <- tmat
    
    ### Detail level
    raw_table <- readRDS(file.path(data_dir, "IO_tables"))[["IxI_TR_2007_2012_PRO_DET"]][["2012"]]
    tmat <- raw_table[4:408, 3:407] %>% lapply(as.numeric) %>% as.data.frame() %>% as.matrix()
    rownames(tmat) <- colnames(tmat) <- raw_table[3, 3:407]
    # Collapse ambiguous industry 23* codes
    # >     rownames(tmat)[25:36]
    # [1] "233210" "233262" "230301" "230302" "2332A0" "233412" "2334A0" "233230" "2332D0" "233240" "233411" "2332C0"
    # TODO: double-check this calculation
    d <- sum(tmat[25:36, 25:36]) / 12
    tmat[, 25] <- rowMeans(tmat[, 25:36])
    tmat <- tmat[, -c(26:36)]
    tmat[25, ] <- colMeans(tmat[25:36, ])
    tmat <- tmat[-c(26:36), ]
    tmat[25, 25] <- d
    colnames(tmat)[25] <- rownames(tmat)[25] <- "23"
    Total_mat[["Detail"]] <- tmat
    
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
    
    ### Sector level
    io_table <- readRDS(file.path(data_dir, "IO_tables"))[["Use_SUT_Framework_1997-2020_SECT"]][["2020"]]
    df <- io_table[6:31, 3:17]
    df[] <- lapply(df, as.numeric)
    colnames(df) <- unlist(io_table[4, 3:17])
    rownames(df) <- unlist(io_table[6:31, 2])
    labor_share <- df["Compensation of employees", ] / df["Total industry output (basic prices)", ]
    Labor_mat[[1]] <- matrix(labor_share, dimnames = list(colnames(df), "labor_share"))
    
    ### Summary level
    io_table <- readRDS(file.path(data_dir, "IO_tables"))[["Use_SUT_Framework_1997-2020_Sum"]][["2020"]]
    df <- io_table[6:87, 3:73]
    df[] <- lapply(df, as.numeric)
    colnames(df) <- unlist(io_table[4, 3:73])
    rownames(df) <- unlist(io_table[6:87, 2])
    labor_share <- df["Compensation of employees", ] / df["Total industry output (basic prices)", ]
    Labor_mat[[2]] <- matrix(labor_share, dimnames = list(colnames(df), "labor_share"))
    
    ### Detail level
    io_table <- readRDS(file.path(data_dir, "IO_tables"))[["Use_SUT_Framework_2007_2012_DET"]][["2012"]]
    df <- io_table[5:418, 3:407]
    df[] <- lapply(df, as.numeric)
    colnames(df) <- unlist(io_table[4, 3:407])
    rownames(df) <- unlist(io_table[5:418, 2])
    # add up all construction columns together
    cols_constr_left <- 1:24
    cols_constr <- 25:36
    cols_constr_right <- 37:405
    df <- cbind(df[, cols_constr_left], rowSums(df[, cols_constr], na.rm=TRUE), df[, cols_constr_right])
    colnames(df)[25] <- "23"
    labor_share <- df["Compensation of employees", ] / df["Total industry output (basic value)", ]
    Labor_mat[[3]] <- matrix(labor_share, dimnames = list(colnames(df), "labor_share"))
    
    
    names(Labor_mat) <- industry_levels
    saver(Labor_mat)
  })
  log_info("Labor matrix complete")
}



  
#Extract county level industry employment data and reshape to industry-by-county matrix
if (!file.exists(file.path(data_dir, "Xemp_mat"))){
  Xemp_mat <- list()
 ### Sector level
    Xemp_mat[[1]] <- file.path(data_dir, "CBP_2019p_Concord_Sector_XBEA") %>% readRDS() %>% .[, c("indcode", "place", "emp")]
    Xemp_mat[[1]] %<>% reshape(idvar = "indcode", v.names = "emp", varying = unique(Xemp_mat[[1]]$place), timevar = "place", new.row.names = unique(Xemp_mat[[1]]$indcode),  direction = "wide")
    Xemp_mat[[1]] %<>% as.data.frame() %>% arrange(factor(Xemp_mat[[1]][[1]], levels = rownames(readRDS(file.path(data_dir, "Total_mat"))[[1]])), .by_group = TRUE) 
    Xemp_mat[[1]] %<>% subset(select = -c(indcode)) %>% as.matrix()
 ### Summary level   
    Xemp_mat[[2]] <- file.path(data_dir, "CBP_2019p_Concord_Summary_XBEA") %>% readRDS() %>% .[, c("indcode", "place", "emp")]
    Xemp_mat[[2]] %<>% reshape(idvar = "indcode", v.names = "emp", varying = unique(Xemp_mat[[2]]$place), timevar = "place", new.row.names = unique(Xemp_mat[[2]]$indcode),  direction = "wide")
    Xemp_mat[[2]] %<>% as.data.frame() %>% arrange(factor(Xemp_mat[[2]][[1]], levels = rownames(readRDS(file.path(data_dir, "Total_mat"))[[2]])), .by_group = TRUE)
    Xemp_mat[[2]] %<>% subset(select = -c(indcode)) %>% as.matrix()
 ### Detail level  
    Xemp_mat[[3]] <- file.path(data_dir, "CBP_2019p_Concord_Detail_XBEA") %>% readRDS() %>% .[, c("indcode", "place", "emp")]
    Xemp_mat[[3]] %<>% reshape(idvar = "indcode", v.names = "emp", varying = unique(Xemp_mat[[3]]$place), timevar = "place", new.row.names = unique(Xemp_mat[[3]]$indcode),  direction = "wide")
    Xemp_mat[[3]] %<>% as.data.frame() %>% arrange(factor(Xemp_mat[[3]][[1]], levels = rownames(readRDS(file.path(data_dir, "Total_mat"))[[3]])), .by_group = TRUE) 
    Xemp_mat[[3]] %<>% subset(select = -c(indcode)) %>% as.matrix()
    
    names(Xemp_mat) <- industry_levels
  saver(Xemp_mat)
  rm(Xemp_mat)
}  
log_info("Employment matrix complete")

  
#Extract county level industry payroll data and reshape to industry-by-county matrix
if (!file.exists(file.path(data_dir, "Xpay_mat"))){
  Xpay_mat <- list()
  ### Sector level
  Xpay_mat[[1]] <- file.path(data_dir, "CBP_2019p_Concord_Sector_XBEA") %>% readRDS() %>% .[, c("indcode", "place", "ap")]
  Xpay_mat[[1]] %<>% reshape(idvar = "indcode", v.names = "ap", varying = unique(Xpay_mat[[1]]$place), timevar = "place", new.row.names = unique(Xpay_mat[[1]]$indcode),  direction = "wide")
  Xpay_mat[[1]] %<>% as.data.frame() %>% arrange(factor(Xpay_mat[[1]][[1]], levels = rownames(readRDS(file.path(data_dir, "Total_mat"))[[1]])), .by_group = TRUE) 
  Xpay_mat[[1]] %<>% subset(select = -c(indcode)) %>% as.matrix()
  ### Summary level   
  Xpay_mat[[2]] <- file.path(data_dir, "CBP_2019p_Concord_Summary_XBEA") %>% readRDS() %>% .[, c("indcode", "place", "ap")]
  Xpay_mat[[2]] %<>% reshape(idvar = "indcode", v.names = "ap", varying = unique(Xpay_mat[[2]]$place), timevar = "place", new.row.names = unique(Xpay_mat[[2]]$indcode),  direction = "wide")
  Xpay_mat[[2]] %<>% as.data.frame() %>% arrange(factor(Xpay_mat[[2]][[1]], levels = rownames(readRDS(file.path(data_dir, "Total_mat"))[[2]])), .by_group = TRUE) 
  Xpay_mat[[2]] %<>% subset(select = -c(indcode)) %>% as.matrix()
  ### Detail level  
  Xpay_mat[[3]] <- file.path(data_dir, "CBP_2019p_Concord_Detail_XBEA") %>% readRDS() %>% .[, c("indcode", "place", "ap")]
  Xpay_mat[[3]] %<>% reshape(idvar = "indcode", v.names = "ap", varying = unique(Xpay_mat[[3]]$place), timevar = "place", new.row.names = unique(Xpay_mat[[3]]$indcode),  direction = "wide")
  Xpay_mat[[3]] %<>% as.data.frame() %>% arrange(factor(Xpay_mat[[3]][[1]], levels = rownames(readRDS(file.path(data_dir, "Total_mat"))[[3]])), .by_group = TRUE) 
  Xpay_mat[[3]] %<>% subset(select = -c(indcode)) %>% as.matrix()
  
    names(Xpay_mat) <- industry_levels
  saver(Xpay_mat)
  rm(Xpay_mat)
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
    farm_sales_file <- file.path(find_rstudio_root_file(), "data", "nass", "agcensus_sales_by_bea_v220909.csv")
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
      labor_share <- Labor_mat[[l]][rownames(payroll), ] %>% unlist
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
    names(Direct_mat) <- industry_levels
    
    for (l in 1:length(Total_mat)){
      Direct_mat[[l]] <- diag(ncol(Total_mat[[l]])) - solve(Total_mat[[l]])
    }

  saver(Direct_mat)
  rm(Direct_mat, Total_mat)
} 
log_info("Direct requirements matrix complete")



#### Trimming distance decay matrix to only counties with economic data (varies by industry level specification)   
if (!file.exists(file.path(data_dir, "D_mat"))){
  importr(Total_mat)
  importr(CBP_2019p_Concord_Sector_XBEA)
  importr(CBP_2019p_Concord_Summary_XBEA)
  importr(CBP_2019p_Concord_Detail_XBEA)
  importr(Dist_mat)
  importr(TIGERData)
  importr(RUCCData)
  
    D_mat <- vector(mode='list', length=length(Total_mat))
    names(D_mat) <- industry_levels
    
    D_mat[[1]] <- Dist_mat
    D_mat[[1]] <- D_mat[[1]][rownames(D_mat[[1]]) %in% intersect(unique(CBP_2019p_Concord_Sector_XBEA$place), intersect(unique(TIGERData$place), unique(RUCCData$place) ) ), ]
    D_mat[[1]] <- D_mat[[1]][ , colnames(D_mat[[1]]) %in% intersect(unique(CBP_2019p_Concord_Sector_XBEA$place), intersect(unique(TIGERData$place), unique(RUCCData$place) ) ) ]
    
    D_mat[[2]] <- Dist_mat
    D_mat[[2]] <- D_mat[[2]][rownames(D_mat[[2]]) %in% intersect(unique(CBP_2019p_Concord_Summary_XBEA$place), intersect(unique(TIGERData$place), unique(RUCCData$place) ) ), ]
    D_mat[[2]] <- D_mat[[2]][ , colnames(D_mat[[2]]) %in% intersect(unique(CBP_2019p_Concord_Summary_XBEA$place), intersect(unique(TIGERData$place), unique(RUCCData$place) ) ) ]
    
    D_mat[[3]] <- Dist_mat
    D_mat[[3]] <- D_mat[[3]][rownames(D_mat[[3]]) %in% intersect(unique(CBP_2019p_Concord_Detail_XBEA$place), intersect(unique(TIGERData$place), unique(RUCCData$place) ) ), ]
    D_mat[[3]] <- D_mat[[3]][ , colnames(D_mat[[3]]) %in% intersect(unique(CBP_2019p_Concord_Detail_XBEA$place), intersect(unique(TIGERData$place), unique(RUCCData$place) ) ) ]
    
  saver(D_mat)
  rm(D_mat, Total_mat, CBP_2019p_Concord_Sector_XBEA, CBP_2019p_Concord_Summary_XBEA, CBP_2019p_Concord_Detail_XBEA, Dist_mat, TIGERData, RUCCData)
}
log_info("Distance decay matrix triming complete")


#Specify spatial impedance structure
if (!file.exists(file.path(data_dir, "Impede_mat"))){
  importr(D_mat) 
  Impede_mat <- vector(mode='list', length = 3)
  
  ### inverse square function
  Impede_mat[[1]] <- D_mat
  for (i in 1:length(D_mat)){
    Impede_mat[[1]][[i]] <- ((1/(D_mat[[i]])^2))
  }
  ### exponential decay function
  Impede_mat[[2]] <- D_mat
  for (i in 1:length(D_mat)){
    Impede_mat[[2]][[i]] <- (exp(-(D_mat[[i]]/10000)) )
  }
  ### hyperbolic secant function
  Impede_mat[[3]] <- D_mat
  for (i in 1:length(D_mat)){
    Impede_mat[[3]][[i]] <-  ((2/(exp(-(D_mat[[i]]/1000000)) + exp(D_mat[[i]]/1000000))))
  }
  ### hyperbolic secant function 2
  Impede_mat[[4]] <- D_mat
  for (i in 1:length(D_mat)){
    Impede_mat[[4]][[i]] <-  ((2/(exp(-(D_mat[[i]]/10000000)) + exp(D_mat[[i]]/10000000))))
  }
  saver(Impede_mat)
  rm(D_mat, Impede_mat)
}
log_info("Spatial impedance structure complete")


# Remove clutter
rm(data_dir, industry_levels, i, j, l) %>% suppressWarnings()


# Display end time
log_info("Define data generation end")

##### prox test
#test <- Sim_mat_imp_rel*Prox_mat
#test <- replace(test, test<=0, Inf)




