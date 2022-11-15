# Generate new data products


# Display start time
log_info("Define data generation start")

# Connect  and  parse  code  from  another  file 
source(file.path(find_rstudio_root_file(), "nbs", "rural_typology_r_data_clean.R"))

options(scipen = 999)

data_dir = file.path(find_rstudio_root_file(), "data", "robjs")

### Total Requirements matrix
if (file.exists(file.path(data_dir, "Total_mat"))) {
  log_info("Total requirements matrix already exists")
} else {
  local({
    importr(TR_matp)
    Total_mat <- list()
    for (i in c("Sector", "Summary", "Detail")){
      df <- TR_matp[[i]] %>% .[1:ncol(.), ] %>% as.matrix()
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
    rm(TR_matp)
  })
  log_info("Total requirements matrix complete")
}


### Labor share
if (file.exists(file.path(data_dir, "Labor_mat"))) {
  log_info("Labor matrix already exists")
} else {
  local({
    importr(SUT_matp)
    Labor_mat <- list()
    for (i in c("Sector", "Summary")){
      df <- SUT_matp[[i]] %>% as.matrix()
      Labor_mat[[i]] <- df["V001", , drop = FALSE]/df["T018", , drop = FALSE]
      rownames( Labor_mat[[i]] ) <- "labor_share"
    }

    ### Detail level
    # Collapse ambiguous industry 23* codes
    # add up all construction columns together
    df <- SUT_matp[["Detail"]] %>% as.matrix()
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
    
    ### Note: only temporary until pubdata.agcensus.md complete 
    download.file(url="https://drive.google.com/download&id=1gDufjSB3vXaloBtlYOjfo8IuSTgebpT3", 
                  destfile=file.path(find_rstudio_root_file(), "data", "nass", "agcensus_sales_by_bea_v220909.csv"))
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
rm(data_dir, i, j, l) %>% suppressWarnings()



# Display end time
log_info("Define data generation end")

##### prox test
#test <- Sim_mat_imp_rel*Prox_mat
#test <- replace(test, test<=0, Inf)




