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
if (!file.exists(file.path(data_dir, "Total_mat"))){
 ### Sector level 15-by-15 
  Total_mat <- file.path(data_dir, "IO_tables") %>% readRDS() %>% .[["IxI_TR_1997-2020_PRO_SEC"]] %>% .[["2020"]] %>% .[6:20,3:17] %>% unlist() %>% as.numeric() %>% matrix(ncol = 15)
  rownames(Total_mat) = colnames(Total_mat) <- file.path(data_dir, "IO_tables") %>% readRDS() %>% .[["IxI_TR_1997-2020_PRO_SEC"]] %>% .[["2020"]] %>%  .[5,3:17] %>% as.list()
  Total_mat <- list(Total_mat)
  
 ### Summary level 66-by-66 Total Requirements matrix
  Total_mat[[2]] <- file.path(data_dir, "IO_tables") %>% readRDS() %>% .[["IxI_TR_1997-2020_PRO_SUM"]] %>% .[["2020"]] %>% .[6:71,3:68] %>% unlist() %>% as.numeric() %>% matrix(ncol = 66)
  rownames(Total_mat[[2]]) = colnames(Total_mat[[2]]) <- file.path(data_dir, "IO_tables") %>% readRDS() %>% .[["IxI_TR_1997-2020_PRO_SUM"]] %>% .[["2020"]] %>%  .[5,3:68] %>% as.list()
  ## Collapse three industries at summary level for functional matching of county specific summary industries 62-by-62
  Total_mat[[2]][,51] <- Total_mat[[2]][,51] + Total_mat[[2]][,52] + Total_mat[[2]][,53]
  Total_mat[[2]] <- Total_mat[[2]][,-53]
  Total_mat[[2]] <- Total_mat[[2]][,-52]
  Total_mat[[2]][,48] <- Total_mat[[2]][,48] + Total_mat[[2]][,49]
  Total_mat[[2]] <- Total_mat[[2]][,-49]
  Total_mat[[2]][,15] <- Total_mat[[2]][,15] + Total_mat[[2]][,16]
  Total_mat[[2]] <- Total_mat[[2]][,-16]
  
  Total_mat[[2]][51,] <- Total_mat[[2]][51,] + Total_mat[[2]][52,] + Total_mat[[2]][53,]
  Total_mat[[2]] <- Total_mat[[2]][-53,]
  Total_mat[[2]] <- Total_mat[[2]][-52,]
  Total_mat[[2]][48,] <- Total_mat[[2]][48,] + Total_mat[[2]][49,]
  Total_mat[[2]] <- Total_mat[[2]][-49,]
  Total_mat[[2]][15,] <- Total_mat[[2]][15,] + Total_mat[[2]][16,]
  Total_mat[[2]] <- Total_mat[[2]][-16,]
  
 ### Detail level 405-by-405 Total Requirements matrix 
    Total_mat[[3]] <- file.path(data_dir, "IO_tables") %>% readRDS() %>% .[["IxI_TR_2007_2012_PRO_DET"]] %>% .[["2012"]] %>% .[4:408,3:407] %>% unlist() %>% as.numeric() %>% matrix(ncol = 405)
    rownames(Total_mat[[3]]) = colnames(Total_mat[[3]]) <- file.path(data_dir, "IO_tables") %>% readRDS() %>% .[["IxI_TR_2007_2012_PRO_DET"]] %>% .[["2012"]] %>%  .[3,3:407] %>% as.list()
    
    #To drop list
    Detail_drop_list <- c("1111A0", "1111B0", "111200", "111300", "111400", "111900",
                          "1121A0", "112120", "112A00", "112300", "33391A", "335221",
                          "335222", "335224", "335228", "322110", "4200ID", "482000",
                          "517110", "517210", "814000", "S00500", "S00600", "491000",
                          "S00101", "S00102", "GSLGE", "GSLGH", "GSLGO", "S00201",
                          "S00202", "S00203")
    
    Total_mat[[3]] <- Total_mat[[3]][! rownames(Total_mat[[3]]) %in% Detail_drop_list, ]
    Total_mat[[3]] <- Total_mat[[3]][ , ! colnames(Total_mat[[3]]) %in% Detail_drop_list]
    
    ## Collapse  industries at detail level for functional matching of county specific detail industries 360-by-360
    Total_mat[[3]][,304] <- Total_mat[[3]][,304] + Total_mat[[3]][,305] + Total_mat[[3]][,306]
    Total_mat[[3]] <- Total_mat[[3]][,-306]
    Total_mat[[3]] <- Total_mat[[3]][,-305]
    Total_mat[[3]][,15] <- Total_mat[[3]][,15] + Total_mat[[3]][,16] + Total_mat[[3]][,17] + Total_mat[[3]][,18] + Total_mat[[3]][,19] + Total_mat[[3]][,20] + Total_mat[[3]][,21] + Total_mat[[3]][,22] + Total_mat[[3]][,23]+ Total_mat[[3]][,24] + Total_mat[[3]][,25] + Total_mat[[3]][,26]
    Total_mat[[3]] <- Total_mat[[3]][,-26]
    Total_mat[[3]] <- Total_mat[[3]][,-25]
    Total_mat[[3]] <- Total_mat[[3]][,-24]
    Total_mat[[3]] <- Total_mat[[3]][,-23]
    Total_mat[[3]] <- Total_mat[[3]][,-22]
    Total_mat[[3]] <- Total_mat[[3]][,-21]
    Total_mat[[3]] <- Total_mat[[3]][,-20]
    Total_mat[[3]] <- Total_mat[[3]][,-19]
    Total_mat[[3]] <- Total_mat[[3]][,-18]
    Total_mat[[3]] <- Total_mat[[3]][,-17]
    Total_mat[[3]] <- Total_mat[[3]][,-16]
    
    Total_mat[[3]][304,] <- Total_mat[[3]][304,] + Total_mat[[3]][305,] + Total_mat[[3]][306,]
    Total_mat[[3]] <- Total_mat[[3]][-306,]
    Total_mat[[3]] <- Total_mat[[3]][-305,]
    Total_mat[[3]][15,] <- Total_mat[[3]][15,] + Total_mat[[3]][16,] + Total_mat[[3]][17,] + Total_mat[[3]][18,] + Total_mat[[3]][19,] + Total_mat[[3]][20,] + Total_mat[[3]][21,] + Total_mat[[3]][22,] + Total_mat[[3]][23,]+ Total_mat[[3]][24,] + Total_mat[[3]][25,] + Total_mat[[3]][26,]
    Total_mat[[3]] <- Total_mat[[3]][-26,]
    Total_mat[[3]] <- Total_mat[[3]][-25,]
    Total_mat[[3]] <- Total_mat[[3]][-24,]
    Total_mat[[3]] <- Total_mat[[3]][-23,]
    Total_mat[[3]] <- Total_mat[[3]][-22,]
    Total_mat[[3]] <- Total_mat[[3]][-21,]
    Total_mat[[3]] <- Total_mat[[3]][-20,]
    Total_mat[[3]] <- Total_mat[[3]][-19,]
    Total_mat[[3]] <- Total_mat[[3]][-18,]
    Total_mat[[3]] <- Total_mat[[3]][-17,]
    Total_mat[[3]] <- Total_mat[[3]][-16,]
    
    #Rename collapsed industry categories
    rownames(Total_mat[[3]])[15] <- "23XX"
    rownames(Total_mat[[3]])[293] <- "531XX"
    colnames(Total_mat[[3]])[15] <- "23XX"
    colnames(Total_mat[[3]])[293] <- "531XX"
    
    names(Total_mat) <- industry_levels
  saver(Total_mat)
  rm(Total_mat)
}
log_info("Total matrix complete")
  
#Extract county level industry employment data and reshape to industry-by-county matrix
if (!file.exists(file.path(data_dir, "Xemp_mat"))){
 ### Sector level
    Xemp_mat <- file.path(data_dir, "CBP_2019_Sector_XBEA") %>% readRDS() %>% .[, c("BEA_Sectors", "place", "emp")]
    Xemp_mat %<>% reshape(idvar = "BEA_Sectors", v.names = "emp", varying = unique(Xemp_mat$place), timevar = "place", new.row.names = unique(Xemp_mat$BEA_Sectors),  direction = "wide")
    Xemp_mat %<>% subset(select = -c(BEA_Sectors)) %>% as.matrix()
    Xemp_mat <- list(Xemp_mat)
 ### Summary level   
    Xemp_mat[[2]] <- file.path(data_dir, "QCEW_2020_Sum_XBEA") %>% readRDS() %>% .[, c("BEA_Summary", "place", "annual_avg_emplvl")]
    Xemp_mat[[2]] %<>% reshape(idvar = "BEA_Summary", v.names = "annual_avg_emplvl", varying = unique(Xemp_mat[[2]]$place), timevar = "place", new.row.names = unique(Xemp_mat[[2]]$BEA_Summary),  direction = "wide")
    Xemp_mat[[2]] %<>% subset(select = -c(BEA_Summary)) %>% as.matrix()
 ### Detail level  
    Xemp_mat[[3]] <- file.path(data_dir, "CBP_2019_Detail_XBEA") %>% readRDS() %>% .[, c("BEA_Details", "place", "emp")]
    Xemp_mat[[3]] %<>% reshape(idvar = "BEA_Details", v.names = "emp", varying = unique(Xemp_mat[[3]]$place), timevar = "place", new.row.names = unique(Xemp_mat[[3]]$BEA_Details),  direction = "wide")
    Xemp_mat[[3]] %<>% subset(select = -c(BEA_Details)) %>% as.matrix()
    
    names(Xemp_mat) <- industry_levels
  saver(Xemp_mat)
  rm(Xemp_mat)
}  
log_info("Employment matrix complete")
  
#Extract county level industry payroll data and reshape to industry-by-county matrix
if (!file.exists(file.path(data_dir, "Xpay_mat"))){
  ### Sector level    
    Xpay_mat <- file.path(data_dir, "CBP_2019_Sector_XBEA") %>% readRDS() %>% .[, c("BEA_Sectors", "place", "ap")]
    Xpay_mat %<>% reshape(idvar = "BEA_Sectors", v.names = "ap", varying = unique(Xpay_mat$place), timevar = "place", new.row.names = unique(Xpay_mat$BEA_Sectors),  direction = "wide")
    Xpay_mat %<>% subset(select = -c(BEA_Sectors)) %>% as.matrix()
    Xpay_mat <- list(Xpay_mat)
  ### Summary level   
    Xpay_mat[[2]] <- file.path(data_dir, "QCEW_2020_Sum_XBEA") %>% readRDS() %>% .[, c("BEA_Summary", "place", "total_annual_wages")]
    Xpay_mat[[2]] %<>% reshape(idvar = "BEA_Summary", v.names = "total_annual_wages", varying = unique(Xpay_mat[[2]]$place), timevar = "place", new.row.names = unique(Xpay_mat[[2]]$BEA_Summary),  direction = "wide")
    Xpay_mat[[2]] %<>% subset(select = -c(BEA_Summary)) %>% as.matrix()
  ### Detail level     
    Xpay_mat[[3]] <- file.path(data_dir, "CBP_2019_Detail_XBEA") %>% readRDS() %>% .[, c("BEA_Details", "place", "ap")]
    Xpay_mat[[3]] %<>% reshape(idvar = "BEA_Details", v.names = "ap", varying = unique(Xpay_mat[[3]]$place), timevar = "place", new.row.names = unique(Xpay_mat[[3]]$BEA_Details),  direction = "wide")
    Xpay_mat[[3]] %<>% subset(select = -c(BEA_Details)) %>% as.matrix()
    
    names(Xpay_mat) <- industry_levels
  saver(Xpay_mat)
  rm(Xpay_mat)
}  
log_info("Payroll matrix complete")

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
if (!file.exists(file.path(data_dir, "Q_mat"))){
  
  Total_mat <- readRDS(file.path(data_dir, "Total_mat"))
  CBP_2019_Sector_XBEA <- readRDS(file.path(data_dir, "CBP_2019_Sector_XBEA"))
  QCEW_2020_Sum_XBEA <- readRDS(file.path(data_dir, "QCEW_2020_Sum_XBEA"))
  CBP_2019_Detail_XBEA <- readRDS(file.path(data_dir, "CBP_2019_Detail_XBEA"))
  QI_mat <- readRDS(file.path(data_dir, "QI_mat"))
  TIGERData <- readRDS(file.path(data_dir, "TIGERData"))
  RUCCData <- readRDS(file.path(data_dir, "RUCCData"))
  
    Q_mat <- vector(mode='list', length=length(Total_mat))
    names(Q_mat) <- industry_levels
    
    Q_mat[[1]] <- QI_mat
    Q_mat[[1]] <- Q_mat[[1]][rownames(Q_mat[[1]]) %in% intersect(unique(CBP_2019_Sector_XBEA$place), intersect(unique(TIGERData$place), unique(RUCCData$place) ) ), ]
    Q_mat[[1]] <- Q_mat[[1]][ , colnames(Q_mat[[1]]) %in% intersect(unique(CBP_2019_Sector_XBEA$place), intersect(unique(TIGERData$place), unique(RUCCData$place) ) ) ]
    
    Q_mat[[2]] <- QI_mat
    Q_mat[[2]] <- Q_mat[[2]][rownames(Q_mat[[2]]) %in% intersect(unique(QCEW_2020_Sum_XBEA$place), intersect(unique(TIGERData$place), unique(RUCCData$place) ) ), ]
    Q_mat[[2]] <- Q_mat[[2]][ , colnames(Q_mat[[2]]) %in% intersect(unique(QCEW_2020_Sum_XBEA$place), intersect(unique(TIGERData$place), unique(RUCCData$place) ) ) ]
    
    Q_mat[[3]] <- QI_mat
    Q_mat[[3]] <- Q_mat[[3]][rownames(Q_mat[[3]]) %in% intersect(unique(CBP_2019_Detail_XBEA$place), intersect(unique(TIGERData$place), unique(RUCCData$place) ) ), ]
    Q_mat[[3]] <- Q_mat[[3]][ , colnames(Q_mat[[3]]) %in% intersect(unique(CBP_2019_Detail_XBEA$place), intersect(unique(TIGERData$place), unique(RUCCData$place) ) ) ]
    
  saver(Q_mat)
  rm(Q_mat, Total_mat, CBP_2019_Sector_XBEA, QCEW_2020_Sum_XBEA, CBP_2019_Detail_XBEA, QI_mat, TIGERData, RUCCData)
}
log_info("Distance decay matrix triming complete")


############ Input Needs
if (!file.exists(file.path(data_dir, "Input_mat"))){
  
  Total_mat <- readRDS(file.path(data_dir, "Total_mat"))
  Direct_mat <- readRDS(file.path(data_dir, "Direct_mat"))
  Xpay_mat <- readRDS(file.path(data_dir, "Xpay_mat"))
  
  Input_mat <- vector(mode='list', length=length(Total_mat))
  names(Input_mat) <- industry_levels
  
    for (l in 1:length(Input_mat)){
      Input_mat[[l]] <- (Direct_mat[[l]]  %*%  Xpay_mat[[l]])
    }

  saver(Input_mat)
  rm(Input_mat, Total_mat, Direct_mat, Xpay_mat)
}
log_info("Input Needs matrix complete")

############ Relative Input Needs
if (!file.exists(file.path(data_dir, "Input_mat_rel"))){
  
  Total_mat <- readRDS(file.path(data_dir, "Total_mat"))
  Direct_mat <- readRDS(file.path(data_dir, "Direct_mat"))
  Input_mat <- readRDS(file.path(data_dir, "Input_mat"))
  
  Input_mat_rel <- vector(mode='list', length=length(Total_mat))
  names(Input_mat_rel) <- industry_levels
  
    for (l in 1:length(Input_mat_rel)){
      Input_mat_rel[[l]] <- (Input_mat[[l]]) %*% (diag(c(1/(rep(c(1), each=ncol(Direct_mat[[l]])) %*% Input_mat[[l]]))))
      colnames(Input_mat_rel[[l]]) <- colnames(Input_mat[[l]])
    }

  saver(Input_mat_rel)
  rm(Input_mat_rel, Total_mat, Direct_mat, Input_mat)
}
log_info("Relative Input Needs matrix complete")

############ Import Input Needs
if (!file.exists(file.path(data_dir, "Input_mat_imp"))){
  
  Total_mat <- readRDS(file.path(data_dir, "Total_mat"))
  Xpay_mat <- readRDS(file.path(data_dir, "Xpay_mat"))
  Input_mat <- readRDS(file.path(data_dir, "Input_mat"))
  
  Input_mat_imp <- vector(mode='list', length=length(Total_mat))
  names(Input_mat_imp) <- industry_levels
  
    for (l in 1:length(Input_mat_imp)){
      Input_mat_imp[[l]] <- pmax(Input_mat[[l]] - Xpay_mat[[l]], 0)
    }

  saver(Input_mat_imp)
  rm(Input_mat_imp, Total_mat, Xpay_mat, Input_mat)
}
log_info("Import Input Needs matrix complete")

############ Net Exports 
if (!file.exists(file.path(data_dir, "Input_mat_exp"))){
  
  Total_mat <- readRDS(file.path(data_dir, "Total_mat"))
  Xpay_mat <- readRDS(file.path(data_dir, "Xpay_mat"))
  Input_mat <- readRDS(file.path(data_dir, "Input_mat"))

  Input_mat_exp <- vector(mode='list', length=length(Total_mat))
  names(Input_mat_exp) <- industry_levels
  
    for (l in 1:length(Input_mat_exp)){
      Input_mat_exp[[l]] <- abs(pmin(Input_mat[[l]] - Xpay_mat[[l]], 0))
    }

  saver(Input_mat_exp)
  rm(Input_mat_exp, Total_mat, Xpay_mat, Input_mat)
}
log_info("Net Exports matrix complete")

############ Relative Import Input Needs
if (!file.exists(file.path(data_dir, "Input_mat_imp_rel"))){
  
  Total_mat <- readRDS(file.path(data_dir, "Total_mat"))
  Xpay_mat <- readRDS(file.path(data_dir, "Xpay_mat"))
  Input_mat <- readRDS(file.path(data_dir, "Input_mat"))
  Input_mat_rel <- readRDS(file.path(data_dir, "Input_mat_rel"))
  
  Input_mat_imp_rel <- vector(mode='list', length=length(Total_mat))
  names(Input_mat_imp_rel) <- industry_levels
  
    for (l in 1:length(Input_mat_imp_rel)){
      Input_mat_imp_rel[[l]]  <- pmax(Input_mat_rel[[l]]  - Xpay_mat[[l]]  %*% (diag(c(1/(rep(c(1), each=nrow(Xpay_mat[[l]] )) %*% Xpay_mat[[l]] )))), 0)
      colnames(Input_mat_imp_rel[[l]]) <- colnames(Input_mat[[l]])
    }

saver(Input_mat_imp_rel)
rm(Input_mat_imp_rel, Total_mat, Xpay_mat, Input_mat, Input_mat_rel)
}
log_info("Relative Import Input Needs matrix complete")


############ Similarity Index

if (!file.exists(file.path(data_dir, "Sim_mat"))){

  Total_mat <- readRDS(file.path(data_dir, "Total_mat"))
  Xpay_mat <- readRDS(file.path(data_dir, "Xpay_mat"))
  Input_mat  <- readRDS(file.path(data_dir, "Input_mat"))
  
  Sim_mat <- vector(mode='list', length=length(Total_mat))
  names(Sim_mat) <- industry_levels
  
    for (l in 1:length(Sim_mat)){
      Sim_mat[[l]] <-  matrix(0, nrow = ncol(Xpay_mat[[l]]), ncol = ncol(Xpay_mat[[l]]))
      rownames(Sim_mat[[l]]) = colnames(Sim_mat[[l]]) <- colnames(Input_mat[[l]])
    }
  
  for (l in 1:length(Sim_mat)){
    for (i in 1:ncol(Xpay_mat[[l]])){
      for (j in 1:ncol(Xpay_mat[[l]])){
        Sim_mat[[l]][i,j]  <- norm((Input_mat[[l]][,i] - (Xpay_mat[[l]][,j])), type = "2")
      }
    }    
  }


  saver(Sim_mat)
  rm(Sim_mat, Total_mat, Xpay_mat, Input_mat)
}
log_info("Similarity Index matrix complete")


############ Relative Similarity Index
if (!file.exists(file.path(data_dir, "Sim_mat_rel"))){
  
  Total_mat <- readRDS(file.path(data_dir, "Total_mat"))
  Xpay_mat <- readRDS(file.path(data_dir, "Xpay_mat"))
  Input_mat  <- readRDS(file.path(data_dir, "Input_mat"))
  Input_mat_rel <- readRDS(file.path(data_dir, "Input_mat_rel"))
  
  Sim_mat_rel <- vector(mode='list', length=length(Total_mat))
  names(Sim_mat_rel) <- industry_levels 
  
    for (l in 1:length(Sim_mat_rel)){
      Sim_mat_rel[[l]] <-  matrix(0, nrow = ncol(Xpay_mat[[l]]), ncol = ncol(Xpay_mat[[l]]))
      rownames(Sim_mat_rel[[l]]) = colnames(Sim_mat_rel[[l]]) <- colnames(Input_mat[[l]])
    }
  
   for (l in 1:length(Sim_mat_rel)){
      for (i in 1:ncol(Xpay_mat[[l]])){
        for (j in 1:ncol(Xpay_mat[[l]])){
          Sim_mat_rel[[l]][i,j]  <- norm((Input_mat_rel[[l]][,i] - (Xpay_mat[[l]][,j] * (c(1/(rep(c(1), each=ncol(Total_mat[[l]])) %*% Xpay_mat[[l]][,j] ))) )), type = "2")
        }
      }
    }
  
  saver(Sim_mat_rel)
  rm(Sim_mat_rel, Total_mat, Xpay_mat, Input_mat, Input_mat_rel)
}
log_info("Relative Similarity Index matrix complete")

############ Import Similarity Index
if (!file.exists(file.path(data_dir, "Sim_mat_imp"))){
  
  Total_mat <- readRDS(file.path(data_dir, "Total_mat"))
  Xpay_mat <- readRDS(file.path(data_dir, "Xpay_mat"))
  Input_mat  <- readRDS(file.path(data_dir, "Input_mat"))
  Input_mat_imp <- readRDS(file.path(data_dir, "Input_mat_imp"))
  
  Sim_mat_imp <- vector(mode='list', length=length(Total_mat))
  names(Sim_mat_imp) <- industry_levels
  
    for (l in 1:length(Sim_mat_imp)){
      Sim_mat_imp[[l]] <- matrix(0, nrow = ncol(Xpay_mat[[l]]), ncol = ncol(Xpay_mat[[l]]))
      rownames(Sim_mat_imp[[l]]) = colnames(Sim_mat_imp[[l]]) <- colnames(Input_mat[[l]])
    }
  
   for (l in 1:length(Sim_mat_imp)){
      for (i in 1:ncol(Xpay_mat[[l]])){
        for (j in 1:ncol(Xpay_mat[[l]])){
          Sim_mat_imp[[l]][i,j]  <- norm((Input_mat_imp[[l]][,i] - (Xpay_mat[[l]][,j])), type = "2")
        }
      }
    } 

  saver(Sim_mat_imp)
  rm(Sim_mat_imp, Total_mat, Xpay_mat, Input_mat, Input_mat_imp)
}
log_info("Import Similarity Index matrix complete")


############ Import Similarity Index - Net Exports
if (!file.exists(file.path(data_dir, "Sim_mat_exp"))){
  
  Total_mat <- readRDS(file.path(data_dir, "Total_mat"))
  Xpay_mat <- readRDS(file.path(data_dir, "Xpay_mat"))
  Input_mat  <- readRDS(file.path(data_dir, "Input_mat"))
  Input_mat_imp <- readRDS(file.path(data_dir, "Input_mat_imp"))
  Input_mat_exp <- readRDS(file.path(data_dir, "Input_mat_exp"))
  
  Sim_mat_exp <- vector(mode='list', length=length(Total_mat))
  names(Sim_mat_exp) <- industry_levels
  
    for (l in 1:length(Sim_mat_exp)){
      Sim_mat_exp[[l]] <-  matrix(0, nrow = ncol(Xpay_mat[[l]]), ncol = ncol(Xpay_mat[[l]]))
      rownames(Sim_mat_exp[[l]]) = colnames(Sim_mat_exp[[l]]) <- colnames(Input_mat[[l]])
    }
  
   for (l in 1:length(Sim_mat_exp)){
      for (i in 1:ncol(Xpay_mat[[l]])){
        for (j in 1:ncol(Xpay_mat[[l]])){
          Sim_mat_exp[[l]][i,j]  <- norm((Input_mat_imp[[l]][,i] - (Input_mat_exp[[l]][,j])), type = "2")
        }
      }
    }

  saver(Sim_mat_exp)
  rm(Sim_mat_exp, Total_mat, Xpay_mat, Input_mat, Input_mat_imp, Input_mat_exp)
}
log_info("Import Similarity Index - Net Exports matrix complete")


############ Relative Import Similarity Index
if (!file.exists(file.path(data_dir, "Sim_mat_imp_rel"))){
  
  Total_mat <- readRDS(file.path(data_dir, "Total_mat"))
  Xpay_mat <- readRDS(file.path(data_dir, "Xpay_mat"))
  Input_mat  <- readRDS(file.path(data_dir, "Input_mat"))
  Input_mat_imp_rel  <- readRDS(file.path(data_dir, "Input_mat_imp_rel"))
  
  Sim_mat_imp_rel <- vector(mode='list', length=length(Total_mat))
  names(Sim_mat_imp_rel) <- industry_levels
  
    for (l in 1:length(Sim_mat_imp_rel)){
      Sim_mat_imp_rel[[l]] <-  matrix(0, nrow = ncol(Xpay_mat[[l]]), ncol = ncol(Xpay_mat[[l]]))
      rownames(Sim_mat_imp_rel[[l]]) = colnames(Sim_mat_imp_rel[[l]]) <- colnames(Input_mat[[l]])
    }
  
    for (l in 1:length(Sim_mat_imp_rel)){     
      for (i in 1:ncol(Xpay_mat[[l]])){
        for (j in 1:ncol(Xpay_mat[[l]])){
          Sim_mat_imp_rel[[l]][i,j]  <- norm((Input_mat_imp_rel[[l]][,i] - (Xpay_mat[[l]][,j] * (c(1/(rep(c(1), each=ncol(Total_mat[[l]])) %*% Xpay_mat[[l]][,j] ))) )), type = "2")
        }
      }
    }

  saver(Sim_mat_imp_rel)
  rm(Sim_mat_imp_rel, Total_mat, Xpay_mat, Input_mat, Input_mat_imp_rel)
}
log_info("Relative Import Similarity Index matrix complete")


############ Relative Import Similarity Index - Net Exports
if (!file.exists(file.path(data_dir, "Sim_mat_exp_rel"))){
  
  Total_mat <- readRDS(file.path(data_dir, "Total_mat"))
  Xpay_mat <- readRDS(file.path(data_dir, "Xpay_mat"))
  Input_mat  <- readRDS(file.path(data_dir, "Input_mat"))
  Input_mat_imp_rel  <- readRDS(file.path(data_dir, "Input_mat_imp_rel"))
  Input_mat_exp <- readRDS(file.path(data_dir, "Input_mat_exp"))
  
  Sim_mat_exp_rel <- vector(mode='list', length=length(Total_mat))
  names(Sim_mat_exp_rel) <- industry_levels
  
    for (l in 1:length(Sim_mat_exp_rel)){
      Sim_mat_exp_rel[[l]] <-  matrix(0, nrow = ncol(Xpay_mat[[l]]), ncol = ncol(Xpay_mat[[l]]))
      rownames(Sim_mat_exp_rel[[l]]) = colnames(Sim_mat_exp_rel[[l]]) <- colnames(Input_mat[[l]])
    }
   for (l in 1:length(Sim_mat_exp_rel)){   
      for (i in 1:ncol(Xpay_mat[[l]])){
        for (j in 1:ncol(Xpay_mat[[l]])){
          Sim_mat_exp_rel[[l]][i,j]  <- norm((Input_mat_imp_rel[[l]][,i] - (Input_mat_exp[[l]][,j] * (c(1/(rep(c(1), each=ncol(Total_mat[[l]])) %*% Input_mat_exp[[l]][,j] ))) )), type = "2")
        }
      }
    }

  saver(Sim_mat_exp_rel)
  rm(Sim_mat_exp_rel, Total_mat, Xpay_mat, Input_mat, Input_mat_imp_rel, Input_mat_exp)
}
log_info("Relative Import Similarity Index - Net Exports matrix complete")


# Remove clutter
rm(data_dir, industry_levels, i, j, l)


# Display end time
log_info("Define data generation end")

##### prox test
#test <- Sim_mat_imp_rel*Prox_mat
#test <- replace(test, test<=0, Inf)




