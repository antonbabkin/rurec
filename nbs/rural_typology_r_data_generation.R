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
  Total_mat <- list()
  
 ### Sector level 14-by-14 
  Total_mat[[1]] <- file.path(data_dir, "IO_tables") %>% readRDS() %>% .[["IxI_TR_1997-2020_PRO_SEC"]] %>% .[["2020"]] %>% .[6:20,3:17] %>% unlist() %>% as.numeric() %>% matrix(ncol = 15)
  rownames(Total_mat[[1]]) = colnames(Total_mat[[1]]) <- file.path(data_dir, "IO_tables") %>% readRDS() %>% .[["IxI_TR_1997-2020_PRO_SEC"]] %>% .[["2020"]] %>%  .[4,3:17] %>% as.list()
  match_x <- file.path(data_dir, "CBP_2019p_Concord_Sector_XBEA") %>% readRDS() %>% .$indcode %>% unique() 
  Total_mat[[1]] %<>% t() %>% subset(rownames(Total_mat[[1]]) %in% match_x) %>% t() %>% subset(rownames(Total_mat[[1]]) %in% match_x) 
  
  
 ### Summary level 59-by-59 Total Requirements matrix
  Total_mat[[2]] <- file.path(data_dir, "IO_tables") %>% readRDS() %>% .[["IxI_TR_1997-2020_PRO_SUM"]] %>% .[["2020"]] %>% .[6:76,3:73] %>% unlist() %>% as.numeric() %>% matrix(ncol = 71)
  rownames(Total_mat[[2]]) = colnames(Total_mat[[2]]) <- file.path(data_dir, "IO_tables") %>% readRDS() %>% .[["IxI_TR_1997-2020_PRO_SUM"]] %>% .[["2020"]] %>% .[4,3:73] %>% as.list()
  match_x <- file.path(data_dir, "CBP_2019p_Concord_Summary_XBEA") %>% readRDS() %>% .$indcode %>% unique() 
  Total_mat[[2]] %<>% t() %>% subset(rownames(Total_mat[[2]]) %in% match_x) %>% t() %>% subset(rownames(Total_mat[[2]]) %in% match_x) 

  
 ### Detail level 360-by-360 Total Requirements matrix 
  Total_mat[[3]] <- file.path(data_dir, "IO_tables") %>% readRDS() %>% .[["IxI_TR_2007_2012_PRO_DET"]] %>% .[["2012"]] %>% .[4:408,3:407] %>% unlist() %>% as.numeric() %>% matrix(ncol = 405)
  rownames(Total_mat[[3]]) = colnames(Total_mat[[3]]) <- file.path(data_dir, "IO_tables") %>% readRDS() %>% .[["IxI_TR_2007_2012_PRO_DET"]] %>% .[["2012"]] %>% .[3,3:407] %>% as.list()
  #Collapse ambiguous industry 23* codes
  d  <- sum(Total_mat[[3]][25:36,25:36])/12
  Total_mat[[3]][,25] <- (Total_mat[[3]][,25] + Total_mat[[3]][,26] + Total_mat[[3]][,27] + Total_mat[[3]][,28] + Total_mat[[3]][,29] + Total_mat[[3]][,30] + Total_mat[[3]][,31] + Total_mat[[3]][,32] + Total_mat[[3]][,33]+ Total_mat[[3]][,34] + Total_mat[[3]][,35] + Total_mat[[3]][,36])/12
  for(i in 36:26){
    Total_mat[[3]] <- Total_mat[[3]][,-i]
  }
  Total_mat[[3]][25,] <- (Total_mat[[3]][25,] + Total_mat[[3]][26,] + Total_mat[[3]][27,] + Total_mat[[3]][28,] + Total_mat[[3]][29,] + Total_mat[[3]][30,] + Total_mat[[3]][31,] + Total_mat[[3]][32,] + Total_mat[[3]][33,]+ Total_mat[[3]][34,] + Total_mat[[3]][35,] + Total_mat[[3]][36,])/12
  for(i in 36:26){
    Total_mat[[3]] <- Total_mat[[3]][-i,]
  }
  Total_mat[[3]][25,25] <- d
  colnames(Total_mat[[3]])[25] = "23"
  rownames(Total_mat[[3]])[25] = "23"
  match_x <- file.path(data_dir, "CBP_2019p_Concord_Detail_XBEA") %>% readRDS() %>% .$indcode %>% unique() 
  Total_mat[[3]] %<>% t() %>% subset(rownames(Total_mat[[3]]) %in% match_x) %>% t() %>% subset(rownames(Total_mat[[3]]) %in% match_x) 

    names(Total_mat) <- industry_levels
  saver(Total_mat)
  rm(Total_mat, match_x, d)
}
log_info("Total matrix complete")
  
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

# ### Test that county industry data and I/O tables are compatible
# importr(Xpay_mat)
# importr(Total_mat)
# for(i in 1:3){
#   identical(rownames(Xpay_mat[[i]] ), rownames(Total_mat[[i]] )) %>% print()
# }

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
  saver(Impede_mat)
  rm(D_mat, Impede_mat)
}
log_info("Spatial impedance structure complete")



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



############  Queeg specification
if (!file.exists(file.path(data_dir, "Queeg"))){
  
  Total_mat <- readRDS(file.path(data_dir, "Total_mat"))
  Xpay_mat <- readRDS(file.path(data_dir, "Xpay_mat"))
  Input_mat  <- readRDS(file.path(data_dir, "Input_mat"))
  Input_mat_imp <- readRDS(file.path(data_dir, "Input_mat_imp"))
  Input_mat_exp <- readRDS(file.path(data_dir, "Input_mat_exp"))
  
  Queeg <- vector(mode='list', length=length(Total_mat))
  names(Queeg) <- industry_levels
  
  for (l in 1:length(Queeg)){
    Queeg[[l]] <-  matrix(0, nrow = ncol(Xpay_mat[[l]]), ncol = ncol(Xpay_mat[[l]]))
    rownames(Queeg[[l]]) = colnames(Queeg[[l]]) <- colnames(Input_mat[[l]])
  }
  
  for (l in 1:length(Queeg)){
    for (i in 1:ncol(Xpay_mat[[l]])){
      for (j in 1:ncol(Xpay_mat[[l]])){
        Queeg[[l]][i,j]  <- rep(c(1), each=ncol(Total_mat[[l]])) %*% pmin(Input_mat_exp[[l]][,i], Input_mat_imp[[l]][,j])
      }
    }
  }
  
  saver(Queeg)
  rm(Queeg, Total_mat, Xpay_mat, Input_mat, Input_mat_imp, Input_mat_exp)
}
log_info("Queeg specification complete")

############ Relative Queeg specification
if (!file.exists(file.path(data_dir, "Queeg_rel"))){

  Total_mat <- readRDS(file.path(data_dir, "Total_mat"))
  Xpay_mat <- readRDS(file.path(data_dir, "Xpay_mat"))
  Input_mat  <- readRDS(file.path(data_dir, "Input_mat"))
  Input_mat_imp <- readRDS(file.path(data_dir, "Input_mat_imp"))
  Input_mat_exp <- readRDS(file.path(data_dir, "Input_mat_exp"))

  Queeg_rel <- vector(mode='list', length=length(Total_mat))
  names(Queeg_rel) <- industry_levels

  for (l in 1:length(Queeg_rel)){
    Queeg_rel[[l]] <-  matrix(0, nrow = ncol(Xpay_mat[[l]]), ncol = ncol(Xpay_mat[[l]]))
    rownames(Queeg_rel[[l]]) = colnames(Queeg_rel[[l]]) <- colnames(Input_mat[[l]])
  }

  for (l in 1:length(Queeg_rel)){
    for (i in 1:ncol(Xpay_mat[[l]])){
      for (j in 1:ncol(Xpay_mat[[l]])){
        Queeg_rel[[l]][i,j]  <- (rep(c(1), each=ncol(Total_mat[[l]])) %*% pmin(Input_mat_exp[[l]][,i], Input_mat_imp[[l]][,j]) ) /  (rep(c(1), each=ncol(Total_mat[[l]])) %*% Input_mat_exp[[l]][,i] )
      }
    }
  }

  saver(Queeg_rel)
  rm(Queeg_rel, Total_mat, Xpay_mat, Input_mat, Input_mat_imp, Input_mat_exp)
}
log_info("Relative Queeg specification complete")


############ All Similarities
if (!file.exists(file.path(data_dir, "Sim_list"))){
  importr(Sim_mat)
  importr(Sim_mat_rel)
  importr(Sim_mat_imp)
  importr(Sim_mat_exp)
  importr(Sim_mat_imp_rel)
  importr(Sim_mat_exp_rel)

  Sim_list <- list(Sim_mat, Sim_mat_rel, Sim_mat_imp, Sim_mat_exp, Sim_mat_imp_rel, Sim_mat_exp_rel)
  names(Sim_list) <- c("Sim_mat", "Sim_mat_rel",  "Sim_mat_imp", "Sim_mat_exp", "Sim_mat_imp_rel", "Sim_mat_exp_rel")
  saver(Sim_list)
  rm(Sim_list, Sim_mat, Sim_mat_rel, Sim_mat_imp, Sim_mat_exp, Sim_mat_imp_rel, Sim_mat_exp_rel)
}
log_info("All Similarity Indices complete")



# Remove clutter
rm(data_dir, industry_levels, i, j, l) %>% suppressWarnings()


# Display end time
log_info("Define data generation end")

##### prox test
#test <- Sim_mat_imp_rel*Prox_mat
#test <- replace(test, test<=0, Inf)




