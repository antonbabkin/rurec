
# Establish working directory relative to location of this file
script_path() %>% setwd() 

source("Data.R")

options(scipen = 999)


### Sector level 15-by-15
Total_mat <- IO_tables[["IxI_TR_1997-2020_PRO_SEC"]][["2020"]][6:20,3:17] %>% unlist() %>% as.numeric() %>% matrix(ncol = 15)
rownames(Total_mat) = colnames(Total_mat) <- IO_tables[["IxI_TR_1997-2020_PRO_SEC"]][["2020"]][5,3:17] %>% as.list()
Total_mat <- list(Total_mat)

local({
  temp <- TIGER_CBP_RUCC[, c("place")]
  st_geometry(temp) <- NULL
  X_mat <<- inner_join(temp, CBP_2019_Sector_XBEA, by = "place")
})

Xemp_mat <- X_mat[, c("BEA_Sectors", "place", "emp")]
Xemp_mat %<>% reshape(idvar = "BEA_Sectors", v.names = "emp", varying = unique(Xemp_mat$place), timevar = "place", new.row.names = unique(Xemp_mat$BEA_Sectors),  direction = "wide")
Xemp_mat %<>% subset(select = -c(BEA_Sectors)) %>% as.matrix()

Xemp_mat <- list(Xemp_mat)


Xpay_mat <- X_mat[, c("BEA_Sectors", "place", "ap")]
Xpay_mat %<>% reshape(idvar = "BEA_Sectors", v.names = "ap", varying = unique(Xpay_mat$place), timevar = "place", new.row.names = unique(Xpay_mat$BEA_Sectors),  direction = "wide")
Xpay_mat %<>% subset(select = -c(BEA_Sectors)) %>% as.matrix()

Xpay_mat <- list(Xpay_mat)


### Summary level 66-by-66
Total_mat[[2]] <- IO_tables[["IxI_TR_1997-2020_PRO_SUM"]][["2020"]][6:71,3:68] %>% unlist() %>% as.numeric() %>% matrix(ncol = 66)
rownames(Total_mat[[2]]) = colnames(Total_mat[[2]]) <- IO_tables[["IxI_TR_1997-2020_PRO_SUM"]][["2020"]][5,3:68] %>% as.list()


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





local({
  temp <- TIGER_QCEW_RUCC[, c("place")]
  st_geometry(temp) <- NULL 
  X_mat <<- inner_join(temp, QCEW_2020_Sum_XBEA, by = "place")
})

Xemp_mat[[2]] <- X_mat[, c("BEA_Summary", "place", "annual_avg_emplvl")]
Xemp_mat[[2]] %<>% reshape(idvar = "BEA_Summary", v.names = "annual_avg_emplvl", varying = unique(Xemp_mat[[2]]$place), timevar = "place", new.row.names = unique(Xemp_mat[[2]]$BEA_Summary),  direction = "wide")
Xemp_mat[[2]] %<>% subset(select = -c(BEA_Summary)) %>% as.matrix()


Xpay_mat[[2]] <- X_mat[, c("BEA_Summary", "place", "total_annual_wages")]
Xpay_mat[[2]] %<>% reshape(idvar = "BEA_Summary", v.names = "total_annual_wages", varying = unique(Xpay_mat[[2]]$place), timevar = "place", new.row.names = unique(Xpay_mat[[2]]$BEA_Summary),  direction = "wide")
Xpay_mat[[2]] %<>% subset(select = -c(BEA_Summary)) %>% as.matrix()


### Detail level 405-by-405
# Total_mat <- IO_tables[["IxI_TR_2007_2012_PRO_DET"]][["2012"]][4:408,3:407] %>% unlist() %>% as.numeric() %>% matrix(ncol = 405)
# Ind_Names <- IO_tables[["IxI_TR_2007_2012_PRO_DET"]][["2012"]][2,3:407] %>% as.list()





#### Direct requirements matrices (Technical Coefficients) 
Direct_mat <- Total_mat
for (l in 1:length(Total_mat)){
Direct_mat[[l]] <- diag(ncol(Total_mat[[l]])) - solve(Total_mat[[l]])
}






############ Simple Location Quotient

# Industry_Count <- dim(Xemp_mat[[1]])[1]
# Region_Count <- dim(Xemp_mat[[1]])[2]

LQ <-  matrix(0, nrow=Industry_Count, ncol = Region_Count)

for (i in 1:Industry_Count){
  for (j in 1:Region_Count){
    LQ[i,j] <-((Xemp_mat[i,j])/sum(Xemp_mat[,j]))/(sum(Xemp_mat[i,])/sum(Xemp_mat))
  }
}
rownames(LQ) <- rownames(Xemp_mat)
colnames(LQ) <- colnames(Xemp_mat)


LQc <-  matrix(0, nrow = Industry_Count, ncol = Region_Count)
for (i in 1:Industry_Count){
  for (j in 1:Region_Count){
    if (LQ[i,j] > 1){
      LQc[i,j] <- 1
    }
    else{
      LQc[i,j] <- LQ[i,j]
    }
  }
}


Dir_LQ <-  c()
for (i in 1:ncol(LQc)){
  Dir_LQ[[i]] <- (diag(LQc[,i]) %*% Direct_mat) %>% round(4)
  rownames(Dir_LQ[[i]]) <-  rownames(Xemp_mat)
}
names(Dir_LQ) <- colnames(Xemp_mat)


############ Cross-Industry Location Quotient

CLQ <-  c()
for (i in 1:Region_Count){
  CLQ[[i]] <- (LQ[,i] %*% t(1/LQ[,i])) %>% round(4)
  rownames(CLQ[[i]]) <- rownames(Xemp_mat)
}
names(CLQ) <- colnames(Xemp_mat)


CLQc <- CLQ
CLQc %<>% finiterer() %>% onerer()
names(CLQc) <- colnames(Xemp_mat)


Dir_CLQ <-  c()
for (i in 1:length(CLQc)){
  Dir_CLQ[[i]] <- (CLQc[[i]] * Direct_mat) %>% round(4)
}
names(Dir_CLQ) <- colnames(Xemp_mat)


############ Semilogarithmic Location Quotient

SLQ <-  c()
for (i in 1:Region_Count){
  SLQ[[i]] <- (LQ[,i] %*% t(1/(log2(1 + LQ[,i])))) %>% round(4)
  rownames(SLQ[[i]]) <- rownames(Xemp_mat)
}
names(SLQ) <- colnames(Xemp_mat)

SLQc <- SLQ
SLQc %<>% finiterer() %>% onerer()
names(SLQc) <- colnames(Xemp_mat)

Dir_SLQ <-  c()
for (i in 1:length(SLQc)){
  Dir_SLQ[[i]] <- (SLQc[[i]] * Direct_mat) %>% round(4)
}
names(Dir_SLQ) <- colnames(Xemp_mat)


############ Flegg Location Quotient
FLQ_delta <- .3
lambda <- (log2(1 + (colSums(Xemp_mat)/sum(Xemp_mat))))^(FLQ_delta)

FLQ <-  c()
for (i in 1:Region_Count){
  FLQ[[i]] <- ((LQ[,i] %*% t(1/LQ[,i])) * lambda[i]) %>% round(4)
  rownames(FLQ[[i]]) <- rownames(Xemp_mat)
}
names(FLQ) <- colnames(Xemp_mat)

FLQc <- FLQ
FLQc %<>% finiterer() %>% onerer()
names(FLQc) <- colnames(Xemp_mat)

Dir_FLQ <-  c()
for (i in 1:length(FLQc)){
  Dir_FLQ[[i]] <- (FLQc[[i]] * Direct_mat) %>% round(4)
}
names(Dir_FLQ) <- colnames(Xemp_mat)


############ Augmented Flegg Location Quotient

LQa <- LQ
for (i in 1:nrow(LQa)){
  for (j in 1:ncol(LQa)){
    if (LQ[i,j] > 1){
      LQa[i,j] <- log2(1 + LQ[i,j])
    }
    else{
      LQa[i,j] <- 1
    }
  }
}

AFLQ <- FLQ %>% finiterer()
for (r in 1:length(AFLQ)){
  AFLQ[[r]] <- (AFLQ[[r]] %*% diag(LQa[,r])) %>% round(4)
  colnames(AFLQ[[r]])  <- rownames(Xemp_mat)
}
names(AFLQ) <- colnames(Xemp_mat)

Dir_AFLQ <-  c()
for (i in 1:length(AFLQ)){
  Dir_AFLQ[[i]] <- (AFLQ[[i]] * Direct_mat) %>% round(4)
}
names(Dir_AFLQ) <- colnames(Xemp_mat)


############ Gravity
#Q_mat <- (1/(Dist_mat/1)^2)
#Q_mat <- exp(-(Dist_mat/10000))
#hyperbolic secant function
Q_mat <- 2/(exp(-(Dist_mat/1000000)) + exp(Dist_mat/1000000))
Gravity_mat <- as.list(c())
for (i in 1:nrow(Xemp_mat)){
  Gravity_mat[[i]] <- ((Xemp_mat[i,] %*% t(Xemp_mat[i,])) / sum(Xemp_mat[i,]) * Q_mat) %>% round(4)
  rownames(Gravity_mat[[i]]) <- colnames(Xemp_mat)
}
names(Gravity_mat) <- rownames(Xemp_mat)



############ Input Needs
Input_mat <- Total_mat
for (l in 1:length(Total_mat)){
  Input_mat[[l]] <- (Direct_mat[[l]]  %*%  Xpay_mat[[l]])
}

############ Relative Input Needs
Input_mat_rel <- Total_mat
for (l in 1:length(Total_mat)){
  Input_mat_rel[[l]] <- (Input_mat[[l]]) %*% (diag(c(1/(rep(c(1), each=ncol(Direct_mat[[l]])) %*% Input_mat[[l]]))))
  colnames(Input_mat_rel[[l]]) <- colnames(Input_mat[[l]])
}

############ Import Input Needs
Input_mat_imp <- Total_mat
for (l in 1:length(Total_mat)){
  Input_mat_imp[[l]] <- pmax(Input_mat[[l]] - Xpay_mat[[l]], 0)
}

############ Net Exports 
Input_mat_exp <- Total_mat
for (l in 1:length(Total_mat)){
  Input_mat_exp[[l]] <- abs(pmin(Input_mat[[l]] - Xpay_mat[[l]], 0))
}

############ Relative Import Input Needs
Input_mat_imp_rel <- Total_mat
for (l in 1:length(Total_mat)){
  Input_mat_imp_rel[[l]]  <- pmax(Input_mat_rel[[l]]  - Xpay_mat[[l]]  %*% (diag(c(1/(rep(c(1), each=nrow(Xpay_mat[[l]] )) %*% Xpay_mat[[l]] )))), 0)
  colnames(Input_mat_imp_rel[[l]] ) <- colnames(Input_mat[[l]])
}







############ Similarity Index

# Sim_mat  <-  matrix(0, nrow = ncol(Xpay_mat), ncol = ncol(Xpay_mat))
# for (i in 1:ncol(Xpay_mat)){
#   for (j in 1:ncol(Xpay_mat)){
#     Sim_mat[i,j]  <- norm((Input_mat[,i] - (Xpay_mat[,j])), type = "2")
#   }
# }
# rownames(Sim_mat) = colnames(Sim_mat) <- colnames(Input_mat)


Sim_mat  <- Total_mat
for (l in 1:length(Total_mat)){
  Sim_mat[[l]] <-  matrix(0, nrow = ncol(Xpay_mat[[l]]), ncol = ncol(Xpay_mat[[l]]))
  
  for (i in 1:ncol(Xpay_mat[[l]])){
    for (j in 1:ncol(Xpay_mat[[l]])){
      Sim_mat[[l]][i,j]  <- norm((Input_mat[[l]][,i] - (Xpay_mat[[l]][,j])), type = "2")
    }
  }
  
  rownames(Sim_mat[[l]]) = colnames(Sim_mat[[l]]) <- colnames(Input_mat[[l]])
}


############ Relative Similarity Index

# Sim_mat_rel  <-  matrix(0, nrow = ncol(Xpay_mat), ncol = ncol(Xpay_mat))
# for (i in 1:ncol(Xpay_mat)){
#   for (j in 1:ncol(Xpay_mat)){
#     Sim_mat_rel[i,j]  <- norm((Input_mat_rel[,i] - (Xpay_mat[,j] * (c(1/(rep(c(1), each=ncol(Direct_mat)) %*% Xpay_mat[,j] ))) )), type = "2")
#   }
# }
# rownames(Sim_mat_rel) = colnames(Sim_mat_rel) <- colnames(Input_mat)
# 


Sim_mat_rel  <- Total_mat
for (l in 1:length(Total_mat)){
  Sim_mat_rel[[l]] <-  matrix(0, nrow = ncol(Xpay_mat[[l]]), ncol = ncol(Xpay_mat[[l]]))
  
  for (i in 1:ncol(Xpay_mat[[l]])){
    for (j in 1:ncol(Xpay_mat[[l]])){
      Sim_mat_rel[[l]][i,j]  <- norm((Input_mat_rel[[l]][,i] - (Xpay_mat[[l]][,j] * (c(1/(rep(c(1), each=ncol(Direct_mat[[l]])) %*% Xpay_mat[[l]][,j] ))) )), type = "2")
    }
  }
  
  rownames(Sim_mat_rel[[l]]) = colnames(Sim_mat_rel[[l]]) <- colnames(Input_mat[[l]])
}



############ Import Similarity Index
# Sim_mat_imp  <-  matrix(0, nrow = ncol(Xpay_mat), ncol = ncol(Xpay_mat))
# for (i in 1:ncol(Xpay_mat)){
#   for (j in 1:ncol(Xpay_mat)){
#     Sim_mat_imp[i,j]  <- norm((Input_mat_imp[,i] - (Xpay_mat[,j])), type = "2")
#   }
# }
# rownames(Sim_mat_imp) = colnames(Sim_mat_imp) <- colnames(Input_mat)
# 


Sim_mat_imp  <- Total_mat
for (l in 1:length(Total_mat)){
  Sim_mat_imp[[l]] <-  matrix(0, nrow = ncol(Xpay_mat[[l]]), ncol = ncol(Xpay_mat[[l]]))
  
  for (i in 1:ncol(Xpay_mat[[l]])){
    for (j in 1:ncol(Xpay_mat[[l]])){
      Sim_mat_imp[[l]][i,j]  <- norm((Input_mat_imp[[l]][,i] - (Xpay_mat[[l]][,j])), type = "2")
    }
  }
  
  rownames(Sim_mat_imp[[l]]) = colnames(Sim_mat_imp[[l]]) <- colnames(Input_mat[[l]])
}





############ *Import Similarity Index - Net Exports
# Sim_mat_exp  <-  matrix(0, nrow = ncol(Xpay_mat), ncol = ncol(Xpay_mat))
# for (i in 1:ncol(Xpay_mat)){
#   for (j in 1:ncol(Xpay_mat)){
#     Sim_mat_exp[i,j]  <- norm((Input_mat_imp[,i] - (Input_mat_exp[,j])), type = "2")
#   }
# }
# rownames(Sim_mat_exp) = colnames(Sim_mat_exp) <- colnames(Input_mat)
# 



Sim_mat_exp  <- Total_mat
for (l in 1:length(Total_mat)){
  Sim_mat_exp[[l]] <-  matrix(0, nrow = ncol(Xpay_mat[[l]]), ncol = ncol(Xpay_mat[[l]]))
  
  for (i in 1:ncol(Xpay_mat[[l]])){
    for (j in 1:ncol(Xpay_mat[[l]])){
      Sim_mat_exp[[l]][i,j]  <- norm((Input_mat_imp[[l]][,i] - (Input_mat_exp[[l]][,j])), type = "2")
    }
  }
  
  rownames(Sim_mat_exp[[l]]) = colnames(Sim_mat_exp[[l]]) <- colnames(Input_mat[[l]])
}




############ Relative Import Similarity Index
# Sim_mat_imp_rel  <-  matrix(0, nrow = ncol(Xpay_mat), ncol = ncol(Xpay_mat))
# for (i in 1:ncol(Xpay_mat)){
#   for (j in 1:ncol(Xpay_mat)){
#     Sim_mat_imp_rel[i,j]  <- norm((Input_mat_imp_rel[,i] - (Xpay_mat[,j] * (c(1/(rep(c(1), each=ncol(Direct_mat)) %*% Xpay_mat[,j] ))) )), type = "2")
#   }
# }
# rownames(Sim_mat_imp_rel) = colnames(Sim_mat_imp_rel) <- colnames(Input_mat)



Sim_mat_imp_rel  <- Total_mat
for (l in 1:length(Total_mat)){
  Sim_mat_imp_rel[[l]] <-  matrix(0, nrow = ncol(Xpay_mat[[l]]), ncol = ncol(Xpay_mat[[l]]))
  
  for (i in 1:ncol(Xpay_mat[[l]])){
    for (j in 1:ncol(Xpay_mat[[l]])){
      Sim_mat_imp_rel[[l]][i,j]  <- norm((Input_mat_imp_rel[[l]][,i] - (Xpay_mat[[l]][,j] * (c(1/(rep(c(1), each=ncol(Direct_mat[[l]])) %*% Xpay_mat[[l]][,j] ))) )), type = "2")
    }
  }
  
  rownames(Sim_mat_imp_rel[[l]]) = colnames(Sim_mat_imp_rel[[l]]) <- colnames(Input_mat[[l]])
}






############ *Relative Import Similarity Index - Net Exports
# Sim_mat_exp_rel  <-  matrix(0, nrow = ncol(Xpay_mat), ncol = ncol(Xpay_mat))
# for (i in 1:ncol(Xpay_mat)){
#   for (j in 1:ncol(Xpay_mat)){
#     Sim_mat_exp_rel[i,j]  <- norm((Input_mat_imp_rel[,i] - (Input_mat_exp[,j] * (c(1/(rep(c(1), each=ncol(Direct_mat)) %*% Input_mat_exp[,j] ))) )), type = "2")
#   }
# }
# rownames(Sim_mat_exp_rel) = colnames(Sim_mat_exp_rel) <- colnames(Input_mat)


Sim_mat_exp_rel  <- Total_mat
for (l in 1:length(Total_mat)){
  Sim_mat_exp_rel[[l]] <-  matrix(0, nrow = ncol(Xpay_mat[[l]]), ncol = ncol(Xpay_mat[[l]]))
  
  for (i in 1:ncol(Xpay_mat[[l]])){
    for (j in 1:ncol(Xpay_mat[[l]])){
      Sim_mat_exp_rel[[l]][i,j]  <- norm((Input_mat_imp_rel[[l]][,i] - (Input_mat_exp[[l]][,j] * (c(1/(rep(c(1), each=ncol(Direct_mat[[l]])) %*% Input_mat_exp[[l]][,j] ))) )), type = "2")
    }
  }
  
  rownames(Sim_mat_exp_rel[[l]]) = colnames(Sim_mat_exp_rel[[l]]) <- colnames(Input_mat[[l]])
}









##### WI test
#test <- Sim_mat_imp_rel*Prox_mat
#test <- replace(test, test<=0, Inf)






####### Saved Data for use in other renderable files
save.image(file="nbs/AllData.RData")
save(list = c("TIGER_CBP_RUCC", "Xpay_mat", "Sim_mat",  "Sim_mat_rel", "Sim_mat_imp", "Sim_mat_imp_rel", "Sim_mat_exp", "Sim_mat_exp_rel", "Prox_mat", "Q_mat", "Dist_mat", "TIGER_QCEW_RUCC"), file="nbs/Data.RData")



