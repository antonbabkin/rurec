root_dir <- rprojroot::find_rstudio_root_file()


# Connect  and  parse  code  from  another  file 
source(file.path(root_dir, "nbs", "rural_typology_r_data_clean.R"))

options(scipen = 999)


### Sector level 15-by-15 Total Requirements matrix
Total_mat <- IO_tables[["IxI_TR_1997-2020_PRO_SEC"]][["2020"]][6:20,3:17] %>% unlist() %>% as.numeric() %>% matrix(ncol = 15)
rownames(Total_mat) = colnames(Total_mat) <- IO_tables[["IxI_TR_1997-2020_PRO_SEC"]][["2020"]][5,3:17] %>% as.list()
Total_mat <- list(Total_mat)

#Extract county level industry employment data and reshape to industry-by-county matrix
Xemp_mat <- CBP_2019_Sector_XBEA[, c("BEA_Sectors", "place", "emp")]
Xemp_mat %<>% reshape(idvar = "BEA_Sectors", v.names = "emp", varying = unique(Xemp_mat$place), timevar = "place", new.row.names = unique(Xemp_mat$BEA_Sectors),  direction = "wide")
Xemp_mat %<>% subset(select = -c(BEA_Sectors)) %>% as.matrix()
Xemp_mat <- list(Xemp_mat)

#Extract county level industry payroll data and reshape to industry-by-county matrix
Xpay_mat <- CBP_2019_Sector_XBEA[, c("BEA_Sectors", "place", "ap")]
Xpay_mat %<>% reshape(idvar = "BEA_Sectors", v.names = "ap", varying = unique(Xpay_mat$place), timevar = "place", new.row.names = unique(Xpay_mat$BEA_Sectors),  direction = "wide")
Xpay_mat %<>% subset(select = -c(BEA_Sectors)) %>% as.matrix()
Xpay_mat <- list(Xpay_mat)


### Summary level 66-by-66 Total Requirements matrix
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

#Extract county level industry employment data and reshape to industry-by-county matrix
Xemp_mat[[2]] <- QCEW_2020_Sum_XBEA[, c("BEA_Summary", "place", "annual_avg_emplvl")]
Xemp_mat[[2]] %<>% reshape(idvar = "BEA_Summary", v.names = "annual_avg_emplvl", varying = unique(Xemp_mat[[2]]$place), timevar = "place", new.row.names = unique(Xemp_mat[[2]]$BEA_Summary),  direction = "wide")
Xemp_mat[[2]] %<>% subset(select = -c(BEA_Summary)) %>% as.matrix()

#Extract county level industry payroll data and reshape to industry-by-county matrix
Xpay_mat[[2]] <- QCEW_2020_Sum_XBEA[, c("BEA_Summary", "place", "total_annual_wages")]
Xpay_mat[[2]] %<>% reshape(idvar = "BEA_Summary", v.names = "total_annual_wages", varying = unique(Xpay_mat[[2]]$place), timevar = "place", new.row.names = unique(Xpay_mat[[2]]$BEA_Summary),  direction = "wide")
Xpay_mat[[2]] %<>% subset(select = -c(BEA_Summary)) %>% as.matrix()



### Detail level 405-by-405 Total Requirements matrix 
Total_mat[[3]] <- IO_tables[["IxI_TR_2007_2012_PRO_DET"]][["2012"]][4:408,3:407] %>% unlist() %>% as.numeric() %>% matrix(ncol = 405)
rownames(Total_mat[[3]]) = colnames(Total_mat[[3]]) <- IO_tables[["IxI_TR_2007_2012_PRO_DET"]][["2012"]][3,3:407] %>% as.list()
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

#Extract county level industry employment data and reshape to industry-by-county matrix
Xemp_mat[[3]] <- CBP_2019_Detail_XBEA[, c("BEA_Details", "place", "emp")]
Xemp_mat[[3]] %<>% reshape(idvar = "BEA_Details", v.names = "emp", varying = unique(Xemp_mat[[3]]$place), timevar = "place", new.row.names = unique(Xemp_mat[[3]]$BEA_Details),  direction = "wide")
Xemp_mat[[3]] %<>% subset(select = -c(BEA_Details)) %>% as.matrix()

#Extract county level industry payroll data and reshape to industry-by-county matrix
Xpay_mat[[3]] <- CBP_2019_Detail_XBEA[, c("BEA_Details", "place", "ap")]
Xpay_mat[[3]] %<>% reshape(idvar = "BEA_Details", v.names = "ap", varying = unique(Xpay_mat[[3]]$place), timevar = "place", new.row.names = unique(Xpay_mat[[3]]$BEA_Details),  direction = "wide")
Xpay_mat[[3]] %<>% subset(select = -c(BEA_Details)) %>% as.matrix()

industry_levels  = c("Sector", "Summary", "Detail")
names(Total_mat) <- industry_levels
names(Xemp_mat) <- industry_levels
names(Xpay_mat) <- industry_levels


#### Direct requirements matrices (Technical Coefficients) 
Direct_mat <- Total_mat
for (l in 1:length(Total_mat)){
Direct_mat[[l]] <- diag(ncol(Total_mat[[l]])) - solve(Total_mat[[l]])
}
names(Direct_mat) <- industry_levels




############ Simple Location Quotient

# Industry_Count <- dim(Xemp_mat[[1]])[1]
# Region_Count <- dim(Xemp_mat[[1]])[2]
# 
# LQ <-  matrix(0, nrow=Industry_Count, ncol = Region_Count)
# 
# for (i in 1:Industry_Count){
#   for (j in 1:Region_Count){
#     LQ[i,j] <-((Xemp_mat[i,j])/sum(Xemp_mat[,j]))/(sum(Xemp_mat[i,])/sum(Xemp_mat))
#   }
# }
# rownames(LQ) <- rownames(Xemp_mat)
# colnames(LQ) <- colnames(Xemp_mat)
# 
# 
# LQc <-  matrix(0, nrow = Industry_Count, ncol = Region_Count)
# for (i in 1:Industry_Count){
#   for (j in 1:Region_Count){
#     if (LQ[i,j] > 1){
#       LQc[i,j] <- 1
#     }
#     else{
#       LQc[i,j] <- LQ[i,j]
#     }
#   }
# }
# 
# Dir_LQ <-  c()
# for (i in 1:ncol(LQc)){
#   Dir_LQ[[i]] <- (diag(LQc[,i]) %*% Direct_mat) %>% round(4)
#   rownames(Dir_LQ[[i]]) <-  rownames(Xemp_mat)
# }
# names(Dir_LQ) <- colnames(Xemp_mat)
# 


LQ <-  Xemp_mat
for (l in 1:length(Xemp_mat)){
  for (i in 1:dim(Xemp_mat[[l]])[1]){
    for (j in 1:dim(Xemp_mat[[l]])[2]){
      LQ[[l]][i,j] <-((Xemp_mat[[l]][i,j])/sum(Xemp_mat[[l]][,j]))/(sum(Xemp_mat[[l]][i,])/sum(Xemp_mat[[l]]))
    }
  }
  rownames(LQ[[l]]) <- rownames(Xemp_mat[[l]])
  colnames(LQ[[l]]) <- colnames(Xemp_mat[[l]])
}

LQc <-  LQ
for (l in 1:length(Xemp_mat)){
  for (i in 1:dim(Xemp_mat[[l]])[1]){
    for (j in 1:dim(Xemp_mat[[l]])[2]){
      if (LQ[[l]][i,j] > 1){
        LQc[[l]][i,j] <- 1
      }
      else{
        LQc[[l]][i,j] <- LQ[[l]][i,j]
      }
    }
  }
}


Dir_LQ <- vector(mode='list', length=length(Xemp_mat))
for (l in 1:length(Xemp_mat)){
  Dir_LQ[[l]] <- vector(mode='list', length=ncol(LQc[[l]]))
}

for (l in 1:length(Xemp_mat)){
  for (i in 1:ncol(LQc[[l]])){
    Dir_LQ[[l]][[i]] <- (diag(LQc[[l]][,i]) %*% Direct_mat[[l]]) %>% round(4)
    rownames(Dir_LQ[[l]][[i]]) <-  rownames(Xemp_mat[[l]])
  }
  names(Dir_LQ[[l]]) <- colnames(Xemp_mat[[l]])
}




############ Cross-Industry Location Quotient

# CLQ <-  c()
# for (i in 1:Region_Count){
#   CLQ[[i]] <- (LQ[,i] %*% t(1/LQ[,i])) %>% round(4)
#   rownames(CLQ[[i]]) <- rownames(Xemp_mat)
# }
# names(CLQ) <- colnames(Xemp_mat)
# 
# 
# CLQc <- CLQ
# CLQc %<>% finiterer() %>% onerer()
# names(CLQc) <- colnames(Xemp_mat)
# 
# 
# Dir_CLQ <-  c()
# for (i in 1:length(CLQc)){
#   Dir_CLQ[[i]] <- (CLQc[[i]] * Direct_mat) %>% round(4)
# }
# names(Dir_CLQ) <- colnames(Xemp_mat)


CLQ <- vector(mode='list', length=length(Xemp_mat))
for (l in 1:length(Xemp_mat)){
  CLQ[[l]] <- vector(mode='list', length=ncol(Xemp_mat[[l]]))
}
for (l in 1:length(Xemp_mat)){
for (i in 1:dim(Xemp_mat[[l]])[2]){
  CLQ[[l]][[i]] <- (LQ[[l]][,i] %*% t(1/LQ[[l]][,i])) %>% round(4)
  rownames(CLQ[[l]][[i]]) <- rownames(Xemp_mat[[l]])
}
names(CLQ[[l]]) <- colnames(Xemp_mat[[l]])
}

CLQc <- CLQ
for (l in 1:length(Xemp_mat)){
 CLQc[[l]] %<>% finiterer() %>% onerer()
 names(CLQc[[l]]) <- colnames(Xemp_mat[[l]])
}

Dir_CLQ <- vector(mode='list', length=length(Xemp_mat))
for (l in 1:length(Xemp_mat)){
  # PROBLEM! the line below throws an error "invalid 'length' argument"
  Dir_CLQ[[l]] <- vector(mode='list', length=ncol(CLQc[[l]]))
}
for (l in 1:length(Xemp_mat)){
  for (i in 1:ncol(CLQc[[l]])){
    Dir_CLQ[[l]][[i]] <- (diag(CLQc[[l]][,i]) %*% Direct_mat[[l]]) %>% round(4)
    rownames(Dir_CLQ[[l]][[i]]) <-  rownames(Xemp_mat[[l]])
  }
  names(Dir_CLQ[[l]]) <- colnames(Xemp_mat[[l]])
}


############ Semilogarithmic Location Quotient

# SLQ <-  c()
# for (i in 1:Region_Count){
#   SLQ[[i]] <- (LQ[,i] %*% t(1/(log2(1 + LQ[,i])))) %>% round(4)
#   rownames(SLQ[[i]]) <- rownames(Xemp_mat)
# }
# names(SLQ) <- colnames(Xemp_mat)
# 
# SLQc <- SLQ
# SLQc %<>% finiterer() %>% onerer()
# names(SLQc) <- colnames(Xemp_mat)
# 
# Dir_SLQ <-  c()
# for (i in 1:length(SLQc)){
#   Dir_SLQ[[i]] <- (SLQc[[i]] * Direct_mat) %>% round(4)
# }
# names(Dir_SLQ) <- colnames(Xemp_mat)


SLQ <- vector(mode='list', length=length(Xemp_mat))
for (l in 1:length(Xemp_mat)){
  SLQ[[l]] <- vector(mode='list', length=ncol(Xemp_mat[[l]]))
}
for (l in 1:length(Xemp_mat)){
  for (i in 1:dim(Xemp_mat[[l]])[2]){
    SLQ[[l]][[i]] <- (LQ[[l]][,i] %*% t(1/(log2(1 + LQ[[l]][,i])))) %>% round(4)
    rownames(SLQ[[l]][[i]]) <- rownames(Xemp_mat[[l]])
  }
  names(SLQ[[l]]) <- colnames(Xemp_mat[[l]])
}

SLQc <- SLQ
for (l in 1:length(Xemp_mat)){
  SLQc[[l]] %<>% finiterer() %>% onerer()
  names(SLQc[[l]]) <- colnames(Xemp_mat[[l]])
}

Dir_SLQ <- vector(mode='list', length=length(Xemp_mat))
for (l in 1:length(Xemp_mat)){
  Dir_SLQ[[l]] <- vector(mode='list', length=ncol(SLQc[[l]]))
}
for (l in 1:length(Xemp_mat)){
  for (i in 1:ncol(SLQc[[l]])){
    Dir_SLQ[[l]][[i]] <- (diag(SLQc[[l]][,i]) %*% Direct_mat[[l]]) %>% round(4)
    rownames(Dir_SLQ[[l]][[i]]) <-  rownames(Xemp_mat[[l]])
  }
  names(Dir_SLQ[[l]]) <- colnames(Xemp_mat[[l]])
}


############ Flegg Location Quotient
# FLQ_delta <- .3
# lambda <- (log2(1 + (colSums(Xemp_mat)/sum(Xemp_mat))))^(FLQ_delta)
# 
# FLQ <-  c()
# for (i in 1:Region_Count){
#   FLQ[[i]] <- ((LQ[,i] %*% t(1/LQ[,i])) * lambda[i]) %>% round(4)
#   rownames(FLQ[[i]]) <- rownames(Xemp_mat)
# }
# names(FLQ) <- colnames(Xemp_mat)
# 
# FLQc <- FLQ
# FLQc %<>% finiterer() %>% onerer()
# names(FLQc) <- colnames(Xemp_mat)
# 
# Dir_FLQ <-  c()
# for (i in 1:length(FLQc)){
#   Dir_FLQ[[i]] <- (FLQc[[i]] * Direct_mat) %>% round(4)
# }
# names(Dir_FLQ) <- colnames(Xemp_mat)

FLQ_delta <- .3
lambda <- vector(mode='list', length=length(Xemp_mat))
for (l in 1:length(Xemp_mat)){
 lambda <- (log2(1 + (colSums(Xemp_mat[[l]])/sum(Xemp_mat[[l]]))))^(FLQ_delta)
}

FLQ <- vector(mode='list', length=length(Xemp_mat))
for (l in 1:length(Xemp_mat)){
  FLQ[[l]] <- vector(mode='list', length=ncol(Xemp_mat[[l]]))
}
for (l in 1:length(Xemp_mat)){
  for (i in 1:dim(Xemp_mat[[l]])[2]){
    FLQ[[l]][[i]] <- ((LQ[[l]][,i] %*% t(1/LQ[[l]][,i])) * lambda[[l]][i]) %>% round(4)
    rownames(FLQ[[l]][[i]]) <- rownames(Xemp_mat[[l]])
  }
  names(FLQ[[l]]) <- colnames(Xemp_mat[[l]])
}

FLQc <- FLQ
for (l in 1:length(Xemp_mat)){
  FLQc[[l]] %<>% finiterer() %>% onerer()
  names(FLQc[[l]]) <- colnames(Xemp_mat[[l]])
}

Dir_FLQ <- vector(mode='list', length=length(Xemp_mat))
for (l in 1:length(Xemp_mat)){
  Dir_FLQ[[l]] <- vector(mode='list', length=ncol(FLQc[[l]]))
}
for (l in 1:length(Xemp_mat)){
  for (i in 1:ncol(FLQc[[l]])){
    Dir_FLQ[[l]][[i]] <- (diag(FLQc[[l]][,i]) %*% Direct_mat[[l]]) %>% round(4)
    rownames(Dir_FLQ[[l]][[i]]) <-  rownames(Xemp_mat[[l]])
  }
  names(Dir_FLQ[[l]]) <- colnames(Xemp_mat[[l]])
}



############ Augmented Flegg Location Quotient

# LQa <- LQ
# for (i in 1:nrow(LQa)){
#   for (j in 1:ncol(LQa)){
#     if (LQ[i,j] > 1){
#       LQa[i,j] <- log2(1 + LQ[i,j])
#     }
#     else{
#       LQa[i,j] <- 1
#     }
#   }
# }
# 
# AFLQ <- FLQ %>% finiterer()
# for (r in 1:length(AFLQ)){
#   AFLQ[[r]] <- (AFLQ[[r]] %*% diag(LQa[,r])) %>% round(4)
#   colnames(AFLQ[[r]])  <- rownames(Xemp_mat)
# }
# names(AFLQ) <- colnames(Xemp_mat)
# 
# Dir_AFLQ <-  c()
# for (i in 1:length(AFLQ)){
#   Dir_AFLQ[[i]] <- (AFLQ[[i]] * Direct_mat) %>% round(4)
# }
# names(Dir_AFLQ) <- colnames(Xemp_mat)



LQa <- LQ
for (l in 1:length(LQa)){
 for (i in 1:nrow(LQa[[l]])){
  for (j in 1:ncol(LQa[[l]])){
    if (LQ[[l]][i,j] > 1){
      LQa[[l]][i,j] <- log2(1 + LQ[[l]][i,j])
    }
    else{
      LQa[[l]][i,j] <- 1
    }
  }
 }
}


AFLQ <- FLQ
for (l in 1:length(Xemp_mat)){
 AFLQ[[l]] <- FLQ[[l]] %>% finiterer()
}

for (l in 1:length(Xemp_mat)){
 for (r in 1:length(AFLQ)){
   AFLQ[[l]][[r]] <- (AFLQ[[l]][[r]] %*% diag(LQa[[l]][,r])) %>% round(4)
   colnames(AFLQ[[l]][[r]])  <- rownames(Xemp_mat[[l]])
 }
 names(AFLQ[[l]]) <- colnames(Xemp_mat[[l]])
}


Dir_AFLQ <- vector(mode='list', length=length(Xemp_mat))
for (l in 1:length(Xemp_mat)){
  Dir_AFLQ[[l]] <- vector(mode='list', length=ncol(AFLQ[[l]]))
}
for (l in 1:length(Xemp_mat)){
  for (i in 1:ncol(AFLQ[[l]])){
    Dir_AFLQ[[l]][[i]] <- (diag(AFLQ[[l]][,i]) %*% Direct_mat[[l]]) %>% round(4)
    rownames(Dir_AFLQ[[l]][[i]]) <-  rownames(Xemp_mat[[l]])
  }
  names(Dir_AFLQ[[l]]) <- colnames(Xemp_mat[[l]])
}





#### Trimming distance decay matrix to only counties with economic data (varies by industry level specification)    
Q_mat[[1]] <- QI_mat
Q_mat[[1]] <- Q_mat[[1]][rownames(Q_mat[[1]]) %in% intersect(unique(CBP_2019_Sector_XBEA$place), intersect(unique(TIGERData$place), unique(RUCCData$place) ) ), ]
Q_mat[[1]] <- Q_mat[[1]][ , colnames(Q_mat[[1]]) %in% intersect(unique(CBP_2019_Sector_XBEA$place), intersect(unique(TIGERData$place), unique(RUCCData$place) ) ) ]

Q_mat[[2]] <- QI_mat
Q_mat[[2]] <- Q_mat[[2]][rownames(Q_mat[[2]]) %in% intersect(unique(QCEW_2020_Sum_XBEA$place), intersect(unique(TIGERData$place), unique(RUCCData$place) ) ), ]
Q_mat[[2]] <- Q_mat[[2]][ , colnames(Q_mat[[2]]) %in% intersect(unique(QCEW_2020_Sum_XBEA$place), intersect(unique(TIGERData$place), unique(RUCCData$place) ) ) ]

Q_mat[[3]] <- QI_mat
Q_mat[[3]] <- Q_mat[[3]][rownames(Q_mat[[3]]) %in% intersect(unique(CBP_2019_Detail_XBEA$place), intersect(unique(TIGERData$place), unique(RUCCData$place) ) ), ]
Q_mat[[3]] <- Q_mat[[3]][ , colnames(Q_mat[[3]]) %in% intersect(unique(CBP_2019_Detail_XBEA$place), intersect(unique(TIGERData$place), unique(RUCCData$place) ) ) ]

names(Q_mat) <- industry_levels

############ Gravity matrix
# Gravity_mat <- as.list(c())
# for (i in 1:nrow(Xemp_mat)){
#   Gravity_mat[[i]] <- ((Xemp_mat[i,] %*% t(Xemp_mat[i,])) / sum(Xemp_mat[i,]) * Q_mat) %>% round(4)
#   rownames(Gravity_mat[[i]]) <- colnames(Xemp_mat)
# }
# names(Gravity_mat) <- rownames(Xemp_mat)

Gravity_mat <- vector(mode='list', length=length(Xemp_mat))
for (l in 1:length(Xemp_mat)){
  Gravity_mat[[l]] <- vector(mode='list', length=ncol(Xemp_mat[[l]]))
}
for (l in 1:length(Gravity_mat)){
 for (i in 1:nrow(Xemp_mat[[l]])){
  Gravity_mat[[l]][[i]] <- ((Xemp_mat[[l]][i,] %*% t(Xemp_mat[[l]][i,])) / sum(Xemp_mat[[l]][i,]) * Q_mat[[l]]) %>% round(4)
  rownames(Gravity_mat[[l]][[i]]) <- colnames(Xemp_mat[[l]])
 }
 names(Gravity_mat[[l]]) <- rownames(Xemp_mat[[l]])
}



############ Input Needs
Input_mat <- Total_mat
for (l in 1:length(Total_mat)){
  Input_mat[[l]] <- (Direct_mat[[l]]  %*%  Xpay_mat[[l]])
}
names(Input_mat) <- industry_levels

############ Relative Input Needs
Input_mat_rel <- Total_mat
for (l in 1:length(Total_mat)){
  Input_mat_rel[[l]] <- (Input_mat[[l]]) %*% (diag(c(1/(rep(c(1), each=ncol(Direct_mat[[l]])) %*% Input_mat[[l]]))))
  colnames(Input_mat_rel[[l]]) <- colnames(Input_mat[[l]])
}
names(Input_mat_rel) <- industry_levels

############ Import Input Needs
Input_mat_imp <- Total_mat
for (l in 1:length(Total_mat)){
  Input_mat_imp[[l]] <- pmax(Input_mat[[l]] - Xpay_mat[[l]], 0)
}
names(Input_mat_imp) <- industry_levels

############ Net Exports 
Input_mat_exp <- Total_mat
for (l in 1:length(Total_mat)){
  Input_mat_exp[[l]] <- abs(pmin(Input_mat[[l]] - Xpay_mat[[l]], 0))
}
names(Input_mat_exp) <- industry_levels

############ Relative Import Input Needs
Input_mat_imp_rel <- Total_mat
for (l in 1:length(Total_mat)){
  Input_mat_imp_rel[[l]]  <- pmax(Input_mat_rel[[l]]  - Xpay_mat[[l]]  %*% (diag(c(1/(rep(c(1), each=nrow(Xpay_mat[[l]] )) %*% Xpay_mat[[l]] )))), 0)
  colnames(Input_mat_imp_rel[[l]] ) <- colnames(Input_mat[[l]])
}
names(Input_mat_imp_rel) <- industry_levels







############ Similarity Index
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
names(Sim_mat) <- industry_levels


############ Relative Similarity Index
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
names(Sim_mat_rel) <- industry_levels


############ Import Similarity Index
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
names(Sim_mat_imp) <- industry_levels


############ *Import Similarity Index - Net Exports
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
names(Sim_mat_exp) <- industry_levels


############ Relative Import Similarity Index
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
names(Sim_mat_imp_rel) <- industry_levels


############ *Relative Import Similarity Index - Net Exports
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
names(Sim_mat_exp_rel) <- industry_levels


Sim_list <- list(Sim_mat, Sim_mat_rel, Sim_mat_imp, Sim_mat_exp, Sim_mat_imp_rel, Sim_mat_exp_rel)
names(Sim_list) <- c("Sim_mat", "Sim_mat_rel",  "Sim_mat_imp", "Sim_mat_exp", "Sim_mat_imp_rel", "Sim_mat_exp_rel")


##### prox test
#test <- Sim_mat_imp_rel*Prox_mat
#test <- replace(test, test<=0, Inf)



####### Saved Data for use in other renderable files
save.image(file=file.path(root_dir, "data", "all_data.RData"))
save(list = c("Sim_list", "TIGER_RUCC", "Direct_mat", "Total_mat", "Xpay_mat", "Sim_mat",  "Sim_mat_rel", "Sim_mat_imp", "Sim_mat_imp_rel", "Sim_mat_exp", "Sim_mat_exp_rel", "Q_mat", "Dist_mat"),
     file=file.path(root_dir, "data", "sub_data.RData"))


