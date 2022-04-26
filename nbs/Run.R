
# Establish working directory relative to location of this file
script_path() %>% setwd() 

source("Data.R")



options(scipen = 999)
Total_mat <- IO_tables[["IxI_TR_1997-2020_PRO_SEC"]][["2020"]][6:20,3:17] %>% unlist() %>% as.numeric() %>% matrix(ncol = 15)
Ind_Names <- IO_tables[["IxI_TR_1997-2020_PRO_SEC"]][["2020"]][5,3:17] %>% as.list()
rownames(Total_mat) = colnames(Total_mat) <- Ind_Names

Direct_mat <- diag(ncol(Total_mat)) - solve(Total_mat)
local({
temp <- TIGER_CBP[, c("place")]
st_geometry(temp) <- NULL 
Xemp_mat <<- inner_join(temp, CBP_table, by = "place")
})
Xemp_mat <- Xemp_mat[, c("BEA_Sectors", "place", "emp")]
Xemp_mat %<>% reshape(idvar = "BEA_Sectors", v.names = "emp", varying = unique(Xemp_mat$place), timevar = "place", new.row.names = unique(Xemp_mat$BEA_Sectors),  direction = "wide")
Xemp_mat %<>% subset(select = -c(BEA_Sectors)) %>% as.matrix()

Industry_Count <- dim(Xemp_mat)[1]
Region_Count <- dim(Xemp_mat)[2]


############ Simple Location Quotient
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
#Q_mat <- (1/(Dist_mat/100000)^2)
Q_mat <- exp(-(Dist_mat/100000))
Gravity_mat <- as.list(c())
for (i in 1:nrow(Xemp_mat)){
  Gravity_mat[[i]] <- ((Xemp_mat[i,] %*% t(Xemp_mat[i,])) / sum(Xemp_mat[i,]) * Q_mat) %>% round(4)
  rownames(Gravity_mat[[i]]) <- colnames(Xemp_mat)
}
names(Gravity_mat) <- rownames(Xemp_mat)





