
# Generate Location Quotient data products

# Load and attach necessary packages
library(rprojroot)

# Connect  and  parse  code  from  another  file 
source(file.path(find_rstudio_root_file(), "nbs", "rural_typology_r_data_clean.R"))

options(scipen = 999)

data_dir = file.path(find_rstudio_root_file(), "data", "robjs")


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



if (!file.exists(file.path(data_dir, "Gravity_mat"))){
  Gravity_mat <- vector(mode='list', length=length(readRDS(file.path(data_dir, "Total_mat"))))
  for (l in 1:length(readRDS(file.path(data_dir, "Total_mat")))){
    Gravity_mat[[l]] <- vector(mode='list', length=ncol(readRDS(file.path(data_dir, "Xemp_mat"))[[l]]))
  }
  for (l in 1:length(Gravity_mat)){
    for (i in 1:nrow(readRDS(file.path(data_dir, "Xemp_mat"))[[l]])){
      Gravity_mat[[l]][[i]] <- ((readRDS(file.path(data_dir, "Xemp_mat"))[[l]][i,] %*% t(readRDS(file.path(data_dir, "Xemp_mat"))[[l]][i,])) / (sum(readRDS(file.path(data_dir, "Xemp_mat"))[[l]][i,]) * readRDS(file.path(data_dir, "Q_mat"))[[l]]) ) %>% round(4)
      rownames(Gravity_mat[[l]][[i]]) <- colnames(readRDS(file.path(data_dir, "Xemp_mat"))[[l]])
    }
    names(Gravity_mat[[l]]) <- rownames(readRDS(file.path(data_dir, "Xemp_mat"))[[l]])
  }
  saver(Gravity_mat)
  rm(Gravity_mat)
}

