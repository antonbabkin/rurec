

options(scipen = 999)
source(file.path(
  rprojroot::find_rstudio_root_file(),
  "nbs",
  "r_backend_functions.R"
))
year = 2012
ilevel = "sec"
#matrix of intermediate uses of commodities by industries: U 
umat <- use_matrix(year, ilevel)
#matrix of total output of commodities by industries: V' 
smat <- supply_matrix(year, ilevel)
#vector of total commodity supply: q=Ui+e note: q!=V'i 
q <- commodity_supply(year, ilevel)
#vector of Total Gross Industry Output: x=U'i+v note: x=Vi 
x <- as.matrix(colSums(smat))
#scaling factor of total commodity output's share of total product supply: phi = (V'i)/q such that V'i=q*phi  
###phi <- commodity_share_factor(year, ilevel)
phi <- rowSums(smat)/q
#vector of Intermediate Industry Use (IIU)=U'i
IIU <- as.matrix(colSums(umat))
#vector of Intermediate Commodity Use (ICU)=Ui
ICU <- as.matrix(rowSums(umat))
#scaling factor of intermediate commodity supply's share of total product supply: lambda = (Ui)/q = (Bx)/q = (ICU)/q such that e=(i-lambda)*q and V'i=Ui+(phi-lambda)*q and Ui=lambda*q
lambda <- ICU/q
#scaling factor of intermediate industry use's share of total industry output: theta = (U'i)/x = (IIU)/x such that v=(i-theta)*x and U'i=theta*x and theta=hat(i'DB)
theta <- IIU/x
#vector of (industry) value added: v=x-U'i
v <- x-IIU
#vector of (commodity) final demand: e=q-Ui
e <- q-ICU
  #sum(IIU) equal to sum(ICU)
  all.equal(sum(IIU), sum(ICU))
  #sum(x) equal to sum(q*phi)
  all.equal(sum(x), sum(q*phi))
  #e=(1-lambda)*q
  all.equal(as.numeric(e), as.numeric((1-lambda)*q))
  #V'i=phi*q
  all.equal(as.numeric(rowSums(smat)), as.numeric(q*phi))
  #V'i=Ui+(phi-lambda)*q
  all.equal(as.numeric(rowSums(smat)), as.numeric(ICU+(phi-lambda)*q))
  #Ui=lambda*q
  all.equal(as.numeric(rowSums(umat)), as.numeric(lambda*q))
  
#OPEN QUESTION: Do we need to adjust phi and lambda and theta based on non-bea output values?
  


#matrix of Technical Coefficients, value of inputs of each commodity per dollar’s worth of industry output B=U*hat(x)^-1
B <- umat %*% diag(1/as.vector(x)) %>% `colnames<-`(colnames(umat))
  #note: B matrix element values independent of commodity selection censoring  
  test <- umat[1,] %*% diag(1/as.vector(x))
  all.equal(as.numeric(B[1,]), as.numeric(test))
  #note: Use matrix is recoverable given B and x: U=B*hat(x)
  test <- B %*% diag(as.vector(x)) %>% `colnames<-`(colnames(B))
  all.equal(umat, test)
  #note: Use matrix recoverability is independent of commodity selection censoring in B
  test <- B[1,] %*% diag(as.vector(x))
  all.equal(as.numeric(umat[1,]), as.numeric(test))
  #note: Use matrix recoverability is independent of industry selection censoring in B & x
  test <- B[,1:14] %*% diag(as.vector(x[1:14]))
  all.equal(as.numeric(umat[,1:14]), as.numeric(test))
#matrix of Commodity Composition of Industry Outputs, fraction of total industry output that is in the form of a given commodity: C=V'*hat(x)^-1
C <- smat %*% diag(1/as.vector(x)) %>% `colnames<-`(colnames(smat))
  #note: q*phi is recoverable given C and x: q*phi=Cx
  test <- C %*% x
  all.equal(as.numeric(q*phi), as.numeric(test))
  #note: q*phi recoverability is independent of commodity selection censoring in C
  test <- C[1,] %*% x
  all.equal(as.numeric((q*phi)[1]), as.numeric(test))
  #note: q recoverability is NOT independent of industry selection censoring in B&x
  test <- C[,1:14] %*% x[1:14]
  cbind(q*phi, test)
  all.equal(as.numeric(q*phi), as.numeric(test))
  #by definition column sums of C should all be one
  colSums(C)
#matrix of Industry Source of Commodity Outputs, fraction of total commodity output that is produced by an industry : D=V*hat(q*phi)^-1
D <- t(smat) %*% diag(1/as.vector(q*phi)) %>% `colnames<-`(rownames(smat))
  #note: x is recoverable given D and q and phi: x=D(q*phi)
  test <- D %*% (q*phi)
  all.equal(as.numeric(x), as.numeric(test))
  #by definition column sums of D should all be one
  colSums(D)
  
  ## QUESTION: Can one derive phi and lambda and theta from B/C/D and x? No, only theta.
  #theta = (DB)'i
  all.equal(as.numeric(theta), as.numeric(rowSums(t(D%*%B))))
  #lambda =(phi)'hat(Bx)hat(Cx)^-1
  all.equal(lambda, t(phi)%*%(diag(as.vector(B%*%x)))%*%(diag(1/as.vector(C%*%x))))
  
  
#Do IIS and ICS exist?
#(IIS)=DBx=D*ICU (????)
IIS = D%*%B%*%x
all.equal(sum(IIU), sum(IIS))
#(ICU)!=C*hat(i'DB)x=C*IIU  (????)
ICS = diag(as.vector(B%*%x))%*%t(D)%*%diag(1/as.vector(x))%*%x
all.equal(sum(ICU), sum(ICS))

#QUESTION: How does one define input needs and output capacity? How do intermediates interact? 
## And can one be derived given the other and a set of technological coefficients and scaling factors?
## Bx+e=Ui+e=Ui+(1-lambda)*q=q=V'i+h=V'i+(1-phi)*q
#Bx+(1-lambda)*q=q=V'i+(1-phi)*q=(Cx)/phi
#Let the total intermediate commodity input need be taken directly from the Use table i.e., Ui
#Let the total commodity output capacity need be taken directly from the Supply table i.e., V'i
##How does one derive an accounting identity to balance Ui and V'i? (V'i=Ui+(phi-lambda)*q)
#Ui+e=q=V'i+h i.e., total intermediate commodity use plus final demand equals total commodity supply plus (factor payments?)(transaction adjustments?)

# QUESTION: Given just x, B, C, D and phi can one recover: q, U, V', IIU, ICU, v, e, IIS, or ICS? 
# q=(Cx)/phi
cbind(q, (C %*% x)/phi)
all.equal(q, (C %*% x)/phi)
# U=B*hat(x)
all.equal(umat, B %*% diag(as.vector(x)) %>% `colnames<-`(colnames(B)))
# V'=C*hat(x)
all.equal(smat, C %*% diag(as.vector(x)) %>% `colnames<-`(colnames(C)))
# (IIU)=U'i
all.equal(IIU, as.matrix(rowSums(t(B %*% diag(as.vector(x))))) %>% `rownames<-`(colnames(B)) )
# (ICU)=Ui
all.equal(ICU, as.matrix(rowSums((B %*% diag(as.vector(x))))) %>% `rownames<-`(rownames(B)) )
# v=x-U'i
all.equal(v, (x - as.matrix(rowSums(t(B %*% diag(as.vector(x)))))))
# e=q-Ui
cbind(e, ((C %*% x)/phi - as.matrix(rowSums(B %*% diag(as.vector(x))))))
all.equal(e, ((C %*% x)/phi - as.matrix(rowSums(B %*% diag(as.vector(x))))))


#Does DBx=U'i=IIU? NO but hat(x)(i'DB)=IIU=hat(i'DB)x
#Does Dq=x? NO but Dq*phi=x
#Does Cx=q? NO but Cx=q*phi or hat(phi)^-1(Cx)=q
#Does Bx=Ui=ICU? YES/NO
#Does BDq=Ui? NO but BD(q*phi)=Ui


#QUESTION: how does this system function with regional components, specifically x broken down into a set of subnational output vectors collected into a single matrix?

#matrix to divide given industry output x across N regions
rm <- replicate(length(x), diff(c(0, sort(runif(4)), 1))) %>% t() 
# regional matrix of Total Gross Industry Output: xr, note: x=xr*i
xr <- (diag(as.vector(x)) %*% rm ) %>% `rownames<-`(rownames(x)) 
all(x == rowSums(xr))
# regional matrix of Total Commodity Supply: qr = hat(phi)^-1(C*xr) 
qr <- (diag(1/as.vector(phi)) %*% (C %*% xr)) %>% `rownames<-`(rownames(C))
all.equal(as.numeric(q), as.numeric(rowSums(qr)))

#regional intermediate industry use: IIUr=hat(i'DB)xr
IIUr <- diag(as.vector(colSums(D%*%B)))%*%xr %>% `rownames<-`(colnames(B)) %>% `colnames<-`(LETTERS[1:ncol(xr)])
all.equal(as.numeric(IIU), as.numeric(rowSums(IIUr)))
#regional intermediate commodity use: ICUr=B*xr
# ICUr <- B%*%xr
ICUr <- B%*%D%*%C%*%xr %>% `rownames<-`(rownames(B)) %>% `colnames<-`(LETTERS[1:ncol(xr)])
all.equal(as.numeric(ICU), as.numeric(rowSums(ICUr)))
#regional intermediate industry supply: IISr=DB*xr
IISr <- D%*%B%*%xr  %>% `rownames<-`(colnames(B)) %>% `colnames<-`(LETTERS[1:ncol(xr)])
all.equal(as.numeric(IIS), as.numeric(rowSums(IISr)))
#regional intermediate commodity supply: ICSr
ICSr <- diag(as.vector(B%*%x))%*%t(D)%*%diag(1/as.vector(x))%*%xr  %>% `rownames<-`(rownames(B)) %>% `colnames<-`(LETTERS[1:ncol(xr)])
all.equal(as.numeric(ICS), as.numeric(rowSums(ICSr)))

##intermediate industry supply by industry != intermediate industry use by industry 
##industries use commodities to make commodities!

##QUESTION: B%*%D%*%C%*%x==B%*%x but B%*%D%*%C%*%xr!=B%*%xr and B%*%D%*%C!=B

cbind(rowSums(diag(as.vector(lambda/phi))%*%C%*%xr), rowSums(B%*%xr))
cbind(rowSums(diag(as.vector(lambda/phi))%*%C%*%xr), rowSums(diag(as.vector(B%*%x))%*%t(D)%*%diag(1/as.vector(x))%*%xr))


#Question: What do we derive from xr, B, C, D, and phi? Answer: qr, IIUr, IISr, ICUr, ICSr and region specific e, v, f, U, V' and h if needed.
#Note: ICUr are the intermediate commodities a region will need to satisfy its known gross industry output (xr)
#Note: ICSr are the intermediate commodities a region can provide, given its known gross industry output (xr)

#Conjecture: Assume all commodities are used locally first i.e., no cross hauling
#The value of net exchangeables in out-flow and in-flow terms are:
net_demand <- pmax(ICUr - ICSr, 0)
net_supply <- pmax(ICSr - ICUr, 0)

rowSums(net_demand)==rowSums(net_supply)
colSums(net_demand)==colSums(net_supply)
sum(net_demand)==sum(net_supply)

trade_flows <- ras_trade_lists(factor_supply = ICSr, factor_demand = ICUr, crosshaul = F)

###Circularity
#capacity=(ICSr)'i(hat(xr'i))^-1
capacity <- rowSums(t(ICSr))%*%diag(1/as.vector(rowSums(t(xr)))) %>% `rownames<-`("capacity") %>% `colnames<-`(LETTERS[1:ncol(xr)])
transmittance <- rowSums(t(net_supply))%*%diag(1/as.vector(rowSums(t(xr))))  %>% `rownames<-`("transmittance") %>% `colnames<-`(LETTERS[1:ncol(xr)])
emissivity <- transmittance/capacity  %>% `rownames<-`("emissivity") %>% `colnames<-`(LETTERS[1:ncol(xr)])
attenuation <- rowSums(t(ICUr))%*%diag(1/as.vector(rowSums(t(xr)))) %>% `rownames<-`("attenuation") %>% `colnames<-`(LETTERS[1:ncol(xr)])
dependency <- rowSums(t(net_demand))%*%diag(1/as.vector(rowSums(t(xr)))) %>% `rownames<-`("dependency") %>% `colnames<-`(LETTERS[1:ncol(xr)])
resourcefulness <- dependency/attenuation %>% `rownames<-`("resourcefulness") %>% `colnames<-`(LETTERS[1:ncol(xr)])
trade_balance <- (rowSums(t(net_demand))-rowSums(t(net_supply)))%*%diag(1/as.vector(rowSums(t(xr)))) %>% `rownames<-`("trade_balance") %>% `colnames<-`(LETTERS[1:ncol(xr)])
trade_openness <- (rowSums(t(net_demand))+rowSums(t(net_supply)))%*%diag(1/as.vector(rowSums(t(xr)))) %>% `rownames<-`("trade_openness") %>% `colnames<-`(LETTERS[1:ncol(xr)])
rbind(capacity,
      transmittance,
      emissivity,
      attenuation,
      dependency,
      resourcefulness,
      trade_balance,
      trade_openness)

################################################################################

supply_mat <- call_supply_table(year = year, ilevel = ilevel) %>% 
  .[1:(nrow(.)-1), 1:(which(colnames(.) == "T007")-1)]
use_mat <- call_use_table(year = year, ilevel = ilevel) %>% 
  .[1:(which(rownames(.) == "T005")-1), 1:(which(colnames(.) == "T001")-1)]

#supply matrix
smat <- supply_mat %>% condense_bea_matrix(., ilevel)
# smat <- supply_mat %>% matrix_collapse(., grep("^23", colnames(.), value = TRUE), "23") %>% 
#   matrix_collapse(., grep("^531", colnames(.), value = TRUE), "531") %>% 
#   .[!grepl("4200ID|S00402|S00300", rownames(.)),!grepl("4200ID", colnames(.)), drop=F] 
#use matrix
umat <- use_mat %>% condense_bea_matrix(., ilevel)
# umat <- use_mat %>% matrix_collapse(., grep("^23", colnames(.), value = TRUE), "23") %>% 
#   matrix_collapse(., grep("^531", colnames(.), value = TRUE), "531") %>% 
#   .[!grepl("4200ID|S00402|S00300", rownames(.)),!grepl("4200ID", colnames(.)), drop=F] 

# tot_ind_out <- call_use_table(2012) %>% 
#   .["T018", !colnames(.) %in% grep("^(F|T)[0-9]", colnames(.), value = TRUE), drop = FALSE] %>% 
#   vector_collapse(., grep("^23", colnames(.), value = TRUE), "23") %>% 
#   vector_collapse(., grep("^531", colnames(.), value = TRUE), "531") %>% 
#   .[,!grepl("4200ID", colnames(.)), drop=F] 



#intermediate industry usage
int_ind_use <- call_use_table(year = year, ilevel = ilevel) %>%
  .["T005", !colnames(.) %in% grep("^(F|T)[0-9]", colnames(.), value = TRUE), drop = FALSE] %>%
  vector_collapse(., grep("^23", colnames(.), value = TRUE), "23") %>%
  vector_collapse(., grep("^531", colnames(.), value = TRUE), "531") %>%
  .[,!grepl("4200ID", colnames(.)), drop=F]

#intermediate commodity usage
int_com_use <- call_use_table(year = year, ilevel = ilevel) %>%
  .[1:(which(rownames(.) == "T005")-1), "T001", drop = FALSE] %>%  t() %>% 
  vector_collapse(., grep("^23", colnames(.), value = TRUE), "23") %>%
  vector_collapse(., grep("^531", colnames(.), value = TRUE), "531") %>%
  .[,!grepl("4200ID|S00402|S00300", colnames(.)), drop=F]

#total industry supply
tot_ind_sup <- call_supply_table(year = year, ilevel = ilevel) %>% 
  .[nrow(.), 1:(which(colnames(.) == "T007")-1), drop=F] %>% 
  vector_collapse(., grep("^23", colnames(.), value = TRUE), "23") %>% 
  vector_collapse(., grep("^531", colnames(.), value = TRUE), "531") %>% 
  .[,!grepl("4200ID", colnames(.)), drop=F] 

#total commodity supply
tot_com_sup <- call_supply_table(year = year, ilevel = ilevel) %>% 
  .[1:(nrow(.)-1), "T007", drop=F] %>% t() %>% 
  vector_collapse(., grep("^23", colnames(.), value = TRUE), "23") %>% 
  vector_collapse(., grep("^531", colnames(.), value = TRUE), "531") %>% 
  .[,!grepl("4200ID|S00402|S00300", colnames(.)), drop=F] 

#B matrix
bmat <- umat %*% diag(1/as.vector(tot_ind_sup)) %>% 
  `colnames<-`(colnames(umat)) 
#C matrix
cmat <- smat %*% diag(1/as.vector(tot_ind_sup)) %>% 
  `colnames<-`(colnames(smat))
#D matrix
dmat <- t(smat) %*% diag(1/as.vector(tot_com_sup)) %>% 
  `colnames<-`(rownames(smat)) 

#direct national industry factor ratio ~= (i'DB)
nat_ind_facrat <- int_ind_use/tot_ind_sup
#derived industry factor ratio (hat(i'DB)x)/x = D'B'i
der_ind_facrat <- as.matrix(rowSums(t(bmat) %*% t(dmat)))
View(cbind(t(nat_ind_facrat), der_ind_facrat, t(nat_ind_facrat)/der_ind_facrat))

#direct national commodity factor ratio
nat_com_facrat <- int_com_use/tot_com_sup
#derived commodity factor ratio ~= (i'DB)
der_com_facrat <- (bmat %*% t(tot_ind_sup)) / (cmat %*% t(tot_ind_sup))
View(cbind(t(nat_com_facrat), der_com_facrat, t(nat_com_facrat)/der_com_facrat))

#Dq=x
View(cbind(dmat %*% t(tot_com_sup), t(tot_ind_sup), dmat %*% t(tot_com_sup)/t(tot_ind_sup)))
#Cx=q
View(cbind(cmat %*% t(tot_ind_sup), t(tot_com_sup), cmat %*% t(tot_ind_sup)/t(tot_com_sup)))

#Bx=Ui
View(cbind(bmat %*% t(tot_ind_sup), t(int_com_use), bmat %*% t(tot_ind_sup)/t(int_com_use)))
#BDq=Ui
View(cbind(bmat %*% dmat %*% t(tot_com_sup), t(int_com_use), bmat %*% dmat %*% t(tot_com_sup)/t(int_com_use)))

#NFG  DBx != i'U
#View(cbind(dmat %*% bmat %*% t(tot_ind_sup), t(int_ind_use), dmat %*% bmat %*% t(tot_ind_sup)/t(int_ind_use)))

#i'(DBhat(x))=i'U
View(cbind(colSums(dmat %*% bmat %*% diag(as.vector(t(tot_ind_sup)))), t(int_ind_use), colSums(dmat %*% bmat %*% diag(as.vector(t(tot_ind_sup))))/t(int_ind_use)))
#x*(i'DB)=i'U
View(cbind(t(tot_ind_sup) * as.matrix(colSums(dmat %*% bmat)), t(int_ind_use), t(tot_ind_sup) * as.matrix(colSums(dmat %*% bmat))/t(int_ind_use)))
#hat(i'DB)x=i'U
View(cbind(diag(as.vector(der_ind_facrat)) %*% t(tot_ind_sup), t(int_ind_use), diag(as.vector(der_ind_facrat)) %*% t(tot_ind_sup)/t(int_ind_use)))
# %>% `colnames<-`(colnames(bmat)) %>% `rownames<-`(colnames(bmat)) 




# What we want from IO process?:
# Gross Industry Output by Region - Scale CBP industry payroll by payrollshare coefficient (GIO = x_r = CBP_ir/ps_i) where (ps = Sum_r(CBP_i)/BEA_i) 
# (NFG) Intermediate Industry Demand by Region - Scale GIO by the column sums of technical coefficients matrix (i.e., Industry Technology Technological Coefficients of Industry Inputs) (IID = hat(i'DB)x = (i'DB)'*x = (DBhat(x))'i)
# (NFG) Intermediate Industry Supply by Region - Scale GIO by the derived industry factor ratio to ensure Supply==Demand (ICS = hat(ifr)x_r) where (note: ifr = (hat(i'DB)x)/x = (hat((hat(i'DB)x)i)(hat(x)^-1))i )
# Gross Commodity Output by Region - Transform GIO to GCO with the C_matrix (i.e., commodity composition of industry outputs) (Cx_r = q_r)
# Intermediate Commodity Demand by Region - Transform GIO to ICD with the B_matrix (i.e., technical coefficients) (ICD = Bx_r) (note: sum(ICD)_r = Ui)
# Intermediate Commodity Supply by Region - Scale GCO by the derived commodity factor ratio to ensure Supply==Demand (ICS = hat(cfr)q_r) where (cfr = hat(Bx)(Cx)^-1)




#can we build commodity factor ratio another way?
#(bmat %*% t(tot_ind_sup)) / (cmat %*% t(tot_ind_sup))

#Supply two ways (effect same for commodity side): in theory Supply==Demand at National level and all same but in practice even at the national level only the two methods are not identical
# 1) scale GIO by national BEA ratio (intermediate industry use/total industry supply) 
# 2) scale GIO by derived ratio sum(Intermediate Industry Demand by Region)_r/sum(Gross Industry Output by Region)_r






bmat <- b_matrix(2012)
cmat <- c_matrix(2012)
dmat <- d_matrix(2012)



