
# This script is a diagnostic for suspect statistics

source("R/basic_utilities.R", local = (util <- new.env()))
source("R/dataprep_bea_io.R", local = (bea_io <- new.env()))
source("R/dataprep_cbp.R", local = (cbp <- new.env()))
source("R/dataprep_agcensus.R", local = (agcen <- new.env()))
source("R/geography.R", local = (geog <- new.env()))
source("R/place_output.R", local = (place_output <- new.env()))
source("R/circularity.R", local = (circularity <- new.env()))
source("R/dataprep_infogroup.R", local = (ig <- new.env()))

############
#(Postmultiplication of a matrix by $\mathbf{i}$ creates a column vector whose elements are the row sums of the matrix. Similarly, premultiplication of a matrix by $\mathbf{i'}$ creates a row vector whose elements are the column sums of the matrix.)
# The "hat" over a vector denotes a diagonal matrix with the elements of the vector along the main diagonal

# Domestic Intermediate Commodity Supply matrix = hat(Microdata Commodity Use Shares vector[commodity sub-set])*(Microdata Commodity Output matrix[commodity sub-set, ])
# Microdata Commodity Use Shares vector = (Microdata Intermediate Commodity Use vector) / (Microdata Commodity Supply vector)
# Microdata Commodity Output matrix = (Censored C matrix) * (Microdata Industry Output matrix[industry sub-set, ])
# commodity sub-set = all commodities where: 1) BEA Commodity Share Factor is finite AND 2) (Microdata Commodity Output matrix) * i != 0
# Microdata Intermediate Commodity Use vector = ((B matrix) * (Microdata Industry Output matrix)) * i
# Microdata Commodity Supply vector = ((Microdata Commodity Output matrix) * i) / (BEA Commodity Share Factor) 
# Censored C matrix = (condensed BEA Supply matrix[, industry sub-set]) * hat(1/((Microdata Industry Output matrix[, industry sub-set]) * i))
# Microdata Industry Output matrix = As given
# BEA Commodity Share Factor = (condensed BEA Commodity Output vector) / (condensed BEA Commodity Supply vector)
# B matrix = (condensed BEA Use matrix %*% hat(1/(condensed BEA Industry Output vector)))
# condensed BEA Supply matrix = As given
# industry sub-set = all industry where: (Microdata Industry Output matrix) * i != 0
# condensed BEA Use matrix = As given
# condensed BEA Industry Output vector = t(i * (condensed BEA Supply matrix))
# condensed BEA Commodity Output vector = (condensed BEA Supply matrix) * i
# condensed BEA Commodity Supply vector = As given. Taken from condensed BEA Supply table column "T016" aka "Total Product Supply (producer prices)" aka "Total Use of Products" aka condensed BEA Use table column "T019"


year = 2012
ilevel = "det"
bus_data = "infogroup"


sm <- bea_io$call_supply_matrix(year = year, ilevel = ilevel, condense = T)*1000 # condensed BEA Supply matrix
um <- bea_io$call_use_matrix(year = year, ilevel = ilevel, condense = T)*1000 # condensed BEA Use matrix
io <- place_output$call_output(year = year, class_system = "industry", ilevel = ilevel, bus_data = bus_data, verbose = F) %>% util$long2matrix() # Microdata Industry Output matrix
BEA_cs <- bea_io$call_commodity_supply(year = year, ilevel = ilevel, condense = T)*1000 # condensed BEA Commodity Supply vector

iss <- io %>% {rownames(.)[rowSums(.) != 0]} # industry sub-set
io %>% {rownames(.)[rowSums(.) == 0]} # list of problem industries
ccmat <-  sm[, iss] %*% diag(1/as.vector(rowSums(io[iss, , drop=F]))) %>% `colnames<-`(iss) # Censored C matrix 
co <- ccmat %*% io[iss, ] # Microdata Commodity Output matrix
BEA_iov <- colSums(sm) %>% as.matrix() # condensed BEA Industry Output vector
BEA_cov <- rowSums(sm) %>% as.matrix() # condensed BEA Commodity Output vector
BEA_csf <- BEA_cov / BEA_cs # BEA Commodity Share Factor
cs <- rowSums(co) / BEA_csf # Microdata Commodity Supply vector
bmat <- um %*% diag(1/as.vector(BEA_iov)) %>% `colnames<-`(colnames(um)) # B matrix
icu <- rowSums(bmat %*% io) %>% as.matrix() # Microdata Intermediate Commodity Use vector
css <- intersect(rownames(BEA_csf)[is.finite(BEA_csf)], rownames(co)[rowSums(co) != 0]) # commodity sub-set
c(rownames(BEA_csf)[!is.finite(BEA_csf)], rownames(co)[rowSums(co) == 0]) # list of problem commodities
cus <- icu / cs # Microdata Commodity Use Shares vector
ics <-  diag(as.vector(cus[css, , drop = F])) %*% co[css, ,  drop = F] %>% `rownames<-`(css) # Domestic Intermediate Commodity Supply matrix


co_df <- as.data.frame.table(co) %>% `colnames<-`(c("indcode", "place", "gross_output")) # Microdata Commodity Output dataframe
ics_df <- as.data.frame.table(ics) %>% `colnames<-`(c("indcode", "place", "intermediate_supply")) # Domestic Intermediate Commodity Supply dataframe
df <- left_join(co_df, ics_df,  by = join_by(indcode, place)) 
df[is.na(df)]=0

dff <- df %>% 
  {aggregate(.[sapply(.,is.numeric)], list(.[["place"]]), FUN=sum)} %>% 
  `colnames<-`(c("place", names(.)[-1])) %>% 
  mutate(cap = intermediate_supply/gross_output)




BEA_cov_alt <- rowSums(sm[, iss]) %>% as.matrix() # censored condensed BEA Commodity Output vector
BEA_csf_alt <- BEA_cov_alt / BEA_cs # censored BEA Commodity Share Factor
cs_alt <- rowSums(co) / BEA_csf_alt # censored Microdata Commodity Supply vector
bmat_alt <- um[, iss]  %*% diag(1/as.vector(BEA_iov[rownames(BEA_iov) %in% iss] )) # censored B matrix
icu_alt <- rowSums(bmat_alt %*% io[iss, ,drop=F]) %>% as.matrix() # censored Microdata Intermediate Commodity Use vector
cus_alt <- icu_alt / cs_alt # censored Microdata Commodity Use Shares vector
ics_alt <-  diag(as.vector(cus_alt[css, , drop = F])) %*% co[css, ,  drop = F] %>% `rownames<-`(css) # censored Domestic Intermediate Commodity Supply matrix

co_df <- as.data.frame.table(co) %>% `colnames<-`(c("indcode", "place", "gross_output")) # Microdata Commodity Output dataframe
ics_df_alt <- as.data.frame.table(ics_alt) %>% `colnames<-`(c("indcode", "place", "intermediate_supply")) # Domestic Intermediate Commodity Supply dataframe
df_alt <- left_join(co_df, ics_df_alt,  by = join_by(indcode, place)) 
df_alt[is.na(df_alt)]=0

dff_alt <- df_alt %>% 
  {aggregate(.[sapply(.,is.numeric)], list(.[["place"]]), FUN=sum)} %>% 
  `colnames<-`(c("place", names(.)[-1])) %>% 
  mutate(cap = intermediate_supply/gross_output)






if(FALSE){
  sm <- sm[, iss, drop=F] # censored Supply matrix
  um <- um[, iss, drop=F] # censored Use matrix
  io <- io[iss, , drop=F] # censored Industry Output
  
  
  # BEA_iov <- colSums(sm) %>% as.matrix() # condensed BEA Industry Output vector
  # BEA_cov <- rowSums(sm[, iss]) %>% as.matrix() # censored condensed BEA Commodity Output vector
  # BEA_csf <- BEA_cov / BEA_cs # censored BEA Commodity Share Factor
  # cs <- rowSums(co) / BEA_csf # censored Microdata Commodity Supply vector
  # bmat <- um[, iss]  %*% diag(1/as.vector(BEA_iov[rownames(BEA_iov) %in% iss] )) # censored B matrix
  # icu <- rowSums(bmat %*% io[iss, ,drop=F]) %>% as.matrix() # censored Microdata Intermediate Commodity Use vector
  # cus <- icu / cs # censored Microdata Commodity Use Shares vector
  # ics <-  diag(as.vector(cus[css, , drop = F])) %*% co[css, ,  drop = F] %>% `rownames<-`(css) # censored Domestic Intermediate Commodity Supply matrix
  # 
  # co_df <- as.data.frame.table(co) %>% `colnames<-`(c("indcode", "place", "gross_output")) # Microdata Commodity Output dataframe
  # ics_df <- as.data.frame.table(ics) %>% `colnames<-`(c("indcode", "place", "intermediate_supply")) # Domestic Intermediate Commodity Supply dataframe
  # df <- left_join(co_df, ics_df,  by = join_by(indcode, place)) 
  # df[is.na(df)]=0
  # 
  # dff <- df %>% 
  #   {aggregate(.[sapply(.,is.numeric)], list(.[["place"]]), FUN=sum)} %>% 
  #   `colnames<-`(c("place", names(.)[-1])) %>% 
  #   mutate(cap = intermediate_supply/gross_output)
}


#########
# Why and how does supply>output?

# If cus > 1, then supply>output
cus[css, , drop=F] %>% {rownames(.)[.>1]} 
# If icu > cs, then cus > 1
(icu > cs) %>% {rownames(.)[.]}
# Thus either icu is too large OR cs is too small OR both. Relative to what standard?
# If icu is the issue, the theoretical foundation may be at issue!
# does Intermediate Commodity Use vector recovery from B matrix and Industry Output hold for national level BEA data? Yes* BUT rounding precision is an issue.
((bmat %*% BEA_iov) == rowSums(um) %>% as.matrix() ) %>% all() # FALSE
(abs((bmat %*% BEA_iov)-as.matrix(rowSums(um))) <= .0001) %>% all() # TRUE
# does (abs(rowSums(um)-icu) <= .0001) %>% all()? FALSE
(abs(rowSums(um)-icu) > .0001) %>% {rownames(.)[.]} # list of commodities not consistent between BEA and microdata. 332 of 389 commodities
((abs(rowSums(um)-icu) > .0001) %>% {rownames(.)[.]} %>% icu[.,])/((abs(rowSums(um)-icu) > .0001) %>% {rownames(.)[.]} %>% rowSums(um)[.])  # is the difference small or large?
# or even does sum(icu) == sum(um)? FALSE 
sum(icu) / sum(um) # is the difference small or large?

# accounting check 0: 
  # does (rowSums(io) == BEA_iov) %>% all()? FALSE
    sum((rowSums(io) != BEA_iov)) == sum(abs(rowSums(io)-BEA_iov) > .0001) # BUT tolerance is an issue! 124 vs 15 of 391 industries
  # does (abs(rowSums(io)-BEA_iov) <= .0001) %>% all()? FALSE
    (abs(rowSums(io)-BEA_iov) > .0001) %>% {rownames(.)[.]} # list of industries that cannot be reconciled between BEA and microdata
    io %>% {rownames(.)[rowSums(.) == 0]} %>% {rownames(sm)[(rowSums(sm[, .]) > 0)]} # list of all commodities produced by industries that cannot be reconciled between BEA and microdata. 128 commodities. 
      io %>% {rownames(.)[rowSums(.) == 0]} %>% sm[, .] %>% View() # sub-matrix 
      # io %>% {rownames(.)[rowSums(.) == 0]} %>% sm[, .] %>% rowSums() %>% as.data.frame() %>% View() 
    (abs(rowSums(io)-BEA_iov) > .0001) %>% {rownames(.)[.]} %>% rowSums(io)[.] # value of for microdata industries that do not match BEA
    (abs(rowSums(io)-BEA_iov) > .0001) %>% {names(.)[.]} %>% BEA_iov[.] # value of for BEA industries that do not match microdata
  # or even does sum(io) == sum(sm)? FALSE 
    sum(io)/sum(sm) # is the difference small or large?
  # or even does (rownames(io) == colnames(sm)) %>% all()? TRUE
  # or even does dim(io)[1] == dim(sm)[2]? TRUE


# If cs is the issue, look to rowSums(co) OR BEA_csf OR both
rownames(BEA_cov)[BEA_cov==0] # commodities with zero BEA commodity output? zero
rownames(BEA_cs)[BEA_cs==0] # commodities with zero BEA commodity supply? 8 
rownames(BEA_csf)[BEA_csf>1] # commodities with share factor greater than one? 33
rownames(BEA_csf)[!is.finite(BEA_csf)] # commodities with infinite share factor? 8
rownames(co)[rowSums(co)==0] # commodities with zero microdata commodity output? 8
rownames(cs)[rowSums(cs)==0] # commodities with zero microdata commodity supply? 16

# does Commodity Supply vector recovery from Commodity Share Factor and Commodity Output hold for national level BEA data? Yes* BUT rounding precision is an issue.
(BEA_cs == BEA_cov / BEA_csf) %>% all() # FALSE
(abs(BEA_cs-(BEA_cov / BEA_csf)) <= .0001) %>% all() # TRUE


# accounting check 1: 
  # does (rowSums(co) == BEA_cov) %>% all()? FALSE
    sum((rowSums(co) != BEA_cov)) == sum(abs(rowSums(co)-BEA_cov) > .0001) # BUT tolerance is an issue! 209 vs 128 of 389 commodities
  # are the non conforming commodities the same as those commodities produced by industries that cannot be reconciled? TRUE 
    length(io %>% {rownames(.)[rowSums(.) == 0]} %>% {rownames(sm)[(rowSums(sm[, .]) > 0)]}) == sum(abs(rowSums(co)-BEA_cov) > .0001)
    io %>% {rownames(.)[rowSums(.) == 0]} %>% {rownames(sm)[(rowSums(sm[, .]) > 0)]} == (abs(rowSums(co)-BEA_cov) > .0001) %>% {rownames(.)[.]}
  # does (abs(rowSums(co)-BEA_cov) <= .0001) %>% all()? FALSE
    (abs(rowSums(co)-BEA_cov) > .0001) %>% {rownames(.)[.]} # list of commodities not consistent between BEA and microdata
    (abs(rowSums(co)-BEA_cov) > .0001) %>% {rownames(.)[.]} %>% rowSums(co)[.] # value of for microdata commodities that do not match BEA
    (abs(rowSums(co)-BEA_cov) > .0001) %>% {rownames(.)[.]} %>% BEA_cov[., ] # value of for BEA commodities that do not match microdata
    ((abs(rowSums(co)-BEA_cov) > .0001) %>% {rownames(.)[.]} %>% rowSums(co)[.]) / ((abs(rowSums(co)-BEA_cov) > .0001) %>% {rownames(.)[.]} %>% BEA_cov[., ]) # is the difference small or large?
  # or even does sum(co) == sum(sm)? FALSE 
    sum(co)/sum(sm) # is the difference small or large?
  # but does sum(co) == sum(io)? TRUE 
  # or even does (rownames(co) == rownames(sm)) %>% all()? TRUE
  # or even does dim(co)[1] == dim(sm)[1]? TRUE
# accounting check 2: 

#########





# Why is it that for some counties the circularity metrics Trade Capacity and Production Capacity are greater than one i.e., infeasible?
# This is an issue because Trade Capacity is given by the ratio (intermediate_supply / gross_output) and Production Capacity is given by the ratio (excess_supply / gross_output). 
# Where Excess Supply is the Sum of max(intermediate_supply_i - intermediate_demand_i, 0) for all industries or commodities i
# UPDATE: fixing the HS to 531 concordance issue fixed 4 places with supply>gross

for(l in c("infogroup", "cbp_imp")){
  for(i in c("sec", "sum", "det")){
    circularity$call_circularity_metrics(
      year = 2012,
      ilevel = i,
      class_system = "commodity",
      paradigm = "domestic",
      bus_data = l,
      spatial = FALSE ) %>% 
      filter(if_any(production_capacity:autonomy, \(x) x > 1))  %>% 
      {dim(.)[1]} %>% 
      paste(., i, l) %>% 
      print()
  }
}

# Why is it that Intermediate Supply is greater than Gross Output?
# Why is it only an issue on the supply side?
for(l in c("infogroup", "cbp_imp")){
  for(i in c("sec", "sum", "det")){
    place_output$call_extraction_table(2012, class_system = "commodity", paradigm = "domestic", bus_data = l, spatial = F, ilevel = i) %>% 
      filter(intermediate_supply/gross_output > 1 | intermediate_demand/gross_output > 1) %>% 
      {dim(.)[1]} %>% 
      paste(., i, l) %>% 
      print()
  }
}


# Why does CBP exhibit intermediate_supply > gross_output at the commodity specific level too but this does not translate up to the county aggregate level like Infogroup?
# Are we being led astray by the use of supply and demand and terms? In past we used more IO field specific terminology such as "Industry/Commodity Factor Cost" and "Intermediate Industry/Commodity Use". 
# Note: it is perfectly reasonable for a county to have zero production in a commodity or by an industry and yet have a need for a specific commodity in an industry production function. 
# Similarly, it is possible that an industry in a county produces a commodity with no local intermediate market or is a good only consumed by final demand.  
# Note: Look into negative final demand and what that may mean for the model and results. 
# Is the issue coming from the industry commodity translation where the supply matrix is not diagonal thus making a county with off diagonal commodities where it has no associated principle industry?

for(l in c("infogroup", "cbp_imp")){
  for(i in c("sec", "sum", "det")){
    place_output$call_factor_list(2012, class_system = "commodity", paradigm = "domestic", bus_data = l, ilevel = i) %>% select(place) %>% unique() %>% dim() %>% print()
    place_output$call_factor_list(2012, class_system = "commodity", paradigm = "domestic", bus_data = l, ilevel = i) %>% mutate(cap = intermediate_supply/gross_output) %>% filter(cap>1) %>% select(place) %>% unique() %>% dim() %>% print()
    place_output$call_factor_list(2012, class_system = "commodity", paradigm = "domestic", bus_data = l, ilevel = i) %>% mutate(cap = intermediate_supply/gross_output) %>% filter(cap>1) %>% select(indcode) %>% unique() %>% print()
    paste(i, l) %>% print()
  }
}






