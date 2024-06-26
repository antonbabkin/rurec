
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
ccmat <-  sm[, iss] %*% diag(1/as.vector(rowSums(io[iss, , drop=F]))) %>% `colnames<-`(iss)  # Censored C matrix 
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
  `colnames<-`(c("place", names(.)[-1]))

