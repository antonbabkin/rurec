

source("Data.R")



head(RegionalData_Sector)

options(scipen = 999)
Total_mat <- IO_tables[["IxI_TR_1997-2020_PRO_SEC"]][["2020"]][6:20,3:17] %>% unlist() %>% as.numeric() %>% matrix(ncol = 15)
Ind_Names <- IO_tables[["IxI_TR_1997-2020_PRO_SEC"]][["2020"]][5,3:17] %>% as.list()
rownames(Total_mat) = colnames(Total_mat) <- Ind_Names

Direct_mat <- diag(ncol(Total_mat)) - solve(Total_mat)

Xemp_mat <- CBP_table[, c("BEA_Sectors", "place", "emp")]

Xemp_mat %<>% reshape(idvar = "BEA_Sectors", varying = c("place", "emp"), direction = "wide")





