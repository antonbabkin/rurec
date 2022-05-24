

# Establish working directory relative to location of this file
script_path() %>% setwd() 

# Connect and parse code from another file 
source("Data_import.R")



# QCEW 2020 subseted and cleaned into private, county level observations and pertinent variables
QCEW_2020_Sum <- filter(QCEW_2020, agglvl_code == 75)
QCEW_2020_Sum %<>% filter(own_code == 5)
QCEW_2020_Sum %<>% filter(industry_code != 999)
QCEW_2020_Sum %<>% filter(disclosure_code != "N")
QCEW_2020_Sum %<>% subset(select = c(area_fips, industry_code, annual_avg_estabs, annual_avg_emplvl, total_annual_wages, avg_annual_pay))
QCEW_2020_Sum %<>% rename(place = area_fips, 
                          naics = industry_code)

# Add empty missing counties to maintain consistent dimensionality across hierarchical specifications
QCEW_2020_miss <- c("13265", "17151", "31005", "31009", "31091", "31113", "31115", "31171", "35021", "38065", "38085", "41069", "46017", "46095", "46137")
for (i in 1:length(QCEW_2020_miss)){
  QCEW_2020_Sum %<>% add_row(place = QCEW_2020_miss[i], naics = "111" )
}



# Generate a cross walk to transform 3-level NAICS  used by CBP into BEA Summary level industries 
QCEW_2020_Sum_XBEA <- QCEW_2020_Sum
QCEW_2020_Sum_XBEA %<>% reshape(idvar = "naics", timevar = "place", direction = "wide")
QCEW_2020_Sum_XBEA %<>% arrange(naics)
QCEW_2020_Sum_XBEA[is.na(QCEW_2020_Sum_XBEA)] <- 0
QCEW_2020_Sum_XBEA %<>% as.data.frame() %>% dplyr::arrange(naics)
QCEW_2020_Sum_XBEA %<>% t() %>%  as.data.frame()
QCEW_2020_Sum_XBEA[] <- lapply(QCEW_2020_Sum_XBEA, function(x) as.numeric(as.character(x)))


QCEW_2020_Sum_XBEA %<>% mutate("111CA" = V1 + V2)
QCEW_2020_Sum_XBEA %<>% mutate("113FF" = V3 + V4 + V5)
QCEW_2020_Sum_XBEA %<>% mutate("211" = V6)
QCEW_2020_Sum_XBEA %<>% mutate("212" = V7)
QCEW_2020_Sum_XBEA %<>% mutate("213" = V8)
QCEW_2020_Sum_XBEA %<>% mutate("22" = V9)
QCEW_2020_Sum_XBEA %<>% mutate("23" = V10 + V11 + V12)
QCEW_2020_Sum_XBEA %<>% mutate("311FT" = V13 + V14)
QCEW_2020_Sum_XBEA %<>% mutate("313TT" = V15 + V16)
QCEW_2020_Sum_XBEA %<>% mutate("315AL" = V17 + V18)
QCEW_2020_Sum_XBEA %<>% mutate("321" = V19)
QCEW_2020_Sum_XBEA %<>% mutate("322" = V20)
QCEW_2020_Sum_XBEA %<>% mutate("323" = V21)
QCEW_2020_Sum_XBEA %<>% mutate("324" = V22)
QCEW_2020_Sum_XBEA %<>% mutate("325" = V23)
QCEW_2020_Sum_XBEA %<>% mutate("326" = V24)
QCEW_2020_Sum_XBEA %<>% mutate("327" = V25)
QCEW_2020_Sum_XBEA %<>% mutate("331" = V26)
QCEW_2020_Sum_XBEA %<>% mutate("332" = V27)
QCEW_2020_Sum_XBEA %<>% mutate("333" = V28)
QCEW_2020_Sum_XBEA %<>% mutate("334" = V29)
QCEW_2020_Sum_XBEA %<>% mutate("335" = V30)
QCEW_2020_Sum_XBEA %<>% mutate("336XX" = V31)
QCEW_2020_Sum_XBEA %<>% mutate("337" = V32)
QCEW_2020_Sum_XBEA %<>% mutate("339" = V33)
QCEW_2020_Sum_XBEA %<>% mutate("42" = V34 + V35 + V36)
QCEW_2020_Sum_XBEA %<>% mutate("441" = V37)
QCEW_2020_Sum_XBEA %<>% mutate("445" = V41)
QCEW_2020_Sum_XBEA %<>% mutate("452" = V46)
QCEW_2020_Sum_XBEA %<>% mutate("4A0" = V38 + V39 + V40 + V42 + V43 + V44 + V45 + V47 + V48)
QCEW_2020_Sum_XBEA %<>% mutate("481" = V49)
QCEW_2020_Sum_XBEA %<>% mutate("482" = V50)
QCEW_2020_Sum_XBEA %<>% mutate("483" = V51)
QCEW_2020_Sum_XBEA %<>% mutate("484" = V52)
QCEW_2020_Sum_XBEA %<>% mutate("485" = V53)
QCEW_2020_Sum_XBEA %<>% mutate("486" = V54)
QCEW_2020_Sum_XBEA %<>% mutate("487OS" = V55 + V56 + V58)
QCEW_2020_Sum_XBEA %<>% mutate("493" = V59)
QCEW_2020_Sum_XBEA %<>% mutate("511" = V60)
QCEW_2020_Sum_XBEA %<>% mutate("512" = V61)
QCEW_2020_Sum_XBEA %<>% mutate("513" = V62 + V63)
QCEW_2020_Sum_XBEA %<>% mutate("514" = V64 + V65)
QCEW_2020_Sum_XBEA %<>% mutate("521CI" = V66 + V67)
QCEW_2020_Sum_XBEA %<>% mutate("523" = V68)
QCEW_2020_Sum_XBEA %<>% mutate("524" = V69)
QCEW_2020_Sum_XBEA %<>% mutate("525" = V70)
QCEW_2020_Sum_XBEA %<>% mutate("HSOREXX" = V71)
QCEW_2020_Sum_XBEA %<>% mutate("532RL" = V72 + V73)
QCEW_2020_Sum_XBEA %<>% mutate("541XX" = V74)
QCEW_2020_Sum_XBEA %<>% mutate("55" = V75)
QCEW_2020_Sum_XBEA %<>% mutate("561" = V76)
QCEW_2020_Sum_XBEA %<>% mutate("562" = V77)
QCEW_2020_Sum_XBEA %<>% mutate("61" = V78)
QCEW_2020_Sum_XBEA %<>% mutate("621" = V79)
QCEW_2020_Sum_XBEA %<>% mutate("622" = V80)
QCEW_2020_Sum_XBEA %<>% mutate("623" = V81)
QCEW_2020_Sum_XBEA %<>% mutate("624" = V82)
QCEW_2020_Sum_XBEA %<>% mutate("711AS" = V83 + V84)
QCEW_2020_Sum_XBEA %<>% mutate("713" = V85)
QCEW_2020_Sum_XBEA %<>% mutate("721" = V86)
QCEW_2020_Sum_XBEA %<>% mutate("722" = V87)
QCEW_2020_Sum_XBEA %<>% mutate("81" = V88 + V89 + V90 + V91)


QCEW_2020_Sum_XBEA %<>% subset(select = -c(V1:V91)) %>% slice(-c(1)) %>% t()

BEA_Summary <- row.names(QCEW_2020_Sum_XBEA)

QCEW_2020_Sum_XBEA <- cbind(BEA_Summary, QCEW_2020_Sum_XBEA) %>% as.data.frame()
QCEW_2020_Sum_XBEA %<>% reshape(idvar = "BEA_Summary", varying = c(colnames(QCEW_2020_Sum_XBEA)[-1]), direction = "long")
rownames(QCEW_2020_Sum_XBEA) <- 1:nrow(QCEW_2020_Sum_XBEA)
names(QCEW_2020_Sum_XBEA)[names(QCEW_2020_Sum_XBEA)=="time"] <- "place"
QCEW_2020_Sum_XBEA$place  %<>% formatC(width = 5, format = "d", flag = "0")
QCEW_2020_Sum_XBEA$annual_avg_estabs <-  as.numeric(QCEW_2020_Sum_XBEA$annual_avg_estabs)
QCEW_2020_Sum_XBEA$annual_avg_emplvl <-  as.numeric(QCEW_2020_Sum_XBEA$annual_avg_emplvl)
QCEW_2020_Sum_XBEA$total_annual_wages <-  as.numeric(QCEW_2020_Sum_XBEA$total_annual_wages)
QCEW_2020_Sum_XBEA$avg_annual_pay <-  as.numeric(QCEW_2020_Sum_XBEA$avg_annual_pay)



# Process and parse hierarchical structure  of CBP data
CBP_2019 %<>% subset(select = c(fipstate, fipscty, place, naics, emp, qp1, ap, est))
CBP_2019_C <- filter(CBP_2019, naics == "------")
CBP_2019_places <- CBP_2019_C[, 1:2]
CBP_2019_places$place <- paste0(CBP_2019_places$fipstate, CBP_2019_places$fipscty)
CBP_2019_Sector <- CBP_2019 %>% filter(grepl('*----', naics) & naics != '------' )
####  Note:  Six counties (30069, 31007, 31117, 32009, 48033, 48301)  do not have Sector level naicscodes  only top level "------" in 2019
CBP_2019_places_Sector <- as.data.frame(unique(CBP_2019_Sector$place)) 
names(CBP_2019_places_Sector) <- "place"
CBP_2019_Subsector <- CBP_2019 %>% filter(grepl('///', naics))
CBP_2019_IndustryGroup <- CBP_2019 %>% filter(grepl('//', naics) & !grepl('///', naics))
CBP_2019_NAICSIndustry <- CBP_2019 %>% filter(grepl('/', naics) & !grepl('///', naics)  & !grepl('//', naics))
CBP_2019_USNAICS <- CBP_2019 %>% filter(!grepl('/', naics) & !grepl('-', naics))



# Generate a cross walk to transform NAICS sectors used by CBP into BEA sectors 
CBP_2019_Sector_XBEA <- CBP_2019_Sector
CBP_2019_Sector_XBEA$place <- paste0(CBP_2019_Sector_XBEA$fipstate, CBP_2019_Sector_XBEA$fipscty)
CBP_2019_Sector_XBEA %<>% subset(select = c(place, naics, emp, qp1, ap, est))
CBP_2019_Sector_XBEA$naics %<>% substr(0,2) %>% as.numeric()

CBP_2019_Sector_XBEA %<>% reshape(idvar = "naics", timevar = "place", direction = "wide")
CBP_2019_Sector_XBEA[is.na(CBP_2019_Sector_XBEA)] <- 0
CBP_2019_Sector_XBEA %<>%  as.data.frame() %>% dplyr::arrange(naics)

CBP_2019_Sector_XBEA %<>% t() %>%  as.data.frame()
CBP_2019_Sector_XBEA %<>% mutate("11" = V1)
CBP_2019_Sector_XBEA %<>% mutate("21" = V2)
CBP_2019_Sector_XBEA %<>% mutate("22" = V3)
CBP_2019_Sector_XBEA %<>% mutate("23" = V4)
CBP_2019_Sector_XBEA %<>% mutate("31G" = V5)
CBP_2019_Sector_XBEA %<>% mutate("42" = V6)
CBP_2019_Sector_XBEA %<>% mutate("44RT" = V7)
CBP_2019_Sector_XBEA %<>% mutate("48WT" = V8)
CBP_2019_Sector_XBEA %<>% mutate("51" = V9)
CBP_2019_Sector_XBEA %<>% mutate("FIRE" = V10 + V11)
CBP_2019_Sector_XBEA %<>% mutate("PROF" = V12 + V13 + V14)
CBP_2019_Sector_XBEA %<>% mutate("6" = V15 + V16)
CBP_2019_Sector_XBEA %<>% mutate("7" = V17 + V18)
CBP_2019_Sector_XBEA %<>% mutate("81" = V19)
CBP_2019_Sector_XBEA %<>% mutate("G" = V20)
CBP_2019_Sector_XBEA %<>% subset(select = -c(V1:V20)) %>% slice(-c(1)) %>% t()
BEA_Sectors <- row.names(CBP_2019_Sector_XBEA)
CBP_2019_Sector_XBEA <- cbind(BEA_Sectors, CBP_2019_Sector_XBEA) %>% as.data.frame()
CBP_2019_Sector_XBEA %<>% reshape(idvar = "BEA_Sectors", varying = c(colnames(CBP_2019_Sector_XBEA)[-1]), direction = "long")
rownames(CBP_2019_Sector_XBEA) <- 1:nrow(CBP_2019_Sector_XBEA)
names(CBP_2019_Sector_XBEA)[names(CBP_2019_Sector_XBEA)=="time"] <- "place"
CBP_2019_Sector_XBEA$place  %<>% formatC(width = 5, format = "d", flag = "0")
CBP_2019_Sector_XBEA$emp <-  as.numeric(CBP_2019_Sector_XBEA$emp)
CBP_2019_Sector_XBEA$qp1 <-  as.numeric(CBP_2019_Sector_XBEA$qp1)
CBP_2019_Sector_XBEA$ap <-  as.numeric(CBP_2019_Sector_XBEA$ap)
CBP_2019_Sector_XBEA$est <-  as.numeric(CBP_2019_Sector_XBEA$est)





# Parse TIGER and CBP 2019 county overlap
TIGER_CBP <- inner_join(TIGERData, CBP_2019_places_Sector, by = "place")
TIGER_CBP  <- TIGER_CBP[order(TIGER_CBP$place), ]
rownames(TIGER_CBP) <- TIGER_CBP$place



#Parse TIGER/CBP and RUCC crosswalk
TIGER_CBP_RUCC <- inner_join(TIGER_CBP, RUCCData, by = "place")
rownames(TIGER_CBP_RUCC) <- TIGER_CBP_RUCC$place
# add augmented hierarchical classification 

TIGER_CBP_RUCC <- transform(TIGER_CBP_RUCC, H3 = ifelse(RUCC_2013 %in% 1:3, 1, ifelse(RUCC_2013 %in% 4:6, 2, ifelse(RUCC_2013%in% 7:9, 3, 0)  ) ) ) 



# Produce Distance Matrix
Dist_mat <- TIGER_CBP_RUCC$center %>% as_Spatial() %>% distm()
rownames(Dist_mat) = colnames(Dist_mat) <- TIGER_CBP_RUCC$place


## Produce Proximity Matrix
Prox_mat <- poly2nb(TIGER_CBP_RUCC, queen = TRUE) %>% nb2mat(style = "B", zero.policy = TRUE)
colnames(Prox_mat) <- rownames(Prox_mat)




# Parse TIGER and QCED crosswalk
QCEW_2020_places_Summary <- as.data.frame(unique(QCEW_2020_Sum_XBEA$place)) 
names(QCEW_2020_places_Summary) <- "place"
TIGER_QCEW <- inner_join(TIGERData, QCEW_2020_places_Summary, by = "place")
TIGER_QCEW  <- TIGER_CBP[order(TIGER_CBP$place), ]
rownames(TIGER_QCEW) <- TIGER_CBP$place


#Parse TIGER/CBP and RUCC crosswalk
TIGER_QCEW_RUCC <- inner_join(TIGER_QCEW, RUCCData, by = "place")
rownames(TIGER_QCEW_RUCC) <- TIGER_QCEW_RUCC$place
# add augmented hierarchical classification 
TIGER_QCEW_RUCC <- transform(TIGER_QCEW_RUCC, H3 = ifelse(RUCC_2013 %in% 1:3, 1, ifelse(RUCC_2013 %in% 4:6, 2, ifelse(RUCC_2013%in% 7:9, 3, 0)  ) ) )







# Import CBP data using separate Python pre-processing (If trouble restart R, load reticulate, load conda env "rurec")
# library(reticulate)
# use_condaenv('rurec')
# cbp <- import('rurec.cbp')
# regional_data_py <- cbp$get_df("state", 2019L)
# head(regional_data_py)
# 
# CBP <- cbp$get_df("county", 2019L)
# CBP %<>% filter(industry != "-")

## Detail level CBP processing 
# CBP_detail <- filter(CBP_2019, naics != "------") 
# CBP_detail$naics <- as.numeric(regmatches(CBP_detail$naics, gregexpr("[0-9.]+", CBP_detail$naics)))
# AllNaics <- unique(CBP_detail$naics) %>% as.vector() %>% sort()
# CBP_detail <- filter(CBP_detail, naics %in% unique(Naics_D))
# 
# CBP_detail %<>% reshape(idvar = "naics", timevar = "place", direction = "wide")
# CBP_detail[is.na(CBP_detail)] <- 0
# CBP_detail %<>%  as.data.frame() %>% dplyr::arrange(naics) 
# CBP_detail %<>% t() %>%  as.data.frame()
# CBP_detail[1,] <- as.numeric(CBP_detail[1,] )









