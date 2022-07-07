# Clean imported outside data

# Load and attach necessary packages
library(rprojroot)
library(dplyr)
library(geosphere)
library(spdep)
library(rlog)

# Display start time
log_info("Define data clean start")

# Connect  and  parse  code  from  another  file 
source(file.path(find_rstudio_root_file(), "nbs", "rural_typology_r_data_import.R"))

data_dir = file.path(find_rstudio_root_file(), "data", "robjs")


# QCEW  2020  subseted  and  cleaned  into  private county  level  observations  and  pertinent  variables
if (!file.exists(file.path(data_dir, "QCEW_2020_Sum"))){
  QCEW_2020_Sum <- file.path(data_dir, "QCEW_2020") %>% readRDS() 
  QCEW_2020_Sum %<>% filter(agglvl_code == 75)
  QCEW_2020_Sum %<>% filter(own_code == 5)
  QCEW_2020_Sum %<>% filter(industry_code != 999)
  QCEW_2020_Sum %<>% filter(disclosure_code != "N")
  QCEW_2020_Sum %<>% filter(!grepl("^72" , area_fips))
  QCEW_2020_Sum %<>% filter(!grepl("^78" , area_fips))
  QCEW_2020_Sum %<>% subset(select = c(area_fips, industry_code, annual_avg_estabs, annual_avg_emplvl, total_annual_wages, avg_annual_pay))
  QCEW_2020_Sum %<>% rename(place = area_fips, 
                            naics = industry_code)
saver(QCEW_2020_Sum)
rm(QCEW_2020_Sum)
}

# # Add  empty  missing  counties  to  maintain  consistent  dimensionality  across  hierarchical  specifications
# QCEW_2020_miss <- c("13265", "17151", "31005", "31009", "31091", "31113", "31115", "31171", "35021", "38065", "38085", "41069", "46017", "46095", "46137")
# for (i in  1:length(QCEW_2020_miss)){
#   QCEW_2020_Sum %<>% add_row(place = QCEW_2020_miss[i], naics = "111" )
# }



# Generate  a  cross  walk  to  transform  3-level  NAICS  used  by  CBP  into  BEA  Summary  level  industries 
if (!file.exists(file.path(data_dir, "QCEW_2020_Sum_XBEA"))){
  QCEW_2020_Sum_XBEA <- file.path(data_dir, "QCEW_2020_Sum") %>% readRDS() %>% reshape(idvar = "naics", timevar = "place", direction = "wide")
  QCEW_2020_Sum_XBEA %<>% arrange(naics)
  QCEW_2020_Sum_XBEA[is.na(QCEW_2020_Sum_XBEA)] <- 0
  QCEW_2020_Sum_XBEA %<>% as.data.frame() %>% dplyr::arrange(naics)
  QCEW_2020_Sum_XBEA %<>% t() %>%  as.data.frame()
  QCEW_2020_Sum_XBEA[] <- lapply(QCEW_2020_Sum_XBEA, function(x) as.numeric(as.character(x)))
  colnames(QCEW_2020_Sum_XBEA) <- file.path(data_dir, "QCEW_2020_Sum") %>% readRDS() %>% .$naics %>% unique() %>% sort()  %>% paste0("///")
  
  QCEW_2020_Sum_XBEA %<>% mutate("111CA" = `111///` + `112///`)
  QCEW_2020_Sum_XBEA %<>% mutate("113FF" = `113///` + `114///` + `115///`)
  QCEW_2020_Sum_XBEA %<>% mutate("211" = `211///`)
  QCEW_2020_Sum_XBEA %<>% mutate("212" = `212///`)
  QCEW_2020_Sum_XBEA %<>% mutate("213" = `213///`)
  QCEW_2020_Sum_XBEA %<>% mutate("22" = `221///`)
  QCEW_2020_Sum_XBEA %<>% mutate("23" = `236///` + `237///` + `238///`)
  QCEW_2020_Sum_XBEA %<>% mutate("311FT" = `311///` + `312///`)
  QCEW_2020_Sum_XBEA %<>% mutate("313TT" = `313///` + `314///`)
  QCEW_2020_Sum_XBEA %<>% mutate("315AL" = `315///` + `316///`)
  QCEW_2020_Sum_XBEA %<>% mutate("321" = `321///`)
  QCEW_2020_Sum_XBEA %<>% mutate("322" = `322///`)
  QCEW_2020_Sum_XBEA %<>% mutate("323" = `323///`)
  QCEW_2020_Sum_XBEA %<>% mutate("324" = `324///`)
  QCEW_2020_Sum_XBEA %<>% mutate("325" = `325///`)
  QCEW_2020_Sum_XBEA %<>% mutate("326" = `326///`)
  QCEW_2020_Sum_XBEA %<>% mutate("327" = `327///`)
  QCEW_2020_Sum_XBEA %<>% mutate("331" = `331///`)
  QCEW_2020_Sum_XBEA %<>% mutate("332" = `332///`)
  QCEW_2020_Sum_XBEA %<>% mutate("333" = `333///`)
  QCEW_2020_Sum_XBEA %<>% mutate("334" = `334///`)
  QCEW_2020_Sum_XBEA %<>% mutate("335" = `335///`)
  QCEW_2020_Sum_XBEA %<>% mutate("336XX" = `336///`)
  QCEW_2020_Sum_XBEA %<>% mutate("337" = `337///`)
  QCEW_2020_Sum_XBEA %<>% mutate("339" = `339///`)
  QCEW_2020_Sum_XBEA %<>% mutate("42" = `423///` + `424///` + `425///`)
  QCEW_2020_Sum_XBEA %<>% mutate("441" = `441///`)
  QCEW_2020_Sum_XBEA %<>% mutate("445" = `445///`)
  QCEW_2020_Sum_XBEA %<>% mutate("452" = `452///`)
  QCEW_2020_Sum_XBEA %<>% mutate("4A0" = `442///` + `443///` + `444///` + `446///` + `447///` + `448///` + `451///` + `453///` + `454///`)
  QCEW_2020_Sum_XBEA %<>% mutate("481" = `481///`)
  QCEW_2020_Sum_XBEA %<>% mutate("482" = `482///`)
  QCEW_2020_Sum_XBEA %<>% mutate("483" = `483///`)
  QCEW_2020_Sum_XBEA %<>% mutate("484" = `484///`)
  QCEW_2020_Sum_XBEA %<>% mutate("485" = `485///`)
  QCEW_2020_Sum_XBEA %<>% mutate("486" = `486///`)
  QCEW_2020_Sum_XBEA %<>% mutate("487OS" = `487///` + `488///` + `492///`)
  QCEW_2020_Sum_XBEA %<>% mutate("493" = `493///`)
  QCEW_2020_Sum_XBEA %<>% mutate("511" = `511///`)
  QCEW_2020_Sum_XBEA %<>% mutate("512" = `512///`)
  QCEW_2020_Sum_XBEA %<>% mutate("513" = `515///` + `517///`)
  QCEW_2020_Sum_XBEA %<>% mutate("514" = `518///` + `519///`)
  QCEW_2020_Sum_XBEA %<>% mutate("521CI" = `521///` + `522///`)
  QCEW_2020_Sum_XBEA %<>% mutate("523" = `523///`)
  QCEW_2020_Sum_XBEA %<>% mutate("524" = `524///`)
  QCEW_2020_Sum_XBEA %<>% mutate("525" = `525///`)
  QCEW_2020_Sum_XBEA %<>% mutate("HSOREXX" = `531///`)
  QCEW_2020_Sum_XBEA %<>% mutate("532RL" = `532///` + `533///`)
  QCEW_2020_Sum_XBEA %<>% mutate("541XX" = `541///`)
  QCEW_2020_Sum_XBEA %<>% mutate("55" = `551///`)
  QCEW_2020_Sum_XBEA %<>% mutate("561" = `561///`)
  QCEW_2020_Sum_XBEA %<>% mutate("562" = `562///`)
  QCEW_2020_Sum_XBEA %<>% mutate("61" = `611///`)
  QCEW_2020_Sum_XBEA %<>% mutate("621" = `621///`)
  QCEW_2020_Sum_XBEA %<>% mutate("622" = `622///`)
  QCEW_2020_Sum_XBEA %<>% mutate("623" = `623///`)
  QCEW_2020_Sum_XBEA %<>% mutate("624" = `624///`)
  QCEW_2020_Sum_XBEA %<>% mutate("711AS" = `711///` + `712///`)
  QCEW_2020_Sum_XBEA %<>% mutate("713" = `713///`)
  QCEW_2020_Sum_XBEA %<>% mutate("721" = `721///`)
  QCEW_2020_Sum_XBEA %<>% mutate("722" = `722///`)
  QCEW_2020_Sum_XBEA %<>% mutate("81" = `811///` + `812///` + `813///` + `814///`)
  
  QCEW_2020_Sum_XBEA %<>% subset(select = -c(1:91)) %>% slice(-c(1)) %>% t()
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
saver(QCEW_2020_Sum_XBEA)
rm(QCEW_2020_Sum_XBEA)
}

log_info("BEA/QCEW 2020 Summary complete")

# Process  and  parse  hierarchical  structure  of  CBP  data
if (!file.exists(file.path(data_dir, "CBP_2019_C"))){
  CBP_2019_C <- file.path(data_dir, "CBP_2019") %>% readRDS() %>% subset(select = c(fipstate, fipscty, place, naics, emp, qp1, ap, est)) %>% filter(naics == "------")
saver(CBP_2019_C)
rm(CBP_2019_C)
}

if (!file.exists(file.path(data_dir, "CBP_2019_places"))){
  CBP_2019_places <- file.path(data_dir, "CBP_2019_C") %>% readRDS() %>% .[, 1:2]
  CBP_2019_places$place <- paste0(CBP_2019_places$fipstate, CBP_2019_places$fipscty)
saver(CBP_2019_places)
rm(CBP_2019_places)
}

if (!file.exists(file.path(data_dir, "CBP_2019_Sector"))){
  CBP_2019_Sector <- file.path(data_dir, "CBP_2019") %>% readRDS() %>% subset(select = c(fipstate, fipscty, place, naics, emp, qp1, ap, est)) %>% filter(grepl('*----', naics) & naics != '------' )
saver(CBP_2019_Sector)
rm(CBP_2019_Sector)
}
####  Note:  Six  counties (30069, 31007, 31117, 32009, 48033, 48301)  do,  not,  have,  Sector,  level,  naicscodes,  only,  top,  level "------" in,  2019

if (!file.exists(file.path(data_dir, "CBP_2019_places_Sector"))){
  CBP_2019_places_Sector <- file.path(data_dir, "CBP_2019_Sector") %>% readRDS()  %>% .$place %>% unique()  %>%  as.data.frame() 
  names(CBP_2019_places_Sector) <- "place"
saver(CBP_2019_places_Sector)
rm(CBP_2019_places_Sector)
}

if (!file.exists(file.path(data_dir, "CBP_2019_Subsector"))){
  CBP_2019_Subsector <- file.path(data_dir, "CBP_2019") %>% readRDS() %>% subset(select = c(fipstate, fipscty, place, naics, emp, qp1, ap, est)) %>% filter(grepl('///', naics))
saver(CBP_2019_Subsector)
rm(CBP_2019_Subsector)
}

if (!file.exists(file.path(data_dir, "CBP_2019_IndustryGroup"))){
  CBP_2019_IndustryGroup <- file.path(data_dir, "CBP_2019") %>% readRDS() %>% subset(select = c(fipstate, fipscty, place, naics, emp, qp1, ap, est)) %>% filter(grepl('//', naics) & !grepl('///', naics))
saver(CBP_2019_IndustryGroup)
rm(CBP_2019_IndustryGroup)
}

if (!file.exists(file.path(data_dir, "CBP_2019_NAICSIndustry"))){
  CBP_2019_NAICSIndustry <- file.path(data_dir, "CBP_2019") %>% readRDS() %>% subset(select = c(fipstate, fipscty, place, naics, emp, qp1, ap, est)) %>% filter(grepl('/', naics) & !grepl('///', naics)  & !grepl('//', naics))
saver(CBP_2019_NAICSIndustry)
rm(CBP_2019_NAICSIndustry)
}

if (!file.exists(file.path(data_dir, "CBP_2019_USNAICS"))){
  CBP_2019_USNAICS <- file.path(data_dir, "CBP_2019") %>% readRDS() %>% subset(select = c(fipstate, fipscty, place, naics, emp, qp1, ap, est)) %>% filter(!grepl('/', naics) & !grepl('-', naics))
saver(CBP_2019_USNAICS)
rm(CBP_2019_USNAICS)
}

# Generate  a  cross  walk  to  transform  NAICS  sectors  used  by  CBP  into  BEA  sectors 
if (!file.exists(file.path(data_dir, "CBP_2019_Sector_XBEA"))){
  CBP_2019_Sector_XBEA <- file.path(data_dir, "CBP_2019_Sector") %>% readRDS()
  CBP_2019_Sector_XBEA$place <- paste0(CBP_2019_Sector_XBEA$fipstate, CBP_2019_Sector_XBEA$fipscty)
  CBP_2019_Sector_XBEA %<>% subset(select = c(place, naics, emp, qp1, ap, est))
  CBP_2019_Sector_XBEA$naics %<>% substr(0,2) %>% as.numeric()
  
  CBP_2019_Sector_XBEA %<>% reshape(idvar = "naics", timevar = "place", direction = "wide")
  CBP_2019_Sector_XBEA[is.na(CBP_2019_Sector_XBEA)] <- 0
  CBP_2019_Sector_XBEA %<>%  as.data.frame() %>% dplyr::arrange(naics)
  CBP_2019_Sector_XBEA %<>% t() %>%  as.data.frame()
  colnames(CBP_2019_Sector_XBEA) <- file.path(data_dir, "CBP_2019_Sector") %>% readRDS() %>% .$naics %>% unique() %>% sort()
  
  CBP_2019_Sector_XBEA %<>% mutate("11" = `11----`)
  CBP_2019_Sector_XBEA %<>% mutate("21" = `21----`)
  CBP_2019_Sector_XBEA %<>% mutate("22" = `22----`)
  CBP_2019_Sector_XBEA %<>% mutate("23" = `23----`)
  CBP_2019_Sector_XBEA %<>% mutate("31G" = `31----`)
  CBP_2019_Sector_XBEA %<>% mutate("42" = `42----`)
  CBP_2019_Sector_XBEA %<>% mutate("44RT" = `44----`)
  CBP_2019_Sector_XBEA %<>% mutate("48WT" = `48----`)
  CBP_2019_Sector_XBEA %<>% mutate("51" = `51----`)
  CBP_2019_Sector_XBEA %<>% mutate("FIRE" = `52----` + `53----`)
  CBP_2019_Sector_XBEA %<>% mutate("PROF" = `54----` + `55----` + `56----`)
  CBP_2019_Sector_XBEA %<>% mutate("6" = `61----` + `62----`)
  CBP_2019_Sector_XBEA %<>% mutate("7" = `71----` + `72----`)
  CBP_2019_Sector_XBEA %<>% mutate("81" = `81----`)
  CBP_2019_Sector_XBEA %<>% mutate("G" = `99----`)
  CBP_2019_Sector_XBEA %<>% subset(select = -c(1:20)) %>% slice(-c(1)) %>% t()
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
saver(CBP_2019_Sector_XBEA)
rm(CBP_2019_Sector_XBEA)
}
log_info("BEA/CBP 2019 Sector complete")

### All unique "related NAICS codes" from BEA industry code detail level economic accounts
Naics_Det = c(11111,  11112,  11113,  11114,  11115,  11116,  11119,   1112,   1113,   1114,   1119,  11212,  11211,  11213,   1123,   1122,   1124,   1125,   1129, 113, 114, 115,
              211,   2121,  21223,  21221,  21222,  21229,  21231,  21232,  21239, 213111, 213112, 213113, 213114, 213115, 2211,   2212,   2213, 23,
              311111, 311119,  31121, 311221, 311224, 311225,  31123,   3113,  31141,  31142, 311513, 311514, 311511, 311512, 31152, 311615, 
              311611, 311612, 311613,   3117,  31181,  31182,  31183,  31191,  31192,  31193,  31194, 31199,  
              31211,  31212, 31213,  31214,   3122,   3131,   3132,   3133,  31411,  31412,   3149, 315, 316,   
              3211,   3212,  32191,  32192, 32193,  32194,  32195,  32196,  32197,  32198,  32199,  32211,  32212,  32213,  32221,  32222,  32223,  322291,  322299,  
              32311,  32312,  32411,  324121,  324122, 32419,  32511,  32512,  32513,  32518,  32519,  325211,  325212,  32522,  32531,  32532,  
              325411, 325412,  325413,  325414,  32551,  32552,  32561,  32562,  32591,  32592,  32599,  
              32611,  32612,  32613,  32614,  32615,  32616,  32619,  32621,  32622,  32629,  3271,  3272,  32731,  32732,  32733,  32739,  3274,  32791,  327991,  327992,  327993,  327999, 
              3311,  3312,  331313,  331314,  331315,  331318,  331410,  33142,  33149,  33151,  33152,  332114,  332119,  332111,  332112,  332117,  3322,  33231,  33232,  33241,  33242,  33243,  
              3325, 3326,  33271,  33272, 3328,  332913,  332911,  332912,  332919,  332991, 332996,  332999,  332992,  332993,  332994,  
              333111,  333112,  33312,  33313,  333242,  333241,  333243,  333244,  333249,  333314,  333316,  333318,  333413,  333414,  333415,  333511,  333514,  333517,  333515,  333519,  
              333611,  333612,  333613,  333618,  333912,  333911,  333913, 33392,  333991,  333993,  333994,  333992,  333997,  333999,  333995,  333996,  
              334111,  334112,  334118,  33421,  33422,  33429,  3343,  334413,  334418,  334412,  334416,  334417,  334419,  334510,  334511,  334512,  334513,  334514,  334515,  334516,  334517,  334519,  33461, 
              33511,  33512,  33521,  335221,  335222,  335224,  335228,  335311,  335312,  335313,  335314,  335911,  335912,  33592,  33593,  335991,  335999,  
              336111,  336112,  33612,  336211,  336212,  336213,  336214,  33631,  33632,  33635,  33636,  33637,  33639,  33633,  33634, 
              336411,  336412,  336413,  336414,  336415,  336419,  3365,  336611,  336612,  336991,  336992,  336999,  
              33711,  337121,  337122,  337127,  337124,  337125,  337215,  337211,  337212,  337214,  3379,  339112,  339113,  339114,  339115,  339116,  33991,  33992,  33993,  33994, 33995,  33999, 
              4231,  4234,  4236,  4238,  4232,  4233,  4235,  4237,  4239,  4242,  4244,  4247,  4241,  4243,  4245,  4246,  4248,  4249,  425,
              441,  444,  445,  446,  447,  448,  452,  454,  481,  482,  483, 484,  485,  486,  487,  488,  491,  492,  493,  442,  443,  451,  453, 
              51111,  51112,  51113,  51114,  51119,  51121,  5121,  5122,  5151,  5152,  5171,  5172,  5174,  5175,  5176,  5177,  5178,  5179,  5182,  51913, 51911,  51912,  51919,  
              5222,  5223,  5239,  5231,  5232,  524113,  524114,  52412,  52413,  5242,  525,  521,  5221,  531,  5321,  5324,  5322,  5323,  533,  5411,  5412,  5413,  5414,  
              541511,  541512,  541513,  541519,  54161, 54162,  54169,  5417,  5418,  54192,  54194,  54191,  54193,  54199,  55,  5611,  5612,  5613,  5614,  5615,  5616,  5617,  5619,  562,  
              6111,  6112,  6113,  6114,  6115,  6116,  6117,  6211,  6212,  6213,  6214,  6215,  6216, 6219,  622,  6231,  6233,  6232,  6239,  6241,  6244,  6242,  6243,
              7111,  7112,  7115,  7113,  7114,  712,  7131,  7132,  7139,  721,  722511,  722513,  7223,  7224,  722514,  722515,  
              8111,  8112,  8113,  8114,  8121,  8122, 8123,  8129,  8131,  8132,  8133,  8134,  8139,  814)


## Structure code values to match CBP syntax
Naics_Det[nchar(Naics_Det) == 2] %<>% paste0("----")
Naics_Det %<>% paste0("////") %>% substr(1,6)

## Detail level CBP processing 
if (!file.exists(file.path(data_dir, "CBP_2019_Detail"))){
  CBP_2019_Detail <- file.path(data_dir, "CBP_2019") %>% readRDS() %>% subset(select = c(fipstate, fipscty, place, naics, emp, qp1, ap, est))  %>% filter(naics %in% Naics_Det)
saver(CBP_2019_Detail)
rm(CBP_2019_Detail)
}

### Data loss ratio test example 
#sum(CBP_2019_Detail$emp) / sum(filter(CBP_2019, naics == '------')$emp)

if (!file.exists(file.path(data_dir, "CBP_2019_Detail_XBEA"))){
  CBP_2019_Detail_XBEA <- file.path(data_dir, "CBP_2019_Detail") %>% readRDS() %>% subset(select = c(place, naics, emp, qp1, ap, est))
  CBP_2019_Detail_XBEA %<>% reshape(idvar = "naics", timevar = "place", direction = "wide")
  CBP_2019_Detail_XBEA %<>%  as.data.frame() %>% dplyr::arrange(naics)
  CBP_2019_Detail_XBEA[is.na(CBP_2019_Detail_XBEA)] <- 0
  CBP_2019_Detail_XBEA <- CBP_2019_Detail_XBEA[,-1]
  CBP_2019_Detail_XBEA %<>% t() %>%  as.data.frame()
  colnames(CBP_2019_Detail_XBEA) <- file.path(data_dir, "CBP_2019_Detail") %>% readRDS() %>% .$naics %>% unique() %>% sort()
  
  CBP_2019_Detail_XBEAt <- matrix(ncol = 0, nrow = dim(CBP_2019_Detail_XBEA)[1]) %>% as.data.frame()
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "113000" = `113///`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "114000" = `114///`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "115000" = `115///`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "211000" = `211///`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "212100" = `2121//`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "2122A0" = `21221/` + `21222/`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "212230" = `21223/`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "212310" = `21231/`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "2123A0" = `21232/` + `21239/`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "213111" = `213111`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "21311A" = `213112` + `213113` + `213114` + `213115`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "221100" = `2211//`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "221200" = `2212//`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "221300" = `2213//`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "23XX" = `23----`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "321100" = `3211//`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "321200" = `3212//`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "321910" = `32191/`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "3219A0" = `32192/` + `32199/`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "327100" = `3271//`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "327200" = `3272//`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "327310" = `32731/`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "327320" = `32732/`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "327330" = `32733/`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "327390" = `32739/`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "327400" = `3274//`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "327910" = `32791/`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "327991" = `327991`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "327992" = `327992`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "327993" = `327993`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "327999" = `327999`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "331110" = `3311//`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "331200" = `3312//`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "331313" = `331313`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "331314" = `331314`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "33131B" = `331315` + `331318`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "331410" = `331410`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "331420" = `33142/`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "331490" = `33149/`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "331510" = `33151/`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "331520" = `33152/`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "33211A" = `332111` + `332112` + `332117`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "332114" = `332114`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "332119" = `332119`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "332200" = `3322//`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "332310" = `33231/`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "332320" = `33232/`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "332410" = `33241/`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "332420" = `33242/`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "332430" = `33243/`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "332500" = `3325//`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "332600" = `3326//`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "332710" = `33271/`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "332720" = `33272/`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "332800" = `3328//`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "33291A" = `332911` + `332912` + `332919`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "332913" = `332913`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "332991" = `332991`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "33299A" = `332992` + `332994`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "332996" = `332996`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "332999" = `332999`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "333111" = `333111`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "333112" = `333112`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "333120" = `33312/`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "333130" = `33313/`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "333242" = `333242`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "33329A" = `333241` +`333243` +`333244` + `333249`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "333314" = `333314`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "333316" = `333316`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "333318" = `333318`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "333413" = `333413`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "333414" = `333414`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "333415" = `333415`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "333511" = `333511`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "333517" = `333517`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "333514" = `333514`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "33351B" = `333515` + `333519`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "333611" = `333611`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "333612" = `333612`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "333613" = `333613`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "333618" = `333618`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "333912" = `333912`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "333920" = `33392/`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "333991" = `333991`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "33399A" = `333992` + `333997` + `333999`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "333993" = `333993`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "333994" = `333994`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "33399B" = `333995` + `333996`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "334111" = `334111`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "334112" = `334112`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "334118" = `334118`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "334210" = `33421/`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "334220" = `33422/`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "334290" = `33429/`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "334413" = `334413`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "334418" = `334418`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "33441A" = `334412` + `334416` + `334417` + `334419`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "334510" = `334510`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "334511" = `334511`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "334512" = `334512`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "334513" = `334513`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "334514" = `334514`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "334515" = `334515`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "334516" = `334516`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "334517" = `334517`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "33451A" = `334519`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "334300" = `3343//`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "334610" = `33461/`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "335110" = `33511/`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "335120" = `33512/`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "335210" = `33521/`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "335311" = `335311`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "335312" = `335312`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "335313" = `335313`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "335314" = `335314`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "335911" = `335911`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "335912" = `335912`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "335920" = `33592/`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "335930" = `33593/`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "335991" = `335991`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "335999" = `335999`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "336111" = `336111`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "336112" = `336112`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "336120" = `33612/`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "336211" = `336211`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "336212" = `336212`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "336213" = `336213`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "336214" = `336214`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "336310" = `33631/`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "336320" = `33632/`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "3363A0" = `33633/` + `33634/`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "336350" = `33635/`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "336360" = `33636/`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "336370" = `33637/`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "336390" = `33639/`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "336411" = `336411`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "336412" = `336412`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "336413" = `336413`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "336414" = `336414`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "33641A" = `336415` + `336419`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "336500" = `3365//`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "336611" = `336611`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "336612" = `336612`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "336991" = `336991`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "336992" = `336992`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "336999" = `336999`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "337110" = `33711/`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "337121" = `337121`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "337122" = `337122`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "33712N" = `337124` + `337125`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "337127" = `337127`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "33721A" = `337211` + `337212` + `337214`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "337215" = `337215`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "337900" = `3379//`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "339112" = `339112`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "339113" = `339113`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "339114" = `339114`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "339115" = `339115`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "339116" = `339116`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "339910" = `33991/`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "339920" = `33992/`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "339930" = `33993/`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "339940" = `33994/`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "339950" = `33995/`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "339990" = `33999/`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "311111" = `311111`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "311119" = `311119`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "311210" = `31121/`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "311221" = `311221`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "311224" = `311224`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "311225" = `311225`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "311230" = `31123/`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "311300" = `3113//`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "311410" = `31141/`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "311420" = `31142/`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "31151A" = `311511` + `311512`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "311513" = `311513`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "311514" = `311514`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "311520" = `31152/`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "31161A" = `311611` + `311612` + `311613`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "311615" = `311615`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "311700" = `3117//`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "311810" = `31181/`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "3118A0" = `31182/` + `31183/`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "311910" = `31191/`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "311920" = `31192/`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "311930" = `31193/`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "311940" = `31194/`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "311990" = `31199/`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "312110" = `31211/`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "312120" = `31212/`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "312130" = `31213/`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "312140" = `31214/`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "312200" = `3122//`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "313100" = `3131//`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "313200" = `3132//`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "313300" = `3133//`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "314110" = `31411/`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "314120" = `31412/`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "314900" = `3149//`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "315000" = `315///`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "316000" = `316///`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "322120" = `32212/`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "322130" = `32213/`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "322210" = `32221/`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "322220" = `32222/`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "322230" = `32223/`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "322291" = `322291`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "322299" = `322299`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "323110" = `32311/`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "323120" = `32312/`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "324110" = `32411/`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "324121" = `324121`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "324122" = `324122`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "324190" = `32419/`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "325110" = `32511/`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "325120" = `32512/`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "325130" = `32513/`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "325180" = `32518/`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "325190" = `32519/`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "325211" = `325211`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "3252A0" = `325212` + `32522/`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "325411" = `325411`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "325412" = `325412`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "325413" = `325413`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "325414" = `325414`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "325310" = `32531/`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "325320" = `32532/`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "325510" = `32551/`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "325520" = `32552/`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "325610" = `32561/`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "325620" = `32562/`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "325910" = `32591/`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "3259A0" = `32592/` + `32599/`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "326110" = `32611/`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "326120" = `32612/`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "326130" = `32613/`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "326140" = `32614/`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "326150" = `32615/`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "326160" = `32616/`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "326190" = `32619/`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "326210" = `32621/`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "326220" = `32622/`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "326290" = `32629/`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "423100" = `4231//`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "423400" = `4234//`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "423600" = `4236//`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "423800" = `4238//`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "423A00" = `4232//` + `4233//` + `4235//` + `4237//` + `4239//`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "424200" = `4242//`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "424400" = `4244//`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "424700" = `4247//`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "424A00" = `4241//` +  `4243//` + `4245//` + `4246//` + `4248//` + `4249//`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "425000" = `425///`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "441000" = `441///`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "445000" = `445///`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "452000" = `452///`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "444000" = `444///`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "446000" = `446///`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "447000" = `447///`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "448000" = `448///`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "454000" = `454///`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "4B0000" = `442///`+ `443///` + `451///` + `453///`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "481000" = `481///`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "483000" = `483///`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "484000" = `484///`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "485000" = `485///`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "486000" = `486///`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "48A000" = `487///` + `488///`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "492000" = `492///`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "493000" = `493///`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "511110" = `51111/`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "511120" = `51112/`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "511130" = `51113/`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "5111A0" = `51114/` + `51119/`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "511200" = `51121/`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "512100" = `5121//`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "512200" = `5122//`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "515100" = `5151//`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "515200" = `5152//`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "517A00" = `5174//` + `5179//`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "518200" = `5182//`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "519130" = `51913/`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "5191A0" = `51911/` + `51912/` + `51919/`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "52A000" = `521///` + `5221//`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "522A00" = `5222//` + `5223//`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "523A00" = `5231//` + `5232//`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "523900" = `5239//`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "524113" = `524113`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "5241XX" = `524114` + `52412/` + `52413/`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "524200" = `5242//`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "525000" = `525///`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "531XX" = `531///`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "532100" = `5321//`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "532A00" = `5322//` + `5323//`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "532400" = `5324//`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "533000" = `533///`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "541100" = `5411//`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "541200" = `5412//`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "541300" = `5413//`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "541610" = `54161/`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "5416A0" = `54162/` + `54169/`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "541700" = `5417//`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "541800" = `5418//`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "541400" = `5414//`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "5419A0" = `54191/` + `54193/` + `54199/`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "541920" = `54192/`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "541940" = `54194/`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "541511" = `541511`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "541512" = `541512`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "54151A" = `541513` + `541519`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "550000" = `55----`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "561300" = `5613//`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "561700" = `5617//`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "561100" = `5611//`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "561200" = `5612//`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "561400" = `5614//`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "561500" = `5615//`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "561600" = `5616//`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "561900" = `5619//`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "562000" = `562///`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "611100" = `6111//`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "611A00" = `6112//` + `6113//`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "611B00" = `6114//` + `6115//` + `6116//` + `6117//`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "621100" = `6211//`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "621200" = `6212//`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "621300" = `6213//`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "621400" = `6214//`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "621500" = `6215//`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "621600" = `6216//`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "621900" = `6219//`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "622000" = `622///`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "623A00" = `6231//` + `6233//`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "623B00" = `6232//` + `6239//`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "624100" = `6241//`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "624A00" = `6242//` + `6243//`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "624400" = `6244//`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "711100" = `7111//`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "711200" = `7112//`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "711A00" = `7113//` + `7114//`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "711500" = `7115//`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "712000" = `712///`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "713100" = `7131//`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "713200" = `7132//`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "713900" = `7139//`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "721000" = `721///`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "722110" = `722511`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "722211" = `722513`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "722A00" = `7223//` + `7224//` + `722514` + `722515`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "811100" = `8111//`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "811200" = `8112//`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "811300" = `8113//`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "811400" = `8114//`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "812100" = `8121//`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "812200" = `8122//`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "812300" = `8123//`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "812900" = `8129//`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "813100" = `8131//`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "813A00" = `8132//` + `8133//`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  CBP_2019_Detail_XBEAt <- transmute(CBP_2019_Detail_XBEA, "813B00" = `8134//` + `8139//`) %>% cbind(CBP_2019_Detail_XBEAt, .)
  
  CBP_2019_Detail_XBEA <- CBP_2019_Detail_XBEAt %>% t()
  BEA_Details <- row.names(CBP_2019_Detail_XBEA)
  CBP_2019_Detail_XBEA <- cbind(BEA_Details, CBP_2019_Detail_XBEA) %>% as.data.frame()
  CBP_2019_Detail_XBEA %<>% reshape(idvar = "BEA_Details", varying = c(colnames(CBP_2019_Detail_XBEA)[-1]), direction = "long")
  rownames(CBP_2019_Detail_XBEA) <- 1:nrow(CBP_2019_Detail_XBEA)
  names(CBP_2019_Detail_XBEA)[names(CBP_2019_Detail_XBEA)=="time"] <- "place"
  CBP_2019_Detail_XBEA$place  %<>% formatC(width = 5, format = "d", flag = "0")
  CBP_2019_Detail_XBEA$emp <-  as.numeric(CBP_2019_Detail_XBEA$emp)
  CBP_2019_Detail_XBEA$qp1 <-  as.numeric(CBP_2019_Detail_XBEA$qp1)
  CBP_2019_Detail_XBEA$ap <-  as.numeric(CBP_2019_Detail_XBEA$ap)
  CBP_2019_Detail_XBEA$est <-  as.numeric(CBP_2019_Detail_XBEA$est)
saver(CBP_2019_Detail_XBEA) 
rm(CBP_2019_Detail_XBEA, CBP_2019_Detail_XBEAt)
}
log_info("BEA/CBP 2019 Detail complete")

# Produce  Distance  Matrix
if (!file.exists(file.path(data_dir, "Dist_mat"))){
  Dist_mat <- file.path(data_dir, "TIGERData") %>% readRDS() %>% .$center %>% as_Spatial() %>% distm()
  rownames(Dist_mat) = colnames(Dist_mat) <- file.path(data_dir, "TIGERData") %>% readRDS() %>%  .$place
saver(Dist_mat)
}
log_info("Distance Matrix complete")

# Produce  Decay/Impedance  Matrix
if (!file.exists(file.path(data_dir, "QI_mat"))){
  #QI_mat <- (1/(Dist_mat/1)^2)
  #QI_mat <- exp(-(Dist_mat/10000))
  ### hyperbolic secant function
  QI_mat <- 2/(exp(-(Dist_mat/1000000)) + exp(Dist_mat/1000000))
saver(QI_mat)
}
rm(Dist_mat, QI_mat)
log_info("Impedance Matrix complete")

## Produce Proximity  Matrix
if (!file.exists(file.path(data_dir, "Prox_mat"))){
  Prox_mat <-  file.path(data_dir, "TIGERData") %>% readRDS() %>% poly2nb(queen = TRUE) %>% nb2mat(style = "B", zero.policy = TRUE)
  colnames(Prox_mat) <- rownames(Prox_mat)
saver(Prox_mat)
rm(Prox_mat)
}
log_info("Proximity Matrix complete")

# TIGER  and RUCC
if (!file.exists(file.path(data_dir, "TIGER_RUCC"))){
  TIGER_RUCC <- inner_join(readRDS(file.path(data_dir, "TIGERData")), readRDS(file.path(data_dir, "RUCCData")), by = "place")
  ### Four non overlapping counties from each ("02063" "02066" "02158" "46102") and ("02261" "02270" "46113" "51515")
  TIGER_RUCC <- TIGER_RUCC[order(TIGER_RUCC$place), ]
  rownames(TIGER_RUCC) <- TIGER_RUCC$place
  TIGER_RUCC <- transform(TIGER_RUCC, H3 = ifelse(RUCC_2013 %in% 1:3, 1, ifelse(RUCC_2013 %in% 4:6, 2, ifelse(RUCC_2013%in% 7:9, 3, 0)  ) ) )
saver(TIGER_RUCC)
rm(TIGER_RUCC)
}
log_info("TIGER/RUCC merge complete")


# Parse  TIGER  and  CBP  2019  county  overlap
if (!file.exists(file.path(data_dir, "TIGER_CBP"))){
  TIGER_CBP <- inner_join(readRDS(file.path(data_dir, "TIGERData")), readRDS(file.path(data_dir, "CBP_2019_places_Sector")), by = "place")
  TIGER_CBP  <- TIGER_CBP[order(TIGER_CBP$place), ]
  rownames(TIGER_CBP) <- TIGER_CBP$place
saver(TIGER_CBP)
rm(TIGER_CBP)
}
log_info("TIGER/CBP 2019 merge complete")


#Parse  TIGER/CBP  and  RUCC  crosswalk
if (!file.exists(file.path(data_dir, "TIGER_CBP_RUCC"))){
  TIGER_CBP_RUCC <- inner_join(readRDS(file.path(data_dir, "TIGER_CBP")), readRDS(file.path(data_dir, "RUCCData")), by = "place")
  rownames(TIGER_CBP_RUCC) <- TIGER_CBP_RUCC$place
  # add augmented hierarchical classification 
  TIGER_CBP_RUCC <- transform(TIGER_CBP_RUCC, H3 = ifelse(RUCC_2013 %in% 1:3, 1, ifelse(RUCC_2013 %in% 4:6, 2, ifelse(RUCC_2013%in% 7:9, 3, 0)  ) ) ) 
saver(TIGER_CBP_RUCC)
rm(TIGER_CBP_RUCC)
}
log_info("TIGER/CBP/RUCC merge complete")


# Parse  TIGER  and  QCED  crosswalk
if (!file.exists(file.path(data_dir, "QCEW_2020_places_Summary"))){
  QCEW_2020_places_Summary <- file.path(data_dir, "QCEW_2020_Sum_XBEA") %>% readRDS() %>% .$place %>% unique() %>% as.data.frame()
  names(QCEW_2020_places_Summary) <- "place"
saver(QCEW_2020_places_Summary)
rm(QCEW_2020_places_Summary)
}
if (!file.exists(file.path(data_dir, "TIGER_QCEW"))){
  TIGER_QCEW <- inner_join(readRDS(file.path(data_dir, "TIGERData")), readRDS(file.path(data_dir, "QCEW_2020_places_Summary")), by = "place")
  TIGER_QCEW  <- TIGER_QCEW[order(TIGER_QCEW$place), ]
  rownames(TIGER_QCEW) <- TIGER_QCEW$place
saver(TIGER_QCEW)
rm(TIGER_QCEW)
}
log_info("TIGER/QCED merge complete")

#Parse  TIGER/QCEW  and  RUCC  crosswalk
if (!file.exists(file.path(data_dir, "TIGER_QCEW_RUCC"))){
  TIGER_QCEW_RUCC <- inner_join(readRDS(file.path(data_dir, "TIGER_QCEW")),  readRDS(file.path(data_dir, "RUCCData")), by = "place")
  rownames(TIGER_QCEW_RUCC) <- TIGER_QCEW_RUCC$place
  # add  augmented  hierarchical  classification 
  TIGER_QCEW_RUCC <- transform(TIGER_QCEW_RUCC, H3 = ifelse(RUCC_2013 %in% 1:3, 1, ifelse(RUCC_2013 %in% 4:6, 2, ifelse(RUCC_2013%in% 7:9, 3, 0)  ) ) )
saver(TIGER_QCEW_RUCC)
rm(TIGER_QCEW_RUCC)
}
log_info("TIGER/QCEW/RUCC merge complete")

# Remove clutter
rm(BEA_Details, BEA_Sectors, BEA_Summary, data_dir, Naics_Det)

# Display end time
log_info("Define data clean end")




