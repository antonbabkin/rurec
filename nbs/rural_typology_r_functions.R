# Function repository 

# Load and attach necessary packages
library(rprojroot)
library(fs)
library(readxl)
library(openxlsx)
library(rlog)
library(reticulate)

library(tidyr)
library(tools)

library(magrittr)
library(sf)

library(dplyr)
library(geosphere)
library(spdep)

library(REAT)

library(arrow)

library(ggplot2)
library(ggiraph)
library(glue)
library(RColorBrewer)
library(viridis)
library(reshape2)
library(scales)
library(cowplot)
library(ggnewscale)

library(matlib)
#detach("package:MASS")

library(magick)

library(tmaptools)


# Display start time
log_info("Define functions start")

# Load conda environment "rurec"
use_condaenv('rurec')

# Import pubdata Python modules
bea_io <- import("rurec.pubdata.bea_io")
cbp <- import("rurec.pubdata.cbp")
ers_rurality <- import("rurec.pubdata.ers_rurality")
geography <- import("rurec.pubdata.geography")
naics <- import("rurec.pubdata.naics")
geography_cbsa <- import("rurec.pubdata.geography_cbsa")
ag_output <- import("rurec.ag_output")

if (!file.exists(file.path(find_rstudio_root_file(), "data", "robjs"))) {
  dir.create(file.path(find_rstudio_root_file(), "data", "robjs"))
}

# Function to download  data
data_getr <- function(FileURL,
                      DestDir){
  local({
    destfile <- FileURL %>% basename() %>% path(DestDir, .)
    if (!file.exists(destfile)) {
      download.file(url=FileURL, destfile=destfile, quiet=TRUE, overwrite = TRUE)
    }
  }) 
}
# Function to download and unzip data
data_zipr <- function(ZipURL,
                      DestDir,
                      FileExt = ""){
  local({
    
    zip_dir = file.path(DestDir, "rawzip")
    if (!file.exists(zip_dir)) {
      dir.create(zip_dir)
    }
    
    DestFile <- ZipURL %>% basename() %>% file_path_sans_ext() %>% path(DestDir, ., ext = FileExt)
    ZipDest <- ZipURL %>% basename() %>% file_path_sans_ext() %>% file.path(zip_dir,  .)
    if (!file.exists(ZipDest)) {
      download.file(url=ZipURL, destfile=ZipDest, quiet=TRUE)
    }
      if (!file.exists(DestFile)) {
        if (file.info(ZipDest)$size > 0){
          unzip(zipfile = ZipDest, exdir = DestDir, overwrite = FALSE)
        }
      }
  })
}
# Function to import a file of excel data tables
excel_importr <- function(TableName,
                          FileDir,
                          RegExType = "*.xlsx"){
  if (!exists(TableName)){
    local({ 
      temp <- FileDir %>% list.files(pattern = RegExType, full.names = TRUE)
      IO_tables <<- vector("list", length(temp))
      for (i in 1:length(temp)){
        DataSheets <- temp[i] %>% excel_sheets()
        SheetList <- lapply(DataSheets, read.xlsx, xlsxFile=temp[i])
        names(SheetList) <- DataSheets
        IO_tables[[i]] <<- SheetList
      }
      names(IO_tables) <<- temp %>% basename() %>% file_path_sans_ext()
    })
  }
}

# Function(s) to clean non-finite values in lists of matrix 
finiter <- function(x){
  if (!is.finite(x)){
    x <- 0
  }
  else {
    x=x
  }
} 
finiterer <- function(x){ 
  lapply(1:length(x), function(i) apply(x[[i]], c(1,2), finiter))
}
# Function(s) to truncate values in lists of matrix  
oner <- function(x, t=1, l=1){
  if (x > l){
    x <- t
  }
  else {
    x=x
  }
}
onerer <- function(x){ 
  lapply(1:length(x), function(i) apply(x[[i]], c(1,2), oner))
}

#Function to save parsed r data tables
saver <- function (dataname, filepath = file.path("data", "robjs")){
  
  data_dir = file.path(find_rstudio_root_file(), filepath)
  if (!file.exists(data_dir)) {
    dir.create(data_dir)
  }
  
  if (!file.exists(file.path(data_dir, as.character(substitute(dataname))))){
    saveRDS(dataname, file = file.path(data_dir, as.character(substitute(dataname))))
  }
}
#Function to import robj data
importr <- function(x, filepath = file.path("data", "robjs")){
  require(rprojroot)
  assign(deparse(substitute(x)), readRDS(file.path(find_rstudio_root_file(), filepath, as.character(substitute(x)) )), envir=.GlobalEnv)
}

# Function to display 3 industry level map together 
dismapr <- function(p){
  if (exists(as.character(substitute(p)))){
    g <- vector(mode='list', length=length(p))
    names(g) <- names(p)
    
    for (i in 1:length(p)){
      gplot = plot_grid(p[[i]][[1]] + theme(legend.position = 'none'), 
                        p[[i]][[2]] + theme(legend.position = 'none'), 
                        p[[i]][[3]] + theme(legend.position = 'none'),
                        labels = c("Sector", "Summary", "Detail"),
                        nrow = 1,
                        label_x = 0, label_y = .75,
                        hjust = -0.5
      )
      
      gleg = get_legend(p[[i]][[1]] +
                          guides(color = guide_legend(nrow = 1)) +
                          theme(legend.position = "bottom", 
                                legend.key.size = unit(.2, "cm"))
      )
      
      gall = gplot + draw_grob(gleg, x = 0, y = 0, width = 1, height = .5, scale = .5)
      
      g[[i]] <- girafe(ggobj = gall, 
                       options = list(opts_hover(css = "stroke:gray;r:20pt;"),
                                      opts_tooltip(css = "font-family:sans-serif;background-color:gray;color:white;padding:10px;border-radius:5px;") 
                       )
      )
    }
    assign(paste0(deparse(substitute(p)), "_XINT"), g, envir=.GlobalEnv)
  } 
  else{
    print("Error: Base plots not found")
  }
}

###### Call and tidy NAICS to BEA industry concordance table
call_industry_concordance <- function(){
  df <- bea_io$get_naics_df() %>% filter(NAICS != "n.a.") %>% filter(NAICS != "NaN") 
  for(i in names(df)){
    df[[i]] <- unlist(df[[i]], use.names = FALSE) 
  }
  df %<>% add_row(SECTOR = "23", SUMMARY = "23", U_SUMMARY = "23",DETAIL = "23", DESCRIPTION = "Construction", NAICS = "23", .after = 39)
  df %<>% filter(NAICS != "23*")
  df %<>% filter(DETAIL != "531HST")
  df <- df[order(df$NAICS), ]
  rownames(df) <- 1:nrow(df)
  b <- c("11", "21", "22", "23", "31G", "31G", "31G", "42", "44RT", "44RT", "48TW", "48TW", "51", "FIRE", "FIRE", "PROF", "PROF", "PROF", "6", "6", "7", "7", "81", "G")
  n <- c("11", "21", "22", "23", "31", "32", "33", "42", "44", "45", "48", "49", "51", "52", "53", "54", "55", "56", "61", "62", "71", "72", "81", "92")
  for(i in 1:length(n)){
    df$SECTOR[substr(df$NAICS, 1,2) %in% n[i]] <- b[i]
  }
  return(df)
}

beacode2description <- function(code){
  #arbitrary selection
  ilevel = "det"
  year = "2012"
  bea_year <- year2bea(year, ilevel)
  sec <- data.frame("code" = names(bea_io$get_sup(strtoi(bea_year), "sec", FALSE)), 
                  "description" = names(bea_io$get_sup(strtoi(bea_year), "sec", TRUE))
                  )
  sum <- data.frame("code" = names(bea_io$get_sup(strtoi(bea_year), "sum", FALSE)), 
                    "description" = names(bea_io$get_sup(strtoi(bea_year), "sum", TRUE))
  )
  det <- data.frame("code" = names(bea_io$get_sup(strtoi(bea_year), "det", FALSE)), 
                    "description" = names(bea_io$get_sup(strtoi(bea_year), "det", TRUE))
  )
  x <- rbind(sec, sum, det)
  df <- x["description"][x["code"] == code]
  return(df)
}


###### Get specific industry NAICS to BEA concordance
ilevel_concord <- function(ilevel = c("det", "sum", "sec"), ...){
  ilevel <- match.arg(ilevel)
  x <- call_industry_concordance()
  if(ilevel == "det"){
    df <- x %>% select(DETAIL, NAICS)
    }
  if(ilevel == "sum"){
    df <- x %>% select(SUMMARY, NAICS) 
    df$NAICS <- substr(df$NAICS, 1,3)
    df <- df[!duplicated(df), ]
    rownames(df) <- 1:nrow(df)
  }
  if(ilevel == "sec"){
    df <- x %>% select(SECTOR, NAICS) 
    df$NAICS <- substr(df$NAICS, 1,2)
    df <- df[!duplicated(df), ]
    rownames(df) <- 1:nrow(df)
  }
  return(df)
}

# Call up and clean TIGER data
# shapefile formats are available for 2000, 2007 and every year after that.
call_tiger <- function(tiger_year,
                  scale = c("20m", "500k", "5m"),
                  geometry = TRUE, 
                  center = TRUE,
                  ...){
  scale <- match.arg(scale)
  df <- geography$get_county_df(strtoi(tiger_year), 
                                geometry, 
                                scale)
  df %<>% rename(place = CODE)
  df$COUNTY <- paste(df$NAME, "County")
  if(isTRUE(geometry) & isTRUE(center)){df$center <- st_centroid(df$geometry)}
  st <- geography$get_state_df(geometry = FALSE) %>% select(c(1:3)) 
  names(st) <- c("STATE_CODE", "STATE_NAME", "STATE")
  df <- left_join(df, st, by = "STATE_CODE")
  df %<>% arrange(place)
  return(df)
}

# Convert miles to meters
miles2meters <- function(miles){
  df <- as.integer(miles)*1609.344
  return(df)
}

# Produce Distance  Matrix
dist_matc <- function(...){
  t <- call_tiger(...)
  df <- t$center %>% 
    as_Spatial() %>% 
    distm()
  rownames(df) = colnames(df) <- t$place
  return(df)
}

# Produce Distance Matrix from polygon edge with variable distance
dist_matb <- function(dist, 
                     ...){
  t <- call_tiger(...)
  df = st_is_within_distance(t$geometry, 
                             dist = dist)
  df <- +as.matrix(df)
  diag(df) <- 0
  rownames(df) = colnames(df) <- t$place
  return(df)
}


# Produce Border Proximity  Matrix
bprox_mat <- function(queen = TRUE, 
                      ...){
  t <- call_tiger(...)
  df <- t$geometry %>% 
    poly2nb(queen = queen) %>%
    nb2mat(style = "B", zero.policy = TRUE)
  rownames(df) = colnames(df) <- t$place
  return(df)
}

# Produce Distance Proximity Matrix
# distance in meters
dprox_mat <- function(boundary_limit,
                      ...){
  df <- dist_matc(...)
  df[df < boundary_limit & df > 0] <- 1
  df[df > boundary_limit] <- 0
  return(df)
}

# Produce inverse power distance decay impedance matrix
power_impedance_mat <- function(decay_power = 2, 
                                ...){
  df <- dist_matc(...)
  #ifelse(df == 0, 1, (1/(df)^decay_power))
  df <- (1/(df)^decay_power)
  df[is.infinite(df)] = 1
  #diag(df) <- 1
  return(df)
}

# Produce exponential distance decay impedance matrix
expo_impedance_mat <- function(decay_constant = 10000,
                               ...){
  df <- dist_matc(...)
  df <- exp(-(df/decay_constant)) 
  return(df)
}

# Produce Gaussian distance decay impedance matrix
gaus_impedance_mat <- function(rms_width = 1609344,
                               ...){
  df <- dist_matc(...)
  df <- exp(-.5*(df/rms_width)^2) 
  return(df)
}

# Produce hyperbolic secant distance decay impedance matrix
hyper_impedance_mat <- function(decay_constant = 1000000,
                                ...){
  df <- dist_matc(...)
  df <- ((2/(exp(-(df/decay_constant)) + exp(df/decay_constant))))
  return(df)
}

# Produce bi-square distance decay 
bisquare_impedance_mat <- function(decay_zero,
                                   ...){
  dis <- dist_matc(...)
  #ifelse(dis>decay_zero, 0, (1-(dis/decay_zero)^2)^2)
  df <- (1-(dis/decay_zero)^2)^2
  df[dis>decay_zero] <- 0
  return(df)
}

# Call up and clean RUCC data
call_rucc <- function(ryear = c("2013", "2003", "1993", "1983", "1974"),
                      ...){
  ryear <- match.arg(ryear)
  df <- ers_rurality$get_ruc_df()
  df <- df %>% filter(RUC_YEAR == ryear)
  df$place <- df$FIPS
  return(df)
}

# Produce TIGER and RUCC table
tiger_rucc <- function(...){
  t <- call_tiger(...)
  r <- call_rucc(...) %>% select(-c(2))
  df <- inner_join(t, r, by = "place")
  df <- df[order(df$place), ]
  return(df)
}

# Call up and clean CBP
# ($1,000 of dollars)
# available for years 1986:2020
call_cbp <- function(cbp_year,
                     cbp_scale = c("county", "state", "us"),
                     ...){
  cbp_scale <- match.arg(cbp_scale)
  df <- cbp$get_df(cbp_scale, 
                   strtoi(cbp_year))
  if(cbp_scale == "county"){
    df$place <- paste0(df$fipstate, df$fipscty)
  }
  if(cbp_scale == "state"){
    df$place <- df$fipstate
  }
  if(cbp_scale == "us"){
    df$place <- "usa"
  }
  n <- c("lfo", "fipstate", "fipscty", "place", "industry", "emp", "qp1", "ap", "est")
  df <- df[names(df) %in% n] %>% 
    rename(NAICS = industry)
  return(df)
}

year2cbsa <- function(year,
                      ...){
  cbsa_year = c("2020", "2018", "2017", "2015", "2013", "2009", "2008", "2007", "2006", "2005", "2004", "2003")
  if(!year %in% cbsa_year){
    warning("CBSA concordance years do not contain [",year,"]")
    }
  if(year %in% cbsa_year){
    return(year)
  }else if(year > "2018"){
    return(cbsa_year[1])
  }else if(year == "2016"){
    return(cbsa_year[3])
  }else if(year == "2014"){
    return(cbsa_year[4])
  }else if(year %in% 2012:2010){
    return(cbsa_year[5])
  }else if(year < "2004"){
    return(cbsa_year[12])
  }
}

# Call up and clean CBSA concordance codes 
#(Delineations available for 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2013, 2015, 2017, 2018, 2020)
call_cbsa_concord <- function(...){
  cbsa_year <- year2cbsa(...)  
  df <- geography_cbsa$get_cbsa_delin_df(strtoi(cbsa_year))
  df$CBSA_TITLE <- sapply(strsplit(df$CBSA_TITLE, ","), "[", 1)
  df$CBSA_TITLE <- paste(df$CBSA_TITLE, rep("CBSA", length(df$CBSA_TITLE)))
  df$place <- paste0(df$STATE_CODE, df$COUNTY_CODE)
  df <- df %>% select(CBSA_CODE, place, CBSA_TITLE)
  return(df)
}

#Convert a fips code into a cbsa code 
fips2cbsa <- function(fips, 
                      ...){
  cb <- call_cbsa_concord(...)
  if(isFALSE(fips %in% paste0(dataRetrieval::countyCd$STATE, dataRetrieval::countyCd$COUNTY))){
    warning("FIPS entry [",fips,"] not found in ANSI FIPS records\n See: https://www2.census.gov/geo/docs/reference/codes/national_county.txt")
    }
  if(isTRUE(fips %in% cb$place)){cb$CBSA_CODE[fips == cb$place]}else{fips}
}

year2tiger <- function(year,
                      ...){
  tiger_year = c("2022":"2013", "2010", "2000", "1990")
  if(!year %in% tiger_year){
    warning("Shapefile years do not contain [",year,"]")
    }
  if(year %in% tiger_year){
    return(year)
  }else if(year > max(tiger_year)){
    return(max(tiger_year))
  }else if(year > "2011"){
    return("2013")
  }else if(year > "2005"){
    return("2010")
  }else if(year > "1995"){
    return("2000")
  }else if(year < "1995"){
      return(min(tiger_year))
    }
}

# Aggregate spatial features of each CBSA member in a cluster
cbsa_spatial_cluster <- function(...){
  tiger_year <- year2tiger(...)
  t <- call_tiger(...)
  c <- call_cbsa_concord(...)
  c <- c[c$place %in% intersect(c$place, t$place),]
  c <- data.frame(CBSA_CODE = c(c$CBSA_CODE, setdiff(t$place, c$place)), 
                  place = c(c$place, setdiff(t$place, c$place)),
                  CBSA_TITLE = c(c$CBSA_TITLE, t$COUNTY[t$place %in% setdiff(t$place, c$place)]))
  c <- c[order(c$CBSA_CODE), ]
  rownames(c) <- 1:nrow(c)

  j <- inner_join(t, c, by = "place", copy = TRUE)
  
  x <- j$CBSA_CODE %>% unique() %>% .[order(.)]
  df <- j %>% distinct(CBSA_CODE, .keep_all = TRUE) %>% select(CBSA_CODE, CBSA_TITLE)
  for (i in x){
    #print(paste(list_names, "start cluster: ", i, which(i == x), "of", length(x), Sys.time()))
    df$geometry[df$CBSA_CODE == i] <- j %>% filter(j$CBSA_CODE == i) %>% st_union()
    #print(paste(list_names, "  end cluster: ", i, which(i == x), "of", length(x), Sys.time()))
  }
  return(df)
}

### Call geographic features
call_geog <- function(year,
                      cbsa_clust = FALSE, 
                      ...){
  if(isFALSE(cbsa_clust)){
    tiger_year <- year2tiger(year, ...)
    df <- call_tiger(tiger_year, ...) 
  } else {
    cbsa_year <- year2cbsa(year, ...)
    df <- cbsa_spatial_cluster(cbsa_year, ...)
    df <- rename(df, place = CBSA_CODE)
    df <- rename(df, NAME = CBSA_TITLE)
  }
  return(df)
}



#Agglomerate NAICS and BEA concordance by year and industry specificity (sector, summary, or detail)
place_industry_economy_long <- function(cbp_scale = c("county", "state", "us"), ...){
  cbp_scale <- match.arg(cbp_scale)
  conc <- ilevel_concord(...)
  n <- names(conc)[1]
  cbp_dat <- call_cbp(..., cbp_scale = cbp_scale)
  if (cbp_scale != "county"){
    cbp_dat <- cbp_dat %>% filter(.[["lfo"]] == "-")
  }
  x <- left_join(cbp_dat, conc, by = "NAICS") 
  x <- x %>%
    filter(.[dim(x)[2]] != "NULL") %>%
    group_by(place, .[dim(x)[2]]) %>%
    summarise(across(where(is.numeric), sum), .groups = 'drop') %>%
    as.data.frame()
  if (cbp_scale != "us"){
    x <- x %>%
      group_by(place) %>%
      arrange(factor(x[[2]], levels = unique(conc[[1]])), .by_group = TRUE) %>%
      as.data.frame()
      df <- x %>% pivot_wider(id_cols = n, names_from = "place", values_from = c("emp", "qp1", "ap", "est"), names_sep = ".", values_fill = 0)
      df <- df %>% pivot_longer(-n, names_to = c(".value", "place"), names_pattern = "([^\\.]*)\\.*(\\d+)") %>% as.data.frame()
      df <- df %>% group_by(df[[2]]) %>% arrange(factor(df[[1]], levels = unique(conc[[1]])), .by_group = TRUE)
    names(df)[1] <- "indcode"
  } else {
    df <- x %>% arrange(factor(x[[2]], levels = unique(conc[[1]])), .by_group = TRUE)
    names(df)[2] <- "indcode"
  }
  df <- df[1:6]
  return(df)
}

#Generate industry output ("ap", "emp", "qp1", or "est") by location (county) from CBP in terms of BEA industry codes ("det_cord", "sum_cord", or "sec_cord") for any available year
industry_output_by_place <- function(output_metric = c("ap", "emp", "qp1", "est"),
                                     cbp_scale = c("county", "state", "us"),
                                     ...){
  output_metric <- match.arg(output_metric)
  cbp_scale <- match.arg(cbp_scale)
  df <- place_industry_economy_long(cbp_scale = cbp_scale, ...) %>% .[, c("indcode", "place", output_metric)]
  if (cbp_scale != "us"){
    df <- df %>% pivot_wider(id_cols = "indcode", names_from = "place", values_from = output_metric) %>% as.data.frame()
  } 
  rownames(df) <- df$indcode
  df <- df[, !colnames(df) %in% c("indcode","place"), drop=F] %>% as.matrix()
  return(df)
}

############ Call and clean pubdata BEA IO Use table
call_use_table <- function(bea_year,
                           ilevel = c("det", "sum", "sec"),
                           ...){
  ilevel <- match.arg(ilevel)
  if(ilevel == "det"){
    x <- bea_year %in% c("2007", "2012")
    stopifnot("BEA detail level tables only exist for years 2007 and 2012" = x == TRUE)
  }
  df <- bea_io$get_use(strtoi(bea_year), ilevel) %>% as.matrix()
  df[is.na(df)] = 0
  return(df)
}

############ Call and clean pubdata BEA IO Supply table
call_supply_table <- function(bea_year,
                              ilevel = c("det", "sum", "sec"),
                              ...){
  ilevel <- match.arg(ilevel)
  if(ilevel == "det"){
    x <- bea_year %in% c("2007", "2012")
    stopifnot("BEA detail level tables only exist for years 2007 and 2012" = x == TRUE)
  }
  df <- bea_io$get_sup(strtoi(bea_year), ilevel) %>% as.matrix()
  df[is.na(df)] = 0
  return(df)
}

############ Derive the industry labor shares by year and industry scale
labor_share <- function(...){
  df <- call_use_table(...)
  ext <- grep("^(F|T)[0-9]", colnames(df), value = TRUE)
  df <- df[, !colnames(df) %in% ext]
  lcv <- grep("^V001", rownames(df), value = TRUE)
  df <- (df[lcv, , drop = FALSE] / df["T018", , drop = FALSE]) 
  rownames(df) <- "LaborShare"
  return(df)
}

############ Derive the Factor Ratio of National Commodity Factor Demand to National Gross Commodity Output
factor_ratio <- function(bea_year,
                         ilevel = c("det", "sum", "sec"),
                         ...){
  ilevel <- match.arg(ilevel)
  ut <- call_use_table(bea_year, ilevel = ilevel, ...)
  st <- call_supply_table(bea_year, ilevel = ilevel, ...)
  #df <- ut[1:(which(rownames(ut) == "T005")-1),"T001", drop=F]/st[1:(nrow(st)-1),"T007", drop=F]
  df <- ut[1:(which(rownames(ut) == "T005")-1),"T001", drop=F]/st[1:(nrow(st)-1),"T016", drop=F]
  colnames(df) <- "factor_ratio"
  if(ilevel == "det"){
    ind_names <- rownames(df)[!rownames(df) %in%  c(grep("^23", rownames(df), value = TRUE), "4200ID")] %>% append("23", after = 24)
    cn <- sum(ut[grep("^23", rownames(ut), value = TRUE), "T001"]) / sum(st[grep("^23", rownames(ut), value = TRUE), "T007"]) %>% 
      matrix(dimnames = list(c("23"), c("factor_ratio")) ) 
    nc <- df[!grepl("^23", rownames(df)), , drop = FALSE]  
    df <- rbind(cn, nc)[ind_names, , drop=F]
  }
  return(df)
}


############ Derive the national level, industry specific, payroll share of gross output by year and industry scale
payroll_share <- function(cbp_year,
                          ilevel = c("det", "sum", "sec"),
                          ...){
  ilevel <- match.arg(ilevel)
  indout <- year2bea(cbp_year, ilevel, ...) %>% 
    call_use_table(ilevel, ...) %>% 
    .["T018", !colnames(.) %in% grep("^(F|T)[0-9]", colnames(.), value = TRUE), drop = FALSE]*1000000
  if(ilevel == "det"){
    cn <- indout %>% .[, grep("^23", colnames(.), value = TRUE)] %>% sum() %>% matrix(dimnames = list(rownames(indout), c("23")) ) 
    nc <- indout %>% .[, !grepl("^23", colnames(.)), drop = FALSE]  
    indout <- cbind(cn, nc) 
  }
  conc <- ilevel_concord(ilevel = ilevel, ...)
  cbp_dat <- call_cbp(..., cbp_year = cbp_year, cbp_scale = "us")
  ap <- left_join(cbp_dat, conc, by = "NAICS") %>% 
    filter(.[dim(.)[2]] != "NULL" & .[["lfo"]] == "-")
  n <- names(ap)[dim(ap)[2]]
  ap <- ap %>% 
    group_by(.[dim(.)[2]]) %>%
    summarise(across(where(is.numeric), sum), .groups = 'drop') %>% 
    .[c("ap", n )] %>% 
    pivot_wider(names_from = n, values_from = c("ap"), names_sep = ".", values_fill = 0) %>% 
    as.matrix()*1000
  rownames(ap) <- "ap"
  intind <- intersect(colnames(ap), colnames(indout))
  df <- (ap["ap", intind , drop = FALSE] / indout["T018", intind , drop = FALSE]) 
  rownames(df) <- "psogo"
  return(df)
}

# Call up and clean Ag Output data
call_agoutput <- function(ag_year = c("2017", "2012", "2007", "2002"), 
                          geo_level = c("county", "state", "national"),
                          ...){
  ag_year <- match.arg(ag_year)
  geo_level <- match.arg(geo_level)
  df <- ag_output$get_farm_sales_by_bea_detail(strtoi(ag_year), geo_level) %>% as.data.frame()
  place <- c(place = rownames(df))
  df <- sapply(df, function(x)x/1000) %>% as.data.frame()
  if(geo_level == "county" | geo_level == "state"){
    df <- cbind(place, df)
    rownames(df) <- 1:nrow(df)
  } else {
    df <- t(df)
  }
  return(df)
}

year2bea <- function(year,
                     ilevel,
                     ...){
  if(ilevel != "det"){
    bea_year = c("2021":"1997")
    if(!year %in% bea_year){
      warning("BEA years do not contain [",year,"]")
      }
    if(year %in% bea_year){
      return(year)
    }else if(year > max(bea_year)){
      return(max(bea_year))
    }else if(year < min(bea_year)){
      return(min(bea_year))
    }
  }else{
    bea_year = c("2012", "2007")
    if(!year %in% bea_year){
      warning("Detail level BEA years do not contain  [",year,"]")
      }
    if(year %in% bea_year){
      return(year)
    }else if(year > "2007"){
      return("2012")
    }else if(year < "2007"){
      return("2007")
    }
  }
}

year2agcensus <- function(year,
                          ...){
  ag_year = c("2017", "2012", "2007", "2002")
  if(!year %in% ag_year){
    warning("AgCensus years do not contain [",year,"]")
    }
  if(year %in% ag_year){
    return(year)
  }else if(year > "2014"){
    return("2017")
  }else if(year > "2009"){
    return("2012")
  }else if(year > "2004"){
    return("2007")
  }else if(year < "2005"){
    return("2002")
  }
}

### Need to add farm sales tax/inventory correction
############ Derive the Total Output Matrix (in thousands of dollars)
total_output <- function (cbp_year,
                          ilevel = c("det", "sum", "sec"), 
                          ...){
  ilevel <- match.arg(ilevel)
  ag_year <- year2agcensus(cbp_year, ...)
  farm_sales <- call_agoutput(ag_year, ...)
  fn <- colnames(farm_sales)[-c(1)]
  iout <- industry_output_by_place(cbp_year = cbp_year, ilevel = ilevel, ...)
  ps <- payroll_share(cbp_year, ilevel, ...)
  ps <- ps[, rownames(iout)[rownames(iout) %in% colnames(ps)], drop = FALSE] 
  df <- apply(iout, 2, function (x) {x / ps})
  df[is.infinite(df)] = 0
  rownames(df) <- rownames(iout)
  df <- t(df) %>% as.data.frame()
  df$place <- rownames(df)
  df <- left_join(df, farm_sales, by = "place")
  if (ilevel == "sec") {
    df[["11"]] <- rowSums(df[, c("11", fn)], na.rm = T)
    df <- df %>% select(!all_of(c(fn)))
  } else if (ilevel == "sum") {
    df[["111CA"]] <- rowSums(df[, c(fn)], na.rm = T)
    df <- df %>% select(!all_of(c(fn))) %>% select("111CA", everything())
  } else if (ilevel == "det") {
    df <- df %>% select(all_of(fn), everything())
  }
  rownames(df) <- df$place
  df$place <- NULL
  df <- t(df)
  df[is.na(df)] = 0
  df[is.infinite(df)] = 0
  return(df)
}

#### Commodities-by-Industries parallel to ordinary technical coefficients matrix 
b_matrix <- function(bea_year,
                     ilevel = c("det", "sum", "sec"),
                     ...){
  ilevel <- match.arg(ilevel)
  if(ilevel == "det"){
    x <- bea_year %in% c("2007", "2012")
    stopifnot("BEA detail level tables only exist for years 2007 and 2012" = x == TRUE)
  }
  #bea_year <- year2bea(cbp_year, ilevel, ...)
  df <- bea_io$get_use(strtoi(bea_year), ilevel, F)
  df[is.na(df)] = 0
  u <- df[1:(which(rownames(df) == "T005")-1), 1:(which(colnames(df) == "T001")-1)] %>% as.matrix()
  x <- df["T018", 1:(which(colnames(df) == "T001")-1)]
  if(ilevel == "det"){ 
    con <- grep("^23", colnames(u), value = TRUE)
    cm <- matrix(sum(u[con,con]), 
                 dimnames = list(c("23"), c("23")))
    cr <- t(matrix(colSums(u[con, ]), 
                   dimnames = list(colnames(u), c("23")) ))
    cc <- matrix(rowSums(u[, con]), 
                 dimnames = list(rownames(u), c("23"))) 
    
    com_names <- rownames(u)[!rownames(u) %in% c(con, "4200ID")] %>% append("23", after = 24)
    ind_names <- colnames(u)[!colnames(u) %in% c(con, "4200ID")] %>% append("23", after = 24)
    u <- cbind(rbind(cr, u), rbind(cm, cc )) 
    u <- u[com_names, ind_names] 
    x <- cbind(matrix(sum(x[con]), dimnames = list(c("T018"), c("23"))), x[colnames(x)[!colnames(x) %in% con]])
    x <- x[ind_names] 
  }
  df <- u %*% diag(1/x)
  colnames(df) <- colnames(x)
  return(df)
}


####Commodity Composition of Industry Outputs
c_matrix <- function(bea_year,
                     ilevel = c("det", "sum", "sec"),
                     ...){
  ilevel <- match.arg(ilevel)
  #bea_year <- year2bea(cbp_year, ilevel, ...)
  df <- bea_io$get_sup(strtoi(bea_year), ilevel, F)
  df[is.na(df)] = 0
  ind_supply <- df[nrow(df), 1:(which(colnames(df) == "T007")-1), drop=F]
  supply_mat <- as.matrix(df[1:(nrow(df)-1), 1:(which(colnames(df) == "T007")-1)])
  
  if(ilevel == "det"){ 
    con <- grep("^23", colnames(supply_mat), value = TRUE)
    cm <- matrix(sum(supply_mat[con,con]), 
                 dimnames = list(c("23"), c("23")))
    cr <- t(matrix(colSums(supply_mat[con, ]), 
                   dimnames = list(colnames(supply_mat), c("23")) ))
    cc <- matrix(rowSums(supply_mat[, con]), 
                 dimnames = list(colnames(supply_mat), c("23"))) 
    com_names <- rownames(supply_mat)[!rownames(supply_mat) %in% c(con, "4200ID")] %>% append("23", after = 24)
    ind_names <- colnames(supply_mat)[!colnames(supply_mat) %in% c(con, "4200ID")] %>% append("23", after = 24)
    supply_mat <- cbind(rbind(cr, supply_mat), rbind(cm, cc )) 
    supply_mat <- supply_mat[com_names, ind_names] 
    ind_supply <- cbind(matrix(sum(ind_supply[con]), dimnames = list(c("T017"), c("23"))), ind_supply[colnames(ind_supply)[!colnames(ind_supply) %in% con]])
    ind_supply <- ind_supply[ind_names] 
  }
  df <- supply_mat %*% diag(1/ind_supply)
  colnames(df) <- colnames(supply_mat)
  #temp fix needs correction in rurec.pubdata.bea_io module
  if (ilevel == "sec") {
    rownames(df) <- c(colnames(supply_mat), "Used", "Other")
  }
  return(df)
}


#### Industry Source of Commodity Outputs
d_matrix <- function(bea_year,
                     ilevel = c("det", "sum", "sec"),
                     ...){
  ilevel <- match.arg(ilevel)
  #bea_year <- year2bea(cbp_year, ilevel, ...)
  df <- bea_io$get_sup(strtoi(bea_year), ilevel, F)
  df[is.na(df)] = 0
  com_supply <- df[1:(nrow(df)-1), "T007", drop=F] %>% as.matrix()
  supply_mat <- as.matrix(df[1:(nrow(df)-1), 1:(which(colnames(df) == "T007")-1)])
  
  if(ilevel == "det"){ 
    con <- grep("^23", rownames(supply_mat), value = TRUE)
    cm <- matrix(sum(supply_mat[con,con]), 
                 dimnames = list(c("23"), c("23")))
    cr <- t(matrix(colSums(supply_mat[con, ]), 
                   dimnames = list(colnames(supply_mat), c("23")) ))
    cc <- matrix(rowSums(supply_mat[, con]), 
                 dimnames = list(colnames(supply_mat), c("23"))) 
    com_names <- rownames(supply_mat)[!rownames(supply_mat) %in% c(con, "4200ID")] %>% append("23", after = 24)
    ind_names <- colnames(supply_mat)[!colnames(supply_mat) %in% c(con, "4200ID")] %>% append("23", after = 24)
    supply_mat <- cbind(rbind(cr, supply_mat), rbind(cm, cc )) 
    supply_mat <- supply_mat[com_names, ind_names] 
    com_supply <- rbind(matrix(sum(com_supply[con,]), dimnames = list(c("23"), c("T007"))), com_supply[rownames(com_supply)[!rownames(com_supply) %in% con], , drop=F])
    com_supply <- com_supply[com_names, , drop=F] 
  }
  df <- t(supply_mat) %*% diag(as.vector(1/com_supply))
  #temp fix needs correction in rurec.pubdata.bea_io module
  if (ilevel == "sec") {
    colnames(df) <- c(colnames(supply_mat), "Used", "Other")
  } else {
    colnames(df) <- rownames(supply_mat)
  }
  return(df)
}

####Commodity Output Supply Ratio - ratio of domestic total commodity output to total product supply
cos_ratio <- function(bea_year,
                      ilevel = c("det", "sum", "sec"),
                      ...){
  ilevel <- match.arg(ilevel)
  #bea_year <- year2bea(cbp_year, ilevel, ...)
  sup <- bea_io$get_sup(strtoi(bea_year), ilevel, F)
  sup[is.na(sup)] = 0
  com_supply <- sup[1:(nrow(sup)-1), "T007", drop = F] %>% as.matrix()
  pro_supply <- sup[1:(nrow(sup)-1), "T016", drop = F] %>% as.matrix()
  if(ilevel == "det"){ 
    con <- grep("^23", rownames(sup), value = TRUE)
    com_names <- rownames(sup)[1:(nrow(sup)-1)][!rownames(sup)[1:(nrow(sup)-1)] %in% c(con, "4200ID")] %>% append("23", after = 24)
    com_supply <- rbind(matrix(sum(com_supply[con,]), dimnames = list(c("23"), c("T007"))), com_supply[rownames(com_supply)[!rownames(com_supply) %in% con], , drop=F])
    com_supply <- com_supply[com_names, , drop=F] 
    pro_supply <- rbind(matrix(sum(pro_supply[con,]), dimnames = list(c("23"), c("T007"))), pro_supply[rownames(pro_supply)[!rownames(pro_supply) %in% con], , drop=F])
    pro_supply <- pro_supply[com_names, , drop=F] 
  }
  df <- pro_supply / com_supply
  colnames(df) <- "cos_ratio"
  #temp fix needs correction in rurec.pubdata.bea_io module
  if (ilevel == "sec") {
    rownames(df) <- c(colnames(sup)[1:(which(colnames(sup) == "T007")-1)], "Used", "Other")
  }
  return(df)
}








# Aggregate industry output of each CBSA members in a cluster
cbsa_aggregate_industry_output <- function(cbp_year,
                                           ...){
  o <- total_output(cbp_year, ...)
  c <- call_cbsa_concord(cbp_year, ...)
  c <- c[c$place %in% intersect(c$place, colnames(o)),]
  c <- data.frame(CBSA_CODE = c(c$CBSA_CODE, setdiff(colnames(o), c$place)), 
                  place = c(c$place, setdiff(colnames(o), c$place)))
  c <- c[order(c$CBSA_CODE), ]
  rownames(c) <- 1:nrow(c)
  x <- c$CBSA_CODE %>% unique()
  df <- data.frame(row.names = rownames(o))
  for(i in x){
    df[, i] <- rowSums(o[, c$place[c$CBSA_CODE == i], drop = FALSE])
  } 
  df <- as.matrix(df)
  df[is.na(df)] = 0
  return(df)
}

#Need to adjust to not use tiger
############ Derive clean Output Matrix 
total_output_tidy <- function(cbp_year,
                              cbsa_clust = FALSE, 
                              ...){
  t <- year2tiger(cbp_year, ...)  %>% call_tiger(...)
  if(isFALSE(cbsa_clust)){
    df <- total_output(cbp_year, ...)
    df <- df[, colnames(df) %in% t$place]
  } else {
    df <- cbsa_aggregate_industry_output(cbp_year, ...)
    c <- call_cbsa_concord(cbp_year, ...)
    c <- c[c$place %in% intersect(c$place, t$place),]
    c <- data.frame(CBSA_CODE = c(c$CBSA_CODE, setdiff(t$place, c$place)),
                    place = c(c$place, setdiff(t$place, c$place)))
    df <- df[, colnames(df) %in% c$CBSA_CODE]
  }
  df <- df[, !colnames(df) %in% colnames(df[,grep('^(02|15)', colnames(df))]) ]
  df[is.na(df)] = 0
  df <- df[, colSums(df != 0) > 0]
  return(df)
}

############ Call and clean the total requirements matrix 
call_total_requirements <- function(bea_year,
                                    ilevel = c("det", "sum", "sec"), 
                                    ...){
  ilevel <- match.arg(ilevel)
  if(ilevel == "det"){
    x <- bea_year %in% c("2007", "2012")
    stopifnot("BEA detail level tables only exist for years 2007 and 2012" = x == TRUE)
  }
  df <- bea_io$get_ixi(strtoi(bea_year), ilevel) %>% .[1:ncol(.), ] %>% as.matrix()
  rownames(df) <- colnames(df)
  return(df)
}


### Need better methods for matrix inversion in tests inv() gave more accurate results but solve() was MUCH faster: inv() will also reach memory limit
############ Derive the direct requirements matrix (Technical Coefficients) 
call_direct_requirements <- function(...){
  df <- call_total_requirements(...)
  df <- diag(ncol(df)) - solve(df)
  df[df < 0] = 0
  return(df)
}

############ Derive the Gross Commodity Output Matrix
commodity_output <- function (c_matrix,
                              industry_output_matrix){
  ## Check industry level specificity match between industry_output_matrix and technical_coefficients_matrix
  df <- setequal(colnames(c_matrix), rownames(industry_output_matrix))
  stopifnot("Industry names do not match" = df == TRUE)
  df <- c_matrix %*% industry_output_matrix
}

############ Industry Input Needs 
### Derive the commodity-by-county matrix of input needs DY
industry_input <- function(technical_coefficients_matrix, 
                           industry_output_matrix){
  ## Check industry level specificity match between industry_output_matrix and technical_coefficients_matrix
    df <- setequal(colnames(technical_coefficients_matrix), rownames(industry_output_matrix))
    stopifnot("Industry names do not match" = df == TRUE)
    o <- industry_output_matrix
    d <- technical_coefficients_matrix
    df <- d %*% o
}

############ Net Input Demand
### Derive the industry-by-county matrix of net input demand 
net_input_demand <- function(industry_output_matrix, 
                             industry_input_matrix){
    i <- industry_input_matrix
    o <- industry_output_matrix
    df <- pmax(i - o, 0)
}

############ Net Input Supply
### Derive the industry-by-county matrix of net input supply 
net_input_supply <- function(industry_output_matrix, 
                             industry_input_matrix){
    i <- industry_input_matrix
    o <- industry_output_matrix
    df <- pmax(o - i, 0)
}

############ Stacked Absorption Share 
stacked_absorption_share <- function(nis_matrix, 
                                     nid_matrix,
                                     list_names = NULL){
  
  s <- nis_matrix
  d <- nid_matrix
  x <- rep(c(1), each=nrow(s))
  ## Check counties match between nis_matrix and nid_matrix
  df <- identical(colnames(d), colnames(s))
  stopifnot(df)
  print(paste(list_names, "Absorption calculation started", Sys.time() ))
    df <-  matrix(0, nrow = ncol(s), 
                       ncol = ncol(s) )
    rownames(df) = colnames(df) <- colnames(s)
    for (i in 1:ncol(s)){
      for (j in 1:ncol(s)){
        df[i,j] <- 
          (x %*% pmin(s[,i], d[,j]))
      }
    }
 print(paste(list_names, "Absorption calculation finished", Sys.time() ))
 invisible(df)
}

############ Normalized Absorption Share
normalized_absorption_share <- function(sas_matrix, 
                                        nis_matrix){
  s <- sas_matrix
  n <- nis_matrix
  df <- s / colSums(n)
  df[is.na(df)] = 0
  return(df)
}

############ Row-wise Absorption Potential Maximum and Match
absorption_maximum_match <- function(absorption_matrix, 
                                     threshold = .05,
                                     row_max_match = TRUE,
                                     ...){
  a <- absorption_matrix
  if(isTRUE(row_max_match)){
    df <-  cbind(place = rownames(a), 
                 match = colnames(a)[apply(a, 1, which.max)],
                 max_absorption_alpha = apply(a, 1, max), 
                 second_max_absorption_alpha = apply(a, 1, function(x){max(x[x != max(x), drop = FALSE])}) %>% suppressWarnings(), 
                 absorption_alpha_gini = apply(a, 1, gini),
                 absorption_alpha_total = apply(a, 1, sum),
                 absorption_alpha_mean = apply(a, 1, mean),
                 absorption_alpha_sd = apply(a, 1, sd)
    ) %>% as.data.frame()
  } else {
    df <-  cbind(place = rownames(a), 
                 match = colnames(a)[apply(a, 2, which.max)],
                 max_absorption_alpha = apply(a, 2, max), 
                 second_max_absorption_alpha = apply(a, 2, function(x){max(x[x != max(x), drop = FALSE])}), 
                 absorption_alpha_gini = apply(a, 2, gini),
                 absorption_alpha_total = apply(a, 2, sum),
                 absorption_alpha_mean = apply(a, 2, mean),
                 absorption_alpha_sd = apply(a, 2, sd)
    ) %>% as.data.frame()
  } 
  
  df$max_absorption_alpha <- as.numeric(df$max_absorption_alpha)
  df$second_max_absorption_alpha <- as.numeric(df$second_max_absorption_alpha)
  df$absorption_alpha_gini <- as.numeric(df$absorption_alpha_gini)
  df$absorption_alpha_total <- as.numeric(df$absorption_alpha_total)
  df$absorption_alpha_mean <- as.numeric(df$absorption_alpha_mean)
  df$absorption_alpha_sd <- as.numeric(df$absorption_alpha_sd)
  
  ### cluster_class reevaluates the maximum absorption match to account for an isolation threshold and ECA isolated corner cases (i.e., no one imports your excess so you are isolated, but you are max import sink for someone else)
  df$cluster_class <- df$match
  df$cluster_class[df$max_absorption_alpha < threshold] <- "Isolated"
  df$cluster_class[df$place %in% unique(df$match) & df$cluster_class == "Isolated"] <- "ECA Isolated"
  
  ### eca_class reevaluates the maximum absorption match and returns the corrected self-match locations for "ECA Isolated" and "Cluster Core" locations 
  df$eca_class <- df$cluster_class
  df$eca_class[df$cluster_class == "ECA Isolated"] <- df$place[df$cluster_class == "ECA Isolated"]
  df$eca_class[df$place %in% unique(df$cluster_class)] <- df$place[df$place %in% unique(df$cluster_class)]
  
  ### cluster_category gives the categorical classification of each location as one of: "Isolated", "Isolated, Cluster Sink", "Cluster Sink", or "Cluster Source"
  df$cluster_category <- df$cluster_class
  df$cluster_category[df$place %in% unique(df$cluster_class)] <- "Cluster Sink"
  df$cluster_category[df$eca_class != df$place] <- "Cluster Source"
  df$cluster_category[df$cluster_class == "Isolated"] <- "Isolated"
  df$cluster_category[df$cluster_class == "ECA Isolated"] <- "Isolated, Cluster Sink"
  
  ### eca_membership gives all places their ECA corrected matching location explicitly
  df$eca_membership <- df$eca_class
  df$eca_membership[df$eca_class == "Isolated"] <- df$place[df$eca_class == "Isolated"]
  
  ### cluster_members_count is a tally of the number of places belonging to a cluster
  df <- df%>% group_by(eca_membership) %>% mutate(cluster_members_count = n())
  
  ### keep only the pertinent variables 
  df <- df %>% select(place, 
                      max_absorption_alpha, 
                      second_max_absorption_alpha, 
                      absorption_alpha_gini, 
                      absorption_alpha_total, 
                      absorption_alpha_mean, 
                      absorption_alpha_sd,
                      match,
                      cluster_category, 
                      eca_membership, 
                      cluster_members_count)
}

############ Test for non-singular row-wise absorption potential maximums 
absorption_max_check <- function(connectedness_table, 
                                 list_names = NULL, 
                                 max_absorption_alpha = max_absorption_alpha, 
                                 second_max_absorption_alpha = second_max_absorption_alpha){
  x <- connectedness_table %>% 
    {any(.$max_absorption_alpha == .$second_max_absorption_alpha)}
  stopifnot("Absorption potential maximum is non-singular" = x == FALSE) 
  paste(list_names, "absorption maximums are all singular")
}

############ Join spatial and other location specific information to ECA classification data tables
join_space_with_connectedness <- function(connectedness_table,
                                          space_data, 
                                          join_variable = place){
  df <- inner_join(space_data, connectedness_table, by = deparse(substitute(join_variable)), copy = TRUE)
}

############ Single function of nested functions to derive connectedness tables from an output matrix and direct requirements matrix of multiple industry specificity levels
direct_connectedness <- function(technical_coefficients_matrix, industry_output_matrix, threshold=.05){
  tc <- technical_coefficients_matrix
  io <- industry_output_matrix
  df <- lapply(mapply(normalized_absorption_share, mapply(stacked_absorption_share, mapply(net_input_supply, io, mapply(industry_input, tc, io)), mapply(net_input_demand, io, mapply(industry_input, tc, io))), mapply(net_input_supply, io, mapply(industry_input, tc, io))), absorption_maximum_match, threshold) 
}

############ Single function of nested functions to derive a single connectedness table from a single output matrix and a single direct requirements matrix
one_direct_connect <- function(technical_coefficients_matrix, industry_output_matrix, threshold=.05){
  tc <- technical_coefficients_matrix
  io <- industry_output_matrix
  df <- absorption_maximum_match(normalized_absorption_share(stacked_absorption_share(net_input_supply(io, industry_input(tc, io)), net_input_demand(io, industry_input(tc, io))), net_input_supply(io, industry_input(tc, io))), threshold)  
}


############ Call Commodity Factor Demand
commodity_factor_demand_tidy_matrix <- function(cbp_year,
                                 ilevel = c("det", "sum", "sec"),
                                 ...){
  ilevel <- match.arg(ilevel)
  bea_year <- year2bea(cbp_year, ilevel, ...)
  o <- total_output_tidy(cbp_year = cbp_year, ilevel = ilevel, ...)
  d <- b_matrix(bea_year, ilevel, ...)[, rownames(o)]
  df <- industry_input(d, o)
  return(df)
}

############ Call Industry Factor Demand
industry_factor_demand_tidy_matrix <- function(cbp_year,
                                                ilevel = c("det", "sum", "sec"),
                                                ...){
  ilevel <- match.arg(ilevel)
  bea_year <- year2bea(cbp_year, ilevel, ...)
  o <- total_output_tidy(cbp_year = cbp_year, ilevel = ilevel, ...)
  df <- bea_io$get_sup(strtoi(bea_year), ilevel, F)[,"T007", drop=F]
  df[is.na(df)] = 0
  rownames(df)[df == 0]
  dm <- d_matrix(bea_year, ilevel, ...)
  bm <- b_matrix(bea_year, ilevel, ...)
  d <- (dm[,setdiff(colnames(dm), rownames(df)[df == 0])] %*% bm[setdiff(colnames(dm), rownames(df)[df == 0]),])[rownames(o), rownames(o)]
  df <- industry_input(d, o)
  return(df)
}


############ Call tidy Gross Commodity Output
commodity_output_tidy_matrix <- function(cbp_year,
                                         ilevel = c("det", "sum", "sec"),
                                         ...){
  ilevel <- match.arg(ilevel)
  bea_year <- year2bea(cbp_year, ilevel, ...)
  cm <- c_matrix(bea_year, ilevel, ...)
  tot <- total_output_tidy(cbp_year = cbp_year, ilevel = ilevel, ...)
  df <- commodity_output(cm[, rownames(tot)], tot)
}

############ Derive the Factor Ratio of National Commodity Factor Demand to National Gross Commodity Output
commodity_region_factor_ratio <- function(cbp_year,
                                ilevel = c("det", "sum", "sec"),
                                ...){
  ilevel <- match.arg(ilevel)
  fd <- commodity_factor_demand_tidy_matrix(cbp_year, ilevel, ...)
  co <- commodity_output_tidy_matrix(cbp_year, ilevel, ...)
  df <- rowSums(fd)/rowSums(co) %>% as.matrix()
  colnames(df) <- "factor_ratio"
  
  return(df)
}

############ Derive the Factor Ratio of National Industry Factor Demand to National Gross Industry Output
industry_region_factor_ratio <- function(cbp_year,
                                          ilevel = c("det", "sum", "sec"),
                                          ...){
  ilevel <- match.arg(ilevel)
  fd <- industry_factor_demand_tidy_matrix(cbp_year, ilevel, ...)
  io <- total_output_tidy(cbp_year = cbp_year, ilevel = ilevel, ...)
  df <- rowSums(fd)/rowSums(io) %>% as.matrix()
  colnames(df) <- "factor_ratio"
  
  return(df)
}
 
############ Call Commodity Factor Supply
commodity_factor_supply_tidy_matrix <- function(cbp_year,
                                 ilevel = c("det", "sum", "sec"),
                                 ...){
  co <- commodity_output_tidy_matrix(cbp_year, ilevel, ...)
  fr <- commodity_region_factor_ratio(cbp_year, ilevel, ...)
  df <- diag(as.vector(fr)) %*% co
  rownames(df) <- rownames(fr)
  return(df)
}

############ Call Industry Factor Supply
industry_factor_supply_tidy_matrix <- function(cbp_year,
                                                ilevel = c("det", "sum", "sec"),
                                                ...){
  io <- total_output_tidy(cbp_year = cbp_year, ilevel = ilevel, ...)
  fr <- industry_region_factor_ratio(cbp_year, ilevel, ...)
  df <- diag(as.vector(fr)) %*% io
  rownames(df) <- rownames(fr)
  return(df)
}

#RAS trade matrix
ras_trade_lists <- function(factor_supply,
                            factor_demand, 
                            impedance_mat = NULL,
                            ...){
  
  df <- setequal(rownames(factor_supply), rownames(factor_demand))
  stopifnot("Commodity/Industry names do not match" = df == TRUE)
  
  if(!is.null(impedance_mat)){ 
    rn <- colnames(factor_supply)
    cn <- colnames(factor_demand)
    df <- all(
      setequal(rn, cn),
      setequal(rn, rownames(impedance_mat[rn, cn])), 
      setequal(cn, colnames(impedance_mat[rn, cn]))
    )
    stopifnot("Place names do not match" = df == TRUE)
  }
  
  #Starting position of trade matrix 
  x <- list()
  y <- intersect(
    names(which(!is.na(rowSums(factor_demand)) & 
                !rowSums(factor_demand) == 0 )), 
    names(which(!is.na(rowSums(factor_supply)) & 
                !rowSums(factor_supply) == 0 ))
    )
  for (i in y){
    if(!is.null(impedance_mat)){
      x[[i]] <- (t(factor_supply[i, , drop=F]) %*% factor_demand[i, , drop=F]) * (impedance_mat[rn, cn])
    } else {
      x[[i]] <- (t(factor_supply[i, , drop=F]) %*% factor_demand[i, , drop=F])
    }
  }
  
  df <- list()
  for (i in names(x)){
    x0 = x[[i]]
    r1 = factor_supply[i, , drop=F]
    c1 = factor_demand[i, , drop=F]
    df[[i]] <- ras_trade_flows(x0 = x0,
                               rs1 = r1, 
                               cs1 = c1, ...)
    colnames(df[[i]]) = rownames(df[[i]]) = colnames(factor_demand)
  }
  return(df)
}


############ Call matrices of commodity or industry imputed trade flows from RAS procedure
call_imputed_tradeflows <- function(cbp_year,
                                    ilevel,
                                    industryflow = TRUE,
                                    impedance_mat = NULL,
                                    subsectors = NULL,
                                    totaled = FALSE,
                                    ...){
  if(!"ras_trade_fls" %in% c(lsf.str())){
    source(file.path(find_rstudio_root_file(), "nbs", "io_analysis.R"))
  }
  if(isTRUE(industryflow)){
    fs <- industry_factor_supply_tidy_matrix(cbp_year = cbp_year, 
                                              ilevel = ilevel,
                                              ...)
    fd <- industry_factor_demand_tidy_matrix(cbp_year = cbp_year, 
                                              ilevel = ilevel,
                                              ...)
  } else {
    fs <- commodity_factor_supply_tidy_matrix(cbp_year = cbp_year, 
                                              ilevel = ilevel,
                                              ...)
    fd <- commodity_factor_demand_tidy_matrix(cbp_year = cbp_year, 
                                              ilevel = ilevel,
                                              ...)
  }
  df <- setequal(rownames(fs), rownames(fd))
  stopifnot("Commodity/Industry names do not match" = df == TRUE)
  if(!is.null(subsectors)){
    fs <- fs[subsectors, , drop = FALSE]
    fd <- fd[subsectors, , drop = FALSE]
  }
  y <- intersect(
    names(which(!is.na(rowSums(fd)) & 
                  !rowSums(fd) == 0 )), 
    names(which(!is.na(rowSums(fs)) & 
                  !rowSums(fs) == 0 ))
  )
  if(!is.null(impedance_mat)){
    rn <- colnames(fs)
    cn <- colnames(fd)
    df <- all(
      setequal(rn, cn),
      setequal(rn, rownames(impedance_mat[rn, cn])),
      setequal(cn, colnames(impedance_mat[rn, cn]))
    )
    stopifnot("Place names do not match" = df == TRUE)
  }
  temp <- tempdir()
  for (i in y){
    if(!is.null(impedance_mat)){
      x0 <- (t(fs[i, , drop=F]) %*% fd[i, , drop=F]) * (impedance_mat[rn, cn])
    } else {
      x0 <- (t(fs[i, , drop=F]) %*% fd[i, , drop=F])
    }
    r1 <- fs[i, , drop=F]
    c1 <- fd[i, , drop=F]
    tf <- ras_trade_flows(x0 = x0,
                          rs1 = r1, 
                          cs1 = c1,
                          ...)
    colnames(tf) = colnames(c1)
    rownames(tf) = colnames(r1)
    saveRDS(tf, file = file.path(temp, i))
    rm(tf, x0, r1, c1)
  }
  df <- lapply(file.path(temp, y), readRDS)
  names(df) <- y 
  if(isTRUE(totaled)){
   df <- Reduce('+', df)
  }
  return(df)
}


load_tradeflows <- function(cbp_year,
                            ilevel,
                            industryflow = TRUE,
                            impedance_mat = NULL,
                            subsectors = NULL,
                            totaled = FALSE,
                            data_dir = file.path("data", "robjs"),
                            impedance_call = NULL, 
                            ...){
  
  if(!is.null(impedance_mat) & is.null(impedance_call)){warning("Impedance is applied without a specific description to save/load")}
  
  rasf <- paste0("ras", "_", ilevel,"class", "_", cbp_year, "cbp", "_", if(isTRUE(industryflow)){"industries"}else{"commodities"}, "_", if(is.null(subsectors)){"all"}else{subsectors}, "sectors", "_", if(is.null(impedance_mat)){"NA"}else{impedance_call}, "impedance")
  stop
  if (file.exists(file.path(find_rstudio_root_file(), data_dir, rasf) ) ){ 
    df <- readRDS(file.path(find_rstudio_root_file(), data_dir, rasf))
  } else {
    df <- call_imputed_tradeflows(cbp_year,
                                  ilevel,
                                  industryflow,
                                  impedance_mat,
                                  subsectors,
                                  totaled,
                                  ...)
    saveRDS(df,  file = file.path(find_rstudio_root_file(), data_dir, rasf) )
  }
  return(df)
}



### Summarize data from trade flow matrix
inbound2outbound <- function(trade_matrix){
  df <- data.frame("place" = rownames(trade_matrix),
                   "inbound" = colSums(trade_matrix), 
                   "outbound" = rowSums(trade_matrix), 
                   "out2in" = rowSums(trade_matrix)/colSums(trade_matrix), 
                   "out_less_in" = rowSums(trade_matrix)-colSums(trade_matrix)
                   )
  return(df)
}


matrix2edgelist <- function(x){
  df <- do.call(cbind, 
                lapply(list("row_index" = row(x), 
                            "col_index" = col(x), 
                            "value" = x), 
                       as.vector))
}

edgelist2matrix <- function(x){
  df <- matrix(x[,3], 
               nrow = length(unique(x[,1])), 
               ncol = length(unique(x[,2])), 
               dimnames = list(unique(x[,1]), 
                               unique(x[,2])))
  return(df)
}





############ Call connectedness matrix for any available year and industry scale
absorption_matrix <- function(cbp_year,
                              ilevel = c("det", "sum", "sec"),
                              cbsa_clust = FALSE, 
                              normalized = TRUE,
                              impedance = NULL, 
                              data_dir = file.path("data", "robjs"),
                              ...){
  ilevel <- match.arg(ilevel)
  ag_year <- year2agcensus(cbp_year, ...)
  bea_year <- year2bea(cbp_year, ilevel, ...)
  cbsa_year <- year2cbsa(cbp_year, ...)  %>% suppressWarnings() 
  tiger_year <- year2tiger(cbp_year, ...) %>% suppressWarnings()
  o <- total_output_tidy(cbp_year = cbp_year, ilevel = ilevel, cbsa_clust = cbsa_clust, ...)
  d <- b_matrix(bea_year, ilevel, ...)[, rownames(o)]
  i <- industry_input(d, o)
  oc <- c_matrix(bea_year, ilevel, ...)[, rownames(o)]
  co <- commodity_output(oc, o)
  sasf <- paste0("sas", "_", match.arg(ilevel),"class", "_", cbp_year, "cbp", "_", bea_year, "bea", "_", ag_year, "ag", "_", tiger_year, "tiger", "_", if(isTRUE(cbsa_clust)){cbsa_year}else{"NA"}, "cbsa")
  if (file.exists(file.path(find_rstudio_root_file(), data_dir, sasf) ) ){ 
    df <- readRDS(file.path(find_rstudio_root_file(), data_dir, sasf))
  } else {
    df <- stacked_absorption_share(net_input_supply(co, i), net_input_demand(co, i))
    saveRDS(df,  file = file.path(find_rstudio_root_file(), data_dir, sasf) )
  }
  if(isTRUE(normalized)){
    df <- normalized_absorption_share(df, net_input_supply(co, i))
  }
  if(!is.null(impedance)){
    df <- df * impedance[colnames(df), rownames(df)]
  }
 return(df)
}

############ Call connectedness for any available year and industry scale
connectedness <- function (cbp_year,
                           threshold = .05,
                           row_max_match = TRUE,
                           ...){
  df <- absorption_matrix(cbp_year, ...)
  df <- absorption_maximum_match(absorption_matrix = df, 
                                 threshold = threshold, 
                                 row_max_match = row_max_match,
                                 ...)
  return(df)
}

############ industry (output, input, nid, nis) distributions by county
industry_distribution <- function(industry_aggregate_class = c("sec", "sum", "det"),
                                  cbp_year,
                                  cbsa_clust = FALSE,
                                  ilevel = c("det", "sum", "sec"),
                                  ...){
  industry_aggregate_class <- match.arg(industry_aggregate_class)
  ilevel <- match.arg(ilevel)
  bea_year <- year2bea(cbp_year, ilevel, ...)
  o <- total_output(cbp_year = cbp_year, ilevel = ilevel, ...)
  if(isTRUE(cbsa_clust)){
    o <- cbsa_aggregate_industry_output(cbp_year, ...)
  }
  d <- call_direct_requirements(bea_year, ilevel, ...)
  if(ilevel == "det"){ 
    con <- grep("^23", colnames(d), value = TRUE)
    cm <- matrix(sum(d[con,con])/length(con), 
                 dimnames = list(c("23"), c("23")))
    cr <- t(matrix(colMeans(d[con, ]), 
                   dimnames = list(colnames(d), c("23")) ))
    cc <- matrix(rowMeans(d[, con]), 
                 dimnames = list(colnames(d), c("23"))) 
    d <- cbind( rbind(cr, d ), rbind(cm, cc )) 
    d <- d[colnames(d)[!colnames(d) %in% con] , colnames(d)[!colnames(d) %in% con]]
    d <- d[rownames(o)[rownames(o) %in% rownames(d)], rownames(o)[rownames(o) %in% colnames(d)]] 
  }
  i <- industry_input(d, o)
  nis <- net_input_supply(o, i)
  nid <- net_input_demand(o, i)
  indicator_type = c("output", "input", "nis", "nid")
  df <- vector("list", length(indicator_type))
  names(df) <- indicator_type
  df[[1]] <- o
  df[[2]] <- i
  df[[3]] <- nis
  df[[4]] <- nid
  for (i in 1:length(df)){
    df[[i]] <- melt(df[[i]])
    df[[i]] <- df[[i]] %>% 
      filter(value > 0)
    names(df[[i]]) <- c("DETAIL", "place", indicator_type[i])
    df[[i]]$place <- df[[i]]$place  %>% formatC(width = 5, format = "d", flag = "0")
  }
  des <- data.frame(names(bea_io$get_sup(strtoi(bea_year), industry_aggregate_class, FALSE)), 
                    names(bea_io$get_sup(strtoi(bea_year), industry_aggregate_class, TRUE)))
  if(industry_aggregate_class=="det"){bc <- "DETAIL"}
  if(industry_aggregate_class=="sum"){bc <- "SUMMARY"}
  if(industry_aggregate_class=="sec"){bc <- "SECTOR"}
  names(des) <- c(bc, "Description")
  conc <- call_industry_concordance() %>% select(-c("DESCRIPTION", "U_SUMMARY", "NAICS"))
  sn <- left_join(conc, des, by = bc) 
  pal <- data.frame(color = viridis(length(unique(sn$Description))),
                    Description = unique(sn$Description))
  sn <- inner_join(sn, pal, by = "Description") %>% 
    distinct(DETAIL, .keep_all = TRUE) 
  sn$abv <- sn[[bc]]
  for (i in names(df)){
    df[[i]] <- inner_join(df[[i]], sn, by = "DETAIL")
  }
  return(df)
}


############ Bar charts of industry distributions by county
industry_distribution_barcharts <- function(data,
                                            interact = TRUE,
                                            short = TRUE,
                                            ...){
  df <- c()
  l <- unique(distinct(data, DETAIL, abv)[order(distinct(data, DETAIL, abv)$DETAIL),]$abv)
  for (i in 1:length(unique(data$place))){
    d <- data[data$place == unique(data$place)[i], ]
    
    if(isTRUE(interact)){
      df[[i]] <- ggplot(d, aes(if(isTRUE(short)){x = factor(abv, levels = l)}else{x = factor(name, levels = unique(name))},
                               y = .data[[names(data)[3]]],
                               fill = DETAIL,
                               tooltip = glue("industry: {DETAIL}\n{names(data)[3]}: {round(.data[[names(data)[3]]], 0)}"),  
                               data_id = DETAIL)) +
        geom_col_interactive(color = NA)
    } else {
      df[[i]] <- ggplot(d, aes(if(isTRUE(short)){x = factor(abv, levels = l)}else{x = factor(name, levels = unique(name))},
                               y = .data[[names(data)[3]]],
                               fill = DETAIL)) +
        geom_col(color = NA)
    }
    df[[i]] <- df[[i]] +
      scale_fill_manual(values = d$color) +
      theme_minimal() +
      theme(axis.text.x = element_text(size = rel(.75), angle = 300, hjust = 0),
            axis.text.y = element_text(size = rel(.75)),
            plot.subtitle = element_text(size = rel(.75), vjust = -2), 
            plot.margin = margin(t = 0, r = 50, b = 0, l = 0, unit = "pt")) +
      labs(x = element_blank(),
           y = element_blank(),
           subtitle = names(data)[3],
           title = element_blank()) +
      guides(fill = "none")
    names(df)[i] <- unique(data$place)[i]
  }
  return(df)
}

############ html plots of bar charts of industry distributions by county [used in mapping interactive tooltips]
html_industry_dist_plots <- function(cbp_year,
                                     short = TRUE,
                                     ...){
  t <- industry_distribution(cbp_year = cbp_year, ...)
  b <- vector("list")
  for (i in names(t) ){
    b[[i]] <- industry_distribution_barcharts(t[[i]], 
                                              interact = FALSE, 
                                              short = short)
  }
  df <- vector("list", length = length(b))
  names(df) <- names(b)
  for (i in names(df)){
    print(i)
    print(Sys.time())
   df[[i]] <- vector("list", length = length(b[[i]]))
   for (p in 1:length(df[[i]])){
     df[[i]][p] <- htmltools::plotTag(b[[i]][p], alt = "") %>% as.character()
   }
   names(df[[i]])<-names(b[[i]])
  }
  return(df)
}

###Need to make adding html a separate operation
############ Call connectedness for any available year and industry scale with spatial component
spatial_connectedness <- function(cbp_year,
                                  cbsa_clust = FALSE,
                                  add_html = FALSE,
                                  industry_aggregate_class = "sec",
                                  data_dir = file.path("data", "robjs"),
                                  ...){
  tiger_year <- year2tiger(cbp_year, ...) %>% suppressWarnings() 
  c <- connectedness(cbp_year = cbp_year, cbsa_clust = cbsa_clust, ...)
  ag_year <- year2agcensus(cbp_year, ...) %>% suppressWarnings()
  if(isFALSE(cbsa_clust)){
    s <- call_tiger(tiger_year, ...) 
  } else {
    s <- cbsa_spatial_cluster(cbp_year, ...)
    s <- rename(s, place = CBSA_CODE)
    s <- rename(s, NAME = CBSA_TITLE)
    cbsa_year <- year2cbsa(cbp_year, ...)  %>% suppressWarnings() 
  }
  df <- join_space_with_connectedness(c, s)
  if(isTRUE(add_html)){
    hp <- paste0("htmlplots", "_", industry_aggregate_class, "class", "_", cbp_year, "cbp", "_",  ag_year, "ag", "_", tiger_year, "tiger", "_", if(isTRUE(cbsa_clust)){cbsa_year}else{"NA"}, "cbsa")
    if (file.exists(file.path(find_rstudio_root_file(), data_dir, hp))){ 
      h <- readRDS(file.path(find_rstudio_root_file(), data_dir, hp))
    } else {
      h <- html_industry_dist_plots(cbp_year,
                                    ...)
      saveRDS(h, file = file.path(find_rstudio_root_file(), data_dir, hp))
    }
    indicator_type = names(h)
    for(i in indicator_type){
      df[[paste0("html_", i) ]] <- h[[i]]
    }
  } 
  return(df)
}

############ Spatial union each ECA member in a cluster
spatial_cluster <- function(spatial_connectedness_table,
                            quiet = TRUE,
                            ...){
  df <-  spatial_connectedness_table %>% select(names(.)[!(names(.) %in% c("center", "html_output", "html_input", "html_nis", "html_nid"))])
  x <- df$eca_membership %>% unique() %>% .[order(.)]
  for (i in x){
    if(!quiet == TRUE){print(paste("start cluster: ", i, which(i == x), "of", length(x), Sys.time()))}
    df[df$place == i,]$geometry <- df %>% filter(df$eca_membership == i) %>% st_union()
    if(!quiet == TRUE){print(paste("end cluster: ", i, which(i == x), "of", length(x), Sys.time()))}
  }
  df <- df %>% .[.$place %in% .$eca_membership, ]
}

############ Call spatial connectedness for any available year and industry scale aggregating eca clusters across space
cluster_spatial_connectedness <- function (cbp_year,
                                           list_names = NULL,
                                           ...){
  df <- spatial_connectedness(cbp_year, ...)
  x <- df$eca_membership %>% unique() %>% .[order(.)]
  for (i in x){
    print(paste(list_names, "start cluster: ", i, which(i == x), "of", length(x), Sys.time()))
    df[df$place == i,]$geometry <- df %>% filter(df$eca_membership == i) %>% st_union()
    print(paste(list_names, "  end cluster: ", i, which(i == x), "of", length(x), Sys.time()))
  }
  df <- df %>% .[.$place %in% .$eca_membership, ]
  return(df)
}

############ Single function of nested functions to derive a hierarchies of connectedness tables and resulting output matrices from a base single output matrix and single direct requirements matrix
one_hierarchical_connectedness <- function(cbp_year,
                                           ilevel = c("det", "sum", "sec"),
                                           cbsa_clust = FALSE,
                                           normalized = TRUE,
                                           impedance = NULL,
                                           data_dir = file.path("data", "robjs"),
                                           queen = TRUE,
                                           ...){
  ilevel <- match.arg(ilevel)
  cbsa_year <- year2cbsa(cbp_year, ...)  %>% suppressWarnings() 
  ag_year <- year2agcensus(cbp_year, ...)
  bea_year <- year2bea(cbp_year, ilevel, ...)
  tiger_year <- year2tiger(cbp_year, ...) %>% suppressWarnings()
  sasf <- paste0("hier", "_", ilevel,"class", "_", cbp_year, "cbp", "_", if(isTRUE(cbsa_clust)){cbsa_year}else{"NA"}, "cbsa", "_", if(is.null(impedance)){"NA_impedance"}else if(is.numeric(impedance)){paste0(impedance,"_impedance")}else{if(queen==TRUE){"queenborder_impedance"}else{"rookborder_impedance"}} )
  if (file.exists(file.path(find_rstudio_root_file(), data_dir, sasf) ) ){ 
    df <- readRDS(file.path(find_rstudio_root_file(), data_dir, sasf))
  } else {
    o <- total_output_tidy(cbp_year = cbp_year, ilevel = ilevel, cbsa_clust = cbsa_clust, ...)
    d <- call_direct_requirements(bea_year, ilevel, ...)
    if(ilevel == "det"){ 
      con <- grep("^23", colnames(d), value = TRUE)
      cm <- matrix(sum(d[con,con])/length(con), 
                   dimnames = list(c("23"), c("23")))
      cr <- t(matrix(colMeans(d[con, ]), 
                     dimnames = list(colnames(d), c("23")) ))
      cc <- matrix(rowMeans(d[, con]), 
                   dimnames = list(colnames(d), c("23"))) 
      d <- cbind( rbind(cr, d ), rbind(cm, cc )) 
      d <- d[colnames(d)[!colnames(d) %in% con] , colnames(d)[!colnames(d) %in% con]]
      d <- d[rownames(o)[rownames(o) %in% rownames(d)], rownames(o)[rownames(o) %in% colnames(d)]] 
    }
    hct <- list()
    hsct <- list()
    hsct$level_0 <- spatial_connectedness(cbp_year, ...)
    hom <- list()
    hom$level_0 <- o
    n = 1
    i = FALSE
    while(i == FALSE){
      print(paste("level", n))
      ii <- industry_input(d, o)
      nis <- net_input_supply(o, ii)
      nid <- net_input_demand(o, ii)
      df <- stacked_absorption_share(nis, nid)
      if(isTRUE(normalized)){
        df <- normalized_absorption_share(df, nis)
      }
      if(is.null(impedance)){
        df <- df
        } else if(is.numeric(impedance)){
          impd = st_is_within_distance(hsct[[paste0("level_", n-1)]]$geometry, dist = miles2meters(impedance))
          impd <- +as.matrix(impd)
          diag(impd) <- 0
          rownames(impd) = colnames(impd) <- hsct[[paste0("level_", n-1)]]$place
          df <- df * impd[colnames(df), rownames(df)]
        } else {
        impd = hsct[[paste0("level_", n-1)]]$geometry %>% 
          poly2nb(queen = queen) %>%
          nb2mat(style = "B", zero.policy = TRUE)
        rownames(impd) = colnames(impd) <- hsct[[paste0("level_", n-1)]]$place
        df <- df * impd[colnames(df), rownames(df)]
      }
      c <- absorption_maximum_match(absorption_matrix = df, ...)  
      s <-  hsct[[paste0("level_", n-1)]] %>% select(c("place", "NAME", "STATE_CODE", "COUNTY_CODE", "COUNTY", "STATE_NAME", "STATE", "geometry" ))
      hct[[paste0("level_", n)]] <- join_space_with_connectedness(c, s)
      i <- all(c$place %in% c$eca_membership) 
      if (i == TRUE){next}
      hsct[[paste0("level_", n)]] <- hct[[paste0("level_", n)]] %>% spatial_cluster()
      o <- aggregate_industry_output(o, c)
      hom[[paste0("level_", n)]] <- o
      n = n + 1
    }
    df <- list("Hierarchical_Connectedness_table" = hct,
               "Hierarchical_Spatial_Cluster_table" = hsct,
               "Hierarchical_Output_mat" = hom)
    saveRDS(df,  file = file.path(find_rstudio_root_file(), data_dir, sasf) )
  }
  return(df)
}

###Need to make adding html a separate operation
############ Place-centric connectedness
place_centric_connect <- function(central_place,
                                  cbp_year,
                                  cbsa_clust = FALSE,
                                  add_html = FALSE,
                                  industry_aggregate_class = "sec",
                                  data_dir = file.path("data", "robjs"),
                                  ...){
  df <- absorption_matrix(cbp_year, cbsa_clust = cbsa_clust, ...)
  if(isTRUE(cbsa_clust)){central_place <- fips2cbsa(central_place)}
  df <- cbind(export_absorption = c(t(df[central_place, , drop = FALSE])),
              import_absorption = c(df[, central_place , drop = FALSE]),
              place = rownames(df)) 
  tiger_year <- year2tiger(cbp_year, ...) 
  ag_year <- year2agcensus(cbp_year, ...) %>% suppressWarnings()
  if(isFALSE(cbsa_clust)){
    s <- call_tiger(tiger_year, ...) 
  } else {
    s <- cbsa_spatial_cluster(cbp_year, ...)
    s <- rename(s, place = CBSA_CODE)
    s <- rename(s, NAME = CBSA_TITLE)
    cbsa_year <- year2cbsa(cbp_year, ...)  %>% suppressWarnings() 
  }
  df <- join_space_with_connectedness(df, s)
  df$export_absorption <- as.numeric(df$export_absorption)
  df$import_absorption <- as.numeric(df$import_absorption)
  if(isTRUE(add_html)){
    hp <- paste0("htmlplots", "_", industry_aggregate_class, "class", "_", cbp_year, "cbp", "_",  ag_year, "ag", "_", tiger_year, "tiger", "_", if(isTRUE(cbsa_clust)){cbsa_year}else{"NA"}, "cbsa")
    if (file.exists(file.path(find_rstudio_root_file(), data_dir, hp))){ 
      h <- readRDS(file.path(find_rstudio_root_file(), data_dir, hp))
    } else {
      h <- html_industry_dist_plots(cbp_year,
                                    ...)
      saveRDS(h, file = file.path(find_rstudio_root_file(), data_dir, hp))
    }
    indicator_type = names(h)
    for(i in indicator_type){
      df[[paste0("html_", i) ]] <- h[[i]]
    }
  } 
  return(df)
}

############ Change in connectedness over time for a county 
place_connect_delta <- function(central_place, 
                                sample_years, # vector of years
                                ...){
  n <- c("export_absorption", "import_absorption", "html_output", "html_input", "html_nis", "html_nid")
  place_connect <- vector("list", length(sample_years))
  names(place_connect) <- sample_years
  for (i in sample_years){
    place_connect[[i]] <- place_centric_connect(central_place = central_place,
                                                cbp_year = i,
                                                ...)
    x <- n[n %in% names(place_connect[[i]])]
    names(place_connect[[i]])[names(place_connect[[i]]) %in% x] <- paste(x, i, sep=".")
  }
  df <- place_connect[[1]]
  for(i in 2:length(sample_years)){
    df <- st_set_geometry(place_connect[[i]], NULL) %>% .[c(grep("*[0-9]", colnames(.), value = TRUE), "place")] %>% inner_join(df, ., by = "place")
  }
  com <- combn(sample_years, 2)
  for (x in 1:ncol(com)){
    df[[paste0("export_absorption_delta_", substr(com[2, x], 3, 4), substr(com[1, x], 3, 4))]] <- df[[paste0("export_absorption.", com[1, x])]] - df[[paste0("export_absorption.", com[2, x])]]
    df[[paste0("import_absorption_delta_", substr(com[2, x], 3, 4), substr(com[1, x], 3, 4))]] <- df[[paste0("import_absorption.", com[1, x])]] - df[[paste0("import_absorption.", com[2, x])]]
  }
  ###Percent Change option
  # for (x in 1:ncol(com)){
  #   df[[paste0("export_absorption_delta_", substr(com[2, x], 3, 4), substr(com[1, x], 3, 4))]] <- (df[[paste0("export_absorption.", com[1, x])]] - df[[paste0("export_absorption.", com[2, x])]])/df[[paste0("export_absorption.", com[1, x])]]*100
  #   df[[paste0("import_absorption_delta_", substr(com[2, x], 3, 4), substr(com[1, x], 3, 4))]] <- (df[[paste0("import_absorption.", com[1, x])]] - df[[paste0("import_absorption.", com[2, x])]])/df[[paste0("import_absorption.", com[1, x])]]*100
  # }
return(df)
}

############ Generate neighbors of neighbors for a place ad nauseam
#Watch out for Nantucket, MA; Staten Island,NY; and San Juan, WA
neighbor_of_neighbor <- function(central_place = "20183",
                                 year = "2012",
                                 quiet = TRUE,
                                 ...){
  t <- year2tiger(year) %>% call_tiger(...)
  t <- t[t$STATE_CODE != "02" & 
         t$STATE_CODE != "15" & 
         t$STATE_CODE != "72", ]
  df <- data.frame("place" = t$place, 
                   "nn" = vector(mode = "character", length = length(t$place)))
  df$nn[df$place %in% central_place] <- "n0"
  nn <- central_place
  l <- 1
  x <- 0
  while(sum(df$nn=="") != x){
    x = sum(df$nn=="")
    tp <- st_touches(t$geometry, 
                     t[t$place %in% nn, ]$geometry)
    tp <- +as.matrix(tp)
    rownames(tp) <- t$place
    colnames(tp) <- c(nn)
    nx <- setdiff(rownames(tp)[apply(tp, 1, function(x){any(x==1)})], nn)
    df$nn[df$place %in% nx] <- paste0("n",l)
    nn <- c(nn, nx)
    l = l + 1
    if(!quiet == TRUE){
      print(paste("Interval level:", l))
      print(paste("Places remaining:", sum(df$nn=="")))
    }
  }
  return(df)
}

############ hierarchy GIF generator
national_hierarchy_gif <- function(df,
                                  folder,
                                  map_function, 
                                  caption = NULL,
                                  ...){
  r <- file.path(find_rstudio_root_file(), "data", folder)
  if(!dir.exists(r)){
    dir.create(r)
  }
  fn <- list()
  lev <- list(names(df))
  for(l in names(df)){
    fp <- file.path(r, paste0(folder, "_", l, ".png"))
    if(!file.exists(fp)){ 
      argList <- list()
      argList$df <- df[[l]]
      argList$caption <- glue("Level {which(names(df) == l)} Hierarchy\n 5% Isolation Threshold")
      do.call(map_function, argList)
      ggsave(fp)
    }
    fn[[which(names(df) == l)]] <- fp
  }
  anim <- fn %>% unlist() %>% lapply(image_read) %>% image_join() %>% image_animate(fps = 1)
  image_write(image = anim,
              path = file.path(r, paste0(folder, ".gif")))
}

############ Distance impedance map GIF generator
national_progressiveimpedance_gif <- function(cbp_year,
                                              dist,
                                              impd,
                                              folder,
                                              map_function,
                                              ...){
  r <- file.path(find_rstudio_root_file(), "data", folder)
  if(!dir.exists(r)){
    dir.create(r)
  }
  fn <- list()
  tiger_year = year2tiger(cbp_year) %>% suppressWarnings()
  for(d in dist){
    fp <- file.path(r, paste0(folder, "_", d, ".png"))
    if(!file.exists(fp)){
      if(impd == "B"){
        impedance = dist_matb(dist = miles2meters(d), 
                              tiger_year = tiger_year)
      }
      if(impd == "C"){
        impedance = dprox_mat(boundary_limit = miles2meters(d), 
                              tiger_year = tiger_year)
      }
      df <- spatial_connectedness(cbp_year = cbp_year,
                                  impedance = impedance,
                                  ...)
      argList <- list()
      argList$df <- df
      argList$caption <- glue("{d} Mile Impedance \n 5% Isolation Threshold")
      g <- do.call(map_function, argList)
      ggsave(fp)
    }
    fn[[which(dist == d)]] <- fp
  }
  anim <- fn %>% unlist() %>% lapply(image_read) %>% image_join() %>% image_animate(fps = 1)
  image_write(image = anim,
              path = file.path(r, paste0(folder, ".gif")))
}


############ Aggregate economic industry output of each ECA member in a cluster, keep all non source places as ECA core unit label
aggregate_industry_output <- function(industry_output_matrix, 
                                      connectedness_table,
                                      ...){
  df <- industry_output_matrix
  c <- connectedness_table
  x <- c$eca_membership %>% unique() %>% .[order(.)]
  for(i in x){
    df[, i] <- rowSums(df[, c$place[c$eca_membership == i], drop = FALSE])
  } 
  df <- df[, x]
}

############ Multi plot of bar charts of industry distributions for a given county
industry_dist_plots <- function(central_place,
                                cbp_year,
                                cbsa_clust = FALSE,
                                ...){
  if(isTRUE(cbsa_clust)){central_place <- fips2cbsa(central_place)}
  idis <- industry_distribution(cbp_year = cbp_year, ...)
  bd <-  vector("list")
  for (i in names(idis) ){
    x <- idis[[i]][idis[[i]]$place == central_place, ] 
    bd[[i]] <- industry_distribution_barcharts(data = x, ...)
  }
  pobj <- plot_grid(bd$output[[central_place]],
                    bd$input[[central_place]],
                    bd$nis[[central_place]],
                    bd$nid[[central_place]])
  girafe(ggobj = pobj, 
         options = list(
           opts_hover(
             css = girafe_css(
               css = "stroke:gray;r:8pt;",
               text = "stroke:none;fill:black;fill-opacity:1;" ) ),
           opts_tooltip(css = "font-family:sans-serif;
                                             background-color:gray;
                                             color:white;
                                             padding:10px;
                                             border-radius:5px;") ))
}

############ Absorption matching outcomes over time
absorption_match_overtime <- function(years = 2000:2020,
                                      threshold = 0,
                                      normalized = TRUE,
                                      ...){
  dis <- vector("list", length(years))
  names(dis) <- years
  for (y in years){
    dis[[y]] <- connectedness(cbp_year = as.character(y),
                              threshold = threshold,
                              normalized = normalized)
  }
  df <- bind_rows(dis, .id = "id")
  return(df)
}

############ Map arbitrary qualitative data (four color theorem) 
aqual_map <- function(df,
                      fcmt = FALSE,
                      tooltip = glue("Place: {NAME}\nECA: {eca_membership}"),
                      data_id = eca_membership,
                      ncols = 8,
                      minimize = FALSE,
                      palette = "Set2",
                      caption = NULL,
                      ...){
  if(fcmt == FALSE){
    g <- ggplot(df) + 
      geom_sf_interactive(aes(fill = factor(max_absorption_alpha), 
                              tooltip = {{tooltip}}, 
                              data_id = {{data_id}})) + 
      theme_void() +
      theme(legend.position = "none") + 
      labs(caption = caption)
  } else {
    stopifnot("dataframe needs sf geometry" = "sf" %in% class(df) == TRUE) 
    if(!"Ncol" %in% names(df)){
      df[["Ncol"]] <- map_coloring(df$geometry,
                                 ncols = ncols,
                                 minimize = minimize)
    }
      g <- ggplot(df) + 
        geom_sf_interactive(aes(fill = factor(Ncol), 
                                tooltip = {{tooltip}}, 
                                data_id = {{data_id}})) + 
        theme(legend.position = "none") + 
        scale_fill_brewer(palette = palette) + 
        labs(caption = caption)
  }
  girafe(ggobj = g,
         options = list(
           opts_hover(
             css = girafe_css(
               css = "stroke:gray;r:8pt;fill:orange",
               text = "stroke:none;fill:black;fill-opacity:1;" ) ),
           opts_zoom(max = 5),
           opts_selection(type = "multiple", 
                          only_shiny = FALSE,
                          css = "fill:black"),
           opts_sizing = opts_sizing(rescale = TRUE),
           opts_tooltip(offx = 20, offy = 20,
                        css = "font-family:sans-serif;
                               background-color:gray;
                               color:white;
                               padding:10px;
                               border-radius:5px;",
                        use_cursor_pos = TRUE) ))
}


############ Map change in connectedness over time for a county 
absorption_delta_map <- function(central_place,
                         df, 
                         fill,
                         delta_min,
                         delta_max){
  g <- ggplot(df) +
    geom_sf_interactive(aes(fill = .data[[fill]], 
                            tooltip = if("STATE" %in% names(df)){
                              glue("{NAME}, {STATE}\nFIPS: {place}\nAbsorption Change: {round(.data[[fill]], 5)}")
                            }else{
                              glue("{NAME}\nFIPS: {place}\nAbsorption Change: {round(.data[[fill]], 5)}")
                            },
                            data_id = place
    ), 
    color = NA
    ) + 
    guides(alpha = "none") +
    coord_sf() +
    theme_void() +
    scale_fill_gradientn(colors = c("#d7191c", "#ffffbf", "#2b83ba"), values=rescale(c(delta_min, 0, delta_max)), limits=c(delta_min, delta_max)) +
    geom_sf_interactive(data = df[df$place==central_place,], fill = "#A020F0", color = NA) +
    labs(fill = if("STATE" %in% names(df)){
      glue("{df[df$place==central_place,]$NAME}, {df[df$place==central_place,]$STATE} \nAggregate: {round(sum(df[[fill]]), 3)} \nAbsorption Change")
    }else{
      glue("{df[df$place==central_place,]$NAME} \nAggregate: {round(sum(df[[fill]]), 3)} \nAbsorption Change")
    } )  +
    theme(plot.margin = margin(t = 1, r = 1, b = 10, l = 1, unit = "pt"),
          legend.key.size = unit(.2, "cm"),
          legend.title = element_text(size = rel(0.5), color = "white"), 
          legend.text = element_text(size = rel(0.5), color = "white"),
          legend.position = c(0.9, 0.3),
          panel.background = element_rect(fill = "black")) 
  
  
  girafe(ggobj = g, 
         options = list(
           opts_hover(
             css = girafe_css(
               css = "stroke:gray;r:8pt;",
               text = "stroke:none;fill:black;fill-opacity:1;" ) ),
           opts_zoom(max = 5),
           opts_sizing = opts_sizing(rescale = TRUE),
           opts_tooltip(css = "font-family:sans-serif;
                                           background-color:gray;
                                           color:white;
                                           padding:10px;
                                           border-radius:5px;",
                        use_cursor_pos = TRUE) ))
}


############ Map absorption metrics
absorption_map <- function(df, 
                           add_html = FALSE,
                           fill = "max_absorption_alpha", 
                           fill_lab = "Max Absorption",
                           unit_scale = TRUE,
                           caption = paste0(5,"% Isolation Threshold")){
  if(!isTRUE(add_html)){
    g <- ggplot(df) +
      geom_sf_interactive(aes(fill = .data[[fill]], 
                              tooltip = if("STATE" %in% names(df)){
                                          glue("{NAME}, {STATE}\nFIPS: {place}\nMatch: {eca_membership}\nAlpha: {round(.data[[fill]], 5)}")
                                        }else{
                                          glue("{NAME}\nFIPS: {place}\nMatch: {eca_membership}\nAlpha: {round(.data[[fill]], 5)}")
                                          },
                              data_id = place), 
                          color = NA) + 
      guides(alpha = "none") +
      coord_sf() +
      theme_void() +
      theme(plot.margin = margin(t = 1, r = 1, b = 10, l = 1, unit = "pt"),
            legend.key.size = unit(.2, "cm"),
            legend.title = element_text(size = rel(0.5)), 
            legend.text = element_text(size = rel(0.5)),
            legend.position = c(0.9, 0.2)) + {
        if(isTRUE(unit_scale)){
          scale_fill_viridis(direction = -1, limits=c(floor(0), ceiling(1)))
        } else {
          scale_fill_viridis(direction = -1)
        } } + 
      labs(fill = fill_lab,
           caption = caption)
  } else {
      g <- ggplot(df) +
        geom_sf_interactive(aes(fill = .data[[fill]], 
                                tooltip = if("STATE" %in% names(df)){
                                  glue("{NAME}, {STATE}\nFIPS: {place}\nMatch: {eca_membership}\nAlpha: {round(.data[[fill]], 5)}\nIndustry Output\n{html_output}")
                                }else{
                                  glue("{NAME}\nFIPS: {place}\nMatch: {eca_membership}\nAlpha: {round(.data[[fill]], 5)}\nIndustry Output\n{html_output}")
                                },
                                data_id = place), 
                            color = NA) + 
        guides(alpha = "none") +
        coord_sf() +
        theme_void() +
        theme(plot.margin = margin(t = 1, r = 1, b = 10, l = 1, unit = "pt"),
              legend.key.size = unit(.2, "cm"),
              legend.title = element_text(size = rel(0.5)), 
              legend.text = element_text(size = rel(0.5)),
              legend.position = c(0.9, 0.2)) + {
                if(isTRUE(unit_scale)){
                  scale_fill_viridis(direction = -1, limits=c(floor(0), ceiling(1)))
                } else {
                  scale_fill_viridis(direction = -1)
                } } + 
        labs(fill = fill_lab,
             caption = caption)
  }
  girafe(ggobj = g, 
         options = list(
           opts_hover(
             css = girafe_css(
               css = "stroke:gray;r:8pt;",
               text = "stroke:none;fill:black;fill-opacity:1;" ) ),
           opts_zoom(max = 5),
           opts_sizing = opts_sizing(rescale = TRUE),
           opts_tooltip(css = "font-family:sans-serif;
                                           background-color:gray;
                                           color:white;
                                           padding:10px;
                                           border-radius:5px;",
                        use_cursor_pos = FALSE,
                        offx = 0, offy = 330) ))
      
}

############ Hierarchical Map absorption metrics
hier_ab_map <- function(df, 
                        central_place,
                        threshold = .05){
    g <- ggplot() +
      geom_sf_interactive(aes(fill = eca_membership, 
                              tooltip = if("STATE" %in% names(df)){
                                glue("{NAME}, {STATE}\nFIPS: {place}\nCluster Count: {cluster_members_count}")
                              }else{
                                glue("{NAME}\nFIPS: {place}\nCluster Count: {cluster_members_count}")
                              },
                              data_id = place),
                          data = subset(df, eca_membership == df$eca_membership[df$place == central_place]),
                          color = NA) +
      scale_fill_manual(values = "#feb24c",
                        guide = guide_legend(order = 2),
                        labels = "Economic Catchment Area",
                        name = NULL) +
      #guides(fill=guide_legend(title=NULL)) +
      new_scale_fill() +
      geom_sf_interactive(aes(fill = eca_membership, 
                              tooltip = if("STATE" %in% names(df)){
                                glue("{NAME}, {STATE}\nFIPS: {place}\nCluster Count: {cluster_members_count}")
                              }else{
                                glue("{NAME}\nFIPS: {place}\nCluster Count: {cluster_members_count}")
                              },
                              data_id = place), 
                          data = subset(df, eca_membership != df$eca_membership[df$place == central_place]),
                          color = NA) +
      scale_fill_grey(start = 0.5,
                      end = 0.5,
                      guide = "none") +
      new_scale_fill() +
      geom_sf_interactive(data = df[df$place==central_place,], aes(fill = place), color = NA) +
      scale_fill_manual(values = "#56B1F7",
                        guide = guide_legend(order = 1),
                        labels = "Place of Interest",
                        name = NULL) +
      #guides(fill=guide_legend(title=NULL)) +
      new_scale_fill() +
      geom_sf_interactive(data = df[df$place == df$eca_membership[df$place == central_place],], aes(fill = place), color = NA) +
      scale_fill_manual(values = "#f03b20",
                        guide = guide_legend(order = 3),
                        labels = "Catchment Sink",
                        name = NULL) +
      #guides(fill=guide_legend(title=NULL)) +
      guides(alpha = "none") +
      coord_sf() +
      theme_void() +
      theme(plot.margin = margin(t = 1, r = 1, b = 10, l = 1, unit = "pt"),
            legend.key.size = unit(.2, "cm"),
            legend.title = element_text(size = rel(0.5)), 
            legend.text = element_text(size = rel(0.5)),
            legend.position = c(0.9, 0.2)) +
      {if(!is.null(threshold)){labs(caption = paste0(threshold*100,"% Isolation Threshold")) } }
  
  girafe(ggobj = g, 
         options = list(
           opts_hover(
             css = girafe_css(
               css = "stroke:gray;r:8pt;",
               text = "stroke:none;fill:black;fill-opacity:1;" ) ),
           opts_zoom(max = 5),
           opts_sizing = opts_sizing(rescale = TRUE),
           opts_tooltip(css = "font-family:sans-serif;
                                           background-color:gray;
                                           color:white;
                                           padding:10px;
                                           border-radius:5px;") ))
}

############ Map cluster membership counts by absorption 
clustmember_map <- function(df, 
                            alpha = "max_absorption_alpha",
                            add_html = FALSE,
                            caption = paste0(5,"% Isolation Threshold")){
if(!isTRUE(add_html)){
  g <- ggplot() +
    geom_sf_interactive(aes(fill = cluster_members_count,
                            tooltip = if("STATE" %in% names(df)){
                              glue("{NAME}, {STATE}\nFIPS: {place}\nMatch: {eca_membership}\nAlpha: {round(.data[[alpha]], 5)}")
                            }else{
                              glue("{NAME}\nFIPS: {place}\nMatch: {eca_membership}\nAlpha: {round(.data[[alpha]], 5)}")
                            },
                            data_id = place),
                        data = subset(df, cluster_category == "Cluster Sink" | cluster_category == "Isolated, Cluster Sink" ),
                        color = NA) + 
    labs(fill = "Cluster Sink") +
    scale_fill_gradient(low = "#feb24c", high = "#f03b20", guide = guide_colorbar(order = 2)) +
    new_scale_fill() +
    geom_sf_interactive(aes(fill = cluster_members_count,
                            tooltip = if("STATE" %in% names(df)){
                              glue("{NAME}, {STATE}\nFIPS: {place}\nMatch: {eca_membership}\nAlpha: {round(.data[[alpha]], 5)}")
                            }else{
                              glue("{NAME}\nFIPS: {place}\nMatch: {eca_membership}\nAlpha: {round(.data[[alpha]], 5)}")
                            },
                            data_id = place),
                        data = subset(df, cluster_category == "Cluster Source"),
                        color = NA) + 
    labs(fill = "Cluster Source") +
    scale_fill_gradient(low = "#56B1F7", high = "#132B43", guide = guide_colorbar(order = 1)) +
    new_scale_fill() +
    geom_sf_interactive(aes(fill = cluster_category,
                            tooltip = if("STATE" %in% names(df)){
                              glue("{NAME}, {STATE}\nFIPS: {place}\nMatch: {eca_membership}\nAlpha: {round(.data[[alpha]], 5)}")
                            }else{
                              glue("{NAME}\nFIPS: {place}\nMatch: {eca_membership}\nAlpha: {round(.data[[alpha]], 5)}")
                            },
                            data_id = place),
                        data = subset(df, cluster_category == "Isolated"),
                        color = NA) + 
    guides(fill=guide_legend(title=NULL)) +
    scale_fill_manual(values = "#b2df8a",
                      guide = guide_legend(order = 3)) +
    coord_sf() +
    theme_void() +
    theme(plot.margin = margin(t = 1, r = 1, b = 10, l = 1, unit = "pt"),
          legend.key.size = unit(.2, "cm"),
          legend.title = element_text(size = rel(0.5)), 
          legend.text = element_text(size = rel(0.5)),
          legend.position = c(0.9, 0.2)) +
    labs(caption = caption) 
    } else {
      g <- ggplot() +
        geom_sf_interactive(aes(fill = cluster_members_count,
                                tooltip = if("STATE" %in% names(df)){
                                  glue("{NAME}, {STATE}\nFIPS: {place}\nMatch: {eca_membership}\nAlpha: {round(.data[[alpha]], 5)}\nIndustry Output\n{html_output}")
                                }else{
                                  glue("{NAME}\nFIPS: {place}\nMatch: {eca_membership}\nAlpha: {round(.data[[alpha]], 5)}\nIndustry Output\n{html_output}")
                                },
                                data_id = place),
                            data = subset(df, cluster_category == "Cluster Sink" | cluster_category == "Isolated, Cluster Sink"),
                            color = NA) + 
        labs(fill = "Cluster Matches") +
        scale_fill_gradient(low = "#feb24c", high = "#f03b20", guide = guide_colorbar(order = 2)) +
        new_scale_fill() +
        geom_sf_interactive(aes(fill = cluster_members_count,
                                tooltip = if("STATE" %in% names(df)){
                                  glue("{NAME}, {STATE}\nFIPS: {place}\nMatch: {eca_membership}\nAlpha: {round(.data[[alpha]], 5)}\nIndustry Output\n{html_output}")
                                }else{
                                  glue("{NAME}\nFIPS: {place}\nMatch: {eca_membership}\nAlpha: {round(.data[[alpha]], 5)}\nIndustry Output\n{html_output}")
                                },
                                data_id = place),
                            data = subset(df, cluster_category == "Cluster Source"),
                            color = NA) + 
        labs(fill = "Cluster Source") +
        scale_fill_gradient(low = "#56B1F7", high = "#132B43", guide = guide_colorbar(order = 1)) +
        new_scale_fill() +
        geom_sf_interactive(aes(fill = cluster_category,
                                tooltip = if("STATE" %in% names(df)){
                                  glue("{NAME}, {STATE}\nFIPS: {place}\nMatch: {eca_membership}\nAlpha: {round(.data[[alpha]], 5)}\nIndustry Output\n{html_output}")
                                }else{
                                  glue("{NAME}\nFIPS: {place}\nMatch: {eca_membership}\nAlpha: {round(.data[[alpha]], 5)}\nIndustry Output\n{html_output}")
                                },
                                data_id = place),
                            data = subset(df, cluster_category == "Isolated"),
                            color = NA) + 
        guides(fill=guide_legend(title=NULL)) +
        scale_fill_manual(values = "#b2df8a",
                          guide = guide_legend(order = 3)) +
        coord_sf() +
        theme_void() +
        theme(plot.margin = margin(t = 1, r = 1, b = 10, l = 1, unit = "pt"),
              legend.key.size = unit(.2, "cm"),
              legend.title = element_text(size = rel(0.5)), 
              legend.text = element_text(size = rel(0.5)),
              legend.position = c(0.9, 0.2)) +
        labs(caption = caption) 
    }
  girafe(ggobj = g, 
         options = list(
           opts_hover(
             css = girafe_css(
               css = "stroke:gray;r:8pt;",
               text = "stroke:none;fill:black;fill-opacity:1;" ) ),
           opts_zoom(max = 5),
           opts_sizing = opts_sizing(rescale = TRUE),
           opts_tooltip(css = "font-family:sans-serif;
                                           background-color:gray;
                                           color:white;
                                           padding:10px;
                                           border-radius:5px;",
                        use_cursor_pos = FALSE,
                        offx = 0, offy = 330),
           opts_toolbar = opts_toolbar(position = "top", saveaspng = FALSE) ))
}

############ Map place-centric connectedness for a county 
place_absorption_map <- function(central_place,
                                 df, 
                                 fill, 
                                 add_html = FALSE,
                                 unit_scale = TRUE){
  
  # if(fill == "export_absorption"){x <- "Export"}else{x <- "Import"}
  g <- ggplot(df) + {
    if(!isTRUE(add_html)){
    geom_sf_interactive(aes(fill = (round(.data[[fill]], 5)), 
                            tooltip = if("STATE" %in% names(df)){
                              glue("{NAME}, {STATE}\nFIPS: {place}\nAbsorption: {round(.data[[fill]], 4)}")
                            }else{
                              glue("{NAME}\nFIPS: {place}\nAbsorption: {round(.data[[fill]], 4)}")
                            },
                            data_id = place ), color = NA ) } else {
                              nis <- paste0("html_nis.", substr(fill, nchar(fill) - 3, nchar(fill)) )
                              nid <- paste0("html_nid.", substr(fill, nchar(fill) - 3, nchar(fill)) )
    geom_sf_interactive(aes(fill = (round(.data[[fill]], 5)), 
                            tooltip = if("STATE" %in% names(df)){
                              glue("{NAME}, {STATE}\nFIPS: {place}\nAbsorption: {round(.data[[fill]], 4)}\nOutput Excess and Input Needs\n{.data[[nis]]}{.data[[nid]]}")
                            }else{
                              glue("{NAME}\nFIPS: {place}\nAbsorption: {round(.data[[fill]], 4)}\nOutput Excess and Input Needs\n{.data[[nis]]}{.data[[nid]]}")
                            },
                            data_id = place ), color = NA ) } 
    } + 
    guides(alpha = "none") +
    coord_sf() +
    theme_void() + {
      if(isTRUE(unit_scale)){
      scale_fill_viridis(direction = -1, limits=c(floor(0), ceiling(1)))
    } else {
      scale_fill_viridis(direction = -1)
      } } +
    geom_sf_interactive(data = df[df$place==central_place,], fill = "#8b0000", color = NA) +
    labs(fill = if("STATE" %in% names(df)){
      glue("{df[df$place==central_place,]$NAME}, {df[df$place==central_place,]$STATE} \nAbsorption")
    }else{
      glue("{df[df$place==central_place,]$NAME} \nAbsorption")
    } )  +
    theme(plot.margin = margin(t = 1, r = 1, b = 10, l = 1, unit = "pt"),
          legend.key.size = unit(.2, "cm"),
          legend.title = element_text(size = rel(0.5)), 
          legend.text = element_text(size = rel(0.5)),
          legend.position = c(0.9, 0.3)) 
  
  girafe(ggobj = g, 
         options = list(
           opts_hover(
             css = girafe_css(
               css = "stroke:gray;r:8pt;",
               text = "stroke:none;fill:black;fill-opacity:1;" ) ),
           opts_zoom(max = 5),
           opts_sizing = opts_sizing(rescale = TRUE),
           opts_tooltip(css = "font-family:sans-serif;
                                           background-color:gray;
                                           color:white;
                                           padding:10px;
                                           border-radius:5px;",
                        use_cursor_pos = FALSE,
                        offx = 0, offy = 330) ))
}


############ Map place-centric trade flows for a county (takes county-to-county matrix or sector list of county-to-county matrices as data inputs)
place_trade_map <- function(df,
                            central_place,
                            sector = NULL,
                            export_flows = TRUE, 
                            geog_year = "2013",
                            censor_scale_lowerbound = 0,
                            ...){
  df <- if(is.null(sector)){
    if(isTRUE(export_flows)){
      t(df[central_place, , drop = F] )
    } else {
        df[, central_place, drop = F]
    }
  } else {
      if(isTRUE(export_flows)){
        t(df[[sector]][central_place, , drop = F] )
      } else {
          df[[sector]][, central_place, drop = F]
      }
    } 
    df <- df %>% 
      as_tibble(rownames = "place") %>%
    rename("trade" = central_place)
  geog <- call_geog(geog_year)
  df <- join_space_with_connectedness(df, geog, ...)
  tfl <- if(isTRUE(export_flows)){
    "Outbound Trade"
  } else {
    "Inbound Trade"
    }
  g <- ggplot(df) + 
    geom_sf_interactive(aes(fill = (round(trade, 2)), 
                              tooltip = if("STATE" %in% names(df)){
                                glue("{NAME}, {STATE}\nFIPS: {place}\nQuantity: {round(trade, 2)}")
                              }else{
                                glue("{NAME}\nFIPS: {place}\nQuantity: {round(trade, 2)}")
                              }, 
                              data_id = place),
                          color = NA) + 
    guides(alpha = "none") +
    coord_sf() +
    theme_void() + 
      scale_fill_viridis(direction = -1, 
                         limits = c(floor(censor_scale_lowerbound), 
                                    ceiling(max(df$trade)))) +
    geom_sf_interactive(data = df[df$place==central_place,], fill = "#8b0000", color = NA) +
    labs(fill = if("STATE" %in% names(df)){
      glue("{df[df$place==central_place,]$NAME}, {df[df$place==central_place,]$STATE} \n{tfl}")
    }else{
      glue("{df[df$place==central_place,]$NAME} \n{tfl}")
    } )  +
    theme(plot.margin = margin(t = 1, r = 1, b = 10, l = 1, unit = "pt"),
          legend.key.size = unit(.2, "cm"),
          legend.title = element_text(size = rel(0.5)), 
          legend.text = element_text(size = rel(0.5)),
          legend.position = c(0.9, 0.3),
          plot.caption = element_text(hjust = 0.9, size = rel(0.5)) ) +
    {
      if(is.null(sector)){
        labs(caption = "Aggregate Trade Flows") 
      } else { 
        labs(caption = glue(sector, ": ", beacode2description(code = sector), " trade flow")) 
      }
    }

  girafe(ggobj = g, 
         options = list(
           opts_hover(
             css = girafe_css(
               css = "stroke:gray;r:8pt;",
               text = "stroke:none;fill:black;fill-opacity:1;" ) ),
           opts_zoom(max = 5),
           opts_sizing = opts_sizing(rescale = TRUE),
           opts_tooltip(css = "font-family:sans-serif;
                                           background-color:gray;
                                           color:white;
                                           padding:10px;
                                           border-radius:5px;",
                        use_cursor_pos = FALSE,
                        offx = 0, offy = 330) ))
}



############ Map Outbound to Inbound trade flows (takes county-to-county matrix or sector list of county-to-county matrices as data inputs)
inbound2outbound_map <- function(df,
                                 sector = NULL,
                                 geog_year = "2013",
                                 ...){
  df <- if(is.null(sector)){
    inbound2outbound(df)
  } else { 
    inbound2outbound(df[[sector]])
  }
  geog <- call_geog(geog_year)
  df <- join_space_with_connectedness(df, geog, ...)
  g <- ggplot() +
    geom_sf_interactive(aes(fill = round(log(-out_less_in), 2), 
                            tooltip = if("STATE" %in% names(df)){
                              glue("{NAME}, {STATE}\nFIPS: {place}\nInbound: {round(inbound, 2)}\nOutbound: {round(outbound, 2)}\nRatio: {round(out2in, 2)}\nNet: {round(out_less_in, 2)}")
                            }else{
                              glue("{NAME}\nFIPS: {place}\nInbound: {round(inbound, 2)}\nOutbound: {round(outbound, 2)}\nRatio: {round(out2in, 2)}\nNet: {round(out_less_in, 2)}")
                            },
                            data_id = place), 
                        color = NA,
                        data = subset(df, out_less_in < 0)) +
    scale_fill_gradient(low = "#56B1F7", high = "#132B43") +
    labs(fill = "Net Sink (log)") +
    new_scale_fill() +
    geom_sf_interactive(aes(fill = round(log(out_less_in), 2), 
                            tooltip = if("STATE" %in% names(df)){
                              glue("{NAME}, {STATE}\nFIPS: {place}\nInbound: {round(inbound, 2)}\nOutbound: {round(outbound, 2)}\nRatio: {round(out2in, 2)}\nNet: {round(out_less_in, 2)}")
                            }else{
                              glue("{NAME}\nFIPS: {place}\nInbound: {round(inbound, 2)}\nOutbound: {round(outbound, 2)}\nRatio: {round(out2in, 2)}\nNet: {round(out_less_in, 2)}")
                            },
                            data_id = place), 
                        color = NA,
                        data = subset(df, out_less_in > 0)) +
    scale_fill_gradient(low = "#feb24c", high = "#f03b20") +
    labs(fill = "Net Source (log)") +
  coord_sf() +
    theme_void() +
    theme(plot.margin = margin(t = 1, r = 1, b = 10, l = 1, unit = "pt"),
          legend.key.size = unit(.2, "cm"),
          legend.title = element_text(size = rel(0.5)), 
          legend.text = element_text(size = rel(0.5)),
          legend.position = c(0.9, 0.2),
          plot.caption = element_text(hjust = 0.9, size = rel(0.5))) +
    {
      if(is.null(sector)){
        labs(caption = "Aggregate Sector Provenance") 
      } else { 
        labs(caption = glue(sector, ": ", beacode2description(code = sector), " provenance")) 
      }
    }
  
  girafe(ggobj = g, 
         options = list(
           opts_hover(
             css = girafe_css(
               css = "stroke:gray;r:8pt;",
               text = "stroke:none;fill:black;fill-opacity:1;" ) ),
           opts_zoom(max = 5),
           opts_sizing = opts_sizing(rescale = TRUE),
           opts_tooltip(css = "font-family:sans-serif;
                                           background-color:gray;
                                           color:white;
                                           padding:10px;
                                           border-radius:5px;",
                        use_cursor_pos = FALSE,
                        offx = 0, offy = 330),
           opts_toolbar = opts_toolbar(position = "top", saveaspng = FALSE) ))
}






############ Map ratio of NIS to NID
nis2nid_map <- function(df){
  g <- ggplot(df) +
    geom_sf_interactive(aes(fill = (round(nis2nid, 5)), 
                            tooltip = if("STATE" %in% names(df)){
                              glue("{NAME}, {STATE}\nFIPS: {place}\nTotal Output: {round(total_output, 0)}\nTotal Input: {round(total_input, 0)}\nNet Export Supply: {round(total_nis, 0)}\nNet Import Demand: {round(total_nid, 0)}\nRatio: {round(nis2nid, 2)} ")
                            }else{
                              glue("{NAME}\nFIPS: {place}\nTotal Output: {round(total_output, 0)}\nTotal Input: {round(total_input, 0)}\nNet Export Supply: {round(total_nis, 0)}\nNet Import Demand: {round(total_nid, 0)}\nRatio: {round(nis2nid, 2)} ")
                            },
                            data_id = place
    ), 
    color = NA
    ) + 
    guides(alpha = "none") +
    coord_sf() +
    theme_void() +
    scale_fill_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0) +
    labs(fill = glue("Export Supply to\n Import Demand Ratio"))  +
    theme(plot.margin = margin(t = 1, r = 1, b = 10, l = 1, unit = "pt"),
          legend.key.size = unit(.2, "cm"),
          legend.title = element_text(size = rel(0.5)), 
          legend.text = element_text(size = rel(0.5)),
          legend.position = c(0.9, 0.3)) 

  girafe(ggobj = g, 
         options = list(
           opts_hover(
             css = girafe_css(
               css = "stroke:gray;r:8pt;",
               text = "stroke:none;fill:black;fill-opacity:1;" ) ),
           opts_zoom(max = 5),
           opts_sizing = opts_sizing(rescale = TRUE),
           opts_tooltip(css = "font-family:sans-serif;
                                             background-color:gray;
                                             color:white;
                                             padding:10px;
                                             border-radius:5px;",
                        use_cursor_pos = FALSE,
                        offx = 0, offy = 330) ))
  
}

absorption_density_plot <- function(df,
                                    fill,
                                    fill_lab,
                                    normalized = TRUE,
                                    trans = NULL,
                                    #colorbreaks = c("#440154FF" = 1:7 , "#1E9C89FF" = 8:17 , "#D64B40FF" = 18, "#FDE725FF" =  19:21)){
                                    colorbreaks = c("#440154FF" = 2000:2006, "#1E9C89FF" = 2007:2016 , "#D64B40FF" = 2017, "#FDE725FF" =  2018:2020)){

g <- ggplot(df) + 
     geom_density_interactive(aes(x = (.data[[fill]]), 
                                 tooltip = id,
                                 data_id = id,
                                 `data-id` = id,
                                 color = id),
                              extra_interactive_params = "data-id"
                              #position = position_fill(reverse = TRUE)
                              ) + { 
        if(!is.null(colorbreaks)){
          scale_color_manual_interactive(values = substr(names(colorbreaks), 1, 9),
                                         extra_interactive_params = "data-id",
                                         `data-id` = function(x) x, 
                                         data_id = function(x) x) 
        }     
      } + { 
        if(!is.null(trans)){
          scale_x_continuous(trans = trans)
        }     
      } + 
     labs(color = "CBP Year", x = paste(if(isTRUE(normalized)){"Normalized"}else{"Nominal"}, fill_lab), y = "Density")

girafe(ggobj = g, 
       options = list(
         opts_hover(
           css = girafe_css(
             css = "stroke:orange;r:12pt;",
             text = "stroke:none;fill:black;fill-opacity:1;" ) ),
         opts_zoom(max = 5),
         opts_sizing = opts_sizing(rescale = TRUE),
         opts_tooltip(css = "font-family:sans-serif;
                                           background-color:gray;
                                           color:white;
                                           padding:10px;
                                           border-radius:5px;") )
       )
}

############ Map various spatial impedance functions
impedance_distribution_map <- function(year = "2012",
                                       central_place = "20183", 
                                       decay_function = c("bisquare", "hyper", "gaus", "expo", "power", "dprox", "bprox"),
                                       boundary_limit = 200,
                                       rms_width = 200,
                                       hyper_decay_constant = 200,
                                       expo_decay_constant = 200,
                                       decay_power = 1/8,
                                       queen = TRUE,
                                       ...){
  decay_function <- match.arg(decay_function)
  df <- year2tiger(year) %>% 
    call_tiger(...)
  if(decay_function == "bisquare"){
    sf <- bisquare_impedance_mat(tiger_year = year2tiger(year), 
                                 decay_zero = miles2meters(boundary_limit),
                                 ...)
    caption <- glue("Bi-square Decay: {boundary_limit} mile limit")}
  if(decay_function == "hyper"){
    sf <- hyper_impedance_mat(tiger_year = year2tiger(year),
                              decay_constant = miles2meters(hyper_decay_constant),
                                 ...)
    caption <- glue("Hyperbolic Secant Decay: {hyper_decay_constant} mile scewness scalar")}
  if(decay_function == "gaus"){
    sf <- gaus_impedance_mat(tiger_year = year2tiger(year),
                             rms_width = miles2meters(rms_width),
                              ...)
    caption <- glue("Gaussian Decay: {rms_width} mile RMS scalar")}
  if(decay_function == "expo"){
    sf <- expo_impedance_mat(tiger_year = year2tiger(year),
                             decay_constant = miles2meters(expo_decay_constant),
                             ...)
    caption <- glue("Exponential Decay: {expo_decay_constant} mile disintegration scalar")}
  if(decay_function == "power"){
    sf <- power_impedance_mat(tiger_year = year2tiger(year),
                              decay_power = decay_power,
                              ...)
    sf[is.infinite(sf)] = 1
    caption <- glue("Inverse Power Decay: decay power of {decay_power}")}
  if(decay_function == "dprox"){
    sf <- dprox_mat(tiger_year = year2tiger(year),
                    boundary_limit = miles2meters(boundary_limit),
                    ...)
    diag(sf) <- 1
    caption <- glue("Uniform Proximity: {boundary_limit} mile limit")}
  if(decay_function == "bprox"){
    sf <- bprox_mat(tiger_year = year2tiger(year),
                    queen = queen,
                    ...)
    diag(sf) <- 1
    caption <- if(isTRUE(queen)){
      glue("Queen Adjacent Borders")
    }else{
        glue("Rook Adjacent Borders")}}
  
  sf <- data.frame(place = rownames(sf), 
                   imped = c(sf[, central_place, drop = FALSE]))
  df <- left_join(df, sf, by = "place")
  df <- df[df$STATE_CODE != "02" & 
           df$STATE_CODE != "15" & 
           df$STATE_CODE != "72", ]
  g <- ggplot() + 
    geom_sf_interactive(aes(fill = imped, 
                            tooltip = glue("Value: {round(imped,4)}\nCounty: {NAME}\nFIPS: {place}"), 
                            data_id = place), 
                        data = df, 
                        color = NA) + 
    guides(fill = guide_legend(title = "Impedance", 
                               reverse = TRUE)) + 
    theme_void() + 
    scale_fill_viridis(direction = -1, 
                       limits=c(floor(0), 
                                ceiling(1))) +
    theme(plot.margin = margin(t = 1, r = 1, b = 10, l = 1, unit = "pt"),
           legend.key.size = unit(.2, "cm"),
           legend.title = element_text(size = rel(0.5)), 
           legend.text = element_text(size = rel(0.5)),
           legend.position = c(0.9, 0.3),
           plot.caption = element_text(hjust = 0.9, size = rel(0.5))) + 
    labs(caption = caption)
  girafe(ggobj = g, 
         options = list(
           opts_hover(
             css = girafe_css(
               css = "stroke:orange;r:12pt;",
               text = "stroke:none;fill:black;fill-opacity:1;" ) ),
           opts_zoom(max = 5),
           opts_sizing = opts_sizing(rescale = TRUE),
           opts_tooltip(css = "font-family:sans-serif;
                                             background-color:gray;
                                             color:white;
                                             padding:10px;
                                             border-radius:5px;") ))
}

############ Map hierarchy of neighbors from central place
hierarchy_of_neighbors_map <- function(year,
                                       ...){
  df <- year2tiger(year) %>% call_tiger(...)
  df <- neighbor_of_neighbor(...) %>% inner_join(df, ., by = "place")
  g <- ggplot() + 
    geom_sf_interactive(aes(fill = nn, 
                            tooltip = glue("Interval level: {nn}\nCounty: {NAME}\nFIPS: {place}"), 
                            data_id = place), 
                        data = df, 
                        color = NA) + 
    guides(fill = guide_legend(title = "Neighbors", 
                               reverse = TRUE)) + 
    theme_void() +
    theme(plot.margin = margin(t = 1, r = 1, b = 10, l = 1, unit = "pt"),
          legend.key.size = unit(.2, "cm"),
          legend.title = element_text(size = rel(0.75)), 
          legend.text = element_text(size = rel(0.75)),
          legend.position = "none") + 
    labs(caption = paste0(df$COUNTY[df$nn=="n0"], ", ", df$STATE[df$nn=="n0"], " Hierarchy of Neighbors"))
  girafe(ggobj = g, 
         options = list(
           opts_hover(
             css = girafe_css(
               css = "stroke:orange;r:12pt;",
               text = "stroke:none;fill:black;fill-opacity:1;" ) ),
           opts_zoom(max = 5),
           opts_sizing = opts_sizing(rescale = TRUE),
           opts_tooltip(css = "font-family:sans-serif;
                                               background-color:gray;
                                               color:white;
                                               padding:10px;
                                               border-radius:5px;") ))
}


################################### 
################################### 
###################################


####Custom color/county pallet 
cus_pal_maps <- function(specname, values, placenames){
  df <- c()
  for(m in 1:length(setdiff(unique(placenames), c("Isolated", "ECA Isolated")))){
    df[m] =  values[m]
  }
  names(df) <- sort(setdiff(unique(placenames), c("Isolated", "ECA Isolated")))
  if(isTRUE("Isolated" %in%  unique(placenames) )){
    df <- c(df, "Isolated" = "#000000")
  }
  if(isTRUE("ECA Isolated" %in%  unique(placenames))){
    df <- c(df, "ECA Isolated" = "#A9A9A9")
  }
  assign(deparse(substitute(specname)), df, envir=.GlobalEnv) 
}








# S3 methods for automatic reticulate conversion of GeoDataFrame and GeoSeries ----

# Convert Python geopandas.GeoSeries to R sfc object
py_to_r.geopandas.geoseries.GeoSeries <- function(x) {
  crs <- x$crs$to_epsg()
  # GeoSeries.to_wkt() returns numpy array of WKT strings, which is automatically converted to R char array
  x <- x$to_wkt()
  # convert char array to sfc, keeping original CRS
  sf::st_as_sfc(x, crs = sf::st_crs(crs))
}

# Convert Python geopandas.GeoDataFrame to R sf object
py_to_r.geopandas.geodataframe.GeoDataFrame <- function(x) {
  # GeoSeries automatically converts to sfc
  geom_col <- x$geometry
  # convert geopandas.GeoDataFrame to pandas.DataFrame,
  # which automatically converts to R data.frame
  pd <- import("pandas")
  x <- pd$DataFrame(x)
  # replace geometry with sfc object
  x$geometry <- geom_col
  # convert data.frame to sf object
  sf::st_as_sf(x)
}



# Display end time
log_info("Define functions end")





