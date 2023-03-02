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

# Call up and clean TIGER data
# shapefile formats are available for 2000, 2007 and every year after that.
call_tiger <- function(tiger_year,
                  scale = c("20m", "500k", "5m"),
                  geometry = TRUE){
  scale <- match.arg(scale)
  df <- geography$get_county_df(strtoi(tiger_year), 
                                geometry, 
                                scale)
  df %<>% rename(place = CODE)
  df$COUNTY <- paste(df$NAME, "County")
  if(isTRUE(geometry)){df$center <- st_centroid(df$geometry)}
  st <- geography$get_state_df(geometry = FALSE) %>% select(c(1:3)) 
  names(st) <- c("STATE_CODE", "STATE_NAME", "STATE")
  df <- left_join(df, st, by = "STATE_CODE")
  df %<>% arrange(place)
  return(df)
}

# Produce Distance  Matrix
dist_mat <- function(year,
                     scale = c("20m", "500k", "5m")){
  t <- call_tiger(year, scale)
  df <- t$center %>% 
    as_Spatial() %>% 
    distm()
  rownames(df) = colnames(df) <- t$place
  return(df)
}

# Produce Border Proximity  Matrix
bprox_mat <- function(year,
                      scale = c("20m", "500k", "5m"),
                      queen = TRUE){
  t <- tiger(year, scale)
  df <- t$geometry %>% 
    poly2nb(queen = queen) %>%
    nb2mat(style = "B", zero.policy = TRUE)
  rownames(df) = colnames(df) <- t$place
  return(df)
}

# Produce Distance Proximity Matrix
dprox_mat <- function(year,
                      scale = c("20m","500k", "5m"),
                      boundary_limit){
  df <- dist_mat(year, scale)
  df[df<boundary_limit & df>0] <- 1
  df[df>boundary_limit] <- 0
  return(df)
}

# Produce inverse power distance decay impedance matrix
power_impedance_mat <- function(year,
                      scale = c("20m", "500k", "5m"),
                      decay_power = 2){
  df <- dist_mat(year, scale)
  df <- (1/(df)^decay_power)
  return(df)
}

# Produce exponential distance decay impedance matrix
expo_impedance_mat <- function(year,
                      scale = c("20m", "500k", "5m"),
                      decay_constant = 10000){
  df <- dist_mat(year, scale)
  df <- exp(-(df/decay_constant)) 
  return(df)
}

# Produce hyperbolic secant distance decay impedance matrix
hyper_impedance_mat <- function(year,
                               scale = c("20m", "500k", "5m"),
                               decay_constant = 1000000){
  df <- dist_mat(year, scale)
  df <- ((2/(exp(-(df/decay_constant)) + exp(df/decay_constant))))
  return(df)
}

# Call up and clean RUCC data
call_rucc <- function(ryear = c("2013", "2003", "1993", "1983", "1974")){
  ryear <- match.arg(ryear)
  df <- ers_rurality$get_ruc_df()
  df <- df %>% filter(RUC_YEAR==ryear)
  df$place <- df$FIPS
  return(df)
}

# Produce TIGER and RUCC table
tiger_rucc <- function(year,
                       scale = c("20m", "500k", "5m"),
                       ryear = c("2013", "2003", "1993", "1983", "1974")){
  t <- call_tiger(year, scale)
  r <- call_rucc(ryear) %>% select(-c(2))
  df <- inner_join(t, r, by = "place")
  df <- df[order(df$place), ]
  return(df)
}

# Call up and clean CBP
# available for years 1986:2020
call_cbp <- function(cbp_year,
                     scale = c("county", "state", "us")){
  scale <- match.arg(scale)
  df <- cbp$get_df(scale, strtoi(cbp_year))
  df %<>% rename(NAICS = industry)
  df$place <- paste0(df$fipstate, df$fipscty)
  df %<>% select(fipstate, fipscty, place, NAICS, emp, qp1, ap, est)
  return(df)
}

# Call up and clean CBSA concordance codes 
#(Delineations available for 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2013, 2015, 2017, 2018, 2020)
call_cbsa_concord <- function(cbsa_year = c("2020", "2018", "2017", "2015", "2013", "2009", "2008", "2007", "2006", "2005", "2004", "2003")){
  cbsa_year <- match.arg(cbsa_year)
  df <- geography_cbsa$get_cbsa_delin_df(strtoi(cbsa_year))
  df$CBSA_TITLE <- sapply(strsplit(df$CBSA_TITLE, ","), "[", 1)
  df$CBSA_TITLE <- paste(df$CBSA_TITLE, rep("CBSA", length(df$CBSA_TITLE)))
  df$place <- paste0(df$STATE_CODE, df$COUNTY_CODE)
  df <- df %>% select(CBSA_CODE, place, CBSA_TITLE)
  return(df)
}

#Convert a fips code into a cbsa code 
fips2cbsa <- function(fips,
                      cbsa_year = c("2020", "2018", "2017", "2015", "2013", "2009", "2008", "2007", "2006", "2005", "2004", "2003")){
  cb <- call_cbsa_concord(cbsa_year)
  if(isTRUE(fips %in% cb$place)){cb$CBSA_CODE[fips == cb$place]}else{fips}
}

# Aggregate industry output of each CBSA members in a cluster
cbsa_aggregate_industry_output <- function(cbsa_year = c("2020", "2018", "2017", "2015", "2013", "2009", "2008", "2007", "2006", "2005", "2004", "2003"),
                                           cbp_year,
                                           ilevel = c("det", "sum", "sec"),
                                           scale = c("county", "state", "us"),
                                           data_dir = file.path("data", "robjs"),
                                           geo_level = c("county", "state", "national")){
  o <- total_output(cbp_year = cbp_year,
                    ilevel = ilevel,
                    scale = scale,
                    data_dir = data_dir,
                    geo_level = geo_level)
    
  
  c <- call_cbsa_concord(cbsa_year)
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

# Aggregate spatial features of each CBSA members in a cluster
cbsa_spatial_cluster <- function(cbsa_year = c("2020", "2018", "2017", "2015", "2013", "2009", "2008", "2007", "2006", "2005", "2004", "2003"),
                                 tiger_year){
  
  #if(strtoi(cbsa_year) != tiger_year){cat("Warning: CBSA concordance year [",cbsa_year,"] not the same as TIGER shapefile year [",tiger_year,"]" )}
  
  t <- call_tiger(tiger_year)
  c <- call_cbsa_concord(cbsa_year)
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


#Agglomerate NAICS and BEA concordance by year and industry specificity (sector, summary, or detail)
place_industry_economy_long <- function(cbp_year,
                                        ilevel_concord = c("det_cord", "sum_cord", "sec_cord"), 
                                        scale = c("county", "state", "us"),
                                        data_dir = file.path("data", "robjs")){
  ilevel_concord <- match.arg(ilevel_concord)
  ## Check industry level concordance exists
  concord <- file.path(find_rstudio_root_file(), data_dir, ilevel_concord)
  stopifnot("industry level concordance does not exist" = file.exists(concord) == TRUE) 
  
  conc <- readRDS(concord)
  cbp_dat <- call_cbp(cbp_year, scale)
  x <- left_join(cbp_dat, conc, by = "NAICS") 
  x <- x %>%
    filter(.[9] != "NULL") %>%
    group_by(place, .[9]) %>%
    summarise(across(where(is.numeric), sum), .groups = 'drop') %>%
    as.data.frame()
  x <- x %>%
    group_by(place) %>%
    arrange(factor(x[[2]], levels = unique(conc[[1]])), .by_group = TRUE) %>%
    as.data.frame()
  df <- x %>% pivot_wider(id_cols = "DETAIL", names_from = "place", values_from = c("emp", "qp1", "ap", "est"), names_sep = ".", values_fill = 0) 
  df <- df %>% pivot_longer(-DETAIL, names_to = c(".value", "place"), names_pattern = "([^\\.]*)\\.*(\\d+)") %>% as.data.frame()
  df <- df %>% group_by(df[[2]]) %>% arrange(factor(df[[1]], levels = unique(conc[[1]])), .by_group = TRUE)
  names(df)[1] <- "indcode"
  df <- df[1:6]
  return(df)
}

#Generate industry output ("ap", "emp", "qp1", or "est") by location (county) from CBP in terms of BEA industry codes ("det_cord", "sum_cord", or "sec_cord") for any available year
industry_output_by_place <- function(cbp_year,
                                     ilevel_concord = c("det_cord", "sum_cord", "sec_cord"), 
                                     scale = c("county", "state", "us"),
                                     data_dir = file.path("data", "robjs"), 
                                     output_metric = c("ap", "emp", "qp1", "est")){
  output_metric <- match.arg(output_metric)
  df <- place_industry_economy_long(cbp_year = cbp_year, 
                                    ilevel_concord = ilevel_concord, 
                                    scale = scale, 
                                    data_dir = data_dir) %>% 
    .[, c("indcode", "place", output_metric)] 
  df <- df %>% pivot_wider(id_cols = "indcode", names_from = "place", values_from = output_metric) %>% as.data.frame()
  rownames(df) <- df[,1]
  df <- df[, colnames(df) != "indcode"] %>% as.matrix()
  return(df)
}




############ Call and clean pubdata BEA IO Use table
call_use_table <- function(year,
                           ilevel = c("det", "sum", "sec")){
  
  ilevel <- match.arg(ilevel)
  
  if(ilevel == "det"){
    x <- year %in% c("2007", "2012")
    stopifnot("BEA detail level tables only exist for years 2007 and 2012" = x == TRUE)
  }
  
  df <- bea_io$get_use(strtoi(year), ilevel) %>% as.matrix()
  return(df)
}


############ Derive the industry labor shares by year and scale
labor_share <- function(year,
                        ilevel = c("det", "sum", "sec")){
  ilevel <- match.arg(ilevel)
  df <- call_use_table(year, ilevel) %>% as.matrix()
  
  if (ilevel == "det"){
    # Collapse ambiguous detail level industry 23* codes
    # adding up all construction columns together
    l <- 1:24
    c <- 25:36
    r <- 37:405
    df <- cbind(df[, l], rowSums(df[, c], na.rm = TRUE), df[, r])
    colnames(df)[25] <- "23"
    df <- df["V00100", , drop = FALSE]/df["T018", , drop = FALSE] %>% as.matrix()
  } else {
    df <- df["V001", , drop = FALSE]/df["T018", , drop = FALSE] %>% as.matrix()
  }
  rownames(df) <- "labor_share"
  return(df)
}


# Call up and clean Ag Output data
call_agoutput <- function(ag_year = c("2017", "2012", "2007", "2002"), 
                          geo_level = c("county", "state", "national")){
  ag_year <- match.arg(ag_year)
  geo_level <- match.arg(geo_level)
  df <- ag_output$get_farm_sales_by_bea_detail(strtoi(ag_year), geo_level)
  place <- c(place = rownames(df))
  df <- sapply(df, function(x)x/1000) %>% as.data.frame()
  df <- cbind(place, df)
  rownames(df) <- 1:nrow(df)
  return(df)
}


############ Derive the Total Output Matrix (in thousands of dollars)
total_output <- function (cbp_year,
                          ilevel = c("det", "sum", "sec"),
                          scale = c("county", "state", "us"),
                          data_dir = file.path("data", "robjs"), 
                          geo_level = c("county", "state", "national")){
  
  ilevel <- match.arg(ilevel)
  scale <- match.arg(scale)
  ilevel_concord <- paste0(ilevel, "_cord")
  
  if(cbp_year > "2014"){
    ag_year = "2017"
  }else if(cbp_year %in% 2014:2010){
    ag_year = "2012"
  }else if(cbp_year %in% 2009:2005){
    ag_year = "2007"
  }else if(cbp_year < "2005"){
    ag_year = "2002"}
  
  if(strtoi(cbp_year) != ag_year){cat("Warning: CBP Output year [",cbp_year,"] not the same as Ag Output year [",ag_year,"]\n" )}

  if(ilevel != "det"){
    labor_share_year = cbp_year
  }else{
      if(isTRUE(cbp_year > "2007")){
        labor_share_year = "2012"
      }else{
          labor_share_year = "2007"}
    }
  
  farm_sales <- call_agoutput(ag_year, geo_level)
  fn <- colnames(farm_sales)[-c(1)]
  
  iout <- industry_output_by_place(cbp_year, 
                                   ilevel_concord = ilevel_concord, 
                                   scale = scale,
                                   data_dir = data_dir)
  ls <- labor_share(labor_share_year, ilevel) %>% .[, rownames(iout)] %>% unlist()
  
  df <- apply(iout, 2, function (x) {x / ls})
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
  return(df)
}


############ Derive clean Output Matrix 
total_output_tidy <- function (cbp_year,
                                ilevel = c("det", "sum", "sec"),
                                scale = c("county", "state", "us"),
                                data_dir = file.path("data", "robjs"),
                                tiger_year = cbp_year,
                                geo_level = c("county", "state", "national"),
                                cbsa_year = NULL){
  t <- call_tiger(tiger_year)
  if(is.null(cbsa_year)){
    df <- total_output(cbp_year = cbp_year, 
                       ilevel = ilevel,
                       scale = scale,
                       data_dir = data_dir,
                       geo_level = geo_level)
    df <- df[, colnames(df) %in% c(t$place)]
    df <- df[, colnames(df) %in% c(filter(t, !STATE_CODE %in% c("02", "15"))$place)]
    
  } else {
    df <- cbsa_aggregate_industry_output(cbsa_year = cbsa_year,
                                         cbp_year = cbp_year,
                                         ilevel = ilevel,
                                         scale = scale,
                                         data_dir = data_dir,
                                         geo_level = geo_level)
    
    c <- call_cbsa_concord(cbsa_year)
    c <- c[c$place %in% intersect(c$place, t$place),]
    c <- data.frame(CBSA_CODE = c(c$CBSA_CODE, setdiff(t$place, c$place)), 
                    place = c(c$place, setdiff(t$place, c$place)))

    df <- df[,!colnames(df) %in% c[grepl('^02', c$place),]$place]
    df <- df[,!colnames(df) %in% c[grepl('^15', c$place),]$place]
    df <- df[,!colnames(df) %in% c[grepl('^72', c$place),]$place]
    df <- df[,!colnames(df) %in% c[grepl('^02', c$place),]$CBSA_CODE]
    df <- df[,!colnames(df) %in% c[grepl('^15', c$place),]$CBSA_CODE]
    df <- df[,!colnames(df) %in% c[grepl('^72', c$place),]$CBSA_CODE]
    df <- df[,!colnames(df) %in% colnames(df[,grep('999$', colnames(df))])]
  }
  df[is.na(df)] = 0
  df <- df[, colSums(df != 0) > 0]
  return(df)
}


############ Call and clean the total requirements matrix 
call_total_requirements <- function(year,
                                    ilevel = c("det", "sum", "sec")){
  ilevel <- match.arg(ilevel)
  if(ilevel == "det"){
    x <- year %in% c("2007", "2012")
    stopifnot("BEA detail level tables only exist for years 2007 and 2012" = x == TRUE)
  }
  
  df <- bea_io$get_ixi(year, ilevel) %>% 
    .[1:ncol(.), ] %>% 
    as.matrix()
  rownames(df) <- colnames(df)
  
  # Collapse ambiguous industry 23* codes at detail level
  if(ilevel == "det"){ 
    d <- sum(df[25:36, 25:36]) / 12
    df[, 25] <- rowMeans(df[, 25:36])
    df <- df[, -c(26:36)]
    df[25, ] <- colMeans(df[25:36, ])
    df <- df[-c(26:36), ]
    df[25, 25] <- d
    colnames(df)[25] <- rownames(df)[25] <- "23"
  }
  
  return(df)
}

############ Derive the direct requirements matrix (Technical Coefficients) 
direct_requirements <- function(bea_year,
                                ilevel = c("det", "sum", "sec")){
  options(scipen=999)
  df <- call_total_requirements(bea_year, ilevel)
  df <- diag(ncol(df)) - solve(df)
  return(df)
}

############ Industry Input Needs 
### Derive the industry-by-county matrix of input needs DY
industry_input <- function(technical_coefficients_matrix, 
                           industry_output_matrix){
  
  ## Check industry level specificity match between industry_output_matrix and technical_coefficients_matrix
    df <- intersect(rownames(industry_output_matrix), rownames(technical_coefficients_matrix)) %>% 
      setequal(rownames(industry_output_matrix), .)
    stopifnot(df)
    
    o <- industry_output_matrix
    i <- rownames(o)
    d <- technical_coefficients_matrix[i, i]
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
                                     row_max_match = TRUE){
  a <- absorption_matrix
  if(isTRUE(row_max_match)){
    df <-  cbind(place = rownames(a), 
                 match = colnames(a)[apply(a, 1, which.max)],
                 max_absorption_alpha = apply(a, 1, max), 
                 second_max_absorption_alpha = apply(a, 1, function(x){max(x[x != max(x), drop = FALSE])}), 
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



############ Call connectedness matrix for any available year and industry scale
connectedness_matrix <- function(cbp_year,
                                 ilevel = c("det", "sum", "sec"),
                                 normalized = TRUE,
                                 impedance = NULL, 
                                 tiger_year = cbp_year,
                                 scale = c("county", "state", "us"),
                                 data_dir = file.path("data", "robjs"),
                                 geo_level = c("county", "state", "national"),
                                 cbsa_year = NULL){
  
  ilevel <- match.arg(ilevel)
  if(ilevel != "det"){
    bea_year = cbp_year
  }else{
    if(isTRUE(cbp_year > "2007")){
      bea_year = "2012"
    }else{
      bea_year = "2007"}}
  
  if(cbp_year > "2014"){
    ag_year = "2017"
  }else if(cbp_year %in% 2014:2010){
    ag_year = "2012"
  }else if(cbp_year %in% 2009:2005){
    ag_year = "2007"
  }else if(cbp_year < "2005"){
    ag_year = "2002"}
  
  o <- total_output_tidy(cbp_year = cbp_year,
                         ilevel = ilevel,
                         scale = scale,
                         data_dir = data_dir,
                         tiger_year = tiger_year,
                         geo_level = geo_level,
                         cbsa_year = cbsa_year)
  d <- direct_requirements(bea_year = bea_year, 
                           ilevel = ilevel)
  i <- industry_input(d, o)
  
  sasf <- paste0("sas", "_", match.arg(ilevel),"class", "_", cbp_year, "cbp", "_", bea_year, "bea", "_", ag_year, "ag", "_", tiger_year, "tiger", "_", if(!is.null(cbsa_year)){cbsa_year}else{"NA"}, "cbsa")
  if (file.exists(file.path(find_rstudio_root_file(), data_dir, sasf) ) ){ 
    df <- readRDS(file.path(find_rstudio_root_file(), data_dir, sasf))
  } else {
    df <- stacked_absorption_share(net_input_supply(o, i), net_input_demand(o, i))
    saveRDS(df,  file = file.path(find_rstudio_root_file(), data_dir, sasf) )
  }
  
  if(isTRUE(normalized)){
    df <- normalized_absorption_share(df, net_input_supply(o, i))
  }
  
  if(!is.null(impedance)){
    df <- df * impedance[colnames(df), rownames(df)]
  }
  return(df)
  
}


############ Call connectedness for any available year and industry scale
connectedness <- function (cbp_year,
                           ilevel = c("det", "sum", "sec"),
                           scale = c("county", "state", "us"),
                           data_dir = file.path("data", "robjs"),
                           tiger_year = cbp_year,
                           impedance = NULL, 
                           normalized = TRUE,
                           geo_level = c("county", "state", "national"),
                           cbsa_year = NULL,
                           threshold = .05,
                           row_max_match = TRUE){
  
  df <- connectedness_matrix(cbp_year = cbp_year,
                             ilevel = ilevel,
                             normalized = normalized,
                             impedance = impedance, 
                             tiger_year = tiger_year,
                             scale = scale,
                             data_dir = data_dir,
                             geo_level = geo_level,
                             cbsa_year = cbsa_year)
  
  df <- absorption_maximum_match(absorption_matrix = df, 
                                 threshold = threshold, 
                                 row_max_match = row_max_match)
  
  return(df)
}


############ Call connectedness for any available year and industry scale with spatial component
spatial_connectedness <- function (cbp_year,
                                   ilevel = c("det", "sum", "sec"),
                                   scale = c("county", "state", "us"),
                                   data_dir = file.path("data", "robjs"),
                                   tiger_year = cbp_year,
                                   threshold = .05,
                                   impedance = NULL, 
                                   normalized = TRUE,
                                   geo_level = c("county", "state", "national"),
                                   cbsa_year = NULL,
                                   row_max_match = TRUE,
                                   add_html = FALSE,
                                   industry_aggregate_class = "sec"){
  
  c <- connectedness(cbp_year = cbp_year,
                     ilevel = ilevel,
                     scale = scale,
                     data_dir = data_dir,
                     tiger_year = tiger_year,
                     threshold = threshold,
                     impedance = impedance, 
                     normalized = normalized,
                     geo_level = geo_level,
                     cbsa_year = cbsa_year,
                     row_max_match = row_max_match)
  
  if(is.null(cbsa_year)){
    s <- call_tiger(tiger_year) 
  } else {
    s <- cbsa_spatial_cluster(cbsa_year,
                              tiger_year)
    s <- rename(s, place = CBSA_CODE)
    s <- rename(s, NAME = CBSA_TITLE)
  }
  df <- join_space_with_connectedness(c, s)
  
  if(cbp_year > "2014"){
    ag_year = "2017"
  }else if(cbp_year %in% 2014:2010){
    ag_year = "2012"
  }else if(cbp_year %in% 2009:2005){
    ag_year = "2007"
  }else if(cbp_year < "2005"){
    ag_year = "2002"}
  

  if(isTRUE(add_html)){
    hp <- paste0("htmlplots", "_", industry_aggregate_class, "class", "_", cbp_year, "cbp", "_",  ag_year, "ag", "_", tiger_year, "tiger", "_", if(!is.null(cbsa_year)){cbsa_year}else{"NA"}, "cbsa")
    if (file.exists(file.path(find_rstudio_root_file(), data_dir, hp))){ 
      h <- readRDS(file.path(find_rstudio_root_file(), data_dir, hp))
    } else {
      h <- html_industry_dist_plots(cbp_year,
                                    industry_aggregate_class = industry_aggregate_class,
                                    cbsa_year = cbsa_year)
      saveRDS(h, file = file.path(find_rstudio_root_file(), data_dir, hp))
    }
    indicator_type = c("output", "input", "nis", "nid")
    for(i in indicator_type){
      df[[paste0("html_", i) ]] <- h[[i]]
    }
  } 
  
  
  return(df)
}


############ Spatial union each ECA member in a cluster
spatial_cluster <- function(spatial_connectedness_table,
                            list_names = NULL, 
                            place = place, 
                            geometry = geometry, 
                            eca_membership = eca_membership){
  
  df <-  spatial_connectedness_table %>% select(names(.)[!(names(.) %in% c("center", "html_output", "html_input", "html_nis", "html_nid"))])
  x <- df$eca_membership %>% unique() %>% .[order(.)]
  for (i in x){
    print(paste(list_names, "start cluster: ", i, which(i == x), "of", length(x), Sys.time()))
    df[df$place == i,]$geometry <- df %>% filter(df$eca_membership == i) %>% st_union()
    print(paste(list_names, "  end cluster: ", i, which(i == x), "of", length(x), Sys.time()))
  }
  df <- df %>% .[.$place %in% .$eca_membership, ]
}


############ Call spatial connectedness for any available year and industry scale aggregating eca clusters across space
cluster_spatial_connectedness <- function (cbp_year,
                                           ilevel = c("det", "sum", "sec"),
                                           scale = c("county", "state", "us"),
                                           data_dir = file.path("data", "robjs"),
                                           tiger_year = cbp_year,
                                           threshold = .05,
                                           impedance = NULL,
                                           list_names = NULL,
                                           normalized = TRUE, 
                                           geo_level = c("county", "state", "national"),
                                           cbsa_year = NULL,
                                           row_max_match = TRUE){
  df <- spatial_connectedness(cbp_year = cbp_year,
                              ilevel = ilevel,
                              scale = scale,
                              data_dir = data_dir,
                              tiger_year = tiger_year,
                              threshold = threshold,
                              impedance = impedance,
                              normalized = normalized,
                              geo_level = geo_level,
                              cbsa_year = cbsa_year,
                              row_max_match = row_max_match)
  x <- df$eca_membership %>% unique() %>% .[order(.)]
  for (i in x){
    print(paste(list_names, "start cluster: ", i, which(i == x), "of", length(x), Sys.time()))
    df[df$place == i,]$geometry <- df %>% filter(df$eca_membership == i) %>% st_union()
    print(paste(list_names, "  end cluster: ", i, which(i == x), "of", length(x), Sys.time()))
  }
  df <- df %>% .[.$place %in% .$eca_membership, ]
  return(df)
}




############ Place-centric connectedness
place_centric_connect <- function(central_place,
                                  cbp_year,
                                  ilevel = c("det", "sum", "sec"),
                                  scale = c("county", "state", "us"),
                                  data_dir = file.path("data", "robjs"),
                                  tiger_year = cbp_year,
                                  impedance = NULL, 
                                  normalized = TRUE,
                                  geo_level = c("county", "state", "national"),
                                  cbsa_year = NULL,
                                  add_html = FALSE,
                                  industry_aggregate_class = "sec"){
  
  
  df <- connectedness_matrix(cbp_year = cbp_year,
                             ilevel = ilevel,
                             normalized = normalized,
                             impedance = impedance, 
                             tiger_year = tiger_year,
                             scale = scale,
                             data_dir = data_dir,
                             geo_level = geo_level,
                             cbsa_year = cbsa_year)
  
  
  if(!is.null(cbsa_year)){central_place <- fips2cbsa(central_place)}
    
  df <- cbind(export_absorption = c(t(df[central_place, , drop=FALSE])),
              import_absorption = c(df[, central_place , drop=FALSE]),
              place = rownames(df))
  
  if(is.null(cbsa_year)){
    t <- call_tiger(tiger_year)
  } else {
    t <- cbsa_spatial_cluster(cbsa_year,
                              tiger_year)
    t <- rename(t, place = CBSA_CODE)
    t <- rename(t, NAME = CBSA_TITLE)
  }

  df <- join_space_with_connectedness(df, t)

  df$export_absorption <- as.numeric(df$export_absorption)
  df$import_absorption <- as.numeric(df$import_absorption)
  
  if(cbp_year > "2014"){
    ag_year = "2017"
  }else if(cbp_year %in% 2014:2010){
    ag_year = "2012"
  }else if(cbp_year %in% 2009:2005){
    ag_year = "2007"
  }else if(cbp_year < "2005"){
    ag_year = "2002"}
  
  if(isTRUE(add_html)){
    indicator_type = c("output", "input", "nis", "nid")
    hp <- paste0("htmlplots", "_", industry_aggregate_class, "class", "_", cbp_year, "cbp", "_",  ag_year, "ag", "_", tiger_year, "tiger", "_", if(!is.null(cbsa_year)){cbsa_year}else{"NA"}, "cbsa")
    
    if (file.exists(file.path(find_rstudio_root_file(), data_dir, hp))){ 
      h <- readRDS(file.path(find_rstudio_root_file(), data_dir, hp))
      for(i in indicator_type){
        df[[paste0("html_", i) ]] <- h[[i]]
      }
    } else {
      h <- html_industry_dist_plots(cbp_year,
                                    industry_aggregate_class = industry_aggregate_class,
                                    cbsa_year = cbsa_year)
      saveRDS(h, file = file.path(find_rstudio_root_file(), data_dir, hp))
      for(i in indicator_type){
        df[[paste0("html_", i) ]] <- h[[i]]
      }
    }
  } 
  
  return(df)
  
}


## Need to revise with better TIGER tracking across time
############ Change in connectedness over time for a county 
place_connect_delta <- function(central_place, 
                                sample_years = c("2017", "2012", "2007", "2002"),
                                ilevel = c("det", "sum", "sec"),
                                tiger_year = "2013",
                                impedance = NULL, 
                                normalized = TRUE,
                                cbsa_year = NULL,
                                add_html = FALSE,
                                industry_aggregate_class = "sec"){
  place_connect <- vector("list", length(sample_years))
  names(place_connect) <- sample_years
  for (i in sample_years){
    place_connect[[i]] <- place_centric_connect(central_place = central_place,
                                                cbp_year = i,
                                                ilevel = ilevel,
                                                tiger_year = tiger_year,
                                                impedance = impedance, 
                                                normalized = normalized,
                                                cbsa_year = cbsa_year,
                                                add_html = add_html,
                                                industry_aggregate_class = industry_aggregate_class)
  }

  cl <- c("export_absorption", "import_absorption", "place", "html_output", "html_input", "html_nis", "html_nid") %>% .[. %in% names(place_connect[[sample_years[2]]])]
  df <- st_set_geometry(place_connect[[sample_years[2]]], NULL) %>% select(cl) %>% 
      inner_join(place_connect[[sample_years[1]]], ., by = "place", suffix = c(paste0(".", sample_years[1]), paste0(".", sample_years[2])))
  
  for (s in 2:(length(sample_years)-1)){
    cl <- c("export_absorption", "import_absorption", "place", "html_output", "html_input", "html_nis", "html_nid") %>% .[. %in% names(place_connect[[sample_years[s+1]]])]
    df <- st_set_geometry(place_connect[[sample_years[s+1]]], NULL) %>% select(cl) %>% 
      inner_join(df, ., by = "place", suffix = c(paste0(".", sample_years[s]), paste0(".", sample_years[s+1])))
  }


  com <- combn(sample_years, 2)
  for (c in 1:ncol(com)){
    df[[paste0("export_absorption_delta_", substr(com[2, c], 3, 4), substr(com[1, c], 3, 4))]] <- df[[paste0("export_absorption.", com[1, c])]] - df[[paste0("export_absorption.", com[2, c])]]
    df[[paste0("import_absorption_delta_", substr(com[2, c], 3, 4), substr(com[1, c], 3, 4))]] <- df[[paste0("import_absorption.", com[1, c])]] - df[[paste0("import_absorption.", com[2, c])]]
  }
  
  ###Percent Change option
  # for (c in 1:ncol(com)){
  #   df[[paste0("export_absorption_delta_", substr(com[2, c], 3, 4), substr(com[1, c], 3, 4))]] <- (df[[paste0("export_absorption.", com[1, c])]] - df[[paste0("export_absorption.", com[2, c])]])/df[[paste0("export_absorption.", com[1, c])]]*100
  #   df[[paste0("import_absorption_delta_", substr(com[2, c], 3, 4), substr(com[1, c], 3, 4))]] <- (df[[paste0("import_absorption.", com[1, c])]] - df[[paste0("import_absorption.", com[2, c])]])/df[[paste0("import_absorption.", com[1, c])]]*100
  # }

  return(df)
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
                           threshold = .05,
                           fill, 
                           fill_lab,
                           unit_scale = TRUE){
  
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
        } } + {
      if(!is.null(threshold)){labs(fill = fill_lab,
           caption = paste0(threshold*100,"% Isolation Threshold"))} else {labs(fill = fill_lab)} }
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
                } } + {
                  if(!is.null(threshold)){labs(fill = fill_lab,
                                               caption = paste0(threshold*100,"% Isolation Threshold"))} else {labs(fill = fill_lab)} }
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
                            add_html = FALSE){
if(!isTRUE(add_html)){
  g <- ggplot() +
    geom_sf_interactive(aes(fill = cluster_members_count,
                            tooltip = if("STATE" %in% names(df)){
                              glue("{NAME}, {STATE}\nFIPS: {place}\nMatch: {eca_membership}\nAlpha: {round(.data[[alpha]], 5)}")
                            }else{
                              glue("{NAME}\nFIPS: {place}\nMatch: {eca_membership}\nAlpha: {round(.data[[alpha]], 5)}")
                            },
                            data_id = place),
                        data = subset(df, cluster_category == "Cluster Sink"),
                        color = NA) + 
    labs(fill = "Cluster Matches") +
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
                        data = subset(df, cluster_category == "Isolated, Cluster Sink"),
                        color = NA) + 
    guides(fill=guide_legend(title=NULL)) +
    scale_fill_manual(values = "#FFC0CB",
                      guide = guide_legend(order = 3)) +
    coord_sf() +
    theme_void() +
    theme(plot.margin = margin(t = 1, r = 1, b = 10, l = 1, unit = "pt"),
          legend.key.size = unit(.2, "cm"),
          legend.title = element_text(size = rel(0.5)), 
          legend.text = element_text(size = rel(0.5)),
          legend.position = c(0.9, 0.2)) +
    labs(caption = paste0(5,"% Isolation Threshold")) 
    } else {
      g <- ggplot() +
        geom_sf_interactive(aes(fill = cluster_members_count,
                                tooltip = if("STATE" %in% names(df)){
                                  glue("{NAME}, {STATE}\nFIPS: {place}\nMatch: {eca_membership}\nAlpha: {round(.data[[alpha]], 5)}\nIndustry Output\n{html_output}")
                                }else{
                                  glue("{NAME}\nFIPS: {place}\nMatch: {eca_membership}\nAlpha: {round(.data[[alpha]], 5)}\nIndustry Output\n{html_output}")
                                },
                                data_id = place),
                            data = subset(df, cluster_category == "Cluster Sink"),
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
                            data = subset(df, cluster_category == "Isolated, Cluster Sink"),
                            color = NA) + 
        guides(fill=guide_legend(title=NULL)) +
        scale_fill_manual(values = "#FFC0CB",
                          guide = guide_legend(order = 3)) +
        coord_sf() +
        theme_void() +
        theme(plot.margin = margin(t = 1, r = 1, b = 10, l = 1, unit = "pt"),
              legend.key.size = unit(.2, "cm"),
              legend.title = element_text(size = rel(0.5)), 
              legend.text = element_text(size = rel(0.5)),
              legend.position = c(0.9, 0.2)) +
        labs(caption = paste0(5,"% Isolation Threshold")) 

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

############ Aggregate economic industry output of each ECA member in a cluster, keep all non source places as ECA core unit label
aggregate_industry_output <- function(industry_output_matrix, 
                                      connectedness_table,
                                      place = place, 
                                      eca_membership = eca_membership){
  
  df <- industry_output_matrix
  c <- connectedness_table
  
  x <- c$eca_membership %>% unique() %>% .[order(.)]
  for(i in x){
    df[, i] <- rowSums(df[, c$place[c$eca_membership == i], drop = FALSE])
  } 
  df <- df[, x]
}


############ Single function of nested functions to derive a hierarchies of connectedness tables and resulting output matrices from a base single output matrix and single direct requirements matrix
one_hierarchical_connectedness <- function(direct_mat, 
                                           output_mat, 
                                           space_mat,
                                           threshold = .05, 
                                           list_names = NULL){
  d <- direct_mat
  o <- output_mat
  s <- space_mat
  hct <- list()
  hsct <- list()
  hom <- list()
  hom$level_0 <- o
  n = 1
  i = FALSE
  df <- list()
  print(list_names)
  while(i == FALSE){
    print(paste("level", n))
    c <- one_direct_connect(d, o, threshold)
    hct[[paste0("level_", deparse(n))]] <- c
    i <- all(c$place %in% c$eca_membership) 
    if (i == TRUE){next}
    hsct[[paste0("level_", deparse(n))]] <- join_space_with_connectedness(c, s) %>% spatial_cluster()
    o <- aggregate_industry_output(o, c)
    hom[[paste0("level_", deparse(n))]] <- o
    n = n + 1
  }
  df[[deparse(list_names)]] <- list("Hierarchical_Connectedness_table" = hct,
                                    "Hierarchical_Spatial_Cluster_table" = hsct,
                                    "Hierarchical_Output_mat" = hom)
}


### Need to correct for CBSA capability
############ industry (output, input, nid, nis) distributions by county
industry_distribution <- function(industry_aggregate_class = c("sec", "sum", "det"),
                                  cbp_year,
                                  ilevel = c("det", "sum", "sec"),
                                  scale = c("county", "state", "us"),
                                  data_dir = file.path("data", "robjs"), 
                                  geo_level = c("county", "state", "national"),
                                  cbsa_year = NULL){
  
  industry_aggregate_class <- match.arg(industry_aggregate_class)
  ilevel <- match.arg(ilevel)
  if(ilevel != "det"){
    bea_year = cbp_year
  }else{
    if(isTRUE(cbp_year > "2007")){
      bea_year = "2012"
    }else{
      bea_year = "2007"}}
  
  o <-  total_output(cbp_year = cbp_year, 
                     ilevel = ilevel,
                     scale = scale,
                     data_dir = data_dir,
                     geo_level = geo_level)
  if(!is.null(cbsa_year)){ 
    c <- call_cbsa_concord(cbsa_year)
    c <- c[c$place %in% intersect(c$place, colnames(o)),]
    c <- data.frame(CBSA_CODE = c(c$CBSA_CODE, setdiff(colnames(o), c$place)), 
                    place = c(c$place, setdiff(colnames(o), c$place)))
    c <- c[order(c$CBSA_CODE), ]
    rownames(c) <- 1:nrow(c)
    x <- c$CBSA_CODE %>% unique()
    cbo <- data.frame(row.names = rownames(o))
    for(i in x){
      cbo[, i] <- rowSums(o[, c$place[c$CBSA_CODE == i], drop = FALSE])
    } 
    o <- as.matrix(cbo)
    o[is.na(o)] = 0
  }
  
  i <- industry_input(direct_requirements(bea_year = bea_year,
                                          ilevel = ilevel), o)
  nis <- net_input_supply(o, i)
  nid <- net_input_demand(o, i)
  
  indicator_type = c("output", "input", "nis", "nid")
  ti <- vector("list", length(indicator_type))
  names(ti) <- indicator_type
  ti[[1]] <- o
  ti[[2]] <- i
  ti[[3]] <- nis
  ti[[4]] <- nid
  for (i in 1:length(ti)){
    ti[[i]] <- melt(ti[[i]])
    ti[[i]] <- ti[[i]] %>% 
      filter(value > 0)
    names(ti[[i]]) <- c("DETAIL", "place", indicator_type[i])
    ti[[i]]$place <- ti[[i]]$place  %>% formatC(width = 5, format = "d", flag = "0")
  }
  
  c <- bea_io$get_naics_df() %>% 
    filter(DETAIL != "NaN") %>% 
    filter(NAICS != "n.a.") %>% 
    distinct(DETAIL, .keep_all = TRUE) %>% 
    unnest(., cols = names(.))
  
  if (industry_aggregate_class == "sum"){
    n <- data.frame(names(bea_io$get_sup(strtoi(bea_year), industry_aggregate_class, FALSE)), 
                    names(bea_io$get_sup(strtoi(bea_year), industry_aggregate_class, TRUE)))
    names(n) <- c("SUMMARY", "name")
    sn <- inner_join(n, c, by = "SUMMARY") 
    sn$abv <- sn$SUMMARY
  } else if (industry_aggregate_class == "sec") {
    cord = importr(sec_cord)
    n <- data.frame(names(bea_io$get_sup(strtoi(bea_year), industry_aggregate_class, FALSE)), 
                    names(bea_io$get_sup(strtoi(bea_year), industry_aggregate_class, TRUE)))
    names(n) <- c("SECTOR", "name")
    n <- left_join(cord, n, by = "SECTOR") 
    names(n) <- c("BEA_SECTOR", "SECTOR", "name")
    c[["SECTOR"]] <- substr(c[["SECTOR"]], 1, 2)
    sn <- inner_join(n, c, by = "SECTOR") 
    sn$abv <- sn$BEA_SECTOR
  } else if (industry_aggregate_class == "det") {
    sn <- c
    sn <- rename(sn, name = DESCRIPTION)
    sn$abv <- sn$DETAIL
  }
  pal <- data.frame(color = viridis(length(unique(sn$name))),
                    name = unique(sn$name))
  sn <- inner_join(sn, pal, by = "name")
  sn["DETAIL"][sn["NAICS"] == "23*"] <- "23"
  sn <- sn %>% 
    distinct(DETAIL, .keep_all = TRUE) 
  
  df <- vector("list", length(indicator_type))
  names(df) <- indicator_type
  for (i in 1:length(df)){
    df[[i]] <- inner_join(ti[[i]], sn , by = "DETAIL")
  }
  return(df)
}



############ Bar charts of industry distributions by county
industry_distribution_barcharts <- function(data,
                                            interact = TRUE,
                                            short = TRUE){
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

############ Multi plot of bar charts of industry distributions for a given county
industry_dist_plots <- function(central_place,
                                cbp_year,
                                cbsa_year = NULL,
                                interact = TRUE,
                                short = TRUE,
                                industry_aggregate_class = c("sec", "sum", "det"),
                                geo_level = c("county", "state", "national"),
                                ilevel = c("det", "sum", "sec"),
                                scale = c("county", "state", "us"),
                                data_dir = file.path("data", "robjs")
                                ){
  
  idis <- industry_distribution(industry_aggregate_class = industry_aggregate_class,
                                       cbp_year = cbp_year,
                                       ilevel = ilevel,
                                       scale = scale,
                                       data_dir = data_dir,
                                       geo_level = geo_level,
                                       cbsa_year = cbsa_year)
  bd <-  vector("list")
  for (i in names(idis) ){
    x <- idis[[i]][idis[[i]]$place == central_place, ] 
    bd[[i]] <- industry_distribution_barcharts(data = x, 
                                              interact = interact, 
                                              short = short)
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



############ html plots of bar charts of industry distributions by county [used in mapping interactive tooltips]
html_industry_dist_plots <- function(cbp_year,
                                     industry_aggregate_class = c("sec", "sum", "det"),
                                     cbsa_year = NULL){
  
  t <- industry_distribution(industry_aggregate_class = industry_aggregate_class,
                             cbp_year = cbp_year,
                             ilevel = "det",
                             cbsa_year = cbsa_year)
  
  b <-  vector("list")
  for (i in names(t) ){
    b[[i]] <- industry_distribution_barcharts(t[[i]], 
                                              interact = FALSE, 
                                              short = TRUE)
  }
  
  h <- b
  
  for (i in names(h)){
    print(i)
    print(Sys.time())
    for (p in 1:length(h[[i]])){
      h[[i]][p] <- htmltools::plotTag(b[[i]][p], alt = "") %>% as.character()
    }
  }
  
  return(h)
  
}

############ Absorption matching outcomes over time
absorption_match_overtime <- function(years = 2000:2020,
                                      normalized = TRUE,
                                      ilevel = c("det", "sum", "sec"),
                                      threshold = 0,
                                      cbsa_year = NULL,
                                      row_max_match = TRUE,
                                      tiger_year = "2013",
                                      data_dir = file.path("data", "robjs")){
  dis <- vector("list", length(years))
  names(dis) <- years
  for (y in names(dis)){
    dis[[y]] <- connectedness(cbp_year = y,
                              normalized = normalized,
                              ilevel = ilevel,
                              threshold = threshold,
                              cbsa_year = cbsa_year,
                              row_max_match = row_max_match,
                              tiger_year = tiger_year,
                              data_dir = data_dir)
  }
  df <- bind_rows(dis, .id = "id")
  return(df)
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





