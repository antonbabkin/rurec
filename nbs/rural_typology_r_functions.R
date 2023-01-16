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
call_tiger <- function(year,
                  scale = c("500k", "20m", "5m"),
                  geometry = TRUE){
  scale <- match.arg(scale)
  df <- geography$get_county_df(strtoi(year), 
                                geometry, 
                                scale)
  df %<>% rename(place = CODE)
  df$center <- st_centroid(df$geometry)
  df %<>% arrange(place)
  return(df)
}

# Produce Distance  Matrix
dist_mat <- function(year,
                     scale = c("500k", "20m", "5m")){
  t <- call_tiger(year, scale)
  df <- t$center %>% 
    as_Spatial() %>% 
    distm()
  rownames(df) = colnames(df) <- t$place
  return(df)
}

# Produce Border Proximity  Matrix
bprox_mat <- function(year,
                      scale = c("500k", "20m", "5m"),
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
                      scale = c("500k", "20m", "5m"),
                      boundary_limit){
  df <- dist_mat(year, scale)
  df[df<boundary_limit & df>0] <- 1
  df[df>boundary_limit] <- 0
  return(df)
}

# Produce inverse power distance decay impedance matrix
power_impedance_mat <- function(year,
                      scale = c("500k", "20m", "5m"),
                      decay_power = 2){
  df <- dist_mat(year, scale)
  df <- (1/(df)^decay_power)
  return(df)
}

# Produce exponential distance decay impedance matrix
expo_impedance_mat <- function(year,
                      scale = c("500k", "20m", "5m"),
                      decay_constant = 10000){
  df <- dist_mat(year, scale)
  df <- exp(-(df/decay_constant)) 
  return(df)
}

# Produce hyperbolic secant distance decay impedance matrix
hyper_impedance_mat <- function(year,
                               scale = c("500k", "20m", "5m"),
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
                       scale = c("500k", "20m", "5m"),
                       ryear = c("2013", "2003", "1993", "1983", "1974")){
  t <- call_tiger(year, scale)
  r <- call_rucc(ryear)
  df <- inner_join(t, r, by = "place")
  df <- df[order(df$place), ]
  #rownames(df) <- df$place
  return(df)
}

# Call up and clean CBP
call_cbp <- function(year,
                     scale = c("county", "state", "us")){
  scale <- match.arg(scale)
  df <- cbp$get_df(scale, strtoi(year))
  df %<>% rename(NAICS = industry)
  df$place <- paste0(df$fipstate, df$fipscty)
  df %<>% select(fipstate, fipscty, place, NAICS, emp, qp1, ap, est)
  return(df)
}



#Agglomerate NAICS and BEA concordance by year and industry specificity (sector, summary, or detail)
place_industry_economy_long <- function(year,
                                        ilevel_concord = c("det_cord", "sum_cord", "sec_cord"), 
                                        scale = c("county", "state", "us"),
                                        data_dir = file.path("data", "robjs")){
  ilevel_concord <- match.arg(ilevel_concord)
  ## Check industry level concordance exists
  concord <- file.path(find_rstudio_root_file(), data_dir, ilevel_concord)
  stopifnot("industry level concordance does not exist" = file.exists(concord) == TRUE) 
  
  c <- readRDS(concord)
  cbp <- call_cbp(year, scale)
  x <- left_join(cbp, c, by = "NAICS") 
  x <- x %>%
    filter(.[9] != "NULL") %>%
    group_by(place, .[9]) %>%
    summarise(across(where(is.numeric), sum), .groups = 'drop') %>%
    as.data.frame()
  x <- x %>%
    group_by(place) %>%
    arrange(factor(x[[2]], levels = unique(c[[1]])), .by_group = TRUE) %>%
    as.data.frame()
  df <- reshape(x, idvar = names(x[2]), timevar = "place", direction = "wide") %>% 
    suppressWarnings()
  df <- df %>% arrange(factor(df[[1]], levels = unique(c[[1]])), .by_group = TRUE)
  rownames(df) <- df[,1]
  df[is.na(df)] <- 0
  colnames(df)[1] <- "indcode"
  df <- df %>% reshape(idvar = "place", varying = c(colnames(df)[-1]), direction = "long")
  rownames(df) <- 1:nrow(df)
  names(df)[names(df)=="time"] <- "place"
  df$place <- df$place  %>% formatC(width = 5, format = "d", flag = "0")
  df$emp <-  as.numeric(df$emp)
  df$qp1 <-  as.numeric(df$qp1)
  df$ap <-  as.numeric(df$ap)
  df$est <-  as.numeric(df$est)
  df <- df[1:6]
  return(df)
}

#Generate industry output ("ap", "emp", "qp1", or "est") by location (county) from CBP in terms of BEA industry codes ("det_cord", "sum_cord", or "sec_cord") for any available year
industry_output_by_place <- function(year,
                                     ilevel_concord = c("det_cord", "sum_cord", "sec_cord"), 
                                     scale = c("county", "state", "us"),
                                     data_dir = file.path("data", "robjs"), 
                                     output_metric = c("ap", "emp", "qp1", "est")){
  output_metric <- match.arg(output_metric)
  df <- place_industry_economy_long(year = year, 
                                    ilevel_concord = ilevel_concord, 
                                    scale = scale, 
                                    data_dir = data_dir) %>% 
    .[, c("indcode", "place", output_metric)] 
  
  df <- reshape(df,
                idvar = "indcode",
                v.names = output_metric,
                varying = unique(df[["place"]]),
                timevar = "place",
                new.row.names = unique(df[["indcode"]]),
                direction = "wide")
  df <- df %>% subset(select = -c(1)) %>% as.matrix()
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
total_output <- function (year,
                          ilevel = c("det", "sum", "sec"),
                          scale = c("county", "state", "us"),
                          output_metric = c("ap", "emp", "qp1", "est"),
                          data_dir = file.path("data", "robjs"),
                          labor_share_year = year,
                          ag_year = c("2017", "2012", "2007", "2002"), 
                          geo_level = c("county", "state", "national")){
  
  a <- strtoi(match.arg(ag_year))
  if(strtoi(year) != a){cat("Warning: CBP Output year [",year,"] not the same as Ag Output year [",a,"]" )}
  
  farm_sales <- call_agoutput(ag_year, geo_level)
  fn <- colnames(farm_sales)[-c(1)]
  
  ilevel <- match.arg(ilevel)
  scale <- match.arg(scale)
  output_metric <- match.arg(output_metric)
  ilevel_concord <- paste0(ilevel, "_cord")
  
  
  iout <- industry_output_by_place(year, 
                                   ilevel_concord = ilevel_concord, 
                                   scale = scale,
                                   data_dir = data_dir, 
                                   output_metric = output_metric)
  ls <- labor_share(labor_share_year, ilevel) %>% .[, rownames(iout)] %>% unlist()
  
  df <- apply(iout, 2, function (x) {x / ls})
  df <- t(df) %>% as.data.frame()
  df$place <- rownames(df)
  df <- left_join(df, farm_sales, by = "place")
  if (ilevel == "sec") {
    df[["11"]] <- rowSums(df[, c("11", fn)], na.rm = T)
    df <- df %>% select(!c(fn))
  } else if (ilevel == "sum") {
    df[["111CA"]] <- rowSums(df[, c(fn)], na.rm = T)
    df <- df %>% select(!c(fn)) %>% select("111CA", everything())
  } else if (ilevel == "det") {
    df <- df %>% select(fn, everything())
  }
  rownames(df) <- df$place
  df$place <- NULL
  df <- t(df)
  return(df)
}



############ Derive clean Output Matrix 
total_output_tidy <- function (year,
                                ilevel = c("det", "sum", "sec"),
                                scale = c("county", "state", "us"),
                                output_metric = c("ap", "emp", "qp1", "est"),
                                data_dir = file.path("data", "robjs"),
                                labor_share_year = year,
                                tiger_year = year,
                                ag_year = c("2017", "2012", "2007", "2002"), 
                                geo_level = c("county", "state", "national")){
  
  df <- total_output(year = year, 
                     ilevel = ilevel,
                     scale = scale,
                     output_metric = output_metric,
                     data_dir = data_dir,
                     labor_share_year = labor_share_year,
                     ag_year = ag_year, 
                     geo_level = geo_level)
  t <- tiger_rucc(tiger_year)
  
  
  df <- df[, colnames(df) %in% c(unique(t$place))]
  df <- df[, colnames(df) %in% c(filter(t, !STATE_CODE %in% c("02", "15"))$place)]
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
direct_requirements <- function(year,
                                ilevel = c("det", "sum", "sec")){
  options(scipen=999)
  df <- call_total_requirements(year, ilevel)
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
          (rep(c(1), each=nrow(s)) %*% pmin(s[,i], d[,j]))
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
}

############ Row-wise Absorption Potential Maximum and Match
absorption_maximum_match <- function(absorption_matrix, 
                                     threshold = .05, 
                                     impedance = NULL){
  if(is.null(impedance)){
    a <- absorption_matrix
  } else {
    a <- absorption_matrix * impedance[colnames(absorption_matrix), rownames(absorption_matrix)]
  }
  df <-  cbind(place = rownames(a), 
               match = colnames(a)[apply(a, 1, which.max)],
               max_absorption_alpha = apply(a, 1, max), 
               second_max_absorption_alpha = apply(a, 1, function(x){max(x[x != max(x), drop = FALSE])}), 
               absorption_alpha_gini = apply(a, 1, gini),
               absorption_alpha_total = apply(a, 1, sum),
               absorption_alpha_mean = apply(a, 1, mean),
               absorption_alpha_sd = apply(a, 1, sd)
  ) %>% as.data.frame()
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


############ Call connectedness for any available year and industry scale
connectedness <- function (cbp_year,
                           ilevel = c("det", "sum", "sec"),
                           scale = c("county", "state", "us"),
                           output_metric = c("ap", "emp", "qp1", "est"),
                           data_dir = file.path("data", "robjs"),
                           labor_share_year = bea_year,
                           tiger_year = cbp_year,
                           bea_year = cbp_year,
                           threshold = .05,
                           impedance = NULL, 
                           normalized = TRUE,
                           ag_year = c("2017", "2012", "2007", "2002"), 
                           geo_level = c("county", "state", "national")){
  
  o <- total_output_tidy(year = cbp_year,
                         ilevel = ilevel,
                         scale = scale,
                         output_metric = output_metric,
                         data_dir = data_dir,
                         labor_share_year = labor_share_year,
                         tiger_year = tiger_year,
                         ag_year = ag_year, 
                         geo_level = geo_level)
  d <- direct_requirements(year = bea_year, 
                           ilevel = ilevel)
  i <- industry_input(d, o)
  s <- stacked_absorption_share(net_input_supply(o, i), net_input_demand(o, i))
  
  if(!isTRUE(normalized)){
    n <- s
  } else {
    n <- normalized_absorption_share(s, net_input_supply(o, i))
  }
  
  df <- absorption_maximum_match(n, threshold = threshold, impedance = impedance)
  
  return(df)
}


############ Call connectedness for any available year and industry scale with spatial component
spatial_connectedness <- function (cbp_year,
                                   ilevel = c("det", "sum", "sec"),
                                   scale = c("county", "state", "us"),
                                   output_metric = c("ap", "emp", "qp1", "est"),
                                   data_dir = file.path("data", "robjs"),
                                   labor_share_year = bea_year,
                                   tiger_year = cbp_year,
                                   bea_year = cbp_year,
                                   threshold = .05,
                                   impedance = NULL, 
                                   normalized = TRUE,
                                   ag_year = c("2017", "2012", "2007", "2002"), 
                                   geo_level = c("county", "state", "national")){
  c <- connectedness(cbp_year = cbp_year,
                     ilevel = ilevel,
                     scale = scale,
                     output_metric = output_metric,
                     data_dir = data_dir,
                     labor_share_year = labor_share_year,
                     tiger_year = tiger_year,
                     bea_year = bea_year,
                     threshold = threshold,
                     impedance = impedance, 
                     normalized = normalized,
                     ag_year = ag_year, 
                     geo_level = geo_level)
  s <- tiger_rucc(tiger_year) %>% 
        select(place, NAME, STATE_CODE, COUNTY_CODE, FIPS, STATE, COUNTY, RUC_CODE, POPULATION, geometry, center)
  df <- join_space_with_connectedness(c, s)
  return(df)
}


############ Spatial union each ECA member in a cluster
spatial_cluster <- function(spatial_connectedness_table,
                            list_names = NULL, 
                            place = place, 
                            geometry = geometry, 
                            eca_membership = eca_membership){
  
  df <-  spatial_connectedness_table
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
                                           output_metric = c("ap", "emp", "qp1", "est"),
                                           data_dir = file.path("data", "robjs"),
                                           labor_share_year = bea_year,
                                           tiger_year = cbp_year,
                                           bea_year = cbp_year,
                                           threshold = .05,
                                           impedance = NULL,
                                           list_names = NULL,
                                           normalized = normalized,
                                           ag_year = c("2017", "2012", "2007", "2002"), 
                                           geo_level = c("county", "state", "national")){
  df <- spatial_connectedness(cbp_year = cbp_year,
                              ilevel = ilevel,
                              scale = scale,
                              output_metric = output_metric,
                              data_dir = data_dir,
                              labor_share_year = labor_share_year,
                              tiger_year = tiger_year,
                              bea_year = bea_year,
                              threshold = threshold,
                              impedance = impedance,
                              normalized = normalized,
                              ag_year = ag_year, 
                              geo_level = geo_level)
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
                                  output_metric = c("ap", "emp", "qp1", "est"),
                                  data_dir = file.path("data", "robjs"),
                                  labor_share_year = bea_year,
                                  tiger_year = cbp_year,
                                  bea_year = cbp_year,
                                  impedance = NULL, 
                                  normalized = TRUE,
                                  ag_year = c("2017", "2012", "2007", "2002"), 
                                  geo_level = c("county", "state", "national")){
  
  o <- total_output_tidy(year = cbp_year,
                         ilevel = ilevel,
                         scale = scale,
                         output_metric = output_metric,
                         data_dir = data_dir,
                         labor_share_year = labor_share_year,
                         tiger_year = tiger_year,
                         ag_year = ag_year, 
                         geo_level = geo_level)
  d <- direct_requirements(year = bea_year, 
                           ilevel = ilevel)
  i <- industry_input(d, o)
  s <- stacked_absorption_share(net_input_supply(o, i), net_input_demand(o, i))
  
  if(!isTRUE(normalized)){
    n <- s
  } else {
    n <- normalized_absorption_share(s, net_input_supply(o, i))
  }
  
  if(is.null(impedance)){
    a <- n
  } else {
    a <- n * impedance[colnames(n), rownames(n)]
  }
  
  b <- cbind(export_absorption = c(t(a[central_place, , drop=FALSE])),
             import_absorption = c(a[, central_place , drop=FALSE]),
             place = rownames(a))
  
  t <- tiger_rucc(tiger_year)
  
  df <- join_space_with_connectedness(b, t)
  
  df$export_absorption <- as.numeric(df$export_absorption)
  df$import_absorption <- as.numeric(df$import_absorption)
  
  return(df)
  
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





################################### 
################################### 
################################### To respecify: absorbr, graphr, cus_pal_maps, mapr2, maprCNT, maprECA

##### Absorption tables
absorbr <- function(top_absorb, all_absorb, in_core, im_mat, ex_mat, impind, macc, industry_levels){
  df_core <- vector(mode='list', length=length(industry_levels))
  names(df_core) <- industry_levels
  out_core <- df_core
  
  df_top <- df_core
  df_all <- df_core
  
  for (l in 1:length(industry_levels)){
    df_core[[l]] <- im_mat[[l]][, impind] %>% as.data.frame() %>% select(order(colnames(.)))
    df_core[[l]]$industry_label <- rownames(df_core[[l]])
    df_core[[l]] <- melt(df_core[[l]], id = "industry_label") %>% rename(NIS = value, core_fips = variable)
  }
  
  for (l in 1:length(industry_levels)){
    for (p in 1:length(macc)){
      out_core[[l]][[p]] <- ex_mat[[l]][, macc][, p] %>% as.data.frame()
      colnames(out_core[[l]][[p]]) <- c("NIE")
      out_core[[l]][[p]]$industry_label <- rownames(out_core[[l]][[p]])
      
    }
    names(out_core[[l]]) <- colnames(ex_mat[[l]][, macc])
  }
  
  for (l in 1:length(industry_levels)){
    for (p in 1:length(macc)){
      df_top[[l]][[p]] <- inner_join(out_core[[l]][[p]], df_core[[l]], by = "industry_label") %>% arrange(core_fips, industry_label) 
      df_top[[l]][[p]]$absorb <- pmin(df_top[[l]][[p]]$NIE, df_top[[l]][[p]]$NIS)
      df_top[[l]][[p]] <- df_top[[l]][[p]] %>% group_by(core_fips) %>% mutate(ab_sum = sum(absorb))
      df_all[[l]][[p]] <- df_top[[l]][[p]]
      df_top[[l]][[p]]$trim <- ""
      df_top[[l]][[p]] <- df_top[[l]][[p]] %>% filter(NIE > 1)
      df_top[[l]][[p]] <- df_top[[l]][[p]] %>% filter(absorb > 0)
      
      df_top[[l]][[p]] <- df_top[[l]][[p]] %>% arrange(desc(NIE)) %>% subset( NIE %in% head(unique(NIE),5))
      
      if (length(unique(df_top[[l]][[p]]$industry_label)) > 20){
        df_top[[l]][[p]] <- df_top[[l]][[p]] %>% filter(absorb > 0) %>% group_by(core_fips) %>% slice_max(order_by = absorb, n = 7) %>% arrange(core_fips)
        df_top[[l]][[p]]$trim <- "(top truncated values)"
      }
      df_top[[l]][[p]] <- df_top[[l]][[p]] %>% group_by(industry_label) %>% mutate(indust_count = n())
    }
    names(df_top[[l]]) <- names(macc)
  }
  
  assign(deparse(substitute(in_core)), df_core, envir=.GlobalEnv) 
  assign(deparse(substitute(top_absorb)), df_top, envir=.GlobalEnv) 
  assign(deparse(substitute(all_absorb)), df_all, envir=.GlobalEnv) 
}

###Graphs of import and export absorption
graphr <- function(specname, top_absorb, TIGER_RUCC, my_pal, macc, industry_levels){
  
  df <- vector(mode='list', length=length(industry_levels))
  names(df) <- industry_levels
  for (l in 1:length(industry_levels)){
    df[[l]] <- vector(mode='list', length=length(macc))
    names( df[[l]]) <- macc
  }
  
  for (l in 1:length(industry_levels)){
    for (p in 1:length(macc)){
      county <- TIGER_RUCC %>% filter(TIGER_RUCC$FIPS %in% macc[p]) %>% pull(COUNTY)  
      if  (dim(top_absorb[[l]][[p]])[1] == 0){
        df[[l]][[p]] <- c(glue("No absorbion overlap for {county}"))
      } else {
        trim <- top_absorb[[l]][[p]]$trim[1]
        df[[l]][[p]] <- ggplot(top_absorb[[l]][[p]]) +
          geom_col_interactive(
            aes(x = industry_label,
                y = (NIE/indust_count),
                tooltip = glue("NIE: {round(NIE)}")
            ),
            fill = "black") +
          geom_point_interactive(
            aes(x = industry_label,
                y = (NIS),
                color = core_fips,
                tooltip = glue("NIS: {round(NIS)}") 
            ), 
            size  = 3.5) +  
          labs(x = glue("Industry Sector {trim}"), y = "Input Value") +
          theme_bw() +
          labs(title = "Absorption Distribution Capacitance",
               subtitle = glue("Net Excess: {county}"),
               color = "Net Shortage:") +
          #scale_colour_brewer(palette = "Set3", labels = (TIGER_RUCC %>% filter(TIGER_RUCC$FIPS %in% colnames(cuml_core_out[[l]])) %>% pull(COUNTY)) ) +
          scale_color_manual(values = c(my_pal[as.vector(unique(in_core[[l]]$core_fips))]),
                             labels = paste0(TIGER_RUCC %>% filter(TIGER_RUCC$FIPS %in% as.vector(unique(in_core[[l]]$core_fips))) %>% pull(COUNTY), ":\n Absorption ",
                                             top_absorb[[l]][[p]] %>% arrange(core_fips) %>%  .$ab_sum  %>%  unique() %>% round(), "\n") ) +
          theme(axis.text.x = element_text(angle=45, hjust=1),
                legend.position = "top", 
                legend.key.size = unit(.2, "cm")) 
        # if (isFALSE(all(top_absorb[[l]][[p]]$NIS < sd(top_absorb[[l]][[p]]$NIS)*3))){
        #     if (mean(top_absorb[[l]][[p]]$NIS) > sd(top_absorb[[l]][[p]]$NIS)){
        #          df[[l]][[p]] <- df[[l]][[p]] + facet_zoom(ylim = c(0, (mean(top_absorb[[l]][[p]]$NIS) + (sd(top_absorb[[l]][[p]]$NIS)*3))))
        #     } else{
        #         df[[l]][[p]] <- df[[l]][[p]] +  facet_zoom(ylim = c(0, (median(top_absorb[[l]][[p]]$NIS) + (sd(top_absorb[[l]][[p]]$NIS)/3))))
        #     }
        # }
      }
    }
  }
  assign(deparse(substitute(specname)), df, envir=.GlobalEnv)
}

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

####Mapping Economic and Spatial cluster matching with isolation
mapr2 <- function(specname, shade, hatch, hm, my_pal){
  df <- vector(mode='list', length=length(industry_levels))
  names(df) <- industry_levels
  for (l in 3){ 
  #for (l in 1:length(industry_levels)){  
    df[[l]] <- ggplot( hm[[l]] ) +
      { if(isFALSE(shade))
        geom_sf_interactive(aes(fill = lab, 
                                tooltip = glue("County: {NAME}\nFIPS: {place}\nMatch: {match_name}\nValue: {q_value}"), 
                                data_id = place
        ), 
        environment = environment(),
        color = NA
        ) }+ 
      { if(isTRUE(shade))
        geom_sf_interactive(aes(fill = lab, 
                                alpha = a_value, 
                                tooltip = glue("County: {NAME}\nFIPS: {place}\nMatch: {match_name}\nValue: {q_value}"), 
                                data_id = place
        ), 
        color = NA
        ) }+ 
      { if(isTRUE(hatch))
        geom_sf_pattern(data = filter(hm[[l]], place %in% unique(hm[[l]]$match)) ,
                        aes(fill = lab
                        ),
                        pattern = 'crosshatch',
                        pattern_size = .01,
                        pattern_density = 0.01, 
                        pattern_spacing = 0.07,
                        pattern_fill    = 'black',
                        pattern_colour  = 'black'
        )}+ 
      guides(alpha = "none") +
      coord_sf() +
      theme_void() +
      labs(fill = "Cluster Core",
           caption = paste0(isolation_th*100,"% Isolation Threshold")) +
      scale_fill_manual(values = my_pal)
  }
  assign(deparse(substitute(specname)), df, envir=.GlobalEnv)     
} 

####Mapping Economic and Spatial cluster matching with isolation
maprCNT <- function(specname, shade, hatch, hm, my_pal){
  df <- vector(mode='list', length=length(industry_levels))
  names(df) <- industry_levels
  
  for (l in 3){  
   # for (l in 1:length(industry_levels)){  
    df[[l]] <- ggplot( hm[[l]] ) +
      { if(isFALSE(shade))
        geom_sf_interactive(aes(fill = cnt_place, 
                                tooltip = glue("County: {NAME}\nFIPS: {place}\nMatch: {match_name}\nValue: {q_value}"), 
                                data_id = place
        ), 
        environment = environment(),
        color = NA
        ) }+ 
      { if(isTRUE(shade))
        geom_sf_interactive(aes(fill = cnt_place, 
                                alpha = a_value, 
                                tooltip = glue("County: {NAME}\nFIPS: {place}\nMatch: {match_name}\nValue: {q_value}"), 
                                data_id = place
        ), 
        color = NA
        ) }+ 
      { if(isTRUE(hatch))
        geom_sf_pattern(data = filter(hm[[l]], place %in% unique(hm[[l]]$match)) ,
                        aes(fill = cnt_place
                        ),
                        pattern = 'crosshatch',
                        pattern_size = .01,
                        pattern_density = 0.01, 
                        pattern_spacing = 0.07,
                        pattern_fill    = 'black',
                        pattern_colour  = 'black'
        )}+ 
      guides(alpha = "none") +
      coord_sf() +
      theme_void() +
      labs(fill = "Cluster Core",
           caption = paste0(isolation_th*100,"% Isolation Threshold")) +
      scale_fill_manual(values = my_pal)
  }
  assign(deparse(substitute(specname)), df, envir=.GlobalEnv)     
} 

####Mapping Economic and Spatial cluster matching with isolation
maprECA <- function(specname, shade, hatch, hm, my_pal){
  df <- vector(mode='list', length=length(industry_levels))
  names(df) <- industry_levels
  for (l in 3){ 
  #for (l in 1:length(industry_levels)){  
    df[[l]] <- ggplot( hm[[l]] ) +
      { if(isFALSE(shade))
        geom_sf_interactive(aes(fill = eca, 
                                tooltip = glue("County: {NAME}\nFIPS: {place}\nMatch: {match_name}\nValue: {q_value}"), 
                                data_id = place
        ), 
        color = NA
        ) }+ 
      { if(isTRUE(shade))
        geom_sf_interactive(aes(fill = eca, 
                                alpha = a_value, 
                                tooltip = glue("County: {NAME}\nFIPS: {place}\nMatch: {match_name}\nValue: {q_value}"), 
                                data_id = place
        ), 
        color = NA
        ) }+ 
      { if(isTRUE(hatch))
        geom_sf_pattern(data = filter(hm[[l]], place %in% unique(hm[[l]]$match)) ,
                        aes(fill = eca
                        ), pattern = 'crosshatch',
                        pattern_size = .01,
                        pattern_density = 0.01, 
                        pattern_spacing = 0.07,
                        pattern_fill    = 'black',
                        pattern_colour  = 'black'
        )}+ 
      guides(alpha = "none") +
      coord_sf() +
      theme_void() +
      labs(fill = "ECA Core",
           caption = paste0(isolation_th*100,"% Isolation Threshold")) +
      scale_fill_manual(values = my_pal)
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





