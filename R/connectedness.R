
# This script calculates absorption matrices, 



# R libraries ----
library(logger)
log_threshold(DEBUG)
library(arrow)
library(tidyverse)
library(glue)
library(REAT)


# R scripts ----
source("R/basic_utilities.R", local = (util <- new.env()))
source("R/geography.R", local = (geog <- new.env()))
source("R/place_output.R", local = (place_output <- new.env()))
source("R/dataprep_bea_io.R", local = (bea_io <- new.env()))


# Data objects ----
ipath <- list(
  # data dependencies
)

opath <- list(
  abmatrix_ = "data/connectedness/abmatrix_{year}_{class_system}_{ilevel}_{bus_data}_{cbsa}_{paradigm}.pq"
  # eca_ = "data/connectedness/eca_{year}_{class_system}_{ilevel}_{bus_data}_{cbsa}_{paradigm}_{normalized}_{central}_{flow_direction}_{function_class}_{threshold}_{alpha_vector}_{impedance}_{functional_form}_{scalar_constant}_{from}_{trim}.pq"
  # data products
)

clear_outputs <- function() {
  util$clear_paths(opath)
}

# Absorption functions----

# Stacked Absorption Shares function
stacked_absorption_share <- function(net_supply_matrix, 
                                     net_demand_matrix,
                                     verbose = FALSE){
  s <- net_supply_matrix
  d <- net_demand_matrix
  x <- rep(c(1), each=nrow(s))
  
  ## Check counties match 
  df <- identical(colnames(d), colnames(s))
  stopifnot(df)
  if (verbose) cat(paste("Absorption calculation started", Sys.time() ))
  df <-  matrix(0, nrow = ncol(s), ncol = ncol(s) )
  rownames(df) = colnames(df) <- colnames(s)
  for (i in 1:ncol(s)){
    if (verbose) cat(paste("Starting row ", i, " of ", ncol(s), "\n"))
    for (j in 1:ncol(s)){
      df[i,j] <- (x %*% pmin(s[,i], d[,j]))
    }
  }
  if (verbose) cat(paste("Absorption calculation finished", Sys.time() ))
  return(df)
}

# Normalized Absorption Shares function
normalized_absorption_share <- function(absorption_share_matrix, 
                                        net_supply_matrix){
  s <- absorption_share_matrix
  n <- net_supply_matrix
  df <- s / colSums(n)
  df[is.na(df)] = 0
  return(df)
}

# Apply an impedance factor to a place-by-place economic activity matrix 
apply_impedance <- function(activity_matrix,
                            impedance_matrix){
  places <- intersect(colnames(activity_matrix), colnames(activity_matrix)) %>% 
    {intersect(., colnames(impedance_matrix))} %>% 
    {intersect(., rownames(impedance_matrix))}
  df <- activity_matrix[places, places] * impedance_matrix[places, places]
  return(df)
}

# Apply various approaches to generate single "absorption" statistic from n-by-n matrix possibility space
apply_absorption_metrics <- function(absorption_matrix,
                                     flow_direction = c("out", "in"),
                                     function_class = c("max", "gini", "sum", "mean", "sd") ){
  
  fc <- match.arg(function_class)
  fd <- match.arg(flow_direction)
  if(fd == "out"){fd = 1} 
  if(fd == "in"){fd = 2} 
  a <- absorption_matrix
  df <- cbind(place = rownames(a), 
              match = colnames(a)[apply(a, fd, which.max)],
              max_alpha = apply(a, fd, fc), 
              #second_max_alpha = apply(a, fd function(x){max(x[x != max(x), drop = FALSE])}),
              max_count = apply(a, fd, function(x) {sum(max(x) == x)})
  ) %>% 
    as.data.frame()
  df[, 3:ncol(df)] <- lapply(3:ncol(df), function(x) as.numeric(df[[x]]))
  return(df)
}

# Apply maximum absorption match algorithm function
apply_absorption_algorithm <- function(absorption_metric_table, 
                                       threshold = .05,
                                       place_vector = "place",
                                       match_vector = "match",
                                       alpha_vector = "max_alpha"){
  df <- absorption_metric_table
  
  ### cluster_class reevaluates the maximum absorption match to account for an isolation threshold and ECA isolated corner cases (i.e., no one imports your excess so you are isolated, but you are max import sink for someone else)
  df$cluster_class <- df[[match_vector]]
  df$cluster_class[df[[alpha_vector]] < threshold] <- "Isolated"
  df$cluster_class[df[[place_vector]] %in% unique(df[[match_vector]]) & df$cluster_class == "Isolated"] <- "ECA Isolated"
  
  ### eca_class reevaluates the maximum absorption match and returns the corrected self-match locations for "ECA Isolated" and "Cluster Core" locations 
  df$eca_class <- df$cluster_class
  df$eca_class[df$cluster_class == "ECA Isolated"] <- df[[place_vector]][df$cluster_class == "ECA Isolated"]
  df$eca_class[df[[place_vector]] %in% unique(df$cluster_class)] <- df[[place_vector]][df[[place_vector]] %in% unique(df$cluster_class)]
  
  ### cluster_category gives the categorical classification of each location as one of: "Isolated", "Isolated, Cluster Sink", "Cluster Sink", or "Cluster Source"
  df$cluster_category <- df$cluster_class
  df$cluster_category[df[[place_vector]] %in% unique(df$cluster_class)] <- "Cluster Sink"
  df$cluster_category[df$eca_class != df[[place_vector]]] <- "Cluster Source"
  df$cluster_category[df$cluster_class == "Isolated"] <- "Isolated"
  df$cluster_category[df$cluster_class == "ECA Isolated"] <- "Isolated, Cluster Sink"
  
  ### eca_membership gives all places their ECA corrected matching location explicitly
  df$eca_membership <- df$eca_class
  df$eca_membership[df$eca_class == "Isolated"] <- df[[place_vector]][df$eca_class == "Isolated"]
  
  ### cluster_members_count is a tally of the number of places belonging to a cluster
  df <- df %>% group_by(eca_membership) %>% mutate(cluster_members_count = n()) %>% ungroup() %>% as.data.frame()
  return(df)
}

### Generate vector of maximum non-impedance absorption values using impedance scaled absorption matrix
noimpedance_absorption_maximum <- function(absorption_matrix, 
                                           impedance_matrix, 
                                           flow_direction = c("out", "in")){
  
  flow_direction <- match.arg(flow_direction)
  if(flow_direction == "out"){fd = 1} 
  if(flow_direction == "in"){fd = 2} 
  
  a <- absorption_matrix
  df <- c()
  x <- apply_impedance(activity_matrix = a, 
                       impedance_matrix = impedance_matrix)
  x <- apply(x, fd, which.max)
  if(flow_direction == "out"){
    for(i in names(x)){
      m <- names(x)[x[[which(names(x) == i)]]]
      df <- rbind(df, a[i, m])
    }
  } 
  if(flow_direction == "in"){
    for(i in names(x)){
      m <- names(x)[x[[which(names(x) == i)]]]
      df <- rbind(df, a[m, i])
    }
  }
  df <- as.data.frame(df)
  rownames(df) <- names(x)
  colnames(df) <- "ab_max"
  return(df)
}


# Absorption tables----

# Call nominal absorption table
call_nominal_absorption_table <- function(year,
                                          class_system = c("industry", "commodity"), 
                                          paradigm = c("factor", "domestic", "capital"),
                                          ilevel = c("det", "sum", "sec"),
                                          bus_data = c("cbp_imp", "cbp_raw", "infogroup"),
                                          cbsa = FALSE,
                                          verbose = FALSE){
  class_system <- match.arg(class_system)
  paradigm <- match.arg(paradigm)
  ilevel <- match.arg(ilevel)
  bus_data <- match.arg(bus_data)
  
  cache_path <- glue(opath$abmatrix_)
  if (file.exists(cache_path)) {
    log_debug(paste("read from cache", cache_path))
    return(read_parquet(cache_path))
  }
  
  df <- place_output$call_factor_list(year = year, 
                                       class_system = class_system,
                                       paradigm = paradigm,
                                       ilevel = ilevel,
                                       bus_data = bus_data,
                                       cbsa = cbsa,
                                       verbose = verbose) 
  nis_matrix <- df %>% 
    {.[c("indcode", "place", "net_supply")]} %>% 
    util$long2matrix()
  
  nid_matrix <- df %>% 
    {.[c("indcode", "place", "net_demand")]} %>% 
    util$long2matrix()

  df <- stacked_absorption_share(net_supply_matrix = nis_matrix, 
                                 net_demand_matrix = nid_matrix,
                                 verbose = verbose) %>%
    as.data.frame.table() %>% 
    `colnames<-`(c("rows", "cols", "value"))
  
  log_debug(paste("save to cache", cache_path))
  write_parquet(df, util$mkdir(cache_path))
  
  return(df)  
  
}

# Call normalized absorption table
call_normalized_absorption_table <- function(year,
                                             class_system = c("industry", "commodity"), 
                                             paradigm = c("factor", "domestic", "capital"),
                                             ilevel = c("det", "sum", "sec"),
                                             bus_data = c("cbp_imp", "cbp_raw", "infogroup"),
                                             cbsa = FALSE, 
                                             verbose = FALSE){
  
  nis_matrix <- place_output$call_factor_list(year = year, 
                                               class_system = class_system,
                                               ilevel = ilevel,
                                               bus_data = bus_data,
                                               cbsa = cbsa,
                                               verbose = verbose) %>% 
    {.[c("indcode", "place", "net_supply")]} %>% 
    util$long2matrix()
  
  abt <- call_nominal_absorption_table(year = year, 
                                       class_system = class_system,
                                       paradigm = paradigm,
                                       ilevel = ilevel,
                                       bus_data = bus_data,
                                       cbsa = cbsa,
                                       verbose = verbose) %>% 
    util$long2matrix()
  
  df <- normalized_absorption_share(absorption_share_matrix = abt, 
                                    net_supply_matrix = nis_matrix) %>%
    as.data.frame.table() %>% 
    `colnames<-`(c("rows", "cols", "value"))
  
  return(df)
}

# Call Absorption table
call_absorption_table <- function(year,
                                  class_system = c("industry", "commodity"), 
                                  paradigm = c("factor", "domestic", "capital"),
                                  ilevel = c("det", "sum", "sec"),
                                  bus_data = c("cbp_imp", "cbp_raw", "infogroup"),
                                  cbsa = FALSE,
                                  normalized = TRUE, 
                                  verbose = FALSE){
  
  if(normalized){
    df <- call_normalized_absorption_table(year = year, 
                                           class_system = class_system,
                                           paradigm = paradigm,
                                           ilevel = ilevel,
                                           bus_data = bus_data,
                                           cbsa = cbsa,
                                           verbose = verbose) 
  } else {
    df <- call_nominal_absorption_table(year = year, 
                                        class_system = class_system,
                                        paradigm = paradigm,
                                        ilevel = ilevel,
                                        bus_data = bus_data,
                                        cbsa = cbsa,
                                        verbose = verbose)
    return(df)
  }
}


# ECA functions----

# generate table of ECA's from a given absorption matrix
eca_table <- function(absorption_matrix,
                      impedance_matrix = NULL,
                      flow_direction = c("out", "in"),
                      function_class = c("max", "gini", "sum", "mean", "sd"),
                      threshold = .05,
                      alpha_vector = "max_alpha",
                      trim = "^(02|15|60|66|69|72|78)|(999)$",
                      verbose = FALSE){
  flow_direction <- match.arg(flow_direction)
  function_class <- match.arg(function_class)
  abmat <- absorption_matrix
  if(!is.null(trim)){
    abmat <- trim %>% 
      {!grepl(., rownames(abmat))} %>% 
      {rownames(abmat)[.]} %>% 
      {abmat[., ., drop = F]}
  }
  if(!is.null(impedance_matrix)){
      nom <- noimpedance_absorption_maximum(absorption_matrix = abmat, 
                                            impedance_matrix = impedance_matrix, 
                                            flow_direction = flow_direction)
    abmat <- apply_impedance(activity_matrix = abmat, 
                             impedance_matrix = impedance_matrix)
  }

  df <- apply_absorption_metrics(absorption_matrix = abmat,
                                 flow_direction = flow_direction,
                                 function_class = function_class)

  df <- apply_absorption_algorithm(absorption_metric_table = df, 
                                   threshold = threshold,
                                   alpha_vector = alpha_vector)
  if(!is.null(impedance_matrix)){
      df$nominal_values <- nom
  }
  return(df)
}

# generate place centric table of ECA's from a given absorption matrix
eca_table_central <- function(central_place, 
                              absorption_matrix,
                              year = 2012,
                              impedance_matrix = NULL,
                              cbsa = FALSE,
                              trim = "^(02|15|60|66|69|72|78)|(999)$"){
  abmat <- absorption_matrix
  if(!is.null(trim)){
    abmat <- trim %>% 
      {!grepl(., rownames(abmat))} %>% 
      {rownames(abmat)[.]} %>% 
      {abmat[., ., drop = F]}
  }
  if(!is.null(impedance_matrix)){
    abmat <- apply_impedance(activity_matrix = abmat, 
                             impedance_matrix = impedance_matrix)
  }
  if(cbsa){
    central_place <- geog$fips2cbsa(fips = central_place, 
                                    year = year)
  }
  df <- cbind(export_absorption = c(t(abmat[central_place, , drop = FALSE])),
              import_absorption = c(abmat[, central_place, drop = FALSE]),
              place = rownames(abmat)) %>% 
    as.data.frame()
  return(df)
}



# Aggregate economic industry output of each ECA member in a cluster, keep all non source places as ECA core unit label
eca_aggregate_industry_output <- function(industry_output_matrix, 
                                          eca_table){
  df <- industry_output_matrix
  c <- eca_table
  x <- c$eca_membership %>% unique() %>% .[order(.)]
  for(i in x){
    df[, i] <- rowSums(df[, c$place[c$eca_membership == i], drop = FALSE])
  } 
  df <- df[, x, drop = F]
}

# Spatial union each ECA member in a cluster
eca_spatial_cluster <- function(spatial_eca_table,
                                verbose = FALSE){
  sdf <-  spatial_eca_table 
  df <- sdf %>% select(names(.)[!(names(.) %in% c("center"))])
  x <- df$eca_membership %>% unique() %>% .[order(.)]
  for (i in x){
    if(verbose){print(paste("start cluster: ", i, which(i == x), "of", length(x), Sys.time()))}
    df[df$place == i,]$geometry <- df %>% filter(df$eca_membership == i) %>% 
      #st_transform("+proj=eqc") %>% 
      st_union()
    if(verbose){print(paste("end cluster: ", i, which(i == x), "of", length(x), Sys.time()))}
  }
  #df$geometry <- df$geometry %>% st_transform(st_crs(sdf)[[1]]) 
  df$center <- df$geometry %>% st_transform("EPSG:26911") %>% st_centroid() %>% st_transform(st_crs(sdf)[[1]]) 
  df <- df %>% .[.$place %in% .$eca_membership, ]
}


# Single function of nested functions to derive a hierarchies of connectedness tables and resulting output matrices from a base single output matrix and single direct requirements matrix

hierarchical_connectedness <- function(industry_output_matrix,
                                       io_supply_matrix,
                                       io_b_matrix,
                                       phi_vector,
                                       geog_table,
                                       eca_table,
                                       class_system = c("industry", "commodity"),
                                       paradigm = c("factor", "domestic", "capital"),
                                       normalized = TRUE,
                                       flow_direction = c("out", "in"),
                                       function_class = c("max", "gini", "sum", "mean", "sd"),
                                       threshold = .05,
                                       alpha_vector = "max_alpha",
                                       impedance = FALSE,
                                       from = c("center", "border"),
                                       functional_form = c("bisquare", "secant", "gaussian", "exponential", "power", "distance", "queen", "rook", "neighbor"),
                                       scalar_constant = NULL,
                                       verbose = FALSE){
  class_system <- match.arg(class_system)
  iout <- industry_output_matrix
  smat <- io_supply_matrix
  bmat <- io_b_matrix
  phi <- phi_vector
  geot <- geog_table
  ecatab <- eca_table

  hct <- list()
  hsct <- list()
  hom <- list()
  
  pnames <- intersect(geot$place, colnames(iout))
  
  if(class_system == "commodity"){
    hom$level_0 <- place_output$industry2commodity(iout, smat)[, pnames, drop = F]
  } else {
    hom$level_0  <- iout[, pnames, drop = F]
  }
  
  df <- inner_join(geot, ecatab, by = "place", copy = TRUE)
  hct$level_1 <- df
  n = 1
  if (verbose){cat(paste("Start level: ", n, "\n"))}
  
  sdf <- eca_spatial_cluster(spatial_eca_table = df, 
                                      verbose = verbose)
  hsct$level_1 <- sdf
  iout <- eca_aggregate_industry_output(industry_output_matrix = iout,
                                        eca_table = df) %>% as.matrix()
  hom$level_1 <- iout
  n = n + 1

  i = FALSE
  while(i==FALSE){
    if (verbose){cat(paste("Start level: ", n, "\n"))}
    
    clust_geo <- hsct[[paste0("level_", n-1)]] %>% {.[, intersect(names(geot), names(.))]}
    
    nis <- place_output$intermediate_activity(industry_output_matrix = iout,
                                              io_b_matrix = bmat,
                                              io_supply_matrix = smat,
                                              phi = phi,
                                              schedule = "supply",
                                              paradigm = paradigm,
                                              class_system = class_system)
    
    nid <- place_output$intermediate_activity(industry_output_matrix = iout,
                                              io_b_matrix = bmat,
                                              io_supply_matrix = smat,
                                              phi = phi,
                                              schedule = "demand",
                                              paradigm = paradigm,
                                              class_system = class_system)
    
    df <- stacked_absorption_share(net_supply_matrix = nis, 
                                   net_demand_matrix = nid, 
                                   verbose = verbose)
    if(normalized){
      df <- normalized_absorption_share(absorption_share_matrix = df, 
                                        net_supply_matrix = nis)
    }

    if(impedance){
      impedance_matrix <- geog$impedance_mat(spatial_dataframe = sdf,
                                              from = from,
                                              functional_form = functional_form,
                                              scalar_constant = scalar_constant,
                                              meta = FALSE)
    } else {
      impedance_matrix <- NULL
    }
    
    df <- eca_table(absorption_matrix = df,
                    impedance_matrix = impedance_matrix,
                    flow_direction = flow_direction,
                    function_class = function_class,
                    threshold = threshold,
                    alpha_vector = alpha_vector,
                    trim = NULL,
                    verbose = verbose)
    df <- inner_join(clust_geo, df, by = "place", copy = TRUE)
    hct[[paste0("level_", n)]] <- df
    
    i <- length(unique(df$eca_membership)) == length(unique(hct[[paste0("level_", n-1)]]$eca_membership))
    if (i){next}
    sdf <- eca_spatial_cluster(spatial_eca_table = df,
                               verbose = verbose)
    hsct[[paste0("level_", n)]] <- sdf

    iout <- eca_aggregate_industry_output(industry_output_matrix = iout,
                                          eca_table = df) %>% as.matrix()
    hom[[paste0("level_", n)]] <- iout
    n = n + 1
  }
  df <- list("Hierarchical_Connectedness_table" = hct,
             "Hierarchical_Spatial_Cluster_table" = hsct,
             "Hierarchical_Output_mat" = hom)
  return(df)
}



# ECA tables----

# Call table of ECA's (1 level)
call_eca_table <- function(year,
                           normalized = TRUE, 
                           impedance = FALSE,
                           functional_form = c("bisquare", "secant", "gaussian", "exponential", "power", "distance", "queen", "rook", "neighbor"),
                           scalar_constant = NULL,
                           from = c("center", "border"),
                           central_place = NULL,
                           flow_direction = c("out", "in"),
                           function_class = c("max", "gini", "sum", "mean", "sd"),
                           threshold = .05,
                           alpha_vector = "max_alpha",
                           class_system = c("industry", "commodity"), 
                           paradigm = c("factor", "domestic", "capital"),
                           ilevel = c("det", "sum", "sec"),
                           bus_data = c("cbp_imp", "cbp_raw", "infogroup"),
                           cbsa = FALSE, 
                           trim = "^(02|15|60|66|69|72|78)|(999)$",
                           verbose = FALSE){
  
  functional_form <- match.arg(functional_form)
  from <- match.arg(from)
  flow_direction <- match.arg(flow_direction)
  function_class <- match.arg(function_class)
  class_system <- match.arg(class_system)
  paradigm <- match.arg(paradigm)
  ilevel <- match.arg(ilevel)
  bus_data <- match.arg(bus_data)
  
  if(is.null(central_place)){central <- "NA"}else{central <- central_place}
  
  if(!impedance){
    functional_form <- "NA"
    scalar_constant <- "NA"
    from <- "NA"
  }

  # cache_path <- glue(opath$eca_)
  # if (file.exists(cache_path)) {
  #   log_debug(paste("read from cache", cache_path))
  #   return(read_parquet(cache_path))
  # }
  
  if(impedance){
    impmatrix <- geog$call_impedance_mat(functional_form = functional_form,
                                          scalar_constant = scalar_constant,
                                          year = year, 
                                          cbsa = cbsa,
                                          from = from,
                                          meta = TRUE)
    scalar_constant <- impmatrix$scalar
    impedance_matrix <- impmatrix$imatrix
  } else {
    impedance_matrix <- NULL
  }
  
  abmat <- call_absorption_table(year = year, 
                                 class_system = class_system,
                                 paradigm = paradigm,
                                 ilevel = ilevel,
                                 bus_data = bus_data,
                                 cbsa = cbsa,
                                 normalized = normalized,
                                 verbose = verbose) %>% 
    util$long2matrix()
  
  if(!is.null(central_place)){
    df <- eca_table_central(central_place = central_place, 
                            absorption_matrix = abmat,
                            year = year,
                            cbsa = cbsa,
                            impedance_matrix = impedance_matrix,
                            trim = trim)
  } else {
    df <- eca_table(absorption_matrix = abmat,
                    impedance_matrix = impedance_matrix,
                    flow_direction = flow_direction,
                    function_class = function_class,
                    threshold = threshold,
                    alpha_vector = alpha_vector,
                    trim = trim,
                    verbose = verbose)
  }
  
  # log_debug(paste("save to cache", cache_path))
  # write_parquet(df, util$mkdir(cache_path))
  # 
  return(df)
}

# Call table of ECA's (1 level) with spatial data
call_eca_table_spatial <- function(year,
                                   normalized = TRUE, 
                                   impedance = FALSE,
                                   functional_form = c("bisquare", "secant", "gaussian", "exponential", "power", "distance", "queen", "rook", "neighbor"),
                                   scalar_constant = NULL,
                                   from = c("center", "border"),
                                   central_place = NULL,
                                   flow_direction = c("out", "in"),
                                   function_class = c("max", "gini", "sum", "mean", "sd"),
                                   threshold = .05,
                                   alpha_vector = "max_alpha",
                                   class_system = c("industry", "commodity"), 
                                   paradigm = c("factor", "domestic", "capital"),
                                   ilevel = c("det", "sum", "sec"),
                                   bus_data = c("cbp_imp", "cbp_raw", "infogroup"),
                                   cbsa = FALSE, 
                                   trim = "^(02|15|60|66|69|72|78)|(999)$",
                                   verbose = FALSE){
  df <- call_eca_table(year = year, 
                       normalized = normalized, 
                       impedance = impedance,
                       functional_form = functional_form,
                       scalar_constant = scalar_constant,
                       from = from,
                       central_place = central_place,
                       flow_direction = flow_direction,
                       function_class = function_class,
                       threshold = threshold,
                       alpha_vector = alpha_vector,
                       class_system = class_system, 
                       paradigm = paradigm,
                       ilevel = ilevel,
                       bus_data = bus_data,
                       cbsa = cbsa, 
                       trim = trim,
                       verbose = verbose) %>% 
      {inner_join(geog$call_geog(year = year, cbsa = cbsa), ., by = "place", copy = TRUE)}

  return(df)
}

# Call list of hierarchical ECA tables
call_hierarchical_connectedness <- function(year,
                                            class_system = c("industry", "commodity"),
                                            paradigm = c("factor", "domestic", "capital"),
                                            ilevel = c("det", "sum", "sec"),
                                            bus_data = c("cbp_imp", "cbp_raw", "infogroup"),
                                            cbsa = FALSE,
                                            normalized = TRUE,
                                            flow_direction = c("out", "in"),
                                            function_class = c("max", "gini", "sum", "mean", "sd"),
                                            threshold = .05,
                                            alpha_vector = "max_alpha",
                                            impedance = FALSE,
                                            from = c("center", "border"),
                                            functional_form = c("bisquare", "secant", "gaussian", "exponential", "power", "distance", "queen", "rook", "neighbor"),
                                            scalar_constant = NULL,
                                            trim = "^(02|15|60|66|69|72|78)|(999)$",
                                            verbose = FALSE){
  functional_form <- match.arg(functional_form)
  from <- match.arg(from)
  flow_direction <- match.arg(flow_direction)
  function_class <- match.arg(function_class)
  class_system <- match.arg(class_system)
  ilevel <- match.arg(ilevel)
  bus_data <- match.arg(bus_data)

  
  iout <- place_output$call_output(year = year, 
                                   class_system = "industry", 
                                   ilevel = ilevel, 
                                   bus_data = bus_data,
                                   verbose = verbose) %>% util$long2matrix()
  smat <- bea_io$call_supply_matrix(year = year, 
                                    ilevel = ilevel, 
                                    condense = TRUE)
  bmat <- bea_io$call_b_matrix(year = year, 
                               ilevel = ilevel, 
                               condense = TRUE) 
  phi <- bea_io$call_commodity_share_factor(year = year, 
                                            ilevel = ilevel, 
                                            condense = TRUE)
  geot <- geog$call_geog(year = year, 
                         cbsa = cbsa,
                         verbose = verbose)
  
  
  ecat <- call_eca_table(year = year,
                         normalized = normalized, 
                         impedance = impedance,
                         functional_form = functional_form,
                         scalar_constant = scalar_constant,
                         from = from,
                         flow_direction = flow_direction,
                         function_class = function_class,
                         threshold = threshold,
                         alpha_vector = alpha_vector,
                         class_system = class_system, 
                         paradigm = paradigm,
                         ilevel = ilevel,
                         bus_data = bus_data,
                         cbsa = cbsa, 
                         trim = trim,
                         verbose = verbose)
  
  if(!is.null(trim)){
    iout <- trim %>% 
      {!grepl(., colnames(iout))} %>% 
      {iout[, ., drop = F]}
  }

  df <- hierarchical_connectedness(industry_output_matrix = iout,
                                   io_supply_matrix = smat,
                                   io_b_matrix = bmat,
                                   phi_vector = phi,
                                   geog_table = geot,
                                   eca_table = ecat,
                                   class_system = class_system,
                                   paradigm = paradigm,
                                   normalized = normalized,
                                   flow_direction = flow_direction,
                                   function_class = function_class,
                                   threshold = threshold,
                                   alpha_vector = alpha_vector,
                                   impedance = impedance,
                                   from = from,
                                   functional_form = functional_form,
                                   scalar_constant = scalar_constant,
                                   verbose = verbose)
  return(df)
}



# eca_hierarchical_connectedness <- function(){}

# update n vector and place_centric_connect inputs
############ Change in connectedness over time for a county 
#place_connect_delta <-


# ############ Absorption matching outcomes over time
# absorption_match_overtime <- function(years = 2000:2020,
#                                       ...){
#   dis <- vector("list", length(years))
#   names(dis) <- years
#   for (y in years){
#     dis[[y]] <- connectedness(year = y, ...)
#   }
#   df <- bind_rows(dis, .id = "id")
#   return(df)
# }

# Tests ----





