# Function repository 

# Load and attach necessary packages
library(rprojroot)
library(fs)
library(readxl)
library(openxlsx)
library(rlog)
library(reticulate)

# Display start time
log_info("Define functions start")


# Load conda environment "rurec"
use_condaenv('rurec')

# Import pubdata Python modules
bds <- import("rurec.pubdata.bds")
bea_io <- import("rurec.pubdata.bea_io")
cbp <- import("rurec.pubdata.cbp")
ers_rurality <- import("rurec.pubdata.ers_rurality")
geography <- import("rurec.pubdata.geography")
naics <- import("rurec.pubdata.naics")
population <- import("rurec.pubdata.population")



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




#Function add NAICS to BEA codes  
concordr <- function(cordname, econpath = "CBP_2019p", filepath = file.path("data", "robjs")){
  econ <- file.path(find_rstudio_root_file(), filepath, econpath) %>% readRDS()
  cord <- file.path(find_rstudio_root_file(), filepath, cordname) %>% readRDS()
  x <- left_join(econ, cord, by = "NAICS") 
  x <- x %>%
    filter(.[9] != "NULL") %>%
    group_by(place, .[9]) %>%
    summarise(across(where(is.numeric), sum), .groups = 'drop') %>%
    as.data.frame()
  x <- x %>%
    group_by(place) %>%
    arrange(factor(x[[2]], levels = unique(readRDS(file.path(data_dir, cordname))[[1]])), .by_group = TRUE) %>%
    as.data.frame()
  return(x)
}


#Function to generate full economic industry/county table 
reshaper <- function(industry_data_frame, cordname){ 
  x <- reshape(industry_data_frame, idvar = names(industry_data_frame[2]), timevar = "place", direction = "wide") %>% 
    suppressWarnings()
  x <- x %>% arrange(factor(x[[1]], levels = unique(readRDS(file.path(data_dir, cordname))[[1]])), .by_group = TRUE)
  rownames(x) <- x[,1]
  x[is.na(x)] <- 0
  colnames(x)[1] <- "indcode"
  x <- x %>% reshape(idvar = "place", varying = c(colnames(x)[-1]), direction = "long")
  rownames(x) <- 1:nrow(x)
  names(x)[names(x)=="time"] <- "place"
  x$place <- x$place  %>% formatC(width = 5, format = "d", flag = "0")
  x$emp <-  as.numeric(x$emp)
  x$qp1 <-  as.numeric(x$qp1)
  x$ap <-  as.numeric(x$ap)
  x$est <-  as.numeric(x$est)
  x <- x[1:6]
  assign(paste0(deparse(substitute(industry_data_frame)), "_XBEA"), x, envir=.GlobalEnv)
}





############ Industry Input Needs 
### For each industry specificity level (sector, summary, detail), derive the industry-by-county matrix of input needs DY
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
### For each industry specificity level (sector, summary, detail), derive the industry-by-county matrix of net input demand 
net_input_demand <- function(industry_output_matrix, 
                             industry_input_matrix){
    i <- industry_input_matrix
    o <- industry_output_matrix
    df <- pmax(i - o, 0)
}

############ Net Input Supply
### derive the industry-by-county matrix of net input supply 
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
                                     threshold=.05){
  a <- absorption_matrix
  df <-  cbind(place = rownames(a), 
               match = colnames(a)[apply(a, 1, which.max)], 
               max_absorption_alpha = apply(a, 1, max), 
               second_max_absorption_alpha = apply(a, 1, function(x){max(x[x != max(x), drop = FALSE])})
               ) %>% as.data.frame()
  df$max_absorption_alpha <- as.numeric(df$max_absorption)
  df$second_max_absorption_alpha <- as.numeric(df$second_max_absorption)
  
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
  df <- df %>% select(place, max_absorption_alpha, second_max_absorption_alpha, cluster_category, eca_membership, cluster_members_count)
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


############ Single function of nested functions to derive a hierarchies of connectedness tables and resulting output matricies from a base single output matrix and single direct requirements matrix
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




##### To respecify: absorbr, graphr, cus_pal_maps, mapr2, maprCNT, maprECA

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





