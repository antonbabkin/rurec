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





#Function to map similarity indices 
bmapr <- function(specname,
                 list_of_sim_specifications = Sim_list, 
                 industry_level_names = names(Total_mat),
                 r_extent = rural_extent,
                 p_extent = primary_extent,
                 space_vec = TIGER_RUCC,
                 impedance = NULL){
  
  Sim <- vector(mode='list', length=length(industry_level_names))
  names(Sim) <- industry_level_names
  RowMin <- Sim
  h1m <- Sim
  p <- vector(mode='list', length=length(list_of_sim_specifications))
  names(p) <- names(list_of_sim_specifications)
  for(i in 1:length(p)){ 
    p[[i]] <- vector(mode='list', length=length(industry_level_names))
    names(p[[i]]) <- industry_level_names
  }

  for (i in 1:length(list_of_sim_specifications)){
    for (l in 1:length(industry_level_names)){
      
      if (is.null(impedance)){
        Sim[[l]] <- list_of_sim_specifications[[i]][[l]][r_extent, p_extent] 
      }
      else {
        Sim[[l]] <- list_of_sim_specifications[[i]][[l]][r_extent, p_extent] / impedance[[l]][r_extent, p_extent] 
      }
     
      RowMin[[l]] <- cbind(place = rownames(Sim[[l]]), 
                           match = colnames(Sim[[l]])[apply(Sim[[l]], 1, which.min)], 
                           min_value = apply(Sim[[l]], 1, min)
                           ) %>% as.data.frame()
      RowMin[[l]]$min_value <- as.numeric(RowMin[[l]]$min_value)
      RowMin[[l]] <- rbind(RowMin[[l]], 
                           data.frame(place = setdiff(p_extent, r_extent), 
                                      match = setdiff(p_extent, r_extent), 
                                      min_value = rep(1, length(setdiff(p_extent, r_extent))) 
                                      ) 
                           )
      h1m[[l]] <- inner_join(space_vec, RowMin[[l]], by = "place", copy = TRUE)
      h1m[[l]] %<>% mutate(match_name = h1m[[l]]$NAME[match(match, h1m[[l]]$place)])
      my_colors <-  hue_pal(h = c(20, 320))(length(levels(factor(c(p_extent, r_extent))))) %>% 
        cbind ( color = .,  FIPS =  h1m[[l]]$FIPS) %>% as.data.frame()
      
      
   p[[i]][[l]] <- ggplot( h1m[[l]] ) +
        geom_sf_interactive(aes(fill = match,
                                tooltip = glue("County: {NAME}\nFIPS: {place}\nMatch: {match_name}"), 
                                data_id = place), 
                color = NA
                ) +
        coord_sf() +
        theme_void() +
        labs(fill = "Regional Cluster") + 
        scale_fill_manual(labels = (h1m[[l]] %>% filter(place %in% unique(h1m[[l]]$match) ) %>% pull(COUNTY)), 
                          values = (my_colors %>% filter(FIPS %in% unique(h1m[[l]]$match) ) %>% pull(color)) ) + 
     theme(legend.key.size = unit(2, 'mm'))
    }
  }
  assign(deparse(substitute(specname)), p, envir=.GlobalEnv)
} 



#Function to map similarity indices 
mapr <- function(specname,
                 list_of_sim_specifications = Sim_list, 
                 industry_level_names = names(Total_mat),
                 r_extent = rural_extent,
                 p_extent = primary_extent,
                 space_vec = TIGER_RUCC,
                 impedance = NULL,
                 distance = TRUE,
                 spo = FALSE){
  
  Sim <- vector(mode='list', length=length(industry_level_names))
  names(Sim) <- industry_level_names
  RowMin <- Sim
  h1m <- Sim
  p <- vector(mode='list', length=length(list_of_sim_specifications))
  names(p) <- names(list_of_sim_specifications)
  for(i in 1:length(p)){ 
    p[[i]] <- vector(mode='list', length=length(industry_level_names))
    names(p[[i]]) <- industry_level_names
  }

  for (i in 1:length(list_of_sim_specifications)){
    for (l in 1:length(industry_level_names)){
      
      if (is.null(impedance)){
        Sim[[l]] <- list_of_sim_specifications[[i]][[l]][r_extent, p_extent] 
      }
      else if (isTRUE(spo)){
        Sim[[l]] <- (1 / impedance[[l]][r_extent, p_extent])
      }
      else {
        if (isTRUE(distance)){
          Sim[[l]] <- list_of_sim_specifications[[i]][[l]][r_extent, p_extent] / impedance[[l]][r_extent, p_extent]
        }
        else {
          Sim[[l]] <- list_of_sim_specifications[[i]][[l]][r_extent, p_extent] * impedance[[l]][r_extent, p_extent] 
        }
      }

      RowMin[[l]] <- cbind(place = rownames(Sim[[l]]), 
                           match = colnames(Sim[[l]])[apply(Sim[[l]], 1, which.min)], 
                           min_value = apply(Sim[[l]], 1, min)
      ) %>% as.data.frame()
      
      RowMin[[l]]$match[RowMin[[l]]$min_value == 0] <- "NA"
      
      RowMin[[l]]$min_value <- as.numeric(RowMin[[l]]$min_value)
      
      if (isTRUE(distance)){
        RowMin[[l]] %<>% group_by(match) %>% mutate(Nor = min(min_value)/min_value) %>% as.data.frame()
        RowMin[[l]] <- rbind(RowMin[[l]], 
                             as.data.frame(cbind(place = p_extent, 
                                                 match = p_extent, 
                                                 min_value = diag(list_of_sim_specifications[[i]][[l]][p_extent, p_extent]), 
                                                 Nor = rep(c(1), each=length(p_extent)) 
                             )
                             )
        )
      }
      else {
        RowMin[[l]] %<>% group_by(match) %>% mutate(Nor = min_value/min(min_value)) %>% as.data.frame()
        RowMin[[l]] <- rbind(RowMin[[l]], 
                             as.data.frame(cbind(place = p_extent, 
                                                 match = p_extent, 
                                                 min_value = diag(list_of_sim_specifications[[i]][[l]][p_extent, p_extent]), 
                                                 Nor = rep(c(0), each=length(p_extent)) 
                             )
                             )
        )
      }
      

      RowMin[[l]]$Nor <- as.numeric(RowMin[[l]]$Nor)
      
      h1m[[l]] <- inner_join(space_vec, RowMin[[l]], by = "place", copy = TRUE)
      h1m[[l]] %<>% mutate(match_name = h1m[[l]]$NAME[match(match, h1m[[l]]$place)])
      
      p[[i]][[l]] <- ggplot( h1m[[l]] ) +
        geom_sf_interactive(aes(fill = match, 
                                alpha = Nor, 
                                tooltip = glue("County: {NAME}\nFIPS: {place}\nMatch: {match_name}"), 
                                data_id = place
        ), 
        color = NA
        ) +
        guides(alpha = "none") +
        coord_sf() +
        theme_void() +
        labs(fill = "Cluster") +
        scale_fill_brewer(palette = "Set1",
                          labels = (h1m[[l]]  %>% filter(place %in% unique(h1m[[l]]$match) ) %>% pull(COUNTY)) ) 
    }
  }
  assign(deparse(substitute(specname)), p, envir=.GlobalEnv)
} 



#Function to map capacitance
cmapr <- function(specname,
                 list_of_sim_specifications = Queeg_list, 
                 industry_level_names = names(Total_mat),
                 r_extent = rural_extent,
                 p_extent = primary_extent,
                 space_vec = TIGER_RUCC,
                 impedance = NULL,
                 spo = FALSE){
  
  Sim <- vector(mode='list', length=length(industry_level_names))
  names(Sim) <- industry_level_names
  RowMax <- Sim
  h1m <- Sim
  p <- vector(mode='list', length=length(list_of_sim_specifications))
  names(p) <- names(list_of_sim_specifications)
  for(i in 1:length(p)){ 
    p[[i]] <- vector(mode='list', length=length(industry_level_names))
    names(p[[i]]) <- industry_level_names
  }
  
  for (i in 1:length(list_of_sim_specifications)){
    for (l in 1:length(industry_level_names)){
      
      if (is.null(impedance)){
        Sim[[l]] <- list_of_sim_specifications[[i]][[l]][r_extent, p_extent] 
      }
      else if (isTRUE(spo)){
        Sim[[l]] <- ( impedance[[l]][r_extent, p_extent])
      }
      else {
          Sim[[l]] <- list_of_sim_specifications[[i]][[l]][r_extent, p_extent] * impedance[[l]][r_extent, p_extent] 
      }
      
      RowMax[[l]] <- cbind(place = rownames(Sim[[l]]), 
                           match = colnames(Sim[[l]])[apply(Sim[[l]], 1, which.max)], 
                           max_value = apply(Sim[[l]], 1, max)
      ) %>% as.data.frame()
      
      RowMax[[l]]$match[RowMax[[l]]$max_value == 0] <- "NA"
      
      RowMax[[l]]$max_value <- as.numeric(RowMax[[l]]$max_value)
      
        RowMax[[l]] %<>% group_by(match) %>% mutate(Nor = (max_value)/max(max_value)) %>% as.data.frame()
        RowMax[[1]]$Nor[RowMax[[1]]$match == "NA"] <- 1
        RowMax[[l]] <- rbind(RowMax[[l]], 
                             as.data.frame(cbind(place = p_extent, 
                                                 match = p_extent, 
                                                 max_value = diag(list_of_sim_specifications[[i]][[l]][p_extent, p_extent]), 
                                                 Nor = rep(c(1), each=length(p_extent)) 
                             )
                             )
        )
      
    
      RowMax[[l]]$Nor <- as.numeric(RowMax[[l]]$Nor)
      
      h1m[[l]] <- inner_join(space_vec, RowMax[[l]], by = "place", copy = TRUE)
      h1m[[l]] %<>% mutate(match_name = h1m[[l]]$NAME[match(match, h1m[[l]]$place)])
      h1m[[l]]$max_value %<>% as.numeric() %>% round(digits = 3)


      p[[i]][[l]] <- ggplot( h1m[[l]] ) +
        geom_sf_interactive(aes(fill = match, 
                                #alpha = Nor, 
                                alpha = 1, 
                                tooltip = glue("County: {NAME}\nFIPS: {place}\nMatch: {match_name}\nValue: {max_value}"), 
                                data_id = place
        ), 
        color = NA
        ) +
        guides(alpha = "none") +
        coord_sf() +
        theme_void() +
        labs(fill = "Cluster") +
        scale_fill_manual(values = c("#e31a1c",  "#1f78b4",  "#33a02c", "#ff7f00", "#6a3d9a", "#fb9a99",  "#b2df8a", "#fdbf6f",  "#cab2d6","#a6cee3", "#000000" ), labels = (h1m[[l]]  %>% filter(place %in% unique(h1m[[l]]$match) ) %>% pull(COUNTY)) ) 
        #scale_fill_brewer(palette = "Set3", labels = (h1m[[l]]  %>% filter(place %in% unique(h1m[[l]]$match) ) %>% pull(COUNTY)) ) 
    }
  }
  assign(deparse(substitute(specname)), p, envir=.GlobalEnv)
} 


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

# 
# mapr2 <- function(qdf=qdf, singcc=singcc, macc=macc, pmvec=pmvec, ext=ext, a_shade=TRUE){
#   
#   RowMax <- vector(mode='list', length=length(industry_levels))
#   names(RowMax) <- industry_levels
#   h1m  <- RowMax
#   p <- RowMax
#   
#   for (l in 1:length(industry_levels)){  
#     
#     RowMax[[l]] <-  rbind(
#       as.data.frame( cbind(place = pmvec[[l]]$place, 
#             match = pmvec[[l]]$match 
#       ) ),
#       as.data.frame( cbind(place = singcc, 
#             match = singcc 
#       ))
#     )
#     
#     RowMax[[l]] <- RowMax[[l]][order(RowMax[[l]]$place),] 
#     
#     for (i in 1:nrow(RowMax[[l]])){
#       RowMax[[l]]$q_value[i] <- (qdf[[l]][RowMax[[l]]$place[i], RowMax[[l]]$match[i]] * Impede_mat[[3]][[l]][RowMax[[l]]$place[i], RowMax[[l]]$match[i]])
#     }
#     
#     RowMax[[l]]$q_value[RowMax[[l]]$q_value == 0] <- "NA"
#     
#     h1m[[l]] <- inner_join(TIGER_RUCC, RowMax[[l]], by = "place", copy = TRUE)
#     h1m[[l]] %<>% mutate(match_name = h1m[[l]]$NAME[match(match, h1m[[l]]$place)])
#     h1m[[l]]$match[h1m[[l]]$q_value == "NA" & h1m[[l]]$place %in% macc] <- "*Self Match"
#     h1m[[l]]$q_value %<>% as.numeric() %>% round(digits = 4)
#     h1m[[l]]$ind_level <- industry_levels[[l]]
#     h1m[[l]]$lab <- h1m[[l]]$match_name
#     h1m[[l]]$lab[is.na(h1m[[l]]$q_value) & h1m[[l]]$place %in% macc] <- "*Self Match"
#     
#     for (i in 1:length(ext)){
#       h1m[[l]][h1m[[l]]$place == ext[[i]][1],]$geometry <-  TIGER_RUCC %>% filter(TIGER_RUCC$FIPS %in% ext[[i]]) %>% st_union()
#     }
#     
#     h1m[[l]]$a_shade = h1m[[l]]$q_value
#     if(a_shade!=TRUE){
#       h1m[[l]]$a_shade = NA
#     } 
#     
#   }
#   
#   h1m_all  <- rbind(h1m[[1]],h1m[[2]],h1m[[3]])
#   
#   values = c("#e31a1c",  "#1f78b4",  "#33a02c", "#ff7f00", "#6a3d9a", "#ffff99",  "#b15928", "#fb9a99",  "#b2df8a", "#fdbf6f",  "#cab2d6", "#a6cee3",  "#1b9e77",  "#d95f02",  "#7570b3",  "#e7298a", "#66a61e", "#e6ab02", "#8dd3c7", "#ffffb3",  "#bebada", "#fb8072", "#80b1d3")
#   
#   my_pal <- c()
#   for(m in 1:length(setdiff(unique(h1m_all$lab), c("*Self Match")))){
#     my_pal[m] =  values[m]
#   }
#   names(my_pal) <- sort(setdiff(unique(h1m_all$lab), c("*Self Match")))
#   
#   if(isTRUE("*Self Match" %in%  unique(h1m_all$lab))){
#     my_pal <- c(my_pal, "*Self Match" = "#000000")
#   }
#   
#   for (l in 1:length(industry_levels)){  
#     p[[l]] <- ggplot( h1m[[l]] ) +
#       geom_sf_interactive(aes(fill = lab, 
#                               alpha =  a_shade,
#                               tooltip = glue("County: {NAME}\nFIPS: {place}\nMatch: {match_name}\nValue: {q_value}"),
#                               data_id = place
#       ),
#       color = NA
#       ) +
#       guides(alpha = "none") +
#       coord_sf() +
#       theme_void() +
#       labs(fill = "Cluster") +
#       scale_fill_manual(values = my_pal)
#   }
#   
#   gplot = plot_grid(p[[1]] + theme(legend.position = 'none'), 
#                     p[[2]] + theme(legend.position = 'none'), 
#                     p[[3]] + theme(legend.position = 'none'),
#                     labels = c("Sector", "Summary", "Detail"),
#                     nrow = 1,
#                     label_x = 0, label_y = .75,
#                     hjust = -0.5
#   )
#   
#   gleg = get_legend(p[[3]] +
#                       guides(color = guide_legend(nrow = 1)) +
#                       theme(legend.position = "bottom", 
#                             legend.key.size = unit(.2, "cm"))
#   )
#   
#   gall = gplot + draw_grob(gleg, x = 0, y = 0, width = 1, height = .5, scale = .5)
#   
#   g <- girafe(ggobj = gall, 
#               options = list(opts_hover(css = "stroke:gray;r:20pt;"),
#                              opts_tooltip(css = "font-family:sans-serif;background-color:gray;color:white;padding:10px;border-radius:5px;") 
#               )
#   )
#   
#   
# }
# 
# 





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


#Select restricted total output matrix and cluster matched cores
trimr <- function(specname, macc, singcc, ext, industry_levels, o_mat){
  tmat <- vector(mode='list', length=length(industry_levels))
  names(tmat) <- industry_levels
  
  for (l in 1:length(industry_levels)){
    if(!is.na(ext)){
      for(i in 1:length(ext)){
        df <- o_mat[[l]][, ext[[i]]] %>% as.matrix()
        df[, 1:length(ext[[i]])] <- as.matrix(o_mat[[l]][, ext[[i]]]) %*% rep(c(1), each=length(ext[[i]]))
        tmat[[l]] <-  cbind(tmat[[l]], df)
      }
    }
    df <-  o_mat[[l]][, setdiff(singcc, intersect(colnames(tmat[[l]]), singcc)), drop = FALSE]
    tmat[[l]] <-  cbind(tmat[[l]], df)
    
    tmat[[l]] <- tmat[[l]][, singcc] 
    
    df <-  o_mat[[l]][, setdiff(macc, colnames(tmat[[l]])), drop = FALSE]
    tmat[[l]] <-  cbind(tmat[[l]], df)
    
    tmat[[l]] <- tmat[[l]][, colnames(tmat[[l]])[order(colnames(tmat[[l]]))]] %>% as.matrix()
  }
  assign(deparse(substitute(specname)), tmat, envir=.GlobalEnv)
  
}



############ Input Needs
Input_Needs <- function(specname, o_mat, industry_levels){
  tmat <- vector(mode='list', length=length(industry_levels))
  names(tmat) <- industry_levels
  
  for (l in 1:length(industry_levels)){
    tmat[[l]] <- (Direct_mat[[l]]  %*%  o_mat[[l]])
  }
  assign(deparse(substitute(specname)), tmat, envir=.GlobalEnv)
}
############ Import Input Needs
Import_Needs <- function(specname, o_mat, i_mat, industry_levels){
  tmat <- vector(mode='list', length=length(industry_levels))
  names(tmat) <- industry_levels
  
  for (l in 1:length(industry_levels)){
    tmat[[l]] <- pmax(i_mat[[l]] - o_mat[[l]], 0)
  }
  assign(deparse(substitute(specname)), tmat, envir=.GlobalEnv)
}
############ Net Exports
Export_Needs <- function(specname, o_mat, i_mat, industry_levels){
  tmat <- vector(mode='list', length=length(industry_levels))
  names(tmat) <- industry_levels
  
  for (l in 1:length(industry_levels)){
    tmat[[l]] <- pmax(o_mat[[l]] -  i_mat[[l]], 0)
  }
  assign(deparse(substitute(specname)), tmat, envir=.GlobalEnv)
}
############ Relative Queeg specification
Rel_Queeg <- function(specname, o_mat, im_mat, ex_mat, industry_levels){
  tmat <- vector(mode='list', length=length(industry_levels))
  names(tmat) <- industry_levels
  for (l in 1:length(industry_levels)){
    tmat[[l]] <-  matrix(0, nrow = ncol(o_mat[[l]]), 
                         ncol = ncol(o_mat[[l]]) )
    rownames(tmat[[l]]) = colnames(tmat[[l]]) <- colnames(o_mat[[l]])
  }
  
  for (l in 1:length(industry_levels)){
    for (i in 1:ncol(o_mat[[l]])){
      for (j in 1:ncol(o_mat[[l]])){
        tmat[[l]][i,j] <- (rep(c(1), each=ncol(Direct_mat[[l]])) %*% 
                             pmin(ex_mat[[l]][,i], im_mat[[l]][,j])) / 
          (rep(c(1), each=ncol(Direct_mat[[l]])) %*% 
             ex_mat[[l]][,i])
      }
    }
  }
  assign(deparse(substitute(specname)), tmat, envir=.GlobalEnv)
}


Queeg_selectr <- function(specname, CoreMatch, MatchMat, cuml_core_out, perf_core_out, nomatch_core_out, th_iso, queeg_mat, imp_mat, macc, singcc, industry_levels, out_mat, isolation_th){      
  tmat <- vector(mode='list', length=length(industry_levels))
  names(tmat) <- industry_levels
  
  t2mat <- tmat
  t3mat <- tmat
  t4mat <- tmat
  t5mat <- tmat 
  t6mat <- tmat
  t7mat <- tmat
  
  for (l in 1:length(industry_levels)){
    #all non-zero row-total export counties
    t2mat[[l]] <- (queeg_mat[[l]][macc, singcc] * imp_mat[[l]][macc,  singcc]) %>% 
      as.data.frame() %>% filter(rowSums(.) != 0) %>% as.matrix() %>% .[apply(., 1, function(x){!all(x<isolation_th)}),]
    
    t3mat[[l]] <- sparseMatrix(i = match(rownames(t2mat[[l]]), rownames(t2mat[[l]])),
                               j = match(c(colnames(t2mat[[l]])[apply(t2mat[[l]], 1, which.max)]), colnames(t2mat[[l]])),
                               x = 1L,
                               dims = c(nrow(t2mat[[l]]), ncol(t2mat[[l]])),
                               dimnames = list(rownames(t2mat[[l]]), colnames(t2mat[[l]]))
    ) %>% as.matrix()
    
    #cumulative output of matched cores and non-cores 
    t4mat[[l]] <- (out_mat[[l]][, rownames(t3mat[[l]])[order(rownames(t3mat[[l]]))]] %*% 
                             t3mat[[l]][order(rownames(t3mat[[l]])), order(colnames(t3mat[[l]]))]) %>% 
      .[, sort(unique(colnames(t2mat[[l]])[apply(t2mat[[l]], 1, which.max)]))] + out_mat[[l]][, sort(unique(colnames(t2mat[[l]])[apply(t2mat[[l]], 1, which.max)]))]
    
    #output of unmatched cores
    t5mat[[l]] <- as.data.frame(queeg_mat[[l]][macc, singcc] * imp_mat[[l]][macc,  singcc]) %>% .[apply(., 1, function(x){all(x<isolation_th)}),]  %>% rownames() %>% setdiff(., colnames(t4mat[[l]])) %>% out_mat[[l]][, .]
    
    #output of unmatched cores
    t6mat[[l]] <- union(colnames(t4mat[[l]]), colnames(t5mat[[l]])) %>% setdiff(colnames(queeg_mat[[l]][, singcc]), .) %>% out_mat[[l]][,.]  %>% as.data.frame()
      
    t7mat[[l]] <- as.data.frame(queeg_mat[[l]][macc, singcc] * imp_mat[[l]][macc,  singcc]) %>% .[apply(., 1, function(x){all(x<isolation_th)}),]  %>% rownames() %>% out_mat[[l]][,.]  %>% as.data.frame()
    
    tmat[[l]] <- cbind(t4mat[[l]], t5mat[[l]], t6mat[[l]]) %>% .[, sort(c(colnames(t4mat[[l]]), 
                                                                                               colnames(t5mat[[l]]), 
                                                                                               colnames(t6mat[[l]])
    ))]
    
  }
  
  assign(deparse(substitute(specname)), tmat, envir=.GlobalEnv)  
  assign(deparse(substitute(CoreMatch)), t2mat, envir=.GlobalEnv)
  assign(deparse(substitute(MatchMat)), t3mat, envir=.GlobalEnv) 
  assign(deparse(substitute(cuml_core_out)), t4mat, envir=.GlobalEnv) 
  assign(deparse(substitute(perf_core_out)), t5mat, envir=.GlobalEnv) 
  assign(deparse(substitute(nomatch_core_out)), t6mat, envir=.GlobalEnv) 
  assign(deparse(substitute(th_iso)), t7mat, envir=.GlobalEnv) 
  
}

####Geographic and Economic hierarchy clustering
hierarchr <- function(specname, mat_mat, spec_clust, qdf, ext, isolation_th, imp_mat){  
  
  RowMax <- vector(mode='list', length=length(industry_levels))
  names(RowMax) <- industry_levels
  
  tmat <- vector(mode='list', length=length(industry_levels))
  names(tmat) <- industry_levels
  #tmat  <- RowMax
  
  for (l in 3){ 
    #for (l in 1:length(industry_levels)){  
    
    if(!is.null(spec_clust)){
    RowMax[[l]] <-  rbind(
      cbind(place = rownames(mat_mat[[l]]), 
            match = colnames(mat_mat[[l]])[apply(mat_mat[[l]], 1, which.max)] 
      ) ,
      cbind(place = colnames(spec_clust[[l]]), 
            match = colnames(spec_clust[[l]]) 
      ) %>% as.data.frame()
    )
    } else {
      RowMax[[l]] <-  rbind(
        cbind(place = rownames(mat_mat[[l]]), 
              match = colnames(mat_mat[[l]])[apply(mat_mat[[l]], 1, which.max)] 
        ) ,
        cbind(place = spec_clust, 
              match = spec_clust 
        ) %>% as.data.frame()
      ) 
  }
    RowMax[[l]] <- RowMax[[l]][order(RowMax[[l]]$place),] 
    
    for (i in 1:nrow(RowMax[[l]])){
      RowMax[[l]]$q_value[i] <- (qdf[[l]][RowMax[[l]]$place[i], RowMax[[l]]$match[i]] * imp_mat[[l]][RowMax[[l]]$place[i], RowMax[[l]]$match[i]])
    }
    
    RowMax[[l]]$q_value[RowMax[[l]]$q_value == 0] <- "NA"
    
    tmat[[l]] <- inner_join(TIGER_RUCC, RowMax[[l]], by = "place", copy = TRUE)
    
    if(!is.na(ext)){
        for (i in 1:length(ext)){
          tmat[[l]][tmat[[l]]$place == ext[[i]][1],]$geometry <- TIGER_RUCC %>% filter(TIGER_RUCC$FIPS %in% ext[[i]]) %>% st_union()
        }
    }
    
    tmat[[l]] %<>% mutate(match_name = tmat[[l]]$NAME[match(match, tmat[[l]]$place)])
    tmat[[l]]$match_name[tmat[[l]]$q_value == "NA" ] <- "Isolated"
    
    tmat[[l]]$match_name[tmat[[l]]$NAME %in% unique(tmat[[l]]$match_name) & tmat[[l]]$match_name == "Isolated"] <- "ECA Isolated"
    
    tmat[[l]] <- tmat[[l]] %>% group_by(match) %>% mutate(m_count = n())
       #tmat[[l]]$match_name[tmat[[l]]$place %in% tmat[[l]]$place[tmat[[l]]$m_count == 1] & tmat[[l]]$place %in% tmat[[l]]$match ] <- tmat[[l]]$NAME[tmat[[l]]$place %in% tmat[[l]]$place[tmat[[l]]$m_count == 1] & tmat[[l]]$place %in% tmat[[l]]$match]
    
    #tmat[[l]]$match_name[tmat[[l]]$place %in% tmat[[l]]$place[tmat[[l]]$match_name == "Isolated"] & tmat[[l]]$place %in% tmat[[l]]$match ] <- tmat[[l]]$NAME[tmat[[l]]$place %in% tmat[[l]]$place[tmat[[l]]$match_name == "Isolated"] & tmat[[l]]$place %in% tmat[[l]]$match]
  
    
    #tmat[[l]] <- tmat[[l]] %>% group_by(match_name) %>% mutate(m_count = n())
    #tmat[[l]]$q_value[tmat[[l]]$place %in% tmat[[l]]$place[tmat[[l]]$m_count == 1]] <- "NA"
    tmat[[l]]$q_value[tmat[[l]]$q_value != "NA"] %<>% as.numeric() %>% round(digits = 4)
    tmat[[l]]$ind_level <- industry_levels[[l]]
    tmat[[l]]$lab <- tmat[[l]]$match_name
    #tmat[[l]]$match_name[tmat[[l]]$place %in% unique(tmat[[l]]$match)] <- tmat[[l]]$NAME[tmat[[l]]$place %in% unique(tmat[[l]]$match)]
    #tmat[[l]]$lab[tmat[[l]]$place %in% unique(tmat[[l]]$match)] <- tmat[[l]]$NAME[tmat[[l]]$place %in% unique(tmat[[l]]$match)]
    #tmat[[l]]$q_value[tmat[[l]]$place %in% unique(tmat[[l]]$match)] <- "NA"
      # tmat[[l]]$match_name[tmat[[l]]$q_value < isolation_th ] <- "Isolated"
      # tmat[[l]]$lab[tmat[[l]]$q_value < isolation_th] <- "Isolated"
      
    tmat[[l]]$a_value <- tmat[[l]]$q_value
    # tmat[[l]]$a_value[tmat[[l]]$lab == "Isolated"] <- "NA"
    # 
    # tmat[[l]]$match_name[tmat[[l]]$place %in% tmat[[l]]$place[tmat[[l]]$m_count == 1]] <- "Isolated"
    # tmat[[l]]$lab[tmat[[l]]$place %in% tmat[[l]]$place[tmat[[l]]$m_count == 1]] <- "Isolated"

    tmat[[l]]$eca <- tmat[[l]]$lab
    tmat[[l]]$eca[tmat[[l]]$NAME %in% unique(tmat[[l]]$lab)] <- tmat[[l]]$NAME[tmat[[l]]$NAME %in% unique(tmat[[l]]$lab)]
    if(isTRUE(tmat[[l]]$NAME %in% unique(tmat[[l]]$match_name) & tmat[[l]]$match_name == "Isolated")){
       tmat[[l]]$eca[tmat[[l]]$lab == "ECA Isolated"] <- tmat[[l]]$NAME[tmat[[l]]$lab == "ECA Isolated"]
    }
  }
  
  assign(deparse(substitute(specname)), tmat, envir=.GlobalEnv)
} 

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
               subtitle = glue("Net Input Excess: {county}"),
               color = "Net Import Shortage:") +
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
  
  for (l in 1:length(industry_levels)){  
    df[[l]] <- ggplot( hm[[l]] ) +
      { if(isFALSE(shade))
        geom_sf_interactive(aes(fill = lab, 
                                tooltip = glue("County: {NAME}\nFIPS: {place}\nMatch: {match_name}\nValue: {q_value}"), 
                                data_id = place
        ), 
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
                        ), pattern = 'crosshatch',
                        pattern_density = 0.01, 
                        pattern_spacing = 0.03,
                        pattern_fill    = 'black',
                        pattern_colour  = 'black'
        )}+ 
      guides(alpha = "none") +
      coord_sf() +
      theme_void() +
      labs(fill = "Cluster",
           caption = paste0(isolation_th*100,"% Isolation Threshold")) +
      scale_fill_manual(values = my_pal)
  }
  assign(deparse(substitute(specname)), df, envir=.GlobalEnv)     
} 



####Mapping Economic and Spatial cluster matching with isolation
maprECA <- function(specname, shade, hatch, hm, my_pal){
  df <- vector(mode='list', length=length(industry_levels))
  names(df) <- industry_levels
  
  for (l in 1:length(industry_levels)){  
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
                        pattern_density = 0.01, 
                        pattern_spacing = 0.03,
                        pattern_fill    = 'black',
                        pattern_colour  = 'black'
        )}+ 
      guides(alpha = "none") +
      coord_sf() +
      theme_void() +
      labs(fill = "Cluster",
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





