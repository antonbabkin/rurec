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








mapr <- function(list_of_sim_specifications = Sim_list, 
                  sim_specification_names = sim_levels, 
                  industry_level_names = names(Total_mat),
                  rural_extent = WItest_rural,
                  primary_extent = WItest_primary,
                  space_vec = TIGER_RUCC){
  
  g <<- vector(mode='list', length=length(list_of_sim_specifications))
  names(g) <- names(list_of_sim_specifications)
  title <- g
  Sim <- vector(mode='list', length=length(industry_level_names))
  names(Sim) <- industry_level_names
  RowMin <- Sim
  h1m <- Sim
  p <<- vector(mode='list', length=length(industry_level_names))
  subtitle <- Sim
  
  for (i in 1:length(list_of_sim_specifications)){
    for (l in 1:length(industry_level_names)){
      
      Sim[[l]] <- list_of_sim_specifications[[i]][[l]][rural_extent, primary_extent]
      
      RowMin[[l]] <- cbind(place = rownames(Sim[[l]]), 
                           match = colnames(Sim[[l]])[apply(Sim[[l]], 1, which.min)], 
                           min_value = apply(Sim[[l]], 1, min)
      ) %>% as.data.frame()
      RowMin[[l]]$min_value <- as.numeric(RowMin[[l]]$min_value)
      RowMin[[l]] %<>% group_by(match) %>% mutate(Nor = min_value/max(min_value)) %>% as.data.frame()
      RowMin[[l]] <- rbind(RowMin[[l]], 
                           as.data.frame(cbind(place = primary_extent, 
                                               match = primary_extent, 
                                               min_value = diag(list_of_sim_specifications[[i]][[l]][primary_extent, primary_extent]), 
                                               Nor = rep(c(1), each=length(primary_extent)) 
                           )
                           )
      )
      RowMin[[l]]$Nor <- as.numeric(RowMin[[l]]$Nor)
      
      h1m[[l]] <- inner_join(space_vec, RowMin[[l]], by = "place", copy = TRUE)
      h1m[[l]] %<>% mutate(match_name = h1m[[l]]$NAME[match(match, h1m[[l]]$place)])
      
      p[[l]] <<- ggplot( h1m[[l]] ) +
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
        labs(fill = "Regional Cluster") +
        scale_fill_brewer(palette = "Set1",
                          labels = (h1m[[l]]  %>% filter(place %in% unique(h1m[[l]]$match) ) %>% pull(County_Name)) ) 
    }
    
    title[[i]] <- ggdraw() + 
      draw_label(available_indicators_lab[i], fontface = 'bold', x = 0, hjust = 0) +
      theme(plot.margin = margin(0, 0, 0, 7))
    
    g[[i]] <<- girafe(ggobj = plot_grid(p[[1]], 
                                         p[[2]], 
                                         p[[3]], 
                                         labels = c("Sector", "Summary", "Detail")
    ),
    options = list(opts_hover(css = "stroke:gray;r:20pt;"),
                   opts_hover_inv(css = "opacity:0.9;"),
                   opts_tooltip(css = "font-family:sans-serif;background-color:gray;color:white;padding:10px;border-radius:5px;") 
    )
    )
  }
  
} 





smapr <- function(list_of_sim_specifications = Sim_list, 
                  sim_specification_names = sim_levels, 
                  industry_level_names = names(Total_mat),
                  rural_extent = WItest_rural,
                  primary_extent = WItest_primary,
                  impedance = Q_mat,
                  space_vec = TIGER_RUCC){
  
  sg <<- vector(mode='list', length=length(list_of_sim_specifications))
  names(sg) <- names(list_of_sim_specifications)
  title <- sg
  Sim <- vector(mode='list', length=length(industry_level_names))
  names(Sim) <- industry_level_names
  RowMin <- Sim
  h1m <- Sim
  p <<- vector(mode='list', length=length(industry_level_names))
  subtitle <- Sim
  
  for (i in 1:length(list_of_sim_specifications)){
    for (l in 1:length(industry_level_names)){
      
      Sim[[l]] <- list_of_sim_specifications[[i]][[l]][rural_extent, primary_extent] / impedance[[l]][rural_extent, primary_extent] 
      
      RowMin[[l]] <- cbind(place = rownames(Sim[[l]]), 
                           match = colnames(Sim[[l]])[apply(Sim[[l]], 1, which.min)], 
                           min_value = apply(Sim[[l]], 1, min)
      ) %>% as.data.frame()
      RowMin[[l]]$min_value <- as.numeric(RowMin[[l]]$min_value)
      RowMin[[l]] %<>% group_by(match) %>% mutate(Nor = min_value/max(min_value)) %>% as.data.frame()
      RowMin[[l]] <- rbind(RowMin[[l]], 
                           as.data.frame(cbind(place = primary_extent, 
                                               match = primary_extent, 
                                               min_value = diag(list_of_sim_specifications[[i]][[l]][primary_extent, primary_extent]), 
                                               Nor = rep(c(1), each=length(primary_extent)) 
                           )
                           )
      )
      RowMin[[l]]$Nor <- as.numeric(RowMin[[l]]$Nor)
      
      h1m[[l]] <- inner_join(space_vec, RowMin[[l]], by = "place", copy = TRUE)
      h1m[[l]] %<>% mutate(match_name = h1m[[l]]$NAME[match(match, h1m[[l]]$place)])
      
      p[[l]] <<- ggplot( h1m[[l]] ) +
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
        labs(fill = "Regional Cluster") +
        scale_fill_brewer(palette = "Set1",
                          labels = (h1m[[l]]  %>% filter(place %in% unique(h1m[[l]]$match) ) %>% pull(County_Name)) ) 
    }
    
    title[[i]] <- ggdraw() + 
      draw_label(available_indicators_lab[i], fontface = 'bold', x = 0, hjust = 0) +
      theme(plot.margin = margin(0, 0, 0, 7))
    
    sg[[i]] <<- girafe(ggobj = plot_grid(p[[1]], 
                                         p[[2]], 
                                         p[[3]], 
                                         labels = c("Sector", "Summary", "Detail")
    ),
    options = list(opts_hover(css = "stroke:gray;r:20pt;"),
                   opts_hover_inv(css = "opacity:0.9;"),
                   opts_tooltip(css = "font-family:sans-serif;background-color:gray;color:white;padding:10px;border-radius:5px;") 
    )
    )
  }
  
} 




reshaper <- function(industry_data_frame){ 
  ucname <- industry_data_frame[[2]] %>% unique()
  x <- reshape(industry_data_frame, idvar = names(industry_data_frame[2]), timevar = "place", direction = "wide") %>% suppressWarnings()
  x <- x[order(x[[1]]), ]
  rownames(x) <- 1:nrow(x)
  x[is.na(x)] <- 0
  x <- x[,-1] %>% t() %>%  as.data.frame()
  colnames(x) <- ucname %>% sort()
  x <- x %>% t()
  
  x <- cbind(indcode = row.names(x), x) %>% as.data.frame()
  x <- x %>% reshape(idvar = names(industry_data_frame[1]), varying = c(colnames(x)[-1]), direction = "long")
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





# Display end time
log_info("Define functions end")


