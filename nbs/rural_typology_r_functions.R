# Function repository 

# Load and attach necessary packages
library(rprojroot)
library(fs)
library(readxl)
library(openxlsx)
library(rlog)

# Display start time
log_info("Define functions start")

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








mapr <- function(x){



g <- vector(mode='list', length=length(Sim_list))
names(g) <- sim_levels
title <- g

for (i in 1:length(Sim_list)){
  
  WISim <- vector(mode='list', length=length(Total_mat))
  names(WISim) <- industry_levels
  RowMin <- WISim
  TIGER_RUCC_h1m <- WISim
  p <- WISim
  subtitle <- WISim
  
  
  for (l in 1:length(WISim)){
    WISim[[l]] <- Sim_list[[i]][[l]][WItest_rural, WItest_primary]
    
    
    
    RowMin[[l]] <- cbind(place = rownames(WISim[[l]]), match = colnames(WISim[[l]])[apply(WISim[[l]], 1, which.min)], min_value = apply(WISim[[l]], 1, min)) %>% as.data.frame()
    RowMin[[l]]$min_value <- as.numeric(RowMin[[l]]$min_value)
    RowMin[[l]] %<>% group_by(match) %>% mutate(Nor = min_value/max(min_value)) %>% as.data.frame()
    RowMin[[l]] <- rbind(  RowMin[[l]], as.data.frame(cbind(place = WItest_primary, match = WItest_primary, min_value = diag(Sim_list[[i]][[l]][WItest_primary, WItest_primary]), Nor = rep(c(1), each=length(WItest_primary)) )))
    RowMin[[l]]$Nor <- as.numeric(RowMin[[l]]$Nor)
    
    
    TIGER_RUCC_h1m[[l]] <- inner_join(TIGER_RUCC, RowMin[[l]], by = "place", copy = TRUE)
    TIGER_RUCC_h1m[[l]] %<>% mutate(match_name = TIGER_RUCC_h1m[[l]]$NAME[match(match, TIGER_RUCC_h1m[[l]]$place)])
    
    
    p[[l]] <- ggplot( TIGER_RUCC_h1m[[l]] ) +
      geom_sf_interactive(aes(fill = match, alpha = Nor, tooltip = glue("County: {NAME}\nFIPS: {place}\nMatch: {match_name}"), data_id = place), color = NA) +
      guides(alpha = "none") +
      coord_sf() +
      theme_void() +
      labs(fill = "Regional Cluster") +
      scale_fill_brewer(palette = "Set1",
                        labels = (TIGER_RUCC_h1m[[l]]  %>% filter(place %in% unique(TIGER_RUCC_h1m[[l]]$match) ) %>% pull(County_Name)) ) 
    
    
  }
  
  title[[i]] <- ggdraw() + 
    draw_label(available_indicators_lab[i], fontface = 'bold', x = 0, hjust = 0) +
    theme(plot.margin = margin(0, 0, 0, 7))
  
  
  g[[i]] <- girafe(ggobj = plot_grid(p[[1]], p[[2]], p[[3]], 
                                     labels = c("Sector", "Summary", "Detail")),
                   options = list(
                     opts_hover(css = "stroke:gray;r:20pt;"),
                     opts_hover_inv(css = "opacity:0.9;"),
                     opts_tooltip(css = "font-family:sans-serif;background-color:gray;color:white;padding:10px;border-radius:5px;") ))
  
}

}



# Display end time
log_info("Define functions end")


