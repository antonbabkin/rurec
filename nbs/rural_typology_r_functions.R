# Function repository 

# Load and attach necessary packages
library(rprojroot)
library(fs)
library(readxl)
library(openxlsx)

# #Function to save r functions
# fun_saver <- function (funct){
#   
#   # Create r function folder if not already done
#   fun_dir = file.path(find_rstudio_root_file(), "data", "rfuns")
#   if (!file.exists(fun_dir)) {
#     dir.create(fun_dir)
#   }
#   
#   if (!file.exists(file.path(fun_dir, as.character(substitute(funct))))){
#     saveRDS(funct, file = file.path(fun_dir, as.character(substitute(funct))))
#   }
# }


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
saver <- function (dataname){
  
  data_dir = file.path(find_rstudio_root_file(), "data", "robjs")
  if (!file.exists(data_dir)) {
    dir.create(data_dir)
  }
  
  if (!file.exists(file.path(data_dir, as.character(substitute(dataname))))){
    saveRDS(dataname, file = file.path(data_dir, as.character(substitute(dataname))))
  }

}


#Function to import robj data
importr <- function(x){
  require(rprojroot)
  assign(deparse(substitute(x)), readRDS(file.path(find_rstudio_root_file(), "data", "robjs", as.character(substitute(x)) )), envir=.GlobalEnv)
}



