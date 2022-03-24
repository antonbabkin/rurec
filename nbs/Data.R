
# List packages needed for this exercise
packages <- c("dlm",
              "gtools",
              "knitr",
              "tidyr")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages], dependencies = TRUE)
}

# Load packages as necessary
invisible(lapply(packages, library, character.only = TRUE))

# Establish working directory relative to location of this file
script_path() %>% setwd() 

# Create "data" a file if not already done
if (!file.exists("data")) {
  "data" %>% dir.create()
}

# Function to download and unzip data
data_getr <- function(ZipURL,
                      DestDir,
                      FileExt = ""){
  local({
    DestFile <- ZipURL %>% basename() %>% file_path_sans_ext() %>% path(DestDir, ., ext = FileExt)
    temp <- tempfile()
    if (!file.exists(DestFile)) {
      download.file(url=ZipURL, destfile=temp, quiet=TRUE)
      if (file.info(temp)$size > 0){
        unzip(zipfile = temp, exdir = DestDir, overwrite = FALSE)
      }
    }
  })
}


# Download and unzip BEA IO data.

data_getr(
  ZipURL = "https://apps.bea.gov/industry/iTables%20Static%20Files/AllTablesSUP.zip",
  DestDir = getwd() %>% path("data")
)

# Download and unzip Census CBP data.
data_getr(
  ZipURL = "https://www2.census.gov/programs-surveys/cbp/datasets/2019/cbp19co.zip",
  DestDir =  getwd() %>% path("data"),
  FileExt = "txt"
)

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


# Import IO tables into R 
excel_importr(
  TableName = "IO_tables",
  FileDir = path(getwd(), "data", "AllTablesSUP") 
)


# Import CBP data into R 
if (!exists("RegionalData")){
  RegionalData <- path(getwd(), "data", "cbp19co.txt") %>% read.csv(header = TRUE)
  
  RegionalData$fipstate %<>% formatC(width = 2, format = "d", flag = "0")
  RegionalData$fipscty  %<>% formatC(width = 3, format = "d", flag = "0")
}


# Process and parse hierarchical structure  of CBP data
RegionalData_C <- filter(RegionalData, naics == "------")
Regions <- RegionalData_C[, 1:2]
Regions$place <- paste0(Regions$fipstate, Regions$fipscty)
RegionalData_Sector <- RegionalData %>% filter(grepl('*----', naics) & naics != '------' )
RegionalData_Subsector <- RegionalData %>% filter(grepl('///', naics))
RegionalData_IndustryGroup <- RegionalData %>% filter(grepl('//', naics) & !grepl('///', naics))
RegionalData_NAICSIndustry <- RegionalData %>% filter(grepl('/', naics) & !grepl('///', naics)  & !grepl('//', naics))
RegionalData_USNAICS <- RegionalData %>% filter(!grepl('/', naics) & !grepl('-', naics))


# Generate a cross walk to transform NAICS sectors used by CBP into BEA sectors 
CBP_table <- RegionalData_Sector
CBP_table$place <- paste0(CBP_table$fipstate, CBP_table$fipscty)
CBP_table %<>% subset(select = c(place, naics, emp, qp1, ap, est))
CBP_table$naics %<>% substr(0,2) %>% as.numeric()

CBP_table %<>% reshape(idvar = "naics", timevar = "place", direction = "wide")
CBP_table[is.na(CBP_table)] <- 0
CBP_table %<>%  as.data.frame() %>% arrange(naics)

CBP_table %<>% t() %>%  as.data.frame()
CBP_table %<>% mutate("11" = V1)
CBP_table %<>% mutate("21" = V2)
CBP_table %<>% mutate("22" = V3)
CBP_table %<>% mutate("23" = V4)
CBP_table %<>% mutate("31G" = V5)
CBP_table %<>% mutate("42" = V6)
CBP_table %<>% mutate("44RT" = V7)
CBP_table %<>% mutate("48WT" = V8)
CBP_table %<>% mutate("51" = V9)
CBP_table %<>% mutate("FIRE" = V10 + V11)
CBP_table %<>% mutate("PROF" = V12 + V13 + V14)
CBP_table %<>% mutate("6" = V15 + V16)
CBP_table %<>% mutate("7" = V17 + V18)
CBP_table %<>% mutate("81" = V19)
CBP_table %<>% mutate("G" = V20)
CBP_table %<>% subset(select = -c(V1:V20)) %>% slice(-c(1)) %>% t()
BEA_Sectors <- row.names(CBP_table)
CBP_table <- cbind(BEA_Sectors, CBP_table) %>% as.data.frame()
CBP_table %<>% reshape(idvar = "BEA_Sectors", varying = c(colnames(CBP_table)[-1]), direction = "long")
rownames(CBP_table) <- 1:nrow(CBP_table)
names(CBP_table)[names(CBP_table)=="time"] <- "place"
CBP_table$place  %<>% formatC(width = 5, format = "d", flag = "0")
CBP_table$emp <-  as.numeric(CBP_table$emp)
CBP_table$qp1 <-  as.numeric(CBP_table$qp1)
CBP_table$ap <-  as.numeric(CBP_table$ap)
CBP_table$est <-  as.numeric(CBP_table$est)





