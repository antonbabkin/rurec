
# A little script to practice and debug parquet stuff

set.seed(1234)
nrows <- 100000
random_data <- data.frame(
  x = round(runif(nrows)*100000) %>% {sprintf("%05d", .)} %>% substr(1,5),
  y = round(runif(nrows)*100000) %>% {sprintf("%05d", .)} %>% substr(1,5)) %>% 
  {.[order(.$y), ]}   

for (i in 0:9){
    assign(paste0("rd_", i), random_data[substr(random_data$y, 1, 2) %in% paste0(i, 0):paste0(i, 9), ])
}

for (i in seq(9, 99, 10)){
  assign(paste0("rd_", substr(sprintf("%02d", i), 1, 1)), random_data[substr(random_data$y, 1, 2) %in% sprintf("%02d", (i-9):i), ])
}

rm(list=grep("^rd", ls(), value = T))

unlink(dataset_path, recursive = T)
dataset_path <- file.path(tempdir(), "random_data")


random_data %>% 
  group_by(substr(y, 1, 2)) %>% 
  write_dataset(dataset_path, hive_style = F)

system.time(
  for (i in 0:9){
    get(paste0("rd_", i)) %>% 
      group_by(substr(y, 1, 2)) %>% 
      write_dataset(dataset_path, hive_style = F, existing_data_behavior = "delete_matching")
  }
)

list.files(dataset_path, recursive = TRUE)

open_dataset(dataset_path) %>% collect() %>% str()

fs::dir_tree(dataset_path)




