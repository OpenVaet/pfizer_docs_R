library(tools)
library(zip)

xpt_path <- "xpt_data"
if (!dir.exists(xpt_path)) {
  dir.create(xpt_path)
}

zip_path <- 'zip_data'
zip_files <- list.files(path = zip_path, pattern = "*.zip", full.names = TRUE)

for (zip_file in zip_files) {
  print(paste("zip : [", zip_file, "]"))
  
  if (!file.exists(zip_file)) {
    print(paste('Not an archive : ', zip_file))
    quit()
  }
  
  files_in_zip <- utils::unzip(zip_file, list = TRUE)$Name
  file_name <- files_in_zip[1]
  xpt_file <- file.path(xpt_path, file_name)
  
  if (!file.exists(xpt_file)) {
    print(paste("Extracting [", xpt_file, "]"))
    utils::unzip(zip_file, exdir = xpt_path)
  }
}
