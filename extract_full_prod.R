library(tools)
library(zip)

drops_path <- "drops_data"
if (!dir.exists(drops_path)) {
  dir.create(drops_path)
}

zip_path <- 'prod_data'
zip_files <- list.files(path = zip_path, pattern = "*.zip", full.names = TRUE)

for (zip_file in zip_files) {
  print(paste("zip : [", zip_file, "]"))
  
  if (!file.exists(zip_file)) {
    print(paste('Not an archive : ', zip_file))
    quit()
  }
  
  files_in_zip <- utils::unzip(zip_file, list = TRUE)$Name
  file_name <- files_in_zip[1]
  
  # Extract the archive name and create a new folder within drops_path
  archive_name <- tools::file_path_sans_ext(basename(zip_file))
  archive_folder <- file.path(drops_path, archive_name)
  if (!dir.exists(archive_folder)) {
    dir.create(archive_folder)
  }
  
  drop_file <- file.path(archive_folder, file_name)
  
  if (!file.exists(drop_file)) {
    print(paste("Extracting [", drop_file, "]"))
    utils::unzip(zip_file, exdir = archive_folder)
  }
}
