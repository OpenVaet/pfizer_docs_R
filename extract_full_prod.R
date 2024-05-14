library(tools)
library(zip)

drops_path <- "drops_data"
if (!dir.exists(drops_path)) {
  dir.create(drops_path)
}
xpt_path <- "xpt_data"
if (!dir.exists(xpt_path)) {
  dir.create(xpt_path)
}
pdf_path <- "pdf_data"
if (!dir.exists(pdf_path)) {
  dir.create(pdf_path)
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
  
  # Extracts the archive name and creates a new folder within drops_path
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

# Gets all sub-folders in drops_path
sub_folders <- dir(drops_path, recursive = FALSE)

for (sub_folder in sub_folders) {
  # Gets all files in the sub-folder
  files <- dir(file.path(drops_path, sub_folder), full.names = TRUE)
  
  # Iterates through each file
  for (file in files) {
    # Checks if the file has an extension in ".xpt"
    if (tools::file_ext(file) == "xpt") {
      # Gets the destination file path
      dest_file <- file.path(xpt_path, basename(file))
      
      # Checks if the file doesn't exist in xpt_path
      if (!file.exists(dest_file)) {
        # Copies the file to xpt_path
        file.copy(file, dest_file)
      }
    }
    # Checks if the file has an extension in ".pdf"
    if (tools::file_ext(file) == "pdf") {
      # Gets the destination file path
      dest_file <- file.path(pdf_path, basename(file))
      
      # Checks if the file doesn't exist in pdf_path
      if (!file.exists(dest_file)) {
        # Copies the file to pdf_path
        file.copy(file, dest_file)
      }
    }
  }
}
