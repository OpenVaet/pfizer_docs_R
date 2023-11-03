library(readr)
library(dplyr)

# Read the data
random_data <- read.csv('F6_M6_ADSL_Randomization_subjects.csv', sep = ';', stringsAsFactors = FALSE)

# Make column names unique
names(random_data) <- make.names(names(random_data), unique = TRUE)
print(random_data)

# Initialize the sites data
sites_data <- list()

# Process the random data
for(i in 1:nrow(random_data)){
  trial_site_id <- as.character(random_data[i, "Trial.Site.ID"])
  sites_data[[trial_site_id]] <- list(
    fa_random_20201114 = as.numeric(random_data[i, "FA.Random"]),
    m6_random_20210313 = as.numeric(random_data[i, "M6.Random"]),
    fa_investig_20201119 = as.numeric(random_data[i, "FA.Investig"]),
    m6_investig_20210319 = as.numeric(random_data[i, "M6.Investig"]),
    subjects_came_in = 0,
    subjects_left = 0,
    adsl_20201114 = 0,
    adsl_20201119 = 0,
    adsl_20210313 = 0,
    adsl_20210319 = 0
  )
}

# Read and process the subjects changed sites data
subjects_changed_sites_data <- read.csv('subjects_changed_sites.csv', sep = ',', stringsAsFactors = FALSE)
for(i in 1:nrow(subjects_changed_sites_data)){
  trial_site_id <- as.character(subjects_changed_sites_data[i, "Trial_Site_Id"])
  sites_data[[trial_site_id]]$subjects_came_in <- as.numeric(subjects_changed_sites_data[i, "Subjects_Came_In"])
  sites_data[[trial_site_id]]$subjects_left <- as.numeric(subjects_changed_sites_data[i, "Subjects_Left"])
}

# Read and process the adsl data
adsl_data <- read.csv('csv_data/FDA-CBER-2021-5683-0772469-0773670_125742_S1_M5_C4591001-A-D_adsl.csv', sep = ',', stringsAsFactors = FALSE)
for(i in 1:nrow(adsl_data)){
  trial_site_id <- as.character(adsl_data[i, "SITEID"])
  randdt <- as.numeric(gsub("\\D", "", adsl_data[i, "RANDDT"]))
  if(!is.na(randdt)){
    if(randdt <= 20201114){
      sites_data[[trial_site_id]]$adsl_20201114 <- sites_data[[trial_site_id]]$adsl_20201114 + 1
    }
    if(randdt <= 20201119){
      sites_data[[trial_site_id]]$adsl_20201119 <- sites_data[[trial_site_id]]$adsl_20201119 + 1
    }
    if(randdt <= 20210313){
      sites_data[[trial_site_id]]$adsl_20210313 <- sites_data[[trial_site_id]]$adsl_20210313 + 1
    }
    if(randdt <= 20210319){
      sites_data[[trial_site_id]]$adsl_20210319 <- sites_data[[trial_site_id]]$adsl_20210319 + 1
    }
  }
}

# Prepare the output data
output_data <- data.frame(matrix(ncol = 18, nrow = 0))
names(output_data) <- c("trial_site_id", "fa_random_20201114", "adsl_20201114", "offset_20201114", "fa_investig_20201119", "adsl_20201119", "offset_20201119", "m6_random_20210313", "adsl_20210313", "offset_20210313", "m6_investig_20210319", "adsl_20210319", "offset_20210319", "offset_fa_to_m6_investig", "offset_fa_to_m6_random", "subjects_came_in", "subjects_left")

# Process the sites data and write it to the output data
for(trial_site_id in names(sites_data)){
  row <- data.frame(trial_site_id = trial_site_id,
                    fa_random_20201114 = sites_data[[trial_site_id]]$fa_random_20201114,
                    adsl_20201114 = sites_data[[trial_site_id]]$adsl_20201114,
                    offset_20201114 = sites_data[[trial_site_id]]$adsl_20201114 - sites_data[[trial_site_id]]$fa_random_20201114,
                    fa_investig_20201119 = sites_data[[trial_site_id]]$fa_investig_20201119,
                    adsl_20201119 = sites_data[[trial_site_id]]$adsl_20201119,
                    offset_20201119 = sites_data[[trial_site_id]]$adsl_20201119 - sites_data[[trial_site_id]]$fa_investig_20201119,
                    m6_random_20210313 = sites_data[[trial_site_id]]$m6_random_20210313,
                    adsl_20210313 = sites_data[[trial_site_id]]$adsl_20210313,
                    offset_20210313 = sites_data[[trial_site_id]]$adsl_20210313 - sites_data[[trial_site_id]]$m6_random_20210313,
                    m6_investig_20210319 = sites_data[[trial_site_id]]$m6_investig_20210319,
                    adsl_20210319 = sites_data[[trial_site_id]]$adsl_20210319,
                    offset_20210319 = sites_data[[trial_site_id]]$adsl_20210319 - sites_data[[trial_site_id]]$m6_investig_20210319,
                    offset_fa_to_m6_investig = sites_data[[trial_site_id]]$m6_investig_20210319 - sites_data[[trial_site_id]]$fa_investig_20201119,
                    offset_fa_to_m6_random = sites_data[[trial_site_id]]$m6_random_20210313 - sites_data[[trial_site_id]]$fa_random_20201114,
                    subjects_came_in = sites_data[[trial_site_id]]$subjects_came_in,
                    subjects_left = sites_data[[trial_site_id]]$subjects_left)
  output_data <- rbind(output_data, row)
}

# Write the output data to a CSV file
write.csv(output_data, 'Random_Investig_ADSL_synthesis.csv', row.names = FALSE)
