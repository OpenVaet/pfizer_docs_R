library(readr)
library(dplyr)

adsl_file <- 'csv_data/FDA-CBER-2021-5683-0772469-0773670_125742_S1_M5_C4591001-A-D_adsl.csv'
data <- read_csv(adsl_file)

total_changed_sites <- 0
stats <- list()

# Initialize stats list
unique_ids <- unique(c(substr(data$SUBJID, 1, 4), data$SITEID))
for(id in unique_ids) {
  if (id == 4444) {
    next
  }
  stats[[as.character(id)]] <- list(Subjects_Came_In = 0, Subjects_Left = 0)
}

for(i in 1:nrow(data)) {
  row <- data[i,]
  subject_id <- row$SUBJID
  trial_site_id <- row$SITEID
  original_site_id <- substr(subject_id, 1, 4)
  
  if(trial_site_id == 4444) trial_site_id <- 1231
  if(original_site_id == 4444) original_site_id <- 1231
  
  if(original_site_id != trial_site_id) {
    total_changed_sites <- total_changed_sites + 1
    stats[[as.character(trial_site_id)]]$Subjects_Came_In <- stats[[as.character(trial_site_id)]]$Subjects_Came_In + 1
    stats[[as.character(original_site_id)]]$Subjects_Left <- stats[[as.character(original_site_id)]]$Subjects_Left + 1
  }
}

print(paste("total_changed_sites :", total_changed_sites))

# Convert the list to a data frame
stats_df <- do.call(rbind, lapply(names(stats), function(x) {
  data.frame(Trial_Site_Id = x, Subjects_Came_In = stats[[x]]$Subjects_Came_In, Subjects_Left = stats[[x]]$Subjects_Left)
}))

write.csv(stats_df, file = "subjects_changed_sites.csv", row.names = FALSE)
