library(dplyr)
library(readr)

file1 <- 'na_subjects_found.csv'
file2 <- 'na_subjects_found_in_xpt.csv'

subjects <- list()
subjects_profiles <- list()

data1 <- read_csv(file1)

for(i in 1:nrow(data1)){
  subjid <- as.character(data1$SUBJID[i])
  subjects[[data1$file[i]]]$subjects[subjid] <- 1
  if(is.null(subjects_profiles[[subjid]][data1$file[i]])){
    subjects_profiles[[subjid]][data1$file[i]] <- 1
  } else {
    subjects_profiles[[subjid]][data1$file[i]] <- subjects_profiles[[subjid]][data1$file[i]] + 1
  }
}

data2 <- read_csv(file2)

for(i in 1:nrow(data2)){
  subjid <- as.character(data2$file[i])
  if(is.null(subjects[[subjid]])){
    subjects[[subjid]] <- list(subjects = list(), rows = 0)
  }
  subjects[[subjid]]$rows <- data2$rows[i]
}

# Ensure that all elements of the subjects list are populated
for(i in 1:length(subjects)){
  if(is.null(subjects[[i]]$subjects)){
    subjects[[i]]$subjects <- list()
  }
  if(is.null(subjects[[i]]$rows)){
    subjects[[i]]$rows <- 0
  }
}

write.csv(data.frame("File" = names(subjects), "Subjects" = sapply(subjects, function(x) length(names(x$subjects))), "Rows" = sapply(subjects, function(x) x$rows)), file = "NA_SF_Subjects_by_files.csv", row.names = FALSE)

stats <- list()
df_files_by_subjects <- data.frame("Subject Id" = character(), "Total Files" = integer(), "File" = character(), "Rows" = integer())
df_over_N_xpt_lost <- data.frame("SUBJID" = character())

for(subject_id in names(subjects_profiles)){
  total_files <- length(names(subjects_profiles[[subject_id]]))
  stats[[as.character(total_files)]] <- stats[[as.character(total_files)]] + 1
  
  if(total_files >= 20){
    # Add to df_over_N_xpt_lost
    df_over_N_xpt_lost <- rbind(df_over_N_xpt_lost, data.frame("SUBJID" = subject_id))
    
    for(file in names(subjects_profiles[[subject_id]])){
      rows <- subjects_profiles[[subject_id]][[file]]
      # Add to df_files_by_subjects
      df_files_by_subjects <- rbind(df_files_by_subjects, data.frame("Subject Id" = subject_id, "Total Files" = total_files, "File" = file, "Rows" = rows))
    }
  }
}

# Write data frames to CSV
write.csv(df_files_by_subjects, file = "NA_SF_Files_by_Subjects.csv", row.names = FALSE)
write.csv(df_over_N_xpt_lost, file = "over_N_xpt_lost.csv", row.names = FALSE)
