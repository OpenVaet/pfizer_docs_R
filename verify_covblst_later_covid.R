# Load necessary libraries
library(jsonlite)
library(readr)
library(dplyr)
library(purrr)
library(lubridate)
library(stats)

# Loading ADSL data.
data_file <- 'subjects_test_data.csv'

# Create environments to use as a hash table
subjects <- new.env(hash = TRUE, parent = emptyenv(), size = nrow(data))

# Read the data
data <- read_csv(data_file)

# Initialize variables
row_num <- 0

# Loop over rows
count_display <- 0
for (i in 1:nrow(data)) {
  row <- data[i, ]
  row_num <- row_num + 1
  count_display <- count_display + 1
  if (count_display == 1000) {
    count_display <- 0
    print(paste("Loaded Rows        : ", row_num, " / ", nrow(data)))
  }
  
  values <- setNames(as.list(row), names(row))

  values$subject_id <- as.character(values$subject_id)

  # Create a new list for tests if it doesn't exist
  if (!"tests" %in% names(subjects[[values$subject_id]])) {
    subjects[[values$subject_id]]$tests <- list()
  }

  # Create a new list for this test date if it doesn't exist
  values$test_date <- as.character(values$test_date)
  if (!values$test_date %in% names(subjects[[values$subject_id]]$tests)) {
    subjects[[values$subject_id]]$tests[[values$test_date]] <- list()
  }

  # Create a new list for this test date test type if it doesn't exist
  values$test_type <- as.character(values$test_type)
  if (!values$test_type %in% names(subjects[[values$subject_id]]$tests[[values$test_date]])) {
    subjects[[values$subject_id]]$tests[[values$test_date]][[values$test_type]] <- list()
  }

  # Add or update test data
  subjects[[values$subject_id]]$tests[[values$test_date]][[values$test_type]]$test_visit <- values$test_visit
  subjects[[values$subject_id]]$tests[[values$test_date]][[values$test_type]]$test_result <- values$test_result
}

row_num <- row_num - 1
print(paste("total_rows        : ", row_num))

subjects_file <- "covblst_subjects_data.csv"

data <- read_csv(subjects_file, na = c("", "NA"))

subjects_total <- 0
subjects_negative <- 0
subjects_confirmed_neg_then_positive_pcr <- 0
ct <- 0

tests_stats <- list()
arm_neg_to_pos_stats <- list()
arm_stats <- list()
for(i in 1:nrow(data)) {
  row <- data[i,]
  ct <- ct + 1
  subjects_total <- subjects_total + 1
  if (ct == 1000) {
    print(paste('Parsing Subjects ... ', i, '/', nrow(data)))
    ct <- 0
  }
  subjid <- row$SUBJID
  subjid <- as.character(subjid)
  visit_1_date <- row$visit_1_date
  category_arm <- row$category_arm
  arm <- row$arm
  vax101dt <- row$vax101dt
  vax201dt <- row$vax201dt
  vax201dt <- as.numeric(gsub("\\D", "", vax201dt))
  covblst <- row$covblst
  v1_PCR <- row$v1_PCR
  v1_NB <- row$v1_NB
  abs_days <- row$abs_days

  # print(paste('subjid : ', subjid))
  # print(paste('visit_1_date : ', visit_1_date))
  # print(paste('vax101dt : ', vax101dt))
  # print(paste('covblst : ', covblst))
  # print(paste('v1_PCR : ', v1_PCR))
  # print(paste('v1_NB : ', v1_NB))
  # print(abs_days)
  confirmed_negative <- 0
  confirmed_negative_test <- NULL
  confirmed_negative_date <- 99999999
  if (subjid %in% names(subjects)) {
    for (test_date in sort(names(subjects[[subjid]]$tests))) {
      if (test_date > '2020-11-20') {
        next
      }
      # print(paste('test_date : ', test_date))
      for (test_type in sort(names(subjects[[subjid]]$tests[[test_date]]))) {
        # print(paste('test_type : ', test_type))
        test_visit <- subjects[[subjid]]$tests[[test_date]][[test_type]]$test_visit
        if (is.null(test_visit)) {
          stop("test_visit is null")
        }
        
        test_result <- subjects[[subjid]]$tests[[test_date]][[test_type]]$test_result
        # print(paste('test_visit : ', test_visit))
        # print(paste('test_result : ', test_result))
        
        
        if (test_visit == 'V1_DAY1_VAX1_L') {
          next
        }

        if (test_result == 'NEG') {
          visit_compdate <- as.numeric(gsub("\\D", "", test_date))

          if (visit_compdate < confirmed_negative_date) {
            confirmed_negative_date <- visit_compdate
            confirmed_negative <- 1
            confirmed_negative_test <- test_type
          }
        }
      }
    }
  }

  if (!category_arm %in% names(arm_stats)) {
    arm_stats[[category_arm]] <- list()
    arm_stats[[category_arm]][['total_subjects']] <- 0
  }
  arm_stats[[category_arm]][['total_subjects']] <- arm_stats[[category_arm]][['total_subjects']] + 1
  if (confirmed_negative == 1) {
    subjects_negative <- subjects_negative + 1
    if (!category_arm %in% names(tests_stats)) {
      tests_stats[[category_arm]] <- list()
    }
    if (!confirmed_negative_test %in% names(tests_stats[[category_arm]])) {
      tests_stats[[category_arm]][[confirmed_negative_test]] <- 0
    }
    tests_stats[[category_arm]][[confirmed_negative_test]] <- tests_stats[[category_arm]][[confirmed_negative_test]] + 1

    # Testing if the subject was later detected positive again via PCR.
    confirmed_positive_pcr <- 0
    confirmed_positive_pcr_test <- NULL
    confirmed_positive_pcr_date <- 99999999
    for (test_date in sort(names(subjects[[subjid]]$tests))) {
      if (test_date > '2020-11-20') {
        next
      }
      # print(paste('test_date : ', test_date))
      for (test_type in sort(names(subjects[[subjid]]$tests[[test_date]]))) {
        # print(paste('test_type : ', test_type))
        test_visit <- subjects[[subjid]]$tests[[test_date]][[test_type]]$test_visit
        if (is.null(test_visit)) {
          stop("test_visit is null")
        }
        
        test_result <- subjects[[subjid]]$tests[[test_date]][[test_type]]$test_result
        # print(paste('test_visit : ', test_visit))
        # print(paste('test_result : ', test_result))
        
        
        if (test_visit == 'V1_DAY1_VAX1_L') {
          next
        }

        if (test_result == 'NEG' & (test_type == 'PCR Central' || test_type == 'PCR Local')) {
          visit_compdate <- as.numeric(gsub("\\D", "", test_date))

          if (visit_compdate > confirmed_negative_date & confirmed_positive_pcr_date > visit_compdate) {
            confirmed_positive_pcr_date <- visit_compdate
            confirmed_positive_pcr <- 1
            confirmed_positive_pcr_test <- test_type
          }
        }
      }
    }

    if (!arm %in% names(arm_neg_to_pos_stats)) {
      arm_neg_to_pos_stats[[arm]] <- list()
      arm_neg_to_pos_stats[[arm]][['total_subjects_neg']] <- 0
      arm_neg_to_pos_stats[[arm]][['total_subjects_neg_to_pos']] <- 0
    }
    arm_neg_to_pos_stats[[arm]][['total_subjects_neg']] <- arm_neg_to_pos_stats[[arm]][['total_subjects_neg']] + 1
    if (confirmed_positive_pcr == 1) {
      subjects_confirmed_neg_then_positive_pcr <- subjects_confirmed_neg_then_positive_pcr + 1
      arm_neg_to_pos_stats[[arm]][['total_subjects_neg_to_pos']] <- arm_neg_to_pos_stats[[arm]][['total_subjects_neg_to_pos']] + 1
    }
  }
  print(paste('confirmed_negative_date : ', confirmed_negative_date))
  print(paste('confirmed_negative : ', confirmed_negative))
  print(paste('confirmed_negative_test : ', confirmed_negative_test))
  # break
}

# Write the tests_stats data frame to a CSV file
tests_stats_df <- data.frame(Treatment_Arm = character(), 
                             Confirmed_Negative_Via = character(), 
                             Total_Subjects = numeric(), 
                             stringsAsFactors = FALSE)
for (arm in names(tests_stats)) {
  for (test in names(tests_stats[[arm]])) {
    tests_stats_df <- rbind(tests_stats_df, 
                            data.frame(Treatment_Arm = arm, 
                                       Confirmed_Negative_Via = test, 
                                       Total_Subjects = tests_stats[[arm]][[test]]))
  }
}
write_csv(tests_stats_df, "covblist_pos_or_mis_confirmed_negative.csv")

# Write the arm_neg_to_pos_stats data frame to a CSV file
arm_neg_to_pos_stats_df <- data.frame(Treatment_Arm = character(), 
                                      Subjects_Confirmed_Negative = numeric(), 
                                      Subjects_Later_PCR_Positive = numeric(), 
                                      stringsAsFactors = FALSE)

for (arm in names(arm_neg_to_pos_stats)) {
  arm_neg_to_pos_stats_df <- rbind(arm_neg_to_pos_stats_df, 
                                   data.frame(Treatment_Arm = arm, 
                                              Subjects_Confirmed_Negative = arm_neg_to_pos_stats[[arm]][['total_subjects_neg']], 
                                              Subjects_Later_PCR_Positive = arm_neg_to_pos_stats[[arm]][['total_subjects_neg_to_pos']]))
}

write_csv(arm_neg_to_pos_stats_df, "covblst_neg_to_pos.csv")


# Console outputs.
print(paste('subjects_negative : ', subjects_negative))
print(paste('subjects_total : ', subjects_total))
print(paste('subjects_confirmed_neg_then_positive_pcr : ', subjects_confirmed_neg_then_positive_pcr))
# print(arm_neg_to_pos_stats)
