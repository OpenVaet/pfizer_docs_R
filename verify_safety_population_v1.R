# Load necessary libraries
library(jsonlite)
library(readr)
library(dplyr)
library(purrr)
library(lubridate)
library(stats)

# Loading ADSL data.
data_file <- 'subjects_test_data_baseline.csv'

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

adsl_file <- "csv_data/FDA-CBER-2021-5683-0772469-0773670_125742_S1_M5_C4591001-A-D_adsl.csv"

column_types <- cols(
  .default = col_character(),
  SUBJID = col_double(),
  PHASEN = col_double(),
  AGEGR1N = col_double(),
  SAFFL = col_character(),
  MULENRFL = col_character(),
  HIVFL = col_character(),
  TRT01A = col_character()
)

data <- read_csv(adsl_file, na = c("", "NA"), col_types = column_types)

filtered_data <- data %>%
  filter(
    !is.na(PHASEN),
    !is.na(AGEGR1N),
    !is.na(SAFFL),
    !is.na(HIVFL),
    !is.na(TRT01A),
    PHASEN > 1,
    AGEGR1N > 1,
    SAFFL == "Y",
    HIVFL != "Y",
    TRT01A != "",
    is.na(MULENRFL) | MULENRFL != "Y"
  )

stats <- list()
stats_with_days <- list()
stats_by_arms <- list()
ct <- 0
subjects_parsed <- 0
for(i in 1:nrow(filtered_data)) {
  row <- filtered_data[i,]
  ct <- ct + 1
  subjects_parsed <- subjects_parsed + 1
  if (ct == 1000) {
    print(paste('Parsing Subjects ... ', i, '/', nrow(filtered_data)))
    ct <- 0
  }
  subject_id <- row$SUBJID
  covblst <- row$COVBLST
  arm <- row$ARM
  subject_id <- as.character(subject_id)
  vax101dt <- row$VAX101DT
  vax201dt <- row$VAX201DT
  category_arm <- arm
  # print(category_arm)
  # print(vax201dt)
  if (!is.na(arm) && !is.na(vax201dt) && arm == "Placebo" && vax201dt != "NA") {
    category_arm <- 'Placebo -> BNT162b2 30mg'
  }

  v1_PCR <- NA
  v1_NB <- NA
  visit_1_compdate <- NA
  visit_1_date <- NA
  abs_days <- 'MIS'
  if (subject_id %in% names(subjects)) {
    for (test_date in sort(names(subjects[[subject_id]]$tests))) {
      # print(test_date)
      for (test_type in sort(names(subjects[[subject_id]]$tests[[test_date]]))) {
        # print(test_type)
        test_visit <- subjects[[subject_id]]$tests[[test_date]][[test_type]]$test_visit
        if (is.null(test_visit)) {
          stop("test_visit is null")
        }
        
        test_result <- subjects[[subject_id]]$tests[[test_date]][[test_type]]$test_result
        # print(test_visit)
        # print(test_result)
        
        
        if (test_visit == 'V1_DAY1_VAX1_L') {
          visit_1_compdate <- as.numeric(gsub("\\D", "", test_date))
          visit_1_date <- test_date
          
          if (test_type == 'PCR Central') {
            v1_PCR <- test_result
          }
          
          if (test_type == 'N-Binding') {
            v1_NB <- test_result
          }
        }
      }
    }

    # Compute the absolute number of days between the two dates
    visit_1_date <- as.Date(visit_1_date)
    vax101dt <- as.Date(vax101dt)
    abs_days <- abs(visit_1_date - vax101dt)
    abs_days <- as.character(abs_days)
  }
  
  if (is.na(covblst)) {
    covblst <- 'MIS'
  }
  if (is.na(v1_PCR)) {
    v1_PCR <- 'MIS'
  }
  if (is.na(v1_NB)) {   
    v1_NB <- 'MIS'
  }

  # print(paste('subject_id : ', subject_id))
  # print(paste('visit_1_date : ', visit_1_date))
  # print(paste('vax101dt : ', vax101dt))
  # print(paste('covblst : ', covblst))
  # print(paste('v1_PCR : ', v1_PCR))
  # print(paste('v1_NB : ', v1_NB))
  # print(abs_days)
  # break

  # Create a new list for tests if it doesn't exist
  if (!covblst %in% names(stats)) {
    stats[[covblst]] <- list()
  }
  if (!v1_NB %in% names(stats[[covblst]])) {
    stats[[covblst]][[v1_NB]] <- list()
  }
  if (!v1_PCR %in% names(stats[[covblst]][[v1_NB]])) {
    stats[[covblst]][[v1_NB]][[v1_PCR]] <- 0
  }
  stats[[covblst]][[v1_NB]][[v1_PCR]] <- stats[[covblst]][[v1_NB]][[v1_PCR]] + 1


  # Create a new list for tests by days if it doesn't exist
  if (!covblst %in% names(stats_with_days)) {
    stats_with_days[[covblst]] <- list()
  }
  if (!v1_NB %in% names(stats_with_days[[covblst]])) {
    stats_with_days[[covblst]][[v1_NB]] <- list()
  }
  if (!v1_PCR %in% names(stats_with_days[[covblst]][[v1_NB]])) {
    stats_with_days[[covblst]][[v1_NB]][[v1_PCR]] <- list()
  }
  if (!abs_days %in% names(stats_with_days[[covblst]][[v1_NB]][[v1_PCR]])) {
    stats_with_days[[covblst]][[v1_NB]][[v1_PCR]][[abs_days]] <- 0
  }
  stats_with_days[[covblst]][[v1_NB]][[v1_PCR]][[abs_days]] <- stats_with_days[[covblst]][[v1_NB]][[v1_PCR]][[abs_days]] + 1

  # Create a new list for tests by arms if it doesn't exist
  if (!category_arm %in% names(stats_by_arms)) {
    stats_by_arms[[category_arm]] <- list()
  }
  covblst_set <- 0
  if (covblst == 'POS' || covblst == 'NEG') {
    covblst_set <- 1
  }
  covblst_set <- as.character(covblst_set)
  if (!covblst_set %in% names(stats_by_arms[[category_arm]])) {
    stats_by_arms[[category_arm]][[covblst_set]] <- 0
  }
  stats_by_arms[[category_arm]][[covblst_set]] <- stats_by_arms[[category_arm]][[covblst_set]] + 1
}
# print(stats)
print(paste('Total Subjects : ', subjects_parsed))
print(stats_by_arms)

# Function to flatten the stats list and convert it to a data frame
flatten_stats <- function(stats) {
  do.call(rbind, lapply(names(stats), function(covblst) {
    do.call(rbind, lapply(names(stats[[covblst]]), function(v1_NB) {
      do.call(rbind, lapply(names(stats[[covblst]][[v1_NB]]), function(v1_PCR) {
        data.frame(
          COVBLST = covblst,
          `Visit 1 N-Binding` = v1_NB,
          `Visit 1 PCR` = v1_PCR,
          Subjects = stats[[covblst]][[v1_NB]][[v1_PCR]]
        )
      }))
    }))
  }))
}

# Flatten the stats list and write to CSV
stats_df <- flatten_stats(stats)
write.table(stats_df, "covblst_v1_tests.csv", row.names = FALSE, quote = FALSE, sep = ";")

# Function to flatten the stats_with_days list and convert it to a data frame
flatten_stats_with_days <- function(stats_with_days) {
  do.call(rbind, lapply(names(stats_with_days), function(covblst) {
    do.call(rbind, lapply(names(stats_with_days[[covblst]]), function(v1_NB) {
      do.call(rbind, lapply(names(stats_with_days[[covblst]][[v1_NB]]), function(v1_PCR) {
        do.call(rbind, lapply(names(stats_with_days[[covblst]][[v1_NB]][[v1_PCR]]), function(abs_days) {
          data.frame(
            COVBLST = covblst,
            `Visit 1 N-Binding` = v1_NB,
            `Visit 1 PCR` = v1_PCR,
            `Days to Dose 1` = abs_days,
            Subjects = stats_with_days[[covblst]][[v1_NB]][[v1_PCR]][[abs_days]]
          )
        }))
      }))
    }))
  }))
}

# Flatten the stats_with_days list and write to CSV
stats_with_days_df <- flatten_stats_with_days(stats_with_days)
write.table(stats_with_days_df, "covblst_v1_tests_day_tod_d1.csv", row.names = FALSE, quote = FALSE, sep = ";")

# Create a data frame from the stats_by_arms list
stats_by_arms_df <- data.frame(
  `Treatment Arm` = character(),
  `Tag Set` = integer(),
  `Tag Not Set` = integer(),
  stringsAsFactors = FALSE
)

# Populate the data frame
for (arm in names(stats_by_arms)) {
  stats_by_arms_df <- rbind(stats_by_arms_df, data.frame(
    `Treatment Arm` = arm,
    `Tag Set` = stats_by_arms[[arm]][['1']],
    `Tag Not Set` = stats_by_arms[[arm]][['0']]
  ))
}

# Write the data frame to a CSV file
write.table(stats_by_arms_df, file = "stats_by_arms.csv", row.names = FALSE, sep = ";")
