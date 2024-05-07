# Load necessary libraries
library(jsonlite)
library(readr)
library(dplyr)
library(purrr)
library(lubridate)
library(stringr)
library(stats)

# Loading ADSL data.
data_file <- 'subjects_test_data.csv'

# Create an environment to use as a hash table
subjects <- new.env(hash = TRUE, parent = emptyenv(), size = nrow(data))
phase_1_subjects <- new.env(hash = TRUE, parent = emptyenv(), size = nrow(data))

# Read the data
data <- read_csv(data_file)

# Get all unique subject_ids from your data
all_subject_ids <- unique(data$subject_id)

# Initialize variables
row_num <- 0
total_subjects_p1 <- 0
total_subjects_p3 <- 0
phase_1_subjects <- list()

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
  
  if (grepl("Stage 1", values$cohort)) {
    if (!values$subject_id %in% names(phase_1_subjects)) {
      total_subjects_p1 <- total_subjects_p1 + 1
      phase_1_subjects[[values$subject_id]] <- list()
    }
    phase_1_subjects[[values$subject_id]]$exists <- 1
    next
  }
  
  # Create a new list for this subject if it doesn't exist
  if (!exists(values$subject_id, envir = subjects)) {
    total_subjects_p3 <- total_subjects_p3 + 1
    assign(values$subject_id, list(), envir = subjects)
  }
  
  
  # Add or update subject data
  subjects[[values$subject_id]]$age_years <- values$age_years
  subjects[[values$subject_id]]$treatment_arm <- values$treatment_arm
  subjects[[values$subject_id]]$dose_1_date <- values$dose_1_date
  subjects[[values$subject_id]]$dose_2_date <- values$dose_2_date
  subjects[[values$subject_id]]$dose_3_date <- values$dose_3_date
  subjects[[values$subject_id]]$dose_4_date <- values$dose_4_date
  subjects[[values$subject_id]]$randomization_date <- values$randomization_date
  subjects[[values$subject_id]]$randomization_number <- values$randomization_number
  subjects[[values$subject_id]]$screening_date <- values$screening_date
  subjects[[values$subject_id]]$site_id <- values$site_id
  subjects[[values$subject_id]]$unblinding_date <- values$unblinding_date

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
print(paste("total_subjects_p1 : ", total_subjects_p1))
print(paste("total_subjects_p3 : ", total_subjects_p3))

# Configuring ADRG data anomalies.
adrg_exclusions <- c('10561101', '11331382', '11101123', '11331405', '11491117', '12691090', '12691070', '11351357', '11341006', '10891112', '11231105', '10711213', '11631006', '11631005', '11631008')
adrg_exclusions_env <- new.env(hash = TRUE, parent = emptyenv(), size = length(adrg_exclusions))
for (exclusion in adrg_exclusions) {
  assign(exclusion, TRUE, envir = adrg_exclusions_env)
}

# Performing analysis on subjects, and keeping track of each exclusion layer.
stats <- list("0_treatment_arm" = list(),
              "1_adrg_exclusions" = list(),
              "2_age_exclusions" = list(),
              "3_no_visit_1" = list(),
              "4_no_visit_1_PCR" = list(),
              "5_no_visit_1_NBinding" = list(),
              "6_visit_1_Missing_Test" = list(),
              "7_subjects_with_V1_PCR_and_NBinding" = list(),
              "8_visit_1_POS" = list(),
              "9_visit_1_IND" = list(),
              "10_no_dose_1" = list(),
              "11_visit_1_NEG_and_dose_1" = list(),
              "12_no_visit_3" = list(),
              "13_visit_3" = list(),
              "14_visit_3_NEG" = list(),
              "15_visit_3_POS" = list(),
              "16_visit_1_to_3" = list(),
              "17_detected_pre_visit_3" = list(),
              "18_non_detected_pre_visit_3" = list(),
              "19_detection_pre_visit_3_by_site" = list())

# Get all unique treatment arms
treatment_arms <- unique(unlist(lapply(subjects, function(x) x$treatment_arm)))
sites <- unique(unlist(lapply(subjects, function(x) x$site_id)))

# Initialize a list for each treatment arm in each key of `stats`
for (treatment_arm in treatment_arms) {
  stats[["0_treatment_arm"]][[treatment_arm]] <- 0
  stats[["1_adrg_exclusions"]][[treatment_arm]] <- 0
  stats[["2_age_exclusions"]][[treatment_arm]] <- 0
  stats[["3_no_visit_1"]][[treatment_arm]] <- 0
  stats[["4_no_visit_1_PCR"]][[treatment_arm]] <- 0
  stats[["5_no_visit_1_NBinding"]][[treatment_arm]] <- 0
  stats[["6_visit_1_Missing_Test"]][[treatment_arm]] <- 0
  stats[["7_subjects_with_V1_PCR_and_NBinding"]][[treatment_arm]] <- 0
  stats[["8_visit_1_POS"]][[treatment_arm]] <- 0
  stats[["9_visit_1_IND"]][[treatment_arm]] <- 0
  stats[["10_no_dose_1"]][[treatment_arm]] <- 0
  stats[["11_visit_1_NEG_and_dose_1"]][[treatment_arm]] <- 0
  stats[["13_visit_3"]][[treatment_arm]] <- 0
  stats[["14_visit_3_NEG"]][[treatment_arm]] <- 0
  stats[["15_visit_3_POS"]][[treatment_arm]] <- 0
  stats[["17_detected_pre_visit_3"]][[treatment_arm]] <- 0
  stats[["18_non_detected_pre_visit_3"]][[treatment_arm]] <- 0
}
print(stats)

for (subject_id in sort(names(subjects))) {
  subject_id <- as.character(subject_id)
  # print(subject_id)
  treatment_arm <- subjects[[subject_id]]$treatment_arm
  age_years <- subjects[[subject_id]]$age_years

  if (is.null(treatment_arm)) {
    stop("treatment_arm is null")
  }
  stats[['0_treatment_arm']][[treatment_arm]] <- ifelse(is.null(stats[['0_treatment_arm']][[treatment_arm]]), 0, stats[['0_treatment_arm']][[treatment_arm]])
  stats[['0_treatment_arm']][[treatment_arm]] <- stats[['0_treatment_arm']][[treatment_arm]] + 1
  
  if (exists(subject_id, envir = adrg_exclusions_env)) {
    stats[['1_adrg_exclusions']][[treatment_arm]] <- ifelse(is.null(stats[['1_adrg_exclusions']][[treatment_arm]]), 0, stats[['1_adrg_exclusions']][[treatment_arm]])
    stats[['1_adrg_exclusions']][[treatment_arm]] <- stats[['1_adrg_exclusions']][[treatment_arm]] + 1
    next
  }
  
  age_years <- subjects[[subject_id]]$age_years
  if (is.null(age_years)) {
    stop("age_years is null")
  }
  
  if (age_years < 16) {
    stats[['2_age_exclusions']][[treatment_arm]] <- ifelse(is.null(stats[['2_age_exclusions']][[treatment_arm]]), 0, stats[['2_age_exclusions']][[treatment_arm]])
    stats[['2_age_exclusions']][[treatment_arm]] <- stats[['2_age_exclusions']][[treatment_arm]] + 1
    next
  }
  
  has_v1 <- 0
  has_v1_PCR <- 0
  has_v1_NB <- 0
  has_v1_POS <- 0
  has_v1_IND <- 0
  has_v3 <- 0
  has_v3_POS <- 0
  has_v3_IND <- 0
  has_positive_pcr <- 0
  
  visit_1_compdate <- NULL
  visit_3_compdate <- NULL
  visit_1_date <- NULL
  visit_3_date <- NULL
  
  for (test_date in sort(names(subjects[[subject_id]]$tests))) {
    # print(test_date)
    for (test_type in sort(names(subjects[[subject_id]]$tests[[test_date]]))) {
      # print(test_type)
      test_visit <- subjects[[subject_id]]$tests[[test_date]][[test_type]]$test_visit
      if (is.null(test_visit)) {
        stop("test_visit is null")
      }
      
      test_result <- subjects[[subject_id]]$tests[[test_date]][[test_type]]$test_result
      # print(test_result)
      if (is.null(test_result)) {
        stop("test_result is null")
      }
      
      if (test_visit == 'V1_DAY1_VAX1_L') {
        has_v1 <- 1
        visit_1_compdate <- as.numeric(gsub("\\D", "", test_date))
        visit_1_date <- test_date
        
        if (test_type == 'PCR Central') {
          has_v1_PCR <- 1
        }
        
        if (test_type == 'N-Binding') {
          has_v1_NB <- 1
        }
        
        if (test_result == 'POS') {
          has_v1_POS <- 1
        }
        
        if (test_result == 'IND') {
          has_v1_IND <- 1
        }
      } else if (test_visit == 'V3_MONTH1_POSTVAX2_L') {
        has_v3 <- 1
        visit_3_compdate <- as.numeric(gsub("\\D", "", test_date))
        visit_3_date <- test_date
        
        if (test_result == 'POS') {
          has_v3_POS <- 1
        }
        
        if (test_result == 'IND') {
          has_v3_IND <- 1
        }
      }
      
      if (test_type != 'PCR Central' && test_result == 'POS') {
        has_positive_pcr <- 1
      }
    }
  }
  
  if (!has_v1) {
    stats[['3_no_visit_1']][[treatment_arm]] <- ifelse(is.null(stats[['3_no_visit_1']][[treatment_arm]]), 0, stats[['3_no_visit_1']][[treatment_arm]])
    stats[['3_no_visit_1']][[treatment_arm]] <- stats[['3_no_visit_1']][[treatment_arm]] + 1
    next
  }
  
  if (!has_v1_PCR) {
    stats[['4_no_visit_1_PCR']][[treatment_arm]] <- ifelse(is.null(stats[['4_no_visit_1_PCR']][[treatment_arm]]), 0, stats[['4_no_visit_1_PCR']][[treatment_arm]])
    stats[['4_no_visit_1_PCR']][[treatment_arm]] <- stats[['4_no_visit_1_PCR']][[treatment_arm]] + 1
  }
  
  if (!has_v1_NB) {
    stats[['5_no_visit_1_NBinding']][[treatment_arm]] <- ifelse(is.null(stats[['5_no_visit_1_NBinding']][[treatment_arm]]), 0, stats[['5_no_visit_1_NBinding']][[treatment_arm]])
    stats[['5_no_visit_1_NBinding']][[treatment_arm]] <- stats[['5_no_visit_1_NBinding']][[treatment_arm]] + 1
  }
  
  if (!has_v1_PCR || !has_v1_NB) {
    stats[['6_visit_1_Missing_Test']][[treatment_arm]] <- ifelse(is.null(stats[['6_visit_1_Missing_Test']][[treatment_arm]]), 0, stats[['6_visit_1_Missing_Test']][[treatment_arm]])
    stats[['6_visit_1_Missing_Test']][[treatment_arm]] <- stats[['6_visit_1_Missing_Test']][[treatment_arm]] + 1
    next
  }
  
  stats[['7_subjects_with_V1_PCR_and_NBinding']][[treatment_arm]] <- ifelse(is.null(stats[['7_subjects_with_V1_PCR_and_NBinding']][[treatment_arm]]), 0, stats[['7_subjects_with_V1_PCR_and_NBinding']][[treatment_arm]])
  stats[['7_subjects_with_V1_PCR_and_NBinding']][[treatment_arm]] <- stats[['7_subjects_with_V1_PCR_and_NBinding']][[treatment_arm]] + 1

  if (has_v1_POS) {
    stats[['8_visit_1_POS']][[treatment_arm]] <- ifelse(is.null(stats[['8_visit_1_POS']][[treatment_arm]]), 0, stats[['8_visit_1_POS']][[treatment_arm]])
    stats[['8_visit_1_POS']][[treatment_arm]] <- stats[['8_visit_1_POS']][[treatment_arm]] + 1
    next
  }

  if (has_v1_IND) {
    stats[['9_visit_1_IND']][[treatment_arm]] <- ifelse(is.null(stats[['9_visit_1_IND']][[treatment_arm]]), 0, stats[['9_visit_1_IND']][[treatment_arm]])
    stats[['9_visit_1_IND']][[treatment_arm]] <- stats[['9_visit_1_IND']][[treatment_arm]] + 1
    next
  }
  dose_1_date <- subjects[[subject_id]]$dose_1_date

  dose_1_date_test <- as.character(dose_1_date)
  if (is.na(dose_1_date_test) || dose_1_date_test == 'NA') {
    stats[['10_no_dose_1']][[treatment_arm]] <- ifelse(is.null(stats[['10_no_dose_1']][[treatment_arm]]), 0, stats[['10_no_dose_1']][[treatment_arm]])
    stats[['10_no_dose_1']][[treatment_arm]] <- stats[['10_no_dose_1']][[treatment_arm]] + 1
    next
  }
  
  stats[['11_visit_1_NEG_and_dose_1']][[treatment_arm]] <- ifelse(is.null(stats[['11_visit_1_NEG_and_dose_1']][[treatment_arm]]), 0, stats[['11_visit_1_NEG_and_dose_1']][[treatment_arm]])
  stats[['11_visit_1_NEG_and_dose_1']][[treatment_arm]] <- stats[['11_visit_1_NEG_and_dose_1']][[treatment_arm]] + 1
  if (!has_v3) {
    if (!treatment_arm %in% names(stats[['12_no_visit_3']])) {
      stats[['12_no_visit_3']][[treatment_arm]] <- list()
      stats[['12_no_visit_3']][[treatment_arm]]$total <- 0
    }
    has_positive_pcr <- as.character(has_positive_pcr)
    if (!has_positive_pcr %in% names(stats[['12_no_visit_3']][[treatment_arm]])) {
      stats[['12_no_visit_3']][[treatment_arm]][[has_positive_pcr]] <- 0
    }

    stats[['12_no_visit_3']][[treatment_arm]]$total <- stats[['12_no_visit_3']][[treatment_arm]]$total + 1
    stats[['12_no_visit_3']][[treatment_arm]][[has_positive_pcr]] <- stats[['12_no_visit_3']][[treatment_arm]][[has_positive_pcr]] + 1
    next
  }

  stats[['13_visit_3']][[treatment_arm]] <- ifelse(is.null(stats[['13_visit_3']][[treatment_arm]]), 0, stats[['13_visit_3']][[treatment_arm]])
  stats[['13_visit_3']][[treatment_arm]] <- stats[['13_visit_3']][[treatment_arm]] + 1
  
  if (!has_v3_POS) {
    stats[['14_visit_3_NEG']][[treatment_arm]] <- ifelse(is.null(stats[['14_visit_3_NEG']][[treatment_arm]]), 0, stats[['14_visit_3_NEG']][[treatment_arm]])
    stats[['14_visit_3_NEG']][[treatment_arm]] <- stats[['14_visit_3_NEG']][[treatment_arm]] + 1
    next
  }
  
  stats[['15_visit_3_POS']][[treatment_arm]] <- ifelse(is.null(stats[['15_visit_3_POS']][[treatment_arm]]), 0, stats[['15_visit_3_POS']][[treatment_arm]])
  stats[['15_visit_3_POS']][[treatment_arm]] <- stats[['15_visit_3_POS']][[treatment_arm]] + 1

  days_between <- abs(as.Date(visit_1_date, format = "%Y-%m-%d") - as.Date(visit_3_date, format = "%Y-%m-%d"))
  days_between <- as.character(days_between)
  
  if (!days_between %in% names(stats[['16_visit_1_to_3']])) {
    stats[['16_visit_1_to_3']][[days_between]] <- 0
  }
  stats[['16_visit_1_to_3']][[days_between]] <- stats[['16_visit_1_to_3']][[days_between]] + 1
  
  has_positive_pcr_before_v3 <- 0
  first_positive_date <- 99999999
  
  for (test_date in sort(names(subjects[[subject_id]]$tests))) {
    compdate <- as.numeric(gsub("\\D", "", test_date))
    
    if (compdate >= visit_3_compdate) next
    
    for (test_type in names(subjects[[subject_id]]$tests[[test_date]])) {
      if (test_type != 'PCR Central') next
      
      test_visit <- subjects[[subject_id]]$tests[[test_date]][[test_type]]$test_visit
      test_result <- subjects[[subject_id]]$tests[[test_date]][[test_type]]$test_result
      
      if (test_result == 'POS') {
        first_positive_date <- min(first_positive_date, compdate)
        has_positive_pcr_before_v3 <- 1
      }
    }
  }
  
  site_id <- subjects[[subject_id]]$site_id
  site_id <- as.character(site_id)
  
  if (!site_id %in% names(stats[['19_detection_pre_visit_3_by_site']])) {
    stats[['19_detection_pre_visit_3_by_site']][[site_id]] <- list()
    stats[['19_detection_pre_visit_3_by_site']][[site_id]]$total_cases <- 0
    stats[['19_detection_pre_visit_3_by_site']][[site_id]]$treatment_arms <- list()
  }
  stats[['19_detection_pre_visit_3_by_site']][[site_id]]$total_cases <- stats[['19_detection_pre_visit_3_by_site']][[site_id]]$total_cases + 1
  if (!treatment_arm %in% names(stats[['19_detection_pre_visit_3_by_site']][[site_id]]$treatment_arms)) {
    stats[['19_detection_pre_visit_3_by_site']][[site_id]]$treatment_arms[[treatment_arm]] <- list()
    stats[['19_detection_pre_visit_3_by_site']][[site_id]]$treatment_arms[[treatment_arm]]$detected <- 0
    stats[['19_detection_pre_visit_3_by_site']][[site_id]]$treatment_arms[[treatment_arm]]$undetected <- 0
  }
  
  if (has_positive_pcr_before_v3) {
    stats[['17_detected_pre_visit_3']][[treatment_arm]] <- ifelse(is.null(stats[['17_detected_pre_visit_3']][[treatment_arm]]), 0, stats[['17_detected_pre_visit_3']][[treatment_arm]])
    stats[['17_detected_pre_visit_3']][[treatment_arm]] <- stats[['17_detected_pre_visit_3']][[treatment_arm]] + 1
    stats[['19_detection_pre_visit_3_by_site']][[site_id]]$treatment_arms[[treatment_arm]]$detected <- stats[['19_detection_pre_visit_3_by_site']][[site_id]]$treatment_arms[[treatment_arm]]$detected + 1
    next
  }
  
  stats[['18_non_detected_pre_visit_3']][[treatment_arm]] <- ifelse(is.null(stats[['18_non_detected_pre_visit_3']][[treatment_arm]]), 0, stats[['18_non_detected_pre_visit_3']][[treatment_arm]])
  stats[['18_non_detected_pre_visit_3']][[treatment_arm]] <- stats[['18_non_detected_pre_visit_3']][[treatment_arm]] + 1
  stats[['19_detection_pre_visit_3_by_site']][[site_id]]$treatment_arms[[treatment_arm]]$undetected <- ifelse(is.null(stats[['19_detection_pre_visit_3_by_site']][[site_id]]$treatment_arms[[treatment_arm]]$undetected), 0, stats[['19_detection_pre_visit_3_by_site']][[site_id]]$treatment_arms[[treatment_arm]]$undetected)
  stats[['19_detection_pre_visit_3_by_site']][[site_id]]$treatment_arms[[treatment_arm]]$undetected <- stats[['19_detection_pre_visit_3_by_site']][[site_id]]$treatment_arms[[treatment_arm]]$undetected + 1
}

total_sites_with_cases <- length(names(stats[['19_detection_pre_visit_3_by_site']]))
cat("total_sites_with_cases : ", total_sites_with_cases, "\n")

subjects_by_sites <- list()
subjects_by_sites$of_interest$bnt_detected <- 0
subjects_by_sites$of_interest$bnt_undetected <- 0
subjects_by_sites$of_interest$placebo_detected <- 0
subjects_by_sites$of_interest$placebo_undetected <- 0
subjects_by_sites$of_interest$total_sites <- 0
subjects_by_sites$not_of_interest$bnt_detected <- 0
subjects_by_sites$not_of_interest$bnt_undetected <- 0
subjects_by_sites$not_of_interest$placebo_detected <- 0
subjects_by_sites$not_of_interest$placebo_undetected <- 0
subjects_by_sites$not_of_interest$total_sites <- 0
for (site_id in sort(names(stats[['19_detection_pre_visit_3_by_site']]))) {
  bnt_detected <- stats[['19_detection_pre_visit_3_by_site']][[site_id]]$treatment_arms$BNT162b2$detected
  bnt_undetected <- stats[['19_detection_pre_visit_3_by_site']][[site_id]]$treatment_arms$BNT162b2$undetected
  placebo_detected <- stats[['19_detection_pre_visit_3_by_site']][[site_id]]$treatment_arms$Placebo$detected
  placebo_undetected <- stats[['19_detection_pre_visit_3_by_site']][[site_id]]$treatment_arms$Placebo$undetected
  total_cases <- stats[['19_detection_pre_visit_3_by_site']][[site_id]]$total_cases
  if (is.null(bnt_detected)) {
    bnt_detected = 0
    bnt_undetected = 0
    stats[['19_detection_pre_visit_3_by_site']][[site_id]]$treatment_arms$`BNT162b2 Phase 2/3 (30 mcg)` <- list()
    stats[['19_detection_pre_visit_3_by_site']][[site_id]]$treatment_arms$`BNT162b2 Phase 2/3 (30 mcg)`$detected <- 0
    stats[['19_detection_pre_visit_3_by_site']][[site_id]]$treatment_arms$`BNT162b2 Phase 2/3 (30 mcg)`$undetected <- 0
  }
  if (is.null(placebo_detected)) {
    placebo_detected = 0
    placebo_undetected = 0
    stats[['19_detection_pre_visit_3_by_site']][[site_id]]$treatment_arms$Placebo <- list()
    stats[['19_detection_pre_visit_3_by_site']][[site_id]]$treatment_arms$Placebo$detected <- 0
    stats[['19_detection_pre_visit_3_by_site']][[site_id]]$treatment_arms$Placebo$undetected <- 0
  }
  bnt_total <- bnt_detected + bnt_undetected
  placebo_total <- placebo_detected + placebo_undetected
  bnt_detection_rate <- 0
  if (bnt_total > 0) {
    bnt_detection_rate <- round(bnt_detected * 100 / bnt_total, 2) 
  }
  placebo_detection_rate <- 0
  if (placebo_total > 0) {
    placebo_detection_rate <- round(placebo_detected * 100 / placebo_total, 2) 
  }
  
  if (total_cases     < 5 ||
      bnt_total      == 0 ||
      placebo_total  == 0) {
    
    subjects_by_sites$not_of_interest$bnt_detected <- subjects_by_sites$not_of_interest$bnt_detected + bnt_detected
    subjects_by_sites$not_of_interest$bnt_undetected <- subjects_by_sites$not_of_interest$bnt_undetected + bnt_undetected
    subjects_by_sites$not_of_interest$placebo_detected <- subjects_by_sites$not_of_interest$placebo_detected + placebo_detected
    subjects_by_sites$not_of_interest$placebo_undetected <- subjects_by_sites$not_of_interest$placebo_undetected + placebo_undetected
    subjects_by_sites$not_of_interest$total_sites <- subjects_by_sites$not_of_interest$total_sites + 1
    
    stats[['19_detection_pre_visit_3_by_site']][[site_id]] <- NULL
  } else {
    stats[['19_detection_pre_visit_3_by_site']][[site_id]]$treatment_arms$bnt_total <- bnt_total
    stats[['19_detection_pre_visit_3_by_site']][[site_id]]$treatment_arms$bnt_detection_rate <- bnt_detection_rate
    stats[['19_detection_pre_visit_3_by_site']][[site_id]]$treatment_arms$placebo_total <- placebo_total
    stats[['19_detection_pre_visit_3_by_site']][[site_id]]$treatment_arms$placebo_detection_rate <- placebo_detection_rate

    testing_ratio_offset <- abs(bnt_detection_rate - placebo_detection_rate)

    if (bnt_detection_rate < placebo_detection_rate && testing_ratio_offset > 15) {      
      subjects_by_sites$of_interest$bnt_detected <- subjects_by_sites$of_interest$bnt_detected + bnt_detected
      subjects_by_sites$of_interest$bnt_undetected <- subjects_by_sites$of_interest$bnt_undetected + bnt_undetected
      subjects_by_sites$of_interest$placebo_detected <- subjects_by_sites$of_interest$placebo_detected + placebo_detected
      subjects_by_sites$of_interest$placebo_undetected <- subjects_by_sites$of_interest$placebo_undetected + placebo_undetected
      subjects_by_sites$of_interest$total_sites <- subjects_by_sites$of_interest$total_sites + 1
    } else {
      subjects_by_sites$not_of_interest$bnt_detected <- subjects_by_sites$not_of_interest$bnt_detected + bnt_detected
      subjects_by_sites$not_of_interest$bnt_undetected <- subjects_by_sites$not_of_interest$bnt_undetected + bnt_undetected
      subjects_by_sites$not_of_interest$placebo_detected <- subjects_by_sites$not_of_interest$placebo_detected + placebo_detected
      subjects_by_sites$not_of_interest$placebo_undetected <- subjects_by_sites$not_of_interest$placebo_undetected + placebo_undetected
      subjects_by_sites$not_of_interest$total_sites <- subjects_by_sites$not_of_interest$total_sites + 1
      stats[['19_detection_pre_visit_3_by_site']][[site_id]] <- NULL
    }
  }
}

total_sites_with_5_more_cases <- length(names(stats[['19_detection_pre_visit_3_by_site']]))
cat("total_sites_with_5_more_cases : ", total_sites_with_5_more_cases, "\n")

noi_bnt_detected <- subjects_by_sites$not_of_interest$bnt_detected %||% 0
noi_bnt_undetected <- subjects_by_sites$not_of_interest$bnt_undetected %||% 0
noi_placebo_detected <- subjects_by_sites$not_of_interest$placebo_detected %||% 0
noi_placebo_undetected <- subjects_by_sites$not_of_interest$placebo_undetected %||% 0
noi_total_sites <- subjects_by_sites$not_of_interest$total_sites %||% 0

oi_bnt_detected <- subjects_by_sites$of_interest$bnt_detected %||% 0
oi_bnt_undetected <- subjects_by_sites$of_interest$bnt_undetected %||% 0
oi_placebo_detected <- subjects_by_sites$of_interest$placebo_detected %||% 0
oi_placebo_undetected <- subjects_by_sites$of_interest$placebo_undetected %||% 0
oi_total_sites <- subjects_by_sites$of_interest$total_sites %||% 0

noi_bnt_total <- noi_bnt_detected + noi_bnt_undetected
noi_placebo_total <- noi_placebo_detected + noi_placebo_undetected
oi_bnt_total <- oi_bnt_detected + oi_bnt_undetected
oi_placebo_total <- oi_placebo_detected + oi_placebo_undetected

noi_bnt_detection_rate <- round(noi_bnt_detected * 100 / noi_bnt_total, 2)
noi_placebo_detection_rate <- round(noi_placebo_detected * 100 / noi_placebo_total, 2)
oi_bnt_detection_rate <- round(oi_bnt_detected * 100 / oi_bnt_total, 2)
oi_placebo_detection_rate <- round(oi_placebo_detected * 100 / oi_placebo_total, 2)

oi_testing_ratio_offset <- abs(oi_bnt_detection_rate - oi_placebo_detection_rate)
oi_fisher_exact <- fisher.test(matrix(c(oi_bnt_detected, oi_bnt_undetected, oi_placebo_detected, oi_placebo_undetected), nrow=2, byrow=TRUE))$p.value
cat("oi_bnt_detected : ", oi_bnt_detected, "\n")
cat("oi_bnt_undetected : ", oi_bnt_undetected, "\n")
cat("oi_placebo_detected : ", oi_placebo_detected, "\n")
cat("oi_placebo_undetected : ", oi_placebo_undetected, "\n")
cat("oi_fisher_exact : ", oi_fisher_exact, "\n")

noi_testing_ratio_offset <- abs(noi_bnt_detection_rate - noi_placebo_detection_rate)
noi_fisher_exact <- fisher.test(matrix(c(noi_bnt_detected, noi_bnt_undetected, noi_placebo_detected, noi_placebo_undetected), nrow=2, byrow=TRUE))$p.value
cat("noi_bnt_detected : ", noi_bnt_detected, "\n")
cat("noi_bnt_undetected : ", noi_bnt_undetected, "\n")
cat("noi_placebo_detected : ", noi_placebo_detected, "\n")
cat("noi_placebo_undetected : ", noi_placebo_undetected, "\n")
cat("noi_fisher_exact : ", noi_fisher_exact, "\n")

write.csv(data.frame(
  "Site Category" = c("Normal Sites", "Abnormal Sites"),
  "Total Sites" = c(noi_total_sites, oi_total_sites),
  "BNT Detected" = c(noi_bnt_detected, oi_bnt_detected),
  "BNT Undetected" = c(noi_bnt_undetected, oi_bnt_undetected),
  "BNT Detection Rate" = c(noi_bnt_detection_rate, oi_bnt_detection_rate),
  "Placebo Detected" = c(noi_placebo_detected, oi_placebo_detected),
  "Placebo Undetected" = c(noi_placebo_undetected, oi_placebo_undetected),
  "Placebo Detection Rate" = c(noi_placebo_detection_rate, oi_placebo_detection_rate),
  "Fisher Exact Test" = c(noi_fisher_exact, oi_fisher_exact)
), file = "testing_rates_by_sites_nm_vs_anm.csv", row.names = FALSE)

print(stats)

# Fetching values for chi-squares to print.
no_visit_3_bnt162b2 <- stats$`12_no_visit_3`$`BNT162b2 Phase 2/3 (30 mcg)`$total
no_visit_3_placebo <- stats$`12_no_visit_3`$Placebo$total
visit_3_placebo <- stats$`13_visit_3`$Placebo
visit_3_bnt162b2 <- stats$`13_visit_3`$`BNT162b2 Phase 2/3 (30 mcg)`
print(paste('visit_3_bnt162b2 : ', visit_3_bnt162b2))
print(paste('no_visit_3_bnt162b2 : ', no_visit_3_bnt162b2))
print(paste('visit_3_placebo : ', visit_3_placebo))
print(paste('no_visit_3_placebo : ', no_visit_3_placebo))
no_visit_3_no_pcr_bnt162b2 <- stats$`12_no_visit_3`$`BNT162b2 Phase 2/3 (30 mcg)`$`0`
no_visit_3_no_pcr_placebo <- stats$`12_no_visit_3`$Placebo$`0`
print(paste('no_visit_3_no_pcr_bnt162b2 : ', no_visit_3_no_pcr_bnt162b2))
print(paste('no_visit_3_no_pcr_placebo : ', no_visit_3_no_pcr_placebo))

# Printing no-visit chi-square.
no_visit_3_table <- matrix(c(
  no_visit_3_bnt162b2,
  visit_3_bnt162b2,
  no_visit_3_placebo,
  visit_3_placebo
), nrow = 2, byrow = TRUE)
rownames(no_visit_3_table) <- c("BNT162b2 Phase 2/3 (30 mcg)", "Placebo")
colnames(no_visit_3_table) <- c("No Visit 3", "Visit 3")
print(no_visit_3_table)
no_visit_3_chi_sq <- chisq.test(no_visit_3_table)

# Loads the html template
template <- readLines("chi_square_template.html")
print(template)

# Replaces COLUMN_A, COLUMN_B, ROW_A_LABEL, ROW_A_VALUE_1 etc by the values of the contingency table
no_visit_3_template <- template
no_visit_3_template <- gsub("COLUMN_A", colnames(no_visit_3_table)[1], no_visit_3_template)
no_visit_3_template <- gsub("COLUMN_B", colnames(no_visit_3_table)[2], no_visit_3_template)
no_visit_3_template <- gsub("ROW_A_LABEL", rownames(no_visit_3_table)[1], no_visit_3_template)
no_visit_3_template <- gsub("ROW_A_VALUE_1", no_visit_3_table[1, 1], no_visit_3_template)
no_visit_3_template <- gsub("ROW_A_VALUE_2", no_visit_3_table[1, 2], no_visit_3_template)
no_visit_3_template <- gsub("ROW_B_LABEL", rownames(no_visit_3_table)[2], no_visit_3_template)
no_visit_3_template <- gsub("ROW_B_VALUE_1", no_visit_3_table[2, 1], no_visit_3_template)
no_visit_3_template <- gsub("ROW_B_VALUE_2", no_visit_3_table[2, 2], no_visit_3_template)

# Replaces P_VALUE with the p-value from the chi-square test
no_visit_3_template <- gsub("P_VALUE", format.pval(no_visit_3_chi_sq$p.value, digits = 4), no_visit_3_template)

# Writes the modified template
writeLines(no_visit_3_template, "no_visit_3_chi_sq.html")

# Printing no-visit / no positive PCR chi-square.
no_visit_3_no_pcr_table <- matrix(c(
  no_visit_3_no_pcr_bnt162b2,
  visit_3_bnt162b2,
  no_visit_3_no_pcr_placebo,
  visit_3_placebo
), nrow = 2, byrow = TRUE)
rownames(no_visit_3_no_pcr_table) <- c("BNT162b2 Phase 2/3 (30 mcg)", "Placebo")
colnames(no_visit_3_no_pcr_table) <- c("No Visit 3 / No Pos. PCR", "Visit 3")
print(no_visit_3_no_pcr_table)
no_visit_3_no_pcr_chi_sq <- chisq.test(no_visit_3_no_pcr_table)

# Replaces COLUMN_A, COLUMN_B, ROW_A_LABEL, ROW_A_VALUE_1 etc by the values of the contingency table
no_visit_3_no_pcr_template <- template
no_visit_3_no_pcr_template <- gsub("COLUMN_A", colnames(no_visit_3_no_pcr_table)[1], no_visit_3_no_pcr_template)
no_visit_3_no_pcr_template <- gsub("COLUMN_B", colnames(no_visit_3_no_pcr_table)[2], no_visit_3_no_pcr_template)
no_visit_3_no_pcr_template <- gsub("ROW_A_LABEL", rownames(no_visit_3_no_pcr_table)[1], no_visit_3_no_pcr_template)
no_visit_3_no_pcr_template <- gsub("ROW_A_VALUE_1", no_visit_3_no_pcr_table[1, 1], no_visit_3_no_pcr_template)
no_visit_3_no_pcr_template <- gsub("ROW_A_VALUE_2", no_visit_3_no_pcr_table[1, 2], no_visit_3_no_pcr_template)
no_visit_3_no_pcr_template <- gsub("ROW_B_LABEL", rownames(no_visit_3_no_pcr_table)[2], no_visit_3_no_pcr_template)
no_visit_3_no_pcr_template <- gsub("ROW_B_VALUE_1", no_visit_3_no_pcr_table[2, 1], no_visit_3_no_pcr_template)
no_visit_3_no_pcr_template <- gsub("ROW_B_VALUE_2", no_visit_3_no_pcr_table[2, 2], no_visit_3_no_pcr_template)

# Replaces P_VALUE with the p-value from the chi-square test
no_visit_3_no_pcr_template <- gsub("P_VALUE", format.pval(no_visit_3_no_pcr_chi_sq$p.value, digits = 4), no_visit_3_no_pcr_template)

# Writes the modified template
writeLines(no_visit_3_no_pcr_template, "no_visit_3_no_pcr_chi_sq.html")
