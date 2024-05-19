# Loads necessary libraries
library(jsonlite)
library(readr)
library(dplyr)
library(purrr)
library(lubridate)
library(stringr)
library(stats)

# Reading the input CSV file for ADSL data
adsl_data <- read_csv("csv_data/FDA-CBER-2021-5683-1066333-1067534_125742_S6_M5_c4591001-A-D-adsl.csv")

# Filtering and selecting necessary columns
adsl_data <- adsl_data %>%
  filter(RANDDT <= '2021-03-15') %>%
  select(SUBJID, ARM, COHORT, RANDNO, AGETR01, SITEID, UNBLNDDT, RANDDT, RFICDT, VAX101DT, VAX102DT, VAX201DT, VAX202DT)
print(adsl_data)

# Convert SUBJID to character type
adsl_data$SUBJID <- as.character(adsl_data$SUBJID)

# Reading the MB file
mb_data <- read_csv("csv_data/FDA-CBER-2021-5683-0282366 to -0285643_125742_S1_M5_c4591001-S-D-mb.csv")

# Processing MB data
mb_data <- mb_data %>%
  filter(!is.na(MBDTC), !is.na(MBORRES), MBDTC < '2021-03-15', !is.na(VISIT), MBTEST %in% c('Cepheid RT-PCR assay for SARS-CoV-2', 'SEVERE ACUTE RESP SYNDROME CORONAVIRUS 2')) %>%
  mutate(SUBJID = substr(USUBJID, 15, 24),
         TESTDATE = substr(MBDTC, 1, 10),
         TESTTYPE = ifelse(MBTEST == 'SEVERE ACUTE RESP SYNDROME CORONAVIRUS 2', 'PCR Local', 'PCR Central'),
         MBORRES = case_when(
           MBORRES == 'INDETERMINATE' ~ 'IND',
           MBORRES == 'POSITIVE' ~ 'POS',
           MBORRES == 'NEGATIVE' ~ 'NEG',
           TRUE ~ MBORRES
         )) %>%
  select(SUBJID, TESTDATE, VISIT, TESTTYPE, MBORRES)
print(mb_data)

# Reading the ADVA file
adva_data <- read_csv("csv_data/FDA-CBER-2021-5683-0123168 to -0126026_125742_S1_M5_c4591001-A-D-adva.csv")
print(adva_data)

# Processing ADVA data
adva_data <- adva_data %>%
  filter(ADT < '2021-03-15', PARAM == 'N-binding antibody - N-binding Antibody Assay') %>%
  mutate(TESTTYPE = 'N-Binding',
         TESTDATE = substr(ADT, 1, 10)) %>%
  select(SUBJID, TESTDATE, VISIT, TESTTYPE, AVALC)

# Merging ADSL and MB data
merged_mb_data <- merge(adsl_data, mb_data, by = "SUBJID")
print(merged_mb_data)

# Renaming columns for consistency
names(merged_mb_data)[names(merged_mb_data) == 'MBORRES'] <- 'TESTRESULT'

# Merging ADSL and ADVA data
merged_adva_data <- merge(adsl_data, adva_data, by = "SUBJID")
names(merged_adva_data)[names(merged_adva_data) == 'AVALC'] <- 'TESTRESULT'

# Combining both datasets
final_data <- rbind(merged_mb_data, merged_adva_data)

print(final_data)

# Writing the final data to a CSV file
write.csv(final_data, file = "subjects_test_data.csv", row.names = FALSE)

# Creates an environment to use as a hash table
subjects <- new.env(hash = TRUE, parent = emptyenv(), size = nrow(data))
phase_1_subjects <- new.env(hash = TRUE, parent = emptyenv(), size = nrow(data))

# Initializes variables
row_num <- 0
total_subjects_p1 <- 0
total_subjects_p3 <- 0
phase_1_subjects <- list()

# Loops over rows
count_display <- 0
for (i in 1:nrow(final_data)) {
  row <- final_data[i, ]
  row_num <- row_num + 1
  count_display <- count_display + 1
  if (count_display == 1000) {
    count_display <- 0
    print(paste("Loaded Rows        : ", row_num, " / ", nrow(final_data)))
  }
  
  values <- setNames(as.list(row), names(row))

  values$SUBJID <- as.character(values$SUBJID)
  
  if (grepl("Stage 1", values$COHORT)) {
    if (!values$SUBJID %in% names(phase_1_subjects)) {
      total_subjects_p1 <- total_subjects_p1 + 1
      phase_1_subjects[[values$SUBJID]] <- list()
    }
    phase_1_subjects[[values$SUBJID]]$exists <- 1
    next
  }
  
  # Creates a new list for this subject if it doesn't exist
  if (!exists(values$SUBJID, envir = subjects)) {
    total_subjects_p3 <- total_subjects_p3 + 1
    assign(values$SUBJID, list(), envir = subjects)
  }
  
  
  # Adds or updates subject data
  subjects[[values$SUBJID]]$AGE <- values$AGE
  subjects[[values$SUBJID]]$ARM <- values$ARM
  subjects[[values$SUBJID]]$VAX101DT <- values$VAX101DT
  subjects[[values$SUBJID]]$VAX102DT <- values$VAX102DT
  subjects[[values$SUBJID]]$VAX201DT <- values$VAX201DT
  subjects[[values$SUBJID]]$VAX202DT <- values$VAX202DT
  subjects[[values$SUBJID]]$RANDDT <- values$RANDDT
  subjects[[values$SUBJID]]$RANDNO <- values$RANDNO
  subjects[[values$SUBJID]]$RFICDT <- values$RFICDT
  subjects[[values$SUBJID]]$SITEID <- values$SITEID
  subjects[[values$SUBJID]]$UNBLNDDT <- values$UNBLNDDT

  # Creates a new list for tests if it doesn't exist
  if (!"tests" %in% names(subjects[[values$SUBJID]])) {
    subjects[[values$SUBJID]]$tests <- list()
  }

  # Creates a new list for this test date if it doesn't exist
  values$TESTDATE <- as.character(values$TESTDATE)
  if (!values$TESTDATE %in% names(subjects[[values$SUBJID]]$tests)) {
    subjects[[values$SUBJID]]$tests[[values$TESTDATE]] <- list()
  }

  # Creates a new list for this test date test type if it doesn't exist
  values$TESTTYPE <- as.character(values$TESTTYPE)
  if (!values$TESTTYPE %in% names(subjects[[values$SUBJID]]$tests[[values$TESTDATE]])) {
    subjects[[values$SUBJID]]$tests[[values$TESTDATE]][[values$TESTTYPE]] <- list()
  }

  # Adds or updates test data
  subjects[[values$SUBJID]]$tests[[values$TESTDATE]][[values$TESTTYPE]]$VISIT <- values$VISIT
  subjects[[values$SUBJID]]$tests[[values$TESTDATE]][[values$TESTTYPE]]$TESTRESULT <- values$TESTRESULT

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
stats <- list("0_ARM" = list(),
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

# Gets all unique treatment treatment_arm
treatment_arm <- unique(unlist(lapply(subjects, function(x) x$ARM)))
sites <- unique(unlist(lapply(subjects, function(x) x$SITEID)))

# Initializes a list for each treatment arm in each key of `stats`
for (ARM in treatment_arm) {
  stats[["0_ARM"]][[ARM]] <- 0
  stats[["1_adrg_exclusions"]][[ARM]] <- 0
  stats[["2_age_exclusions"]][[ARM]] <- 0
  stats[["3_no_visit_1"]][[ARM]] <- 0
  stats[["4_no_visit_1_PCR"]][[ARM]] <- 0
  stats[["5_no_visit_1_NBinding"]][[ARM]] <- 0
  stats[["6_visit_1_Missing_Test"]][[ARM]] <- 0
  stats[["7_subjects_with_V1_PCR_and_NBinding"]][[ARM]] <- 0
  stats[["8_visit_1_POS"]][[ARM]] <- 0
  stats[["9_visit_1_IND"]][[ARM]] <- 0
  stats[["10_no_dose_1"]][[ARM]] <- 0
  stats[["11_visit_1_NEG_and_dose_1"]][[ARM]] <- 0
  stats[["13_visit_3"]][[ARM]] <- 0
  stats[["14_visit_3_NEG"]][[ARM]] <- 0
  stats[["15_visit_3_POS"]][[ARM]] <- 0
  stats[["17_detected_pre_visit_3"]][[ARM]] <- 0
  stats[["18_non_detected_pre_visit_3"]][[ARM]] <- 0
}
print(stats)

for (SUBJID in sort(names(subjects))) {
  SUBJID <- as.character(SUBJID)
  # print(SUBJID)
  ARM <- subjects[[SUBJID]]$ARM
  AGE <- subjects[[SUBJID]]$AGE

  if (is.null(ARM)) {
    stop("ARM is null")
  }
  stats[['0_ARM']][[ARM]] <- ifelse(is.null(stats[['0_ARM']][[ARM]]), 0, stats[['0_ARM']][[ARM]])
  stats[['0_ARM']][[ARM]] <- stats[['0_ARM']][[ARM]] + 1
  
  if (exists(SUBJID, envir = adrg_exclusions_env)) {
    stats[['1_adrg_exclusions']][[ARM]] <- ifelse(is.null(stats[['1_adrg_exclusions']][[ARM]]), 0, stats[['1_adrg_exclusions']][[ARM]])
    stats[['1_adrg_exclusions']][[ARM]] <- stats[['1_adrg_exclusions']][[ARM]] + 1
    next
  }
  
  AGE <- subjects[[SUBJID]]$AGE
  if (is.null(AGE)) {
    stop("AGE is null")
  }
  
  if (AGE < 16) {
    stats[['2_age_exclusions']][[ARM]] <- ifelse(is.null(stats[['2_age_exclusions']][[ARM]]), 0, stats[['2_age_exclusions']][[ARM]])
    stats[['2_age_exclusions']][[ARM]] <- stats[['2_age_exclusions']][[ARM]] + 1
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
  
  for (TESTDATE in sort(names(subjects[[SUBJID]]$tests))) {
    # print(TESTDATE)
    for (TESTTYPE in sort(names(subjects[[SUBJID]]$tests[[TESTDATE]]))) {
      # print(TESTTYPE)
      VISIT <- subjects[[SUBJID]]$tests[[TESTDATE]][[TESTTYPE]]$VISIT
      if (is.null(VISIT)) {
        stop("VISIT is null")
      }
      
      TESTRESULT <- subjects[[SUBJID]]$tests[[TESTDATE]][[TESTTYPE]]$TESTRESULT
      # print(TESTRESULT)
      if (is.null(TESTRESULT)) {
        stop("TESTRESULT is null")
      }
      
      if (VISIT == 'V1_DAY1_VAX1_L') {
        has_v1 <- 1
        visit_1_compdate <- as.numeric(gsub("\\D", "", TESTDATE))
        visit_1_date <- TESTDATE
        
        if (TESTTYPE == 'PCR Central') {
          has_v1_PCR <- 1
        }
        
        if (TESTTYPE == 'N-Binding') {
          has_v1_NB <- 1
        }
        
        if (TESTRESULT == 'POS') {
          has_v1_POS <- 1
        }
        
        if (TESTRESULT == 'IND') {
          has_v1_IND <- 1
        }
      } else if (VISIT == 'V3_MONTH1_POSTVAX2_L') {
        has_v3 <- 1
        visit_3_compdate <- as.numeric(gsub("\\D", "", TESTDATE))
        visit_3_date <- TESTDATE
        
        if (TESTRESULT == 'POS') {
          has_v3_POS <- 1
        }
        
        if (TESTRESULT == 'IND') {
          has_v3_IND <- 1
        }
      }
      
      if (TESTTYPE != 'PCR Central' && TESTRESULT == 'POS') {
        has_positive_pcr <- 1
      }
    }
  }
  
  if (!has_v1) {
    stats[['3_no_visit_1']][[ARM]] <- ifelse(is.null(stats[['3_no_visit_1']][[ARM]]), 0, stats[['3_no_visit_1']][[ARM]])
    stats[['3_no_visit_1']][[ARM]] <- stats[['3_no_visit_1']][[ARM]] + 1
    next
  }
  
  if (!has_v1_PCR) {
    stats[['4_no_visit_1_PCR']][[ARM]] <- ifelse(is.null(stats[['4_no_visit_1_PCR']][[ARM]]), 0, stats[['4_no_visit_1_PCR']][[ARM]])
    stats[['4_no_visit_1_PCR']][[ARM]] <- stats[['4_no_visit_1_PCR']][[ARM]] + 1
  }
  
  if (!has_v1_NB) {
    stats[['5_no_visit_1_NBinding']][[ARM]] <- ifelse(is.null(stats[['5_no_visit_1_NBinding']][[ARM]]), 0, stats[['5_no_visit_1_NBinding']][[ARM]])
    stats[['5_no_visit_1_NBinding']][[ARM]] <- stats[['5_no_visit_1_NBinding']][[ARM]] + 1
  }
  
  if (!has_v1_PCR || !has_v1_NB) {
    stats[['6_visit_1_Missing_Test']][[ARM]] <- ifelse(is.null(stats[['6_visit_1_Missing_Test']][[ARM]]), 0, stats[['6_visit_1_Missing_Test']][[ARM]])
    stats[['6_visit_1_Missing_Test']][[ARM]] <- stats[['6_visit_1_Missing_Test']][[ARM]] + 1
    next
  }
  
  stats[['7_subjects_with_V1_PCR_and_NBinding']][[ARM]] <- ifelse(is.null(stats[['7_subjects_with_V1_PCR_and_NBinding']][[ARM]]), 0, stats[['7_subjects_with_V1_PCR_and_NBinding']][[ARM]])
  stats[['7_subjects_with_V1_PCR_and_NBinding']][[ARM]] <- stats[['7_subjects_with_V1_PCR_and_NBinding']][[ARM]] + 1

  if (has_v1_POS) {
    stats[['8_visit_1_POS']][[ARM]] <- ifelse(is.null(stats[['8_visit_1_POS']][[ARM]]), 0, stats[['8_visit_1_POS']][[ARM]])
    stats[['8_visit_1_POS']][[ARM]] <- stats[['8_visit_1_POS']][[ARM]] + 1
    next
  }

  if (has_v1_IND) {
    stats[['9_visit_1_IND']][[ARM]] <- ifelse(is.null(stats[['9_visit_1_IND']][[ARM]]), 0, stats[['9_visit_1_IND']][[ARM]])
    stats[['9_visit_1_IND']][[ARM]] <- stats[['9_visit_1_IND']][[ARM]] + 1
    next
  }
  VAX101DT <- subjects[[SUBJID]]$VAX101DT

  VAX101DT_test <- as.character(VAX101DT)
  if (is.na(VAX101DT_test) || VAX101DT_test == 'NA') {
    stats[['10_no_dose_1']][[ARM]] <- ifelse(is.null(stats[['10_no_dose_1']][[ARM]]), 0, stats[['10_no_dose_1']][[ARM]])
    stats[['10_no_dose_1']][[ARM]] <- stats[['10_no_dose_1']][[ARM]] + 1
    next
  }
  
  stats[['11_visit_1_NEG_and_dose_1']][[ARM]] <- ifelse(is.null(stats[['11_visit_1_NEG_and_dose_1']][[ARM]]), 0, stats[['11_visit_1_NEG_and_dose_1']][[ARM]])
  stats[['11_visit_1_NEG_and_dose_1']][[ARM]] <- stats[['11_visit_1_NEG_and_dose_1']][[ARM]] + 1
  if (!has_v3) {
    if (!ARM %in% names(stats[['12_no_visit_3']])) {
      stats[['12_no_visit_3']][[ARM]] <- list()
      stats[['12_no_visit_3']][[ARM]]$total <- 0
    }
    has_positive_pcr <- as.character(has_positive_pcr)
    if (!has_positive_pcr %in% names(stats[['12_no_visit_3']][[ARM]])) {
      stats[['12_no_visit_3']][[ARM]][[has_positive_pcr]] <- 0
    }

    stats[['12_no_visit_3']][[ARM]]$total <- stats[['12_no_visit_3']][[ARM]]$total + 1
    stats[['12_no_visit_3']][[ARM]][[has_positive_pcr]] <- stats[['12_no_visit_3']][[ARM]][[has_positive_pcr]] + 1
    next
  }

  stats[['13_visit_3']][[ARM]] <- ifelse(is.null(stats[['13_visit_3']][[ARM]]), 0, stats[['13_visit_3']][[ARM]])
  stats[['13_visit_3']][[ARM]] <- stats[['13_visit_3']][[ARM]] + 1
  
  if (!has_v3_POS) {
    stats[['14_visit_3_NEG']][[ARM]] <- ifelse(is.null(stats[['14_visit_3_NEG']][[ARM]]), 0, stats[['14_visit_3_NEG']][[ARM]])
    stats[['14_visit_3_NEG']][[ARM]] <- stats[['14_visit_3_NEG']][[ARM]] + 1
    next
  }
  
  stats[['15_visit_3_POS']][[ARM]] <- ifelse(is.null(stats[['15_visit_3_POS']][[ARM]]), 0, stats[['15_visit_3_POS']][[ARM]])
  stats[['15_visit_3_POS']][[ARM]] <- stats[['15_visit_3_POS']][[ARM]] + 1

  days_between <- abs(as.Date(visit_1_date, format = "%Y-%m-%d") - as.Date(visit_3_date, format = "%Y-%m-%d"))
  days_between <- as.character(days_between)
  
  if (!days_between %in% names(stats[['16_visit_1_to_3']])) {
    stats[['16_visit_1_to_3']][[days_between]] <- 0
  }
  stats[['16_visit_1_to_3']][[days_between]] <- stats[['16_visit_1_to_3']][[days_between]] + 1
  
  has_positive_pcr_before_v3 <- 0
  first_positive_date <- 99999999
  
  for (TESTDATE in sort(names(subjects[[SUBJID]]$tests))) {
    compdate <- as.numeric(gsub("\\D", "", TESTDATE))
    
    if (compdate >= visit_3_compdate) next
    
    for (TESTTYPE in names(subjects[[SUBJID]]$tests[[TESTDATE]])) {
      if (TESTTYPE != 'PCR Central') next
      
      VISIT <- subjects[[SUBJID]]$tests[[TESTDATE]][[TESTTYPE]]$VISIT
      TESTRESULT <- subjects[[SUBJID]]$tests[[TESTDATE]][[TESTTYPE]]$TESTRESULT
      
      if (TESTRESULT == 'POS') {
        first_positive_date <- min(first_positive_date, compdate)
        has_positive_pcr_before_v3 <- 1
      }
    }
  }
  
  SITEID <- subjects[[SUBJID]]$SITEID
  SITEID <- as.character(SITEID)
  
  if (!SITEID %in% names(stats[['19_detection_pre_visit_3_by_site']])) {
    stats[['19_detection_pre_visit_3_by_site']][[SITEID]] <- list()
    stats[['19_detection_pre_visit_3_by_site']][[SITEID]]$total_cases <- 0
    stats[['19_detection_pre_visit_3_by_site']][[SITEID]]$treatment_arm <- list()
  }
  stats[['19_detection_pre_visit_3_by_site']][[SITEID]]$total_cases <- stats[['19_detection_pre_visit_3_by_site']][[SITEID]]$total_cases + 1
  if (!ARM %in% names(stats[['19_detection_pre_visit_3_by_site']][[SITEID]]$treatment_arm)) {
    stats[['19_detection_pre_visit_3_by_site']][[SITEID]]$treatment_arm[[ARM]] <- list()
    stats[['19_detection_pre_visit_3_by_site']][[SITEID]]$treatment_arm[[ARM]]$detected <- 0
    stats[['19_detection_pre_visit_3_by_site']][[SITEID]]$treatment_arm[[ARM]]$undetected <- 0
  }
  
  if (has_positive_pcr_before_v3) {
    stats[['17_detected_pre_visit_3']][[ARM]] <- ifelse(is.null(stats[['17_detected_pre_visit_3']][[ARM]]), 0, stats[['17_detected_pre_visit_3']][[ARM]])
    stats[['17_detected_pre_visit_3']][[ARM]] <- stats[['17_detected_pre_visit_3']][[ARM]] + 1
    stats[['19_detection_pre_visit_3_by_site']][[SITEID]]$treatment_arm[[ARM]]$detected <- stats[['19_detection_pre_visit_3_by_site']][[SITEID]]$treatment_arm[[ARM]]$detected + 1
    next
  }
  
  stats[['18_non_detected_pre_visit_3']][[ARM]] <- ifelse(is.null(stats[['18_non_detected_pre_visit_3']][[ARM]]), 0, stats[['18_non_detected_pre_visit_3']][[ARM]])
  stats[['18_non_detected_pre_visit_3']][[ARM]] <- stats[['18_non_detected_pre_visit_3']][[ARM]] + 1
  stats[['19_detection_pre_visit_3_by_site']][[SITEID]]$treatment_arm[[ARM]]$undetected <- ifelse(is.null(stats[['19_detection_pre_visit_3_by_site']][[SITEID]]$treatment_arm[[ARM]]$undetected), 0, stats[['19_detection_pre_visit_3_by_site']][[SITEID]]$treatment_arm[[ARM]]$undetected)
  stats[['19_detection_pre_visit_3_by_site']][[SITEID]]$treatment_arm[[ARM]]$undetected <- stats[['19_detection_pre_visit_3_by_site']][[SITEID]]$treatment_arm[[ARM]]$undetected + 1
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
for (SITEID in sort(names(stats[['19_detection_pre_visit_3_by_site']]))) {
  bnt_detected <- stats[['19_detection_pre_visit_3_by_site']][[SITEID]]$treatment_arm$BNT162b2$detected
  bnt_undetected <- stats[['19_detection_pre_visit_3_by_site']][[SITEID]]$treatment_arm$BNT162b2$undetected
  placebo_detected <- stats[['19_detection_pre_visit_3_by_site']][[SITEID]]$treatment_arm$Placebo$detected
  placebo_undetected <- stats[['19_detection_pre_visit_3_by_site']][[SITEID]]$treatment_arm$Placebo$undetected
  total_cases <- stats[['19_detection_pre_visit_3_by_site']][[SITEID]]$total_cases
  if (is.null(bnt_detected)) {
    bnt_detected = 0
    bnt_undetected = 0
    stats[['19_detection_pre_visit_3_by_site']][[SITEID]]$treatment_arm$`BNT162b2 Phase 2/3 (30 mcg)` <- list()
    stats[['19_detection_pre_visit_3_by_site']][[SITEID]]$treatment_arm$`BNT162b2 Phase 2/3 (30 mcg)`$detected <- 0
    stats[['19_detection_pre_visit_3_by_site']][[SITEID]]$treatment_arm$`BNT162b2 Phase 2/3 (30 mcg)`$undetected <- 0
  }
  if (is.null(placebo_detected)) {
    placebo_detected = 0
    placebo_undetected = 0
    stats[['19_detection_pre_visit_3_by_site']][[SITEID]]$treatment_arm$Placebo <- list()
    stats[['19_detection_pre_visit_3_by_site']][[SITEID]]$treatment_arm$Placebo$detected <- 0
    stats[['19_detection_pre_visit_3_by_site']][[SITEID]]$treatment_arm$Placebo$undetected <- 0
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
    
    stats[['19_detection_pre_visit_3_by_site']][[SITEID]] <- NULL
  } else {
    stats[['19_detection_pre_visit_3_by_site']][[SITEID]]$treatment_arm$bnt_total <- bnt_total
    stats[['19_detection_pre_visit_3_by_site']][[SITEID]]$treatment_arm$bnt_detection_rate <- bnt_detection_rate
    stats[['19_detection_pre_visit_3_by_site']][[SITEID]]$treatment_arm$placebo_total <- placebo_total
    stats[['19_detection_pre_visit_3_by_site']][[SITEID]]$treatment_arm$placebo_detection_rate <- placebo_detection_rate

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
      stats[['19_detection_pre_visit_3_by_site']][[SITEID]] <- NULL
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
