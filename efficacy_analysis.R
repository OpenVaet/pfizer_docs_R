library(lubridate)
library(dplyr)
library(readr)
library(zoo)
library(ggplot2)

within_days <- 4         # We consider the symptom has been tested if a test has occurred within N days.
cutoff_date <- 20210331  # The date at which the treatment stops.
nullify_v1_pcr <- FALSE  # Defines if we nullify the V1 PCR tests (TRUE or FALSE)

symptoms_file <- 'covid_symptoms_accross_datasets.csv'
randoms_file <- 'phase_3_randomized_pop.csv'
tests_file <- 'subjects_test_data.csv'

# Reads symptoms file
symptoms_data <- read_csv(symptoms_file, col_types = cols(.default = "c"))
symptoms <- list()
symptoms_data <- symptoms_data %>%
  mutate(compdate = gsub("\\D", "", REPORTDATE))
for (i in seq_len(nrow(symptoms_data))) {
  row <- symptoms_data[i,]
  SUBJID <- as.character(row$SUBJID)
  compdate <- as.character(row$compdate)
  ARM <- row$ARM
  REPORTDATE <- row$REPORTDATE
  
  if (!is.null(symptoms[[SUBJID]])) {
    symptoms[[SUBJID]][[compdate]] <- list(ARM = ARM, REPORTDATE = REPORTDATE)
  } else {
    symptoms[[SUBJID]] <- list()
    symptoms[[SUBJID]][[compdate]] <- list(ARM = ARM, REPORTDATE = REPORTDATE)
  }
}

# Filters symptoms data
subjects_symptoms <- list()
for (SUBJID in sort(names(symptoms))) {
  former_date <- NULL
  for (compdate in sort(names(symptoms[[SUBJID]]))) {
    REPORTDATE <- symptoms[[SUBJID]][[compdate]]$REPORTDATE
    ARM <- symptoms[[SUBJID]][[compdate]]$ARM
    if (!is.null(former_date)) {
      days_between <- abs(as.integer(difftime(ymd(REPORTDATE), ymd(former_date), units = "days")))
      if (days_between <= within_days) next
    }
    if (as.integer(compdate) > cutoff_date) next
    former_date <- REPORTDATE
    if (!is.null(subjects_symptoms[[SUBJID]])) {
      subjects_symptoms[[SUBJID]][[compdate]] <- REPORTDATE
    } else {
      subjects_symptoms[[SUBJID]] <- list()
      subjects_symptoms[[SUBJID]][[compdate]] <- REPORTDATE
    }
  }
}
print(subjects_symptoms)

# Reads randomization file
randoms_data <- read_csv(randoms_file, col_types = cols(.default = "c"))
randoms <- list()
for (i in seq_len(nrow(randoms_data))) {
  row <- randoms_data[i,]
  SUBJID <- as.character(row$SUBJID)
  randoms[[SUBJID]] <- row
}
print(randoms)

# Reads tests file
tests_data <- read_csv(tests_file, col_types = cols(.default = "c"))
tests <- list()
for (i in seq_len(nrow(tests_data))) {
  row <- tests_data[i, ]
  SUBJID <- as.character(row$SUBJID)
  VISIT <- row$VISIT
  TESTTYPE <- row$TESTTYPE
  TESTRESULT <- row$TESTRESULT
  TESTDATE <- row$TESTDATE
  COMPDATE <- format(ymd(TESTDATE), "%Y%m%d")
  
  if (TESTTYPE == 'PCR Local') next
  if (COMPDATE > cutoff_date) next
  
  if (!is.null(tests[[SUBJID]])) {
    if (!is.null(tests[[SUBJID]][[VISIT]])) {
      if (!is.null(tests[[SUBJID]][[VISIT]][[TESTTYPE]])) {
        if (tests[[SUBJID]][[VISIT]][[TESTTYPE]]$TESTRESULT != TESTRESULT) {
          stop("Test results do not match")
        }
      } else {
        tests[[SUBJID]][[VISIT]][[TESTTYPE]] <- list(TESTRESULT = TESTRESULT, TESTDATE = TESTDATE)
      }
    } else {
      tests[[SUBJID]][[VISIT]] <- list()
      tests[[SUBJID]][[VISIT]][[TESTTYPE]] <- list(TESTRESULT = TESTRESULT, TESTDATE = TESTDATE)
    }
  } else {
    tests[[SUBJID]] <- list()
    tests[[SUBJID]][[VISIT]] <- list()
    tests[[SUBJID]][[VISIT]][[TESTTYPE]] <- list(TESTRESULT = TESTRESULT, TESTDATE = TESTDATE)
  }
}

# Calculates the testing rates on symptoms reported
sites_stats <- list()
tests_processed <- tests
symptoms_tested <- list()
stats <- list(total_subjects = 0, total_with_dose_1 = 0, total_with_dose_1_pre_cutoff = 0,
              subjects_with_baseline = 0, subjects_with_neg_baseline = 0, subjects_with_symptoms = 0)

for (SUBJID in sort(names(randoms))) {
  stats$total_subjects <- stats$total_subjects + 1
  VAX101DT <- randoms[[SUBJID]]$VAX101DT
  if (is.na(VAX101DT)) next
  VAX101DTcp <- gsub("\\D", "", VAX101DT)
  stats$total_with_dose_1 <- stats$total_with_dose_1 + 1
  if (as.integer(VAX101DTcp) > cutoff_date) next
  stats$total_with_dose_1_pre_cutoff <- stats$total_with_dose_1_pre_cutoff + 1
  
  # Covid at baseline
  v1_pcr <- tests_processed[[SUBJID]][['V1_DAY1_VAX1_L']][['N-Binding']]$TESTRESULT
  v1_nbinding <- tests_processed[[SUBJID]][['V1_DAY1_VAX1_L']][['PCR Central']]$TESTRESULT
  
  if (is.null(v1_pcr) | is.null(v1_nbinding)) next
  stats$subjects_with_baseline <- stats$subjects_with_baseline + 1
  if (v1_pcr != 'NEG' || v1_nbinding != 'NEG') next
  stats$subjects_with_neg_baseline <- stats$subjects_with_neg_baseline + 1
  if (nullify_v1_pcr) {
    tests_processed[[SUBJID]][['V1_DAY1_VAX1_L']] <- NULL
  }
  
  # Retrieving ARM
  ARM <- randoms[[SUBJID]]$ARM
  
  # Retrieving country
  SITEID <- randoms[[SUBJID]]$SITEID
  COUNTRY <- randoms[[SUBJID]]$COUNTRY
  if (is.null(sites_stats[[SITEID]])) {
    sites_stats[[SITEID]] <- list(
      `BNT162b2 Phase 2/3 (30 mcg)` = list(NegativeAtBaseline = 0, Positive = 0),
      `Placebo` = list(NegativeAtBaseline = 0, Positive = 0)
    )
  }
  sites_stats[[SITEID]][[ARM]][["NegativeAtBaseline"]] <- sites_stats[[SITEID]][[ARM]][["NegativeAtBaseline"]] + 1
  
  # Increments subjects screened on this date.
  RFICDT <- randoms[[SUBJID]]$RFICDT
  screendate <- format(ymd(RFICDT), "%Y%m%d")
  if (is.null(symptoms_tested[[screendate]])) {
    symptoms_tested[[screendate]] <- list(
      `BNT162b2 Phase 2/3 (30 mcg)` = list(Tested = 0, NotTested = 0),
      `Placebo` = list(Tested = 0, NotTested = 0),
      `Population` = 0
    )
  }
  symptoms_tested[[screendate]][["Population"]] <- symptoms_tested[[screendate]][["Population"]] + 1
  
  # Sustains only subjects who reported symptoms at some point during the study
  if (is.null(subjects_symptoms[[SUBJID]])) next
  stats$subjects_with_symptoms <- stats$subjects_with_symptoms + 1
  
  # Reviews if the symptom has been tested within X days
  has_positive <- FALSE
  for (symptdate in sort(names(subjects_symptoms[[SUBJID]]))) {
    REPORTDATE <- subjects_symptoms[[SUBJID]][[symptdate]]
    was_tested <- FALSE
    for (VISIT in names(tests_processed[[SUBJID]])) {
      for (TESTTYPE in names(tests_processed[[SUBJID]][[VISIT]])) {
        if (TESTTYPE != 'PCR Central') next(TESTTYPE)
        TESTDATE <- tests_processed[[SUBJID]][[VISIT]][[TESTTYPE]]$TESTDATE
        TESTRESULT <- tests_processed[[SUBJID]][[VISIT]][[TESTTYPE]]$TESTRESULT
        days_between <- abs(as.integer(difftime(ymd(REPORTDATE), ymd(TESTDATE), units = "days")))
        if (days_between <= within_days) {
          was_tested <- TRUE
          if (TESTRESULT == 'POS') {
            has_positive <- TRUE
            sites_stats[[SITEID]][[ARM]][["Positive"]] <- sites_stats[[SITEID]][[ARM]][["Positive"]] + 1
          }
          break
        }
      }
      if (was_tested) break
      if (has_positive) break
    }
    if (has_positive) break
  }
}

cat("total_subjects :", stats$total_subjects, "\n")
cat("total_with_dose_1 :", stats$total_with_dose_1, "\n")
cat("total_with_dose_1_pre_cutoff :", stats$total_with_dose_1_pre_cutoff, "\n")
cat("subjects_with_baseline :", stats$subjects_with_baseline, "\n")
cat("subjects_with_neg_baseline :", stats$subjects_with_neg_baseline, "\n")
cat("subjects_with_symptoms :", stats$subjects_with_symptoms, "\n")

print(sites_stats)

# Flattening the structure and writing to CSV
output <- data.frame(
  SITEID = character(),
  PLACEBONEGBASELINE = integer(),
  PLACEBOPOS = integer(),
  BNTNEGBASELINE = integer(),
  BNTPOS = integer()
)

for (site_id in names(sites_stats)) {
  placebonb <- sites_stats[[site_id]][['Placebo']][['NegativeAtBaseline']]
  placebopos <- sites_stats[[site_id]][['Placebo']][['Positive']]
  bntnb <- sites_stats[[site_id]][['BNT162b2 Phase 2/3 (30 mcg)']][['NegativeAtBaseline']]
  bntpos <- sites_stats[[site_id]][['BNT162b2 Phase 2/3 (30 mcg)']][['Positive']]
  
  output <- rbind(output, data.frame(
    SITEID = site_id,
    PLACEBONEGBASELINE = placebonb,
    PLACEBOPOS = placebopos,
    BNTNEGBASELINE = bntnb,
    BNTPOS = bntpos
  ))
}

write_csv(output, 'sites_symptomatic_positive_pcrs.csv')




# Calculates the testing rates on symptoms reported
sites_stats <- list()
tests_processed <- tests
symptoms_tested <- list()
stats <- list(total_subjects = 0, total_with_dose_1 = 0, total_with_dose_1_pre_cutoff = 0,
              subjects_with_baseline = 0, subjects_with_neg_baseline = 0, subjects_with_symptoms = 0)

for (SUBJID in sort(names(randoms))) {
  stats$total_subjects <- stats$total_subjects + 1
  VAX101DT <- randoms[[SUBJID]]$VAX101DT
  if (is.na(VAX101DT)) next
  VAX101DTcp <- gsub("\\D", "", VAX101DT)
  stats$total_with_dose_1 <- stats$total_with_dose_1 + 1
  if (as.integer(VAX101DTcp) > cutoff_date) next
  stats$total_with_dose_1_pre_cutoff <- stats$total_with_dose_1_pre_cutoff + 1
  
  # Covid at baseline
  v1_pcr <- tests_processed[[SUBJID]][['V1_DAY1_VAX1_L']][['N-Binding']]$TESTRESULT
  v1_nbinding <- tests_processed[[SUBJID]][['V1_DAY1_VAX1_L']][['PCR Central']]$TESTRESULT
  
  if (is.null(v1_pcr) | is.null(v1_nbinding)) next
  stats$subjects_with_baseline <- stats$subjects_with_baseline + 1
  if (v1_pcr != 'NEG' || v1_nbinding != 'NEG') next
  stats$subjects_with_neg_baseline <- stats$subjects_with_neg_baseline + 1
  if (nullify_v1_pcr) {
    tests_processed[[SUBJID]][['V1_DAY1_VAX1_L']] <- NULL
  }
  
  # Retrieving ARM
  ARM <- randoms[[SUBJID]]$ARM
  
  # Retrieving country
  SITEID <- randoms[[SUBJID]]$SITEID
  COUNTRY <- randoms[[SUBJID]]$COUNTRY
  if (is.null(sites_stats[[SITEID]])) {
    sites_stats[[SITEID]] <- list(
      `BNT162b2 Phase 2/3 (30 mcg)` = list(NegativeAtBaseline = 0, Positive = 0),
      `Placebo` = list(NegativeAtBaseline = 0, Positive = 0)
    )
  }
  sites_stats[[SITEID]][[ARM]][["NegativeAtBaseline"]] <- sites_stats[[SITEID]][[ARM]][["NegativeAtBaseline"]] + 1
  
  # Increments subjects screened on this date.
  RFICDT <- randoms[[SUBJID]]$RFICDT
  screendate <- format(ymd(RFICDT), "%Y%m%d")
  if (is.null(symptoms_tested[[screendate]])) {
    symptoms_tested[[screendate]] <- list(
      `BNT162b2 Phase 2/3 (30 mcg)` = list(Tested = 0, NotTested = 0),
      `Placebo` = list(Tested = 0, NotTested = 0),
      `Population` = 0
    )
  }
  symptoms_tested[[screendate]][["Population"]] <- symptoms_tested[[screendate]][["Population"]] + 1
  
  # Sustains only subjects who reported symptoms at some point during the study
  if (is.null(subjects_symptoms[[SUBJID]])) next
  stats$subjects_with_symptoms <- stats$subjects_with_symptoms + 1
  
  # Reviews if the symptom has been tested within X days
  has_positive <- FALSE
  for (symptdate in sort(names(subjects_symptoms[[SUBJID]]))) {
    REPORTDATE <- subjects_symptoms[[SUBJID]][[symptdate]]
    was_tested <- FALSE
    for (VISIT in names(tests_processed[[SUBJID]])) {
      for (TESTTYPE in names(tests_processed[[SUBJID]][[VISIT]])) {
        if (TESTTYPE != 'PCR Central') next(TESTTYPE)
        TESTDATE <- tests_processed[[SUBJID]][[VISIT]][[TESTTYPE]]$TESTDATE
        TESTRESULT <- tests_processed[[SUBJID]][[VISIT]][[TESTTYPE]]$TESTRESULT
        days_between <- abs(as.integer(difftime(ymd(REPORTDATE), ymd(TESTDATE), units = "days")))
        if (days_between <= within_days) {
          was_tested <- TRUE
          if (TESTRESULT == 'POS') {
            has_positive <- TRUE
            sites_stats[[SITEID]][[ARM]][["Positive"]] <- sites_stats[[SITEID]][[ARM]][["Positive"]] + 1
          }
          break
        }
      }
      if (was_tested) break
      if (has_positive) break
    }
    if (has_positive) break
  }
}

cat("total_subjects :", stats$total_subjects, "\n")
cat("total_with_dose_1 :", stats$total_with_dose_1, "\n")
cat("total_with_dose_1_pre_cutoff :", stats$total_with_dose_1_pre_cutoff, "\n")
cat("subjects_with_baseline :", stats$subjects_with_baseline, "\n")
cat("subjects_with_neg_baseline :", stats$subjects_with_neg_baseline, "\n")
cat("subjects_with_symptoms :", stats$subjects_with_symptoms, "\n")

print(sites_stats)

# Flattening the structure and writing to CSV
output <- data.frame(
  SITEID = character(),
  PLACEBONEGBASELINE = integer(),
  PLACEBOPOS = integer(),
  BNTNEGBASELINE = integer(),
  BNTPOS = integer()
)

for (site_id in names(sites_stats)) {
  placebonb <- sites_stats[[site_id]][['Placebo']][['NegativeAtBaseline']]
  placebopos <- sites_stats[[site_id]][['Placebo']][['Positive']]
  bntnb <- sites_stats[[site_id]][['BNT162b2 Phase 2/3 (30 mcg)']][['NegativeAtBaseline']]
  bntpos <- sites_stats[[site_id]][['BNT162b2 Phase 2/3 (30 mcg)']][['Positive']]
  
  output <- rbind(output, data.frame(
    SITEID = site_id,
    PLACEBONEGBASELINE = placebonb,
    PLACEBOPOS = placebopos,
    BNTNEGBASELINE = bntnb,
    BNTPOS = bntpos
  ))
}

write_csv(output, 'sites_symptomatic_positive_pcrs.csv')

