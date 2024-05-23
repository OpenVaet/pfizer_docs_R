library(lubridate)
library(dplyr)
library(readr)
library(zoo)

within_days <- 4  # We consider the symptom has been tested if a test has occurred within N days.
rolling_avg <- 3  # We smooth the testing rates for each date based on this rolling average.

symptoms_file <- 'covid_symptoms_accross_datasets.csv'
randoms_file <- 'phase_3_randomized_pop.csv'
tests_file <- 'subjects_test_data.csv'

# Read tests file
tests_data <- read_csv(tests_file, col_types = cols(.default = "c"))
tests <- list()
visits_counts <- list()

for (i in seq_len(nrow(tests_data))) {
  row <- tests_data[i, ]
  SUBJID <- as.character(row$SUBJID)
  VISIT <- row$VISIT
  TESTTYPE <- row$TESTTYPE
  TESTRESULT <- row$TESTRESULT
  TESTDATE <- row$TESTDATE
  
  if (TESTTYPE == 'PCR Local') next
  
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
  
  if (!is.null(visits_counts[[VISIT]])) {
    if (!is.null(visits_counts[[VISIT]][[TESTTYPE]])) {
      visits_counts[[VISIT]][[TESTTYPE]] <- visits_counts[[VISIT]][[TESTTYPE]] + 1
    } else {
      visits_counts[[VISIT]][[TESTTYPE]] <- 1
    }
  } else {
    visits_counts[[VISIT]] <- list()
    visits_counts[[VISIT]][[TESTTYPE]] <- 1
  }
}

# Debugging print to inspect the structure of the tests list
print(tests)

# Print the counts for each TESTTYPE on each VISIT
print(visits_counts)


# Debugging print to inspect the structure of the tests list
print(visits_counts)

# Count the total entries for each TESTTYPE on each VISIT
visit_testtype_counts <- list()

for (subject in tests) {
  for (visit in names(subject)) {
    for (testtype in names(subject[[visit]])) {
      if (!is.null(visit_testtype_counts[[visit]])) {
        if (!is.null(visit_testtype_counts[[visit]][[testtype]])) {
          visit_testtype_counts[[visit]][[testtype]] <- visit_testtype_counts[[visit]][[testtype]] + 1
        } else {
          visit_testtype_counts[[visit]][[testtype]] <- 1
        }
      } else {
        visit_testtype_counts[[visit]] <- list()
        visit_testtype_counts[[visit]][[testtype]] <- 1
      }
    }
  }
}

# Print the counts for each TESTTYPE on each VISIT
print(visit_testtype_counts)



print(tests)


# Subroutine to calculate days between two dates
calculate_days_between <- function(REPORTDATE, former_date) {
  as.integer(difftime(ymd(REPORTDATE), ymd(former_date), units = "days"))
}

symptoms_tested <- list()
stats <- list(total_subjects = 0, total_with_dose_1 = 0, total_with_dose_1_pre_cutoff = 0,
              subjects_with_baseline = 0, subjects_with_neg_baseline = 0, subjects_with_symptoms = 0,
              subjects_with_symptoms_in_country = 0)
for (SUBJID in sort(names(randoms))) {
  stats$total_subjects <- stats$total_subjects + 1
  VAX101DT <- randoms[[SUBJID]]$VAX101DT
  if (is.na(VAX101DT)) next
  VAX101DTcp <- gsub("\\D", "", VAX101DT)
  stats$total_with_dose_1 <- stats$total_with_dose_1 + 1
  if (as.integer(VAX101DTcp) > 20201114) next
  stats$total_with_dose_1_pre_cutoff <- stats$total_with_dose_1_pre_cutoff + 1
  
  # Covid at baseline
  # print(paste('SUBJID : ', SUBJID))
  # print(tests[[SUBJID]])
  v1_pcr <- tests[[SUBJID]][['V1_DAY1_VAX1_L']][['N-Binding']]$TESTRESULT
  v1_nbinding <- tests[[SUBJID]][['V1_DAY1_VAX1_L']][['PCR Central']]$TESTRESULT
  
  # break #### DEBUG
  # print(paste('v1_pcr : ', v1_pcr))
  # print(paste('v1_nbinding : ', v1_nbinding))
  if (is.null(v1_pcr) | is.null(v1_nbinding)) next
  stats$subjects_with_baseline <- stats$subjects_with_baseline + 1
  if (v1_pcr != 'NEG' && v1_nbinding != 'NEG') next
  stats$subjects_with_neg_baseline <- stats$subjects_with_neg_baseline + 1
  tests[[SUBJID]][['V1_DAY1_VAX1_L']] <- NULL
  
  # Sustains only subjects who reported symptoms at some point during the study
  if (is.null(subjects_symptoms[[SUBJID]])) next
  stats$subjects_with_symptoms <- stats$subjects_with_symptoms + 1
  
  # Retrieving country
  COUNTRY <- randoms[[SUBJID]]$COUNTRY
  if (COUNTRY != 'ARG') next
  stats$subjects_with_symptoms_in_country <- stats$subjects_with_symptoms_in_country + 1
  
  # Retrieving ARM
  ARM <- randoms[[SUBJID]]$ARM
  
  # Reviews if the symptom has been tested within X days
  for (symptdate in sort(names(subjects_symptoms[[SUBJID]]))) {
    REPORTDATE <- subjects_symptoms[[SUBJID]][[symptdate]]
    
    was_tested <- FALSE
    for (VISIT in names(tests[[SUBJID]])) {
      for (TESTTYPE in names(tests[[SUBJID]][[VISIT]])) {
        if (TESTTYPE != 'PCR Central') next
        TESTDATE <- tests[[SUBJID]][[VISIT]][[TESTTYPE]]$TESTDATE
        days_between <- calculate_days_between(REPORTDATE, TESTDATE)
        if (days_between <= within_days) {
          was_tested <- TRUE
          break
        }
      }
      if (was_tested) break
    }
    if (!is.null(symptoms_tested[[symptdate]])) {
      if (!is.null(symptoms_tested[[symptdate]]$ARMs)) {
        if (!is.null(symptoms_tested[[symptdate]]$ARMs[[ARM]])) {
          if (was_tested) {
            if (!is.null(symptoms_tested[[symptdate]]$ARMs[[ARM]][["TRUE"]])) {
              symptoms_tested[[symptdate]]$ARMs[[ARM]][["TRUE"]] <- symptoms_tested[[symptdate]]$ARMs[[ARM]][["TRUE"]] + 1
            } else {
              symptoms_tested[[symptdate]]$ARMs[[ARM]][["TRUE"]] <- 1
            }
          } else {
            if (!is.null(symptoms_tested[[symptdate]]$ARMs[[ARM]][["FALSE"]])) {
              symptoms_tested[[symptdate]]$ARMs[[ARM]][["FALSE"]] <- symptoms_tested[[symptdate]]$ARMs[[ARM]][["FALSE"]] + 1
            } else {
              symptoms_tested[[symptdate]]$ARMs[[ARM]][["FALSE"]] <- 1
            }
          }
        } else {
          symptoms_tested[[symptdate]]$ARMs[[ARM]] <- list()
          symptoms_tested[[symptdate]]$ARMs[[ARM]][["TRUE"]] <- ifelse(was_tested, 1, 0)
          symptoms_tested[[symptdate]]$ARMs[[ARM]][["FALSE"]] <- ifelse(!was_tested, 1, 0)
        }
      } else {
        symptoms_tested[[symptdate]]$ARMs <- list()
        symptoms_tested[[symptdate]]$ARMs[[ARM]] <- list()
        symptoms_tested[[symptdate]]$ARMs[[ARM]][["TRUE"]] <- ifelse(was_tested, 1, 0)
        symptoms_tested[[symptdate]]$ARMs[[ARM]][["FALSE"]] <- ifelse(!was_tested, 1, 0)
      }
    } else {
      symptoms_tested[[symptdate]] <- list(REPORTDATE = REPORTDATE, 
                                           ARMs = list(ARM = list("TRUE" = ifelse(was_tested, 1, 0), 
                                                                  "FALSE" = ifelse(!was_tested, 1, 0))))
    }
  }
}

cat("total_subjects :", stats$total_subjects, "\n")
cat("total_with_dose_1 :", stats$total_with_dose_1, "\n")
cat("total_with_dose_1_pre_cutoff :", stats$total_with_dose_1_pre_cutoff, "\n")
cat("subjects_with_baseline :", stats$subjects_with_baseline, "\n")
cat("subjects_with_neg_baseline :", stats$subjects_with_neg_baseline, "\n")
cat("subjects_with_symptoms :", stats$subjects_with_symptoms, "\n")
cat("subjects_with_symptoms_in_country :", stats$subjects_with_symptoms_in_country, "\n")

print(symptoms_tested)

# Initialize a list to store the rolling average results
rolling_avg_results <- list()

# Loop through each report date to calculate the rolling average
for (reportcp in sort(as.integer(names(symptoms_tested)))) {
  REPORTDATE <- symptoms_tested[[as.character(reportcp)]]$REPORTDATE
  print(paste('reportcp : ', reportcp))
  print(paste('REPORTDATE : ', REPORTDATE))
  
  # Initialize the daily statistics for the current report date
  daily_stats <- list(bnt_test = 0, bnt_no_test = 0, placebo_test = 0, placebo_no_test = 0)
  
  # Loop through past dates to accumulate statistics within the rolling average period
  for (pastcp in sort(as.integer(names(symptoms_tested)), decreasing = TRUE)) {
    if (pastcp > reportcp) next
    
    past_date <- symptoms_tested[[as.character(pastcp)]]$REPORTDATE
    days_between <- (as.numeric(difftime(REPORTDATE, past_date, units = "days")))
    
    if (days_between > rolling_avg) next
    print(paste('past_date : ', past_date))
    print(paste('days_between : ', days_between))
    
    # Safely extract the values using the coalesce function to handle missing data
    bnt_test <- coalesce(symptoms_tested[[as.character(pastcp)]]$arms[['BNT162b2 Phase 2/3 (30 mcg)']][['1']], 0)
    bnt_no_test <- coalesce(symptoms_tested[[as.character(pastcp)]]$arms[['BNT162b2 Phase 2/3 (30 mcg)']][['0']], 0)
    placebo_test <- coalesce(symptoms_tested[[as.character(pastcp)]]$arms[['Placebo']][['1']], 0)
    placebo_no_test <- coalesce(symptoms_tested[[as.character(pastcp)]]$arms[['Placebo']][['0']], 0)
    
    # Accumulate the statistics
    daily_stats$bnt_test <- daily_stats$bnt_test + bnt_test
    daily_stats$bnt_no_test <- daily_stats$bnt_no_test + bnt_no_test
    daily_stats$placebo_test <- daily_stats$placebo_test + placebo_test
    daily_stats$placebo_no_test <- daily_stats$placebo_no_test + placebo_no_test
  }
  
  # Store the accumulated statistics in the rolling average results list
  rolling_avg_results[[as.character(reportcp)]] <- daily_stats
}

# Print or use the rolling average results as needed
# print(rolling_avg_results)
