library(lubridate)
library(dplyr)
library(readr)
library(zoo)
library(ggplot2)

within_days <- 4         # We consider the symptom has been tested if a test has occurred within N days.
rolling_avg <- 3         # We smooth the testing rates for each date based on this rolling average.
target_country <- 'USA'  # The country targeted by the current analysis.
cutoff_date <- 20201114  # The date at which the treatment stops.
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
      if (days_between <= 4) next
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
tests_processed <- tests
symptoms_tested <- list()
stats <- list(total_subjects = 0, total_with_dose_1 = 0, total_with_dose_1_pre_cutoff = 0,
              subjects_with_baseline = 0, subjects_with_neg_baseline = 0, subjects_with_symptoms = 0,
              subjects_in_country = 0)

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
  
  # Retrieving country
  COUNTRY <- randoms[[SUBJID]]$COUNTRY
  if (COUNTRY != target_country) next
  stats$subjects_in_country <- stats$subjects_in_country + 1
  
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
  
  # Retrieving ARM
  ARM <- randoms[[SUBJID]]$ARM
  
  # Reviews if the symptom has been tested within X days
  for (symptdate in sort(names(subjects_symptoms[[SUBJID]]))) {
    REPORTDATE <- subjects_symptoms[[SUBJID]][[symptdate]]
    was_tested <- FALSE
    for (VISIT in names(tests_processed[[SUBJID]])) {
      for (TESTTYPE in names(tests_processed[[SUBJID]][[VISIT]])) {
        if (TESTTYPE != 'PCR Central') next(TESTTYPE)
        TESTDATE <- tests_processed[[SUBJID]][[VISIT]][[TESTTYPE]]$TESTDATE
        days_between <- abs(as.integer(difftime(ymd(REPORTDATE), ymd(TESTDATE), units = "days")))
        if (days_between <= within_days) {
          was_tested <- TRUE
          break
        }
      }
      if (was_tested) break
    }
    
    if (is.null(symptoms_tested[[symptdate]])) {
      symptoms_tested[[symptdate]] <- list(
        `BNT162b2 Phase 2/3 (30 mcg)` = list(Tested = 0, NotTested = 0),
        `Placebo` = list(Tested = 0, NotTested = 0),
        `Population` = 0
      )
    }
    
    if (was_tested) {
      symptoms_tested[[symptdate]][[ARM]][["Tested"]] <- symptoms_tested[[symptdate]][[ARM]][["Tested"]] + 1
    } else {
      symptoms_tested[[symptdate]][[ARM]][["NotTested"]] <- symptoms_tested[[symptdate]][[ARM]][["NotTested"]] + 1
    }
  }
}

cat("total_subjects :", stats$total_subjects, "\n")
cat("total_with_dose_1 :", stats$total_with_dose_1, "\n")
cat("total_with_dose_1_pre_cutoff :", stats$total_with_dose_1_pre_cutoff, "\n")
cat("subjects_with_baseline :", stats$subjects_with_baseline, "\n")
cat("subjects_with_neg_baseline :", stats$subjects_with_neg_baseline, "\n")
cat("subjects_in_country :", stats$subjects_in_country, "\n")
cat("subjects_with_symptoms :", stats$subjects_with_symptoms, "\n")

# Calculate cumulative population up to each date
cumulative_population <- list()
for (date in sort(names(symptoms_tested))) {
  
  if (is.null(cumulative_population[[date]])) {
    cumulative_population[[date]] <- list(
      `Population` = 0
    )
  }
  cumulative_population[[date]] <- sum(sapply(names(symptoms_tested), function(d) {
    if (d <= date && !is.null(symptoms_tested[[d]][["Population"]])) {
      return(as.numeric(symptoms_tested[[d]][["Population"]]))
    } else {
      return(0)
    }
  }))
}

# Merges the cumulative data into the symptoms_tested
for (date in sort(names(symptoms_tested))) {
  symptoms_tested[[date]][["CumulativePopulation"]] <- cumulative_population[[date]]
}

print(symptoms_tested)

# Converts symptoms_tested to a data frame for cross-checks.
symptoms_df <- as.data.frame(do.call(rbind, lapply(names(symptoms_tested), function(symptdate) {
  arms <- symptoms_tested[[symptdate]]
  data.frame(
    symptdate = rep(as.character(symptdate), 2),
    ARM = c("BNT162b2 Phase 2/3 (30 mcg)", "Placebo"),
    CumulativePopulation = rep(cumulative_population[[symptdate]], 2),
    Tested = c(arms[["BNT162b2 Phase 2/3 (30 mcg)"]][["Tested"]], arms[["Placebo"]][["Tested"]]),
    NotTested = c(arms[["BNT162b2 Phase 2/3 (30 mcg)"]][["NotTested"]], arms[["Placebo"]][["NotTested"]])
  )
})))
write.csv(symptoms_df, "symptoms2.csv", row.names = FALSE)

# Opens CSV file for writing
out_file <- 'daily_testing_rates.csv'
out <- file(out_file, "w")
writeLines("report_date,bnt_test,bnt_no_test,bnt_total,bnt_percent,placebo_test,placebo_no_test,placebo_total,placebo_percent,cumulative_population,cumulated_percent", out)

# Iterates over the symptoms_tested list
for (report_date in sort(names(symptoms_tested))) {
  daily_stats <- c(bnt_test = 0, bnt_no_test = 0, placebo_test = 0, placebo_no_test = 0)
  cumulative_population <- symptoms_tested[[report_date]][["CumulativePopulation"]]
  
  for (past_date in sort(names(symptoms_tested))) {
    if (past_date > report_date) next
    days_between <- abs(as.integer(difftime(ymd(report_date), ymd(past_date), units = "days")))
    if (days_between > rolling_avg) next
    arms <- symptoms_tested[[past_date]]
    
    if (!is.null(arms)) {
      bnt_arms <- arms$`BNT162b2 Phase 2/3 (30 mcg)`
      placebo_arms <- arms$Placebo
      
      if (!is.null(bnt_arms)) {
        daily_stats['bnt_test'] <- daily_stats['bnt_test'] + bnt_arms$Tested
        daily_stats['bnt_no_test'] <- daily_stats['bnt_no_test'] + bnt_arms$NotTested
      }
      
      if (!is.null(placebo_arms)) {
        daily_stats['placebo_test'] <- daily_stats['placebo_test'] + placebo_arms$Tested
        daily_stats['placebo_no_test'] <- daily_stats['placebo_no_test'] + placebo_arms$NotTested
      }
    }
  }
  
  bnt_total <- daily_stats['bnt_test'] + daily_stats['bnt_no_test']
  placebo_total <- daily_stats['placebo_test'] + daily_stats['placebo_no_test']
  bnt_percent <- ifelse(bnt_total > 0, round(daily_stats['bnt_test'] * 100 / bnt_total, 2), 0)
  placebo_percent <- ifelse(placebo_total > 0, round(daily_stats['placebo_test'] * 100 / placebo_total, 2), 0)
  cumulated_tested <- daily_stats['bnt_test'] + daily_stats['placebo_test']
  cumulated_total <- placebo_total + bnt_total
  cumulated_percent <- ifelse(cumulated_total > 0, round(cumulated_tested * 100 / cumulated_total, 2), 0)
  
  # Converts report_date to YYYY-MM-DD format
  formatted_report_date <- format(ymd(report_date), "%Y-%m-%d")
  
  # Writes to CSV
  writeLines(paste(formatted_report_date, daily_stats['bnt_test'], daily_stats['bnt_no_test'], bnt_total, bnt_percent,
                   daily_stats['placebo_test'], daily_stats['placebo_no_test'], placebo_total, placebo_percent,
                   cumulative_population, cumulated_percent, sep = ","), out)
}
close(out)

# Reads the CSV file
df <- read.csv("daily_testing_rates.csv")
print(df)

# Find the max value for placebo_total and bnt_total
max_total <- max(max(df$placebo_total), max(df$bnt_total)) + 200

# Find the max value for placebo_percent and bnt_percent
max_percent <- 100

# Calculate the scaling factor for the left Y axis based on max_total and max_percent
scaling_factor_right <- max_total / max_percent
print(max_total)
print(max_percent)
print(scaling_factor_right)

# Converts report_date to Date type
df$report_date <- ymd(df$report_date)

# Creates the plot
p <- ggplot(df, aes(x = report_date)) +
  geom_col(aes(y = bnt_total / scaling_factor_right, fill = "BNT Total"), alpha = 0.5) +
  geom_col(aes(y = placebo_total / scaling_factor_right, fill = "Placebo Total"), alpha = 0.5) +
  geom_line(aes(y = bnt_percent, color = "BNT Percent"), size = 1.5) +
  geom_line(aes(y = placebo_percent, color = "Placebo Percent"), size = 1.5) +
  scale_y_continuous(
    name = "Symptoms Tested Percent",
    sec.axis = sec_axis(~.*scaling_factor_right, name = "Total Symptoms Reported")
  ) +
  labs(
    x = "Report Date",
    color = "Legend",
    fill = "Legend",
    title = "C4591001 - USA - PCR Testing on negative at baseline subjects reporting Symptoms, from Dose 1 to November 14, 2020"
  ) +
  scale_color_manual(
    values = c("BNT Percent" = "#6082B6", "Placebo Percent" = "#AA4A44")
  ) +
  scale_fill_manual(
    values = c("BNT Total" = "#7393B3", "Placebo Total" = "#E97451")
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 18),
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 18),
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 18),
    axis.text.x = element_text(angle = 90, hjust = 1) 
  )

# Prints the plot
print(p)

# Calculates the scaling factor for cumulative_population
max_cumulative_population <- max(df$cumulative_population)
scaling_factor_left <- 100 / max_cumulative_population

# Converts report_date to numeric representation
df$numeric_date <- as.numeric(as.Date(df$report_date))

# Adds the dashed line for cumulative_population
p <- p + 
  geom_line(aes(y = cumulative_population * scaling_factor_left, linetype = "Cumulative Population"), 
            color = "black", size = 1, linetype = "dashed") +
  geom_text(data = df[df$numeric_date %% 7 == 0, ], 
            aes(x = numeric_date, y = cumulative_population * scaling_factor_left, 
                label = cumulative_population),
            vjust = -0.5, hjust = 0.5, size = 4, color = "black") +
  scale_x_continuous(breaks = df$numeric_date[df$numeric_date %% 7 == 0],
                     labels = df$report_date[df$numeric_date %% 7 == 0])

# Prints the updated plot
print(p)



