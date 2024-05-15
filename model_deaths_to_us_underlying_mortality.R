# Loads required packages
library(readr)
library(tidyverse)
library(jsonlite)
library(ggplot2)
library(lubridate)
library(dplyr)

us_mortality_file <- "us_mortality/daily_chance_to_die_by_states.csv"
us_simu_mort_file <- "us_mortality/mort_by_date.csv"

# If the daily mortality hasn't been calculated yet, proceeds from Wonder data.
if (!file.exists(us_mortality_file)) {
  # Reads the text file
  cdc_data <- read_delim("us_mortality/Underlying Cause of Death, 2018-2022, Single Race.txt", 
                         delim = "\t", 
                         escape_double = FALSE, 
                         trim_ws = TRUE,
                         n_max = Inf,
                         comment = "---")
  
  cdc_df <- suppressWarnings(cdc_data %>%
                               # Renames the columns
                               rename(state = "State",
                                      state_code = "State Code",
                                      age_group = "Five-Year Age Groups",
                                      age_group_code = "Five-Year Age Groups Code",
                                      year = "Year",
                                      year_code = "Year Code",
                                      month = "Month",
                                      month_code = "Month Code",
                                      deaths = "Deaths",
                                      population = "Population",
                                      crude_rate = "Crude Rate") %>%
                               # Converts relevant columns to appropriate data types
                               mutate(state_code = as.integer(state_code),
                                      age_group_code = case_when(
                                        age_group_code %in% c("1", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85-89", "90-94", "95-99") ~ as.integer(age_group_code),
                                        TRUE ~ NA_integer_
                                      ),
                                      year_code = as.integer(year_code),
                                      deaths = as.integer(deaths),
                                      population = as.integer(population),
                                      crude_rate = as.numeric(crude_rate)) %>%
                               mutate(across(where(is.numeric), ~replace_na(., 0))))
  # Converts month column from "YYYY/MM" to "MM"
  cdc_df$month_code <- substr(cdc_df$month_code, 6, 7)
  print(cdc_df)
  
  # Reads the yearly population text file
  cdc_pop_data <- read_delim("us_mortality/Single-Race Population Estimates 2020-2022.txt", 
                         delim = "\t", 
                         escape_double = FALSE, 
                         trim_ws = TRUE,
                         n_max = Inf,
                         comment = "---")
  print(cdc_pop_data)
  
  cdc_pop_df <- suppressWarnings(cdc_pop_data %>%
                               # Renames the columns
                               rename(state = "State",
                                      state_code = "State Code",
                                      age_group = "Age Group",
                                      age_group_code = "Age Group Code",
                                      year = "Yearly July 1st Estimates",
                                      year_code = "Yearly July 1st Estimates Code",
                                      population = "Population") %>%
                               # Converts relevant columns to appropriate data types
                               mutate(state_code = as.integer(state_code),
                                      age_group_code = case_when(
                                        age_group_code %in% c("1", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85-89", "90-94", "95-99") ~ as.integer(age_group_code),
                                        TRUE ~ NA_integer_
                                      ),
                                      year_code = as.integer(year_code),
                                      population = as.integer(population)) %>%
                               mutate(across(where(is.numeric), ~replace_na(., 0))))
  print(cdc_pop_df)
  print(cdc_df)
  
  # Converts age groups above or equal to "85-89 years" to "85+ years" in cdc_df
  cdc_df <- cdc_df %>%
    mutate(age_group = case_when(
      age_group %in% c("85-89 years", "90-94 years", "95-99 years", "100+ years") ~ "85+ years",
      TRUE ~ age_group
    )) %>%
    group_by(state, state_code, age_group, age_group_code, year, year_code, month, month_code) %>%
    summarize(deaths = sum(deaths)) %>%
    ungroup()
  
  print(unique(cdc_pop_df$state))
  print(unique(cdc_df$state))
  
  
  # Creates an empty data frame to store the results
  result_df <- data.frame()
  
  # Iterates over each state, year, month, and age group in cdc_df
  for (st in unique(cdc_df$state)) {
    if (is.na(st) | st == 0 | st == 'Alaska') {
      next
    }
    for (y in unique(cdc_df$year)) {
      if (y == 0) {
        next
      }
      for (m in unique(cdc_df$month_code)) {
        for (ag in unique(cdc_df$age_group)) {
          if (is.na(ag) | ag == 'Not Stated' | ag == '< 1 year' | ag == '1-4 years' | ag == '5-9 years') {
            next
          }
          # Filters cdc_df to get the deaths for the current state, year, month_code, and age group
          deaths <- cdc_df %>% 
            filter(state == st, year == y, month_code == m, age_group == ag) %>% 
            pull(deaths)
          
          # If deaths is empty, set it to 0
          if (length(deaths) == 0) {
            deaths <- 0
          }
          
          # Filters cdc_pop_df to get the population for the current state, year, and age group
          population <- cdc_pop_df %>% 
            filter(state == st, year == y, age_group == ag) %>% 
            pull(population)
          
          # Calculates the number of days in the month
          days_in_month <- ifelse(m %in% c("02", "02"), 
                                  ifelse(y %% 4 == 0, 29, 28), 
                                  ifelse(m %in% c("04", "06", "09", "11"), 30, 31))
          
          # Calculates the daily chance to die
          daily_chance_to_die <- deaths / (population * days_in_month)
          
          # Creates a data frame with the results
          temp_df <- data.frame(state = st, year = y, month = m, age_group = ag, deaths = deaths, population = population, daily_chance_to_die = daily_chance_to_die, days_in_month = days_in_month)
          
          # Adds the results to the main data frame
          result_df <- rbind(result_df, temp_df)
        }
      }
    }
  }
  
  print(result_df)
  write_csv(result_df, us_mortality_file)
}

# Loads the Phase 3 population randomized.
randomized_pop_file <- 'phase_3_randomized_pop.csv'
randomized_pop <- read.csv(randomized_pop_file)
print(randomized_pop)

# Loads the daily probabilities to die by state & year/month.
us_mortality <- read.csv(us_mortality_file)
print(us_mortality)

# Loads the trial sites data.
trial_sites_file <- 'trial_site_data.json'
trial_sites_data <- fromJSON(trial_sites_file, simplifyDataFrame = TRUE)
trial_sites_data <- trial_sites_data[trial_sites_data$country == "USA", ]
colnames(trial_sites_data)[colnames(trial_sites_data) == "trial_site_id"] <- "SITEID"
print(trial_sites_data)
print(unique(trial_sites_data$state))
print(unique(us_mortality$age_group ))

# Checks if all states in trial_sites_data are present in us_mortality
missing_states <- setdiff(trial_sites_data$state, us_mortality$state)

if (length(missing_states) == 0) {
  cat("All states in trial_sites_data are present in us_mortality.\n")
} else {
  cat("The following states in trial_sites_data are not present in us_mortality:\n")
  print(missing_states)
}

# Filters randomized_pop to only sustain US sites.
randomized_pop <- randomized_pop[randomized_pop$SITEID %in% trial_sites_data$SITEID, ]

# Creates the age_group column in randomized_pop
randomized_pop$age_group <- cut(randomized_pop$AGE, 
                                breaks = c(0, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, Inf), 
                                labels = c("< 10 years", "10-14 years", "15-19 years", "20-24 years", "25-29 years", "30-34 years", 
                                           "35-39 years", "40-44 years", "45-49 years", "50-54 years", "55-59 years", "60-64 years", 
                                           "65-69 years", "70-74 years", "75-79 years", "80-84 years", "85+ years"))
print(randomized_pop)

if (!file.exists(us_simu_mort_file)) {
  
  
  # Finds the earliest randomized_pop.RFICDT
  earliest_rficdt <- min(as.Date(randomized_pop$RFICDT))# Initialize the mort_by_date data frame with the relevant interval
  end_date <- as.Date("2021-02-28")
  dates <- earliest_rficdt + seq(0, as.integer((end_date - earliest_rficdt) + 1) - 1)
  mort_by_date <- data.frame(date = dates, mortality_rate = 0)
  
  # Iterates over each subject in the randomized_pop data frame
  total_subjects <- nrow(randomized_pop)
  current <- 0
  cpt <- 0
  for (i in 1:total_subjects) {
    cpt <- cpt  + 1
    current <- current  + 1
    if (cpt == 5) {
      cpt = 0
      print(paste('Processing ', current, '/', total_subjects))
    }
    subject_id <- randomized_pop$SUBJID[i]
    start_date <- as.Date(randomized_pop$RFICDT[i])
    ag <- randomized_pop$age_group[i]
    site_id <- randomized_pop$SITEID[i]
    st <- trial_sites_data$state[trial_sites_data$SITEID == site_id]
    
    # Calculates the number of days the subject is present
    days_present <- as.integer(end_date - start_date + 1)
    
    # Iterates over each day the subject is present
    for (j in 1:days_present) {
      current_date <- start_date + (j - 1)
      
      # Finds the year, month for the current date
      y <- as.integer(format(current_date, "%Y"))
      m <- as.integer(format(current_date, "%m"))
      
      # Finds the corresponding row in the us_mortality data frame
      mortality_prob <- us_mortality %>%
        filter(state == st, year == y, month == m, age_group == ag) %>% 
        pull(daily_chance_to_die)
      
      # Adds the daily chance to die to the mort_by_date data frame
      mort_by_date$mortality_rate[mort_by_date$date == current_date] <- 
        mort_by_date$mortality_rate[mort_by_date$date == current_date] + mortality_prob
    }
  }
  
  # Prints the mort_by_date vector
  print(mort_by_date)
  write.csv(mort_by_date, us_simu_mort_file)
  print(mort_by_date)
}

# Loads expected mortality data & unify cohort & expected mortality weekly format.
mort_by_date <- read.csv(us_simu_mort_file)
mort_by_date$date <- ymd(mort_by_date$date)
print(mort_by_date)
weekly_mort <- mort_by_date %>%
  group_by(week = floor_date(date, "week", week_start = 1)) %>%
  summarise(mortality_rate = sum(mortality_rate))
print(weekly_mort, n=100)
cohort_mortality <- randomized_pop %>% filter(!is.na(DTHDT))
print(cohort_mortality)
cohort_mortality$DTHDT <- ymd(cohort_mortality$DTHDT)
deaths_by_date <- cohort_mortality %>%
  group_by(DTHDT) %>%
  tally()
print(deaths_by_date, n=100)
deaths_mort <- deaths_by_date %>%
  group_by(week = floor_date(DTHDT, "week", week_start = 1)) %>%
  summarise(n = sum(n))
print(deaths_mort, n=100)
merged_data <- weekly_mort %>% 
  left_join(deaths_mort, by = "week") %>% 
  replace_na(list(n = 0))
print(merged_data, n=100)
merged_data <- merged_data %>% 
  mutate(year_week = format(week, "%Y-%W"))


# Plots the mortality expected vs actual.
print(merged_data, n=100)
ggplot(merged_data, aes(x = year_week)) + 
  geom_col(aes(y = mortality_rate, fill = "Expected")) + 
  geom_col(aes(y = n, fill = "Actual")) + 
  labs(title = "C4591001 - Expected vs Actual deaths for the USA population", 
       x = "Year-Week", y = "Mortality Rate", fill = "Type") + 
  theme_classic() + 
  theme(text = element_text(size = 18),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) + 
  scale_fill_manual(values = c("Expected" = "#c1c9b3", "Actual" = "#b75b47"))

# Summarizes a few metrics.
expected_deaths <- sum(merged_data$mortality_rate)
actual_deaths <- sum(merged_data$n)
percentage_diff <- (actual_deaths / expected_deaths) * 100
cat("Actual deaths are at", percentage_diff, "% of the expected deaths.")
print(paste('expected_deaths : ', expected_deaths))
print(paste('actual_deaths : ', actual_deaths))

print(nrow(randomized_pop))

# Adjusts the ARM column based on VAX201DT (only Placebo subjects were receiving BNT as third dose)
cohort_mortality$ARM[!is.na(cohort_mortality$VAX201DT)] <- "Placebo -> BNT162b2"
arm_counts <- table(cohort_mortality$ARM)
print(arm_counts)

