library(haven)
library(dplyr)
library(lubridate)
library(stringr)
library(ggplot2)

# Loads the official EUA cases.
official_cases_file <- 'eua_official_cases.csv'
official_cases <- read.csv(official_cases_file)

symptoms_file <- 'xpt_data/FDA-CBER-2021-5683-0663135-0671344-125742_S1_M5_c4591001-A-D-adsympt.xpt'

# Reads the primary XPT files
symptoms <- read_xpt(symptoms_file)
print(colnames(symptoms))
print(symptoms)

# Filters the symptoms data to only include subjects in the official cases population
symptoms <- symptoms[symptoms$SUBJID %in% official_cases$SUBJID, ]
symptoms <- symptoms %>%
  filter(PARCAT1 == "SIGNS AND SYMPTOMS OF DISEASE")
symptoms <- symptoms %>%
  filter(AVALC == "Y")
print(colnames(symptoms))
print(symptoms)

# Sustains only columns required for the current analysis.
symptoms_selected <- symptoms[c("SUBJID", "SITEID", "ARM", "AVISIT", "PARAM", "VISITNUM", "VISIT", "ADT", "ASTDT")]
print(symptoms_selected)

# Loads the supplementary file.
symptoms_sup_file <- 'xpt_data/FDA-CBER-2021-5683-0539816-0593326-125742_S1_M5_c4591001-01-S-Supp-D-face.xpt'
symptoms_sup <- read_xpt(symptoms_sup_file)
symptoms_sup <- symptoms_sup %>%
  filter(FATEST == "First Symptom Date")

# Filters the symptoms sup data to only include subjects in the official cases population
symptoms_sup <- symptoms_sup %>%
  mutate(SUBJID = str_extract(USUBJID, "\\d+$"))
symptoms_sup <- symptoms_sup[symptoms_sup$SUBJID %in% official_cases$SUBJID, ]
print(colnames(symptoms_sup))
print(symptoms_sup)

# Sustains only columns required for the current analysis (date of first symptoms).
symptoms_sup_selected <- symptoms_sup[c("SUBJID", "FAORRES", "VISIT")]
print(symptoms_sup_selected)

# Creates a dataframe subjects_sympto_visits containing, for each unique SUBJID-VISIT pair in symptoms_selected
subjects_sympto_visits <- symptoms_selected %>%
  group_by(SUBJID, VISIT, ARM, SITEID) %>%
  summarise(
    EARLIESTSYMPTDT = min(ADT, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(
    symptoms_sup_selected %>%
      mutate(FAORRES = as.Date(FAORRES)),
    by = "SUBJID",
    relationship = "many-to-many"
  ) %>%
  mutate(
    EARLIESTDT = coalesce(FAORRES, EARLIESTSYMPTDT),
    VISIT = VISIT.x
  ) %>%
  select(-VISIT.y)

# Removes the VISIT.x column from the subjects_sympto_visits dataframe
subjects_sympto_visits <- subjects_sympto_visits %>%
  select(-VISIT.x)

print(subjects_sympto_visits)

# Displays the exceptions where EARLIESTSYMPTDT is not greater than or equal to EARLIESTDT
exceptions <- subjects_sympto_visits[subjects_sympto_visits$EARLIESTSYMPTDT < subjects_sympto_visits$EARLIESTDT, ]
if (nrow(exceptions) > 0) {
  print(exceptions)
  
  # Reformats the exceptions dataframe to only contain the earliest date in EARLIESTDT
  exceptions <- exceptions %>%
    group_by(SUBJID, VISIT) %>%
    summarize(
      EARLIESTSYMPTDT = min(EARLIESTSYMPTDT),
      FAORRES = min(FAORRES, na.rm = TRUE),
      EARLIESTDT = min(EARLIESTDT)
    )
  print(exceptions)
} else {
  cat("No exceptions found. EARLIESTSYMPTDT is greater than or equal to EARLIESTDT for all rows.\n")
}
print(subjects_sympto_visits)

# Reformats the subjects_sympto_visits dataframe to ensure each row is unique
subjects_sympto_visits <- subjects_sympto_visits %>%
  group_by(SUBJID, ARM, SITEID, VISIT) %>%
  summarize(
    EARLIESTSYMPTDT = min(EARLIESTSYMPTDT),
    FAORRES = if (all(is.na(FAORRES))) NA else min(FAORRES, na.rm = TRUE),
    EARLIESTDT = min(EARLIESTDT),
    .groups = "drop"
  )
print(subjects_sympto_visits)

# Filters out rows where EARLIESTDT is after 2020-11-14
subjects_sympto_visits <- subjects_sympto_visits %>%
  filter(ymd(EARLIESTDT) <= ymd("2020-11-14"))

# Removes the EARLIESTSYMPTDT column from the subjects_sympto_visits dataframe
subjects_sympto_visits <- subjects_sympto_visits %>%
  select(-EARLIESTSYMPTDT, -FAORRES)

print(subjects_sympto_visits)

# Loads tests file.
tests_file <- "xpt_data/FDA-CBER-2021-5683-0282366 to -0285643_125742_S1_M5_c4591001-S-D-mb.xpt"
tests <- read_xpt(tests_file)

# Filters the tests dataframe
tests <- tests %>%
  filter(MBTEST %in% c("Cepheid RT-PCR assay for SARS-CoV-2", "SEVERE ACUTE RESP SYNDROME CORONAVIRUS 2"))
tests <- tests %>%
  mutate(SUBJID = str_extract(USUBJID, "\\d+$"))
print(colnames(tests))
print(tests)

# Sustains only columns required for the current analysis.
tests_selected <- tests[c("SUBJID", "VISIT", "MBTEST", "MBORRES", "MBDTC")]

# Replaces values in MBORRES column
tests_selected$MBORRES <- replace(tests_selected$MBORRES, tests_selected$MBORRES == "NEGATIVE", "NEG")
tests_selected$MBORRES <- replace(tests_selected$MBORRES, tests_selected$MBORRES == "POSITIVE", "POS")
tests_selected$MBORRES <- replace(tests_selected$MBORRES, tests_selected$MBORRES == "INDETERMINATE", "IND")

# Filters tests_selected to keep only rows where VISIT contains "COVID_"
tests_selected <- tests_selected[grepl("COVID_", tests_selected$VISIT), ]
print(tests_selected)

# Filters out rows where MBDTC is after 2020-11-14
tests_selected <- tests_selected %>%
  filter(ymd(MBDTC) <= ymd("2020-11-14"))

# Creates the central_test dataframe
central_test = tests_selected[tests_selected$MBTEST == 'Cepheid RT-PCR assay for SARS-CoV-2', c('SUBJID', 'VISIT', 'MBORRES', 'MBDTC')]

# Creates the local_test dataframe
local_test = tests_selected[tests_selected$MBTEST == 'SEVERE ACUTE RESP SYNDROME CORONAVIRUS 2', c('SUBJID', 'VISIT', 'MBORRES', 'MBDTC')]

print(central_test)
print(local_test)
print(subjects_sympto_visits)

subjects_sympto_visits <- subjects_sympto_visits %>%
  left_join(
    central_test %>% 
      group_by(SUBJID, VISIT) %>% 
      summarise(.groups = "drop", 
                EARLIESTCENTRALDT = min(MBDTC), 
                EARLIESTCENTRALPOSDT = ifelse(length(MBDTC[MBORRES == "POS"]) == 0, NA, min(MBDTC[MBORRES == "POS"]))),
    by = c("SUBJID", "VISIT")
  ) %>%
  left_join(
    local_test %>% 
      group_by(SUBJID, VISIT) %>% 
      summarise(.groups = "drop", 
                EARLIESTLOCALDT = min(MBDTC), 
                EARLIESTLOCALPOSDT = ifelse(length(MBDTC[MBORRES == "POS"]) == 0, NA, min(MBDTC[MBORRES == "POS"]))),
    by = c("SUBJID", "VISIT")
  )
print(subjects_sympto_visits, n=200)

# Adds the total of days between the symptoms & the test
subjects_sympto_visits <- subjects_sympto_visits %>%
  mutate(
    EARLIESTCENTRALDAYSTOSYMPT = ifelse(!is.na(EARLIESTCENTRALDT), as.numeric(as.Date(EARLIESTCENTRALDT) - as.Date(EARLIESTDT)), NA),
    EARLIESTCENTRALPOSDAYSTOSYMPT = ifelse(!is.na(EARLIESTCENTRALPOSDT), as.numeric(as.Date(EARLIESTCENTRALPOSDT) - as.Date(EARLIESTDT)), NA),
    EARLIESTLOCALDAYSTOSYMPT = ifelse(!is.na(EARLIESTLOCALDT), as.numeric(as.Date(EARLIESTLOCALDT) - as.Date(EARLIESTDT)), NA),
    EARLIESTLOCALPOSDAYSTOSYMPT = ifelse(!is.na(EARLIESTLOCALPOSDT), as.numeric(as.Date(EARLIESTLOCALPOSDT) - as.Date(EARLIESTDT)), NA)
  )
print(subjects_sympto_visits)

# Filters out visits which didn't resulted in a positive Central test.
subjects_pos_sympto_visits <- subjects_sympto_visits %>%
  filter(!is.na(EARLIESTCENTRALPOSDT))

# Sustains only one row by subject based on the earliest positive central test.
subjects_pos_sympto_visits <- subjects_pos_sympto_visits %>%
  group_by(SUBJID) %>%
  slice(which.min(as.Date(EARLIESTCENTRALPOSDT))) %>%
  ungroup()
print(subjects_pos_sympto_visits)

# Writes the result to a CSV file
write.csv(subjects_pos_sympto_visits, "phase_3_eua_official_subjects_sympto_visits.csv", row.names = FALSE)

# Groups by ARM and count the number of visits with and without central and local tests
arm_summary <- subjects_pos_sympto_visits %>%
  group_by(ARM) %>%
  summarize(
    total_visits = n(),
    central_test_visits = sum(!is.na(EARLIESTCENTRALDT)),
    local_test_visits = sum(!is.na(EARLIESTLOCALDT))
  )

# Prints the result
print(arm_summary)

# Prints subjects which are in the official cases but not in the output.
eua_official_cases_SUBJID <- official_cases$SUBJID
subjects_pos_sympto_visits_SUBJID <- subjects_pos_sympto_visits$SUBJID

missing_SUBJID <- setdiff(eua_official_cases_SUBJID, subjects_pos_sympto_visits_SUBJID)

print(missing_SUBJID)


# Create a date sequence from the earliest to the latest date in EARLIESTCENTRALPOSDT
date_seq <- seq(min(as.Date(subjects_pos_sympto_visits$EARLIESTCENTRALPOSDT)), 
                max(as.Date(subjects_pos_sympto_visits$EARLIESTCENTRALPOSDT)), 
                by = "day")

# Create a dataframe to store the daily cases and accumulated cases
plot_data <- data.frame(date = date_seq, 
                        daily_cases = 0, 
                        accumulated_cases = 0)

# Loop through each date in the sequence and count the number of cases detected on that date
for (i in seq_along(date_seq)) {
  date <- date_seq[i]
  cases_on_date <- subjects_pos_sympto_visits %>%
    filter(as.Date(EARLIESTCENTRALPOSDT) == date) %>%
    nrow()
  plot_data$daily_cases[i] <- cases_on_date
  plot_data$accumulated_cases[i] <- sum(plot_data$daily_cases[1:i])
}

# Find the dates where the accumulated cases exceed 62, 92, and 164 for the first time
marker_dates <- plot_data %>%
  mutate(cumsum = cumsum(daily_cases)) %>%
  filter(cumsum >= 62 & cumsum - daily_cases < 62) %>%
  slice(1) %>%
  pull(date) %>%
  c(.,
    plot_data %>%
      mutate(cumsum = cumsum(daily_cases)) %>%
      filter(cumsum >= 92 & cumsum - daily_cases < 92) %>%
      slice(1) %>%
      pull(date),
    plot_data %>%
      mutate(cumsum = cumsum(daily_cases)) %>%
      filter(cumsum >= 164 & cumsum - daily_cases < 164) %>%
      slice(1) %>%
      pull(date)
  )

# Create a dataframe with the marker dates and corresponding accumulated cases
marker_data <- plot_data %>%
  filter(date %in% marker_dates) %>%
  select(date, accumulated_cases)

# Create the plot
ggplot(plot_data, aes(x = date)) + 
  geom_col(aes(y = daily_cases), fill = "skyblue") + 
  geom_line(aes(y = accumulated_cases / 13.33), color = "red", size = 1.2) + 
  geom_point(data = marker_data, 
             aes(x = date, y = accumulated_cases / 13.33), 
             color = "black", size = 3) + 
  geom_text(data = marker_data, 
            aes(x = date, y = accumulated_cases / 13.33, label = accumulated_cases), 
            color = "black", vjust = -0.5, size = 6) +
  labs(x = "Date", y = "Number of Cases", 
       title = "C4591001 - Official EUA COVID-19 Cases accrued to November 14, 2020") + 
  theme_classic() + 
  scale_x_date(breaks = seq(min(plot_data$date), max(plot_data$date), by = "3 day"), 
               date_labels = "%d %b %Y") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 16), 
        axis.text.y = element_text(size = 16), 
        axis.title.x = element_text(size = 16), 
        axis.title.y = element_text(size = 16), 
        legend.text = element_text(size = 14), 
        text = element_text(size = 18),
        legend.title = element_text(size = 18)) + 
  scale_y_continuous(name = "Number of Cases", limits = c(0, 15), 
                     sec.axis = sec_axis(~ .* 13.33, name = "Accumulated Cases"))

# Prints 10911203 for analysis
subjects_sympto_visits_filtered <- subjects_sympto_visits %>% 
  filter(SUBJID == 10911203)
print(subjects_sympto_visits_filtered)


