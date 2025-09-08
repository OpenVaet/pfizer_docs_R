# Loads necessary packages
library(haven)
library(dplyr)
library(lubridate)
library(tidyr)
library(stringr)
library(ggplot2)

# Define data cutoff date
DATA_CUTOFF <- as.Date("2020-11-14")

# --- Randomized Pop ----------------------------------------------------------
randomized_pop_file <- 'phase_3_randomized_pop.csv'

# --- Load randomized population ----------------------------------------------
randomized_pop <- read.csv(randomized_pop_file, stringsAsFactors = FALSE)

print(head(randomized_pop))
cat("Randomized population loaded with", nrow(randomized_pop), "subjects\n")

# Reading the input XPT files
adsl_data <- read_xpt("xpt_data/FDA-CBER-2021-5683-0772469-0773670_125742_S1_M5_C4591001-A-D_adsl.xpt")
adva_data <- read_xpt('xpt_data/FDA-CBER-2021-5683-0123168 to -0126026_125742_S1_M5_c4591001-A-D-adva.xpt')
mb_data   <- read_xpt('xpt_data/FDA-CBER-2021-5683-0282366 to -0285643_125742_S1_M5_c4591001-S-D-mb.xpt')
symptoms  <- read_xpt('xpt_data/FDA-CBER-2021-5683-0663135-0671344-125742_S1_M5_c4591001-A-D-adsympt.xpt')

# Helper function for date conversion
to_date <- function(x) {
  if (inherits(x, "Date")) return(x)
  if (is.numeric(x)) return(as.Date(x, origin = "1960-01-01"))
  suppressWarnings(lubridate::ymd(x))
}

# Process symptoms data - create daily symptom records WITH CUTOFF
symptoms_days <- symptoms %>%
  filter(PARCAT1 == "SIGNS AND SYMPTOMS OF DISEASE", AVALC == "Y") %>%
  mutate(
    ASTDT     = as.Date(ASTDT),
    AENDT     = as.Date(AENDT),
    end_date  = coalesce(AENDT, ASTDT),
    end_date  = if_else(end_date < ASTDT, ASTDT, end_date),
    # Apply cutoff to symptom dates
    ASTDT = if_else(ASTDT > DATA_CUTOFF, as.Date(NA), ASTDT),
    end_date = if_else(end_date > DATA_CUTOFF, DATA_CUTOFF, end_date),
    n_days    = as.integer(end_date - ASTDT) + 1
  ) %>%
  filter(!is.na(ASTDT), n_days >= 1, ASTDT <= DATA_CUTOFF) %>%
  tidyr::uncount(n_days, .remove = FALSE, .id = "day_index") %>%
  mutate(SYMPTDATE = ASTDT + (day_index - 1)) %>%
  filter(SYMPTDATE <= DATA_CUTOFF) %>%
  select(SUBJID, SYMPTDATE) %>%
  distinct()

# Process subject data
subjects <- adsl_data %>%
  filter(ARM %in% c("Placebo", "BNT162b2 Phase 2/3 (30 mcg)")) %>%
  mutate(across(c(UNBLNDDT, RANDDT, VAX101DT, VAX102DT), to_date)) %>%
  filter(!is.na(VAX101DT), !is.na(VAX102DT)) %>%  # must have both doses
  select(SUBJID, ARM, AGETR01, SITEID, UNBLNDDT, RANDDT, 
         VAX101DT, VAX102DT)

# Process PCR tests WITH CUTOFF
pcr_tests <- mb_data %>%
  filter(!is.na(MBDTC), !is.na(MBORRES),
         MBTEST %in% c('Cepheid RT-PCR assay for SARS-CoV-2',
                       'SEVERE ACUTE RESP SYNDROME CORONAVIRUS 2')) %>%
  mutate(
    SUBJID    = substr(USUBJID, 15, 24),
    TESTDATE  = ymd(substr(MBDTC, 1, 10)),
    TESTTYPE  = ifelse(MBTEST == 'SEVERE ACUTE RESP SYNDROME CORONAVIRUS 2',
                       'PCR Local', 'PCR Central'),
    TESTRESULT = str_trim(MBORRES),
    TESTRESULT = recode(TESTRESULT,
                        'INDETERMINATE' = 'IND',
                        'POSITIVE'      = 'POS',
                        'NEGATIVE'      = 'NEG')
  ) %>%
  filter(TESTDATE <= DATA_CUTOFF) %>%  # Apply cutoff to PCR tests
  select(SUBJID, TESTDATE, TESTTYPE, TESTRESULT)

# Process N-binding antibody tests WITH CUTOFF
nbind_tests <- adva_data %>%
  filter(PARAM == 'N-binding antibody - N-binding Antibody Assay') %>%
  mutate(
    TESTDATE   = ymd(substr(ADT, 1, 10)),
    TESTRESULT = str_trim(AVALC),
    TESTRESULT = recode(TESTRESULT,
                        'INDETERMINATE' = 'IND',
                        'POSITIVE'      = 'POS',
                        'NEGATIVE'      = 'NEG')
  ) %>%
  filter(TESTDATE <= DATA_CUTOFF) %>%  # Apply cutoff to N-binding tests
  select(SUBJID, TESTDATE, TESTRESULT) %>%
  rename(NBIND_RESULT = TESTRESULT)

# Get baseline status (at or before dose 1)
baseline_pcr <- pcr_tests %>%
  inner_join(subjects, by = "SUBJID") %>%
  filter(TESTDATE <= VAX101DT) %>%
  group_by(SUBJID) %>%
  slice_max(TESTDATE, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(SUBJID, PCR_BASELINE_DATE = TESTDATE, PCR_BASELINE_RESULT = TESTRESULT)

baseline_nbind <- nbind_tests %>%
  inner_join(subjects, by = "SUBJID") %>%
  filter(TESTDATE <= VAX101DT) %>%
  group_by(SUBJID) %>%
  slice_max(TESTDATE, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(SUBJID, NBIND_BASELINE_DATE = TESTDATE, NBIND_BASELINE_RESULT = NBIND_RESULT)

# Get all positive PCR tests after dose 1 (within cutoff period)
post_dose1_pcr <- pcr_tests %>%
  inner_join(subjects, by = "SUBJID") %>%
  filter(TESTRESULT == "POS", TESTDATE > VAX101DT, TESTDATE <= DATA_CUTOFF) %>%
  select(SUBJID, PCR_POS_DATE = TESTDATE, PCR_POS_TYPE = TESTTYPE)

# Check which positive PCRs have symptoms within ±4 days
symptomatic_pcr <- post_dose1_pcr %>%
  left_join(symptoms_days, by = "SUBJID", relationship = "many-to-many") %>%
  mutate(days_diff = abs(as.integer(SYMPTDATE - PCR_POS_DATE))) %>%
  filter(days_diff <= 4) %>%
  select(SUBJID, PCR_POS_DATE, PCR_POS_TYPE) %>%
  distinct() %>%
  group_by(SUBJID) %>%
  slice_min(PCR_POS_DATE, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  rename(FIRST_SYMPT_PCR_DATE = PCR_POS_DATE,
         FIRST_SYMPT_PCR_TYPE = PCR_POS_TYPE)

# Create final flattened dataframe
flattened_simple <- subjects %>%
  left_join(baseline_pcr, by = "SUBJID") %>%
  left_join(baseline_nbind, by = "SUBJID") %>%
  left_join(symptomatic_pcr, by = "SUBJID") %>%
  mutate(
    # Define baseline negative status
    BASELINE_NEG = case_when(
      is.na(PCR_BASELINE_RESULT) | is.na(NBIND_BASELINE_RESULT) ~ NA,
      PCR_BASELINE_RESULT == "NEG" & NBIND_BASELINE_RESULT == "NEG" ~ TRUE,
      TRUE ~ FALSE
    ),
    # Days from dose 1 to first symptomatic PCR (if any)
    DAYS_TO_SYMPT_PCR = as.integer(FIRST_SYMPT_PCR_DATE - VAX101DT)
  )

# Filter to baseline negative subjects who are in randomized population
# and add COUNTRY information
baseline_neg_subjects <- flattened_simple %>%
  filter(BASELINE_NEG == TRUE) %>%
  inner_join(randomized_pop %>% 
             mutate(SUBJID = as.character(SUBJID)) %>%
             select(SUBJID, COUNTRY), 
             by = "SUBJID")

# Summary statistics
cat("\n=== DATA CUTOFF DATE: ", format(DATA_CUTOFF, "%Y-%m-%d"), " ===\n")
cat("\n=== BASELINE NEGATIVE SUBJECTS (IN RANDOMIZED POPULATION) ===\n")
cat("Total baseline negative subjects in randomized pop:", nrow(baseline_neg_subjects), "\n")
cat("With data through cutoff date\n\n")

# By arm
arm_summary <- baseline_neg_subjects %>%
  group_by(ARM) %>%
  summarise(
    n_total = n(),
    n_with_sympt_pcr = sum(!is.na(FIRST_SYMPT_PCR_DATE)),
    pct_with_sympt_pcr = 100 * n_with_sympt_pcr / n_total,
    median_days_to_pcr = median(DAYS_TO_SYMPT_PCR, na.rm = TRUE),
    .groups = "drop"
  )

cat("By treatment arm:\n")
print(arm_summary)

# Save the flattened dataframe for further analysis
write.csv(baseline_neg_subjects, "baseline_negative_subjects_with_symptomatic_pcr.csv", row.names = FALSE)
cat("\n=== Data saved to baseline_negative_subjects_with_symptomatic_pcr.csv ===\n")

# =========================================================================
# CUMULATIVE INCIDENCE PLOTS
# =========================================================================

# Create time-to-event data from enrollment (RANDDT)
# Apply cutoff date for censoring
tte_data <- baseline_neg_subjects %>%
  mutate(
    # Use the minimum of unblinding date or cutoff date for censoring
    censor_date = pmin(UNBLNDDT, DATA_CUTOFF, na.rm = TRUE),
    # Calculate days from randomization to event or censoring
    days_to_event = case_when(
      !is.na(FIRST_SYMPT_PCR_DATE) & FIRST_SYMPT_PCR_DATE <= DATA_CUTOFF ~ 
        as.integer(FIRST_SYMPT_PCR_DATE - RANDDT),
      !is.na(censor_date) ~ as.integer(censor_date - RANDDT),
      TRUE ~ NA_integer_
    ),
    # Event indicator (1 = symptomatic PCR positive before cutoff, 0 = censored)
    event = ifelse(!is.na(FIRST_SYMPT_PCR_DATE) & FIRST_SYMPT_PCR_DATE <= DATA_CUTOFF, 1, 0)
  ) %>%
  filter(!is.na(days_to_event), days_to_event >= 0) %>%
  select(SUBJID, ARM, COUNTRY, days_to_event, event, RANDDT, FIRST_SYMPT_PCR_DATE, censor_date)

# Calculate cumulative incidence for each arm
placebo_data <- tte_data %>% 
  filter(ARM == "Placebo") %>%
  arrange(days_to_event)

vaccine_data <- tte_data %>% 
  filter(ARM == "BNT162b2 Phase 2/3 (30 mcg)") %>%
  arrange(days_to_event)

# Function to calculate cumulative incidence
calc_cumulative_incidence <- function(data) {
  data %>%
    arrange(days_to_event) %>%
    mutate(
      cumulative_events = cumsum(event),
      total_n = n(),
      cumulative_incidence = 100 * cumulative_events / total_n
    ) %>%
    select(days_to_event, cumulative_incidence, cumulative_events, event)
}

placebo_ci <- calc_cumulative_incidence(placebo_data)
vaccine_ci <- calc_cumulative_incidence(vaccine_data)

# Plot 1: Placebo arm
p1 <- ggplot(placebo_ci, aes(x = days_to_event, y = cumulative_incidence)) +
  geom_step(color = "red", size = 1) +
  geom_point(data = filter(placebo_ci, event == 1), 
             aes(x = days_to_event, y = cumulative_incidence),
             color = "red", size = 0.5, alpha = 0.6) +
  labs(
    title = "Cumulative Incidence of Symptomatic COVID-19: Placebo Arm",
    subtitle = paste0("N = ", nrow(placebo_data), " baseline-negative subjects (Data cutoff: ", DATA_CUTOFF, ")"),
    x = "Days from Study Enrollment",
    y = "Cumulative Incidence (%)",
    caption = paste0("Total events through cutoff: ", sum(placebo_data$event))
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 11),
    axis.title = element_text(size = 11)
  ) +
  scale_y_continuous(limits = c(0, max(placebo_ci$cumulative_incidence) * 1.1),
                     breaks = seq(0, 100, by = 0.5)) +
  scale_x_continuous(breaks = seq(0, max(placebo_ci$days_to_event, na.rm = TRUE), by = 30))

# Plot 2: Vaccine arm
p2 <- ggplot(vaccine_ci, aes(x = days_to_event, y = cumulative_incidence)) +
  geom_step(color = "blue", size = 1) +
  geom_point(data = filter(vaccine_ci, event == 1), 
             aes(x = days_to_event, y = cumulative_incidence),
             color = "blue", size = 0.5, alpha = 0.6) +
  labs(
    title = "Cumulative Incidence of Symptomatic COVID-19: BNT162b2 Arm",
    subtitle = paste0("N = ", nrow(vaccine_data), " baseline-negative subjects (Data cutoff: ", DATA_CUTOFF, ")"),
    x = "Days from Study Enrollment",
    y = "Cumulative Incidence (%)",
    caption = paste0("Total events through cutoff: ", sum(vaccine_data$event))
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 11),
    axis.title = element_text(size = 11)
  ) +
  scale_y_continuous(limits = c(0, max(vaccine_ci$cumulative_incidence) * 1.1),
                     breaks = seq(0, 100, by = 0.1)) +
  scale_x_continuous(breaks = seq(0, max(vaccine_ci$days_to_event, na.rm = TRUE), by = 30))

# Plot 3: Combined comparison
combined_data <- bind_rows(
  placebo_ci %>% mutate(ARM = "Placebo"),
  vaccine_ci %>% mutate(ARM = "BNT162b2")
)

p3 <- ggplot(combined_data, aes(x = days_to_event, y = cumulative_incidence, color = ARM)) +
  geom_step(size = 1) +
  geom_point(data = filter(combined_data, event == 1), 
             aes(x = days_to_event, y = cumulative_incidence),
             size = 0.5, alpha = 0.6) +
  labs(
    title = "Cumulative Incidence of Symptomatic COVID-19: Comparison",
    subtitle = paste0("Placebo (n=", nrow(placebo_data), ") vs BNT162b2 (n=", nrow(vaccine_data), 
                      ") - Data cutoff: ", DATA_CUTOFF),
    x = "Days from Study Enrollment",
    y = "Cumulative Incidence (%)",
    color = "Treatment Arm"
  ) +
  scale_color_manual(values = c("BNT162b2" = "blue", "Placebo" = "red")) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 11),
    axis.title = element_text(size = 11),
    legend.position = "bottom"
  ) +
  scale_y_continuous(limits = c(0, max(combined_data$cumulative_incidence) * 1.1)) +
  scale_x_continuous(breaks = seq(0, max(combined_data$days_to_event, na.rm = TRUE), by = 30))

# Plot 4: Raw numbers comparison
combined_raw <- bind_rows(
  placebo_ci %>% mutate(ARM = "Placebo", total_n = nrow(placebo_data)),
  vaccine_ci %>% mutate(ARM = "BNT162b2", total_n = nrow(vaccine_data))
)

p4 <- ggplot(combined_raw, aes(x = days_to_event, y = cumulative_events, color = ARM)) +
  geom_step(size = 1) +
  geom_point(data = filter(combined_raw, event == 1), 
             aes(x = days_to_event, y = cumulative_events),
             size = 0.5, alpha = 0.6) +
  labs(
    title = "Cumulative Number of Symptomatic COVID-19 Cases",
    subtitle = paste0("Placebo (n=", nrow(placebo_data), ") vs BNT162b2 (n=", nrow(vaccine_data), 
                      ") - Data cutoff: ", DATA_CUTOFF),
    x = "Days from Study Enrollment",
    y = "Cumulative Number of Cases",
    color = "Treatment Arm"
  ) +
  scale_color_manual(values = c("BNT162b2" = "blue", "Placebo" = "red")) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 11),
    axis.title = element_text(size = 11),
    legend.position = "bottom"
  ) +
  scale_y_continuous(breaks = seq(0, max(combined_raw$cumulative_events), 
                                  by = ifelse(max(combined_raw$cumulative_events) > 100, 50, 10))) +
  scale_x_continuous(breaks = seq(0, max(combined_raw$days_to_event, na.rm = TRUE), by = 30)) +
  annotate("text", 
           x = max(combined_raw$days_to_event, na.rm = TRUE) * 0.85,
           y = max(placebo_ci$cumulative_events) + 5,
           label = paste0("Placebo: ", max(placebo_ci$cumulative_events), " cases"),
           color = "red", size = 4, fontface = "bold") +
  annotate("text",
           x = max(combined_raw$days_to_event, na.rm = TRUE) * 0.85,
           y = max(vaccine_ci$cumulative_events) + 5,
           label = paste0("BNT162b2: ", max(vaccine_ci$cumulative_events), " cases"),
           color = "blue", size = 4, fontface = "bold")

# Display all plots
print(p1)
print(p2)
print(p3)
print(p4)

# Save all plots
ggsave("cumulative_incidence_placebo.png", p1, width = 10, height = 6, dpi = 300)
ggsave("cumulative_incidence_vaccine.png", p2, width = 10, height = 6, dpi = 300)
ggsave("cumulative_incidence_comparison.png", p3, width = 10, height = 6, dpi = 300)
ggsave("cumulative_cases_raw_numbers.png", p4, width = 10, height = 6, dpi = 300)

# Print summary statistics
cat("\n=== CUMULATIVE INCIDENCE SUMMARY (Through ", format(DATA_CUTOFF, "%Y-%m-%d"), ") ===\n")
cat("\nPlacebo Arm:\n")
cat("  Total subjects:", nrow(placebo_data), "\n")
cat("  Total events:", sum(placebo_data$event), "\n")
cat("  Final cumulative incidence:", round(max(placebo_ci$cumulative_incidence), 2), "%\n")

cat("\nVaccine Arm:\n")
cat("  Total subjects:", nrow(vaccine_data), "\n")
cat("  Total events:", sum(vaccine_data$event), "\n")
cat("  Final cumulative incidence:", round(max(vaccine_ci$cumulative_incidence), 2), "%\n")

cat("\nRisk Difference:\n")
cat("  Absolute difference:", 
    round(max(placebo_ci$cumulative_incidence) - max(vaccine_ci$cumulative_incidence), 2), 
    "percentage points\n")
cat("  Relative risk reduction:", 
    round(100 * (1 - max(vaccine_ci$cumulative_incidence)/max(placebo_ci$cumulative_incidence)), 1), 
    "%\n")
cat("  Absolute case difference:", 
    max(placebo_ci$cumulative_events) - max(vaccine_ci$cumulative_events), 
    "cases\n")