# ============================================================
# Expected vs Observed Symptomatic PCR+ by Arm (USA/ARGENTINA)
# EXPECTED uses purely model-based depletion (no observed censoring)
# Uses:
#   - baseline_negative_subjects_with_symptomatic_pcr.csv
#   - jh_data/weekly_cases_by_countries.csv  (from JHU-derived weekly builder)
# ============================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(lubridate)
  library(ISOweek)
  library(stringr)
  library(ggplot2)
})

# ------------------------------------------------------------
# Config
# ------------------------------------------------------------
DATA_CUTOFF <- as.Date("2020-11-14")
weekly_path <- file.path("jh_data", "weekly_cases_by_countries.csv")
baseline_path <- "baseline_negative_subjects_with_symptomatic_pcr.csv"

if (!file.exists(weekly_path))  stop("Missing: ", weekly_path)
if (!file.exists(baseline_path)) stop("Missing: ", baseline_path)

# ------------------------------------------------------------
# Load data
# ------------------------------------------------------------
weekly <- read_csv(weekly_path, show_col_types = FALSE) %>%
  mutate(
    COUNTRY = toupper(COUNTRY),
    week_start = ISOweek2date(paste0(ISOWEEK, "-1")),
    week_end   = ISOweek2date(paste0(ISOWEEK, "-7"))
  )

baseline <- read_csv(baseline_path, show_col_types = FALSE)

needed_cols <- c("SUBJID", "COUNTRY", "ARM", "VAX101DT", "FIRST_SYMPT_PCR_DATE")
missing_cols <- setdiff(needed_cols, names(baseline))
if (length(missing_cols)) stop("baseline CSV missing columns: ", paste(missing_cols, collapse = ", "))

baseline <- baseline %>%
  mutate(
    COUNTRY = toupper(as.character(COUNTRY)),
    ARM = as.character(ARM),
    VAX101DT = as.Date(VAX101DT),
    FIRST_SYMPT_PCR_DATE = suppressWarnings(as.Date(FIRST_SYMPT_PCR_DATE))
  )

# ------------------------------------------------------------
# Keep USA / ARGENTINA, valid dose-1, before cutoff
# ------------------------------------------------------------
baseline_f <- baseline %>%
  filter(COUNTRY %in% c("USA", "ARGENTINA")) %>%
  filter(!is.na(VAX101DT), VAX101DT <= DATA_CUTOFF)

if (nrow(baseline_f) == 0) stop("No subjects after filtering to USA/ARGENTINA with valid VAX101DT.")

# ------------------------------------------------------------
# Build DAILY infection risk per country from JHU weekly rates
# daily_risk = (WEEKLY_PER_100K / 7) / 100000
# ------------------------------------------------------------
weekly_f <- weekly %>% filter(COUNTRY %in% c("USA", "ARGENTINA"))
if (nrow(weekly_f) == 0) stop("Weekly file has no rows for USA/ARGENTINA.")

jh_daily <- weekly_f %>%
  mutate(daily_per_100k = WEEKLY_PER_100K / 7) %>%
  rowwise() %>%
  mutate(date = list(seq(week_start, week_end, by = "day"))) %>%
  ungroup() %>%
  unnest(date) %>%
  transmute(COUNTRY, date, daily_risk = (daily_per_100k / 1e5))

if (nrow(jh_daily) == 0) stop("Failed to build daily risk series.")

# ------------------------------------------------------------
# Analysis window: overlap of subject follow-up and daily-risk dates
# ------------------------------------------------------------
subjects_start <- min(baseline_f$VAX101DT, na.rm = TRUE)
risk_start <- min(jh_daily$date, na.rm = TRUE)
risk_end   <- max(jh_daily$date, na.rm = TRUE)

analysis_start <- max(subjects_start, risk_start)
analysis_end   <- min(DATA_CUTOFF, risk_end)
if (analysis_start > analysis_end) {
  stop("Empty analysis window: ", format(analysis_start), " .. ", format(analysis_end))
}
date_seq <- seq(analysis_start, analysis_end, by = "day")
message("Analysis window: ", format(analysis_start), " .. ", format(analysis_end),
        " (", length(date_seq), " days)")

# ============================================================
# MODEL-BASED EXPECTATION with DEPLETION (no observed censoring)
# Per-subject discrete-time survival:
#   hazard h_{i,t} = daily_risk(country,date) * RR_{i,t}
#   S_{i,t+1} = S_{i,t} * (1 - h_{i,t}),   S_{i,first} = 1
#   E[Infections]_t = sum_i S_{i,t} * h_{i,t}
# RR_{i,t}:
#   - Placebo: 1
#   - BNT162b2 Phase 2/3 (30 mcg): 1 for day 0..13, and 0.05 from day >=14
# ============================================================
bnt_label <- "BNT162b2 Phase 2/3 (30 mcg)"

# Build subject-day panel (from VAX101DT to analysis_end, clamped to analysis window)
subject_days <- baseline_f %>%
  transmute(
    SUBJID, ARM, COUNTRY, VAX101DT,
    start_date = pmax(VAX101DT, analysis_start),
    end_date   = analysis_end
  ) %>%
  filter(start_date <= end_date) %>%
  rowwise() %>%
  mutate(date = list(seq(start_date, end_date, by = "day"))) %>%
  ungroup() %>%
  unnest(date) %>%
  mutate(days_since_dose = as.integer(date - VAX101DT))

# Join country/day risk
subject_days <- subject_days %>%
  left_join(jh_daily, by = c("COUNTRY", "date")) %>%
  mutate(daily_risk = coalesce(daily_risk, 0))

# Compute arm/day-specific relative risk
subject_days <- subject_days %>%
  mutate(
    rr = case_when(
      ARM == bnt_label & days_since_dose >= 14 ~ 0.05,
      TRUE ~ 1
    ),
    hazard = daily_risk * rr
  )

# Per-subject survival and expected infections by day
subject_days <- subject_days %>%
  arrange(SUBJID, date) %>%
  group_by(SUBJID) %>%
  mutate(
    # Survival at start of day t: product of (1 - hazard) over previous days
    S_start = cumprod(dplyr::lag(1 - hazard, default = 1)),
    expected_infection = S_start * hazard
  ) %>%
  ungroup()

# Aggregate expected infections by ARM & date
expected_by_arm <- subject_days %>%
  group_by(ARM, date) %>%
  summarise(expected_daily = sum(expected_infection, na.rm = TRUE), .groups = "drop") %>%
  arrange(ARM, date) %>%
  group_by(ARM) %>%
  mutate(expected_cum = cumsum(expected_daily)) %>%
  ungroup()

# ------------------------------------------------------------
# Observed daily infections
# ------------------------------------------------------------
observed_by_arm <- baseline_f %>%
  filter(!is.na(FIRST_SYMPT_PCR_DATE),
         FIRST_SYMPT_PCR_DATE >= analysis_start,
         FIRST_SYMPT_PCR_DATE <= analysis_end) %>%
  transmute(ARM, date = FIRST_SYMPT_PCR_DATE) %>%
  count(ARM, date, name = "observed_daily") %>%
  complete(ARM, date = date_seq, fill = list(observed_daily = 0)) %>%
  arrange(ARM, date) %>%
  group_by(ARM) %>%
  mutate(observed_cum = cumsum(observed_daily)) %>%
  ungroup()

# Merge expected & observed
series_by_arm <- expected_by_arm %>%
  full_join(observed_by_arm, by = c("ARM", "date")) %>%
  group_by(ARM) %>%
  arrange(date, .by_group = TRUE) %>%
  mutate(
    expected_daily = coalesce(expected_daily, 0),
    expected_cum   = coalesce(expected_cum, 0),
    observed_daily = coalesce(observed_daily, 0),
    observed_cum   = coalesce(observed_cum, 0)
  ) %>%
  ungroup()

# ------------------------------------------------------------
# Plots
# ------------------------------------------------------------
plot_daily <- function(df, arm_label) {
  df_arm <- df %>% filter(ARM == arm_label)
  if (nrow(df_arm) == 0) { message("No data for arm: ", arm_label); return(invisible(NULL)) }
  
  ggplot(df_arm, aes(x = date)) +
    geom_line(aes(y = expected_daily, linetype = "Expected (model-based, depleted)")) +
    geom_line(aes(y = observed_daily, linetype = "Observed")) +
    scale_linetype_manual(values = c("Expected (model-based, depleted)" = "solid", "Observed" = "dashed")) +
    labs(
      title = paste0("Daily symptomatic PCR+ — Expected vs Observed (", arm_label, ")"),
      x = "Date", y = "Count", linetype = NULL,
      caption = paste0(
        "Expected uses JHU-derived weekly rates converted to daily risk, ",
        "with depletion via survival (no observed censoring).\n",
        bnt_label, ": RR=1 for days 0–13 post–dose1, RR=0.05 from day 14+.\n",
        "Window: ", format(analysis_start), "–", format(analysis_end),
        " | Countries: USA, ARGENTINA | Baseline negative subjects only."
      )
    ) +
    theme_minimal(base_size = 12)
}

plot_cum <- function(df, arm_label) {
  df_arm <- df %>% filter(ARM == arm_label)
  if (nrow(df_arm) == 0) { message("No data for arm: ", arm_label); return(invisible(NULL)) }
  
  ggplot(df_arm, aes(x = date)) +
    geom_line(aes(y = expected_cum, linetype = "Expected (model-based, depleted)")) +
    geom_line(aes(y = observed_cum, linetype = "Observed")) +
    scale_linetype_manual(values = c("Expected (model-based, depleted)" = "solid", "Observed" = "dashed")) +
    labs(
      title = paste0("Cumulative symptomatic PCR+ — Expected vs Observed (", arm_label, ")"),
      x = "Date", y = "Cumulative count", linetype = NULL,
      caption = paste0(
        "Expected uses JHU-derived weekly rates converted to daily risk, ",
        "with depletion via survival (no observed censoring).\n",
        bnt_label, ": RR=1 for days 0–13 post–dose1, RR=0.05 from day 14+.\n",
        "Window: ", format(analysis_start), "–", format(analysis_end),
        " | Countries: USA, ARGENTINA | Baseline negative subjects only."
      )
    ) +
    theme_minimal(base_size = 12)
}

print(plot_daily(series_by_arm, "Placebo"))
print(plot_daily(series_by_arm, bnt_label))
print(plot_cum(series_by_arm, "Placebo"))
print(plot_cum(series_by_arm, bnt_label))

# ------------------------------------------------------------
# Save series + quick summary
# ------------------------------------------------------------
out_series_path <- "expected_vs_observed_daily_by_arm.csv"
write_csv(series_by_arm, out_series_path)
message("Wrote daily expected/observed series to: ", out_series_path)

summary_tbl <- series_by_arm %>%
  group_by(ARM) %>%
  summarise(
    total_expected = sum(expected_daily),
    total_observed = sum(observed_daily),
    .groups = "drop"
  )
print(summary_tbl)
