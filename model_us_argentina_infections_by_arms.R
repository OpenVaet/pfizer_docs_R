# ============================================================
# Expected vs Observed Symptomatic PCR+ by Arm (USA/ARGENTINA)
# Uses:
#   - baseline_negative_subjects_with_symptomatic_pcr.csv
#   - owid_data/weekly_cases_by_countries.csv
# ============================================================

# Packages
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(lubridate)
  library(ISOweek)
  library(stringr)
  library(ggplot2)
  library(purrr)
})

# ------------------------------------------------------------
# Config
# ------------------------------------------------------------
DATA_CUTOFF <- as.Date("2020-11-14")
owid_weekly_path <- file.path("jh_data", "weekly_cases_by_countries.csv")
baseline_path    <- "baseline_negative_subjects_with_symptomatic_pcr.csv"

if (!file.exists(owid_weekly_path)) stop("Missing: ", owid_weekly_path)
if (!file.exists(baseline_path))    stop("Missing: ", baseline_path)

# ------------------------------------------------------------
# Load prior outputs
# ------------------------------------------------------------
weekly <- readr::read_csv(owid_weekly_path, show_col_types = FALSE)

# weekly columns expected: COUNTRY, ISOWEEK, WEEKCASES, POP, WEEKLY_PER_100K
# Normalize country naming to uppercase for joining logic
weekly <- weekly %>%
  mutate(
    COUNTRY = toupper(COUNTRY),
    # Recover week boundaries
    week_start = ISOweek2date(paste0(ISOWEEK, "-1")),
    week_end   = ISOweek2date(paste0(ISOWEEK, "-7"))
  )

baseline <- readr::read_csv(baseline_path, show_col_types = FALSE)

# Ensure key fields exist (names come from your earlier pipeline)
needed_cols <- c("SUBJID", "COUNTRY", "ARM", "VAX101DT", "FIRST_SYMPT_PCR_DATE")
missing_cols <- setdiff(needed_cols, names(baseline))
if (length(missing_cols)) stop("baseline CSV missing columns: ", paste(missing_cols, collapse = ", "))

# Coerce types & normalize country casing to uppercase (per instructions)
baseline <- baseline %>%
  mutate(
    COUNTRY = toupper(as.character(COUNTRY)),
    ARM = as.character(ARM),
    VAX101DT = as.Date(VAX101DT),
    FIRST_SYMPT_PCR_DATE = suppressWarnings(as.Date(FIRST_SYMPT_PCR_DATE))
  )

# ------------------------------------------------------------
# Filter to USA / ARGENTINA and baseline-neg set already prepared upstream
# ------------------------------------------------------------
baseline_f <- baseline %>%
  filter(COUNTRY %in% c("USA", "ARGENTINA")) %>%
  # Only keep subjects who have a valid dose 1 date and are on/before the cutoff window
  filter(!is.na(VAX101DT), VAX101DT <= DATA_CUTOFF)

if (nrow(baseline_f) == 0) stop("No subjects after filtering to USA/ARGENTINA with valid VAX101DT.")

# ------------------------------------------------------------
# Build daily infection rate per country from weekly OWID rates
# - Input: WEEKLY_PER_100K (per 100k per week)
# - Daily per person risk ≈ (WEEKLY_PER_100K / 7) / 100,000
# ------------------------------------------------------------
# Restrict OWID to countries of interest and weeks overlapping analysis window
owid_countries <- c("USA", "ARGENTINA")
weekly_f <- weekly %>%
  filter(COUNTRY %in% owid_countries)

if (nrow(weekly_f) == 0) stop("OWID weekly file has no rows for USA/ARGENTINA.")

# Expand week to days, assign constant daily rate within each week
owid_daily <- weekly_f %>%
  mutate(
    daily_per_100k = WEEKLY_PER_100K / 7
  ) %>%
  rowwise() %>%
  mutate(date = list(seq(week_start, week_end, by = "day"))) %>%
  ungroup() %>%
  tidyr::unnest(date) %>%
  transmute(
    COUNTRY,
    date,
    # Convert to daily per-person risk
    daily_risk = (daily_per_100k / 1e5)
  )

if (nrow(owid_daily) == 0) stop("Failed to build daily OWID risk series.")

# ------------------------------------------------------------
# Analysis window = overlap of (subject follow-up window) and (OWID daily dates) and cutoff
# Start at min(VAX101DT) among included subjects; end at DATA_CUTOFF
# ------------------------------------------------------------
subjects_start <- min(baseline_f$VAX101DT, na.rm = TRUE)
owid_start <- min(owid_daily$date, na.rm = TRUE)
owid_end   <- max(owid_daily$date, na.rm = TRUE)

analysis_start <- max(subjects_start, owid_start)
analysis_end   <- min(DATA_CUTOFF, owid_end)

if (analysis_start > analysis_end) {
  stop("Empty analysis window: ", format(analysis_start), " .. ", format(analysis_end))
}

date_seq <- seq(analysis_start, analysis_end, by = "day")
message("Analysis window: ", format(analysis_start), " .. ", format(analysis_end),
        " (", length(date_seq), " days)")

# ------------------------------------------------------------
# Risk-set sizes per ARM & COUNTRY per day
# - pre14 window: [VAX101DT .. VAX101DT + 13] inclusive
# - post14 window: [VAX101DT + 14 .. DATA_CUTOFF] inclusive
# We count subjects present in each window for each day.
# Efficient "event-stream" (difference array) approach to avoid expanding subject x day.
# ------------------------------------------------------------

make_interval_counts <- function(df, start_col, end_col, date_seq) {
  # df must have ARM, COUNTRY, start_col, end_col
  df <- df %>%
    mutate(
      start = !!rlang::enquo(start_col),
      end   = !!rlang::enquo(end_col)
    ) %>%
    # Clamp to analysis window
    mutate(
      start = pmax(start, min(date_seq), na.rm = TRUE),
      end   = pmin(end,   max(date_seq), na.rm = TRUE)
    ) %>%
    filter(start <= end)

  # Build +1 on start, -1 on end+1
  changes <- bind_rows(
    df %>% transmute(ARM, COUNTRY, date = start, delta = 1L),
    df %>% transmute(ARM, COUNTRY, date = end + 1, delta = -1L)
  ) %>%
    # Keep only changes within [min(date_seq), max(date_seq)+1]
    filter(date >= min(date_seq), date <= (max(date_seq) + 1)) %>%
    group_by(ARM, COUNTRY, date) %>%
    summarise(delta = sum(delta), .groups = "drop")

  # Complete grid and cumsum to daily counts
  counts <- changes %>%
    tidyr::complete(ARM, COUNTRY, date = c(date_seq, max(date_seq) + 1), fill = list(delta = 0L)) %>%
    arrange(ARM, COUNTRY, date) %>%
    group_by(ARM, COUNTRY) %>%
    mutate(count = cumsum(delta)) %>%
    ungroup() %>%
    filter(date %in% date_seq) %>%  # drop the +1 endpoint
    select(ARM, COUNTRY, date, count)

  counts
}

# Pre-14d window
pre14_intervals <- baseline_f %>%
  transmute(
    ARM, COUNTRY,
    pre_start = VAX101DT,
    pre_end   = pmin(VAX101DT + 13, analysis_end)
  )
pre14_counts <- make_interval_counts(pre14_intervals, pre_start, pre_end, date_seq)

# Post-14d window
post14_intervals <- baseline_f %>%
  transmute(
    ARM, COUNTRY,
    post_start = VAX101DT + 14,
    post_end   = analysis_end
  ) %>%
  filter(post_start <= post_end)

post14_counts <- make_interval_counts(post14_intervals, post_start, post_end, date_seq)

# Total at-risk (after dose 1) = pre14 + post14
total_counts <- pre14_counts %>%
  full_join(post14_counts, by = c("ARM", "COUNTRY", "date"), suffix = c("_pre", "_post")) %>%
  mutate(
    count_pre  = coalesce(count_pre, 0L),
    count_post = coalesce(count_post, 0L),
    count_total = count_pre + count_post
  )

# ------------------------------------------------------------
# Expected daily infections per ARM & COUNTRY
# - Placebo: expected = IR * (count_total) * 1
# - BNT162b2 Phase 2/3 (30 mcg): expected = IR * (count_pre)*1 + IR*(count_post)*0.05
# ------------------------------------------------------------
owid_daily_u <- owid_daily %>%
  mutate(COUNTRY = toupper(COUNTRY)) %>%
  filter(date %in% date_seq)

expected_by_group <- total_counts %>%
  left_join(owid_daily_u, by = c("COUNTRY", "date")) %>%
  mutate(
    daily_risk = coalesce(daily_risk, 0), # if missing, assume 0 to avoid NA
    is_bnt = ARM == "BNT162b2 Phase 2/3 (30 mcg)",
    expected_daily =
      ifelse(is_bnt,
             daily_risk * (count_pre * 1 + count_post * 0.05),
             daily_risk * (count_total * 1))
  )

# Aggregate to ARM (sum over countries)
expected_by_arm <- expected_by_group %>%
  group_by(ARM, date) %>%
  summarise(expected_daily = sum(expected_daily, na.rm = TRUE), .groups = "drop") %>%
  arrange(ARM, date) %>%
  group_by(ARM) %>%
  mutate(expected_cum = cumsum(expected_daily)) %>%
  ungroup()

# ------------------------------------------------------------
# Observed daily infections per ARM (first symptomatic PCR+)
# ------------------------------------------------------------
observed_by_arm <- baseline_f %>%
  filter(!is.na(FIRST_SYMPT_PCR_DATE),
         FIRST_SYMPT_PCR_DATE >= analysis_start,
         FIRST_SYMPT_PCR_DATE <= analysis_end) %>%
  transmute(ARM, date = FIRST_SYMPT_PCR_DATE) %>%
  count(ARM, date, name = "observed_daily") %>%
  # ensure full grid of dates so we can plot aligned series
  tidyr::complete(ARM, date = date_seq, fill = list(observed_daily = 0)) %>%
  arrange(ARM, date) %>%
  group_by(ARM) %>%
  mutate(observed_cum = cumsum(observed_daily)) %>%
  ungroup()

# Merge expected & observed
series_by_arm <- expected_by_arm %>%
  full_join(observed_by_arm, by = c("ARM", "date")) %>%
  arrange(ARM, date) %>%
  mutate(
    expected_daily  = coalesce(expected_daily, 0),
    expected_cum    = zoo::na.locf(expected_cum, na.rm = FALSE),
    expected_cum    = coalesce(expected_cum, 0),
    observed_daily  = coalesce(observed_daily, 0),
    observed_cum    = coalesce(observed_cum, 0)
  )

# ------------------------------------------------------------
# Plots (one per arm): Daily counts and Cumulative counts overlays
# We'll produce the "daily" comparison as requested; feel free to switch to cumulative if preferred.
# ------------------------------------------------------------
plot_one_arm <- function(df, arm_label) {
  df_arm <- df %>% filter(ARM == arm_label)

  if (nrow(df_arm) == 0) {
    message("No data for arm: ", arm_label)
    return(invisible(NULL))
  }

  p_daily <- ggplot(df_arm, aes(x = date)) +
    geom_line(aes(y = expected_daily, linetype = "Expected")) +
    geom_line(aes(y = observed_daily, linetype = "Observed")) +
    scale_linetype_manual(values = c("Expected" = "solid", "Observed" = "dashed")) +
    labs(
      title = paste0("Daily symptomatic PCR+ — Expected vs Observed (", arm_label, ")"),
      x = "Date",
      y = "Count",
      linetype = NULL,
      caption = paste0("Expected uses general-population daily infection risk from OWID weekly rates\n",
                       "BNT162b2 assumed RR=1 for days 0–13 after dose1; RR=0.05 from day 14+.\n",
                       "Window: ", format(analysis_start), "–", format(analysis_end),
                       " | Countries: USA, ARGENTINA | Baseline negative subjects only.")
    ) +
    theme_minimal(base_size = 12)

  print(p_daily)
}

# Draw the two requested plots
plot_one_arm(series_by_arm, "Placebo")
plot_one_arm(series_by_arm, "BNT162b2 Phase 2/3 (30 mcg)")

# ------------------------------------------------------------
# Also write out the time series for inspection
# ------------------------------------------------------------
out_series_path <- "expected_vs_observed_daily_by_arm.csv"
readr::write_csv(series_by_arm, out_series_path)
message("Wrote daily expected/observed series to: ", out_series_path)

# Quick console summary
summary_tbl <- series_by_arm %>%
  group_by(ARM) %>%
  summarise(
    total_expected = sum(expected_daily),
    total_observed = sum(observed_daily),
    .groups = "drop"
  )
print(summary_tbl)

# ------------------------------------------------------------
# Cumulative plots (one per arm): Expected vs Observed
# ------------------------------------------------------------
plot_cum_one_arm <- function(df, arm_label) {
  df_arm <- df %>% filter(ARM == arm_label)

  if (nrow(df_arm) == 0) {
    message("No data for arm: ", arm_label)
    return(invisible(NULL))
  }

  p_cum <- ggplot(df_arm, aes(x = date)) +
    geom_line(aes(y = expected_cum, linetype = "Expected")) +
    geom_line(aes(y = observed_cum, linetype = "Observed")) +
    scale_linetype_manual(values = c("Expected" = "solid", "Observed" = "dashed")) +
    labs(
      title = paste0("Cumulative symptomatic PCR+ — Expected vs Observed (", arm_label, ")"),
      x = "Date",
      y = "Cumulative count",
      linetype = NULL,
      caption = paste0("Expected uses general-population daily infection risk from OWID weekly rates\n",
                       "BNT162b2 assumed RR=1 for days 0–13 after dose1; RR=0.05 from day 14+.\n",
                       "Window: ", format(analysis_start), "–", format(analysis_end),
                       " | Countries: USA, ARGENTINA | Baseline negative subjects only.")
    ) +
    theme_minimal(base_size = 12)

  print(p_cum)
}

# Draw the two cumulative plots
plot_cum_one_arm(series_by_arm, "Placebo")
plot_cum_one_arm(series_by_arm, "BNT162b2 Phase 2/3 (30 mcg)")