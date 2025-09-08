# ============================================================
# Weekly cases per 100K (USA, Argentina) from JHU time-series
#   - Downloads once to jh_data/
#   - Lists U.S. states/territories for verification
#   - Aggregates national series and computes weekly per 100k
# Output columns:
#   COUNTRY, ISOWEEK, WEEKCASES, POP, WEEKLY_PER_100K
# ============================================================

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(tidyr)
  library(lubridate)
  library(stringr)
  library(ISOweek)   # ISOweek(), ISOweek2date()
  library(purrr)
})

# ------------------------------------------------------------
# Config
# ------------------------------------------------------------
dir.create("jh_data", showWarnings = FALSE, recursive = TRUE)

url_global <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/refs/heads/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"
url_us     <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/refs/heads/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv"

path_global <- file.path("jh_data", "time_series_covid19_confirmed_global.csv")
path_us     <- file.path("jh_data", "time_series_covid19_confirmed_US.csv")

USA_POP <- 331501080
ARG_POP <-  45376763

# ------------------------------------------------------------
# Download once if needed
# ------------------------------------------------------------
if (!file.exists(path_global)) {
  message("Downloading global file to: ", path_global)
  download.file(url_global, destfile = path_global, mode = "wb", quiet = TRUE)
} else {
  message("Using cached global file: ", path_global)
}

if (!file.exists(path_us)) {
  message("Downloading US file to: ", path_us)
  download.file(url_us, destfile = path_us, mode = "wb", quiet = TRUE)
} else {
  message("Using cached US file: ", path_us)
}

# ------------------------------------------------------------
# Helpers
# ------------------------------------------------------------
is_date_col <- function(nm) grepl("^\\d{1,2}/\\d{1,2}/\\d{2}$", nm)

to_daily_from_cumulative <- function(df, date_col = date, cum_col = cum_cases) {
  df %>%
    arrange({{date_col}}) %>%
    mutate(
      daily_cases = {{cum_col}} - dplyr::lag({{cum_col}}),
      daily_cases = dplyr::coalesce(daily_cases, {{cum_col}}),
      # guard against negative corrections
      daily_cases = pmax(daily_cases, 0)
    )
}

summarise_weekly <- function(df, country_name, pop) {
  df %>%
    mutate(iso_week = ISOweek::ISOweek(date)) %>%
    group_by(iso_week) %>%
    summarise(
      weekly_cases = sum(daily_cases, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    transmute(
      COUNTRY = country_name,
      ISOWEEK = iso_week,
      WEEKCASES = weekly_cases,
      POP = as.numeric(pop),
      WEEKLY_PER_100K = WEEKCASES / POP * 1e5
    )
}

# ------------------------------------------------------------
# Read US data, list states/territories, build national series
# ------------------------------------------------------------
us_raw <- readr::read_csv(path_us, show_col_types = FALSE, guess_max = 1e6)

# List states/territories for verification
us_states <- us_raw %>%
  distinct(Province_State) %>%
  arrange(Province_State)

message("\n=== U.S. States / Territories found in JHU file ===")
print(us_states, n = nrow(us_states))

# Pivot dates and aggregate cumulative counts to national level
us_long <- us_raw %>%
  pivot_longer(
    cols = names(us_raw)[is_date_col(names(us_raw))],
    names_to = "date_str",
    values_to = "cum_cases"
  ) %>%
  mutate(
    date = as.Date(date_str, format = "%m/%d/%y"),
    cum_cases = suppressWarnings(as.numeric(cum_cases))
  ) %>%
  group_by(date) %>%
  summarise(cum_cases = sum(cum_cases, na.rm = TRUE), .groups = "drop") %>%
  to_daily_from_cumulative(date, cum_cases)

weekly_usa <- summarise_weekly(us_long, country_name = "USA", pop = USA_POP)

# ------------------------------------------------------------
# Read GLOBAL data, extract Argentina, build national series
# ------------------------------------------------------------
glb_raw <- readr::read_csv(path_global, show_col_types = FALSE, guess_max = 1e6)

arg_long <- glb_raw %>%
  filter(`Country/Region` == "Argentina") %>%
  pivot_longer(
    cols = names(glb_raw)[is_date_col(names(glb_raw))],
    names_to = "date_str",
    values_to = "cum_cases"
  ) %>%
  mutate(
    date = as.Date(date_str, format = "%m/%d/%y"),
    cum_cases = suppressWarnings(as.numeric(cum_cases))
  ) %>%
  group_by(date) %>%
  summarise(cum_cases = sum(cum_cases, na.rm = TRUE), .groups = "drop") %>%
  to_daily_from_cumulative(date, cum_cases)

weekly_arg <- summarise_weekly(arg_long, country_name = "Argentina", pop = ARG_POP)

# ------------------------------------------------------------
# Combine & write
# ------------------------------------------------------------
weekly_out <- bind_rows(weekly_usa, weekly_arg) %>%
  arrange(COUNTRY, ISOWEEK)

out_path <- file.path("jh_data", "weekly_cases_by_countries.csv")
readr::write_csv(weekly_out, out_path)

message("\nWrote ", nrow(weekly_out), " rows to: ", out_path, "\n")
print(head(weekly_out, 12))
