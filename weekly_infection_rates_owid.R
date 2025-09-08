# Packages
library(readr)
library(dplyr)# Packages
library(readr)
library(dplyr)
library(lubridate)
library(ISOweek)   # ISOweek(), ISOweek2date()

# ------------------------------------------------------------------
# Download once to owid_data/owid_cases_data.csv
# ------------------------------------------------------------------
dir.create("owid_data", showWarnings = FALSE, recursive = TRUE)
owid_path <- file.path("owid_data", "owid_cases_data.csv")

# Your URL (compact file)
owid_url  <- "https://catalog.ourworldindata.org/garden/covid/latest/compact/compact.csv"

if (!file.exists(owid_path)) {
  message("Downloading OWID data to: ", owid_path)
  download.file(owid_url, destfile = owid_path, mode = "wb", quiet = TRUE)
} else {
  message("Using cached file: ", owid_path)
}

# ------------------------------------------------------------------
# Read + normalise + diagnostics
# ------------------------------------------------------------------
owid <- readr::read_csv(
  owid_path,
  show_col_types = FALSE,
  progress = FALSE,
  guess_max = 1e6
)

message("Rows: ", nrow(owid), "  |  Cols: ", ncol(owid))
message("First 40 columns: ", paste(head(names(owid), 40), collapse = ", "), ", ...")

# Ensure we have the fields we need (population may be absent in compact)
required_min <- c("country", "date", "new_cases")
missing_min  <- setdiff(required_min, names(owid))
if (length(missing_min)) {
  stop("Missing required columns: ", paste(missing_min, collapse = ", "))
}

# Coerce types
owid <- owid %>%
  mutate(
    country  = as.character(country),
    date      = as.Date(date),
    new_cases = suppressWarnings(as.numeric(new_cases)),
    # new_cases_per_million used as fallback for rates if population is missing
    new_cases_per_million = suppressWarnings(as.numeric(.data[["new_cases_per_million"]]))
  )

# ------------------------------------------------------------------
# Weekly aggregation for Argentina & United States (Jul–Dec 2020)
# ------------------------------------------------------------------
target_countries <- c("Argentina", "United States")
message("Will filter for countrys: ", paste(target_countries, collapse = " & "))
message("Length(country) = ", length(owid$country), " | Length(date) = ", length(owid$date))

has_population <- "population" %in% names(owid)

weekly <- owid %>%
  filter(
    country %in% target_countries,
    date >= as.Date("2020-07-01"),
    date <= as.Date("2020-12-31")
  ) %>%
  mutate(
    iso_week = ISOweek::ISOweek(date)
  ) %>%
  group_by(country, iso_week) %>%
  summarise(
    weekly_cases = sum(coalesce(new_cases, 0), na.rm = TRUE),
    # population may be missing; if so computes
    pop = if (has_population) suppressWarnings(max(population, na.rm = TRUE)) else NA_real_,
    pop = ifelse(is.finite(pop), pop, NA_real_),
    # fallback sum of per-million if no population
    new_cases_per_million_sum = sum(coalesce(new_cases_per_million, 0), na.rm = TRUE),
    week_start = ISOweek2date(paste0(iso_week, "-1")), # Monday
    week_end   = ISOweek2date(paste0(iso_week, "-7")), # Sunday
    .groups = "drop"
  ) %>%
  mutate(
    weekly_per_100k = ifelse(!is.na(pop),
                             weekly_cases / pop * 1e5,
                             new_cases_per_million_sum / 10)
  ) %>%
  arrange(country, week_start)
print(head(weekly, 10))

# Finetunes output.
weekly_out <- weekly %>%
  mutate(country = if_else(country == "United States", "USA", country)) %>%
  transmute(
    COUNTRY = country,
    ISOWEEK = iso_week,
    WEEKCASES = weekly_cases,
    POP = pop,
    WEEKLY_PER_100K = weekly_per_100k
  )

print(head(weekly_out, 10))


# Writes to local file
out_path <- file.path("owid_data", "weekly_cases_by_countries.csv")
readr::write_csv(weekly_out, out_path)
message("Wrote ", nrow(weekly_out), " rows to: ", out_path)

