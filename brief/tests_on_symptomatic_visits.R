# =====================================================================
# COVID-19 Vaccine Trial Analysis: Symptomatic Visits and Testing Rates
# =====================================================================
# This script analyzes the relationship between symptomatic visits and 
# COVID-19 testing rates in vaccine trial participants, comparing 
# treatment and placebo groups across different time periods.
# =====================================================================

# ---------------------------------------------------------------------
# 1. SETUP AND CONFIGURATION
# ---------------------------------------------------------------------

# Load required libraries
library(haven)      # For reading XPT files
library(dplyr)      # For data manipulation
library(lubridate)  # For date handling
library(furrr)      # For parallel processing
library(stringr)    # For string manipulation

# Define file paths
DATA_PATH <- "xpt_data/"

# Primary data files
FILES <- list(
  adsl = paste0(DATA_PATH, "FDA-CBER-2021-5683-1066333-1067534_125742_S6_M5_c4591001-A-D-adsl.xpt"),
  symptoms = paste0(DATA_PATH, "FDA-CBER-2021-5683-0663135-0671344-125742_S1_M5_c4591001-A-D-adsympt.xpt"),
  symptoms_sup = paste0(DATA_PATH, "FDA-CBER-2021-5683-0539816-0593326-125742_S1_M5_c4591001-01-S-Supp-D-face.xpt"),
  tests = paste0(DATA_PATH, "FDA-CBER-2021-5683-0282366 to -0285643_125742_S1_M5_c4591001-S-D-mb.xpt"),
  html_template = paste0("brief/", "chi_square_template.html")
)

# Analysis parameters
EXCLUDED_SUBJECTS <- c(10561101, 11331382, 11101123, 11331405, 11491117, 
                       12691090, 12691070, 11351357, 11341006, 10891112, 
                       11231105, 10711213)
MIN_AGE <- 16
CUTOFF_DATE_MARCH <- "2021-03-13"
CUTOFF_DATE_NOV <- "2020-11-14"

# Output files
OUTPUT_FILES <- list(
  march_csv = "phase_3_subjects_sympto_visits.csv",
  nov_csv = "phase_3_subjects_sympto_visits_nov_14.csv",
  march_html = "local_test_chi_sq_march_13.html",
  nov_html = "local_test_chi_sq_nov_14.html"
)

# ---------------------------------------------------------------------
# 2. DATA LOADING AND INITIAL PROCESSING
# ---------------------------------------------------------------------

cat("Loading and processing ADSL data...\n")

# Load and filter the ADSL (subject-level) data
adsl_data <- read_xpt(FILES$adsl)
cat(sprintf("Initial ADSL records: %d\n", nrow(adsl_data)))

# Apply exclusion criteria
randomized_pop <- adsl_data %>%
  # Remove excluded subjects per ADRG
  filter(!(SUBJID %in% EXCLUDED_SUBJECTS)) %>%
  # Remove Phase 1 participants
  filter(PHASE != "Phase 1") %>%
  # Include only participants aged 16 or older
  filter(as.numeric(AGETR01) >= MIN_AGE) %>%
  # Include only randomized participants
  filter(RANDNO != "") %>%
  # Select relevant columns for analysis
  select(SUBJID, SITEID, COUNTRY, RFICDT, ARM, PHASE,
         AGE, AGETR01, RANDDT, RANDNO, AGEGR1,
         AGEGR2, AGEGR3, UNBLNDDT, SEX, DTHDT,
         VAX101DT, VAX102DT, VAX201DT, VAX202DT, RACE, ETHNIC)

cat(sprintf("Randomized population after filtering: %d\n", nrow(randomized_pop)))

# ---------------------------------------------------------------------
# 3. SYMPTOMS DATA PROCESSING
# ---------------------------------------------------------------------

cat("\nProcessing symptoms data...\n")

# Load primary symptoms data
symptoms <- read_xpt(FILES$symptoms)
cat(sprintf("Initial symptoms records: %d\n", nrow(symptoms)))

# Filter and process symptoms data
symptoms_filtered <- symptoms %>%
  # Include only subjects in randomized population
  filter(SUBJID %in% randomized_pop$SUBJID) %>%
  # Focus on disease signs and symptoms
  filter(PARCAT1 == "SIGNS AND SYMPTOMS OF DISEASE") %>%
  # Include only positive findings
  filter(AVALC == "Y") %>%
  # Select relevant columns
  select(SUBJID, SITEID, ARM, AVISIT, PARAM, VISITNUM, VISIT, ADT, ASTDT)

cat(sprintf("Filtered symptoms records: %d\n", nrow(symptoms_filtered)))

# Load supplementary symptoms data (first symptom dates)
symptoms_sup <- read_xpt(FILES$symptoms_sup) %>%
  filter(FATEST == "First Symptom Date") %>%
  # Extract SUBJID from USUBJID
  mutate(SUBJID = str_extract(USUBJID, "\\d+$")) %>%
  # Include only subjects in randomized population
  filter(SUBJID %in% randomized_pop$SUBJID) %>%
  # Select relevant columns
  select(SUBJID, FAORRES, VISIT)

cat(sprintf("Supplementary symptoms records: %d\n", nrow(symptoms_sup)))

# ---------------------------------------------------------------------
# 4. CREATE SYMPTOMATIC VISITS DATASET
# ---------------------------------------------------------------------

cat("\nCreating symptomatic visits dataset...\n")

# Aggregate symptoms by subject and visit
subjects_sympto_visits <- symptoms_filtered %>%
  group_by(SUBJID, VISIT, ARM, SITEID) %>%
  summarise(
    EARLIESTSYMPTDT = min(ADT, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # Join with supplementary first symptom dates
  left_join(
    symptoms_sup %>% mutate(FAORRES = as.Date(FAORRES)),
    by = "SUBJID",
    relationship = "many-to-many"
  ) %>%
  # Use earliest available date
  mutate(
    EARLIESTDT = coalesce(FAORRES, EARLIESTSYMPTDT),
    VISIT = VISIT.x
  ) %>%
  select(-VISIT.x, -VISIT.y)

# Check for data consistency
exceptions <- subjects_sympto_visits %>%
  filter(EARLIESTSYMPTDT < EARLIESTDT)

if (nrow(exceptions) > 0) {
  cat(sprintf("WARNING: Found %d exceptions where EARLIESTSYMPTDT < EARLIESTDT\n", 
              nrow(exceptions)))
  # Handle exceptions by taking minimum dates
  exceptions_summary <- exceptions %>%
    group_by(SUBJID, VISIT) %>%
    summarize(
      EARLIESTSYMPTDT = min(EARLIESTSYMPTDT),
      FAORRES = min(FAORRES, na.rm = TRUE),
      EARLIESTDT = min(EARLIESTDT),
      .groups = "drop"
    )
} else {
  cat("Data consistency check passed: All EARLIESTSYMPTDT >= EARLIESTDT\n")
}

# Ensure unique rows per subject-visit combination
subjects_sympto_visits <- subjects_sympto_visits %>%
  group_by(SUBJID, ARM, SITEID, VISIT) %>%
  summarize(
    EARLIESTSYMPTDT = min(EARLIESTSYMPTDT),
    FAORRES = if (all(is.na(FAORRES))) NA else min(FAORRES, na.rm = TRUE),
    EARLIESTDT = min(EARLIESTDT),
    .groups = "drop"
  ) %>%
  select(-EARLIESTSYMPTDT)

# ---------------------------------------------------------------------
# 5. COVID-19 TEST DATA PROCESSING
# ---------------------------------------------------------------------

cat("\nProcessing COVID-19 test data...\n")

# Load and filter test data
tests <- read_xpt(FILES$tests) %>%
  # Filter for COVID-19 tests only
  filter(MBTEST %in% c("Cepheid RT-PCR assay for SARS-CoV-2", 
                       "SEVERE ACUTE RESP SYNDROME CORONAVIRUS 2")) %>%
  # Extract SUBJID from USUBJID
  mutate(SUBJID = str_extract(USUBJID, "\\d+$")) %>%
  # Select relevant columns
  select(SUBJID, VISIT, MBTEST, MBORRES, MBDTC)

# Standardize test result values
tests$MBORRES <- case_when(
  tests$MBORRES == "NEGATIVE" ~ "NEG",
  tests$MBORRES == "POSITIVE" ~ "POS",
  tests$MBORRES == "INDETERMINATE" ~ "IND",
  TRUE ~ tests$MBORRES
)

# Filter for COVID visits only
tests_filtered <- tests %>%
  filter(grepl("COVID_", VISIT))

cat(sprintf("COVID test records: %d\n", nrow(tests_filtered)))

# Separate central and local tests
central_tests <- tests_filtered %>%
  filter(MBTEST == 'Cepheid RT-PCR assay for SARS-CoV-2') %>%
  select(SUBJID, VISIT, MBORRES, MBDTC)

local_tests <- tests_filtered %>%
  filter(MBTEST == 'SEVERE ACUTE RESP SYNDROME CORONAVIRUS 2') %>%
  select(SUBJID, VISIT, MBORRES, MBDTC)

cat(sprintf("Central tests: %d, Local tests: %d\n", 
            nrow(central_tests), nrow(local_tests)))

# ---------------------------------------------------------------------
# 6. ANALYSIS FUNCTIONS
# ---------------------------------------------------------------------

# Function to merge test data with symptomatic visits
merge_test_data <- function(sympto_visits, central_tests, local_tests, cutoff_date) {
  # Filter by cutoff date
  sympto_visits_filtered <- sympto_visits %>%
    filter(ymd(EARLIESTDT) <= ymd(cutoff_date))
  
  central_tests_filtered <- central_tests %>%
    filter(ymd(MBDTC) <= ymd(cutoff_date))
  
  local_tests_filtered <- local_tests %>%
    filter(ymd(MBDTC) <= ymd(cutoff_date))
  
  # Merge with test data
  result <- sympto_visits_filtered %>%
    left_join(
      central_tests_filtered %>% 
        group_by(SUBJID, VISIT) %>% 
        summarise(
          EARLIESTCENTRALDT = min(MBDTC),
          EARLIESTCENTRALPOSDT = ifelse(
            length(MBDTC[MBORRES == "POS"]) == 0, 
            NA, 
            min(MBDTC[MBORRES == "POS"])
          ),
          .groups = "drop"
        ),
      by = c("SUBJID", "VISIT")
    ) %>%
    left_join(
      local_tests_filtered %>% 
        group_by(SUBJID, VISIT) %>% 
        summarise(
          EARLIESTLOCALDT = min(MBDTC),
          EARLIESTLOCALPOSDT = ifelse(
            length(MBDTC[MBORRES == "POS"]) == 0, 
            NA, 
            min(MBDTC[MBORRES == "POS"])
          ),
          .groups = "drop"
        ),
      by = c("SUBJID", "VISIT")
    )
  
  # Calculate days from symptoms to test
  result <- result %>%
    mutate(
      EARLIESTCENTRALDAYSTOSYMPT = ifelse(
        !is.na(EARLIESTCENTRALDT), 
        as.numeric(as.Date(EARLIESTCENTRALDT) - as.Date(EARLIESTDT)), 
        NA
      ),
      EARLIESTCENTRALPOSDAYSTOSYMPT = ifelse(
        !is.na(EARLIESTCENTRALPOSDT), 
        as.numeric(as.Date(EARLIESTCENTRALPOSDT) - as.Date(EARLIESTDT)), 
        NA
      ),
      EARLIESTLOCALDAYSTOSYMPT = ifelse(
        !is.na(EARLIESTLOCALDT), 
        as.numeric(as.Date(EARLIESTLOCALDT) - as.Date(EARLIESTDT)), 
        NA
      ),
      EARLIESTLOCALPOSDAYSTOSYMPT = ifelse(
        !is.na(EARLIESTLOCALPOSDT), 
        as.numeric(as.Date(EARLIESTLOCALPOSDT) - as.Date(EARLIESTDT)), 
        NA
      )
    )
  
  return(result)
}

# Function to perform chi-square analysis
perform_chi_square_analysis <- function(data, test_type = "local") {
  # Determine which test column to use
  test_col <- if (test_type == "central") "EARLIESTCENTRALDT" else "EARLIESTLOCALDT"
  
  # Calculate test counts by ARM
  arm_test_counts <- data %>%
    mutate(has_test = !is.na(.data[[test_col]])) %>%
    group_by(ARM) %>%
    summarise(
      total_visits = n(),
      test_visits = sum(has_test),
      test_percentage = (sum(has_test) / n()) * 100,
      .groups = "drop"
    )
  
  # Create contingency table
  contingency_table <- matrix(
    c(arm_test_counts$test_visits[1],
      arm_test_counts$total_visits[1] - arm_test_counts$test_visits[1],
      arm_test_counts$test_visits[2],
      arm_test_counts$total_visits[2] - arm_test_counts$test_visits[2]),
    nrow = 2, 
    byrow = TRUE
  )
  
  rownames(contingency_table) <- arm_test_counts$ARM
  colnames(contingency_table) <- c(
    paste(stringr::str_to_title(test_type), "Test"),
    paste("No", stringr::str_to_title(test_type), "Test")
  )
  
  # Perform chi-square test
  chi_sq_result <- chisq.test(contingency_table)
  
  return(list(
    counts = arm_test_counts,
    table = contingency_table,
    test = chi_sq_result
  ))
}

# Function to generate enhanced HTML report
generate_html_report <- function(chi_sq_results, template_file, output_file, cutoff_date) {
  # Read template
  template <- readLines(template_file)
  
  # Extract values
  cont_table <- chi_sq_results$table
  chi_test <- chi_sq_results$test
  
  # Calculate percentages for display
  row1_total <- sum(cont_table[1,])
  row2_total <- sum(cont_table[2,])
  row1_pct1 <- round(100 * cont_table[1,1] / row1_total, 2)
  row1_pct2 <- round(100 * cont_table[1,2] / row1_total, 2)
  row2_pct1 <- round(100 * cont_table[2,1] / row2_total, 2)
  row2_pct2 <- round(100 * cont_table[2,2] / row2_total, 2)
  
  # Enhanced HTML template with better styling
  enhanced_html <- paste0('
<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Chi-Square Test Results - ', cutoff_date, '</title>
    <style>
        body {
            font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, "Helvetica Neue", Arial, sans-serif;
            line-height: 1.6;
            color: #333;
            max-width: 900px;
            margin: 0 auto;
            padding: 20px;
            background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
            min-height: 100vh;
        }
        .container {
            background: white;
            border-radius: 12px;
            padding: 40px;
            box-shadow: 0 20px 60px rgba(0,0,0,0.3);
        }
        h1 {
            color: #2c3e50;
            text-align: center;
            margin-bottom: 10px;
            font-size: 28px;
        }
        .subtitle {
            text-align: center;
            color: #7f8c8d;
            margin-bottom: 30px;
            font-size: 16px;
        }
        .stats-grid {
            display: grid;
            grid-template-columns: repeat(auto-fit, minmax(200px, 1fr));
            gap: 20px;
            margin-bottom: 30px;
        }
        .stat-card {
            background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
            color: white;
            padding: 20px;
            border-radius: 8px;
            text-align: center;
        }
        .stat-value {
            font-size: 32px;
            font-weight: bold;
            margin-bottom: 5px;
        }
        .stat-label {
            font-size: 14px;
            opacity: 0.9;
        }
        table {
            width: 100%;
            border-collapse: collapse;
            margin: 30px 0;
            background: white;
            box-shadow: 0 5px 15px rgba(0,0,0,0.08);
            border-radius: 8px;
            overflow: hidden;
        }
        th {
            background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
            color: white;
            padding: 15px;
            text-align: left;
            font-weight: 600;
            text-transform: uppercase;
            font-size: 12px;
            letter-spacing: 1px;
        }
        td {
            padding: 15px;
            border-bottom: 1px solid #ecf0f1;
        }
        tr:last-child td {
            border-bottom: none;
        }
        tr:hover {
            background-color: #f8f9fa;
        }
        .cell-value {
            font-size: 18px;
            font-weight: 600;
            color: #2c3e50;
        }
        .cell-percentage {
            font-size: 12px;
            color: #7f8c8d;
            margin-top: 4px;
        }
        .result-section {
            background: #f8f9fa;
            border-left: 4px solid #667eea;
            padding: 20px;
            margin: 30px 0;
            border-radius: 4px;
        }
        .result-title {
            font-size: 18px;
            font-weight: 600;
            color: #2c3e50;
            margin-bottom: 15px;
        }
        .p-value {
            font-size: 24px;
            font-weight: bold;
            color: ', ifelse(chi_test$p.value < 0.05, '#e74c3c', '#27ae60'), ';
        }
        .significance {
            margin-top: 10px;
            padding: 10px;
            background: ', ifelse(chi_test$p.value < 0.05, '#ffe5e5', '#e5ffe5'), ';
            border-radius: 4px;
            color: ', ifelse(chi_test$p.value < 0.05, '#c0392b', '#229954'), ';
        }
        .footer {
            text-align: center;
            margin-top: 40px;
            color: #7f8c8d;
            font-size: 14px;
        }
    </style>
</head>
<body>
    <div class="container">
        <h1>Chi-Square Test of Independence</h1>
        <div class="subtitle">Testing Rate Analysis - Data through ', cutoff_date, '</div>
        
        <div class="stats-grid">
            <div class="stat-card">
                <div class="stat-value">', format(chi_test$statistic, digits = 4), '</div>
                <div class="stat-label">χ² Statistic</div>
            </div>
            <div class="stat-card">
                <div class="stat-value">', chi_test$parameter, '</div>
                <div class="stat-label">Degrees of Freedom</div>
            </div>
            <div class="stat-card">
                <div class="stat-value">', format(chi_test$p.value, digits = 4), '</div>
                <div class="stat-label">P-Value</div>
            </div>
        </div>
        
        <table>
            <thead>
                <tr>
                    <th>Treatment Group</th>
                    <th>', colnames(cont_table)[1], '</th>
                    <th>', colnames(cont_table)[2], '</th>
                    <th>Total</th>
                </tr>
            </thead>
            <tbody>
                <tr>
                    <td><strong>', rownames(cont_table)[1], '</strong></td>
                    <td>
                        <div class="cell-value">', cont_table[1,1], '</div>
                        <div class="cell-percentage">(', row1_pct1, '%)</div>
                    </td>
                    <td>
                        <div class="cell-value">', cont_table[1,2], '</div>
                        <div class="cell-percentage">(', row1_pct2, '%)</div>
                    </td>
                    <td><strong>', row1_total, '</strong></td>
                </tr>
                <tr>
                    <td><strong>', rownames(cont_table)[2], '</strong></td>
                    <td>
                        <div class="cell-value">', cont_table[2,1], '</div>
                        <div class="cell-percentage">(', row2_pct1, '%)</div>
                    </td>
                    <td>
                        <div class="cell-value">', cont_table[2,2], '</div>
                        <div class="cell-percentage">(', row2_pct2, '%)</div>
                    </td>
                    <td><strong>', row2_total, '</strong></td>
                </tr>
                <tr style="background-color: #f8f9fa;">
                    <td><strong>Total</strong></td>
                    <td><strong>', sum(cont_table[,1]), '</strong></td>
                    <td><strong>', sum(cont_table[,2]), '</strong></td>
                    <td><strong>', sum(cont_table), '</strong></td>
                </tr>
            </tbody>
        </table>
        
        <div class="result-section">
            <div class="result-title">Statistical Test Result</div>
            <p><strong>Chi-square statistic (χ²):</strong> ', format(chi_test$statistic, digits = 4), '</p>
            <p><strong>Degrees of freedom:</strong> ', chi_test$parameter, '</p>
            <p><strong>P-value:</strong> <span class="p-value">', format(chi_test$p.value, digits = 4), '</span></p>
            
            <div class="significance">
                <strong>Interpretation:</strong> ',
                ifelse(chi_test$p.value < 0.05,
                       'The difference in testing rates between treatment groups is statistically significant (p < 0.05).',
                       'The difference in testing rates between treatment groups is not statistically significant (p ≥ 0.05).'),
            '</div>
        </div>
        
        <div class="footer">
            <p>Analysis performed on ', Sys.Date(), '</p>
            <p>Data cutoff date: ', cutoff_date, '</p>
        </div>
    </div>
</body>
</html>
  ')
  
  # Write the HTML file
  writeLines(enhanced_html, output_file)
  cat(sprintf("HTML report saved to: %s\n", output_file))
}

# ---------------------------------------------------------------------
# 7. MAIN ANALYSIS EXECUTION
# ---------------------------------------------------------------------

cat("\n" , strrep("=", 60), "\n")
cat("ANALYSIS 1: Data through", CUTOFF_DATE_MARCH, "\n")
cat(strrep("=", 60), "\n")

# Merge test data for March cutoff
subjects_sympto_visits_march <- merge_test_data(
  subjects_sympto_visits, 
  central_tests, 
  local_tests, 
  CUTOFF_DATE_MARCH
)

# Save CSV
write.csv(subjects_sympto_visits_march, OUTPUT_FILES$march_csv, row.names = FALSE)
cat(sprintf("Data saved to: %s\n", OUTPUT_FILES$march_csv))

# Perform chi-square analysis
cat("\nPerforming chi-square analysis...\n")

# Central test analysis
central_chi_march <- perform_chi_square_analysis(subjects_sympto_visits_march, "central")
cat("\nCentral Test Results:\n")
print(central_chi_march$counts)
print(central_chi_march$test)

# Local test analysis
local_chi_march <- perform_chi_square_analysis(subjects_sympto_visits_march, "local")
cat("\nLocal Test Results:\n")
print(local_chi_march$counts)
print(local_chi_march$test)

# Generate HTML report
generate_html_report(
  local_chi_march, 
  FILES$html_template, 
  OUTPUT_FILES$march_html, 
  CUTOFF_DATE_MARCH
)

# ---------------------------------------------------------------------
# 8. NOVEMBER CUTOFF ANALYSIS
# ---------------------------------------------------------------------

cat("\n", strrep("=", 60), "\n")
cat("ANALYSIS 2: Data through", CUTOFF_DATE_NOV, "\n")
cat(strrep("=", 60), "\n")

# Merge test data for November cutoff
subjects_sympto_visits_nov <- merge_test_data(
  subjects_sympto_visits, 
  central_tests, 
  local_tests, 
  CUTOFF_DATE_NOV
)

# Additional filtering for November analysis
subjects_sympto_visits_nov <- subjects_sympto_visits_nov %>%
  mutate(
    EARLIESTCENTRALDT = ifelse(ymd(EARLIESTCENTRALDT) > ymd(CUTOFF_DATE_NOV), NA, EARLIESTCENTRALDT),
    EARLIESTCENTRALPOSDT = ifelse(ymd(EARLIESTCENTRALPOSDT) > ymd(CUTOFF_DATE_NOV), NA, EARLIESTCENTRALPOSDT),
    EARLIESTLOCALDT = ifelse(ymd(EARLIESTLOCALDT) > ymd(CUTOFF_DATE_NOV), NA, EARLIESTLOCALDT),
    EARLIESTLOCALPOSDT = ifelse(ymd(EARLIESTLOCALPOSDT) > ymd(CUTOFF_DATE_NOV), NA, EARLIESTLOCALPOSDT)
  ) %>%
  mutate(
    EARLIESTCENTRALDAYSTOSYMPT = ifelse(!is.na(EARLIESTCENTRALDT), 
                                        as.numeric(as.Date(EARLIESTCENTRALDT) - as.Date(EARLIESTDT)), NA),
    EARLIESTCENTRALPOSDAYSTOSYMPT = ifelse(!is.na(EARLIESTCENTRALPOSDT), 
                                           as.numeric(as.Date(EARLIESTCENTRALPOSDT) - as.Date(EARLIESTDT)), NA),
    EARLIESTLOCALDAYSTOSYMPT = ifelse(!is.na(EARLIESTLOCALDT), 
                                      as.numeric(as.Date(EARLIESTLOCALDT) - as.Date(EARLIESTDT)), NA),
    EARLIESTLOCALPOSDAYSTOSYMPT = ifelse(!is.na(EARLIESTLOCALPOSDT), 
                                         as.numeric(as.Date(EARLIESTLOCALPOSDT) - as.Date(EARLIESTDT)), NA)
  )

# Save CSV
write.csv(subjects_sympto_visits_nov, OUTPUT_FILES$nov_csv, row.names = FALSE)
cat(sprintf("Data saved to: %s\n", OUTPUT_FILES$nov_csv))

# Perform chi-square analysis
cat("\nPerforming chi-square analysis...\n")

# Central test analysis
central_chi_nov <- perform_chi_square_analysis(subjects_sympto_visits_nov, "central")
cat("\nCentral Test Results:\n")
print(central_chi_nov$counts)
print(central_chi_nov$test)

# Local test analysis
local_chi_nov <- perform_chi_square_analysis(subjects_sympto_visits_nov, "local")
cat("\nLocal Test Results:\n")
print(local_chi_nov$counts)
print(local_chi_nov$test)

# Generate HTML report
generate_html_report(
  local_chi_nov, 
  FILES$html_template, 
  OUTPUT_FILES$nov_html, 
  CUTOFF_DATE_NOV
)

# ---------------------------------------------------------------------
# 9. SUMMARY REPORT
# ---------------------------------------------------------------------

cat("\n", strrep("=", 60), "\n")
cat("ANALYSIS COMPLETE\n")
cat(strrep("=", 60), "\n\n")

cat("Summary of Results:\n")
cat(sprintf("1. March %s Analysis:\n", CUTOFF_DATE_MARCH))
cat(sprintf("   - Total symptomatic visits: %d\n", nrow(subjects_sympto_visits_march)))
cat(sprintf("   - Local test p-value: %.4f\n", local_chi_march$test$p.value))
cat(sprintf("   - Central test p-value: %.4f\n\n", central_chi_march$test$p.value))

cat(sprintf("2. November %s Analysis:\n", CUTOFF_DATE_NOV))
cat(sprintf("   - Total symptomatic visits: %d\n", nrow(subjects_sympto_visits_nov)))
cat(sprintf("   - Local test p-value: %.4f\n", local_chi_nov$test$p.value))
cat(sprintf("   - Central test p-value: %.4f\n\n", central_chi_nov$test$p.value))

cat("Output files generated:\n")
cat(sprintf("  - %s\n", OUTPUT_FILES$march_csv))
cat(sprintf("  - %s\n", OUTPUT_FILES$nov_csv))
cat(sprintf("  - %s\n", OUTPUT_FILES$march_html))
cat(sprintf("  - %s\n", OUTPUT_FILES$nov_html))

cat("\n", strrep("=", 60), "\n")