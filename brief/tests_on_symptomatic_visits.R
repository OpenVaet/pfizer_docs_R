# =====================================================================
# C4591001 Protocol Deviations: Symptomatic Visits and Testing Rates
# =====================================================================
# This script analyzes the relationship between symptomatic visits and 
# COVID-19 testing rates in vaccine trial participants, comparing 
# treatment and placebo groups for the March 13, 2021 cutoff.
# =====================================================================
# This script requires that:
# 1 - download_full_prod.R
# https://github.com/OpenVaet/pfizer_docs_R/blob/main/download_full_prod.R
# 2 - extract_full_prod.R
# https://github.com/OpenVaet/pfizer_docs_R/blob/main/extract_full_prod.R
# ... have both been executed first
# ---------------------------------------------------------------------
# 1. SETUP AND CONFIGURATION
# ---------------------------------------------------------------------

# Load required libraries
library(haven)      # For reading XPT files
library(dplyr)      # For data manipulation
library(lubridate)  # For date handling
library(stringr)    # For string manipulation
library(ggplot2)    # For creating plots
library(base64enc)  # For embedding plots in HTML

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

# Output files
OUTPUT_FILES <- list(
  march_csv  = "phase_3_subjects_sympto_visits.csv",
  march_html = "local_test_chi_sq_march_13.html",
  march_plot = "testing_comparison_march"
)

# Helpers
safe_parse_date <- function(x) {
  # Robustly parse character/numeric/labelled to Date
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXt")) return(as.Date(x))
  if (inherits(x, "labelled")) x <- haven::as_factor(x)
  x <- as.character(x)
  if (length(x) == 0) return(as.Date(character()))
  suppressWarnings({
    dt <- parse_date_time(x, orders = c("ymd HMS", "ymd HM", "ymd H", "ymd", "Ymd HMS", "Ymd HM", "Ymd H", "Ymd"))
  })
  as.Date(dt)
}

# ---------------------------------------------------------------------
# 2. DATA LOADING AND INITIAL PROCESSING
# ---------------------------------------------------------------------

cat("Loading and processing ADSL data...\n")
adsl_data <- read_xpt(FILES$adsl)
cat(sprintf("Initial ADSL records: %d\n", nrow(adsl_data)))

randomized_pop <- adsl_data %>%
  filter(!(SUBJID %in% EXCLUDED_SUBJECTS)) %>%
  filter(PHASE != "Phase 1") %>%
  filter(as.numeric(AGETR01) >= MIN_AGE) %>%
  filter(RANDNO != "") %>%
  select(SUBJID, SITEID, COUNTRY, RFICDT, ARM, PHASE,
         AGE, AGETR01, RANDDT, RANDNO, AGEGR1,
         AGEGR2, AGEGR3, UNBLNDDT, SEX, DTHDT,
         VAX101DT, VAX102DT, VAX201DT, VAX202DT, RACE, ETHNIC)

cat(sprintf("Randomized population after filtering: %d\n", nrow(randomized_pop)))

# ---------------------------------------------------------------------
# 3. SYMPTOMS DATA PROCESSING
# ---------------------------------------------------------------------

cat("\nProcessing symptoms data...\n")
symptoms <- read_xpt(FILES$symptoms)
cat(sprintf("Initial symptoms records: %d\n", nrow(symptoms)))

symptoms_filtered <- symptoms %>%
  filter(SUBJID %in% randomized_pop$SUBJID) %>%
  filter(PARCAT1 == "SIGNS AND SYMPTOMS OF DISEASE") %>%
  filter(AVALC == "Y") %>%
  mutate(ADT = safe_parse_date(ADT),
         ASTDT = safe_parse_date(ASTDT)) %>%
  select(SUBJID, SITEID, ARM, AVISIT, PARAM, VISITNUM, VISIT, ADT, ASTDT)

cat(sprintf("Filtered symptoms records: %d\n", nrow(symptoms_filtered)))

symptoms_sup <- read_xpt(FILES$symptoms_sup) %>%
  filter(FATEST == "First Symptom Date") %>%
  mutate(SUBJID = str_extract(USUBJID, "\\d+$"),
         FAORRES = safe_parse_date(FAORRES)) %>%
  filter(SUBJID %in% randomized_pop$SUBJID) %>%
  select(SUBJID, FAORRES, VISIT)

cat(sprintf("Supplementary symptoms records: %d\n", nrow(symptoms_sup)))

# ---------------------------------------------------------------------
# 4. CREATE SYMPTOMATIC VISITS DATASET
# ---------------------------------------------------------------------

cat("\nCreating symptomatic visits dataset...\n")
subjects_sympto_visits <- symptoms_filtered %>%
  group_by(SUBJID, VISIT, ARM, SITEID) %>%
  summarise(EARLIESTSYMPTDT = min(ADT, na.rm = TRUE), .groups = "drop") %>%
  left_join(symptoms_sup, by = "SUBJID") %>%
  mutate(
    EARLIESTDT = coalesce(FAORRES, EARLIESTSYMPTDT),
    VISIT = VISIT.x
  ) %>%
  select(-VISIT.x, -VISIT.y)

exceptions <- subjects_sympto_visits %>%
  filter(!is.na(EARLIESTSYMPTDT), !is.na(EARLIESTDT), EARLIESTSYMPTDT < EARLIESTDT)

if (nrow(exceptions) > 0) {
  cat(sprintf("WARNING: %d rows where EARLIESTSYMPTDT < EARLIESTDT; taking mins\n", nrow(exceptions)))
}

subjects_sympto_visits <- subjects_sympto_visits %>%
  group_by(SUBJID, ARM, SITEID, VISIT) %>%
  summarise(
    EARLIESTSYMPTDT = suppressWarnings(min(EARLIESTSYMPTDT, na.rm = TRUE)),
    FAORRES = {
      tmp <- suppressWarnings(as.Date(FAORRES))
      if (all(is.na(tmp))) as.Date(NA) else min(tmp, na.rm = TRUE)
    },
    EARLIESTDT = suppressWarnings(min(EARLIESTDT, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  select(-EARLIESTSYMPTDT)

# ---------------------------------------------------------------------
# 5. COVID-19 TEST DATA PROCESSING
# ---------------------------------------------------------------------

cat("\nProcessing COVID-19 test data...\n")
tests <- read_xpt(FILES$tests) %>%
  filter(MBTEST %in% c("Cepheid RT-PCR assay for SARS-CoV-2",
                       "SEVERE ACUTE RESP SYNDROME CORONAVIRUS 2")) %>%
  mutate(SUBJID = str_extract(USUBJID, "\\d+$"),
         MBDTC = safe_parse_date(MBDTC),
         MBORRES = case_when(
           MBORRES == "NEGATIVE" ~ "NEG",
           MBORRES == "POSITIVE" ~ "POS",
           MBORRES == "INDETERMINATE" ~ "IND",
           TRUE ~ MBORRES
         )) %>%
  select(SUBJID, VISIT, MBTEST, MBORRES, MBDTC)

tests_filtered <- tests %>% filter(grepl("COVID_", VISIT))

cat(sprintf("COVID test records: %d\n", nrow(tests_filtered)))

central_tests <- tests_filtered %>%
  filter(MBTEST == "Cepheid RT-PCR assay for SARS-CoV-2") %>%
  select(SUBJID, VISIT, MBORRES, MBDTC)

local_tests <- tests_filtered %>%
  filter(MBTEST == "SEVERE ACUTE RESP SYNDROME CORONAVIRUS 2") %>%
  select(SUBJID, VISIT, MBORRES, MBDTC)

cat(sprintf("Central tests: %d, Local tests: %d\n", nrow(central_tests), nrow(local_tests)))

# ---------------------------------------------------------------------
# 6. ANALYSIS FUNCTIONS
# ---------------------------------------------------------------------

merge_test_data <- function(sympto_visits, central_tests, local_tests, cutoff_date) {
  cutoff <- as.Date(cutoff_date)
  sympto_visits_filtered <- sympto_visits %>% filter(EARLIESTDT <= cutoff)
  central_tests_filtered <- central_tests %>% filter(MBDTC <= cutoff)
  local_tests_filtered   <- local_tests   %>% filter(MBDTC <= cutoff)

  result <- sympto_visits_filtered %>%
    left_join(
      central_tests_filtered %>%
        group_by(SUBJID, VISIT) %>%
        summarise(
          EARLIESTCENTRALDT = suppressWarnings(min(MBDTC, na.rm = TRUE)),
          EARLIESTCENTRALPOSDT = {
            pos <- MBDTC[MBORRES == "POS"]
            if (length(pos) == 0) as.Date(NA) else suppressWarnings(min(pos, na.rm = TRUE))
          },
          .groups = "drop"
        ),
      by = c("SUBJID", "VISIT")
    ) %>%
    left_join(
      local_tests_filtered %>%
        group_by(SUBJID, VISIT) %>%
        summarise(
          EARLIESTLOCALDT = suppressWarnings(min(MBDTC, na.rm = TRUE)),
          EARLIESTLOCALPOSDT = {
            pos <- MBDTC[MBORRES == "POS"]
            if (length(pos) == 0) as.Date(NA) else suppressWarnings(min(pos, na.rm = TRUE))
          },
          .groups = "drop"
        ),
      by = c("SUBJID", "VISIT")
    ) %>%
    mutate(
      EARLIESTCENTRALDAYSTOSYMPT = ifelse(!is.na(EARLIESTCENTRALDT),
                                          as.numeric(EARLIESTCENTRALDT - EARLIESTDT), NA_real_),
      EARLIESTCENTRALPOSDAYSTOSYMPT = ifelse(!is.na(EARLIESTCENTRALPOSDT),
                                             as.numeric(EARLIESTCENTRALPOSDT - EARLIESTDT), NA_real_),
      EARLIESTLOCALDAYSTOSYMPT = ifelse(!is.na(EARLIESTLOCALDT),
                                        as.numeric(EARLIESTLOCALDT - EARLIESTDT), NA_real_),
      EARLIESTLOCALPOSDAYSTOSYMPT = ifelse(!is.na(EARLIESTLOCALPOSDT),
                                           as.numeric(EARLIESTLOCALPOSDT - EARLIESTDT), NA_real_)
    )
  result
}

perform_chi_square_analysis <- function(data, test_type = "local") {
  test_col <- if (test_type == "central") "EARLIESTCENTRALDT" else "EARLIESTLOCALDT"

  arm_test_counts <- data %>%
    mutate(has_test = !is.na(.data[[test_col]])) %>%
    group_by(ARM) %>%
    summarise(
      total_visits = n(),
      test_visits = sum(has_test),
      test_percentage = (sum(has_test) / n()) * 100,
      .groups = "drop"
    )

  # Guard: need exactly 2 arms
  if (nrow(arm_test_counts) != 2) {
    stop(sprintf("Expected exactly 2 treatment arms; found %d.", nrow(arm_test_counts)))
  }

  contingency_table <- matrix(
    c(arm_test_counts$test_visits[1],
      arm_test_counts$total_visits[1] - arm_test_counts$test_visits[1],
      arm_test_counts$test_visits[2],
      arm_test_counts$total_visits[2] - arm_test_counts$test_visits[2]),
    nrow = 2, byrow = TRUE
  )
  rownames(contingency_table) <- arm_test_counts$ARM
  colnames(contingency_table) <- c(
    paste(stringr::str_to_title(test_type), "Test"),
    paste("No", stringr::str_to_title(test_type), "Test")
  )

  chi_sq_result <- suppressWarnings(chisq.test(contingency_table))

  list(
    counts = arm_test_counts,
    table = contingency_table,
    test = chi_sq_result
  )
}

create_testing_comparison_plot <- function(central_results, local_results, cutoff_date, text_scale = 1.6) {
  # Build plotting frame
  testing_data <- data.frame(
    Test_Type  = rep(c("Central Test", "Local Test"), each = 2),
    Group      = rep(central_results$counts$ARM, 2),
    Percentage = c(central_results$counts$test_percentage,
                   local_results$counts$test_percentage),
    Count      = c(central_results$counts$test_visits,
                   local_results$counts$test_visits),
    Total      = c(central_results$counts$total_visits,
                   local_results$counts$total_visits)
  )

  # Force consistent group labels & order (align legend/colors deterministically)
  testing_data$Group <- factor(testing_data$Group, levels = c("Placebo", "BNT162b2 Phase 2/3 (30 mcg)"))

  # Sizes
  s_base       <- 12 * text_scale
  s_title      <- 16 * text_scale
  s_subtitle   <- 12 * text_scale
  s_axis       <- 12 * text_scale
  s_axis_title <- 12 * text_scale
  s_legend     <- 12 * text_scale
  s_bar_label  <- 4  * text_scale
  s_annot      <- 3.8 * text_scale

  # Diffs & p-values
  central_diff <- abs(diff(central_results$counts$test_percentage))
  local_diff   <- abs(diff(local_results$counts$test_percentage))
  central_p    <- ifelse(central_results$test$p.value < 0.001, "p < 0.001",
                         paste0("p = ", format(central_results$test$p.value, digits = 3)))
  local_p      <- ifelse(local_results$test$p.value   < 0.001, "p < 0.001",
                         paste0("p = ", format(local_results$test$p.value,   digits = 3)))

  # Bar tops for annotation headroom
  y1 <- max(testing_data$Percentage[1:2])
  y2 <- max(testing_data$Percentage[3:4])
  y_top <- max(y1, y2) + 14 * text_scale

  p <- ggplot(testing_data, aes(x = Test_Type, y = Percentage, fill = Group)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.7) +
    geom_text(aes(label = sprintf("%.1f%%", Percentage)),
              position = position_dodge(0.7),
              vjust = -0.5, size = s_bar_label, fontface = "bold") +

    scale_fill_manual(values = c(
      "Placebo" = "#0d132d",
      "BNT162b2 Phase 2/3 (30 mcg)" = "#a1082c"
    )) +
    labs(
      title    = "PCR Testing Rates by Treatment Arms - Central & Local Tests",
      subtitle = paste0("Data through ", cutoff_date),
      x = "Test Type", y = "Percentage of Symptomatic Visits Tested (%)",
      fill = "Treatment Group"
    ) +

    # Titles (keep your top subtitle) + bottom-right caption "Figure 1"
    labs(
      title    = "PCR Testing Rates by Treatment Arms - Central & Local Tests",
      subtitle = paste0("Data through ", cutoff_date),
      caption  = "Figure 1",
      x = "Test Type", y = "Percentage of Symptomatic Visits Tested (%)"
    ) +

    # Clean y scale
    scale_y_continuous(limits = c(0, y_top), breaks = seq(0, 100, 10),
                       expand = expansion(mult = c(0, 0.05))) +

    # Theme aligned with the other script; force all-white backgrounds for print
    theme_minimal(base_size = s_base) +
    theme(
      plot.title       = element_text(size = s_title, face = "bold", hjust = 0.5),
      plot.subtitle    = element_text(size = s_subtitle, hjust = 0.5, color = "gray35",
                                      margin = margin(b = 6 * text_scale)),
      plot.caption     = element_text(size = s_subtitle, hjust = 1, vjust = 1,
                                      margin = margin(t = 6 * text_scale)),
      plot.caption.position = "plot",

      axis.text        = element_text(size = s_axis),
      axis.title       = element_text(size = s_axis_title, face = "bold"),

      legend.position  = "bottom",
      legend.title     = element_text(size = s_legend, face = "bold"),
      legend.text      = element_text(size = s_legend),
      legend.key.size  = grid::unit(12 * text_scale, "pt"),
      legend.background    = element_rect(fill = "white", color = NA),
      legend.box.background = element_rect(fill = "white", color = NA),

      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "white", color = NA),
      plot.background  = element_rect(fill = "white", color = NA),

      # Match margins to your other plot (note larger bottom margin)
      plot.margin      = margin(t = 12 * text_scale, r = 10 * text_scale,
                                b = 14 * text_scale, l = 10 * text_scale)
    ) +
    coord_cartesian(clip = "off")

  # Central labels
  p <- p +
    annotate("text", x = 1, y = y1 + 6 * text_scale, vjust = 0, lineheight = 0.95,
             label = sprintf("%.1f%% difference", central_diff),
             size = s_annot, fontface = "bold") +
    annotate("text", x = 1, y = y1 + 9.8 * text_scale, vjust = 0, lineheight = 0.95,
             label = central_p, size = s_annot, fontface = "bold",
             color = ifelse(central_results$test$p.value < 0.05, "#a1082c", "black"))

  # Local labels
  p <- p +
    annotate("text", x = 2, y = y2 + 6 * text_scale, vjust = 0, lineheight = 0.95,
             label = sprintf("%.1f%% difference", local_diff),
             size = s_annot, fontface = "bold") +
    annotate("text", x = 2, y = y2 + 9.8 * text_scale, vjust = 0, lineheight = 0.95,
             label = local_p, size = s_annot, fontface = "bold",
             color = ifelse(local_results$test$p.value < 0.05, "#a1082c", "black"))

  # Optional highlight (kept; remove if you prefer 100% monochrome)
  if (local_results$test$p.value < 0.05) {
    p <- p + annotate("rect", xmin = 1.5, xmax = 2.5,
                      ymin = -2, ymax = max(testing_data$Percentage) * 1.25,
                      alpha = 0.08, fill = "#a1082c") +
      annotate("text", x = 2, y = -5, label = "⚠ Significant Disparity",
               size = 3 * text_scale, fontface = "italic", color = "#a1082c")
  }

  p
}

save_plot <- function(plot, base_filename, width = 10, height = 6) {
  ggsave(paste0(base_filename, ".png"), plot = plot, width = width, height = height, dpi = 300, bg = "white")
  ggsave(paste0(base_filename, ".pdf"), plot = plot, width = width, height = height, bg = "white")
  cat(sprintf("Plots saved: %s.png and %s.pdf\n", base_filename, base_filename))
}

generate_html_report <- function(chi_sq_results, template_file, output_file, cutoff_date, plot_filename = NULL) {
  cont_table <- chi_sq_results$table
  chi_test <- chi_sq_results$test

  row1_total <- sum(cont_table[1,])
  row2_total <- sum(cont_table[2,])
  row1_pct1 <- round(100 * cont_table[1,1] / row1_total, 2)
  row1_pct2 <- round(100 * cont_table[1,2] / row1_total, 2)
  row2_pct1 <- round(100 * cont_table[2,1] / row2_total, 2)
  row2_pct2 <- round(100 * cont_table[2,2] / row2_total, 2)

  plot_section <- ""
  if (!is.null(plot_filename) && file.exists(paste0(plot_filename, ".png"))) {
    uri <- base64enc::dataURI(file = paste0(plot_filename, ".png"), mime = "image/png")
    plot_section <- paste0('
      <div class="plot-section">
        <h2 style="text-align: center; color: #2c3e50; margin: 40px 0 20px 0;">Testing Rate Comparison</h2>
        <img src="', uri, '" style="width: 100%; max-width: 900px; display: block; margin: 0 auto; border-radius: 8px; box-shadow: 0 5px 15px rgba(0,0,0,0.1);">
      </div>
    ')
  }

  enhanced_html <- paste0('
<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="UTF-8">
<meta name="viewport" content="width=device-width, initial-scale=1.0">
<title>Chi-Square Test Results - ', cutoff_date, '</title>
<style>
  body{font-family:-apple-system,BlinkMacSystemFont,"Segoe UI",Roboto,"Helvetica Neue",Arial,sans-serif;line-height:1.6;color:#333;max-width:1100px;margin:0 auto;padding:20px;background:linear-gradient(135deg,#667eea 0%,#764ba2 100%);min-height:100vh;}
  .container{background:white;border-radius:12px;padding:40px;box-shadow:0 20px 60px rgba(0,0,0,0.3);}
  h1{color:#2c3e50;text-align:center;margin-bottom:10px;font-size:28px;}
  .subtitle{text-align:center;color:#7f8c8d;margin-bottom:30px;font-size:16px;}
  .stats-grid{display:grid;grid-template-columns:repeat(auto-fit,minmax(200px,1fr));gap:20px;margin-bottom:30px;}
  .stat-card{background:linear-gradient(135deg,#667eea 0%,#764ba2 100%);color:white;padding:20px;border-radius:8px;text-align:center;}
  .stat-value{font-size:32px;font-weight:bold;margin-bottom:5px;}
  .stat-label{font-size:14px;opacity:0.9;}
  table{width:100%;border-collapse:collapse;margin:30px 0;background:white;box-shadow:0 5px 15px rgba(0,0,0,0.08);border-radius:8px;overflow:hidden;}
  th{background:linear-gradient(135deg,#667eea 0%,#764ba2 100%);color:white;padding:15px;text-align:left;font-weight:600;text-transform:uppercase;font-size:12px;letter-spacing:1px;}
  td{padding:15px;border-bottom:1px solid #ecf0f1;}
  tr:last-child td{border-bottom:none;}
  tr:hover{background-color:#f8f9fa;}
  .cell-value{font-size:18px;font-weight:600;color:#2c3e50;}
  .cell-percentage{font-size:12px;color:#7f8c8d;margin-top:4px;}
  .result-section{background:#f8f9fa;border-left:4px solid #667eea;padding:20px;margin:30px 0;border-radius:4px;}
  .result-title{font-size:18px;font-weight:600;color:#2c3e50;margin-bottom:15px;}
  .p-value{font-size:24px;font-weight:bold;color:', ifelse(chi_test$p.value < 0.05, '#e74c3c', '#27ae60'), ';}
  .significance{margin-top:10px;padding:10px;background:', ifelse(chi_test$p.value < 0.05, '#ffe5e5', '#e5ffe5'), ';border-radius:4px;color:', ifelse(chi_test$p.value < 0.05, '#c0392b', '#229954'), ';}
  .plot-section{margin:40px 0;padding:20px;background:#f8f9fa;border-radius:8px;}
  .footer{text-align:center;margin-top:40px;color:#7f8c8d;font-size:14px;}
</style>
</head>
<body>
  <div class="container">
    <h1>Chi-Square Test of Independence</h1>
    <div class="subtitle">Testing Rate Analysis - Data through ', cutoff_date, '</div>

    ', plot_section, '

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
          <td><div class="cell-value">', cont_table[1,1], '</div><div class="cell-percentage">(', row1_pct1, '%)</div></td>
          <td><div class="cell-value">', cont_table[1,2], '</div><div class="cell-percentage">(', row1_pct2, '%)</div></td>
          <td><strong>', row1_total, '</strong></td>
        </tr>
        <tr>
          <td><strong>', rownames(cont_table)[2], '</strong></td>
          <td><div class="cell-value">', cont_table[2,1], '</div><div class="cell-percentage">(', row2_pct1, '%)</div></td>
          <td><div class="cell-value">', cont_table[2,2], '</div><div class="cell-percentage">(', row2_pct2, '%)</div></td>
          <td><strong>', row2_total, '</strong></td>
        </tr>
        <tr style="background-color:#f8f9fa;">
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
      <div class="significance"><strong>Interpretation:</strong> ',
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
</html>')

  writeLines(enhanced_html, output_file)
  cat(sprintf("HTML report saved to: %s\n", output_file))
}

# ---------------------------------------------------------------------
# 7. RUN: MARCH 13 PIPELINE
# ---------------------------------------------------------------------

cat("\n=== MARCH 13 PIPELINE ===\n")
merged_march <- merge_test_data(subjects_sympto_visits, central_tests, local_tests, CUTOFF_DATE_MARCH)

# Write CSV of symptomatic visits used up to MARCH 13
write.csv(merged_march %>% arrange(SUBJID, VISIT), OUTPUT_FILES$march_csv, row.names = FALSE)
cat(sprintf("CSV saved: %s\n", OUTPUT_FILES$march_csv))

central_results_march <- perform_chi_square_analysis(merged_march, "central")
local_results_march   <- perform_chi_square_analysis(merged_march, "local")

p_march <- create_testing_comparison_plot(central_results_march, local_results_march,
                                          CUTOFF_DATE_MARCH, text_scale = 1.6)
save_plot(p_march, OUTPUT_FILES$march_plot, width = 12, height = 7)

generate_html_report(
  chi_sq_results = local_results_march,
  template_file  = FILES$html_template,
  output_file    = OUTPUT_FILES$march_html,
  cutoff_date    = CUTOFF_DATE_MARCH,
  plot_filename  = OUTPUT_FILES$march_plot  # base name only, no extension
)

cat("\nAll done.\n")
