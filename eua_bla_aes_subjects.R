# Load necessary package
library(haven)
library(dplyr)
library(tidyr)
library(pdftools)
library(rmarkdown)
library(flextable)
library(tidyverse)

# Reads the XPT file & retains data required for analysis.
adae_data <- read_xpt('xpt_data/FDA-CBER-2021-5683-0774873-0775804_125742_S1_M5_C4591001-A-D_adae.xpt')
print(adae_data)
print(colnames(adae_data))
adae_data_selected <- adae_data[c("USUBJID", "AESPID", "AESTDTC", "ARM", "AGE", "AETERM", "AESER", "SEX", "VAX101DT", "VAX102DT")]

# Extracts the subject_id (unique identifier by subject which doesn't change as the subject changes sites) from the USUBJID
adae_data_selected$SUBJID <- sub(".*C4591001 \\d{4} (\\d{8}).*", "\\1", adae_data_selected$USUBJID)
print(adae_data_selected)

subjid <- 10051081

filtered_aes <- adae_data_selected %>% 
  filter(SUBJID == subjid)
print(filtered_aes)

subjid <- 10051150

filtered_aes <- adae_data_selected %>% 
  filter(SUBJID == subjid)
print(filtered_aes)

subjid <- 10061115

filtered_aes <- adae_data_selected %>% 
  filter(SUBJID == subjid)
print(filtered_aes)

# ---- Pretty HTML tables for selected subjects --------------------------------
suppressPackageStartupMessages({
  library(flextable)
  library(htmltools)
})

out_dir <- "analysis"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
out_html <- file.path(out_dir, "adae_subject_aes.html")

# Put your target subjects here (the three you filtered above)
subjids <- c(10051081, 10051150, 10061115)

# Helper: keep/rename/order columns, light cleaning
to_date <- function(x) as.Date(substr(as.character(x), 1, 10))
prep_subject_df <- function(df) {
  df %>%
    transmute(
      SUBJID,
      AESPID,
      AE_START = to_date(AESTDTC),
      AETERM,
      SERIOUS  = as.character(AESER),
      ARM,
      AGE,
      SEX,
      VAX101DT = to_date(VAX101DT),
      VAX102DT = to_date(VAX102DT)
    ) %>%
    arrange(AE_START, AESPID)
}

# Helper: build one flextable for a subject
make_subject_ft <- function(df, sid) {
  if (nrow(df) == 0) {
    return(tags$p(class = "muted", sprintf("No AEs found for SUBJID %s.", sid)))
  }
  ft <- flextable(df)
  ft <- set_header_labels(
    ft,
    SUBJID   = "SUBJID",
    AESPID   = "AE ID",
    AE_START = "AE Start Date",
    AETERM   = "AE Term",
    SERIOUS  = "Serious?",
    ARM      = "ARM",
    AGE      = "Age",
    SEX      = "Sex",
    VAX101DT = "Dose 1 Date",
    VAX102DT = "Dose 2 Date"
  )
  # styling
  ft <- theme_vanilla(ft)
  ft <- autofit(ft)
  ft <- align(ft, j = c("AETERM"), align = "left", part = "all")
  ft <- align(ft, j = c("AE_START","VAX101DT","VAX102DT"), align = "center", part = "all")
  ft <- fontsize(ft, part = "all", size = 10)
  ft <- color(ft, i = ~ SERIOUS %in% c("Y","Yes","1","TRUE","True","T"), j = "SERIOUS", color = "#b00020")
  # Wrap long terms
  ft <- flextable::width(ft, j = "AETERM", width = 4.5)

  tagList(
    tags$h2(sprintf("SUBJID %s", sid)),
    htmltools_value(ft)
  )
}

# Build a list of sections (one per subject)
subject_sections <- lapply(subjids, function(sid) {
  df_sid <- adae_data_selected %>% filter(SUBJID == sid) %>% prep_subject_df()
  make_subject_ft(df_sid, sid)
})

# Page styles
styles <- "
  body{font:16px/1.5 system-ui,Segoe UI,Roboto,Helvetica,Arial,sans-serif;margin:36px;color:#222}
  h1{font-size:28px;margin:0 0 6px}
  h2{font-size:20px;margin:20px 0 10px}
  .muted{color:#666}
  .note{color:#555;font-size:13px;margin-top:6px}
"

# Compose and write HTML
page <- tags$html(
  tags$head(
    tags$meta(charset = "utf-8"),
    tags$title("ADAE — Subject AE Tables"),
    tags$style(HTML(styles))
  ),
  tags$body(
    tags$h1("ADAE — Subject AE Tables"),
    tags$p(class = "note", "One table per requested subject (dates shown as YYYY-MM-DD)."),
    subject_sections
  )
)

save_html(page, out_html, background = "white")
message("Wrote HTML tables to: ", out_html)
