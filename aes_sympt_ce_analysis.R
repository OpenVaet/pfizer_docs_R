# Extracts ZIP to XPT.
library(tools)
library(haven)
library(dplyr)
library(pdftools)
library(rmarkdown)
library(flextable)
library(tidyverse)

# Verifies if the requireed files have been properly retrieved.
# ADAE
adae_path <- 'xpt_data/FDA-CBER-2021-5683-0774873-0775804_125742_S1_M5_C4591001-A-D_adae.xpt'
if (!file.exists(adae_path)) {
  stop("ADAE file not found", call. = FALSE)
}
# ADSYMPT
adsympt_path <- 'xpt_data/FDA-CBER-2021-5683-0663135-0671344-125742_S1_M5_c4591001-A-D-adsympt.xpt'
if (!file.exists(adsympt_path)) {
  stop("ADSYMPT file not found", call. = FALSE)
}
# CE
ce_path <- 'xpt_data/FDA-CBER-2021-5683-0149082 to -0158559_125742_S1_M5_c4591001-S-D-ce.xpt'
if (!file.exists(ce_path)) {
  stop("CE file not found", call. = FALSE)
}

# Reads & filters the ADAE file.
adae_data <- read_xpt(adae_path)
adae_selected_data <- adae_data[c("SUBJID", "AESPID", "VPHASE", "AREL", "AESER", "AESTDTC", "AEENDTC", "AEDECOD")]

# Reads & filters the ADSYMPT file.
adsympt_data <- read_xpt(adsympt_path)
adsympt_selected_data <- adsympt_data[c("USUBJID", "PARCAT1", "PARAM", "AVISIT", "AVALC", "ADT", "AENDT")]
adsympt_selected_data <- adsympt_selected_data[adsympt_selected_data$PARCAT1 == 'SIGNS AND SYMPTOMS OF DISEASE', ]
adsympt_selected_data$SUBJID <- sub(".*C4591001 \\d{4} (\\d{8}).*", "\\1", adsympt_selected_data$USUBJID)
adsympt_selected_data <- adsympt_data[c("SUBJID", "PARCAT1", "PARAM", "AVISIT", "AVALC", "ADT", "AENDT")]

# Reads & filters the CE file.
ce_data <- read_xpt(ce_path)
ce_selected_data <- ce_data[c("USUBJID", "CEGRPID", "CETERM", "CECAT", "CEOCCUR", "CESEV", "CESTDTC", "CERFTDTC", "CEENDTC", "CEEVINTX")]
ce_selected_data$SUBJID <- sub(".*C4591001 \\d{4} (\\d{8}).*", "\\1", ce_selected_data$USUBJID)
ce_selected_data <- ce_selected_data[c("SUBJID", "CEGRPID", "CETERM", "CECAT", "CEOCCUR", "CESEV", "CESTDTC", "CERFTDTC", "CEENDTC", "CEEVINTX")]
print(ce_selected_data)

# 10811026
# ADAE
adae_10811026_data <- adae_selected_data %>% 
  filter(SUBJID == 10811026)
print(adae_10811026_data)
adae_html_table_10811026 <- flextable(adae_10811026_data) %>%
  set_header_labels(
    "SUBJID" = "SUBJID",
    "AESPID" = "AESPID",
    "VPHASE" = "VPHASE",
    "AREL" = "AREL",
    "AESER" = "AESER",
    "AESTDTC" = "AESTDTC",
    "AEENDTC" = "AEENDTC",
    "AEDECOD" = "AEDECOD"
  ) %>%
  theme_zebra() %>%  # or another theme with less prominent borders
  align(align = "center", part = "all") %>%
  fontsize(size = 14, part = "all") %>%
  padding(padding = 2) %>%
  autofit() %>%
  set_caption("Table 1: ADAE Reported by Subject 10811026")
save_as_html(adae_html_table_10811026, path = "10811026_adae.html")
# ADSYMPT
adsympt_10811026_data <- adsympt_selected_data %>% 
  filter(SUBJID == 10811026)
print(adsympt_10811026_data)
adsympt_html_table_10811026 <- flextable(adsympt_10811026_data) %>%
  set_header_labels(
    "SUBJID" = "SUBJID",
    "PARCAT1" = "PARCAT1",
    "PARAM" = "PARAM",
    "AVISIT" = "AVISIT",
    "AVALC" = "AVALC",
    "ADT" = "ADT",
    "AENDT" = "AENDT"
  ) %>%
  theme_zebra() %>%  # or another theme with less prominent borders
  align(align = "center", part = "all") %>%
  fontsize(size = 14, part = "all") %>%
  padding(padding = 2) %>%
  autofit() %>%
  set_caption("Table 2: ADSYMPT Reported by Subject 10811026")
save_as_html(adsympt_html_table_10811026, path = "10811026_adsympt.html")
# CE
ce_10811026_data <- ce_selected_data %>% 
  filter(SUBJID == 10811026)
print(ce_10811026_data)
ce_html_table_10811026 <- flextable(ce_10811026_data) %>%
  set_header_labels(
    "SUBJID" = "SUBJID",
    "CEGRPID" = "CEGRPID",
    "CETERM" = "CETERM",
    "CECAT" = "CECAT",
    "CEOCCUR" = "CEOCCUR",
    "CESEV" = "CESEV",
    "CESTDTC" = "CESTDTC",
    "CERFTDTC" = "CERFTDTC",
    "CEENDTC" = "CEENDTC",
    "CEEVINTX" = "CEEVINTX"
  ) %>%
  theme_zebra() %>%  # or another theme with less prominent borders
  align(align = "center", part = "all") %>%
  fontsize(size = 14, part = "all") %>%
  padding(padding = 2) %>%
  autofit() %>%
  set_caption("Table 3: CE Reported by Subject 10811026")
save_as_html(ce_html_table_10811026, path = "10811026_ce.html")
