library(readr)
library(stringr)
library(dplyr)

subjects <- list()
stats <- list()

adsl_file <- 'csv_data/FDA-CBER-2021-5683-1066333-1067534_125742_S6_M5_c4591001-A-D-adsl.csv'
data_csv <- read_csv(adsl_file)

for (i in 1:nrow(data_csv)) {
  if (data_csv$RANDFL[i] == 'Y') {
    randdt <- as.numeric(gsub("\\D", "", data_csv$RANDDT[i]))
    if (!is.na(randdt)) {
      subjects[[as.character(data_csv$SUBJID[i])]] <- list(trial_site_id = as.character(data_csv$SITEID[i]))
    }
  }
}

files <- list.files(path = "html_data", pattern = "randomization", full.names = TRUE)
files <- files[str_detect(files, "sensitive")]

for (file in files) {
  print(file)
  lines <- readLines(file)
  found <- 0
  for (line in lines) {
    line <- gsub("  ", " ", line)
    elems <- str_split(line, " ")[[1]]
    for (elem in elems) {
      if (!is.null(subjects[[elem]])) {
        if (is.null(subjects[[elem]][[file]])) {
          found <- found + 1
          subjects[[elem]][[file]] <- 1
        }
      }
    }
  }
  print(found)
}

# Initialize stats with default values
for (subject_id in sort(names(subjects))) {
  trial_site_id <- subjects[[subject_id]]$trial_site_id
  stats[[trial_site_id]] <- list(fa_random = 0, m6_random = 0, fa_investig = 0, m6_investig = 0)
}

for (subject_id in sort(names(subjects))) {
  trial_site_id <- subjects[[subject_id]]$trial_site_id
  if (!is.null(subjects[[subject_id]][['html_data/125742_S1_M5_5351_c4591001-fa-interim-randomization-sensitive.html']])) {
    if (is.null(subjects[[subject_id]][['html_data/125742_S1_M5_5351_c4591001-interim-mth6-randomization-sensitive.html']])) {
      print(paste("Not found in m6 file but in FA : [", subject_id, "]"))
    }
    stats[[trial_site_id]]$fa_random <- stats[[trial_site_id]]$fa_random + 1
  }
  if (!is.null(subjects[[subject_id]][['html_data/125742_S1_M5_5351_c4591001-interim-mth6-randomization-sensitive.html']])) {
    stats[[trial_site_id]]$m6_random <- stats[[trial_site_id]]$m6_random + 1
  }
}

invest_file <- 'investigators_screening_randomization.csv'
invest_data <- read_csv2(invest_file, col_names = c("file", "trial_site_id", "screened", "randomized"))

for (i in 1:nrow(invest_data)) {
  if (invest_data$file[i] == '125742_S1_M5_5351_c4591001-fa-interim-investigators.pdf') {
    stats[[as.character(invest_data$trial_site_id[i])]]$fa_investig <- as.numeric(invest_data$randomized[i])
  } else if (invest_data$file[i] == '125742_S1_M5_5351_c4591001-interim-mth6-investigators.pdf') {
    stats[[as.character(invest_data$trial_site_id[i])]]$m6_investig <- as.numeric(invest_data$randomized[i])
  } else if (invest_data$file[i] == 'File') {
    next
  } else {
    stop(paste(invest_data$file[i]))
  }
}
 
out <- file('F6_M6_ADSL_Randomization_subjects.csv', 'w')
writeLines("Trial Site ID;FA Random;M6 Random;FA Investig;M6 Investig", out)

for (trial_site_id in sort(names(stats))) {
  fa_random <- stats[[trial_site_id]]$fa_random
  m6_random <- stats[[trial_site_id]]$m6_random
  fa_investig <- stats[[trial_site_id]]$fa_investig
  m6_investig <- stats[[trial_site_id]]$m6_investig
  writeLines(paste(trial_site_id, fa_random, m6_random, fa_investig, m6_investig, sep = ";"), out)
}

close(out)
print(stats)
