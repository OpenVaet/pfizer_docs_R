# =====================================================================
# C4591001 Protocol Deviations: Imbalance Screening & Site-Level Tests
# =====================================================================
# This script screens protocol deviations for imbalances between
# treatment arms and performs site-level Fisher exact tests on
# significantly imbalanced deviations. It also produces weekly
# time-series plots for selected deviations.
# =====================================================================
# This script requires that:
# 1 - download_full_prod.R
# https://github.com/OpenVaet/pfizer_docs_R/blob/main/download_full_prod.R
# 2 - extract_full_prod.R
# https://github.com/OpenVaet/pfizer_docs_R/blob/main/extract_full_prod.R
# ... have both been executed first
# ---------------------------------------------------------------------
# OUTPUTS (created in the working directory):
#   - deviations.csv                                   (flattened deviations with sources)
#   - deviations_statistics.csv                         (arm-level chi-square per deviation)
#   - imbalanced_deviations.html                        (arm-level significant table)
#   - imbalanced_deviations_by_sites.html               (site-level significant table)
#   - deviations_significant_results_by_sites.csv
#   - staff_related_imbalanced_weekly.png
#   - other_vaccine_weekly.png
# =====================================================================

# ---------------------------------------------------------------------
# 1) SETUP
# ---------------------------------------------------------------------

suppressPackageStartupMessages({
  library(haven)      # read_xpt
  library(dplyr)      # data wrangling
  library(lubridate)  # dates
  library(stringr)    # strings
  library(ggplot2)    # plots
  library(flextable)  # HTML tables
})

options(stringsAsFactors = FALSE)

# ------------------ Configuration ------------------

DATA_PATH <- "xpt_data/"
CUTOFF_DATE <- as.Date("2021-03-13")

FILES <- list(
  dv = file.path(DATA_PATH, "FDA-CBER-2021-5683-0065774 to -0066700_125742_S1_M5_c4591001-A-D-addv.xpt"),
  suppdv = file.path(DATA_PATH, "FDA-CBER-2021-5683-0174607 to -0178318_125742_S1_M5_c4591001-S-D-suppdv.xpt"),
  adsl = paste0(DATA_PATH, "FDA-CBER-2021-5683-1066333-1067534_125742_S6_M5_c4591001-A-D-adsl.xpt")
)

OUTPUTS <- list(
  flattened_csv = "deviations.csv",
  arm_stats_csv = "deviations_statistics.csv",
  arm_stats_html = "imbalanced_deviations.html",
  site_stats_csv = "deviations_significant_results_by_sites.csv",
  site_stats_html = "imbalanced_deviations_by_sites.html",
  staff_weekly_png = "staff_related_imbalanced_weekly.png",
  other_vax_weekly_png = "other_vaccine_weekly.png",
  dev_pair_counts_plot = "deviation_pair_counts",
  dev_pair_counts_html = "deviation_pair_counts.html"
)


# Analysis thresholds
MIN_TOTAL_SUBJECTS_PER_DEV <- 100  # only test deviations seen in >=100 subjects
MIN_SITE_DEV_EVENTS <- 20          # only test sites with >=20 events of a given deviation
ALPHA <- 0.05

EXCLUDED_SUBJECTS <- c(10561101, 11331382, 11101123, 11331405, 11491117,
                       12691090, 12691070, 11351357, 11341006, 10891112,
                       11231105, 10711213)
MIN_AGE <- 16

# Staff-related deviations to visualize weekly (keep/edit as needed)
STAFF_RELATED_DEVS <- c(
  "Assessment of acute reaction for protocol specified timeframe after study intervention administration not performed at the vaccination visits.",
  "Nasal swab not collected by site staff prior to vaccination.",
  "Nasal swab not collected for the visit where it is required",
  "Procedure/Test not performed per protocol",
  "Urine pregnancy test not performed."
)

# ---------------------------------------------------------------------
# 2) HELPERS
# ---------------------------------------------------------------------

safe_parse_date <- function(x) {
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXt")) return(as.Date(x))
  if (inherits(x, "labelled")) x <- haven::as_factor(x)
  x <- as.character(x)
  suppressWarnings(as.Date(parse_date_time(x, orders = c("Ymd HMS","Ymd HM","Ymd H","Ymd","ymd HMS","ymd HM","ymd H","ymd"))))
}

format_p_bin <- function(p) {
  if (is.na(p)) return(NA_character_)
  if (p < 1e-6)  "<0.000001"
  else if (p < 1e-5) "<0.00001"
  else if (p < 1e-4) "<0.0001"
  else if (p < 1e-3) "<0.001"
  else if (p < 1e-2) "<0.01"
  else              "<0.1"
}

# clean trailing spaces / en-dashes
normalize_term <- function(x) {
  x <- iconv(x, from = "UTF-8", to = "ASCII", sub = "")
  x <- gsub("–", "-", x)
  str_trim(x)
}

# ---- Minimal HTML wrapper to embed a PNG as base64 ----
generate_plot_html <- function(title, subtitle, plot_base, outfile) {
  if (!file.exists(paste0(plot_base, ".png"))) {
    stop("PNG not found: ", paste0(plot_base, ".png"))
  }
  uri <- base64enc::dataURI(file = paste0(plot_base, ".png"), mime = "image/png")
  html <- sprintf('
<!DOCTYPE html><html lang="en"><head>
<meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1">
<title>%s</title>
<style>
body{font-family:system-ui,Segoe UI,Roboto,Helvetica,Arial,sans-serif;background:#fafafa;color:#222;margin:0}
.container{max-width:1100px;margin:0 auto;padding:24px}
h1{font-size:28px;margin:0 0 4px}
.subtitle{color:#666;margin:0 0 20px}
img{width:100%%;max-width:1000px;border-radius:8px;box-shadow:0 8px 30px rgba(0,0,0,.12)}
</style>
</head><body>
  <div class="container">
    <h1>%s</h1>
    <div class="subtitle">%s</div>
    <img alt="Deviation pair comparison (counts)" src="%s">
  </div>
</body></html>', title, title, subtitle, uri)
  writeLines(html, outfile)
  cat("HTML saved:", outfile, "\n")
}

# ---- Deviation-pair arm comparison ----
create_deviation_pair_plot_counts <- function(filtered_data, arm_counts, target_terms,
                                              bnt_label   = "BNT162b2 Phase 2/3 (30 mcg)",
                                              plc_label   = "Placebo",
                                              text_scale  = 1.6,
                                              wrap_width  = 30,
                                              pad_top_mult = 0.12,   # fraction of max count added above annotations
                                              subtitle_gap = 8,      # gap (in text_scale units) under subtitle
                                              fig_caption = NULL      # e.g., "Figure 2" (bottom-right); NULL to omit
) {
  terms_norm <- normalize_term(target_terms)

  df <- filtered_data %>%
    filter(CONCATTERM %in% terms_norm) %>%
    distinct(SUBJID, ARM, CONCATTERM) %>%
    group_by(CONCATTERM, ARM) %>%
    summarise(n_subjects = n(), .groups = "drop")

  bnt_total <- arm_counts$total_subjects[arm_counts$ARM == bnt_label]
  plc_total <- arm_counts$total_subjects[arm_counts$ARM == plc_label]

  build_row <- function(term) {
    bnt_n <- df$n_subjects[df$CONCATTERM == term & df$ARM == bnt_label]
    plc_n <- df$n_subjects[df$CONCATTERM == term & df$ARM == plc_label]
    bnt_n <- ifelse(length(bnt_n) == 0, 0L, as.integer(bnt_n))
    plc_n <- ifelse(length(plc_n) == 0, 0L, as.integer(plc_n))
    data.frame(
      Deviation = term,
      Group     = c(bnt_label, plc_label),
      Count     = c(bnt_n, plc_n),
      Total     = c(bnt_total, plc_total),
      stringsAsFactors = FALSE
    )
  }
  plot_df <- do.call(rbind, lapply(terms_norm, build_row))
  plot_df$Deviation <- factor(plot_df$Deviation, levels = terms_norm)

  # Per-term annotation (Δ & p), with dynamic headroom above bars
  annot <- lapply(terms_norm, function(term) {
    sub   <- subset(plot_df, Deviation == term)
    bnt_n <- sub$Count[sub$Group == bnt_label]; bnt_t <- sub$Total[sub$Group == bnt_label]
    plc_n <- sub$Count[sub$Group == plc_label]; plc_t <- sub$Total[sub$Group == plc_label]

    m <- matrix(c(bnt_n, bnt_t - bnt_n, plc_n, plc_t - plc_n), nrow = 2, byrow = TRUE)
    p <- suppressWarnings(chisq.test(m)$p.value)

    ymax    <- max(sub$Count, na.rm = TRUE)
    offset  <- max(6 * text_scale, 0.06 * ymax)
    y_label <- ymax + offset

    delta   <- abs(diff(sub$Count))
    p_label <- ifelse(p < 0.001, "p < 0.001", paste0("p = ", format(p, digits = 3)))
    label   <- sprintf("\u0394 = %s\n%s", format(delta, big.mark=","), p_label)
    col     <- ifelse(p < 0.05, "#a1082c", "black")  # align sig color

    data.frame(Deviation = term, y = y_label, label = label, col = col, ymax = ymax)
  })
  annot <- do.call(rbind, annot)

  # --- Sizing (matches your other plot) ---
  s_base       <- 12 * text_scale
  s_title      <- 16 * text_scale
  s_subtitle   <- 12 * text_scale
  s_axis       <- 12 * text_scale
  s_axis_title <- 12 * text_scale
  s_legend     <- 12 * text_scale
  s_bar_label  <- 4  * text_scale
  s_annot      <- 3.8 * text_scale

  # Legend stability & color mapping
  plot_df$Group <- factor(plot_df$Group, levels = c(plc_label, bnt_label))
  fill_vals <- c(
    plc_label = "#0d132d",          # Placebo
    bnt_label = "#a1082c"           # BNT162b2
  )

  # Extra headroom above annotations so they never collide with subtitle
  max_count <- max(plot_df$Count, na.rm = TRUE)
  pad_top   <- max(8 * text_scale, pad_top_mult * max_count)
  y_top     <- max(annot$y) + pad_top

  ggplot(plot_df, aes(x = Deviation, y = Count, fill = Group)) +
    geom_bar(stat = "identity",
             position = position_dodge2(width = 0.72, preserve = "single"),
             width = 0.68) +
    geom_text(aes(label = format(Count, big.mark = ",")),
              position = position_dodge2(width = 0.72, preserve = "single"),
              vjust = -0.5, size = s_bar_label, fontface = "bold",
              check_overlap = TRUE) +
    scale_y_continuous(limits = c(0, y_top),
                       expand = expansion(mult = c(0, 0.02))) +
    scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = wrap_width)) +

    # >>> Requested colors + robust legend (no dropping)
    scale_fill_manual(
      values = c(setNames(fill_vals, c(plc_label, bnt_label))),
      breaks = c(plc_label, bnt_label),
      drop   = FALSE,
      guide  = guide_legend(title = "Treatment Group")
    ) +

    labs(
      title    = "Arm Comparison for Selected Deviations (Counts)",
      subtitle = "Number of subjects with ≥1 deviation (C4591001; through 2021-03-13)",
      caption  = fig_caption,                      # bottom-right if provided
      x = NULL, y = "Subjects with deviation (count)"
    ) +

    # Theme aligned + full white backgrounds for print
    theme_minimal(base_size = s_base) +
    theme(
      plot.title      = element_text(size = s_title, face = "bold", hjust = 0.5),
      plot.subtitle   = element_text(size = s_subtitle, hjust = 0.5, color = "gray35",
                                     margin = margin(b = subtitle_gap * text_scale)),
      plot.caption    = element_text(size = s_subtitle, hjust = 1,
                                     margin = margin(t = 6 * text_scale)),
      plot.caption.position = "plot",

      axis.text       = element_text(size = s_axis),
      axis.title      = element_text(size = s_axis_title, face = "bold"),

      legend.position = "bottom",
      legend.title    = element_text(size = s_legend, face = "bold"),
      legend.text     = element_text(size = s_legend),
      legend.key.size = grid::unit(12 * text_scale, "pt"),
      legend.background     = element_rect(fill = "white", color = NA),
      legend.box.background = element_rect(fill = "white", color = NA),

      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "white", color = NA),
      plot.background  = element_rect(fill = "white", color = NA),

      # match margins (larger bottom for labels)
      plot.margin     = margin(t = (12 + subtitle_gap) * text_scale,
                               r = 10 * text_scale,
                               b = 14 * text_scale,
                               l = 10 * text_scale)
    ) +
    coord_cartesian(clip = "off") +
    geom_text(data = annot,
              aes(x = Deviation, y = y, label = label, color = col),
              inherit.aes = FALSE, vjust = 0, lineheight = 0.95,
              size = s_annot, fontface = "bold", show.legend = FALSE) +
    scale_color_identity()
}

# ---------------------------------------------------------------------
# 3) LOAD DATA
# ---------------------------------------------------------------------

cat("Reading XPT files...\n")
protocol_devs_analysis <- read_xpt(FILES$dv)
protocol_devs_sup      <- read_xpt(FILES$suppdv)

cat(sprintf("addv rows: %d | suppdv rows: %d\n",
            nrow(protocol_devs_analysis), nrow(protocol_devs_sup)))

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

# Coerce keys to character for safe joins/filters
randomized_pop <- randomized_pop %>%
  mutate(
    SUBJID = as.character(SUBJID),
    SITEID = as.character(SITEID),
    ARM    = as.character(ARM)
  )

# ---------------------------------------------------------------------
# 4) SUPPLEMENTARY FLATTENING & JOIN
# ---------------------------------------------------------------------

cat("Extracting DVSEQ from SUPPDV and flattening SOURCE...\n")
protocol_devs_sup <- protocol_devs_sup %>%
  mutate(DVSEQ = suppressWarnings(as.numeric(gsub("\\D", "", as.character(IDVARVAL)))))

flattened_sup <- protocol_devs_sup %>%
  group_by(USUBJID, DVSEQ) %>%
  summarize(SOURCE = paste(QVAL[QNAM == "SOURCE"], collapse = ", "), .groups = "drop")

cat("Joining SUPPDV SOURCE back to ADDV...\n")
flattened_data <- protocol_devs_analysis %>%
  left_join(flattened_sup, by = c("USUBJID","DVSEQ"))

# ---------------------------------------------------------------------
# 5) CLEANING, FILTERS & EXPORT FLAT FILE
# ---------------------------------------------------------------------

cat("Parsing dates, standardizing terms, filtering by cutoff...\n")
flattened_data <- flattened_data %>%
  mutate(
    DVSTDTC   = safe_parse_date(DVSTDTC),
    DVTERM    = normalize_term(DVTERM),
    DVTERM1   = if ("DVTERM1" %in% names(.)) normalize_term(DVTERM1) else NA_character_,
    CONCATTERM = ifelse(!is.na(DVTERM1) & nchar(DVTERM1) > 0, paste(DVTERM, DVTERM1), DVTERM),
    SUBJID     = str_extract(USUBJID, "\\d+$")
  ) %>%
  filter(DVSTDTC <= CUTOFF_DATE)

cat(sprintf("Rows after cutoff (%s): %d\n", CUTOFF_DATE, nrow(flattened_data)))

write.csv(flattened_data, OUTPUTS$flattened_csv, row.names = FALSE)
cat(sprintf("Saved: %s\n", OUTPUTS$flattened_csv))

# ---------------------------------------------------------------------
# 6) LIMIT TO PHASE 3 RANDOMIZED & ARMS OF INTEREST
# ---------------------------------------------------------------------

ARMS_OF_INTEREST <- c("BNT162b2 Phase 2/3 (30 mcg)", "Placebo")

# keep only the two arms
unique_arms <- sort(unique(flattened_data$ARM))
cat("Arms found in ADDV:", paste(unique_arms, collapse = " | "), "\n")

filtered_data <- flattened_data %>%
  mutate(SUBJID = as.character(SUBJID),
         ARM    = as.character(ARM),
         SITEID = as.character(SITEID),
         CONCATTERM = normalize_term(CONCATTERM)) %>%
  filter(ARM %in% ARMS_OF_INTEREST) %>%
  filter(SUBJID %in% randomized_pop$SUBJID)

cat(sprintf("Rows after arm filter & randomized-pop filter: %d\n", nrow(filtered_data)))

# counts per arm in the randomized population (denominators)
arm_counts <- randomized_pop %>%
  filter(ARM %in% ARMS_OF_INTEREST) %>%
  group_by(ARM) %>%
  summarize(total_subjects = n_distinct(SUBJID), .groups = "drop")

stopifnot(nrow(arm_counts) == 2)

# ---------------------------------------------------------------------
# 7) ARM-LEVEL CHI-SQUARE PER DEVIATION
# ---------------------------------------------------------------------

cat("Computing arm-level chi-square per deviation...\n")
deviation_counts <- filtered_data %>%
  distinct(SUBJID, CONCATTERM, ARM) %>%     # 1 subject counted once per deviation
  group_by(CONCATTERM) %>%
  summarize(
    BNT_SUBJECTS     = sum(ARM == ARMS_OF_INTEREST[1]),
    PLACEBO_SUBJECTS = sum(ARM == ARMS_OF_INTEREST[2]),
    .groups = "drop"
  ) %>%
  mutate(TOTAL_SUBJECTS = BNT_SUBJECTS + PLACEBO_SUBJECTS) %>%
  filter(TOTAL_SUBJECTS >= MIN_TOTAL_SUBJECTS_PER_DEV)

# run chi-square per row
chi_calc <- function(bnt, plc, arm_counts_df) {
  bnt_total <- arm_counts_df$total_subjects[arm_counts_df$ARM == ARMS_OF_INTEREST[1]]
  plc_total <- arm_counts_df$total_subjects[arm_counts_df$ARM == ARMS_OF_INTEREST[2]]
  bnt_other <- max(bnt_total - bnt, 0)
  plc_other <- max(plc_total - plc, 0)
  m <- matrix(c(bnt, bnt_other, plc, plc_other), nrow = 2, byrow = TRUE)
  suppressWarnings(chisq.test(m))
}

chi_list <- mapply(
  chi_calc,
  deviation_counts$BNT_SUBJECTS,
  deviation_counts$PLACEBO_SUBJECTS,
  MoreArgs = list(arm_counts_df = arm_counts),
  SIMPLIFY = FALSE
)

deviation_stats <- deviation_counts %>%
  mutate(
    chi_square = vapply(chi_list, function(x) as.numeric(x$statistic), numeric(1)),
    p_value   = vapply(chi_list, function(x) as.numeric(x$p.value),  numeric(1))
  ) %>%
  filter(p_value <= ALPHA) %>%
  mutate(p_value = vapply(p_value, format_p_bin, character(1))) %>%
  arrange(desc(TOTAL_SUBJECTS))

write.csv(deviation_stats, OUTPUTS$arm_stats_csv, row.names = FALSE)
cat(sprintf("Saved: %s\n", OUTPUTS$arm_stats_csv))

# Arm-level significant table (HTML)
ft_arm <- flextable(deviation_stats %>%
                      select(CONCATTERM, BNT_SUBJECTS, PLACEBO_SUBJECTS, p_value)) %>%
  set_header_labels(
    CONCATTERM = "Deviation",
    BNT_SUBJECTS = "BNT162b2 Phase 2/3 (30mcg)",
    PLACEBO_SUBJECTS = "Placebo",
    p_value = "p-value"
  ) %>%
  align(align = "center", part = "all") %>%
  theme_zebra() %>%
  fontsize(size = 16, part = "all") %>%
  padding(padding = 3) %>%
  autofit() %>%
  set_caption("Table 1: Deviations significantly imbalanced between arms")

save_as_html(ft_arm, path = OUTPUTS$arm_stats_html)
cat(sprintf("Saved: %s\n", OUTPUTS$arm_stats_html))

# Use all significant deviations for site-level testing
significant_dev_terms <- unique(deviation_stats$CONCATTERM)
print(deviation_stats)

target_devs_counts <- c(
  "Nasal swab not collected for the visit where it is required",
  "Urine pregnancy test not performed."
)

pair_counts_plot <- create_deviation_pair_plot_counts(
  filtered_data = filtered_data,
  arm_counts    = arm_counts,
  target_terms  = target_devs_counts,
  text_scale    = 1.6,
  fig_caption   = "Figure 2"
)

ggsave(paste0(OUTPUTS$dev_pair_counts_plot, ".png"), pair_counts_plot,
       width = 12, height = 7, dpi = 300, bg = "white")
ggsave(paste0(OUTPUTS$dev_pair_counts_plot, ".pdf"), pair_counts_plot,
       width = 12, height = 7, bg = "white")

generate_plot_html(
  title     = "C4591001 – BNT vs Placebo for Two Deviations (Counts)",
  subtitle  = "Number of subjects with ≥1 deviation; through 2021-03-13",
  plot_base = OUTPUTS$dev_pair_counts_plot,
  outfile   = OUTPUTS$dev_pair_counts_html
)

# ---------------------------------------------------------------------
# 8) SITE-LEVEL FISHER EXACT TESTS (subjects, not events)
# ---------------------------------------------------------------------

cat("Preparing site-level counts (unique subjects per deviation)...\n")

# Only the deviations that were significant at arm level
imbalanced_deviations <- filtered_data %>%
  filter(CONCATTERM %in% significant_dev_terms)

# Per site/arm/deviation: number of UNIQUE subjects with ≥1 such deviation
site_term_counts <- imbalanced_deviations %>%
  group_by(SITEID, ARM, CONCATTERM) %>%
  summarise(n_subjects = n_distinct(SUBJID), .groups = "drop")

# Keep site+deviation combos with enough subjects showing the deviation
filtered_site_devs <- site_term_counts %>%
  group_by(SITEID, CONCATTERM) %>%
  filter(sum(n_subjects) >= MIN_SITE_DEV_EVENTS) %>%  # threshold now means "subjects"
  ungroup()

# Denominators: randomized SUBJECTS per site/arm
randomized_pop_by_arms <- randomized_pop %>%
  filter(ARM %in% ARMS_OF_INTEREST) %>%
  group_by(SITEID, ARM) %>%
  summarise(total_subjects = n_distinct(SUBJID), .groups = "drop") %>%
  filter(SITEID %in% filtered_site_devs$SITEID)

cat(sprintf("Site+deviation combos retained: %d\n",
            n_distinct(paste(filtered_site_devs$SITEID, filtered_site_devs$CONCATTERM))))

# Fisher tests on subject counts
results <- list()
for (site_id in unique(filtered_site_devs$SITEID)) {
  site_rand <- randomized_pop_by_arms %>% filter(SITEID == site_id)
  terms_here <- unique(filtered_site_devs$CONCATTERM[filtered_site_devs$SITEID == site_id])

  for (term in terms_here) {
    site_dev <- filtered_site_devs %>% filter(SITEID == site_id, CONCATTERM == term)

    bnt_dev <- site_dev %>% filter(ARM == ARMS_OF_INTEREST[1]) %>% pull(n_subjects)
    plc_dev <- site_dev %>% filter(ARM == ARMS_OF_INTEREST[2]) %>% pull(n_subjects)
    bnt_dev <- ifelse(length(bnt_dev) == 0 || is.na(bnt_dev), 0L, as.integer(bnt_dev))
    plc_dev <- ifelse(length(plc_dev) == 0 || is.na(plc_dev), 0L, as.integer(plc_dev))

    bnt_total <- site_rand %>% filter(ARM == ARMS_OF_INTEREST[1]) %>% pull(total_subjects)
    plc_total <- site_rand %>% filter(ARM == ARMS_OF_INTEREST[2]) %>% pull(total_subjects)
    bnt_total <- ifelse(length(bnt_total) == 0 || is.na(bnt_total), 0L, as.integer(bnt_total))
    plc_total <- ifelse(length(plc_total) == 0 || is.na(plc_total), 0L, as.integer(plc_total))

    # skip if no denominators
    if (bnt_total + plc_total == 0) next

    # safety: cap numerators at denominators
    bnt_dev <- min(bnt_dev, bnt_total)
    plc_dev <- min(plc_dev, plc_total)

    m <- matrix(c(bnt_dev, bnt_total - bnt_dev,
                  plc_dev, plc_total - plc_dev),
                nrow = 2, byrow = TRUE)

    if (any(!is.finite(m)) || any(m < 0)) next  # extra guard

    ft <- suppressWarnings(fisher.test(m))

    if (!is.na(ft$p.value) && ft$p.value <= ALPHA) {
      results[[length(results) + 1]] <- data.frame(
        SITEID = site_id,
        CONCATTERM = term,
        bnt162b2_deviations = bnt_dev,
        bnt162b2_no_deviations = bnt_total - bnt_dev,
        placebo_deviations = plc_dev,
        placebo_no_deviations = plc_total - plc_dev,
        fisher_exact_pvalue = ft$p.value
      )
    }
  }
}

deviations_significant_results <- if (length(results)) bind_rows(results) else {
  data.frame(SITEID=character(), CONCATTERM=character(),
             bnt162b2_deviations=numeric(), bnt162b2_no_deviations=numeric(),
             placebo_deviations=numeric(), placebo_no_deviations=numeric(),
             fisher_exact_pvalue=numeric())
}

if (nrow(deviations_significant_results) > 0) {
  deviations_significant_results <- deviations_significant_results %>%
    mutate(fisher_exact_pvalue = vapply(fisher_exact_pvalue, format_p_bin, character(1))) %>%
    arrange(SITEID, CONCATTERM)

  write.csv(deviations_significant_results, OUTPUTS$site_stats_csv, row.names = FALSE)
  cat(sprintf("Saved: %s\n", OUTPUTS$site_stats_csv))

  ft_site <- flextable(deviations_significant_results) %>%
    set_header_labels(
      SITEID = "Site Id",
      CONCATTERM = "Deviation",
      bnt162b2_deviations = "BNT162b2 Deviations",
      bnt162b2_no_deviations = "BNT162b2 No Deviations",
      placebo_deviations = "Placebo Deviations",
      placebo_no_deviations = "Placebo No Deviations",
      fisher_exact_pvalue = "p-value"
    ) %>%
    align(align = "center", part = "all") %>%
    theme_zebra() %>%
    fontsize(size = 16, part = "all") %>%
    padding(padding = 3) %>%
    autofit() %>%
    set_caption("Table 2: Site-level significant imbalances (Fisher's exact)")

  save_as_html(ft_site, path = OUTPUTS$site_stats_html)
  cat(sprintf("Saved: %s\n", OUTPUTS$site_stats_html))
} else {
  cat("No site-level significant imbalances found at alpha = 0.05.\n")
}

# ---------------------------------------------------------------------
# 9) WEEKLY TIME-SERIES PLOTS
# ---------------------------------------------------------------------

# helper to add YEARWEEK "YYYY-WW"
add_yearweek <- function(df, date_col = "DVSTDTC") {
  d <- df[[date_col]]
  df$YEARWEEK <- paste0(year(d), "-", sprintf("%02d", isoweek(d)))
  df
}

# --- Staff-related imbalanced deviations (weekly) ---
staff_dev_df <- filtered_data %>%
  mutate(CONCATTERM = normalize_term(CONCATTERM)) %>%
  filter(CONCATTERM %in% normalize_term(STAFF_RELATED_DEVS)) %>%
  mutate(DVSTDTC = safe_parse_date(DVSTDTC)) %>%
  add_yearweek("DVSTDTC") %>%
  group_by(YEARWEEK) %>%
  summarize(total_deviations = n(), .groups = "drop")

if (nrow(staff_dev_df) > 0) {
  p1 <- ggplot(staff_dev_df, aes(x = YEARWEEK, y = total_deviations)) +
    geom_col(fill = "#D3D3D3") +
    labs(title = 'C4591001 - Total "Staff-Related" Imbalanced Deviations by ISO Year-Week',
         x = "Year-Week", y = "Total Deviations") +
    theme_minimal(base_size = 18) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
  ggsave(OUTPUTS$staff_weekly_png, p1, width = 12, height = 6, dpi = 300)
  cat(sprintf("Saved: %s\n", OUTPUTS$staff_weekly_png))
} else {
  cat("No staff-related deviations found for weekly plot.\n")
}

# --- Other nonstudy coronavirus vaccine (weekly) ---
other_vaccine_df <- filtered_data %>%
  filter(CONCATTERM == normalize_term("Receipt of any other nonstudy coronavirus vaccine at any time prior to or during the study.")) %>%
  mutate(DVSTDTC = safe_parse_date(DVSTDTC)) %>%
  add_yearweek("DVSTDTC") %>%
  group_by(YEARWEEK) %>%
  summarize(total_deviations = n(), .groups = "drop")

if (nrow(other_vaccine_df) > 0) {
  p2 <- ggplot(other_vaccine_df, aes(x = YEARWEEK, y = total_deviations)) +
    geom_col(fill = "#A0A0A0") +
    labs(title = 'C4591001 - Total "Other Vaccine" Deviations by ISO Year-Week',
         x = "Year-Week", y = "Total Deviations") +
    theme_minimal(base_size = 18) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
  ggsave(OUTPUTS$other_vax_weekly_png, p2, width = 12, height = 6, dpi = 300)
  cat(sprintf("Saved: %s\n", OUTPUTS$other_vax_weekly_png))
} else {
  cat("No 'other vaccine' deviations found for weekly plot.\n")
}

cat("\nDone.\n")
