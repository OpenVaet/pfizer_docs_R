# Load necessary package
library(haven)
library(dplyr)
library(readr)
library(jsonlite)
library(ggplot2)

eua_adsl_file <- 'eua_data/xpt_data/FDA-CBER-2021-5683-1226624-1227706_27034_S1_M5_c4591001-ia efficacy-A-adsl.xpt'

bla_adsl_file <- 'xpt_data/FDA-CBER-2021-5683-0772469-0773670_125742_S1_M5_C4591001-A-D_adsl.xpt'

# Ensure needed packages are available
suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(readr)
  library(jsonlite)
})

# Output folder
out_dir <- "analysis"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

read_xpt_compat <- function(path) {
  tryCatch(
    haven::read_xpt(path, name_repair = "minimal"),
    error = function(e) haven::read_xpt(path)
  )
}

eua <- read_xpt_compat(eua_adsl_file)
bla <- read_xpt_compat(bla_adsl_file)


# Basic checks
if (!("SUBJID" %in% names(eua))) stop("SUBJID not found in EUA dataset.")
if (!("SUBJID" %in% names(bla))) stop("SUBJID not found in BLA dataset.")

# Always compare SUBJID as character
eua <- eua %>% mutate(SUBJID = as.character(SUBJID))
bla <- bla %>% mutate(SUBJID = as.character(SUBJID))

# Helper to normalize values for fair comparison
normalize_vec <- function(x) {
  x <- haven::zap_labels(x)              # drop labelled class, keep underlying values
  if (inherits(x, "POSIXt")) {
    return(format(x, "%Y-%m-%d %H:%M:%S", tz = "UTC"))
  }
  if (inherits(x, "Date")) {
    return(as.character(x))              # ISO yyyy-mm-dd
  }
  if (is.numeric(x)) {
    return(format(x, trim = TRUE, scientific = FALSE, digits = 15))
  }
  # default: character/factor/logical/etc.
  x <- as.character(x)
  x <- trimws(x)
  x[x == ""] <- NA
  x
}

# 1) Columns added/removed ------------------------------------------------------
cols_eua <- names(eua)
cols_bla <- names(bla)

all_cols <- union(cols_eua, cols_bla)
col_report <- tibble::tibble(
  column = all_cols,
  in_eua = column %in% cols_eua,
  in_bla = column %in% cols_bla
) %>%
  mutate(status = dplyr::case_when(
    in_eua & in_bla ~ "common",
    in_eua & !in_bla ~ "only_in_eua",
    !in_eua & in_bla ~ "only_in_bla",
    TRUE ~ "unknown"
  )) %>%
  arrange(match(status, c("only_in_eua","only_in_bla","common")), column)

write_csv(col_report, file.path(out_dir, "adsl_eua_vs_bla_columns.csv"))

# 2) SUBJIDs inserted/deleted ---------------------------------------------------
subj_eua <- unique(eua$SUBJID)
subj_bla <- unique(bla$SUBJID)

subj_only_in_eua <- setdiff(subj_eua, subj_bla)
subj_only_in_bla <- setdiff(subj_bla, subj_eua)
subj_common      <- intersect(subj_eua, subj_bla)

subj_changes <- tibble::tibble(
  SUBJID = c(subj_only_in_eua, subj_only_in_bla),
  status = c(
    rep("only_in_eua", length(subj_only_in_eua)),
    rep("only_in_bla", length(subj_only_in_bla))
  )
) %>% arrange(status, SUBJID)

write_csv(subj_changes, file.path(out_dir, "adsl_eua_vs_bla_subjid_changes.csv"))

# Also flag any duplicate SUBJIDs in either file (useful sanity check)
dup_eua <- eua %>% count(SUBJID, name = "n") %>% filter(n > 1) %>% mutate(dataset = "EUA")
dup_bla <- bla %>% count(SUBJID, name = "n") %>% filter(n > 1) %>% mutate(dataset = "BLA")
dup_both <- bind_rows(dup_eua, dup_bla) %>% select(dataset, SUBJID, n)
if (nrow(dup_both) > 0) {
  write_csv(dup_both, file.path(out_dir, "adsl_eua_vs_bla_duplicate_subjid.csv"))
}

# 3) Value comparisons for common SUBJIDs & columns ----------------------------
common_cols <- intersect(cols_eua, cols_bla)
common_cols_no_id <- setdiff(common_cols, "SUBJID")

# Keep only common SUBJIDs and common columns
eua_common <- eua %>%
  filter(SUBJID %in% subj_common) %>%
  select(SUBJID, dplyr::all_of(common_cols_no_id))

bla_common <- bla %>%
  filter(SUBJID %in% subj_common) %>%
  select(SUBJID, dplyr::all_of(common_cols_no_id))

# Join and compare
joined <- inner_join(
  eua_common, bla_common,
  by = "SUBJID", suffix = c("_eua", "_bla")
)

# Build a long table of differences
diff_rows <- list()

for (col in common_cols_no_id) {
  e_col <- normalize_vec(joined[[paste0(col, "_eua")]])
  b_col <- normalize_vec(joined[[paste0(col, "_bla")]])
  
  different <- xor(is.na(e_col), is.na(b_col)) | (!is.na(e_col) & !is.na(b_col) & e_col != b_col)
  
  if (any(different)) {
    idx <- which(different)
    if (length(idx) > 0) {
      diff_rows[[col]] <- tibble::tibble(
        SUBJID = joined$SUBJID[idx],
        variable = col,
        value_eua = e_col[idx],
        value_bla = b_col[idx]
      )
    }
  }
}

diff_table <- if (length(diff_rows)) bind_rows(diff_rows) else tibble::tibble(
  SUBJID = character(), variable = character(), value_eua = character(), value_bla = character()
)

write_csv(diff_table, file.path(out_dir, "adsl_eua_vs_bla_value_differences.csv"))

# Per-variable summary (handy overview)
by_var <- diff_table %>%
  count(variable, name = "n_differences") %>%
  arrange(desc(n_differences))

write_csv(by_var, file.path(out_dir, "adsl_eua_vs_bla_value_differences_by_variable.csv"))

# JSON summary ------------------------------------------------------------------
summary_list <- list(
  columns = list(
    n_eua = length(cols_eua),
    n_bla = length(cols_bla),
    n_common = length(common_cols),
    only_in_eua = sort(setdiff(cols_eua, cols_bla)),
    only_in_bla = sort(setdiff(cols_bla, cols_eua))
  ),
  subjid = list(
    n_eua = length(subj_eua),
    n_bla = length(subj_bla),
    n_common = length(subj_common),
    only_in_eua = length(subj_only_in_eua),
    only_in_bla = length(subj_only_in_bla)
  ),
  value_differences = list(
    total_rows = nrow(diff_table),
    by_variable = setNames(as.list(by_var$n_differences), by_var$variable)
  ),
  notes = list(
    comparison = "Exact match after normalization (trimmed strings; empty strings -> NA; dates to ISO; numeric formatted non-scientific with up to 15 digits)."
  )
)

write_json(summary_list, file.path(out_dir, "adsl_eua_vs_bla_summary.json"), pretty = TRUE, auto_unbox = TRUE)

# Console summary
cat("\nReports written to '", out_dir, "':\n", sep = "")
cat(" - eua_vs_bla_columns.csv\n")
cat(" - eua_vs_bla_subjid_changes.csv\n")
if (exists("dup_both") && nrow(dup_both) > 0) cat(" - eua_vs_bla_duplicate_subjid.csv\n")
cat(" - eua_vs_bla_value_differences.csv\n")
cat(" - eua_vs_bla_value_differences_by_variable.csv\n")
cat(" - eua_vs_bla_summary.json\n\n")

# ---- Visual HTML report: ADSL EUA vs BLA ----
suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
})

report_html <- file.path(out_dir, "eua_vs_bla_adsl_data.html")

# Helpers -----------------------------------------------------------------------
human_readable <- function(bytes) {
  if (is.na(bytes)) return("—")
  units <- c("B","KB","MB","GB","TB"); pow <- ifelse(bytes > 0, floor(log(bytes, 1024)), 0)
  pow[!is.finite(pow)] <- 0; pow <- pmax(0, pmin(pow, length(units) - 1))
  res <- bytes / (1024^pow)
  paste0(format(round(res, 2), big.mark = ",", nsmall = 2, trim = TRUE), " ", units[pow + 1])
}
html_escape <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x
}
html_list <- function(vec, max_n = 30) {
  if (length(vec) == 0) return("<p class='muted'>None</p>")
  shown <- vec[seq_len(min(max_n, length(vec)))]
  more <- length(vec) - length(shown)
  paste0(
    "<ul>", paste0("<li><code>", html_escape(shown), "</code></li>", collapse = ""), "</ul>",
    if (more > 0) paste0("<p class='muted'>(+", more, " more not shown)</p>") else ""
  )
}

# File metadata -----------------------------------------------------------------
fi_eua <- tryCatch(file.info(eua_adsl_file), error = function(e) data.frame(size=NA, mtime=as.POSIXct(NA)))
fi_bla <- tryCatch(file.info(bla_adsl_file), error = function(e) data.frame(size=NA, mtime=as.POSIXct(NA)))

size_eua <- human_readable(fi_eua$size[1])
size_bla <- human_readable(fi_bla$size[1])
mtime_eua <- if (is.na(fi_eua$mtime[1])) "—" else format(fi_eua$mtime[1], "%Y-%m-%d %H:%M:%S")
mtime_bla <- if (is.na(fi_bla$mtime[1])) "—" else format(fi_bla$mtime[1], "%Y-%m-%d %H:%M:%S")

# Derived summaries -------------------------------------------------------------
n_cols_eua <- length(cols_eua)
n_cols_bla <- length(cols_bla)
n_cols_common <- length(common_cols)

n_subj_eua <- length(subj_eua)
n_subj_bla <- length(subj_bla)
n_subj_common <- length(subj_common)

only_cols_eua <- sort(setdiff(cols_eua, cols_bla))
only_cols_bla <- sort(setdiff(cols_bla, cols_eua))

# Duplicates count (from earlier dup_* frames)
dup_n_eua <- if (exists("dup_eua")) nrow(dup_eua) else 0L
dup_n_bla <- if (exists("dup_bla")) nrow(dup_bla) else 0L

# Chart: Top variables by differences ------------------------------------------
chart_file <- file.path(out_dir, "adsl_diff_by_variable_top20.png")
if (exists("by_var") && nrow(by_var) > 0) {
  top_by_var <- by_var %>% arrange(desc(n_differences)) %>% head(20)
  p <- ggplot(top_by_var, aes(x = reorder(variable, n_differences), y = n_differences)) +
    geom_col() +
    coord_flip() +
    labs(title = "Top 20 variables by # of differing rows",
         x = "Variable", y = "# differing rows") +
    theme_minimal(base_size = 12)
  ggsave(chart_file, p, width = 9, height = 6, dpi = 150)
} else {
  chart_file <- NA_character_
}

# SUBJIDs only in each set (show up to 30) -------------------------------------
subj_only_in_eua_vec <- sort(subj_only_in_eua)
subj_only_in_bla_vec <- sort(subj_only_in_bla)

# -------------------------------------------------------------------------------

# ---------- A) FULL V01DT / V02DT anomaly table + ADSL enrichment + tests -----

# robust date parser already defined above: as_date_smart()

# Handle occasional typos in column names gracefully
v01_name <- if ("V01DT" %in% names(eua) && "V01DT" %in% names(bla)) "V01DT" else if ("VO1DT" %in% names(eua) && "VO1DT" %in% names(bla)) "VO1DT" else NULL
v02_name <- if ("V02DT" %in% names(eua) && "V02DT" %in% names(bla)) "V02DT" else if ("VO2DT" %in% names(eua) && "VO2DT" %in% names(bla)) "VO2DT" else NULL

get_date_diff <- function(var) {
  if (is.null(var)) return(dplyr::tibble())
  e <- eua %>% dplyr::select(SUBJID, value_eua = dplyr::all_of(var)) %>% dplyr::mutate(value_eua = as_date_smart(value_eua))
  b <- bla %>% dplyr::select(SUBJID, value_bla = dplyr::all_of(var)) %>% dplyr::mutate(value_bla = as_date_smart(value_bla))
  df <- dplyr::inner_join(e, b, by = "SUBJID") %>%
    dplyr::mutate(changed = !is.na(value_eua) & !is.na(value_bla) & value_eua != value_bla)
  df %>%
    dplyr::filter(changed) %>%
    dplyr::transmute(SUBJID, variable = var, value_eua = as.character(value_eua), value_bla = as.character(value_bla))
}

v01_diff <- get_date_diff(v01_name)
v02_diff <- get_date_diff(v02_name)

v01_v02_diff <- dplyr::bind_rows(v01_diff, v02_diff)

# Enrich with BLA ADSL context
adsl_pick <- bla %>%
  dplyr::transmute(
    SUBJID = as.character(SUBJID),
    ARM    = as.character(ARM),
    RANDDT = as.character(as_date_smart(RANDDT)),
    VAX101DT = as.character(as_date_smart(VAX101DT)),
    VAX102DT = as.character(as_date_smart(VAX102DT))
  )

v01_v02_diff_enriched <- v01_v02_diff %>%
  dplyr::left_join(adsl_pick, by = "SUBJID") %>%
  dplyr::relocate(ARM, RANDDT, VAX101DT, VAX102DT, .after = SUBJID)

# (optional) Save the full list to CSV as an artifact
readr::write_csv(v01_v02_diff_enriched, file.path(out_dir, "adsl_v01_v02_date_changes_full.csv"))

# Build HTML rows for the full list
if (exists("html_escape") == FALSE) {
  html_escape <- function(x) {
    x <- gsub("&", "&amp;", x, fixed = TRUE)
    x <- gsub("<", "&lt;",  x, fixed = TRUE)
    x <- gsub(">", "&gt;",  x, fixed = TRUE)
    x
  }
}

differences_block_table_rows <- if (nrow(v01_v02_diff_enriched) > 0) {
  paste0(
    "<tr>",
    "<td><code>", html_escape(v01_v02_diff_enriched$SUBJID), "</code></td>",
    "<td><code>", html_escape(v01_v02_diff_enriched$ARM), "</code></td>",
    "<td>", html_escape(v01_v02_diff_enriched$RANDDT), "</td>",
    "<td>", html_escape(v01_v02_diff_enriched$VAX101DT), "</td>",
    "<td>", html_escape(v01_v02_diff_enriched$VAX102DT), "</td>",
    "<td><code>", html_escape(v01_v02_diff_enriched$variable), "</code></td>",
    "<td>", html_escape(ifelse(is.na(v01_v02_diff_enriched$value_eua), "NA", v01_v02_diff_enriched$value_eua)), "</td>",
    "<td>", html_escape(ifelse(is.na(v01_v02_diff_enriched$value_bla), "NA", v01_v02_diff_enriched$value_bla)), "</td>",
    "</tr>",
    collapse = "\n"
  )
} else {
  "<tr><td colspan='8' class='muted'>No V01DT/V02DT anomalies detected.</td></tr>"
}

# ---------- B) Per-ARM tests vs full ADSL randomized base (two arms) ----------

allowed_arms <- c("Placebo", "BNT162b2 Phase 2/3 (30 mcg)")

eligible_subj_adsl <- bla %>%
  dplyr::mutate(ARM = stringr::str_trim(as.character(ARM))) %>%
  dplyr::filter(ARM %in% allowed_arms, !is.na(RANDDT)) %>%  # randomized + target arms
  dplyr::distinct(SUBJID, ARM)

fmt_p <- function(p) {
  if (is.na(p)) return("NA")
  if (p < 1e-4) return("&lt;0.0001")
  sprintf("%.4f", p)
}

run_arm_test_vs_adsl <- function(affected_ids, label, eligible_base = eligible_subj_adsl) {
  base_counts <- eligible_base %>%
    dplyr::mutate(affected = SUBJID %in% affected_ids) %>%
    dplyr::count(ARM, affected, name = "n") %>%
    tidyr::complete(ARM, affected, fill = list(n = 0)) %>%
    tidyr::pivot_wider(names_from = affected, values_from = n) %>%
    dplyr::rename(not_affected = `FALSE`, affected = `TRUE`) %>%
    dplyr::mutate(total = affected + not_affected,
                  affected_pct = ifelse(total > 0, round(100 * affected / total, 1), 0))
  mat <- base_counts %>%
    dplyr::select(ARM, affected, not_affected) %>%
    tibble::column_to_rownames("ARM") %>%
    as.matrix()

  suppressWarnings({ chi <- try(chisq.test(mat), silent = TRUE) })
  use_fisher <- FALSE; use_sim <- FALSE
  if (inherits(chi, "htest")) {
    exp_min <- min(chi$expected)
    if (nrow(mat) == 2 && ncol(mat) == 2 && exp_min < 5) use_fisher <- TRUE
    else if (exp_min < 5) use_sim <- TRUE
  } else {
    if (nrow(mat) == 2 && ncol(mat) == 2) use_fisher <- TRUE else use_sim <- TRUE
  }

  if (use_fisher) {
    tst <- fisher.test(mat); method <- "Fisher's exact test"; pval <- tst$p.value
  } else if (use_sim) {
    tst <- chisq.test(mat, simulate.p.value = TRUE, B = 10000)
    method <- "Chi-square test (simulated p-value)"; pval <- tst$p.value
  } else {
    tst <- chi; method <- "Chi-square test"; pval <- tst$p.value
  }

  list(label = label, method = method, pval = pval, counts = base_counts)
}

# affected subject sets
v01_affected_ids <- unique(v01_diff$SUBJID)
v02_affected_ids <- unique(v02_diff$SUBJID)

v01_test <- run_arm_test_vs_adsl(v01_affected_ids, ifelse(is.null(v01_name), "V01DT (not found)", paste0(v01_name, " changed")))
v02_test <- run_arm_test_vs_adsl(v02_affected_ids, ifelse(is.null(v02_name), "V02DT (not found)", paste0(v02_name, " changed")))

render_test_table_html <- function(test_res) {
  if (is.null(test_res) || is.null(test_res$counts)) return("")
  rows <- paste0(
    "<tr>",
    "<td>", html_escape(test_res$counts$ARM), "</td>",
    "<td class='num'>", test_res$counts$affected, "</td>",
    "<td class='num'>", test_res$counts$not_affected, "</td>",
    "<td class='num'>", test_res$counts$total, "</td>",
    "<td class='num'>", sprintf("%.1f", test_res$counts$affected_pct), "</td>",
    "</tr>",
    collapse = "\n"
  )
  paste0(
    "<h3>", html_escape(test_res$label), " — ", html_escape(test_res$method),
    " (p = ", fmt_p(test_res$pval), ")</h3>",
    "<table><thead><tr>",
    "<th>ARM</th><th class='num'>Affected</th><th class='num'>Not affected</th><th class='num'>Total</th><th class='num'>Affected (%)</th>",
    "</tr></thead><tbody>", rows, "</tbody></table>"
  )
}

# Assemble the new block that will REPLACE the old sample table in the HTML
differences_block <- paste0(
  "<h2>Value-level differences: V01DT &amp; V02DT (complete list)</h2>",
  "<table>",
  "<thead><tr>",
  "<th>SUBJID</th><th>ARM</th><th>RANDDT</th><th>VAX101DT</th><th>VAX102DT</th>",
  "<th>Variable</th><th>EUA value</th><th>BLA value</th>",
  "</tr></thead>",
  "<tbody>", differences_block_table_rows, "</tbody>",
  "</table>",
  "<h2>Association of V01DT/V02DT changes with ARM</h2>",
  render_test_table_html(v01_test),
  render_test_table_html(v02_test)
)

# -------------------------------------------------------------------------------

# ---- V01DT / V02DT date-delta stats (BLA − EUA) ------------------------------

# We already resolved v01_name / v02_name and have as_date_smart() above.
compute_date_delta_stats <- function(var) {
  if (is.null(var)) return(NULL)
  e <- eua %>% dplyr::select(SUBJID, e = dplyr::all_of(var)) %>% dplyr::mutate(e = as_date_smart(e))
  b <- bla %>% dplyr::select(SUBJID, b = dplyr::all_of(var)) %>% dplyr::mutate(b = as_date_smart(b))
  df <- dplyr::inner_join(e, b, by = "SUBJID") %>%
    dplyr::filter(!is.na(e) & !is.na(b)) %>%
    dplyr::transmute(delta = as.integer(b - e))
  if (nrow(df) == 0) {
    return(list(var = var, n_pairs = 0,
                min = NA, median = NA, mean = NA, max = NA,
                n_changed = 0, min_changed = NA, median_changed = NA,
                mean_changed = NA, max_changed = NA))
  }
  d <- df$delta
  ch <- d[d != 0]
  list(
    var           = var,
    n_pairs       = length(d),
    min           = suppressWarnings(min(d, na.rm = TRUE)),
    median        = suppressWarnings(stats::median(d, na.rm = TRUE)),
    mean          = suppressWarnings(mean(d, na.rm = TRUE)),
    max           = suppressWarnings(max(d, na.rm = TRUE)),
    n_changed     = length(ch),
    min_changed   = if (length(ch)) min(ch) else NA,
    median_changed= if (length(ch)) stats::median(ch) else NA,
    mean_changed  = if (length(ch)) mean(ch) else NA,
    max_changed   = if (length(ch)) max(ch) else NA
  )
}

v01_stats <- compute_date_delta_stats(v01_name)
v02_stats <- compute_date_delta_stats(v02_name)

# HTML helpers
if (!exists("html_escape")) {
  html_escape <- function(x) {
    x <- gsub("&", "&amp;", x, fixed = TRUE)
    x <- gsub("<", "&lt;",  x, fixed = TRUE)
    x <- gsub(">", "&gt;",  x, fixed = TRUE)
    x
  }
}
fmt_int  <- function(x) ifelse(is.null(x) || is.na(x), "—", format(round(as.numeric(x)), big.mark = ","))
fmt_mean <- function(x) ifelse(is.null(x) || is.na(x), "—", sprintf("%.1f", as.numeric(x)))

build_stats_row <- function(st) {
  if (is.null(st)) return("")
  paste0(
    "<tr>",
    "<td><code>", html_escape(st$var), "</code></td>",
    "<td class='num'>", fmt_int(st$n_pairs), "</td>",
    "<td class='num'>", fmt_int(st$min), "</td>",
    "<td class='num'>", fmt_int(st$median), "</td>",
    "<td class='num'>", fmt_mean(st$mean), "</td>",
    "<td class='num'>", fmt_int(st$max), "</td>",
    "<td class='num'>", fmt_int(st$n_changed), "</td>",
    "<td class='num'>", fmt_int(st$min_changed), "</td>",
    "<td class='num'>", fmt_int(st$median_changed), "</td>",
    "<td class='num'>", fmt_mean(st$mean_changed), "</td>",
    "<td class='num'>", fmt_int(st$max_changed), "</td>",
    "</tr>"
  )
}

stats_rows <- paste0(
  build_stats_row(v01_stats),
  build_stats_row(v02_stats)
)

date_delta_stats_block <- paste0(
  "<h2>V01DT / V02DT date differences (BLA − EUA, in days)</h2>",
  "<p class='muted small'>Metrics shown across all comparable pairs (both EUA and BLA present); the right-side columns are restricted to rows where the date actually changed (Δ ≠ 0).</p>",
  "<table>",
  "<thead><tr>",
  "<th>Variable</th><th class='num'>N pairs</th><th class='num'>Min</th><th class='num'>Median</th><th class='num'>Average</th><th class='num'>Max</th>",
  "<th class='num'>N changed</th><th class='num'>Min (chg)</th><th class='num'>Median (chg)</th><th class='num'>Average (chg)</th><th class='num'>Max (chg)</th>",
  "</tr></thead><tbody>",
  if (nchar(stats_rows) > 0) stats_rows else "<tr><td colspan='11' class='muted'>No comparable pairs for V01DT/V02DT.</td></tr>",
  "</tbody></table>"
)

# -------------------------------------------------------------------------------

# Build HTML --------------------------------------------------------------------
generated_on <- format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")
by_var_rows <- if (exists("by_var") && nrow(by_var) > 0) {
  paste0(
    "<tr><td><code>", html_escape(by_var$variable), "</code></td>",
    "<td class='num'>", format(by_var$n_differences, big.mark = ","), "</td></tr>",
    collapse = "\n"
  )
} else "<tr><td colspan='2' class='muted'>No variables with differences.</td></tr>"

html <- paste0(
  "<!doctype html><html lang='en'><head><meta charset='utf-8'>",
  "<meta name='viewport' content='width=device-width, initial-scale=1'>",
  "<title>ADSL EUA vs BLA — Visual Comparison</title>",
  "<style>
body{font:16px/1.6 'Georgia',serif;max-width:1000px;margin:48px auto;padding:0 20px;color:#222}
h1,h2{font-weight:700;margin:0 0 12px}
h1{font-size:30px} h2{font-size:22px;margin-top:28px}
p{margin:8px 0 14px}.muted{color:#666}
.grid{display:grid;grid-template-columns:repeat(auto-fit,minmax(220px,1fr));gap:12px;margin:12px 0 4px}
.card{border:1px solid #e6e6e6;border-radius:10px;padding:12px 14px;background:#fff;box-shadow:0 1px 2px rgba(0,0,0,.03)}
.card h3{margin:0 0 6px;font-size:16px}
.metric{font-weight:700}
table{border-collapse:collapse;width:100%;margin:10px 0 26px}
th,td{padding:10px 12px;border-bottom:1px solid #ddd;vertical-align:top}
thead th{font-variant:small-caps;letter-spacing:.04em;text-align:left}
tbody tr:nth-child(odd){background:#fafafa}
td.num, th.num{text-align:right}
code{background:#f6f6f6;padding:2px 4px;border-radius:4px}
.small{font-size:90%}
</style></head><body>",
  
  "<h1>ADSL EUA vs BLA — Visual Comparison</h1>",
  "<p class='muted'>Generated on ", html_escape(generated_on), "</p>",
  
  "<div class='grid'>",
  "<div class='card'><h3>Subjects (EUA)</h3><div class='metric'>", format(n_subj_eua, big.mark=","), "</div></div>",
  "<div class='card'><h3>Subjects (BLA)</h3><div class='metric'>", format(n_subj_bla, big.mark=","), "</div></div>",
  "<div class='card'><h3>Common subjects</h3><div class='metric'>", format(n_subj_common, big.mark=","), "</div></div>",
  "<div class='card'><h3>Columns (EUA/BLA/common)</h3><div class='metric'>",
  format(n_cols_eua, big.mark=",")," / ",format(n_cols_bla, big.mark=",")," / ",format(n_cols_common, big.mark=","),"</div></div>",
  "<div class='card'><h3>Duplicates (EUA)</h3><div class='metric'>", format(dup_n_eua, big.mark=","), "</div></div>",
  "<div class='card'><h3>Duplicates (BLA)</h3><div class='metric'>", format(dup_n_bla, big.mark=","), "</div></div>",
  "</div>",
  
  "<h2>Files</h2>",
  "<table><tbody>",
  "<tr><th>EUA file</th><td><code>", html_escape(normalizePath(eua_adsl_file, winslash = "/", mustWork = FALSE)), "</code>",
  " <span class='small muted'>(size ", size_eua, ", mtime ", html_escape(mtime_eua), ")</span></td></tr>",
  "<tr><th>BLA file</th><td><code>", html_escape(normalizePath(bla_adsl_file, winslash = "/", mustWork = FALSE)), "</code>",
  " <span class='small muted'>(size ", size_bla, ", mtime ", html_escape(mtime_bla), ")</span></td></tr>",
  "</tbody></table>",
  
  "<h2>Differences overview</h2>",
  "<table>",
  "<thead><tr><th>Variable</th><th class='num'># differing rows</th></tr></thead>",
  "<tbody>", by_var_rows, "</tbody>",
  "</table>",
  
  if (!is.na(chart_file)) paste0("<p><img src='", html_escape(basename(chart_file)), "' alt='Top variables by differences' style='max-width:100%'></p>") else
    "<p class='muted'>No differences to chart.</p>",
  
  "<h2>Columns only in EUA</h2>",  html_list(only_cols_eua, max_n = 50),
  "<h2>Columns only in BLA</h2>",  html_list(only_cols_bla, max_n = 50),
  
  "<h2>SUBJIDs only in EUA</h2>", html_list(subj_only_in_eua_vec, max_n = 50),
  "<h2>SUBJIDs only in BLA</h2>", html_list(subj_only_in_bla_vec, max_n = 50),

  "<h2>Differences overview</h2>",
  "<table>",
  "<thead><tr><th>Variable</th><th class='num'># differing rows</th></tr></thead>",
  "<tbody>", by_var_rows, "</tbody>",
  "</table>",
  
  if (!is.na(chart_file)) paste0("<p><img src='", html_escape(basename(chart_file)), "' alt='Top variables by differences' style='max-width:100%'></p>") else
    "<p class='muted'>No differences to chart.</p>",
  
  "<h2>Columns only in EUA</h2>",  html_list(only_cols_eua, max_n = 50),
  "<h2>Columns only in BLA</h2>",  html_list(only_cols_bla, max_n = 50),
  
  "<h2>SUBJIDs only in EUA</h2>", html_list(subj_only_in_eua_vec, max_n = 50),
  "<h2>SUBJIDs only in BLA</h2>", html_list(subj_only_in_bla_vec, max_n = 50),
  
  # >>> Add this line:
  date_delta_stats_block,
  
  # Then keep your full list + tests section:
  differences_block,
  
  "<p class='muted small'>Full CSVs are available in <code>", html_escape(normalizePath(out_dir, winslash = "/", mustWork = FALSE)), "</code>.</p>",
  
  "</body></html>"
)

writeLines(html, report_html, useBytes = TRUE)
message("Wrote visual report to: ", report_html)

# ---- Visuals for date-variable changes (V01DT / V02DT) -----------------------
suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
})

# Robust date parser that handles Date/POSIXt/character and SAS numeric dates
as_date_smart <- function(x) {
  x <- haven::zap_labels(x)
  if (inherits(x, "Date"))   return(as.Date(x))
  if (inherits(x, "POSIXt")) return(as.Date(x))
  if (is.numeric(x)) {
    # assume SAS date (days since 1960-01-01)
    return(as.Date(x, origin = "1960-01-01"))
  }
  x <- trimws(as.character(x))
  x[x == ""] <- NA_character_
  out <- suppressWarnings(as.Date(x))  # ISO yyyy-mm-dd first
  if (any(is.na(out) & !is.na(x))) {
    try_formats <- c("%Y-%m-%d","%d%b%Y","%d-%b-%Y","%m/%d/%Y","%d/%m/%Y","%b %d, %Y","%d %b %Y")
    for (fmt in try_formats) {
      miss <- is.na(out) & !is.na(x)
      if (!any(miss)) break
      out[miss] <- suppressWarnings(as.Date(x[miss], format = fmt))
    }
  }
  out
}

# If html_escape isn't in scope (older session), define it
if (!exists("html_escape")) {
  html_escape <- function(x) {
    x <- gsub("&", "&amp;", x, fixed = TRUE)
    x <- gsub("<", "&lt;",  x, fixed = TRUE)
    x <- gsub(">", "&gt;",  x, fixed = TRUE)
    x
  }
}

make_date_change_plots <- function(var) {
  if (!(var %in% names(eua)) || !(var %in% names(bla))) return(NULL)
  
  df <- eua %>%
    select(SUBJID, value_eua = dplyr::all_of(var)) %>%
    mutate(value_eua = as_date_smart(value_eua)) %>%
    inner_join(
      bla %>% select(SUBJID, value_bla = dplyr::all_of(var)) %>%
        mutate(value_bla = as_date_smart(value_bla)),
      by = "SUBJID"
    )
  
  # keep only rows where both dates are present
  df <- df %>% filter(!is.na(value_eua) & !is.na(value_bla))
  if (nrow(df) == 0) return(NULL)
  
  df <- df %>%
    mutate(
      changed    = value_eua != value_bla,
      delta_days = as.integer(value_bla - value_eua)
    )
  
  changed_df <- df %>% filter(changed)
  n_changed  <- nrow(changed_df)
  n_total    <- nrow(df)
  
  # 1) Daily counts: EUA (original) vs BLA (revised)
  daily_eua <- changed_df %>% count(date = value_eua, name = "n") %>%
    mutate(which = "EUA date (original)")
  daily_bla <- changed_df %>% count(date = value_bla, name = "n") %>%
    mutate(which = "BLA date (revised)")
  daily <- bind_rows(daily_eua, daily_bla)
  
  p1 <- ggplot(daily, aes(x = date, y = n, linetype = which)) +
    geom_line() + geom_point(size = 0.9) +
    labs(title = paste0(var, " — subjects with changed date (daily)"),
         x = "Date", y = "Subjects (changed)") +
    theme_minimal(base_size = 12)
  file1 <- file.path(out_dir, paste0("adsl_", tolower(var), "_daily_changes.png"))
  ggsave(filename = file1, plot = p1, width = 9, height = 5.5, dpi = 150)
  
  # 2) Share changed per EUA date (numerator = changed, denominator = all with that EUA date)
  denom <- df %>% count(date = value_eua, name = "n_total")
  numer <- changed_df %>% count(date = value_eua, name = "n_changed")
  share <- denom %>% left_join(numer, by = "date") %>%
    mutate(n_changed = dplyr::coalesce(n_changed, 0L),
           pct = 100 * n_changed / n_total)
  
  p2 <- ggplot(share, aes(x = date, y = pct)) +
    geom_line() + geom_point(size = 0.8) +
    labs(title = paste0(var, " — % changed (by EUA date)"),
         x = "EUA date", y = "% of subjects changed") +
    theme_minimal(base_size = 12)
  file2 <- file.path(out_dir, paste0("adsl_", tolower(var), "_daily_share.png"))
  ggsave(filename = file2, plot = p2, width = 9, height = 5, dpi = 150)
  
  # 3) Distribution of day shifts (BLA − EUA)
  dist <- changed_df %>% count(delta_days)
  p3 <- ggplot(dist, aes(x = delta_days, y = n)) +
    geom_col() +
    labs(title = paste0("Shift in ", var, " (BLA − EUA)"),
         x = "Delta (days)", y = "Subjects") +
    theme_minimal(base_size = 12)
  file3 <- file.path(out_dir, paste0("adsl_", tolower(var), "_delta_hist.png"))
  ggsave(filename = file3, plot = p3, width = 9, height = 5, dpi = 150)
  
  # 4) EUA vs BLA scatter for changed rows
  p4 <- ggplot(changed_df, aes(x = value_eua, y = value_bla)) +
    geom_point(alpha = 0.2, size = 0.8) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    labs(title = paste0(var, " — EUA vs BLA (changed only)"),
         x = "EUA date", y = "BLA date") +
    theme_minimal(base_size = 12)
  file4 <- file.path(out_dir, paste0("adsl_", tolower(var), "_scatter.png"))
  ggsave(filename = file4, plot = p4, width = 6, height = 6, dpi = 150)
  
  list(
    var       = var,
    n_changed = n_changed,
    n_total   = n_total,
    files     = c(file1, file2, file3, file4)
  )
}

res_v01 <- make_date_change_plots("V01DT")
res_v02 <- make_date_change_plots("V02DT")

# Inject sections into the existing HTML report ---------------------------------
append_var_section <- function(res) {
  if (is.null(res)) return("")
  pct_str <- if (is.finite(res$n_total) && res$n_total > 0) {
    sprintf("%.1f", 100 * res$n_changed / res$n_total)
  } else {
    "0.0"
  }
  paste0(
    "<h2>", html_escape(res$var), " — Date changes</h2>",
    "<p class='muted'>Changed subjects: ",
    format(res$n_changed, big.mark = ","), " of ",
    format(res$n_total,   big.mark = ","), " (", pct_str, "%)</p>",
    "<figure><img src='", html_escape(basename(res$files[1])), "' alt='Daily changes'>",
    "<figcaption>Daily counts of subjects whose ", html_escape(res$var),
    " changed between EUA and BLA (shown by EUA vs BLA date).</figcaption></figure>",
    "<figure><img src='", html_escape(basename(res$files[2])), "' alt='% changed per EUA date'>",
    "<figcaption>Share of subjects changed among all subjects with that EUA date.</figcaption></figure>",
    "<figure><img src='", html_escape(basename(res$files[3])), "' alt='Day shift distribution'>",
    "<figcaption>Distribution of the day shift (BLA − EUA). Positive values mean the BLA date is later.</figcaption></figure>",
    "<figure><img src='", html_escape(basename(res$files[4])), "' alt='EUA vs BLA scatter (changed only)'>",
    "<figcaption>Scatter of EUA vs BLA dates for changed rows (dashed line is equality).</figcaption></figure>"
  )
}

sections <- paste0(
  if (!is.null(res_v01)) append_var_section(res_v01) else "",
  if (!is.null(res_v02)) append_var_section(res_v02) else ""
)

if (nzchar(sections)) {
  # If 'html' (the earlier string) is still in scope, augment it; otherwise read the file.
  if (!exists("html")) {
    html <- paste(readLines(report_html, warn = FALSE), collapse = "\n")
  }
  html <- sub("</body>\\s*</html>\\s*$", paste0(sections, "\n</body></html>"), html, perl = TRUE)
  writeLines(html, report_html, useBytes = TRUE)
  message("Injected V01DT/V02DT visuals into: ", report_html)
} else {
  message("V01DT/V02DT not found or no comparable dates; no visuals injected.")
}