library(tools)
library(zip)

folder_path <- "eua_data"
if (!dir.exists(folder_path)) {
  dir.create(folder_path)
}

drops_path <- "eua_data/drops_data"
if (!dir.exists(drops_path)) {
  dir.create(drops_path)
}
xpt_path <- "eua_data/xpt_data"
if (!dir.exists(xpt_path)) {
  dir.create(xpt_path)
}
pdf_path <- "eua_data/pdf_data"
if (!dir.exists(pdf_path)) {
  dir.create(pdf_path)
}

zip_path <- 'eua_zip'
zip_files <- list.files(path = zip_path, pattern = "*.zip", full.names = TRUE)

for (zip_file in zip_files) {
  print(paste("zip : [", zip_file, "]"))
  
  if (!file.exists(zip_file)) {
    print(paste('Not an archive : ', zip_file))
    quit()
  }
  
  files_in_zip <- utils::unzip(zip_file, list = TRUE)$Name
  file_name <- files_in_zip[1]
  
  # Extracts the archive name and creates a new folder within drops_path
  archive_name <- tools::file_path_sans_ext(basename(zip_file))
  archive_folder <- file.path(drops_path, archive_name)
  if (!dir.exists(archive_folder)) {
    dir.create(archive_folder)
  }
  
  drop_file <- file.path(archive_folder, file_name)
  
  if (!file.exists(drop_file)) {
    print(paste("Extracting [", drop_file, "]"))
    utils::unzip(zip_file, exdir = archive_folder)
  }
}

# Gets all sub-folders in drops_path
sub_folders <- dir(drops_path, recursive = FALSE)

for (sub_folder in sub_folders) {
  # Gets all files in the sub-folder
  files <- dir(file.path(drops_path, sub_folder), full.names = TRUE)
  
  # Iterates through each file
  for (file in files) {
    # Checks if the file has an extension in ".xpt"
    if (tools::file_ext(file) == "xpt") {
      # Gets the destination file path
      dest_file <- file.path(xpt_path, basename(file))
      
      # Checks if the file doesn't exist in xpt_path
      if (!file.exists(dest_file)) {
        # Copies the file to xpt_path
        file.copy(file, dest_file)
      }
    }
    # Checks if the file has an extension in ".pdf"
    if (tools::file_ext(file) == "pdf") {
      # Gets the destination file path
      dest_file <- file.path(pdf_path, basename(file))
      
      # Checks if the file doesn't exist in pdf_path
      if (!file.exists(dest_file)) {
        # Copies the file to pdf_path
        file.copy(file, dest_file)
      }
    }
  }
}


# Ensure analysis dir exists
analysis_dir <- "analysis"
if (!dir.exists(analysis_dir)) dir.create(analysis_dir, recursive = TRUE)
report_file <- file.path(analysis_dir, "eua_file_types.html")

# Helper: human-readable sizes
human_readable <- function(bytes) {
  units <- c("B","KB","MB","GB","TB")
  pow <- ifelse(bytes > 0, floor(log(bytes, 1024)), 0)
  pow[!is.finite(pow)] <- 0
  pow <- pmax(0, pmin(pow, length(units) - 1))
  res <- bytes / (1024^pow)
  paste0(format(round(res, 2), big.mark = ",", nsmall = 2, trim = TRUE), " ", units[pow + 1])
}
# Helper: simple HTML escape
html_escape <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x
}

# Collect files recursively
all_paths <- list.files(drops_path, recursive = TRUE, full.names = TRUE, include.dirs = FALSE)

# Collect files recursively (files only)
files <- list.files(drops_path, recursive = TRUE, full.names = TRUE, include.dirs = FALSE)
fi <- file.info(files)

# If no files, write a minimal report and exit
if (length(files) == 0) {
  html <- paste0(
    "<!doctype html><html lang='en'><head><meta charset='utf-8'>",
    "<title>EUA dump — File types</title>",
    "<style>",
    "body{font:16px/1.5 'Georgia',serif;max-width:900px;margin:40px auto;color:#222}",
    ".muted{color:#666}",
    "</style></head><body>",
    "<h1>EUA Dump — File Type Summary</h1>",
    "<p class='muted'>Generated on ", html_escape(format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")), "</p>",
    "<p>No files found under <code>", html_escape(drops_path), "</code>.</p>",
    "</body></html>"
  )
  writeLines(html, report_file, useBytes = TRUE)
  message("Wrote empty report to: ", report_file)
} else {
  # Derive extensions
  ext <- tools::file_ext(files)
  ext <- tolower(ext)
  ext[ext == ""] <- "(no ext)"
  
  # Stats per extension
  sizes <- fi$size
  split_sizes <- split(sizes, ext)
  stats <- lapply(split_sizes, function(x) c(
    n = length(x),
    total_bytes = sum(x, na.rm = TRUE),
    min_bytes = min(x, na.rm = TRUE),
    median_bytes = median(x, na.rm = TRUE),
    mean_bytes = mean(x, na.rm = TRUE),
    max_bytes = max(x, na.rm = TRUE)
  ))
  tab <- do.call(rbind, stats)
  tab <- data.frame(extension = rownames(tab), tab, row.names = NULL, check.names = FALSE)
  # Order by total size desc, then count desc
  tab <- tab[order(-tab$total_bytes, -tab$n, tab$extension), ]
  total_files <- nrow(fi)
  total_bytes <- sum(fi$size, na.rm = TRUE)
  tab$share <- tab$total_bytes / total_bytes
  
  # Format columns
  tab_fmt <- transform(
    tab,
    n = format(n, big.mark = ","),
    total = human_readable(total_bytes),
    min = human_readable(min_bytes),
    median = human_readable(median_bytes),
    mean = human_readable(mean_bytes),
    max = human_readable(max_bytes),
    share = sprintf("%.1f%%", share * 100)
  )
  tab_fmt <- tab_fmt[, c("extension","n","total","share","min","median","mean","max")]
  
  # Key types quick readout
  get_count <- function(ext_name) {
    i <- match(tolower(ext_name), tab$extension)
    if (is.na(i)) 0L else as.integer(tab$n[i])
  }
  n_pdf  <- get_count("pdf")
  n_docx <- get_count("docx")
  n_xpt  <- get_count("xpt")
  
  # Top 10 largest files
  ord <- order(fi$size, decreasing = TRUE)
  top_n <- min(10L, length(ord))
  top_idx <- ord[seq_len(top_n)]
  top_paths  <- files[top_idx]
  top_ext    <- ext[top_idx]
  top_sizes  <- fi$size[top_idx]
  top_mtime  <- fi$mtime[top_idx]
  
  # Build HTML rows
  rows <- paste0(
    "<tr>",
    "<td>", html_escape(tab_fmt$extension), "</td>",
    "<td class='num'>", tab_fmt$n, "</td>",
    "<td class='num'>", tab_fmt$total, "</td>",
    "<td class='num'>", tab_fmt$share, "</td>",
    "<td class='num'>", tab_fmt$min, "</td>",
    "<td class='num'>", tab_fmt$median, "</td>",
    "<td class='num'>", tab_fmt$mean, "</td>",
    "<td class='num'>", tab_fmt$max, "</td>",
    "</tr>",
    collapse = "\n"
  )
  
  top_rows <- paste0(
    "<tr>",
    "<td>", html_escape(top_paths), "</td>",
    "<td>", html_escape(top_ext), "</td>",
    "<td class='num'>", human_readable(top_sizes), "</td>",
    "<td>", html_escape(format(top_mtime, "%Y-%m-%d %H:%M:%S")), "</td>",
    "</tr>",
    collapse = "\n"
  )
  
  generated_on <- format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")
  
  html <- paste0(
    "<!doctype html><html lang='en'><head><meta charset='utf-8'>",
    "<meta name='viewport' content='width=device-width, initial-scale=1'>",
    "<title>EUA dump — File types</title>",
    "<style>",
    "body{font:16px/1.6 'Georgia',serif;max-width:1000px;margin:48px auto;padding:0 20px;color:#222}",
    "h1,h2{font-weight:700;margin:0 0 12px}",
    "h1{font-size:30px} h2{font-size:22px;margin-top:28px}",
    "p{margin:8px 0 14px}",
    ".muted{color:#666}",
    "table{border-collapse:collapse;width:100%;margin:10px 0 26px}",
    "th,td{padding:10px 12px;border-bottom:1px solid #ddd;vertical-align:top}",
    "thead th{font-variant:small-caps;letter-spacing:.04em;text-align:left}",
    "tbody tr:nth-child(odd){background:#fafafa}",
    "td.num, th.num{text-align:right}",
    "code{background:#f6f6f6;padding:2px 4px;border-radius:4px}",
    ".summary{display:grid;grid-template-columns:repeat(auto-fit,minmax(220px,1fr));gap:12px;margin:12px 0 4px}",
    ".card{border:1px solid #e6e6e6;border-radius:10px;padding:12px 14px;background:#fff;box-shadow:0 1px 2px rgba(0,0,0,.03)}",
    ".card h3{margin:0 0 6px;font-size:16px}",
    ".metric{font-weight:700}",
    "</style></head><body>",
    "<h1>EUA Dump — File Type Summary</h1>",
    "<p class='muted'>Location: <code>", html_escape(normalizePath(drops_path, winslash = "/", mustWork = FALSE)),
    "</code><br>Generated on ", html_escape(generated_on), "</p>",
    
    "<div class='summary'>",
    "<div class='card'><h3>Total files</h3><div class='metric'>", format(total_files, big.mark = ","), "</div></div>",
    "<div class='card'><h3>Total size</h3><div class='metric'>", human_readable(total_bytes), "</div></div>",
    "<div class='card'><h3>PDF files</h3><div class='metric'>", format(n_pdf, big.mark = ","), "</div></div>",
    "<div class='card'><h3>DOCX files</h3><div class='metric'>", format(n_docx, big.mark = ","), "</div></div>",
    "<div class='card'><h3>XPT files</h3><div class='metric'>", format(n_xpt, big.mark = ","), "</div></div>",
    "</div>",
    
    "<h2>By File Type (extension)</h2>",
    "<table>",
    "<thead><tr>",
    "<th>Extension</th>",
    "<th class='num'>Count</th>",
    "<th class='num'>Total size</th>",
    "<th class='num'>Share</th>",
    "<th class='num'>Min</th>",
    "<th class='num'>Median</th>",
    "<th class='num'>Mean</th>",
    "<th class='num'>Max</th>",
    "</tr></thead>",
    "<tbody>", rows, "</tbody>",
    "</table>",
    
    "<h2>Top 10 Largest Files</h2>",
    "<table>",
    "<thead><tr>",
    "<th>Path</th><th>Extension</th><th class='num'>Size</th><th>Modified</th>",
    "</tr></thead>",
    "<tbody>", top_rows, "</tbody>",
    "</table>",
    
    "</body></html>"
  )
  
  writeLines(html, report_file, useBytes = TRUE)
  message("Wrote report to: ", report_file)
}
