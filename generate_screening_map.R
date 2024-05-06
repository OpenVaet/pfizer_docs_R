library(readr)
library(jsonlite)
library(dplyr)

# Loads sites data
sites_data <- read_csv('subjects_screened_by_sites.csv')

# Checks for missing columns in any rows
if (any(colSums(is.na(sites_data)) > 0)) {
  stop("Data is missing in some rows - make sure subjects_by_arms_and_sites.R has been executed first.")
}

# Populates the list with site data
sites_data <- sites_data %>%
  select(ORISITEID, Country, Site_Name, Latitude, Longitude, Investigator, Subjects_Screened)

# Loads the map template
html_template <- readLines('screening_map_template.html', warn = FALSE)
html_template <- paste(html_template, collapse = "\n")

# Generates data-points
data_points <- ""

for (i in 1:nrow(sites_data)) {
  country <- sites_data$Country[i]
  site_name <- sites_data$Site_Name[i]
  latitude <- sites_data$Latitude[i]
  longitude <- sites_data$Longitude[i]
  investigator <- sites_data$Investigator[i]
  subjects_screened <- sites_data$Subjects_Screened[i]
  
  # Calculates the total area for the subjects screened
  total_area <- 100000000 * subjects_screened # 100.000.000 square meters per subject
  
  # Calculates the radius based on the area
  radius_size <- as.integer(sqrt(total_area / pi)) # sqrt(Area / pi)
  
  site_name_print <- gsub("'", "\\\\'", site_name, fixed = TRUE)
  
  data_points <- paste0(data_points, sprintf("var circle%s = L.circle([%s, %s], {
    color: '#3ebfed',
    fillColor: '#3ebfed',
    fillOpacity: 0.9,
    radius: %s,
    site_name: '%s',
    investigator: '%s',
    subjects_screened: '%s',
    trial_site_id: %s
}).addTo(map).on('mouseover', onClick);\n", i, latitude, longitude, radius_size, site_name_print, investigator, subjects_screened, i))
  
  if (subjects_screened > 1500) {
    data_points <- paste0(data_points, sprintf("
  var label%s = L.marker([%s, %s], {
  icon: L.divIcon({
    className: 'label',
    html: '<div style=\"font-size: 16px; font-weight: bold; color: black; text-align: center;\">%s</div>',
    iconSize: [100, 40]
  })
}).addTo(map);", i, latitude, longitude, subjects_screened))
  } else if (subjects_screened > 650) {
    data_points <- paste0(data_points, sprintf("
  var label%s = L.marker([%s, %s], {
  icon: L.divIcon({
    className: 'label',
    html: '<div style=\"font-size: 14px; font-weight: bold; color: black; text-align: center;\">%s</div>',
    iconSize: [100, 40]
  })
}).addTo(map);", i, latitude, longitude, subjects_screened))
  }
}

html_template <- gsub("\\[---DATAPOINTS---\\]", data_points, html_template)

# Writes the modified HTML to a new file
writeLines(html_template, 'map_of_subjects_screened.html')

