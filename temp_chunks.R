knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE)

# Set CRAN mirror
options(repos = c(CRAN = "https://cloud.r-project.org/"))

# Install packages if not already installed
required_packages <- c("tidyverse", "sf", "lwgeom", "leaflet", "leaflet.extras",
                       "data.table", "zip", "httr", "lubridate", "mapview", "tigris", "kableExtra", "tidytransit")

new_packages <- required_packages[!required_packages %in% installed.packages()[,"Package"]]
if(length(new_packages)) install.packages(new_packages)

# Load required packages
library(tidyverse)
library(sf)
library(leaflet)
library(lwgeom)
library(leaflet.extras)
library(data.table)
library(zip)
library(httr)
library(lubridate)
library(mapview)
library(tigris) # For Illinois state boundary
library(kableExtra) # Added for enhanced table formatting
library(tidytransit) # For GTFS data processing

# Disable s2 processing to avoid geometry validation issues
sf_use_s2(FALSE)

# Load Illinois state boundary for clipping transit areas
illinois_boundary <- states(cb = TRUE, year = 2023) %>%
  filter(STUSPS == "IL") %>%
  st_transform(4326)  # Transform to WGS84

## Load Existing Hub Data and Corridor Data

# Load the existing hub processing results (from the original analysis)
# We'll reuse the hub identification logic from the original script

# Load GTFS utility functions from extracted modules
source("R/agency_metadata.R")  # Centralized agency configuration
source("R/tidytransit_integration.R")  # tidytransit wrapper functions
source("R/gtfs_download.R")
source("R/gtfs_normalize.R")
source("R/gtfs_validate.R")
source("R/gtfs_processing.R")
source("R/spatial_validate.R")
source("R/spatial_clustering.R")
source("R/frequency_calc.R")
source("R/hub_identification.R")
source("R/hub_processing.R")
source("R/corridor_processing.R")
source("R/buffer_processing.R")
source("R/map_creation.R")
source("R/summary_stats.R")

## Download and Process GTFS Data

# Get agency configurations from centralized metadata
# Now includes all 14 Illinois transit agencies (6 existing + 8 new)
agency_configs <- get_agency_configs_for_download()

# Process all GTFS data using tidytransit: download, validate, add unique IDs
agencies_data <- process_agencies_with_tidytransit(agency_configs, validate = TRUE)

# Combine data from all agencies into unified tables
combined_tables <- combine_agency_data(agencies_data)

# Extract combined tables for easy access
all_stops <- combined_tables$stops
all_routes <- combined_tables$routes
all_trips <- combined_tables$trips
all_stop_times <- combined_tables$stop_times
all_calendar <- combined_tables$calendar
all_calendar_dates <- combined_tables$calendar_dates
all_shapes <- combined_tables$shapes

# Enrich stop_times with route information (adds unique_route_id and route_type)
cat("Enriching stop_times with route information...\n")
all_stop_times <- enrich_stop_times(all_stop_times, all_trips, all_routes)

# Individual agency data for backward compatibility
# Access via agencies_data[[agency_id]]$data
cta_data <- agencies_data$cta$data
pace_data <- agencies_data$pace$data
metra_data <- agencies_data$metra$data
metro_stl_data <- agencies_data$metro_stl$data
cumtd_data <- agencies_data$cumtd$data
rmtd_data <- agencies_data$rmtd$data
# New agencies (8):
metrolink_quad_cities_data <- agencies_data$metrolink_quad_cities$data
citylink_data <- agencies_data$citylink$data
smtd_data <- agencies_data$smtd$data
dekalb_data <- agencies_data$dekalb$data
connect_transit_data <- agencies_data$connect_transit$data
dpts_data <- agencies_data$dpts$data
galesburg_data <- agencies_data$galesburg$data
gowest_data <- agencies_data$gowest$data

# Store validation results for reference
validation_results <- lapply(agencies_data, function(a) a$validation)

## Explore direction_id availability in GTFS data

# Check if direction_id exists in trips data
cat("=== Checking direction_id availability ===\n\n")

# Iterate over all agencies dynamically
all_agency_ids <- get_all_agency_ids()
for (agency_id in all_agency_ids) {
  trips_data <- agencies_data[[agency_id]]$data$trips
  agency_name <- get_agency_display_name(agency_id)

  if ("direction_id" %in% names(trips_data)) {
    cat(sprintf("%s: Has direction_id field\n", agency_name))
    cat(sprintf("  - Total trips: %d\n", nrow(trips_data)))
    cat(sprintf("  - Trips with direction_id: %d\n", sum(!is.na(trips_data$direction_id))))
    cat(sprintf("  - Unique direction values: %s\n",
                paste(sort(unique(trips_data$direction_id)), collapse = ", ")))
  } else {
    cat(sprintf("%s: NO direction_id field\n", agency_name))
  }
  cat("\n")
}

# If direction_id exists in any agency, ensure it exists in all and rebuild all_trips
has_direction_id <- any(sapply(all_agency_ids, function(agency_id) {
  "direction_id" %in% names(agencies_data[[agency_id]]$data$trips)
}))

if (has_direction_id) {
  # Ensure direction_id exists in all agency datasets (add NA if missing)
  for (agency_id in all_agency_ids) {
    if (!"direction_id" %in% names(agencies_data[[agency_id]]$data$trips)) {
      # Add direction_id column using regular assignment (works with data.table)
      agencies_data[[agency_id]]$data$trips$direction_id <- NA_integer_
    }
  }

  # Rebuild all_trips with direction_id from all agencies dynamically
  all_trips_list <- lapply(all_agency_ids, function(agency_id) {
    agencies_data[[agency_id]]$data$trips
  })
  all_trips <- rbindlist(all_trips_list, fill = TRUE)

  cat("direction_id added to all_trips\n")
  cat(sprintf("Trips with direction_id: %d / %d (%.1f%%)\n",
              sum(!is.na(all_trips$direction_id)),
              nrow(all_trips),
              100 * sum(!is.na(all_trips$direction_id)) / nrow(all_trips)))
}

## Identify Public Transportation Hubs

# Identify all transit hubs (rail stations + qualifying bus hubs)
hub_results <- identify_all_hubs(
  all_stops, all_routes, all_trips,
  all_stop_times, all_calendar, all_calendar_dates
)

# Extract results
all_hubs_sf <- hub_results$all_hubs_sf
am_peak_bus_stops <- hub_results$am_peak_bus_stops
pm_peak_bus_stops <- hub_results$pm_peak_bus_stops

# Filter hubs to only those within Illinois boundary
# This excludes St. Louis metro stations in Missouri
all_hubs_sf <- st_intersection(all_hubs_sf, illinois_boundary)
cat(sprintf("Hubs after filtering to Illinois boundary: %d\n", nrow(all_hubs_sf)))

## Identify Corridors and Create Buffers

# Identify qualifying transit corridors (direction-aware combined frequency)
corridor_results <- identify_qualifying_corridors(
  all_stops, am_peak_bus_stops, pm_peak_bus_stops,
  all_trips, all_shapes
)

qualifying_corridor_segments_sf <- corridor_results$qualifying_corridor_segments
corridor_qualification_summary <- corridor_results$qualification_summary

# Create corridor buffers (1/8 mile along actual route geometry from GTFS shapes)
all_corridors_union_wgs84 <- create_corridor_buffers(
  qualifying_corridor_segments_sf,
  illinois_boundary
)

# Create hub buffers (1/2 mile around hubs)
hub_buffers <- create_hub_buffers(all_hubs_sf, illinois_boundary)

# Extract hub buffer components for area calculations
all_hub_areas_wgs84 <- hub_buffers$all_hub_areas
half_mile_buffers_wgs84 <- hub_buffers$half_mile_buffers

# Legacy hub unions (backward compatibility)
cta_hubs_union <- hub_buffers$cta_hubs_union
pace_hubs_union <- hub_buffers$pace_hubs_union
metra_hubs_union <- hub_buffers$metra_hubs_union
metro_stl_hubs_union <- hub_buffers$metro_stl_hubs_union
cumtd_hubs_union <- hub_buffers$cumtd_hubs_union
rmtd_hubs_union <- hub_buffers$rmtd_hubs_union

# New agencies available via hub_buffers$per_agency_union[[agency_id]]

# Combine all affected areas (hubs + corridors)
all_affected_areas_combined <- create_combined_buffers(
  all_hub_areas_wgs84, all_corridors_union_wgs84, illinois_boundary
)

## Calculate Areas

# Calculate areas in square miles
hub_area_sqft <- st_area(all_hub_areas_wgs84)
hub_area_sqmi <- units::set_units(hub_area_sqft, "mi^2")

corridor_area_sqft <- st_area(all_corridors_union_wgs84)
corridor_area_sqmi <- units::set_units(corridor_area_sqft, "mi^2")

combined_area_sqft <- st_area(all_affected_areas_combined)
combined_area_sqmi <- units::set_units(combined_area_sqft, "mi^2")

# Total area covered by analysis
# Chicago MSA (6 IL counties): Cook: 953.6, DuPage: 336.5, Kane: 524.2, Lake: 470, McHenry: 611, Will: 849.2
# St. Louis MSA (3 IL counties): St. Clair: 674.6, Madison: 741.5, Monroe: 398.2
# Champaign-Urbana: Champaign County: 1,008
# Rockford: Winnebago County: 519
chicago_il_msa_area_sqmi <- 953.6 + 336.5 + 524.2 + 470 + 611 + 849.2
stlouis_il_msa_area_sqmi <- 674.6 + 741.5 + 398.2
champaign_area_sqmi <- 1008
rockford_area_sqmi <- 519
total_il_area_sqmi <- chicago_il_msa_area_sqmi + stlouis_il_msa_area_sqmi + champaign_area_sqmi + rockford_area_sqmi

# Calculate percentages (against total IL area covered)
pct_hubs <- as.numeric(hub_area_sqmi) / total_il_area_sqmi * 100
pct_corridors <- as.numeric(corridor_area_sqmi) / total_il_area_sqmi * 100
pct_combined <- as.numeric(combined_area_sqmi) / total_il_area_sqmi * 100

# Count hubs and routes
hub_counts <- table(all_hubs_sf$agency_name)
qualifying_corridor_segment_count <- nrow(qualifying_corridor_segments_sf)

# Create the interactive map (now with dynamic agency handling)
invisible(capture.output(
  map <- create_interactive_map(
    all_hubs_sf,
    all_affected_areas_combined,
    hub_buffers,  # Now accepts full buffer object
    all_corridors_union_wgs84,
    illinois_boundary,  # Add Illinois state outline
    center_lng = -87.6079,
    center_lat = 41.8917,
    zoom = 9
  )
))

# Display the map
map

# Generate all summary statistics
stats <- generate_summary_statistics(all_hubs_sf, qualifying_corridor_segments_sf)

# Extract summary variables for use in inline R code
bus_hub_summary <- stats$bus_hub_summary
corridor_summary <- stats$corridor_summary
rail_hub_count <- stats$rail_hub_count
total_hubs <- stats$total_hubs
cta_hub <- stats$cta_hub
pace_hub <- stats$pace_hub
metro_stl_hub <- stats$metro_stl_hub
cumtd_hub <- stats$cumtd_hub
rmtd_hub <- stats$rmtd_hub
cta_corridor <- stats$cta_corridor
pace_corridor <- stats$pace_corridor
metro_stl_corridor <- stats$metro_stl_corridor
cumtd_corridor <- stats$cumtd_corridor
rmtd_corridor <- stats$rmtd_corridor
rail_hub_counts <- stats$rail_hub_counts
qualifying_bus_hubs <- stats$qualifying_bus_hubs
rail_stops <- stats$rail_stops
qualifying_corridor_segments <- stats$qualifying_corridor_segments

# Calculate per-agency relief areas
agency_metadata <- get_agency_metadata()
agency_ids <- get_all_agency_ids()

# Build summary table
agency_summary <- data.frame(
  Agency = character(),
  `Bus Hubs` = integer(),
  `Rail Hubs` = integer(),
  `Bus Stops in Hubs` = integer(),
  `Relief Area (sq mi)` = numeric(),
  stringsAsFactors = FALSE,
  check.names = FALSE
)

for (agency_id in agency_ids) {
  # Get display name
  agency_name <- get_agency_display_name(agency_id)

  # Bus hubs count
  bus_hubs <- if (agency_id %in% bus_hub_summary$agency) {
    bus_hub_summary[bus_hub_summary$agency == agency_id, ]$unique_clusters
  } else {
    0
  }

  # Rail hubs count
  rail_hubs <- if (agency_id %in% names(rail_hub_counts)) {
    as.integer(rail_hub_counts[agency_id])
  } else {
    0
  }

  # Bus stops in hubs
  bus_stops <- if (agency_id %in% bus_hub_summary$agency) {
    bus_hub_summary[bus_hub_summary$agency == agency_id, ]$total_stops
  } else {
    0
  }

  # Calculate relief area for this agency
  if (agency_id %in% names(hub_buffers$per_agency_union)) {
    agency_buffer <- hub_buffers$per_agency_union[[agency_id]]
    area_sqmi <- as.numeric(units::set_units(st_area(agency_buffer), "mi^2"))
  } else {
    area_sqmi <- 0
  }

  # Add row to summary table
  agency_summary <- rbind(agency_summary, data.frame(
    Agency = agency_name,
    `Bus Hubs` = bus_hubs,
    `Rail Hubs` = rail_hubs,
    `Bus Stops in Hubs` = bus_stops,
    `Relief Area (sq mi)` = area_sqmi,
    stringsAsFactors = FALSE,
    check.names = FALSE
  ))
}

# Add totals row
agency_summary <- rbind(agency_summary, data.frame(
  Agency = "**TOTAL**",
  `Bus Hubs` = sum(agency_summary$`Bus Hubs`),
  `Rail Hubs` = sum(agency_summary$`Rail Hubs`),
  `Bus Stops in Hubs` = sum(agency_summary$`Bus Stops in Hubs`),
  `Relief Area (sq mi)` = sum(agency_summary$`Relief Area (sq mi)`),
  stringsAsFactors = FALSE,
  check.names = FALSE
))

# Format and display table
knitr::kable(
  agency_summary,
  format = "html",
  align = c("l", "r", "r", "r", "r"),
  digits = 2,
  format.args = list(big.mark = ","),
  caption = "Transit Agency Summary: People Over Parking Act Impact"
) %>%
  kableExtra::kable_styling(
    bootstrap_options = c("striped", "hover", "condensed"),
    full_width = FALSE,
    position = "left"
  ) %>%
  kableExtra::row_spec(nrow(agency_summary), bold = TRUE, background = "#f0f0f0")
