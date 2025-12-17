# =============================================================================
# SMTD Corridor Identification Test Script
# =============================================================================
#
# Purpose: Test and visualize corridor identification using SMTD (Springfield
# Mass Transit District) as a test case. SMTD has a simpler network than Chicago,
# making it easier to verify corridor identification logic.
#
# This script:
#   1. Loads SMTD GTFS data
#   2. Calculates route-level frequency (for reference)
#   3. Runs edge-based corridor identification method
#   4. Creates corridor buffers
#   5. Generates interactive diagnostic map
#
# Output: Interactive Leaflet map showing qualifying corridor segments
#
# =============================================================================

# Load required packages
library(tidyverse)
library(sf)
library(leaflet)
library(data.table)
library(mapview)

# Set options
sf_use_s2(FALSE)
options(repos = c(CRAN = "https://cloud.r-project.org/"))

# Source project modules
source("R/agency_metadata.R")
source("R/tidytransit_integration.R")
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
source("R/corridor_processing_v2.R")
source("R/buffer_processing.R")
source("R/map_creation.R")

cat("\n")
cat("=============================================================================\n")
cat("  SMTD CORRIDOR IDENTIFICATION TEST\n")
cat("=============================================================================\n\n")

# =============================================================================
# STEP 1: Download and Process SMTD GTFS Data Only
# =============================================================================

cat("STEP 1: Loading SMTD GTFS Data\n")
cat("-------------------------------\n\n")

# Get only SMTD configuration
smtd_config <- list(smtd = get_agency_metadata()$smtd)

# Process SMTD data
smtd_agencies <- process_agencies_with_tidytransit(smtd_config, validate = TRUE)
smtd_combined <- combine_agency_data(smtd_agencies)

# Extract data tables
smtd_stops <- smtd_combined$stops
smtd_routes <- smtd_combined$routes
smtd_trips <- smtd_combined$trips
smtd_stop_times <- smtd_combined$stop_times
smtd_calendar <- smtd_combined$calendar
smtd_calendar_dates <- smtd_combined$calendar_dates
smtd_shapes <- smtd_combined$shapes

# Enrich stop_times
smtd_stop_times <- enrich_stop_times(smtd_stop_times, smtd_trips, smtd_routes)

cat("\nSMTD Data Summary:\n")
cat(sprintf("  Stops: %d\n", nrow(smtd_stops)))
cat(sprintf("  Routes: %d\n", nrow(smtd_routes)))
cat(sprintf("  Trips: %d\n", nrow(smtd_trips)))
cat(sprintf("  Stop times: %d\n", nrow(smtd_stop_times)))
cat(sprintf("  Shapes: %d unique\n", length(unique(smtd_shapes$unique_shape_id))))

# =============================================================================
# STEP 2: Calculate Route-Level Frequency (Diagnostic)
# =============================================================================

cat("\n\nSTEP 2: Calculating Route-Level Frequency\n")
cat("-----------------------------------------\n\n")

# Get representative weekday service
bus_routes <- smtd_routes[route_type == 3]
bus_trips <- smtd_trips[unique_route_id %in% bus_routes$unique_route_id]

rep_services <- get_representative_services_by_agency(
  smtd_calendar, smtd_calendar_dates, bus_trips
)
cat("\nRepresentative service selected:\n")
print(rep_services)

weekday_trips <- filter_trips_to_representative_service(bus_trips, rep_services)

# Filter stop_times to weekday service
weekday_stop_times <- smtd_stop_times[unique_trip_id %in% weekday_trips$unique_trip_id]

# Parse arrival times
weekday_stop_times[, arrival_time_hhmmss := substr(arrival_time, 1, 8)]
weekday_stop_times[, arrival_hour := as.integer(substr(arrival_time, 1, 2))]
weekday_stop_times <- weekday_stop_times[arrival_hour < 24]
weekday_stop_times[, arrival_time_obj := as.ITime(arrival_time_hhmmss, format = "%H:%M:%S")]

# Define peak periods
am_start <- as.ITime("07:00:00")
am_end <- as.ITime("09:00:00")
pm_start <- as.ITime("16:00:00")
pm_end <- as.ITime("18:00:00")

# Filter to AM and PM peak
am_stop_times <- weekday_stop_times[arrival_time_obj >= am_start & arrival_time_obj <= am_end]
pm_stop_times <- weekday_stop_times[arrival_time_obj >= pm_start & arrival_time_obj <= pm_end]

cat(sprintf("\nAM peak stop events: %d\n", nrow(am_stop_times)))
cat(sprintf("PM peak stop events: %d\n", nrow(pm_stop_times)))

# Join with trips to get direction_id
am_stop_times <- merge(am_stop_times,
                       weekday_trips[, .(unique_trip_id, direction_id, unique_shape_id)],
                       by = "unique_trip_id", all.x = TRUE)
pm_stop_times <- merge(pm_stop_times,
                       weekday_trips[, .(unique_trip_id, direction_id, unique_shape_id)],
                       by = "unique_trip_id", all.x = TRUE)

# Calculate ROUTE-LEVEL frequency (not edge-level)
# This tells us what SHOULD qualify at the route level
cat("\n--- Route-Level Frequency Analysis ---\n\n")

# AM peak by route/direction
am_route_freq <- am_stop_times[, .(
  trips_am = uniqueN(unique_trip_id)
), by = .(unique_route_id, direction_id)]
am_route_freq[, interval_am := 120 / trips_am]

# PM peak by route/direction
pm_route_freq <- pm_stop_times[, .(
  trips_pm = uniqueN(unique_trip_id)
), by = .(unique_route_id, direction_id)]
pm_route_freq[, interval_pm := 120 / trips_pm]

# Combine
route_freq <- merge(am_route_freq, pm_route_freq,
                    by = c("unique_route_id", "direction_id"), all = TRUE)
route_freq[is.na(trips_am), trips_am := 0]
route_freq[is.na(trips_pm), trips_pm := 0]
route_freq[is.na(interval_am), interval_am := Inf]
route_freq[is.na(interval_pm), interval_pm := Inf]

# Add route names
route_freq <- merge(route_freq,
                    smtd_routes[, .(unique_route_id, route_short_name, route_long_name)],
                    by = "unique_route_id", all.x = TRUE)

# Determine qualification
route_freq[, qualifies := interval_am <= 15 | interval_pm <= 15]
route_freq[, best_interval := pmin(interval_am, interval_pm)]

# Sort by frequency
setorder(route_freq, best_interval)

cat("Route-level frequency (sorted by best interval):\n")
print(route_freq[, .(route_short_name, direction_id, trips_am, trips_pm,
                     interval_am = round(interval_am, 1),
                     interval_pm = round(interval_pm, 1),
                     best_interval = round(best_interval, 1),
                     qualifies)])

qualifying_routes <- route_freq[qualifies == TRUE, unique(unique_route_id)]
cat(sprintf("\n%d route-directions QUALIFY at route level (≤15 min in AM or PM)\n",
            nrow(route_freq[qualifies == TRUE])))
cat(sprintf("%d unique routes with qualifying service\n", length(qualifying_routes)))

# =============================================================================
# STEP 3: Run Current Edge-Based Method (for comparison)
# =============================================================================

cat("\n\nSTEP 3: Running Current Edge-Based Method\n")
cat("------------------------------------------\n\n")

# Run hub identification first (to get peak stop times)
hub_results <- identify_all_hubs(
  smtd_stops, smtd_routes, smtd_trips,
  smtd_stop_times, smtd_calendar, smtd_calendar_dates
)

am_peak_bus_stops <- hub_results$am_peak_bus_stops
pm_peak_bus_stops <- hub_results$pm_peak_bus_stops

# Run current corridor identification
corridor_results <- identify_qualifying_corridors(
  smtd_stops, am_peak_bus_stops, pm_peak_bus_stops,
  smtd_trips, smtd_shapes
)

current_method_segments <- corridor_results$qualifying_corridor_segments
current_method_summary <- corridor_results$qualification_summary

cat(sprintf("\nCurrent edge-based method found: %d qualifying segments\n",
            nrow(current_method_segments)))

if (nrow(current_method_summary) > 0) {
  cat("\nEdge summary statistics:\n")
  cat(sprintf("  Total edges analyzed: %d\n", nrow(current_method_summary)))
  cat(sprintf("  Qualifying edges: %d\n", sum(current_method_summary$qualifies)))
  cat(sprintf("  Non-qualifying edges: %d\n", sum(!current_method_summary$qualifies)))

  # Show some examples of edges that didn't qualify
  non_qualifying <- current_method_summary[qualifies == FALSE]
  if (nrow(non_qualifying) > 0) {
    cat("\nSample of non-qualifying edges (sorted by best interval):\n")
    non_qualifying[, best_interval := pmin(interval_am, interval_pm, na.rm = TRUE)]
    setorder(non_qualifying, best_interval)
    print(head(non_qualifying[, .(from_cluster, to_cluster, direction_id,
                                  trips_am, trips_pm,
                                  interval_am = round(interval_am, 1),
                                  interval_pm = round(interval_pm, 1),
                                  best_interval = round(best_interval, 1))], 20))
  }
}

# Convert shapes to linestrings (needed for map display)
shapes_sf <- convert_shapes_to_linestrings(smtd_shapes)

# =============================================================================
# STEP 4: Create Current Method Buffers
# =============================================================================

cat("\n\nSTEP 4: Creating Current Method Buffers\n")
cat("----------------------------------------\n\n")

if (nrow(current_method_segments) > 0) {
  current_segments_projected <- st_transform(current_method_segments, 3435)
  current_buffers <- st_buffer(current_segments_projected, 680)
  current_buffer_union <- st_union(current_buffers)
  current_buffer_union_wgs84 <- st_sf(geometry = st_transform(current_buffer_union, 4326))
  cat("Current method buffers created\n")
} else {
  current_buffer_union_wgs84 <- st_sf(geometry = st_sfc(crs = 4326))
  cat("WARNING: No segments from current method to buffer!\n")
}

# =============================================================================
# STEP 5: Calculate Buffer Area
# =============================================================================

cat("\n\nSTEP 5: Buffer Area Calculation\n")
cat("--------------------------------\n\n")

# Calculate area in square miles
calc_area_sqmi <- function(geom) {
  # Handle NULL or empty
  if (is.null(geom) || length(geom) == 0) {
    return(0)
  }

  # Handle sf objects
  if (inherits(geom, "sf")) {
    if (nrow(geom) == 0) return(0)
  }

  # Handle sfc objects (just geometry)
  if (inherits(geom, "sfc")) {
    if (length(geom) == 0) return(0)
  }

  # If neither sf nor sfc, return 0
  if (!inherits(geom, "sf") && !inherits(geom, "sfc")) {
    return(0)
  }

  tryCatch({
    area_sqft <- st_area(geom)
    as.numeric(units::set_units(area_sqft, "mi^2"))
  }, error = function(e) 0)
}

current_area <- calc_area_sqmi(current_buffer_union_wgs84)

cat(sprintf("Current edge-based method corridor area: %.2f sq mi\n", current_area))

# =============================================================================
# STEP 6: Create Diagnostic Map
# =============================================================================

cat("\n\nSTEP 6: Creating Diagnostic Map\n")
cat("--------------------------------\n\n")

# Get Springfield center for map
springfield_center <- c(-89.6437, 39.7817)

# Create base map
comparison_map <- leaflet() %>%
  addTiles(group = "Street Map") %>%
  addProviderTiles(providers$CartoDB.Positron, group = "Light") %>%
  addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
  setView(lng = springfield_center[1], lat = springfield_center[2], zoom = 12)

# Add all shapes (for reference)
if (nrow(shapes_sf) > 0) {
  comparison_map <- comparison_map %>%
    addPolylines(
      data = shapes_sf,
      color = "gray",
      weight = 2,
      opacity = 0.5,
      group = "All Bus Routes",
      popup = ~paste0("Shape: ", unique_shape_id)
    )
}

# Add current method segments
if (nrow(current_method_segments) > 0) {
  comparison_map <- comparison_map %>%
    addPolylines(
      data = current_method_segments,
      color = "red",
      weight = 3,
      opacity = 0.9,
      group = "Current Method Segments",
      popup = ~paste0(
        "Trips AM: ", trips_am, "<br>",
        "Trips PM: ", trips_pm, "<br>",
        "Interval AM: ", round(interval_am, 1), " min<br>",
        "Interval PM: ", round(interval_pm, 1), " min"
      )
    )
}

# Add corridor buffer (red, transparent)
if (inherits(current_buffer_union_wgs84, "sf") && nrow(current_buffer_union_wgs84) > 0 && !st_is_empty(current_buffer_union_wgs84)) {
  comparison_map <- comparison_map %>%
    addPolygons(
      data = current_buffer_union_wgs84,
      fillColor = "red",
      fillOpacity = 0.2,
      stroke = TRUE,
      color = "red",
      weight = 1,
      group = "Corridor Buffer"
    )
}

# Add bus stops
if (nrow(smtd_stops) > 0) {
  # Convert stops to sf if needed
  stops_sf <- if (inherits(smtd_stops, "sf")) {
    smtd_stops
  } else {
    st_as_sf(smtd_stops, coords = c("stop_lon", "stop_lat"), crs = 4326)
  }

  comparison_map <- comparison_map %>%
    addCircleMarkers(
      data = stops_sf,
      radius = 4,
      color = "blue",
      fillColor = "white",
      fillOpacity = 0.8,
      weight = 1,
      group = "Bus Stops",
      popup = ~paste0("<b>", stop_name, "</b><br>Stop ID: ", stop_id)
    )
}

# Add layer controls
comparison_map <- comparison_map %>%
  addLayersControl(
    baseGroups = c("Street Map", "Light", "Satellite"),
    overlayGroups = c(
      "All Bus Routes",
      "Current Method Segments",
      "Corridor Buffer",
      "Bus Stops"
    ),
    options = layersControlOptions(collapsed = FALSE)
  ) %>%
  hideGroup(c("All Bus Routes", "Bus Stops"))

# Add legend
comparison_map <- comparison_map %>%
  addLegend(
    position = "bottomright",
    colors = c("gray", "red", "red"),
    labels = c(
      "All Routes",
      "Qualifying Corridor Segments",
      "Corridor Buffer (680 ft)"
    ),
    title = "SMTD Corridor Analysis"
  )

# =============================================================================
# STEP 7: Save Results and Display
# =============================================================================

cat("\n\nSTEP 7: Saving Results\n")
cat("----------------------\n\n")

# Save the map
htmlwidgets::saveWidget(comparison_map, "tests/smtd_corridor_comparison.html",
                        selfcontained = TRUE)
cat("Map saved to: tests/smtd_corridor_comparison.html\n")

# Save diagnostic data
diagnostic_data <- list(
  route_frequency = route_freq,
  current_method_summary = current_method_summary,
  qualifying_routes = qualifying_routes,
  corridor_area = current_area
)
saveRDS(diagnostic_data, "tests/smtd_corridor_diagnostics.rds")
cat("Diagnostic data saved to: tests/smtd_corridor_diagnostics.rds\n")

# =============================================================================
# SUMMARY
# =============================================================================

cat("\n")
cat("=============================================================================\n")
cat("  ANALYSIS SUMMARY\n")
cat("=============================================================================\n\n")

cat("ROUTE-LEVEL FREQUENCY ANALYSIS (for reference):\n")
cat(sprintf("  - %d total bus routes in SMTD\n", nrow(smtd_routes[route_type == 3])))
cat(sprintf("  - %d route-directions have ≤15 min frequency in AM or PM\n",
            nrow(route_freq[qualifies == TRUE])))
cat(sprintf("  - %d unique routes with qualifying service\n",
            length(qualifying_routes)))

cat("\nEDGE-BASED CORRIDOR IDENTIFICATION:\n")
cat(sprintf("  - Found %d qualifying corridor segments\n", nrow(current_method_segments)))
cat(sprintf("  - Corridor buffer area: %.2f sq mi\n", current_area))

if (nrow(current_method_summary) > 0) {
  cat(sprintf("  - Total edges analyzed: %d\n", nrow(current_method_summary)))
  cat(sprintf("  - Qualifying edges: %d\n", sum(current_method_summary$qualifies)))
}

cat("\nHOW IT WORKS:\n")
cat("  1. Clusters bus stops spatially (150 ft radius)\n")
cat("  2. Builds edges between consecutive clusters along each trip\n")
cat("  3. Aggregates trip counts per edge (from_cluster → to_cluster)\n")
cat("  4. Routes sharing the same street get combined automatically\n")
cat("  5. Qualifies edges with ≤15 min frequency in AM or PM peak\n")

cat("\n")
cat("=============================================================================\n")
cat("  VIEW MAP: Open tests/smtd_corridor_comparison.html in a browser\n")
cat("=============================================================================\n")

# Display map if in interactive session
if (interactive()) {
  print(comparison_map)
}
