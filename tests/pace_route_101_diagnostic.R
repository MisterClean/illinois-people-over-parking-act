# =============================================================================
# PACE Route 101 (Dempster) Corridor Diagnostic Test Script
# =============================================================================
#
# Purpose: Diagnose why PACE Route 101 (Dempster Line) is not qualifying as a
# high-frequency corridor when it should have 15-minute or better frequency.
#
# This script:
#   1. Loads PACE GTFS data
#   2. Analyzes Route 101 at route level (should it qualify?)
#   3. Analyzes at edge level (what does the current method find?)
#   4. Compares the two to identify the discrepancy
#   5. Generates diagnostic output and visualization
#
# Expected output: Understanding of why Route 101 isn't qualifying and what
# needs to be fixed in the corridor identification methodology.
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
source("R/buffer_processing.R")
source("R/map_creation.R")

cat("\n")
cat("=============================================================================\n")
cat("  PACE ROUTE 101 (DEMPSTER) CORRIDOR DIAGNOSTIC\n")
cat("=============================================================================\n\n")

# =============================================================================
# STEP 1: Download and Process PACE GTFS Data Only
# =============================================================================

cat("STEP 1: Loading PACE GTFS Data\n")
cat("-------------------------------\n\n")

# Get only PACE configuration
pace_config <- list(pace = get_agency_metadata()$pace)

# Process PACE data
pace_agencies <- process_agencies_with_tidytransit(pace_config, validate = TRUE)
pace_combined <- combine_agency_data(pace_agencies)

# Extract data tables
pace_stops <- pace_combined$stops
pace_routes <- pace_combined$routes
pace_trips <- pace_combined$trips
pace_stop_times <- pace_combined$stop_times
pace_calendar <- pace_combined$calendar
pace_calendar_dates <- pace_combined$calendar_dates
pace_shapes <- pace_combined$shapes

# Enrich stop_times
pace_stop_times <- enrich_stop_times(pace_stop_times, pace_trips, pace_routes)

cat("\nPACE Data Summary:\n")
cat(sprintf("  Stops: %d\n", nrow(pace_stops)))
cat(sprintf("  Routes: %d\n", nrow(pace_routes)))
cat(sprintf("  Trips: %d\n", nrow(pace_trips)))
cat(sprintf("  Stop times: %d\n", nrow(pace_stop_times)))
cat(sprintf("  Shapes: %d unique\n", length(unique(pace_shapes$unique_shape_id))))

# =============================================================================
# STEP 2: Identify Route 101 (Dempster)
# =============================================================================

cat("\n\nSTEP 2: Identifying Route 101 (Dempster)\n")
cat("-----------------------------------------\n\n")

# Find Route 101 in the routes table
route_101 <- pace_routes[route_short_name == "101" | route_id == "101"]

if (nrow(route_101) == 0) {
  # Try searching by long name
  route_101 <- pace_routes[grepl("Dempster|101", route_long_name, ignore.case = TRUE)]
}

if (nrow(route_101) == 0) {
  cat("WARNING: Could not find Route 101 in PACE routes!\n")
  cat("Available routes:\n")
  print(head(pace_routes[, .(route_id, route_short_name, route_long_name)], 20))
  stop("Route 101 not found")
}

cat("Route 101 found:\n")
print(route_101[, .(unique_route_id, route_id, route_short_name, route_long_name, route_type)])

route_101_id <- route_101$unique_route_id[1]
cat(sprintf("\nUsing unique_route_id: %s\n", route_101_id))

# Get Route 101 trips
route_101_trips <- pace_trips[unique_route_id == route_101_id]
cat(sprintf("Total Route 101 trips (all services): %d\n", nrow(route_101_trips)))

# Get Route 101 shapes
route_101_shape_ids <- unique(route_101_trips$unique_shape_id)
cat(sprintf("Route 101 shape IDs: %s\n", paste(route_101_shape_ids, collapse = ", ")))

# =============================================================================
# STEP 3: Analyze Representative Service Selection
# =============================================================================

cat("\n\nSTEP 3: Representative Service Selection\n")
cat("-----------------------------------------\n\n")

# Check calendar data
cat("PACE Calendar entries:\n")
if (nrow(pace_calendar) > 0) {
  print(pace_calendar)
} else {
  cat("  No calendar.txt data (uses calendar_dates.txt)\n")
}

cat("\nPACE Calendar Dates summary:\n")
if (nrow(pace_calendar_dates) > 0) {
  cat(sprintf("  Total entries: %d\n", nrow(pace_calendar_dates)))
  cat(sprintf("  Unique service_ids: %d\n", uniqueN(pace_calendar_dates$service_id)))

  # Show service_ids with most dates
  service_date_counts <- pace_calendar_dates[, .N, by = service_id]
  setorder(service_date_counts, -N)
  cat("\n  Top 10 service_ids by date count:\n")
  print(head(service_date_counts, 10))
}

# Get representative service
bus_routes <- pace_routes[route_type == 3]
bus_trips <- pace_trips[unique_route_id %in% bus_routes$unique_route_id]

rep_services <- get_representative_services_by_agency(
  pace_calendar, pace_calendar_dates, bus_trips
)
cat("\nRepresentative service selected:\n")
print(rep_services)

weekday_trips <- filter_trips_to_representative_service(bus_trips, rep_services)

# How many Route 101 trips are in the representative service?
route_101_weekday_trips <- weekday_trips[unique_route_id == route_101_id]
cat(sprintf("\nRoute 101 trips in representative service: %d (of %d total)\n",
            nrow(route_101_weekday_trips), nrow(route_101_trips)))

# Check direction_id
if ("direction_id" %in% names(route_101_weekday_trips)) {
  cat("\nRoute 101 trips by direction:\n")
  print(route_101_weekday_trips[, .N, by = direction_id])
}

# =============================================================================
# STEP 4: Analyze Route 101 Stop Times Distribution
# =============================================================================

cat("\n\nSTEP 4: Route 101 Stop Times Distribution\n")
cat("------------------------------------------\n\n")

# Get Route 101 stop times
route_101_stop_times <- pace_stop_times[
  unique_trip_id %in% route_101_weekday_trips$unique_trip_id
]
cat(sprintf("Route 101 stop events in representative service: %d\n", nrow(route_101_stop_times)))

# Parse arrival times
route_101_stop_times[, arrival_time_hhmmss := substr(arrival_time, 1, 8)]
route_101_stop_times[, arrival_hour := as.integer(substr(arrival_time, 1, 2))]

# Filter out overnight (>= 24:00)
route_101_stop_times_sameday <- route_101_stop_times[arrival_hour < 24]
route_101_stop_times_sameday[, arrival_time_obj := as.ITime(arrival_time_hhmmss, format = "%H:%M:%S")]

# Show distribution by hour
cat("\nStop event distribution by hour:\n")
hour_dist <- route_101_stop_times_sameday[, .N, by = arrival_hour]
setorder(hour_dist, arrival_hour)
print(hour_dist)

# Define peak periods
am_start <- as.ITime("07:00:00")
am_end <- as.ITime("09:00:00")
pm_start <- as.ITime("16:00:00")
pm_end <- as.ITime("18:00:00")

# Count stop events in peak periods
am_stop_events <- route_101_stop_times_sameday[
  arrival_time_obj >= am_start & arrival_time_obj <= am_end
]
pm_stop_events <- route_101_stop_times_sameday[
  arrival_time_obj >= pm_start & arrival_time_obj <= pm_end
]

cat(sprintf("\nAM Peak (7-9am) stop events: %d\n", nrow(am_stop_events)))
cat(sprintf("PM Peak (4-6pm) stop events: %d\n", nrow(pm_stop_events)))

# =============================================================================
# STEP 5: Route-Level Frequency Analysis
# =============================================================================

cat("\n\nSTEP 5: Route-Level Frequency Analysis\n")
cat("---------------------------------------\n\n")

# Join stop times with trips to get direction_id
route_101_stop_times_sameday <- merge(
  route_101_stop_times_sameday,
  route_101_weekday_trips[, .(unique_trip_id, direction_id, unique_shape_id)],
  by = "unique_trip_id",
  all.x = TRUE
)

# ROUTE-LEVEL: Count UNIQUE TRIPS in each peak period
# (A trip counts if ANY of its stops arrive during peak)

# AM peak - unique trips
am_trips_in_route <- route_101_stop_times_sameday[
  arrival_time_obj >= am_start & arrival_time_obj <= am_end,
  .(trips_am = uniqueN(unique_trip_id)),
  by = direction_id
]

# PM peak - unique trips
pm_trips_in_route <- route_101_stop_times_sameday[
  arrival_time_obj >= pm_start & arrival_time_obj <= pm_end,
  .(trips_pm = uniqueN(unique_trip_id)),
  by = direction_id
]

# Combine
route_level_freq <- merge(am_trips_in_route, pm_trips_in_route, by = "direction_id", all = TRUE)
route_level_freq[is.na(trips_am), trips_am := 0]
route_level_freq[is.na(trips_pm), trips_pm := 0]
route_level_freq[, interval_am := fifelse(trips_am > 0, 120 / trips_am, Inf)]
route_level_freq[, interval_pm := fifelse(trips_pm > 0, 120 / trips_pm, Inf)]
route_level_freq[, qualifies := interval_am <= 15 | interval_pm <= 15]

cat("ROUTE-LEVEL Frequency for Route 101:\n")
cat("(Counts unique trips with ANY stop during peak period)\n\n")
print(route_level_freq)

route_qualifies_route_level <- any(route_level_freq$qualifies)
cat(sprintf("\n>>> Route 101 QUALIFIES at route level: %s <<<\n",
            ifelse(route_qualifies_route_level, "YES", "NO")))

# =============================================================================
# STEP 6: Run Current Edge-Based Method
# =============================================================================

cat("\n\nSTEP 6: Running Current Edge-Based Method\n")
cat("------------------------------------------\n\n")

# Run hub identification to get peak stop times
hub_results <- identify_all_hubs(
  pace_stops, pace_routes, pace_trips,
  pace_stop_times, pace_calendar, pace_calendar_dates
)

am_peak_bus_stops <- hub_results$am_peak_bus_stops
pm_peak_bus_stops <- hub_results$pm_peak_bus_stops

cat(sprintf("Total AM peak bus stop events (all PACE routes): %d\n", nrow(am_peak_bus_stops)))
cat(sprintf("Total PM peak bus stop events (all PACE routes): %d\n", nrow(pm_peak_bus_stops)))

# Filter to Route 101 only
am_peak_101 <- am_peak_bus_stops[unique_route_id == route_101_id]
pm_peak_101 <- pm_peak_bus_stops[unique_route_id == route_101_id]

cat(sprintf("\nRoute 101 AM peak stop events: %d\n", nrow(am_peak_101)))
cat(sprintf("Route 101 PM peak stop events: %d\n", nrow(pm_peak_101)))

# Run corridor identification for PACE only
corridor_results <- identify_qualifying_corridors(
  pace_stops, am_peak_bus_stops, pm_peak_bus_stops,
  pace_trips, pace_shapes
)

current_method_segments <- corridor_results$qualifying_corridor_segments
edge_summary <- corridor_results$qualification_summary

cat(sprintf("\nTotal qualifying corridor segments (all PACE routes): %d\n",
            nrow(current_method_segments)))

# =============================================================================
# STEP 7: Analyze Edge-Level Results for Route 101
# =============================================================================

cat("\n\nSTEP 7: Edge-Level Analysis for Route 101\n")
cat("------------------------------------------\n\n")

# Find edges that include Route 101
if (nrow(edge_summary) > 0) {
  # The edge_summary has routes_am and routes_pm as lists
  # We need to check if Route 101 is in any of those lists

  route_101_edges <- edge_summary[
    sapply(routes_am, function(r) route_101_id %in% r) |
    sapply(routes_pm, function(r) route_101_id %in% r)
  ]

  cat(sprintf("Edges involving Route 101: %d\n", nrow(route_101_edges)))

  if (nrow(route_101_edges) > 0) {
    cat("\nRoute 101 Edge Summary:\n")
    print(route_101_edges[, .(
      from_cluster, to_cluster, direction_id,
      trips_am, trips_pm,
      interval_am = round(interval_am, 1),
      interval_pm = round(interval_pm, 1),
      qualifies
    )])

    qualifying_101_edges <- route_101_edges[qualifies == TRUE]
    cat(sprintf("\nQualifying Route 101 edges: %d of %d\n",
                nrow(qualifying_101_edges), nrow(route_101_edges)))
  }
} else {
  cat("WARNING: No edges in edge_summary!\n")
}

# =============================================================================
# STEP 8: Diagnose the Issue
# =============================================================================

cat("\n\n")
cat("=============================================================================\n")
cat("  DIAGNOSIS\n")
cat("=============================================================================\n\n")

cat("COMPARISON: Route-Level vs Edge-Level\n")
cat("-------------------------------------\n\n")

cat("Route-Level Analysis:\n")
cat(sprintf("  AM peak trips: %d per direction\n",
            sum(route_level_freq$trips_am)))
cat(sprintf("  PM peak trips: %d per direction\n",
            sum(route_level_freq$trips_pm)))
cat(sprintf("  Best interval: %.1f min\n",
            min(c(route_level_freq$interval_am, route_level_freq$interval_pm))))
cat(sprintf("  Qualifies: %s\n", ifelse(route_qualifies_route_level, "YES", "NO")))

cat("\nEdge-Level Analysis:\n")
if (nrow(edge_summary) > 0 && exists("route_101_edges") && nrow(route_101_edges) > 0) {
  cat(sprintf("  Total edges for Route 101: %d\n", nrow(route_101_edges)))
  cat(sprintf("  Qualifying edges: %d\n", sum(route_101_edges$qualifies)))

  if (nrow(route_101_edges) > 0) {
    best_edge_interval <- min(c(
      route_101_edges$interval_am[is.finite(route_101_edges$interval_am)],
      route_101_edges$interval_pm[is.finite(route_101_edges$interval_pm)]
    ), na.rm = TRUE)
    cat(sprintf("  Best edge interval: %.1f min\n", best_edge_interval))
  }
} else {
  cat("  No edges found for Route 101!\n")
}

# Determine the likely cause
cat("\n\nLIKELY CAUSE:\n")
cat("-------------\n")

if (route_qualifies_route_level) {
  if (!exists("route_101_edges") || nrow(route_101_edges) == 0) {
    cat("SCENARIO A: Route qualifies at route-level but NO EDGES were created.\n")
    cat("This suggests the edge-building process is the issue.\n")
    cat("\nPossible reasons:\n")
    cat("  1. Peak stop times are filtered too aggressively\n")
    cat("  2. Stop clustering is preventing edge creation\n")
    cat("  3. Shape data issues\n")
  } else if (sum(route_101_edges$qualifies) == 0) {
    cat("SCENARIO A: Route qualifies at route-level but EDGES DON'T QUALIFY.\n")
    cat("This is likely because edges are calculated per-direction and the\n")
    cat("trip counts per edge are lower than the route-level trip counts.\n")
    cat("\nThe edge method is more granular - it counts trips on each specific\n")
    cat("segment, which may be lower than total route trips if the route has\n")
    cat("branches or if not all trips cover all segments.\n")
  } else {
    cat("SCENARIO C: Both route-level and edge-level show Route 101 qualifies.\n")
    cat("The issue may be in geometry creation or buffering.\n")
  }
} else {
  cat("SCENARIO B: Route 101 does NOT qualify even at route-level!\n")
  cat("This means the representative service selection may be wrong,\n")
  cat("or Route 101 genuinely doesn't have 15-min service during peak.\n")
}

# =============================================================================
# STEP 9: Create Diagnostic Map
# =============================================================================

cat("\n\nSTEP 9: Creating Diagnostic Map\n")
cat("--------------------------------\n\n")

# Convert Route 101 shapes to linestrings
shapes_sf <- convert_shapes_to_linestrings(pace_shapes)
route_101_shapes_sf <- shapes_sf[shapes_sf$unique_shape_id %in% route_101_shape_ids, ]

cat(sprintf("Route 101 shapes: %d linestrings\n", nrow(route_101_shapes_sf)))

# Get center of Route 101 for map
if (nrow(route_101_shapes_sf) > 0) {
  route_101_bbox <- st_bbox(route_101_shapes_sf)
  map_center <- c(
    (route_101_bbox["xmin"] + route_101_bbox["xmax"]) / 2,
    (route_101_bbox["ymin"] + route_101_bbox["ymax"]) / 2
  )
} else {
  # Fallback to Evanston (where Dempster runs)
  map_center <- c(-87.68, 42.04)
}

# Create diagnostic map
diagnostic_map <- leaflet() %>%
  addTiles(group = "Street Map") %>%
  addProviderTiles(providers$CartoDB.Positron, group = "Light") %>%
  setView(lng = map_center[1], lat = map_center[2], zoom = 12)

# Add all PACE shapes (gray, background)
if (nrow(shapes_sf) > 0) {
  diagnostic_map <- diagnostic_map %>%
    addPolylines(
      data = shapes_sf,
      color = "gray",
      weight = 1,
      opacity = 0.3,
      group = "All PACE Routes"
    )
}

# Add Route 101 shape (blue, highlighted)
if (nrow(route_101_shapes_sf) > 0) {
  diagnostic_map <- diagnostic_map %>%
    addPolylines(
      data = route_101_shapes_sf,
      color = "blue",
      weight = 4,
      opacity = 0.8,
      group = "Route 101 Shape",
      popup = "Route 101 (Dempster)"
    )
}

# Add qualifying corridor segments (red)
if (nrow(current_method_segments) > 0) {
  diagnostic_map <- diagnostic_map %>%
    addPolylines(
      data = current_method_segments,
      color = "red",
      weight = 3,
      opacity = 0.9,
      group = "Qualifying Corridor Segments",
      popup = ~paste0(
        "Trips AM: ", trips_am, "<br>",
        "Trips PM: ", trips_pm, "<br>",
        "Interval AM: ", round(interval_am, 1), " min<br>",
        "Interval PM: ", round(interval_pm, 1), " min"
      )
    )
}

# Add Route 101 stops
route_101_stop_ids <- unique(route_101_stop_times$unique_stop_id)
route_101_stops_sf <- pace_stops[unique_stop_id %in% route_101_stop_ids]

if (nrow(route_101_stops_sf) > 0) {
  if (!inherits(route_101_stops_sf, "sf")) {
    route_101_stops_sf <- st_as_sf(route_101_stops_sf,
                                    coords = c("stop_lon", "stop_lat"),
                                    crs = 4326)
  }

  diagnostic_map <- diagnostic_map %>%
    addCircleMarkers(
      data = route_101_stops_sf,
      radius = 4,
      color = "blue",
      fillColor = "white",
      fillOpacity = 0.8,
      weight = 1,
      group = "Route 101 Stops",
      popup = ~paste0("<b>", stop_name, "</b><br>Stop ID: ", stop_id)
    )
}

# Add layer controls
diagnostic_map <- diagnostic_map %>%
  addLayersControl(
    baseGroups = c("Street Map", "Light"),
    overlayGroups = c(
      "All PACE Routes",
      "Route 101 Shape",
      "Qualifying Corridor Segments",
      "Route 101 Stops"
    ),
    options = layersControlOptions(collapsed = FALSE)
  ) %>%
  hideGroup(c("All PACE Routes", "Route 101 Stops"))

# Add legend
diagnostic_map <- diagnostic_map %>%
  addLegend(
    position = "bottomright",
    colors = c("gray", "blue", "red"),
    labels = c(
      "All PACE Routes",
      "Route 101 (Dempster)",
      "Qualifying Corridor Segments"
    ),
    title = "Route 101 Diagnostic"
  )

# =============================================================================
# STEP 10: Save Results
# =============================================================================

cat("\n\nSTEP 10: Saving Results\n")
cat("-----------------------\n\n")

# Save the map
htmlwidgets::saveWidget(diagnostic_map, "tests/pace_route_101_diagnostic.html",
                        selfcontained = TRUE)
cat("Map saved to: tests/pace_route_101_diagnostic.html\n")

# Save diagnostic data
diagnostic_data <- list(
  route_101_info = route_101,
  route_level_frequency = route_level_freq,
  route_qualifies_route_level = route_qualifies_route_level,
  edge_summary_all = edge_summary,
  route_101_edges = if (exists("route_101_edges")) route_101_edges else data.table(),
  am_peak_stop_events = nrow(am_peak_101),
  pm_peak_stop_events = nrow(pm_peak_101),
  representative_service = rep_services,
  stop_time_distribution = hour_dist
)
saveRDS(diagnostic_data, "tests/pace_route_101_diagnostic.rds")
cat("Diagnostic data saved to: tests/pace_route_101_diagnostic.rds\n")

# =============================================================================
# SUMMARY
# =============================================================================

cat("\n")
cat("=============================================================================\n")
cat("  SUMMARY\n")
cat("=============================================================================\n\n")

cat("ROUTE 101 (DEMPSTER) ANALYSIS:\n")
cat(sprintf("  - Total trips in representative service: %d\n", nrow(route_101_weekday_trips)))
cat(sprintf("  - AM peak trips (route-level): %d\n", sum(route_level_freq$trips_am)))
cat(sprintf("  - PM peak trips (route-level): %d\n", sum(route_level_freq$trips_pm)))
cat(sprintf("  - Route-level best interval: %.1f min\n",
            min(c(route_level_freq$interval_am, route_level_freq$interval_pm))))
cat(sprintf("  - QUALIFIES at route level: %s\n",
            ifelse(route_qualifies_route_level, "YES", "NO")))

cat("\nEDGE-BASED METHOD RESULTS:\n")
if (exists("route_101_edges") && nrow(route_101_edges) > 0) {
  cat(sprintf("  - Edges for Route 101: %d\n", nrow(route_101_edges)))
  cat(sprintf("  - Qualifying edges: %d\n", sum(route_101_edges$qualifies)))
} else {
  cat("  - No edges created for Route 101\n")
}

cat("\nRECOMMENDED FIX:\n")
if (route_qualifies_route_level && (!exists("route_101_edges") || sum(route_101_edges$qualifies) == 0)) {
  cat("Use ROUTE-LEVEL qualification with shape buffering for single-route corridors.\n")
  cat("If a route has <= 15 min frequency in either peak, buffer its entire shape.\n")
  cat("The edge method should be supplementary for combining overlapping routes.\n")
} else if (!route_qualifies_route_level) {
  cat("Investigate representative service selection - Route 101 may genuinely not\n")
  cat("have 15-min service, or the wrong service_id was selected.\n")
}

cat("\n")
cat("=============================================================================\n")
cat("  VIEW MAP: Open tests/pace_route_101_diagnostic.html in a browser\n")
cat("=============================================================================\n")

# Display map if in interactive session
if (interactive()) {
  print(diagnostic_map)
}
