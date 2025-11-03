# Corridor Processing Functions
#
# Functions for identifying qualifying transit corridors and creating
# corridor buffers along street segments.
#
# High-Level Functions:
#   - identify_qualifying_corridors(): Complete corridor qualification workflow
#   - create_corridor_buffers(): Create buffered corridor geometries
#
# Helper Functions:
#   - calculate_corridor_metrics(): Calculate AM/PM frequency metrics per stop
#   - download_tiger_streets(): Download TIGER/Line street data for counties

#' Calculate Corridor Metrics
#'
#' Calculates AM/PM frequency metrics at all bus stops (no clustering).
#' Corridors qualify if frequency <= 15 minutes in EITHER AM or PM peak.
#'
#' @param am_peak_bus_stops AM peak bus stops data.table
#' @param pm_peak_bus_stops PM peak bus stops data.table
#' @return data.table of bus stops with corridor metrics and qualification status
calculate_corridor_metrics <- function(am_peak_bus_stops, pm_peak_bus_stops) {
  # Calculate AM peak frequency at all bus stops (no clustering)
  am_corridor_trips <- am_peak_bus_stops[, .(
    trips_am = .N,
    num_routes_am = uniqueN(unique_route_id)
  ), by = .(unique_stop_id, agency)]
  am_corridor_trips[, interval_am := 120 / trips_am]

  # Calculate PM peak frequency at all bus stops
  pm_corridor_trips <- pm_peak_bus_stops[, .(
    trips_pm = .N,
    num_routes_pm = uniqueN(unique_route_id)
  ), by = .(unique_stop_id, agency)]
  pm_corridor_trips[, interval_pm := 120 / trips_pm]

  # Combine AM and PM metrics
  all_corridor_metrics <- merge(
    am_corridor_trips,
    pm_corridor_trips,
    by = c("unique_stop_id", "agency"),
    all = TRUE
  )

  # Fill NAs
  all_corridor_metrics[is.na(num_routes_am), num_routes_am := 0]
  all_corridor_metrics[is.na(num_routes_pm), num_routes_pm := 0]
  all_corridor_metrics[is.na(trips_am), trips_am := 0]
  all_corridor_metrics[is.na(trips_pm), trips_pm := 0]
  all_corridor_metrics[is.na(interval_am), interval_am := Inf]
  all_corridor_metrics[is.na(interval_pm), interval_pm := Inf]

  # Calculate combined metrics
  all_corridor_metrics[, trips_total := trips_am + trips_pm]
  all_corridor_metrics[, interval_combined := 240 / trips_total]

  # Qualify corridors: frequency <= 15 in EITHER AM or PM
  # (No minimum route requirement - "one or more" routes)
  all_corridor_metrics[, qualifies_corridor := interval_am <= 15 | interval_pm <= 15]

  return(all_corridor_metrics)
}

#' Download TIGER/Line Streets
#'
#' Downloads street data from US Census TIGER/Line shapefiles for specified
#' Illinois counties. Returns combined street network as sf object.
#'
#' @param counties_fips Vector of county FIPS codes (e.g., c("031", "043"))
#' @param year Year for TIGER/Line data (default: 2023)
#' @return sf object with street network geometry
download_tiger_streets <- function(counties_fips, year = 2023) {
  cat(sprintf("Downloading TIGER/Line street data for %d Illinois counties...\n",
              length(counties_fips)))

  all_streets_sf <- rbindlist(lapply(counties_fips, function(co) {
    roads(state = "IL", county = co, year = year)
  })) %>% st_as_sf()

  cat(sprintf("Downloaded %d street segments\n", nrow(all_streets_sf)))

  return(all_streets_sf)
}

#' Identify Qualifying Corridors
#'
#' Identifies bus stops that qualify as transit corridors based on frequency
#' criteria. Returns sf object with geometry for mapping.
#'
#' @param all_stops Combined stops data.table
#' @param am_peak_bus_stops AM peak bus stops data.table
#' @param pm_peak_bus_stops PM peak bus stops data.table
#' @return sf object with qualifying corridor stops
#'
#' @examples
#' \dontrun{
#' corridors_sf <- identify_qualifying_corridors(all_stops, am_peak_bus_stops, pm_peak_bus_stops)
#' }
identify_qualifying_corridors <- function(all_stops, am_peak_bus_stops, pm_peak_bus_stops) {
  cat("\n=== Identifying Transit Corridors ===\n\n")

  # Calculate corridor metrics
  all_corridor_metrics <- calculate_corridor_metrics(am_peak_bus_stops, pm_peak_bus_stops)

  qualifying_corridor_stops_data <- all_corridor_metrics[qualifies_corridor == TRUE]

  cat(sprintf("Found %d qualifying corridor stops\n", nrow(qualifying_corridor_stops_data)))

  # Get geometry for qualifying stops
  qualifying_corridor_stops <- merge(
    all_stops,
    qualifying_corridor_stops_data,
    by = c("unique_stop_id", "agency")
  )

  qualifying_corridor_stops_sf <- st_as_sf(
    qualifying_corridor_stops,
    coords = c("stop_lon", "stop_lat"),
    crs = 4326
  )

  return(qualifying_corridor_stops_sf)
}

#' Create Corridor Buffers
#'
#' Creates 1/8 mile (660 ft) buffers around qualifying corridor street segments.
#' Snaps corridor stops to nearest street segments, then buffers and unions
#' the qualifying streets.
#'
#' @param qualifying_corridor_stops_sf sf object with qualifying corridor stops
#' @param illinois_boundary sf object with Illinois state boundary
#' @param counties_fips Vector of county FIPS codes for street data (default: Illinois transit counties)
#' @param year Year for TIGER/Line data (default: 2023)
#' @return sf object with buffered corridor geometry (WGS84)
#'
#' @examples
#' \dontrun{
#' corridor_buffer <- create_corridor_buffers(qualifying_corridor_stops_sf, illinois_boundary)
#' }
create_corridor_buffers <- function(qualifying_corridor_stops_sf,
                                    illinois_boundary,
                                    counties_fips = c("031", "043", "089", "097", "111", "197",  # Chicago metro
                                                      "163", "119", "133",                        # St. Louis IL
                                                      "019",                                      # Champaign-Urbana
                                                      "201"),                                     # Rockford
                                    year = 2023) {
  cat("\n=== Creating Corridor Buffers ===\n\n")

  # Download street network data
  all_streets_sf <- download_tiger_streets(counties_fips, year)

  # Project streets to IL State Plane (feet) for buffering and snapping
  all_streets_projected <- st_transform(all_streets_sf, 3435)

  # Snap qualifying stops to nearest street segment
  cat("Snapping corridor stops to street network...\n")
  qualifying_stops_projected <- st_transform(qualifying_corridor_stops_sf, 3435)

  # Find the index of the nearest street for each stop
  nearest_street_index <- st_nearest_feature(qualifying_stops_projected, all_streets_projected)

  # Get the unique IDs (LINEARID) of the qualifying street segments
  qualifying_street_ids <- unique(all_streets_projected$LINEARID[nearest_street_index])

  # Select the qualifying street segments
  qualifying_corridors_projected <- all_streets_projected[all_streets_projected$LINEARID %in% qualifying_street_ids, ]

  cat(sprintf("Identified %d qualifying street segments\n", nrow(qualifying_corridors_projected)))

  # Buffer corridors (1/8 mile = 660 feet)
  cat("Creating 1/8 mile buffers around corridors...\n")
  corridor_buffers_projected <- st_buffer(qualifying_corridors_projected, 660)
  all_corridors_union <- st_union(corridor_buffers_projected)

  # Transform back to WGS84 and clip to Illinois boundary
  all_corridors_union_wgs84_raw <- st_transform(all_corridors_union, 4326)
  all_corridors_union_wgs84 <- st_intersection(all_corridors_union_wgs84_raw, illinois_boundary)

  cat("Corridor buffers created successfully\n")

  return(all_corridors_union_wgs84)
}
