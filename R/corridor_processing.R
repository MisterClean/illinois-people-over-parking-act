# Corridor Processing Functions
#
# Functions for identifying qualifying transit corridors and creating
# corridor buffers along actual transit route geometry.
#
# High-Level Functions:
#   - identify_qualifying_corridors(): Complete corridor qualification workflow
#   - convert_shapes_to_linestrings(): Convert GTFS shapes to route geometry
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

#' Convert GTFS Shapes to LINESTRING Geometries
#'
#' Converts GTFS shapes.txt data (sequences of lat/lon points) into sf
#' LINESTRING geometries representing actual transit route paths. Each
#' unique_shape_id becomes one LINESTRING feature.
#'
#' @param all_shapes data.table with GTFS shapes data containing:
#'   unique_shape_id, shape_pt_lat, shape_pt_lon, shape_pt_sequence
#' @return sf object with LINESTRING geometries (one per unique_shape_id) in WGS84
#'
#' @details
#' GTFS shapes represent the actual path vehicles travel, defined as ordered
#' sequences of latitude/longitude points. This function:
#' \enumerate{
#'   \item Groups shape points by unique_shape_id
#'   \item Orders points within each shape by shape_pt_sequence
#'   \item Converts each point sequence to a LINESTRING geometry
#'   \item Returns sf object ready for spatial operations
#' }
#'
#' Empty or single-point shapes are filtered out as they cannot form valid
#' LINESTRING geometries (minimum 2 points required).
#'
#' @examples
#' \dontrun{
#' shapes_sf <- convert_shapes_to_linestrings(all_shapes)
#' # Result has one row per shape with LINESTRING geometry
#' }
convert_shapes_to_linestrings <- function(all_shapes) {
  cat("\n=== Converting GTFS Shapes to LINESTRING Geometries ===\n\n")

  # Filter to non-empty shapes and order by sequence
  shapes_ordered <- all_shapes[!is.na(unique_shape_id) & !is.na(shape_pt_lat) & !is.na(shape_pt_lon)]
  setorder(shapes_ordered, unique_shape_id, shape_pt_sequence)

  # Split into list by unique_shape_id
  shapes_list <- split(shapes_ordered, by = "unique_shape_id")

  cat(sprintf("Processing %d unique shapes...\n", length(shapes_list)))

  # Convert each shape to LINESTRING
  linestrings_list <- lapply(names(shapes_list), function(shape_id) {
    shape_points <- shapes_list[[shape_id]]

    # Need at least 2 points for a valid LINESTRING
    if (nrow(shape_points) < 2) {
      return(NULL)
    }

    # Extract coordinates as matrix (lon, lat order for sf)
    coords <- as.matrix(shape_points[, .(shape_pt_lon, shape_pt_lat)])

    # Create LINESTRING geometry
    linestring <- st_linestring(coords)

    # Return as sf-compatible data.frame
    data.frame(
      unique_shape_id = shape_id,
      agency = shape_points$agency[1],
      num_points = nrow(shape_points),
      geometry = st_sfc(linestring, crs = 4326)
    )
  })

  # Remove NULL entries (shapes with < 2 points)
  linestrings_list <- linestrings_list[!sapply(linestrings_list, is.null)]

  # Combine into single sf object
  shapes_sf <- do.call(rbind, linestrings_list)
  shapes_sf <- st_as_sf(shapes_sf)

  cat(sprintf("Created %d LINESTRING geometries\n", nrow(shapes_sf)))
  cat(sprintf("  Total shape points: %s\n", format(sum(shapes_sf$num_points), big.mark = ",")))

  return(shapes_sf)
}

#' Create Corridor Buffers Using GTFS Route Shapes
#'
#' Creates 1/8 mile buffers around actual transit route paths using GTFS
#' shapes.txt geometry, measured from the street edge rather than centerline.
#' This approach buffers only the portions of routes where frequent service
#' actually operates, rather than entire street segments.
#'
#' @param qualifying_corridor_stops_sf sf object with qualifying corridor stops
#' @param all_stop_times data.table with all stop_times (to link stops to trips)
#' @param all_trips data.table with all trips (must include unique_shape_id)
#' @param all_shapes data.table with GTFS shapes data
#' @param illinois_boundary sf object with Illinois state boundary
#' @return sf object with buffered corridor geometry (WGS84)
#'
#' @details
#' This function:
#' \enumerate{
#'   \item Identifies trips serving qualifying corridor stops
#'   \item Extracts unique shape IDs from those trips
#'   \item Converts qualifying shapes to LINESTRING geometries
#'   \item Buffers each route path by 680 feet (660ft + 20ft for street width)
#'   \item Unions overlapping buffers
#'   \item Clips to Illinois boundary
#' }
#'
#' The buffer distance accounts for:
#' \itemize{
#'   \item 660 feet (1/8 mile) from street edge per SB2111
#'   \item 20 feet for typical half-street-width (centerline to curb)
#'   \item Total: 680 feet from route centerline
#' }
#'
#' The shapes-based approach is more accurate than street network approximation
#' because it uses the actual paths vehicles travel, including highways,
#' expressways, and complex routing not well-represented in TIGER/Line data.
#'
#' @examples
#' \dontrun{
#' corridor_buffer <- create_corridor_buffers(
#'   qualifying_corridor_stops_sf,
#'   all_stop_times,
#'   all_trips,
#'   all_shapes,
#'   illinois_boundary
#' )
#' }
create_corridor_buffers <- function(qualifying_corridor_stops_sf,
                                    all_stop_times,
                                    all_trips,
                                    all_shapes,
                                    illinois_boundary) {
  cat("\n=== Creating Corridor Buffers from GTFS Shapes ===\n\n")

  # Extract unique_stop_ids from qualifying corridor stops
  qualifying_stop_ids <- unique(qualifying_corridor_stops_sf$unique_stop_id)
  cat(sprintf("Qualifying corridor stops: %d\n", length(qualifying_stop_ids)))

  # Find trips that serve qualifying corridor stops
  # Link stops → stop_times → trips → shapes
  cat("Finding trips that serve qualifying corridor stops...\n")
  corridor_stop_times <- all_stop_times[unique_stop_id %in% qualifying_stop_ids]
  cat(sprintf("Stop time records at qualifying stops: %s\n",
              format(nrow(corridor_stop_times), big.mark = ",")))

  # Get unique trip IDs from these stop_times
  qualifying_trip_ids <- unique(corridor_stop_times$unique_trip_id)
  cat(sprintf("Trips serving qualifying stops: %s\n",
              format(length(qualifying_trip_ids), big.mark = ",")))

  # Join with trips to get shape_ids
  corridor_trips <- all_trips[unique_trip_id %in% qualifying_trip_ids]

  # Filter to trips that have shape data
  corridor_trips_with_shapes <- corridor_trips[!is.na(unique_shape_id) & unique_shape_id != ""]
  cat(sprintf("Trips with shape data: %s\n",
              format(nrow(corridor_trips_with_shapes), big.mark = ",")))

  # Get unique shape IDs
  qualifying_shape_ids <- unique(corridor_trips_with_shapes$unique_shape_id)
  cat(sprintf("Unique route shapes to buffer: %d\n", length(qualifying_shape_ids)))

  # Filter shapes to only qualifying shape IDs
  qualifying_shapes <- all_shapes[unique_shape_id %in% qualifying_shape_ids]
  cat(sprintf("Shape points to process: %s\n",
              format(nrow(qualifying_shapes), big.mark = ",")))

  # Convert shapes to LINESTRING geometries
  shapes_sf <- convert_shapes_to_linestrings(qualifying_shapes)

  # Project to Illinois State Plane (feet) for accurate buffering
  cat("Buffering route geometries by 1/8 mile from street edge...\n")
  shapes_projected <- st_transform(shapes_sf, 3435)

  # Adjust buffer to measure from street edge rather than route centerline
  # Add conservative estimate of half-street-width (centerline to curb)
  # Based on research: local streets 10-16ft, arterials 14-25ft (AASHTO standards)
  # 20 feet is conservative middle value covering most street types
  half_street_width <- 20  # feet, centerline to curb
  buffer_from_edge <- 660 + half_street_width  # 680 feet total

  corridor_buffers <- st_buffer(shapes_projected, buffer_from_edge)

  # Union all buffers
  cat("Unioning corridor buffers...\n")
  all_corridors_union <- st_union(corridor_buffers)

  # Transform back to WGS84 and clip to Illinois boundary
  cat("Clipping to Illinois boundary...\n")
  all_corridors_union_wgs84_raw <- st_transform(all_corridors_union, 4326)
  all_corridors_union_wgs84 <- st_intersection(all_corridors_union_wgs84_raw, illinois_boundary)

  cat("Corridor buffers created successfully\n")

  return(all_corridors_union_wgs84)
}
