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

#' Calculate Route-Level Trip Counts
#'
#' Calculates trip counts per route/direction/peak for corridor frequency analysis.
#' Returns data.table with trip counts by route, direction, and peak period.
#'
#' @param am_peak_bus_stops AM peak bus stops data.table (from prepare_peak_stop_times)
#' @param pm_peak_bus_stops PM peak bus stops data.table (from prepare_peak_stop_times)
#' @return data.table with columns: unique_route_id, agency, direction_id,
#'   trips_am_dir0, trips_am_dir1, trips_pm_dir0, trips_pm_dir1
calculate_route_trip_counts <- function(am_peak_bus_stops, pm_peak_bus_stops) {
  # Check if direction_id is available
  has_direction <- "direction_id" %in% names(am_peak_bus_stops) &&
    sum(!is.na(am_peak_bus_stops$direction_id)) > 0

  if (has_direction) {
    # Calculate AM trips by route and direction
    am_trips <- am_peak_bus_stops[, .(
      trips = uniqueN(unique_trip_id)
    ), by = .(unique_route_id, agency, direction_id)]

    # Calculate PM trips by route and direction
    pm_trips <- pm_peak_bus_stops[, .(
      trips = uniqueN(unique_trip_id)
    ), by = .(unique_route_id, agency, direction_id)]

    # Reshape to wide format for easier joining
    am_trips_wide <- dcast(am_trips, unique_route_id + agency ~ direction_id,
                           value.var = "trips", fill = 0,
                           fun.aggregate = sum)
    setnames(am_trips_wide, old = c("0", "1"), new = c("trips_am_dir0", "trips_am_dir1"),
             skip_absent = TRUE)

    pm_trips_wide <- dcast(pm_trips, unique_route_id + agency ~ direction_id,
                           value.var = "trips", fill = 0,
                           fun.aggregate = sum)
    setnames(pm_trips_wide, old = c("0", "1"), new = c("trips_pm_dir0", "trips_pm_dir1"),
             skip_absent = TRUE)

    # Merge AM and PM
    route_trips <- merge(am_trips_wide, pm_trips_wide,
                        by = c("unique_route_id", "agency"), all = TRUE)

    # Fill any missing columns with 0
    for (col in c("trips_am_dir0", "trips_am_dir1", "trips_pm_dir0", "trips_pm_dir1")) {
      if (!col %in% names(route_trips)) {
        route_trips[, (col) := 0]
      } else {
        route_trips[is.na(get(col)), (col) := 0]
      }
    }
  } else {
    # No direction data - treat all trips as direction 0
    am_trips <- am_peak_bus_stops[, .(
      trips_am_dir0 = uniqueN(unique_trip_id)
    ), by = .(unique_route_id, agency)]

    pm_trips <- pm_peak_bus_stops[, .(
      trips_pm_dir0 = uniqueN(unique_trip_id)
    ), by = .(unique_route_id, agency)]

    route_trips <- merge(am_trips, pm_trips,
                        by = c("unique_route_id", "agency"), all = TRUE)

    route_trips[is.na(trips_am_dir0), trips_am_dir0 := 0]
    route_trips[is.na(trips_pm_dir0), trips_pm_dir0 := 0]
    route_trips[, trips_am_dir1 := 0]
    route_trips[, trips_pm_dir1 := 0]
  }

  return(route_trips)
}

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
    trips_am = uniqueN(unique_trip_id),
    num_routes_am = uniqueN(unique_route_id)
  ), by = .(unique_stop_id, agency)]
  am_corridor_trips[, interval_am := 120 / trips_am]

  # Calculate PM peak frequency at all bus stops
  pm_corridor_trips <- pm_peak_bus_stops[, .(
    trips_pm = uniqueN(unique_trip_id),
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


#' Identify Qualifying Corridors with Combined Frequency
#'
#' Converts GTFS shapes to LINESTRING segments and identifies true multi-route
#' corridors by spatially intersecting overlapping segments in each travel
#' direction. Route trips are summed for every shared roadway segment before
#' calculating AM/PM service intervals.
#'
#' @param route_trips data.table with trip counts (from calculate_route_trip_counts)
#' @param all_trips data.table with all trips (must include unique_shape_id and direction_id)
#' @param all_shapes data.table with GTFS shapes
#' @param max_interval_minutes Numeric. Maximum peak service interval (default: 15)
#' @param segment_length_meters Numeric. Target segment length when splitting
#'   shapes (default: 100 meters)
#' @return List with:
#'   - qualifying_shapes: sf object with true overlapping segments that qualify
#'   - qualification_summary: data.table with aggregated AM/PM metrics
identify_overlapping_segments <- function(route_trips, all_trips, all_shapes,
                                          max_interval_minutes = 15,
                                          segment_length_meters = 100) {
  cat("\n=== Identifying Qualifying Corridors (Shared Segment Aggregation) ===\n\n")

  if (!"unique_route_id" %in% names(all_trips) ||
      !"unique_shape_id" %in% names(all_trips)) {
    stop("all_trips must include unique_route_id and unique_shape_id columns")
  }

  # Ensure direction_id exists for downstream joins
  if (!"direction_id" %in% names(all_trips)) {
    all_trips[, direction_id := 0L]
  }

  # Link routes to shapes and keep direction/agency info
  route_shapes_link <- unique(all_trips[
    unique_route_id %in% route_trips$unique_route_id,
    .(unique_route_id, unique_shape_id, direction_id, agency)
  ])
  route_shapes_link <- route_shapes_link[
    !is.na(unique_shape_id) & unique_shape_id != ""
  ]
  route_shapes_link[is.na(direction_id), direction_id := 0L]

  if (nrow(route_shapes_link) == 0) {
    warning("No routes with valid shapes were found for overlap analysis")
    empty_dt <- data.table(
      agency = character(),
      direction_id = integer(),
      num_routes = integer(),
      routes = character(),
      trips_am = numeric(),
      trips_pm = numeric(),
      interval_am = numeric(),
      interval_pm = numeric()
    )
    return(list(
      qualifying_shapes = st_sf(empty_dt, geometry = st_sfc(crs = 4326)),
      qualification_summary = empty_dt
    ))
  }

  # Convert trip counts to long format (route/direction specific)
  route_direction_trips <- rbindlist(list(
    route_trips[, .(
      unique_route_id,
      agency,
      direction_id = 0L,
      trips_am = trips_am_dir0,
      trips_pm = trips_pm_dir0
    )],
    route_trips[, .(
      unique_route_id,
      agency,
      direction_id = 1L,
      trips_am = trips_am_dir1,
      trips_pm = trips_pm_dir1
    )]
  ), use.names = TRUE, fill = TRUE)

  route_direction_trips[, `:=`(
    trips_am = fifelse(is.na(trips_am), 0, trips_am),
    trips_pm = fifelse(is.na(trips_pm), 0, trips_pm)
  )]
  route_direction_trips <- route_direction_trips[, .(
    trips_am = sum(trips_am, na.rm = TRUE),
    trips_pm = sum(trips_pm, na.rm = TRUE)
  ), by = .(unique_route_id, agency, direction_id)]

  setkey(route_direction_trips, unique_route_id, agency, direction_id)

  # Convert shapes to sf LINESTRING geometries
  shapes_sf <- convert_shapes_to_linestrings(all_shapes)

  if (!inherits(shapes_sf, "sf")) {
    stop("convert_shapes_to_linestrings() must return an sf object")
  }

  # Join route/shape links to geometry and trip counts
  route_shapes_sf <- merge(
    route_shapes_link,
    shapes_sf,
    by = c("unique_shape_id", "agency"),
    all.x = TRUE
  )

  route_shapes_sf <- merge(
    route_shapes_sf,
    route_direction_trips,
    by = c("unique_route_id", "agency", "direction_id"),
    all.x = TRUE
  )

  route_shapes_sf[, `:=`(
    trips_am = fifelse(is.na(trips_am), 0, trips_am),
    trips_pm = fifelse(is.na(trips_pm), 0, trips_pm)
  )]

  route_shapes_sf <- route_shapes_sf[!is.na(geometry)]

  if (nrow(route_shapes_sf) == 0) {
    warning("No route shapes available after joining with trip counts")
    empty_dt <- data.table(
      agency = character(),
      direction_id = integer(),
      num_routes = integer(),
      routes = character(),
      trips_am = numeric(),
      trips_pm = numeric(),
      interval_am = numeric(),
      interval_pm = numeric()
    )
    return(list(
      qualifying_shapes = st_sf(empty_dt, geometry = st_sfc(crs = 4326)),
      qualification_summary = empty_dt
    ))
  }

  route_shapes_sf <- st_as_sf(route_shapes_sf)

  # Work in projected coordinates for segmentization and intersection
  target_crs <- 26916
  shapes_projected <- st_transform(route_shapes_sf, target_crs)

  if (requireNamespace("units", quietly = TRUE)) {
    df_max <- units::set_units(segment_length_meters, "m")
  } else {
    df_max <- segment_length_meters
  }

  segmented_projected <- st_segmentize(shapes_projected, dfMaxLength = df_max)
  segmented_projected <- st_cast(segmented_projected, "LINESTRING", warn = FALSE)
  segmented_projected <- st_make_valid(segmented_projected)

  process_direction_segments <- function(dir_segments, dir_value) {
    if (nrow(dir_segments) < 2) {
      return(list())
    }

    neighbor_index <- st_intersects(dir_segments, sparse = TRUE)
    overlaps <- list()
    overlap_id <- 1L

    for (i in seq_len(nrow(dir_segments))) {
      neighbors <- neighbor_index[[i]]
      neighbors <- neighbors[neighbors > i]
      if (length(neighbors) == 0) {
        next
      }

      for (j in neighbors) {
        inter_geom <- st_intersection(
          st_geometry(dir_segments[i, ]),
          st_geometry(dir_segments[j, ])
        )

        if (length(inter_geom) == 0) {
          next
        }

        inter_geom <- st_collection_extract(inter_geom, "LINESTRING")
        if (length(inter_geom) == 0) {
          next
        }

        for (piece in seq_along(inter_geom)) {
          geom_piece <- inter_geom[piece]
          if (isTRUE(st_is_empty(geom_piece))) {
            next
          }

          piece_sf <- st_sf(geometry = geom_piece)
          covering <- st_intersects(dir_segments, piece_sf, sparse = TRUE)[[1]]

          if (length(covering) < 2) {
            next
          }

          covering_dt <- unique(data.table(
            unique_route_id = dir_segments$unique_route_id[covering],
            agency = dir_segments$agency[covering]
          ))

          covering_routes <- sort(covering_dt$unique_route_id)
          covering_agencies <- sort(covering_dt$agency)

          lookup_info <- merge(
            covering_dt,
            route_direction_trips[direction_id == dir_value],
            by = c("unique_route_id", "agency"),
            all.x = TRUE
          )
          lookup_info[is.na(trips_am), trips_am := 0]
          lookup_info[is.na(trips_pm), trips_pm := 0]

          if (nrow(lookup_info) == 0) {
            next
          }

          trips_am_sum <- sum(lookup_info$trips_am, na.rm = TRUE)
          trips_pm_sum <- sum(lookup_info$trips_pm, na.rm = TRUE)

          overlaps[[overlap_id]] <- st_sf(
            agency = paste(covering_agencies, collapse = ";"),
            direction_id = dir_value,
            num_routes = length(covering_routes),
            routes = paste(covering_routes, collapse = ";"),
            trips_am = trips_am_sum,
            trips_pm = trips_pm_sum,
            geometry = geom_piece
          )
          overlap_id <- overlap_id + 1L
        }
      }
    }

    overlaps
  }

  direction_values <- sort(unique(segmented_projected$direction_id))
  overlap_segments <- list()
  overlap_idx <- 1L

  for (dir_val in direction_values) {
    dir_segments <- segmented_projected[segmented_projected$direction_id == dir_val, ]
    dir_overlaps <- process_direction_segments(dir_segments, dir_val)
    if (length(dir_overlaps) > 0) {
      for (seg in dir_overlaps) {
        overlap_segments[[overlap_idx]] <- seg
        overlap_idx <- overlap_idx + 1L
      }
    }
  }

  if (length(overlap_segments) == 0) {
    warning("No overlapping route segments identified")
    empty_dt <- data.table(
      agency = character(),
      direction_id = integer(),
      num_routes = integer(),
      routes = character(),
      trips_am = numeric(),
      trips_pm = numeric(),
      interval_am = numeric(),
      interval_pm = numeric()
    )
    return(list(
      qualifying_shapes = st_sf(empty_dt, geometry = st_sfc(crs = 4326)),
      qualification_summary = empty_dt
    ))
  }

  overlap_sf <- do.call(rbind, overlap_segments)
  overlap_sf <- overlap_sf[overlap_sf$num_routes > 1, ]

  # Merge contiguous segments that represent the same routes/directions
  overlap_sf$group_id <- paste(
    overlap_sf$agency,
    overlap_sf$direction_id,
    overlap_sf$routes,
    overlap_sf$num_routes,
    sep = "|"
  )

  grouped_segments <- split(seq_len(nrow(overlap_sf)), overlap_sf$group_id)

  overlap_sf <- do.call(rbind, lapply(grouped_segments, function(idx) {
    geom_union <- st_line_merge(st_union(st_geometry(overlap_sf[idx, ])))

    st_sf(
      agency = overlap_sf$agency[idx[1]],
      direction_id = overlap_sf$direction_id[idx[1]],
      num_routes = overlap_sf$num_routes[idx[1]],
      routes = overlap_sf$routes[idx[1]],
      trips_am = max(overlap_sf$trips_am[idx], na.rm = TRUE),
      trips_pm = max(overlap_sf$trips_pm[idx], na.rm = TRUE),
      geometry = geom_union
    )
  }))

  if (nrow(overlap_sf) == 0) {
    warning("Overlapping analysis produced only single-route segments")
    empty_dt <- data.table(
      agency = character(),
      direction_id = integer(),
      num_routes = integer(),
      routes = character(),
      trips_am = numeric(),
      trips_pm = numeric(),
      interval_am = numeric(),
      interval_pm = numeric()
    )
    return(list(
      qualifying_shapes = st_sf(empty_dt, geometry = st_sfc(crs = 4326)),
      qualification_summary = empty_dt
    ))
  }

  overlap_sf$interval_am <- ifelse(overlap_sf$trips_am > 0,
                                   120 / overlap_sf$trips_am, Inf)
  overlap_sf$interval_pm <- ifelse(overlap_sf$trips_pm > 0,
                                   120 / overlap_sf$trips_pm, Inf)

  qualifying_segments <- overlap_sf[
    overlap_sf$interval_am <= max_interval_minutes |
      overlap_sf$interval_pm <= max_interval_minutes,
  ]

  qualifying_segments <- st_transform(qualifying_segments, 4326)

  if (nrow(qualifying_segments) == 0) {
    warning("No overlapping segments met the peak interval threshold")
    empty_dt <- data.table(
      agency = character(),
      direction_id = integer(),
      num_routes = integer(),
      routes = character(),
      trips_am = numeric(),
      trips_pm = numeric(),
      interval_am = numeric(),
      interval_pm = numeric()
    )
    return(list(
      qualifying_shapes = st_sf(empty_dt, geometry = st_sfc(crs = 4326)),
      qualification_summary = empty_dt
    ))
  }

  qualification_summary <- data.table(
    agency = qualifying_segments$agency,
    direction_id = qualifying_segments$direction_id,
    num_routes = qualifying_segments$num_routes,
    routes = qualifying_segments$routes,
    trips_am = qualifying_segments$trips_am,
    trips_pm = qualifying_segments$trips_pm,
    interval_am = qualifying_segments$interval_am,
    interval_pm = qualifying_segments$interval_pm
  )

  return(list(
    qualifying_shapes = qualifying_segments,
    qualification_summary = qualification_summary
  ))
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
#' Identifies transit corridors based on direction-aware combined frequency
#' criteria. Routes on the same street that together meet the frequency threshold
#' will qualify even if they don't individually meet it.
#'
#' @param all_stops Combined stops data.table
#' @param am_peak_bus_stops AM peak bus stops data.table (with direction_id)
#' @param pm_peak_bus_stops PM peak bus stops data.table (with direction_id)
#' @param all_trips Combined trips data.table (to link routes to shapes)
#' @param all_shapes Combined shapes data.table (for route geometries)
#' @return List with:
#'   - qualifying_corridor_segments: sf object with qualifying corridor segments
#'   - qualification_summary: data.table with diagnostic information
#'
#' @details
#' This function implements direction-aware combined frequency calculation:
#' \enumerate{
#'   \item Calculates trip counts per route/direction/peak
#'   \item Converts GTFS shapes to LINESTRINGs and splits them into short segments
#'   \item Spatially intersects segments within the same direction to find shared roadway geometry
#'   \item Sums trips across overlapping routes for each direction/peak combination
#'   \item Qualifies if ANY direction in ANY peak has frequency ≤15 min
#' }
#'
#' @examples
#' \dontrun{
#' corridors <- identify_qualifying_corridors(
#'   all_stops, am_peak_bus_stops, pm_peak_bus_stops,
#'   all_trips, all_shapes
#' )
#' }
identify_qualifying_corridors <- function(all_stops, am_peak_bus_stops, pm_peak_bus_stops,
                                         all_trips, all_shapes) {
  cat("\n=== Identifying Transit Corridors (Direction-Aware Combined Frequency) ===\n\n")

  # Calculate route-level trip counts by direction and peak
  cat("Calculating route-level trip counts by direction and peak...\n")
  route_trips <- calculate_route_trip_counts(am_peak_bus_stops, pm_peak_bus_stops)

  cat(sprintf("Routes with trip counts: %d\n", nrow(route_trips)))
  cat(sprintf("  Total AM direction 0 trips: %s\n", format(sum(route_trips$trips_am_dir0), big.mark = ",")))
  cat(sprintf("  Total AM direction 1 trips: %s\n", format(sum(route_trips$trips_am_dir1), big.mark = ",")))
  cat(sprintf("  Total PM direction 0 trips: %s\n", format(sum(route_trips$trips_pm_dir0), big.mark = ",")))
  cat(sprintf("  Total PM direction 1 trips: %s\n", format(sum(route_trips$trips_pm_dir1), big.mark = ",")))

  # Identify overlapping segments with combined frequency
  overlap_results <- identify_overlapping_segments(route_trips, all_trips, all_shapes)

  qualifying_corridor_segments_sf <- overlap_results$qualifying_shapes
  qualification_summary <- overlap_results$qualification_summary

  cat(sprintf("\nTotal qualifying corridor segments: %d\n", nrow(qualifying_corridor_segments_sf)))

  return(list(
    qualifying_corridor_segments = qualifying_corridor_segments_sf,
    qualification_summary = qualification_summary
  ))
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

  # Check if shapes have geometry column (could be sf or data.table with geometry)
  has_geometry <- "geometry" %in% names(all_shapes)

  # Check if shapes are already LINESTRING geometries (from tidytransit)
  if (has_geometry) {
    # Convert to sf if needed for geometry type checking
    if (!inherits(all_shapes, "sf")) {
      all_shapes <- st_as_sf(all_shapes)
    }

    geom_types <- unique(as.character(st_geometry_type(all_shapes)))
    if ("LINESTRING" %in% geom_types || "MULTILINESTRING" %in% geom_types) {
      cat("Shapes already converted to LINESTRING by tidytransit\n")
      cat(sprintf("  %d LINESTRING geometries found\n", nrow(all_shapes)))

      # Ensure required columns exist
      if (!"num_points" %in% names(all_shapes)) {
        # Calculate num_points from geometry if not present
        all_shapes$num_points <- sapply(st_geometry(all_shapes), function(geom) {
          if (inherits(geom, "LINESTRING")) {
            nrow(st_coordinates(geom))
          } else {
            NA_integer_
          }
        })
      }

      return(all_shapes)
    }
  }

  # Fallback: Manual conversion from point data (original approach)
  cat("Converting shapes from point data to LINESTRING...\n")

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

#' Create Corridor Buffers from Qualifying Segments
#'
#' Creates 1/8 mile buffers around the **qualifying street segments** returned by
#' `identify_overlapping_segments()`. Only the overlapping portions that meet
#' the direction-aware, combined-frequency test are buffered, preventing the
#' inflation that occurs when entire route shapes are buffered after a single
#' qualifying overlap.
#'
#' @param qualifying_corridor_segments_sf sf object with qualifying corridor
#'   segments (output of identify_overlapping_segments)
#' @param illinois_boundary sf object with Illinois state boundary
#' @return sf object with buffered corridor geometry (WGS84)
#'
#' @details
#' This function:
#' \enumerate{
#'   \item Takes only the street segments that satisfied the corridor frequency test
#'   \item Buffers each segment by 680 feet (660ft + 20ft for street width)
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
#'   qualifying_corridor_segments_sf,
#'   illinois_boundary
#' )
#' }
create_corridor_buffers <- function(qualifying_corridor_segments_sf, illinois_boundary) {
  cat("\n=== Creating Corridor Buffers from Qualifying Segments ===\n\n")

  if (!inherits(qualifying_corridor_segments_sf, "sf")) {
    stop("qualifying_corridor_segments_sf must be an sf object of qualifying segments")
  }

  # Keep only non-empty geometries and ensure we buffer just the qualifying segments
  qualifying_corridor_segments_sf <- qualifying_corridor_segments_sf[!st_is_empty(qualifying_corridor_segments_sf), ]

  if (nrow(qualifying_corridor_segments_sf) == 0) {
    warning("No qualifying corridor segments available to buffer")
    return(st_sf(geometry = st_sfc(crs = st_crs(4326))))
  }

  cat(sprintf("Qualifying corridor segments to buffer: %d\n", nrow(qualifying_corridor_segments_sf)))

  # Standardize to LINESTRING-only geometries so buffering is limited to the
  # qualifying street pieces
  qualifying_corridor_segments_sf <- st_collection_extract(qualifying_corridor_segments_sf, "LINESTRING", warn = FALSE)
  qualifying_corridor_segments_sf <- st_cast(qualifying_corridor_segments_sf, "LINESTRING", warn = FALSE)
  qualifying_corridor_segments_sf <- st_make_valid(qualifying_corridor_segments_sf)

  # Project to Illinois State Plane (feet) for accurate buffering
  cat("Buffering route geometries by 1/8 mile from street edge...\n")
  shapes_projected <- st_transform(qualifying_corridor_segments_sf, 3435)

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
