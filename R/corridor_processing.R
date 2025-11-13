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

#' Build Directional Shape Metrics
#'
#' Converts route-level peak trip counts into shape-level metrics that retain
#' direction information. Each row represents a route/shape/direction
#' combination with associated AM/PM trip totals.
#'
#' @param route_trips data.table from calculate_route_trip_counts()
#' @param all_trips Combined trips data.table (contains unique_shape_id, direction_id)
#' @param all_shapes Combined shapes data.table
#' @return sf object with columns:
#'   unique_shape_id, unique_route_id, agency, direction_key, trips_am, trips_pm, geometry
build_directional_shape_metrics <- function(route_trips, all_trips, all_shapes) {
  # Link routes to their shapes with direction information
  route_shapes_link <- unique(all_trips[
    unique_route_id %in% route_trips$unique_route_id,
    .(unique_route_id, unique_shape_id, direction_id, agency)
  ])
  route_shapes_link <- route_shapes_link[!is.na(unique_shape_id) & unique_shape_id != ""]
  route_shapes_link[, direction_key := fifelse(is.na(direction_id), -1L, as.integer(direction_id))]

  # Merge route-level trip counts
  routes_with_counts <- merge(
    route_shapes_link,
    route_trips,
    by = c("unique_route_id", "agency"),
    allow.cartesian = TRUE
  )

  # Derive directional trip totals
  routes_with_counts[, trips_am := fifelse(
    direction_key == 1L, trips_am_dir1,
    fifelse(direction_key == 0L, trips_am_dir0,
            trips_am_dir0 + trips_am_dir1)
  )]
  routes_with_counts[, trips_pm := fifelse(
    direction_key == 1L, trips_pm_dir1,
    fifelse(direction_key == 0L, trips_pm_dir0,
            trips_pm_dir0 + trips_pm_dir1)
  )]

  # Drop combinations with no service in either peak
  routes_with_counts <- routes_with_counts[(trips_am > 0) | (trips_pm > 0)]

  if (nrow(routes_with_counts) == 0) {
    empty_dt <- data.table(
      unique_shape_id = character(),
      agency = character(),
      unique_route_id = character(),
      direction_key = integer(),
      trips_am = numeric(),
      trips_pm = numeric()
    )
    empty_sf <- st_sf(empty_dt, geometry = st_sfc(crs = 4326))
    return(empty_sf)
  }

  # Attach geometry
  shape_ids <- unique(routes_with_counts$unique_shape_id)
  shapes_sf <- convert_shapes_to_linestrings(all_shapes[unique_shape_id %in% shape_ids])

  directional_metrics <- merge(
    shapes_sf,
    routes_with_counts[, .(
      unique_shape_id,
      agency,
      unique_route_id,
      direction_key,
      trips_am,
      trips_pm
    )],
    by = c("unique_shape_id", "agency"),
    allow.cartesian = TRUE
  )

  return(directional_metrics)
}

#' Segment Shapes Into Directional Corridor Sections
#'
#' Splits shape geometries into minimal line segments based on overlaps between
#' routes traveling in the same direction. Calculates combined AM/PM trip totals
#' for each segment by summing contributing routes (deduplicated by
#' route/direction).
#'
#' @param directional_shapes sf object from build_directional_shape_metrics()
#' @param coverage_tolerance Numeric between 0 and 1 indicating the minimum
#'   proportion of a segment that must be covered by a route to be considered
#'   present on that segment (default: 0.9).
#' @param min_segment_length_ft Minimum segment length (feet) to keep after
#'   segmentation (default: 30 feet) to avoid sliver geometries.
#' @param simplify_tolerance_ft Numeric tolerance (feet) for optional geometry
#'   simplification prior to segmentation (default: 5). Set to 0 to disable.
#' @return List with elements:
#'   - segments_sf: sf object with one row per directional segment and aggregated metrics
#'   - segments_dt: data.table containing the same attributes (without geometry)
segment_directional_shapes <- function(directional_shapes,
                                       coverage_tolerance = 0.9,
                                       min_segment_length_ft = 30,
                                       simplify_tolerance_ft = 5) {
  if (nrow(directional_shapes) == 0) {
    empty_dt <- data.table(
      segment_id = character(),
      agency = character(),
      direction_key = integer(),
      num_routes = integer(),
      routes = character(),
      shape_ids = character(),
      trips_am_dir0 = numeric(),
      trips_am_dir1 = numeric(),
      trips_pm_dir0 = numeric(),
      trips_pm_dir1 = numeric(),
      interval_am_dir0 = numeric(),
      interval_am_dir1 = numeric(),
      interval_pm_dir0 = numeric(),
      interval_pm_dir1 = numeric(),
      segment_length_ft = numeric()
    )
    empty_sf <- st_sf(empty_dt, geometry = st_sfc(crs = 4326))
    return(list(segments_sf = empty_sf, segments_dt = empty_dt))
  }

  directional_dt <- as.data.table(directional_shapes)
  directional_dt[, direction_key := as.integer(direction_key)]

  # Ensure geometries are valid before segmentation
  directional_shapes <- st_make_valid(directional_shapes)

  segment_records <- list()
  geometry_records <- list()
  seg_index <- 1L

  combos <- unique(directional_dt[, .(agency, direction_key)])
  setorder(combos, agency, direction_key)

  for (combo_idx in seq_len(nrow(combos))) {
    agency_id <- combos$agency[combo_idx]
    direction_id <- combos$direction_key[combo_idx]

    subset_shapes <- directional_shapes[
      directional_dt$agency == agency_id &
        directional_dt$direction_key == direction_id, ]

    if (nrow(subset_shapes) == 0) {
      next
    }

    # Transform to Illinois State Plane (feet) for precise segmentation
    subset_proj <- st_transform(subset_shapes, 3435)

    if (!is.null(simplify_tolerance_ft) && simplify_tolerance_ft > 0) {
      subset_proj$geometry <- st_simplify(
        subset_proj$geometry,
        dTolerance = simplify_tolerance_ft,
        preserveTopology = TRUE
      )
    }

    subset_proj$row_id <- seq_len(nrow(subset_proj))

    intersections <- st_intersects(subset_proj, sparse = TRUE)
    isolated_idx <- which(lengths(intersections) <= 1)
    overlap_idx <- setdiff(seq_len(nrow(subset_proj)), isolated_idx)

    if (length(isolated_idx) > 0) {
      iso_subset <- subset_proj[isolated_idx, ]
      iso_dt <- as.data.table(iso_subset)
      iso_dt[, row_id := .I]

      iso_summary <- iso_dt[, .(
        sample_row = first(row_id),
        total_trips_am = sum(trips_am),
        total_trips_pm = sum(trips_pm),
        num_routes = uniqueN(unique_route_id),
        routes = paste(sort(unique(unique_route_id)), collapse = ";"),
        shape_ids = paste(sort(unique(unique_shape_id)), collapse = ";")
      ), by = .(unique_shape_id, direction_key)]

      for (iso_idx in seq_len(nrow(iso_summary))) {
        sample_row <- iso_summary$sample_row[iso_idx]
        shape_row <- iso_subset[sample_row, ]
        seg_length <- as.numeric(st_length(shape_row))
        if (is.na(seg_length) || seg_length <= 0 || seg_length < min_segment_length_ft) {
          next
        }

        total_routes <- iso_summary$num_routes[iso_idx]
        route_list <- iso_summary$routes[iso_idx]
        shape_list <- iso_summary$shape_ids[iso_idx]
        total_trips_am <- iso_summary$total_trips_am[iso_idx]
        total_trips_pm <- iso_summary$total_trips_pm[iso_idx]
        dir_id <- iso_summary$direction_key[iso_idx]

        trips_am_dir0 <- if (dir_id == 1L) 0 else total_trips_am
        trips_am_dir1 <- if (dir_id == 1L) total_trips_am else 0
        trips_pm_dir0 <- if (dir_id == 1L) 0 else total_trips_pm
        trips_pm_dir1 <- if (dir_id == 1L) total_trips_pm else 0

        interval_am_dir0 <- if (trips_am_dir0 > 0) 120 / trips_am_dir0 else Inf
        interval_am_dir1 <- if (trips_am_dir1 > 0) 120 / trips_am_dir1 else Inf
        interval_pm_dir0 <- if (trips_pm_dir0 > 0) 120 / trips_pm_dir0 else Inf
        interval_pm_dir1 <- if (trips_pm_dir1 > 0) 120 / trips_pm_dir1 else Inf

        segment_id <- sprintf("%s_dir%s_%05d", agency_id, dir_id, seg_index)
        seg_index <- seg_index + 1L

        segment_records[[length(segment_records) + 1L]] <- data.table(
          segment_id = segment_id,
          agency = agency_id,
          direction_key = dir_id,
          num_routes = total_routes,
          routes = route_list,
          shape_ids = shape_list,
          trips_am_dir0 = trips_am_dir0,
          trips_am_dir1 = trips_am_dir1,
          trips_pm_dir0 = trips_pm_dir0,
          trips_pm_dir1 = trips_pm_dir1,
          interval_am_dir0 = interval_am_dir0,
          interval_am_dir1 = interval_am_dir1,
          interval_pm_dir0 = interval_pm_dir0,
          interval_pm_dir1 = interval_pm_dir1,
          segment_length_ft = seg_length
        )

        segment_geom_wgs84 <- st_transform(shape_row, 4326)
        geometry_records[[length(geometry_records) + 1L]] <- segment_geom_wgs84$geometry[[1]]
      }
    }

    if (length(overlap_idx) == 0) {
      next
    }

    subset_proj <- subset_proj[overlap_idx, ]
    subset_proj$row_id <- seq_len(nrow(subset_proj))

    # Node the combined geometry to break at intersections/overlaps
    combined_geom <- suppressWarnings(st_union(subset_proj$geometry))
    if (is.null(combined_geom) || length(combined_geom) == 0) {
      next
    }

    noded_geom <- tryCatch({
      lwgeom::st_node(combined_geom)
    }, error = function(e) {
      # If st_node fails (e.g., invalid geometries), fall back to st_union result
      combined_geom
    })

    segment_lines <- st_collection_extract(noded_geom, "LINESTRING")
    if (length(segment_lines) == 0) {
      next
    }

    segment_sf <- st_sf(
      data.frame(segment_tmp_id = seq_len(length(segment_lines))),
      geometry = segment_lines,
      crs = st_crs(subset_proj)
    )

    # Drop empty or tiny sliver segments
    segment_lengths <- as.numeric(st_length(segment_sf))
    keep_idx <- which(!is.na(segment_lengths) & segment_lengths > 0 &
                        segment_lengths >= min_segment_length_ft)
    if (length(keep_idx) == 0) {
      next
    }
    segment_sf <- segment_sf[keep_idx, ]
    segment_lengths <- segment_lengths[keep_idx]

    # Identify which shapes cover each segment
    coverage_matrix <- st_intersects(segment_sf, subset_proj, sparse = TRUE)

    for (seg_pos in seq_len(nrow(segment_sf))) {
      candidate_idx <- coverage_matrix[[seg_pos]]
      if (length(candidate_idx) == 0) {
        next
      }

      seg_geom_proj <- segment_sf[seg_pos, ]
      seg_length <- segment_lengths[seg_pos]
      if (seg_length <= 0) {
        next
      }

      # Filter shapes that actually cover the majority of the segment
      coverage_details <- lapply(candidate_idx, function(idx) {
        shape_geom <- subset_proj[idx, ]
        intersection_geom <- suppressWarnings(st_intersection(seg_geom_proj, shape_geom))
        intersection_length <- as.numeric(st_length(intersection_geom))
        coverage_ratio <- if (seg_length > 0) intersection_length / seg_length else 0
        list(idx = idx, coverage = coverage_ratio)
      })

      coverage_ratios <- sapply(coverage_details, function(x) x$coverage)
      valid_idx <- vapply(coverage_details, function(x) {
        x$coverage >= coverage_tolerance
      }, logical(1))

      if (!any(valid_idx)) {
        # Retain the shape with the highest coverage if none pass tolerance
        max_idx <- which.max(coverage_ratios)
        valid_idx <- rep(FALSE, length(candidate_idx))
        valid_idx[max_idx] <- TRUE
      }

      selected_rows <- vapply(coverage_details[valid_idx], function(x) x$idx, integer(1))
      covering_shapes <- subset_proj[selected_rows, ]
      covering_dt <- as.data.table(covering_shapes)

      if (nrow(covering_dt) == 0) {
        next
      }

      # Deduplicate by route/direction to avoid double counting variants
      covering_unique <- unique(covering_dt[, .(
        unique_route_id,
        direction_key,
        trips_am,
        trips_pm
      )], by = c("unique_route_id", "direction_key"))

      shape_list <- paste(sort(unique(covering_dt$unique_shape_id)), collapse = ";")

      total_trips_am <- sum(covering_unique$trips_am)
      total_trips_pm <- sum(covering_unique$trips_pm)
      total_routes <- uniqueN(covering_unique$unique_route_id)
      route_list <- paste(sort(unique(covering_unique$unique_route_id)), collapse = ";")

      # Populate direction-aware columns
      trips_am_dir0 <- if (direction_id == 1L) 0 else total_trips_am
      trips_am_dir1 <- if (direction_id == 1L) total_trips_am else 0
      trips_pm_dir0 <- if (direction_id == 1L) 0 else total_trips_pm
      trips_pm_dir1 <- if (direction_id == 1L) total_trips_pm else 0

      interval_am_dir0 <- if (trips_am_dir0 > 0) 120 / trips_am_dir0 else Inf
      interval_am_dir1 <- if (trips_am_dir1 > 0) 120 / trips_am_dir1 else Inf
      interval_pm_dir0 <- if (trips_pm_dir0 > 0) 120 / trips_pm_dir0 else Inf
      interval_pm_dir1 <- if (trips_pm_dir1 > 0) 120 / trips_pm_dir1 else Inf

      segment_id <- sprintf("%s_dir%s_%05d", agency_id, direction_id, seg_index)
      seg_index <- seg_index + 1L

      segment_records[[length(segment_records) + 1L]] <- data.table(
        segment_id = segment_id,
        agency = agency_id,
        direction_key = direction_id,
        num_routes = total_routes,
        routes = route_list,
        shape_ids = shape_list,
        trips_am_dir0 = trips_am_dir0,
        trips_am_dir1 = trips_am_dir1,
        trips_pm_dir0 = trips_pm_dir0,
        trips_pm_dir1 = trips_pm_dir1,
        interval_am_dir0 = interval_am_dir0,
        interval_am_dir1 = interval_am_dir1,
        interval_pm_dir0 = interval_pm_dir0,
        interval_pm_dir1 = interval_pm_dir1,
        segment_length_ft = seg_length
      )

      segment_geom_wgs84 <- st_transform(seg_geom_proj, 4326)
      geometry_records[[length(geometry_records) + 1L]] <- segment_geom_wgs84$geometry[[1]]
    }
  }

  if (length(segment_records) == 0) {
    empty_dt <- data.table(
      segment_id = character(),
      agency = character(),
      direction_key = integer(),
      num_routes = integer(),
      routes = character(),
      shape_ids = character(),
      trips_am_dir0 = numeric(),
      trips_am_dir1 = numeric(),
      trips_pm_dir0 = numeric(),
      trips_pm_dir1 = numeric(),
      interval_am_dir0 = numeric(),
      interval_am_dir1 = numeric(),
      interval_pm_dir0 = numeric(),
      interval_pm_dir1 = numeric(),
      segment_length_ft = numeric()
    )
    empty_sf <- st_sf(empty_dt, geometry = st_sfc(crs = 4326))
    return(list(segments_sf = empty_sf, segments_dt = empty_dt))
  }

  segments_dt <- rbindlist(segment_records)
  segments_sf <- st_sf(
    as.data.frame(segments_dt),
    geometry = st_sfc(geometry_records, crs = 4326)
  )

  return(list(segments_sf = segments_sf, segments_dt = segments_dt))
}

#' Identify Overlapping Route Segments with Combined Frequency
#'
#' Finds where bus route geometries overlap spatially and calculates combined
#' frequencies by direction. Routes on the same street qualify together if their
#' combined frequency (summing trips across routes) meets the threshold.
#'
#' @param route_trips data.table with trip counts (from calculate_route_trip_counts)
#' @param all_trips data.table with all trips (to get unique_shape_id)
#' @param all_shapes data.table with GTFS shapes
#' @param max_interval_minutes Numeric. Maximum service interval (default: 15)
#' @param coverage_tolerance Proportion of a segment that must be shared to
#'   count a route in the combined frequency (default: 0.9)
#' @param min_segment_length_ft Minimum segment length (feet) to retain after
#'   segmentation (default: 30)
#' @param simplify_tolerance_ft Geometry simplification tolerance in feet
#'   applied before segmentation (default: 5). Set to 0 to disable.
#' @return List with:
#'   - qualifying_shapes: sf object with shapes that qualify (either individually or as overlaps)
#'   - qualification_summary: data.table with diagnostic info
identify_overlapping_segments <- function(route_trips, all_trips, all_shapes,
                                          max_interval_minutes = 15,
                                          coverage_tolerance = 0.9,
                                          min_segment_length_ft = 30,
                                          simplify_tolerance_ft = 5) {
  cat("\n=== Identifying Overlapping Route Segments ===\n\n")

  # Build route/shape/direction metrics
  directional_shapes <- build_directional_shape_metrics(route_trips, all_trips, all_shapes)
  cat(sprintf("Directional route-shape combinations: %d\n", nrow(directional_shapes)))

  # Segment overlaps and aggregate combined frequencies
  segment_results <- segment_directional_shapes(
    directional_shapes,
    coverage_tolerance = coverage_tolerance,
    min_segment_length_ft = min_segment_length_ft,
    simplify_tolerance_ft = simplify_tolerance_ft
  )
  segments_sf <- segment_results$segments_sf
  segments_dt <- segment_results$segments_dt

  if (nrow(segments_dt) == 0) {
    cat("No qualifying corridor segments identified.\n")
    return(list(
      qualifying_shapes = segments_sf[0, ],
      qualification_summary = segments_dt[0]
    ))
  }

  # Apply qualification threshold
  segments_dt[, qualifies := (interval_am_dir0 <= max_interval_minutes) |
                               (interval_am_dir1 <= max_interval_minutes) |
                               (interval_pm_dir0 <= max_interval_minutes) |
                               (interval_pm_dir1 <= max_interval_minutes)]
  segments_sf$qualifies <- segments_dt$qualifies

  qualifying_shapes_sf <- segments_sf[segments_sf$qualifies, ]
  qualification_summary <- segments_dt[qualifies == TRUE,
                                       .(
                                         segment_id,
                                         agency,
                                         direction_key,
                                         num_routes,
                                         routes,
                                         shape_ids,
                                         trips_am_dir0,
                                         trips_am_dir1,
                                         trips_pm_dir0,
                                         trips_pm_dir1,
                                         interval_am_dir0,
                                         interval_am_dir1,
                                         interval_pm_dir0,
                                         interval_pm_dir1,
                                         segment_length_ft
                                       )]

  cat(sprintf("Qualifying corridor segments: %d (out of %d total)\n",
              nrow(qualifying_shapes_sf), nrow(segments_sf)))
  cat(sprintf("  Single-route segments: %d\n", sum(qualification_summary$num_routes == 1)))
  cat(sprintf("  Multi-route segments: %d\n", sum(qualification_summary$num_routes > 1)))

  return(list(
    qualifying_shapes = qualifying_shapes_sf,
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
#'   - qualifying_corridor_shapes: sf object with qualifying route shape geometries
#'   - qualification_summary: data.table with diagnostic information
#'
#' @details
#' This function implements direction-aware combined frequency calculation:
#' \enumerate{
#'   \item Calculates trip counts per route/direction/peak
#'   \item Groups routes by shape (routes on same street)
#'   \item Sums trips across routes for each direction/peak combination
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

  qualifying_corridor_shapes_sf <- overlap_results$qualifying_shapes
  qualification_summary <- overlap_results$qualification_summary

  cat(sprintf("\nTotal qualifying corridor shapes: %d\n", nrow(qualifying_corridor_shapes_sf)))

  return(list(
    qualifying_corridor_shapes = qualifying_corridor_shapes_sf,
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
#' This approach buffers only routes that meet direction-aware combined frequency
#' criteria.
#'
#' @param qualifying_corridor_shapes_sf sf object with qualifying corridor route shapes
#' @param illinois_boundary sf object with Illinois state boundary
#' @return sf object with buffered corridor geometry (WGS84)
#'
#' @details
#' This function:
#' \enumerate{
#'   \item Takes qualifying route shapes (already filtered by combined frequency)
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
#'   qualifying_corridor_shapes_sf,
#'   illinois_boundary
#' )
#' }
create_corridor_buffers <- function(qualifying_corridor_shapes_sf, illinois_boundary) {
  cat("\n=== Creating Corridor Buffers from Qualifying Shapes ===\n\n")

  cat(sprintf("Qualifying corridor shapes to buffer: %d\n", nrow(qualifying_corridor_shapes_sf)))

  # Project to Illinois State Plane (feet) for accurate buffering
  cat("Buffering route geometries by 1/8 mile from street edge...\n")
  shapes_projected <- st_transform(qualifying_corridor_shapes_sf, 3435)

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
