# Corridor Processing Functions
#
# Functions for identifying qualifying transit corridors and creating
# corridor buffers along actual transit route geometry.
#
# UPDATED: Now uses edge-based corridor identification (from SMTD V3 methodology)
# and processes agencies separately for performance (except PACE+CTA which overlap).
#
# High-Level Functions:
#   - identify_qualifying_corridors(): Complete corridor qualification workflow
#   - identify_corridors_for_agency_group(): Process corridors for an agency group
#   - convert_shapes_to_linestrings(): Convert GTFS shapes to route geometry
#   - create_corridor_buffers(): Create buffered corridor geometries
#
# Helper Functions:
#   - build_corridor_edges(): Build edges between consecutive stop clusters
#   - calculate_edge_frequency(): Calculate frequency for each edge
#   - extract_shape_segment(): Extract shape geometry for an edge

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


# =============================================================================
# Edge-Based Corridor Identification (SMTD V3 Methodology)
# =============================================================================
# This approach builds corridors by:
# 1. Clustering stops spatially
# 2. Building edges between consecutive stop clusters along each trip
# 3. Aggregating trips by edge (from_cluster, to_cluster, direction_id)
# 4. Using shape geometry to create accurate edge linestrings
# =============================================================================

#' Build Corridor Edges from Peak Stop Times
#'
#' Creates edges between consecutive stop clusters along each trip.
#' This forms the basis for corridor identification.
#'
#' @param peak_stop_times data.table with columns: unique_trip_id, unique_stop_id,
#'   stop_sequence, cluster_id, unique_route_id, direction_id, unique_shape_id
#'
#' @return data.table with columns: from_cluster, to_cluster, unique_trip_id,
#'   unique_route_id, direction_id, unique_shape_id, agency
#'
#' @details
#' For each trip, creates an edge between consecutive stop clusters.
#' Edges where from_cluster == to_cluster (same stop) are excluded.
#'
#' @export
build_corridor_edges <- function(peak_stop_times) {
  # Ensure sorted by trip and sequence
  setorder(peak_stop_times, unique_trip_id, stop_sequence)
  
  # Build edges for each trip
  edges <- peak_stop_times[, {
    if (.N < 2) {
      NULL
    } else {
      prev_cluster <- shift(cluster_id, 1, type = "lag")
      valid <- !is.na(prev_cluster) & prev_cluster != cluster_id
      
      if (sum(valid) == 0) {
        NULL
      } else {
        .(
          from_cluster = prev_cluster[valid],
          to_cluster = cluster_id[valid],
          unique_trip_id = unique_trip_id[valid],
          unique_route_id = unique_route_id[valid],
          direction_id = direction_id[valid],
          unique_shape_id = unique_shape_id[valid],
          agency = agency[valid]
        )
      }
    }
  }, by = unique_trip_id]
  
  return(edges)
}


#' Aggregate Edge Trip Counts
#'
#' Aggregates trip counts for each unique edge (from_cluster, to_cluster, direction).
#'
#' @param edges data.table from build_corridor_edges()
#'
#' @return data.table with columns: from_cluster, to_cluster, direction_id, agency,
#'   trips, routes (list), num_routes, shapes (list)
#'
#' @export
aggregate_edge_trips <- function(edges) {
  edge_summary <- edges[, .(
    trips = uniqueN(unique_trip_id),
    routes = list(unique(unique_route_id)),
    num_routes = uniqueN(unique_route_id),
    shapes = list(unique(unique_shape_id))
  ), by = .(from_cluster, to_cluster, direction_id, agency)]
  
  return(edge_summary)
}


#' Calculate Edge Corridor Metrics
#'
#' Calculates AM/PM frequency metrics for corridor edges and determines
#' which edges qualify based on combined frequency.
#'
#' @param am_edges data.table of aggregated AM peak edges
#' @param pm_edges data.table of aggregated PM peak edges
#' @param max_interval_minutes Numeric. Maximum interval to qualify (default: 15)
#'
#' @return data.table with combined metrics and qualification status
#'
#' @export
calculate_edge_corridor_metrics <- function(am_edges, pm_edges, max_interval_minutes = 15) {
  
  # Handle empty AM edges
  if (nrow(am_edges) == 0) {
    am_summary <- data.table(
      from_cluster = integer(0),
      to_cluster = integer(0),
      direction_id = integer(0),
      agency = character(0),
      trips_am = integer(0),
      routes_am = list(),
      num_routes_am = integer(0),
      shapes_am = list()
    )
  } else {
    am_summary <- copy(am_edges)
    setnames(am_summary,
             c("trips", "routes", "num_routes", "shapes"),
             c("trips_am", "routes_am", "num_routes_am", "shapes_am"),
             skip_absent = TRUE)
  }

  # Handle empty PM edges
  if (nrow(pm_edges) == 0) {
    pm_summary <- data.table(
      from_cluster = integer(0),
      to_cluster = integer(0),
      direction_id = integer(0),
      agency = character(0),
      trips_pm = integer(0),
      routes_pm = list(),
      num_routes_pm = integer(0),
      shapes_pm = list()
    )
  } else {
    pm_summary <- copy(pm_edges)
    setnames(pm_summary,
             c("trips", "routes", "num_routes", "shapes"),
             c("trips_pm", "routes_pm", "num_routes_pm", "shapes_pm"),
             skip_absent = TRUE)
  }
  
  # Merge AM and PM
  edge_summary <- merge(
    am_summary, pm_summary,
    by = c("from_cluster", "to_cluster", "direction_id", "agency"),
    all = TRUE
  )
  
  # Fill NAs
  edge_summary[is.na(trips_am), trips_am := 0]
  edge_summary[is.na(trips_pm), trips_pm := 0]
  edge_summary[is.na(num_routes_am), num_routes_am := 0]
  edge_summary[is.na(num_routes_pm), num_routes_pm := 0]
  
  # Calculate intervals (120 min peak period)
  edge_summary[, interval_am := fifelse(trips_am > 0, 120 / trips_am, Inf)]
  edge_summary[, interval_pm := fifelse(trips_pm > 0, 120 / trips_pm, Inf)]
  
  # Combined route count (max of AM or PM)
  edge_summary[, num_routes := pmax(num_routes_am, num_routes_pm, na.rm = TRUE)]
  
  # Qualification: frequency <= threshold in EITHER AM or PM
  edge_summary[, qualifies := interval_am <= max_interval_minutes | interval_pm <= max_interval_minutes]
  
  return(edge_summary)
}


#' Extract Shape Segment for an Edge
#'
#' Extracts the portion of a GTFS shape geometry that corresponds to
#' the edge between two stop clusters.
#'
#' @param shape_geom sf geometry (LINESTRING) of the full shape
#' @param from_centroid numeric vector c(lon, lat) of from cluster centroid
#' @param to_centroid numeric vector c(lon, lat) of to cluster centroid
#'
#' @return sf LINESTRING geometry for the edge segment, or NULL if extraction fails
#'
#' @details
#' Uses nearest point projection to find where each cluster centroid
#' projects onto the shape, then extracts the segment between those points.
#'
#' @keywords internal
extract_shape_segment <- function(shape_geom, from_centroid, to_centroid) {
  tryCatch({
    shape_coords <- st_coordinates(shape_geom)
    if (nrow(shape_coords) < 2) return(NULL)
    
    # Find nearest points on shape to each centroid
    dists_from <- sqrt((shape_coords[, 1] - from_centroid[1])^2 + 
                       (shape_coords[, 2] - from_centroid[2])^2)
    dists_to <- sqrt((shape_coords[, 1] - to_centroid[1])^2 + 
                     (shape_coords[, 2] - to_centroid[2])^2)
    
    idx_from <- which.min(dists_from)
    idx_to <- which.min(dists_to)
    
    # Extract segment (in correct order along shape)
    if (idx_from < idx_to) {
      segment_coords <- shape_coords[idx_from:idx_to, 1:2, drop = FALSE]
    } else {
      segment_coords <- shape_coords[idx_to:idx_from, 1:2, drop = FALSE]
    }
    
    if (nrow(segment_coords) < 2) return(NULL)
    
    st_linestring(segment_coords)
  }, error = function(e) {
    NULL
  })
}


#' Create Edge Geometries from Shape Data
#'
#' Creates LINESTRING geometries for corridor edges using GTFS shape data.
#' Falls back to straight lines between cluster centroids if shape extraction fails.
#'
#' @param edge_summary data.table with edge metrics (must have from_cluster, to_cluster, shapes_am, shapes_pm)
#' @param shapes_sf sf object with GTFS shapes (LINESTRING geometries)
#' @param cluster_centroids data.table with cluster_id, centroid_lon, centroid_lat
#'
#' @return sf object with edge geometries and metrics
#'
#' @export
create_edge_geometries <- function(edge_summary, shapes_sf, cluster_centroids) {
  
  geoms <- vector("list", nrow(edge_summary))
  geom_types <- character(nrow(edge_summary))
  
  for (i in seq_len(nrow(edge_summary))) {
    row <- edge_summary[i]
    from_cl <- row$from_cluster
    to_cl <- row$to_cluster
    
    # Get cluster centroids
    from_coords <- cluster_centroids[cluster_id == from_cl]
    to_coords <- cluster_centroids[cluster_id == to_cl]
    
    if (nrow(from_coords) == 0 || nrow(to_coords) == 0) {
      geoms[[i]] <- NULL
      next
    }
    
    from_centroid <- c(from_coords$centroid_lon, from_coords$centroid_lat)
    to_centroid <- c(to_coords$centroid_lon, to_coords$centroid_lat)
    
    # Try to extract segment from shapes
    shape_ids <- unique(c(unlist(row$shapes_am), unlist(row$shapes_pm)))
    shape_ids <- shape_ids[!is.na(shape_ids)]
    
    segment <- NULL
    if (length(shape_ids) > 0 && !is.null(shapes_sf) && nrow(shapes_sf) > 0) {
      for (sid in shape_ids) {
        shape_row <- shapes_sf[shapes_sf$unique_shape_id == sid, ]
        if (nrow(shape_row) > 0) {
          segment <- extract_shape_segment(
            st_geometry(shape_row)[[1]],
            from_centroid, to_centroid
          )
          if (!is.null(segment)) {
            geom_types[i] <- "shape"
            break
          }
        }
      }
    }
    
    # Fall back to straight line if shape extraction failed
    if (is.null(segment)) {
      segment <- st_linestring(matrix(c(
        from_centroid[1], from_centroid[2],
        to_centroid[1], to_centroid[2]
      ), ncol = 2, byrow = TRUE))
      geom_types[i] <- "straight"
    }
    
    geoms[[i]] <- segment
  }
  
  # Filter out NULL geometries
  valid <- !sapply(geoms, is.null)
  
  if (sum(valid) == 0) {
    warning("No valid edge geometries created")
    return(st_sf(geometry = st_sfc(crs = 4326)))
  }
  
  # Create sf object
  result_sf <- st_sf(
    from_cluster = edge_summary$from_cluster[valid],
    to_cluster = edge_summary$to_cluster[valid],
    direction_id = edge_summary$direction_id[valid],
    agency = edge_summary$agency[valid],
    trips_am = edge_summary$trips_am[valid],
    trips_pm = edge_summary$trips_pm[valid],
    interval_am = edge_summary$interval_am[valid],
    interval_pm = edge_summary$interval_pm[valid],
    num_routes = edge_summary$num_routes[valid],
    qualifies = edge_summary$qualifies[valid],
    geom_type = geom_types[valid],
    geometry = st_sfc(geoms[valid], crs = 4326)
  )
  
  return(result_sf)
}


#' Identify Corridors for an Agency Group
#'
#' Identifies qualifying transit corridors for a specific group of agencies
#' using the edge-based methodology.
#'
#' @param agency_stops Stops data.table for the agency group
#' @param agency_am_peak AM peak stop times for the agency group
#' @param agency_pm_peak PM peak stop times for the agency group
#' @param agency_trips Trips data.table for the agency group
#' @param agency_shapes Shapes data.table for the agency group
#' @param cluster_radius_ft Numeric. Radius for stop clustering (default: 150)
#' @param max_interval_minutes Numeric. Maximum interval to qualify (default: 15)
#'
#' @return List with:
#'   \itemize{
#'     \item qualifying_edges_sf: sf object with qualifying corridor edges
#'     \item all_edges_sf: sf object with all edges (for debugging)
#'     \item edge_summary: data.table with edge metrics
#'     \item cluster_centroids: data.table with cluster centroids
#'   }
#'
#' @export
identify_corridors_for_agency_group <- function(agency_stops,
                                                 agency_am_peak,
                                                 agency_pm_peak,
                                                 agency_trips,
                                                 agency_shapes,
                                                 cluster_radius_ft = 150,
                                                 max_interval_minutes = 15) {
  
  # Get unique agencies in this group
  agencies_in_group <- unique(c(agency_am_peak$agency, agency_pm_peak$agency))
  cat(sprintf("\n--- Processing agency group: %s ---\n", paste(agencies_in_group, collapse = ", ")))
  
  # Step 1: Cluster stops spatially
  cat("Clustering stops...\n")
  stops_for_clustering <- agency_stops[
    unique_stop_id %in% unique(c(agency_am_peak$unique_stop_id, agency_pm_peak$unique_stop_id))
  ]
  
  if (nrow(stops_for_clustering) == 0) {
    warning("No stops found for clustering")
    return(list(
      qualifying_edges_sf = st_sf(geometry = st_sfc(crs = 4326)),
      all_edges_sf = st_sf(geometry = st_sfc(crs = 4326)),
      edge_summary = data.table(),
      cluster_centroids = data.table()
    ))
  }
  
  stops_clustered <- cluster_stops_spatial(stops_for_clustering, cluster_radius_ft)
  cat(sprintf("Created %d clusters from %d stops\n",
              uniqueN(stops_clustered$cluster_id), nrow(stops_clustered)))
  
  # Calculate cluster centroids
  cluster_centroids <- stops_clustered[, .(
    centroid_lon = mean(stop_lon),
    centroid_lat = mean(stop_lat)
  ), by = cluster_id]
  
  # Step 2: Add cluster_id and shape info to peak stop times
  stop_cluster_map <- stops_clustered[, .(unique_stop_id, cluster_id)]
  
  # Get shape_id from trips
  trip_shape_map <- agency_trips[, .(unique_trip_id, unique_shape_id, unique_route_id, direction_id, agency)]

  # Remove cluster_id if already exists from hub processing to prevent merge collision
  if ("cluster_id" %in% names(agency_am_peak)) {
    agency_am_peak[, cluster_id := NULL]
  }
  if ("cluster_id" %in% names(agency_pm_peak)) {
    agency_pm_peak[, cluster_id := NULL]
  }

  # Remove trip-level columns that will be added from trip_shape_map to prevent .x/.y suffix collision
  cols_to_remove <- c("unique_route_id", "direction_id", "unique_shape_id")
  for (col in cols_to_remove) {
    if (col %in% names(agency_am_peak)) {
      agency_am_peak[, (col) := NULL]
    }
    if (col %in% names(agency_pm_peak)) {
      agency_pm_peak[, (col) := NULL]
    }
  }

  agency_am_peak <- merge(agency_am_peak, stop_cluster_map, by = "unique_stop_id", all.x = TRUE)
  agency_am_peak <- merge(agency_am_peak, trip_shape_map, by = c("unique_trip_id", "agency"), all.x = TRUE)

  agency_pm_peak <- merge(agency_pm_peak, stop_cluster_map, by = "unique_stop_id", all.x = TRUE)
  agency_pm_peak <- merge(agency_pm_peak, trip_shape_map, by = c("unique_trip_id", "agency"), all.x = TRUE)
  
  # Remove stops without cluster assignment
  agency_am_peak <- agency_am_peak[!is.na(cluster_id)]
  agency_pm_peak <- agency_pm_peak[!is.na(cluster_id)]
  
  cat(sprintf("AM peak stops with clusters: %d\n", nrow(agency_am_peak)))
  cat(sprintf("PM peak stops with clusters: %d\n", nrow(agency_pm_peak)))
  
  # Step 3: Build edges
  cat("Building corridor edges...\n")
  am_edges <- build_corridor_edges(agency_am_peak)
  pm_edges <- build_corridor_edges(agency_pm_peak)
  
  cat(sprintf("AM edges: %d\n", nrow(am_edges)))
  cat(sprintf("PM edges: %d\n", nrow(pm_edges)))
  
  if (nrow(am_edges) == 0 && nrow(pm_edges) == 0) {
    warning("No edges built")
    return(list(
      qualifying_edges_sf = st_sf(geometry = st_sfc(crs = 4326)),
      all_edges_sf = st_sf(geometry = st_sfc(crs = 4326)),
      edge_summary = data.table(),
      cluster_centroids = cluster_centroids
    ))
  }
  
  # Step 4: Aggregate edge trips
  am_edge_summary <- if (nrow(am_edges) > 0) aggregate_edge_trips(am_edges) else data.table()
  pm_edge_summary <- if (nrow(pm_edges) > 0) aggregate_edge_trips(pm_edges) else data.table()
  
  # Step 5: Calculate corridor metrics
  cat("Calculating corridor metrics...\n")
  edge_summary <- calculate_edge_corridor_metrics(am_edge_summary, pm_edge_summary, max_interval_minutes)
  
  cat(sprintf("Total edges: %d\n", nrow(edge_summary)))
  cat(sprintf("Qualifying edges (≤%d min): %d\n", max_interval_minutes, sum(edge_summary$qualifies)))
  
  # Step 6: Create edge geometries
  cat("Creating edge geometries...\n")
  
  # Convert shapes to linestrings if needed
  shapes_sf <- NULL
  if (!is.null(agency_shapes) && nrow(agency_shapes) > 0) {
    shapes_sf <- convert_shapes_to_linestrings(agency_shapes)
  }
  
  all_edges_sf <- create_edge_geometries(edge_summary, shapes_sf, cluster_centroids)
  qualifying_edges_sf <- all_edges_sf[all_edges_sf$qualifies, ]
  
  cat(sprintf("Created geometries for %d edges (%d qualifying)\n",
              nrow(all_edges_sf), nrow(qualifying_edges_sf)))
  
  return(list(
    qualifying_edges_sf = qualifying_edges_sf,
    all_edges_sf = all_edges_sf,
    edge_summary = edge_summary,
    cluster_centroids = cluster_centroids
  ))
}


#' Identify Qualifying Corridors (Main Function)
#'
#' Identifies transit corridors using the edge-based methodology.
#' Processes agencies separately for performance, except PACE and CTA
#' which are processed together since they can overlap.
#'
#' @param all_stops Combined stops data.table
#' @param am_peak_bus_stops AM peak bus stops data.table
#' @param pm_peak_bus_stops PM peak bus stops data.table
#' @param all_trips Combined trips data.table
#' @param all_shapes Combined shapes data.table
#' @param max_interval_minutes Numeric. Maximum interval to qualify (default: 15)
#'
#' @return List with:
#'   \itemize{
#'     \item qualifying_corridor_segments: sf object with all qualifying edges
#'     \item qualification_summary: data.table with edge metrics
#'   }
#'
#' @details
#' This function implements the edge-based corridor methodology:
#' \enumerate{
#'   \item Groups agencies: PACE+CTA together, all others separately
#'   \item For each group: clusters stops, builds edges, calculates frequency
#'   \item Combines results from all groups
#' }
#'
#' Processing agencies separately improves performance significantly for
#' large datasets, while keeping PACE and CTA together ensures their
#' overlapping routes are correctly combined.
#'
#' @examples
#' \dontrun{
#' corridors <- identify_qualifying_corridors(
#'   all_stops, am_peak_bus_stops, pm_peak_bus_stops,
#'   all_trips, all_shapes
#' )
#' }
#'
#' @export
identify_qualifying_corridors <- function(all_stops, am_peak_bus_stops, pm_peak_bus_stops,
                                         all_trips, all_shapes,
                                         max_interval_minutes = 15) {
  
  cat("\n=== Identifying Transit Corridors (Edge-Based Method) ===\n\n")
  
  # Define agency groups
  # PACE and CTA can overlap, so process together
  # All other agencies process separately
  all_agencies <- unique(c(am_peak_bus_stops$agency, pm_peak_bus_stops$agency))
  
  pace_cta_agencies <- intersect(all_agencies, c("pace", "cta"))
  other_agencies <- setdiff(all_agencies, c("pace", "cta"))
  
  cat(sprintf("Total agencies: %d\n", length(all_agencies)))
  cat(sprintf("PACE+CTA group: %s\n", paste(pace_cta_agencies, collapse = ", ")))
  cat(sprintf("Other agencies (processed separately): %s\n", paste(other_agencies, collapse = ", ")))
  
  # Build agency groups list
  agency_groups <- list()
  
  if (length(pace_cta_agencies) > 0) {
    agency_groups[["pace_cta"]] <- pace_cta_agencies
  }
  
  for (agency in other_agencies) {
    agency_groups[[agency]] <- agency
  }
  
  # Process each agency group
  all_qualifying_edges <- list()
  all_summaries <- list()
  
  for (group_name in names(agency_groups)) {
    group_agencies <- agency_groups[[group_name]]
    
    # Filter data to this agency group
    group_stops <- all_stops[agency %in% group_agencies]
    group_am_peak <- am_peak_bus_stops[agency %in% group_agencies]
    group_pm_peak <- pm_peak_bus_stops[agency %in% group_agencies]
    group_trips <- all_trips[agency %in% group_agencies]
    group_shapes <- all_shapes[agency %in% group_agencies]
    
    # Skip if no data
    if (nrow(group_am_peak) == 0 && nrow(group_pm_peak) == 0) {
      cat(sprintf("\nSkipping %s - no peak stop times\n", group_name))
      next
    }
    
    # Process this group
    result <- identify_corridors_for_agency_group(
      agency_stops = group_stops,
      agency_am_peak = group_am_peak,
      agency_pm_peak = group_pm_peak,
      agency_trips = group_trips,
      agency_shapes = group_shapes,
      max_interval_minutes = max_interval_minutes
    )
    
    if (nrow(result$qualifying_edges_sf) > 0) {
      all_qualifying_edges[[group_name]] <- result$qualifying_edges_sf
    }
    
    if (nrow(result$edge_summary) > 0) {
      result$edge_summary[, group := group_name]
      all_summaries[[group_name]] <- result$edge_summary
    }
  }
  
  # Combine results
  cat("\n=== Combining Results ===\n")
  
  if (length(all_qualifying_edges) > 0) {
    qualifying_corridor_segments_sf <- do.call(rbind, all_qualifying_edges)
    cat(sprintf("Total qualifying corridor segments: %d\n", nrow(qualifying_corridor_segments_sf)))
  } else {
    qualifying_corridor_segments_sf <- st_sf(geometry = st_sfc(crs = 4326))
    cat("No qualifying corridor segments found\n")
  }
  
  if (length(all_summaries) > 0) {
    qualification_summary <- rbindlist(all_summaries, fill = TRUE)
  } else {
    qualification_summary <- data.table()
  }
  
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
      cat("Shapes already converted to LINESTRING\n")
      cat(sprintf("  %d LINESTRING geometries found\n", nrow(all_shapes)))

      # Ensure required columns exist
      if (!"num_points" %in% names(all_shapes)) {
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

  # Fallback: Manual conversion from point data
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

  if (length(linestrings_list) == 0) {
    return(st_sf(geometry = st_sfc(crs = 4326)))
  }

  # Combine into single sf object
  shapes_sf <- do.call(rbind, linestrings_list)
  shapes_sf <- st_as_sf(shapes_sf)

  cat(sprintf("Created %d LINESTRING geometries\n", nrow(shapes_sf)))

  return(shapes_sf)
}


#' Create Corridor Buffers from Qualifying Segments
#'
#' Creates 1/8 mile buffers around the qualifying corridor segments.
#'
#' @param qualifying_corridor_segments_sf sf object with qualifying corridor
#'   segments (output of identify_qualifying_corridors)
#' @param illinois_boundary sf object with Illinois state boundary
#' @return sf object with buffered corridor geometry (WGS84)
#'
#' @details
#' This function:
#' \enumerate{
#'   \item Takes only the segments that satisfied the corridor frequency test
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
    stop("qualifying_corridor_segments_sf must be an sf object")
  }

  # Keep only non-empty geometries
  qualifying_corridor_segments_sf <- qualifying_corridor_segments_sf[!st_is_empty(qualifying_corridor_segments_sf), ]

  if (nrow(qualifying_corridor_segments_sf) == 0) {
    warning("No qualifying corridor segments available to buffer")
    return(st_sf(geometry = st_sfc(crs = st_crs(4326))))
  }

  cat(sprintf("Qualifying corridor segments to buffer: %d\n", nrow(qualifying_corridor_segments_sf)))

  # Standardize to LINESTRING-only geometries
  qualifying_corridor_segments_sf <- st_collection_extract(qualifying_corridor_segments_sf, "LINESTRING", warn = FALSE)
  qualifying_corridor_segments_sf <- st_cast(qualifying_corridor_segments_sf, "LINESTRING", warn = FALSE)
  qualifying_corridor_segments_sf <- st_make_valid(qualifying_corridor_segments_sf)

  # Project to Illinois State Plane (feet) for accurate buffering
  cat("Buffering route geometries by 1/8 mile from street edge...\n")
  shapes_projected <- st_transform(qualifying_corridor_segments_sf, 3435)

  # Buffer: 660 ft (1/8 mile) + 20 ft (half street width) = 680 ft
  half_street_width <- 20
  buffer_from_edge <- 660 + half_street_width

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
