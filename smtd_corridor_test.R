# =============================================================================
# SMTD Corridor Test Script
# =============================================================================
# 
# Test case: Routes 8 and 11 in Springfield
# - Each route: ~30 minute peak frequency
# - Combined: Should qualify at 15 minutes where they overlap
#
# This script downloads SMTD GTFS, processes only routes 8 & 11, and creates
# a map showing where they overlap and qualify as corridors.
#
# Usage: Source this file in R or run interactively
# =============================================================================

# -----------------------------------------------------------------------------
# Setup: Load required packages
# -----------------------------------------------------------------------------

cat("=== SMTD Corridor Test: Routes 8 & 11 ===\n\n")

required_packages <- c("tidyverse", "sf", "data.table", "leaflet", "httr", "zip", "units")
for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    install.packages(pkg, repos = "https://cloud.r-project.org/")
    library(pkg, character.only = TRUE)
  }
}

# Disable s2 for geometry operations
sf_use_s2(FALSE)

# -----------------------------------------------------------------------------
# Configuration
# -----------------------------------------------------------------------------

SMTD_GTFS_URL <- "http://data.smtd.org/gtfs/smtd_gtfs_feed.zip"
TEST_ROUTES <- c("8", "11")  # GTFS route_id values
AGENCY_ID <- "smtd"

# Peak periods (same as main analysis)
AM_PEAK_START <- as.ITime("07:00:00")
AM_PEAK_END <- as.ITime("09:00:00")
PM_PEAK_START <- as.ITime("16:00:00")
PM_PEAK_END <- as.ITime("18:00:00")

# Working directory
WORK_DIR <- file.path(tempdir(), "smtd_test")
dir.create(WORK_DIR, showWarnings = FALSE, recursive = TRUE)

cat(sprintf("Working directory: %s\n", WORK_DIR))
cat(sprintf("Test routes: %s\n", paste(TEST_ROUTES, collapse = ", ")))

# -----------------------------------------------------------------------------
# Step 1: Download and extract GTFS
# -----------------------------------------------------------------------------

cat("\n--- Step 1: Downloading SMTD GTFS feed ---\n")

gtfs_zip <- file.path(WORK_DIR, "smtd_gtfs.zip")
gtfs_dir <- file.path(WORK_DIR, "smtd_gtfs")

# Download
if (!file.exists(gtfs_zip)) {
  cat(sprintf("Downloading from: %s\n", SMTD_GTFS_URL))
  download.file(SMTD_GTFS_URL, gtfs_zip, mode = "wb", quiet = TRUE)
  cat("Download complete.\n")
} else {
  cat("Using cached GTFS file.\n")
}

# Extract
if (!dir.exists(gtfs_dir)) {
  dir.create(gtfs_dir)
  unzip(gtfs_zip, exdir = gtfs_dir)
  cat("Extracted GTFS files.\n")
}

# List files
gtfs_files <- list.files(gtfs_dir, pattern = "\\.txt$")
cat(sprintf("GTFS files: %s\n", paste(gtfs_files, collapse = ", ")))

# -----------------------------------------------------------------------------
# Step 2: Load GTFS tables
# -----------------------------------------------------------------------------

cat("\n--- Step 2: Loading GTFS tables ---\n")

# Read tables
stops <- fread(file.path(gtfs_dir, "stops.txt"))
routes <- fread(file.path(gtfs_dir, "routes.txt"))
trips <- fread(file.path(gtfs_dir, "trips.txt"))
stop_times <- fread(file.path(gtfs_dir, "stop_times.txt"))
calendar <- if (file.exists(file.path(gtfs_dir, "calendar.txt"))) {
  fread(file.path(gtfs_dir, "calendar.txt"))
} else {
  data.table()
}
calendar_dates <- if (file.exists(file.path(gtfs_dir, "calendar_dates.txt"))) {
  fread(file.path(gtfs_dir, "calendar_dates.txt"))
} else {
  data.table()
}
shapes <- if (file.exists(file.path(gtfs_dir, "shapes.txt"))) {
  fread(file.path(gtfs_dir, "shapes.txt"))
} else {
  data.table()
}

cat(sprintf("Stops: %d\n", nrow(stops)))
cat(sprintf("Routes: %d\n", nrow(routes)))
cat(sprintf("Trips: %d\n", nrow(trips)))
cat(sprintf("Stop times: %d\n", nrow(stop_times)))
cat(sprintf("Shapes: %d points\n", nrow(shapes)))

# -----------------------------------------------------------------------------
# Step 3: Add unique IDs (agency prefix)
# -----------------------------------------------------------------------------

cat("\n--- Step 3: Adding unique IDs ---\n")

stops[, unique_stop_id := paste0(AGENCY_ID, "_", stop_id)]
stops[, agency := AGENCY_ID]

routes[, unique_route_id := paste0(AGENCY_ID, "_", route_id)]
routes[, agency := AGENCY_ID]

trips[, unique_trip_id := paste0(AGENCY_ID, "_", trip_id)]
trips[, unique_route_id := paste0(AGENCY_ID, "_", route_id)]
trips[, agency := AGENCY_ID]
if ("shape_id" %in% names(trips)) {
  trips[, unique_shape_id := paste0(AGENCY_ID, "_", shape_id)]
}

stop_times[, unique_trip_id := paste0(AGENCY_ID, "_", trip_id)]
stop_times[, unique_stop_id := paste0(AGENCY_ID, "_", stop_id)]
stop_times[, agency := AGENCY_ID]

if (nrow(shapes) > 0) {
  shapes[, unique_shape_id := paste0(AGENCY_ID, "_", shape_id)]
  shapes[, agency := AGENCY_ID]
}

# -----------------------------------------------------------------------------
# Step 4: Filter to test routes (8 and 11)
# -----------------------------------------------------------------------------

cat("\n--- Step 4: Filtering to routes 8 and 11 ---\n")

test_route_ids <- paste0(AGENCY_ID, "_", TEST_ROUTES)

# Check what routes exist
cat("Available routes:\n")
print(routes[, .(route_id, route_short_name, route_long_name)])

# Filter routes
test_routes <- routes[unique_route_id %in% test_route_ids]
cat(sprintf("\nTest routes found: %d\n", nrow(test_routes)))
print(test_routes[, .(unique_route_id, route_short_name, route_long_name)])

# Filter trips to test routes
test_trips <- trips[unique_route_id %in% test_route_ids]
cat(sprintf("Trips for test routes: %d\n", nrow(test_trips)))

# Check direction_id
if ("direction_id" %in% names(test_trips)) {
  cat("Direction IDs available:\n")
  print(test_trips[, .N, by = .(unique_route_id, direction_id)])
} else {
  test_trips[, direction_id := 0L]
  cat("No direction_id in data, defaulting to 0\n")
}

# Filter stop_times to test trips
test_stop_times <- stop_times[unique_trip_id %in% test_trips$unique_trip_id]
cat(sprintf("Stop times for test routes: %d\n", nrow(test_stop_times)))

# Get stops used by test routes
test_stop_ids <- unique(test_stop_times$unique_stop_id)
test_stops <- stops[unique_stop_id %in% test_stop_ids]
cat(sprintf("Stops served by test routes: %d\n", nrow(test_stops)))

# Filter shapes if available
if (nrow(shapes) > 0 && "unique_shape_id" %in% names(test_trips)) {
  test_shape_ids <- unique(test_trips$unique_shape_id)
  test_shapes <- shapes[unique_shape_id %in% test_shape_ids]
  cat(sprintf("Shape points for test routes: %d\n", nrow(test_shapes)))
} else {
  test_shapes <- data.table()
  cat("No shapes available for test routes\n")
}

# -----------------------------------------------------------------------------
# Step 5: Identify weekday services
# -----------------------------------------------------------------------------

cat("\n--- Step 5: Identifying weekday services ---\n")

# Try calendar.txt first
if (nrow(calendar) > 0) {
  calendar[, agency := AGENCY_ID]
  weekday_services <- calendar[
    monday == 1 & tuesday == 1 & wednesday == 1 & thursday == 1 & friday == 1,
    .(service_id, agency)
  ]
  cat(sprintf("Weekday services from calendar.txt: %d\n", nrow(weekday_services)))
}

# If no weekday services, try calendar_dates
if (!exists("weekday_services") || nrow(weekday_services) == 0) {
  if (nrow(calendar_dates) > 0) {
    calendar_dates[, agency := AGENCY_ID]
    # Analyze dates to find weekday services
    service_dates <- calendar_dates[exception_type == 1]
    service_dates[, date_obj := as.Date(as.character(date), format = "%Y%m%d")]
    service_dates[, weekday := lubridate::wday(date_obj, week_start = 1)]
    service_dates[, is_weekday := weekday >= 1 & weekday <= 5]
    
    weekday_analysis <- service_dates[, .(
      weekday_ratio = sum(is_weekday) / .N,
      n_dates = .N
    ), by = service_id]
    
    weekday_services <- weekday_analysis[weekday_ratio >= 0.8, .(service_id)]
    weekday_services[, agency := AGENCY_ID]
    cat(sprintf("Weekday services from calendar_dates.txt: %d\n", nrow(weekday_services)))
  }
}

# Fallback: use all services
if (!exists("weekday_services") || nrow(weekday_services) == 0) {
  weekday_services <- unique(test_trips[, .(service_id, agency)])
  cat("Using all services as weekday (no calendar data)\n")
}

print(weekday_services)

# -----------------------------------------------------------------------------
# Step 6: Filter to weekday trips and peak periods
# -----------------------------------------------------------------------------

cat("\n--- Step 6: Filtering to peak periods ---\n")

# Filter trips to weekday service
weekday_trips <- merge(test_trips, weekday_services, by = c("service_id", "agency"))
cat(sprintf("Weekday trips: %d\n", nrow(weekday_trips)))

# Get stop times for weekday trips
weekday_stop_times <- test_stop_times[unique_trip_id %in% weekday_trips$unique_trip_id]

# Parse arrival times
weekday_stop_times[, arrival_hour := as.integer(substr(arrival_time, 1, 2))]
weekday_stop_times <- weekday_stop_times[arrival_hour < 24]  # Filter overnight
weekday_stop_times[, arrival_time_obj := as.ITime(substr(arrival_time, 1, 8))]

# Add route and direction info
weekday_stop_times <- merge(
  weekday_stop_times,
  weekday_trips[, .(unique_trip_id, unique_route_id, direction_id)],
  by = "unique_trip_id"
)

# Split into AM and PM peak
am_peak_stops <- weekday_stop_times[
  arrival_time_obj >= AM_PEAK_START & arrival_time_obj <= AM_PEAK_END
]
pm_peak_stops <- weekday_stop_times[
  arrival_time_obj >= PM_PEAK_START & arrival_time_obj <= PM_PEAK_END
]

cat(sprintf("AM peak stop times: %d\n", nrow(am_peak_stops)))
cat(sprintf("PM peak stop times: %d\n", nrow(pm_peak_stops)))

# -----------------------------------------------------------------------------
# Step 7: Calculate route frequencies
# -----------------------------------------------------------------------------

cat("\n--- Step 7: Calculating route frequencies ---\n")

# Count trips per route/direction
am_route_trips <- am_peak_stops[, .(
  trips_am = uniqueN(unique_trip_id)
), by = .(unique_route_id, direction_id)]

pm_route_trips <- pm_peak_stops[, .(
  trips_pm = uniqueN(unique_trip_id)
), by = .(unique_route_id, direction_id)]

route_frequency <- merge(am_route_trips, pm_route_trips, 
                         by = c("unique_route_id", "direction_id"), 
                         all = TRUE)
route_frequency[is.na(trips_am), trips_am := 0]
route_frequency[is.na(trips_pm), trips_pm := 0]
route_frequency[, interval_am := fifelse(trips_am > 0, 120 / trips_am, Inf)]
route_frequency[, interval_pm := fifelse(trips_pm > 0, 120 / trips_pm, Inf)]

cat("\nRoute frequency summary:\n")
print(route_frequency)

cat("\nInterpretation:\n")
for (i in seq_len(nrow(route_frequency))) {
  row <- route_frequency[i]
  cat(sprintf("  %s (dir %d): %d AM trips (%.0f min), %d PM trips (%.0f min)\n",
              gsub("smtd_", "Route ", row$unique_route_id),
              row$direction_id,
              row$trips_am, row$interval_am,
              row$trips_pm, row$interval_pm))
}

# -----------------------------------------------------------------------------
# Step 8: Cluster stops (for V3 stop-edge approach)
# -----------------------------------------------------------------------------

cat("\n--- Step 8: Clustering stops ---\n")

# Simple clustering function
cluster_stops <- function(stops_dt, radius_ft = 150) {
  stops_sf <- st_as_sf(stops_dt, coords = c("stop_lon", "stop_lat"), crs = 4326)
  stops_proj <- st_transform(stops_sf, 3435)  # IL State Plane (feet)
  
  # Buffer and find intersections
  buffers <- st_buffer(stops_proj, radius_ft)
  intersections <- st_intersects(buffers, buffers)
  
  # Connected components
  n <- nrow(stops_proj)
  cluster_id <- integer(n)
  current_cluster <- 0
  
  for (i in 1:n) {
    if (cluster_id[i] == 0) {
      current_cluster <- current_cluster + 1
      to_visit <- i
      while (length(to_visit) > 0) {
        current <- to_visit[1]
        to_visit <- to_visit[-1]
        if (cluster_id[current] == 0) {
          cluster_id[current] <- current_cluster
          neighbors <- intersections[[current]]
          neighbors <- neighbors[cluster_id[neighbors] == 0]
          to_visit <- unique(c(to_visit, neighbors))
        }
      }
    }
  }
  
  result <- copy(stops_dt)
  result[, cluster_id := cluster_id]
  
  # Add centroid info
  centroids <- result[, .(
    centroid_lat = mean(stop_lat),
    centroid_lon = mean(stop_lon),
    n_stops = .N
  ), by = cluster_id]
  
  result <- merge(result, centroids, by = "cluster_id")
  return(result)
}

stops_clustered <- cluster_stops(test_stops, radius_ft = 150)
cat(sprintf("Created %d clusters from %d stops\n", 
            uniqueN(stops_clustered$cluster_id), nrow(stops_clustered)))

# Add cluster_id to stop times
am_peak_stops <- merge(am_peak_stops, 
                       stops_clustered[, .(unique_stop_id, cluster_id)],
                       by = "unique_stop_id")
pm_peak_stops <- merge(pm_peak_stops,
                       stops_clustered[, .(unique_stop_id, cluster_id)],
                       by = "unique_stop_id")

# -----------------------------------------------------------------------------
# Step 9: V3 - Stop-Edge Graph Approach
# -----------------------------------------------------------------------------

cat("\n--- Step 9: V3 Stop-Edge Graph Analysis ---\n")

# Build edges from stop sequences
build_edges <- function(peak_stops) {
  setorder(peak_stops, unique_trip_id, stop_sequence)
  
  edges <- peak_stops[, {
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
          direction_id = direction_id[valid]
        )
      }
    }
  }, by = unique_trip_id]
  
  return(edges)
}

am_edges <- build_edges(am_peak_stops)
pm_edges <- build_edges(pm_peak_stops)

cat(sprintf("AM edges: %d\n", nrow(am_edges)))
cat(sprintf("PM edges: %d\n", nrow(pm_edges)))

# Aggregate by edge
am_edge_summary <- am_edges[, .(
  trips_am = uniqueN(unique_trip_id),
  routes_am = list(unique(unique_route_id)),
  num_routes_am = uniqueN(unique_route_id)
), by = .(from_cluster, to_cluster, direction_id)]

pm_edge_summary <- pm_edges[, .(
  trips_pm = uniqueN(unique_trip_id),
  routes_pm = list(unique(unique_route_id)),
  num_routes_pm = uniqueN(unique_route_id)
), by = .(from_cluster, to_cluster, direction_id)]

# Merge AM and PM
v3_edge_summary <- merge(am_edge_summary, pm_edge_summary,
                         by = c("from_cluster", "to_cluster", "direction_id"),
                         all = TRUE)
v3_edge_summary[is.na(trips_am), trips_am := 0]
v3_edge_summary[is.na(trips_pm), trips_pm := 0]
v3_edge_summary[is.na(num_routes_am), num_routes_am := 0]
v3_edge_summary[is.na(num_routes_pm), num_routes_pm := 0]

# Calculate intervals
v3_edge_summary[, interval_am := fifelse(trips_am > 0, 120 / trips_am, Inf)]
v3_edge_summary[, interval_pm := fifelse(trips_pm > 0, 120 / trips_pm, Inf)]

# Determine number of routes (combine AM and PM)
v3_edge_summary[, num_routes := pmax(num_routes_am, num_routes_pm)]

# Qualify
v3_edge_summary[, qualifies := interval_am <= 15 | interval_pm <= 15]

cat("\nV3 Edge Summary:\n")
cat(sprintf("Total edges: %d\n", nrow(v3_edge_summary)))
cat(sprintf("Single-route edges: %d\n", nrow(v3_edge_summary[num_routes == 1])))
cat(sprintf("Multi-route edges: %d\n", nrow(v3_edge_summary[num_routes > 1])))
cat(sprintf("Qualifying edges: %d\n", nrow(v3_edge_summary[qualifies == TRUE])))

# Show multi-route edges (where routes 8 & 11 overlap)
cat("\nMulti-route edges (overlap segments):\n")
multi_route_edges <- v3_edge_summary[num_routes > 1]
if (nrow(multi_route_edges) > 0) {
  print(multi_route_edges[, .(from_cluster, to_cluster, direction_id, 
                               trips_am, trips_pm, interval_am, interval_pm, 
                               num_routes, qualifies)])
} else {
  cat("No multi-route edges found!\n")
}

# -----------------------------------------------------------------------------
# Step 10: Create V3 edge geometries
# -----------------------------------------------------------------------------

cat("\n--- Step 10: Creating V3 edge geometries ---\n")

# Get cluster centroids
cluster_centroids <- stops_clustered[, .(
  centroid_lon = mean(stop_lon),
  centroid_lat = mean(stop_lat)
), by = cluster_id]

# Create linestrings for edges
create_edge_geom <- function(edge_summary, centroids) {
  # Join coordinates
  edges_coords <- merge(edge_summary, centroids, 
                        by.x = "from_cluster", by.y = "cluster_id")
  setnames(edges_coords, c("centroid_lon", "centroid_lat"), c("from_lon", "from_lat"))
  
  edges_coords <- merge(edges_coords, centroids,
                        by.x = "to_cluster", by.y = "cluster_id")
  setnames(edges_coords, c("centroid_lon", "centroid_lat"), c("to_lon", "to_lat"))
  
  # Create linestrings
  geoms <- vector("list", nrow(edges_coords))
  for (i in seq_len(nrow(edges_coords))) {
    coords <- matrix(c(
      edges_coords$from_lon[i], edges_coords$from_lat[i],
      edges_coords$to_lon[i], edges_coords$to_lat[i]
    ), ncol = 2, byrow = TRUE)
    geoms[[i]] <- st_linestring(coords)
  }
  
  # Build sf
  sf_obj <- st_sf(
    from_cluster = edges_coords$from_cluster,
    to_cluster = edges_coords$to_cluster,
    direction_id = edges_coords$direction_id,
    trips_am = edges_coords$trips_am,
    trips_pm = edges_coords$trips_pm,
    interval_am = edges_coords$interval_am,
    interval_pm = edges_coords$interval_pm,
    num_routes = edges_coords$num_routes,
    qualifies = edges_coords$qualifies,
    geometry = st_sfc(geoms, crs = 4326)
  )
  
  return(sf_obj)
}

v3_edges_sf <- create_edge_geom(v3_edge_summary, cluster_centroids)
v3_qualifying_sf <- v3_edges_sf[v3_edges_sf$qualifies, ]

cat(sprintf("V3 total edges as sf: %d\n", nrow(v3_edges_sf)))
cat(sprintf("V3 qualifying edges: %d\n", nrow(v3_qualifying_sf)))

# -----------------------------------------------------------------------------
# Step 11: V4 - Spatial Grid Approach
# -----------------------------------------------------------------------------

cat("\n--- Step 11: V4 Spatial Grid Analysis ---\n")

# Build route geometries from shapes (or stops)
if (nrow(test_shapes) > 0) {
  cat("Building route geometry from GTFS shapes...\n")
  
  # Convert shapes to linestrings
  setorder(test_shapes, unique_shape_id, shape_pt_sequence)
  # Filter out shape points with NA coordinates
  test_shapes <- test_shapes[!is.na(shape_pt_lon) & !is.na(shape_pt_lat)]
  shape_list <- split(test_shapes, by = "unique_shape_id")
  
  shape_geoms <- lapply(names(shape_list), function(sid) {
    pts <- shape_list[[sid]]
    if (nrow(pts) < 2) return(NULL)
    coords <- as.matrix(pts[, .(shape_pt_lon, shape_pt_lat)])
    list(unique_shape_id = sid, geom = st_linestring(coords))
  })
  shape_geoms <- shape_geoms[!sapply(shape_geoms, is.null)]
  
  shapes_sf <- st_sf(
    unique_shape_id = sapply(shape_geoms, `[[`, "unique_shape_id"),
    geometry = st_sfc(lapply(shape_geoms, `[[`, "geom"), crs = 4326)
  )
  
  # Link to routes
  route_shape_link <- unique(weekday_trips[, .(unique_route_id, unique_shape_id, direction_id)])
  # Keep shapes_sf as sf object (first arg) to preserve geometry column
  route_geoms_sf <- merge(shapes_sf, route_shape_link,
                          by = "unique_shape_id")
  
  cat(sprintf("Route geometries from shapes: %d\n", nrow(route_geoms_sf)))
} else {
  cat("No shapes available, skipping V4\n")
  route_geoms_sf <- NULL
}

# Create grid and analyze
if (!is.null(route_geoms_sf) && nrow(route_geoms_sf) > 0) {
  # Project to feet
  route_geoms_proj <- st_transform(route_geoms_sf, 3435)
  
  # Create grid (200 ft cells)
  cell_size <- 200
  bbox <- st_bbox(route_geoms_proj)
  bbox_expanded <- c(
    xmin = bbox["xmin"] - 1000,
    ymin = bbox["ymin"] - 1000,
    xmax = bbox["xmax"] + 1000,
    ymax = bbox["ymax"] + 1000
  )
  
  grid <- st_make_grid(
    st_as_sfc(st_bbox(bbox_expanded), crs = 3435),
    cellsize = c(cell_size, cell_size),
    what = "polygons"
  )
  grid_sf <- st_sf(cell_id = seq_along(grid), geometry = grid)
  
  # Filter to cells near routes
  route_buffer <- st_buffer(st_union(route_geoms_proj), cell_size * 2)
  cells_keep <- st_intersects(grid_sf, route_buffer, sparse = FALSE)[, 1]
  grid_sf <- grid_sf[cells_keep, ]
  grid_sf$cell_id <- seq_len(nrow(grid_sf))
  
  cat(sprintf("Grid cells (filtered): %d\n", nrow(grid_sf)))
  
  # Assign routes to cells
  intersections <- st_intersects(route_geoms_proj, grid_sf)
  
  route_cell_assignments <- rbindlist(lapply(seq_along(intersections), function(i) {
    cells <- intersections[[i]]
    if (length(cells) == 0) return(NULL)
    data.table(
      route_idx = i,
      unique_route_id = route_geoms_sf$unique_route_id[i],
      direction_id = route_geoms_sf$direction_id[i],
      cell_id = grid_sf$cell_id[cells]
    )
  }))
  
  cat(sprintf("Route-cell assignments: %d\n", nrow(route_cell_assignments)))
  
  # Join frequency data
  route_cell_assignments <- merge(route_cell_assignments, route_frequency,
                                  by = c("unique_route_id", "direction_id"),
                                  all.x = TRUE)
  route_cell_assignments[is.na(trips_am), trips_am := 0]
  route_cell_assignments[is.na(trips_pm), trips_pm := 0]
  
  # Aggregate by cell
  v4_cell_summary <- route_cell_assignments[, .(
    trips_am = sum(trips_am),
    trips_pm = sum(trips_pm),
    num_routes = uniqueN(unique_route_id),
    routes = paste(unique(unique_route_id), collapse = ";")
  ), by = .(cell_id, direction_id)]
  
  v4_cell_summary[, interval_am := fifelse(trips_am > 0, 120 / trips_am, Inf)]
  v4_cell_summary[, interval_pm := fifelse(trips_pm > 0, 120 / trips_pm, Inf)]
  v4_cell_summary[, qualifies := interval_am <= 15 | interval_pm <= 15]
  
  cat("\nV4 Cell Summary:\n")
  cat(sprintf("Total cells with service: %d\n", nrow(v4_cell_summary)))
  cat(sprintf("Single-route cells: %d\n", nrow(v4_cell_summary[num_routes == 1])))
  cat(sprintf("Multi-route cells: %d\n", nrow(v4_cell_summary[num_routes > 1])))
  cat(sprintf("Qualifying cells: %d\n", nrow(v4_cell_summary[qualifies == TRUE])))
  
  # Create sf for qualifying cells
  qualifying_cell_ids <- unique(v4_cell_summary[qualifies == TRUE, cell_id])
  v4_qualifying_cells <- grid_sf[grid_sf$cell_id %in% qualifying_cell_ids, ]
  v4_qualifying_cells <- st_transform(v4_qualifying_cells, 4326)
  
  # Also get multi-route cells for highlighting
  multi_route_cell_ids <- unique(v4_cell_summary[num_routes > 1, cell_id])
  v4_multi_route_cells <- grid_sf[grid_sf$cell_id %in% multi_route_cell_ids, ]
  v4_multi_route_cells <- st_transform(v4_multi_route_cells, 4326)
  
  cat(sprintf("\nV4 qualifying cells as sf: %d\n", nrow(v4_qualifying_cells)))
  cat(sprintf("V4 multi-route cells: %d\n", nrow(v4_multi_route_cells)))
} else {
  v4_qualifying_cells <- NULL
  v4_multi_route_cells <- NULL
}

# -----------------------------------------------------------------------------
# Step 12: Create comparison map
# -----------------------------------------------------------------------------

cat("\n--- Step 12: Creating comparison map ---\n")

# Prepare route geometries for display
if (!is.null(route_geoms_sf)) {
  route_geoms_display <- st_transform(route_geoms_sf, 4326)
  route_geoms_display$route_label <- gsub("smtd_", "Route ", route_geoms_display$unique_route_id)
}

# Prepare stops for display
stops_display <- st_as_sf(test_stops, coords = c("stop_lon", "stop_lat"), crs = 4326)

# Create map
map <- leaflet() %>%
  addTiles(group = "OpenStreetMap") %>%
  addProviderTiles(providers$CartoDB.Positron, group = "CartoDB Light")

# Add route lines
if (!is.null(route_geoms_sf)) {
  # Route 8
  route8_geom <- route_geoms_display[grepl("_8$", route_geoms_display$unique_route_id), ]
  if (nrow(route8_geom) > 0) {
    map <- map %>% addPolylines(
      data = route8_geom,
      color = "#E31837",  # Red
      weight = 4,
      opacity = 0.8,
      group = "Route 8",
      popup = ~paste("Route 8<br>Direction:", direction_id)
    )
  }
  
  # Route 11
  route11_geom <- route_geoms_display[grepl("_11$", route_geoms_display$unique_route_id), ]
  if (nrow(route11_geom) > 0) {
    map <- map %>% addPolylines(
      data = route11_geom,
      color = "#009CDE",  # Blue
      weight = 4,
      opacity = 0.8,
      group = "Route 11",
      popup = ~paste("Route 11<br>Direction:", direction_id)
    )
  }
}

# Add stops
map <- map %>% addCircleMarkers(
  data = stops_display,
  radius = 4,
  color = "#333",
  fillColor = "#FFD700",
  fillOpacity = 0.8,
  weight = 1,
  group = "Stops",
  popup = ~paste0("<b>", stop_name, "</b><br>ID: ", stop_id)
)

# Add V3 qualifying edges
if (nrow(v3_qualifying_sf) > 0) {
  # Single-route qualifying
  v3_single <- v3_qualifying_sf[v3_qualifying_sf$num_routes == 1, ]
  if (nrow(v3_single) > 0) {
    map <- map %>% addPolylines(
      data = v3_single,
      color = "#32CD32",  # Lime green
      weight = 6,
      opacity = 0.7,
      group = "V3: Single Route Qualifying",
      popup = ~paste0(
        "<b>V3 Single-Route Edge</b><br>",
        "AM: ", trips_am, " trips (", round(interval_am, 1), " min)<br>",
        "PM: ", trips_pm, " trips (", round(interval_pm, 1), " min)"
      )
    )
  }
  
  # Multi-route qualifying (overlap!)
  v3_multi <- v3_qualifying_sf[v3_qualifying_sf$num_routes > 1, ]
  if (nrow(v3_multi) > 0) {
    map <- map %>% addPolylines(
      data = v3_multi,
      color = "#FF6600",  # Orange
      weight = 8,
      opacity = 0.9,
      group = "V3: Multi-Route Qualifying (OVERLAP)",
      popup = ~paste0(
        "<b>V3 OVERLAP Edge (Routes 8 + 11)</b><br>",
        "AM: ", trips_am, " trips (", round(interval_am, 1), " min)<br>",
        "PM: ", trips_pm, " trips (", round(interval_pm, 1), " min)<br>",
        "Combined qualifies: ", qualifies
      )
    )
  }
}

# Add V4 cells
if (!is.null(v4_multi_route_cells) && nrow(v4_multi_route_cells) > 0) {
  map <- map %>% addPolygons(
    data = v4_multi_route_cells,
    fillColor = "#FF6600",
    fillOpacity = 0.3,
    color = "#FF6600",
    weight = 1,
    group = "V4: Multi-Route Cells (OVERLAP)",
    popup = "V4 cell where both routes pass"
  )
}

if (!is.null(v4_qualifying_cells) && nrow(v4_qualifying_cells) > 0) {
  map <- map %>% addPolygons(
    data = v4_qualifying_cells,
    fillColor = "#32CD32",
    fillOpacity = 0.2,
    color = "#32CD32",
    weight = 1,
    group = "V4: All Qualifying Cells"
  )
}

# Add layer control
map <- map %>% addLayersControl(
  baseGroups = c("CartoDB Light", "OpenStreetMap"),
  overlayGroups = c(
    "Route 8", "Route 11", "Stops",
    "V3: Single Route Qualifying",
    "V3: Multi-Route Qualifying (OVERLAP)",
    "V4: Multi-Route Cells (OVERLAP)",
    "V4: All Qualifying Cells"
  ),
  options = layersControlOptions(collapsed = FALSE)
) %>%
  hideGroup("V4: All Qualifying Cells")  # Hide by default to reduce clutter

# Set view to Springfield
map <- map %>% setView(lng = -89.65, lat = 39.80, zoom = 13)

# Add legend
map <- map %>% addLegend(
  position = "bottomright",
  colors = c("#E31837", "#009CDE", "#32CD32", "#FF6600"),
  labels = c("Route 8", "Route 11", "Single-Route Qualifying", "Multi-Route Overlap"),
  title = "SMTD Test"
)

# -----------------------------------------------------------------------------
# Step 13: Save results
# -----------------------------------------------------------------------------

cat("\n--- Step 13: Saving results ---\n")

# Save map
output_dir <- "/mnt/user-data/outputs"
if (!dir.exists(output_dir)) {
  output_dir <- WORK_DIR
}

map_file <- file.path(output_dir, "smtd_corridor_test_map.html")
htmlwidgets::saveWidget(map, map_file, selfcontained = TRUE)
cat(sprintf("Map saved to: %s\n", map_file))

# -----------------------------------------------------------------------------
# Summary
# -----------------------------------------------------------------------------

cat("\n")
cat("=" * 70, "\n")
cat("SMTD CORRIDOR TEST SUMMARY\n")
cat("=" * 70, "\n\n")

cat("ROUTE FREQUENCIES:\n")
for (i in seq_len(nrow(route_frequency))) {
  row <- route_frequency[i]
  cat(sprintf("  Route %s (dir %d): AM=%.0f min, PM=%.0f min\n",
              gsub("smtd_", "", row$unique_route_id),
              row$direction_id,
              row$interval_am, row$interval_pm))
}

cat("\nV3 STOP-EDGE RESULTS:\n")
cat(sprintf("  Total edges: %d\n", nrow(v3_edge_summary)))
cat(sprintf("  Multi-route edges (overlap): %d\n", nrow(v3_edge_summary[num_routes > 1])))
cat(sprintf("  Qualifying edges: %d\n", nrow(v3_edge_summary[qualifies == TRUE])))

if (nrow(v3_edge_summary[num_routes > 1]) > 0) {
  cat("\n  OVERLAP DETAILS:\n")
  overlap_edges <- v3_edge_summary[num_routes > 1]
  for (i in seq_len(min(10, nrow(overlap_edges)))) {
    row <- overlap_edges[i]
    cat(sprintf("    Edge %d→%d: AM=%d trips (%.0f min), PM=%d trips (%.0f min) | Qualifies: %s\n",
                row$from_cluster, row$to_cluster,
                row$trips_am, row$interval_am,
                row$trips_pm, row$interval_pm,
                row$qualifies))
  }
}

if (!is.null(v4_qualifying_cells)) {
  cat("\nV4 SPATIAL GRID RESULTS:\n")
  cat(sprintf("  Total cells with service: %d\n", nrow(v4_cell_summary)))
  cat(sprintf("  Multi-route cells (overlap): %d\n", nrow(v4_cell_summary[num_routes > 1])))
  cat(sprintf("  Qualifying cells: %d\n", nrow(v4_cell_summary[qualifies == TRUE])))
}

cat("\n")
cat("Map saved to view results interactively.\n")
cat("Look for ORANGE lines/cells - these are where Routes 8 & 11 overlap!\n")

# Return map for interactive viewing
map