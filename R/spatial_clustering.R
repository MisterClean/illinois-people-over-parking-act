#' Cluster Stops by Spatial Proximity
#'
#' Groups transit stops within a specified distance into clusters representing
#' physical intersections or hubs. Uses a connected components algorithm on
#' buffered stop geometries.
#'
#' @param stops_dt data.table with columns: stop_lat, stop_lon, unique_stop_id
#' @param cluster_radius_ft Numeric. Radius in feet for clustering (default: 150)
#'
#' @return data.table with original columns plus:
#'   \describe{
#'     \item{cluster_id}{Integer. Cluster assignment}
#'     \item{cluster_lat}{Numeric. Cluster centroid latitude}
#'     \item{cluster_lon}{Numeric. Cluster centroid longitude}
#'     \item{stops_in_cluster}{Integer. Number of stops in this cluster}
#'   }
#'
#' @details
#' The clustering algorithm:
#' \enumerate{
#'   \item Projects stops to IL State Plane (EPSG:3435) for accurate distance in feet
#'   \item Creates circular buffers around each stop with specified radius
#'   \item Finds intersecting buffers using spatial index
#'   \item Assigns cluster IDs using connected components (breadth-first search)
#'   \item Calculates cluster centroids in WGS84 coordinates
#' }
#'
#' A cluster with 1 stop indicates an isolated stop with no nearby stops.
#' Large clusters (>20 stops) may indicate major transit centers or overly
#' generous clustering radius.
#'
#' @section Why 150 feet?:
#' The 150-foot radius was chosen to group stops that serve the same
#' intersection while avoiding false positives from parallel routes on nearby
#' parallel streets (typically 300+ feet apart in urban grids).
#'
#' @section Algorithm Choice:
#' Uses breadth-first search (BFS) connected components instead of spatial
#' clustering algorithms (like DBSCAN) because:
#' \itemize{
#'   \item Guarantees all stops within radius are in same cluster
#'   \item Deterministic results (no random initialization)
#'   \item Simple to understand and verify
#'   \item Works well for transit network topology
#' }
#'
#' @examples
#' \dontrun{
#' # Cluster CTA bus stops
#' cta_stops <- data.table(
#'   unique_stop_id = c("cta_1", "cta_2", "cta_3"),
#'   stop_name = c("State & Madison", "State & Madison NE", "Clark & Lake"),
#'   stop_lat = c(41.8819, 41.8820, 41.8856),
#'   stop_lon = c(-87.6278, -87.6277, -87.6308)
#' )
#' clustered <- cluster_stops_spatial(cta_stops, cluster_radius_ft = 150)
#'
#' # Check cluster sizes
#' clustered[, .N, by = cluster_id]
#' }
#'
#' @export
cluster_stops_spatial <- function(stops_dt, cluster_radius_ft = 150) {
  # Convert stops to sf object in geographic coordinates
  stops_sf <- sf::st_as_sf(stops_dt,
                      coords = c("stop_lon", "stop_lat"),
                      crs = 4326)

  # Transform to Illinois State Plane East (feet) for accurate distance measurement
  stops_sf_projected <- sf::st_transform(stops_sf, crs = 3435)

  # Create buffers around each stop
  stop_buffers <- sf::st_buffer(stops_sf_projected, dist = cluster_radius_ft)

  # Find intersections between buffers to identify clusters
  intersection_matrix <- sf::st_intersects(stop_buffers, stop_buffers)

  # Assign cluster IDs using connected components approach
  n_stops <- nrow(stops_sf_projected)
  cluster_id <- integer(n_stops)
  current_cluster <- 0

  for (i in 1:n_stops) {
    if (cluster_id[i] == 0) {
      # Start new cluster
      current_cluster <- current_cluster + 1
      to_visit <- i
      visited <- integer(0)

      while (length(to_visit) > 0) {
        current <- to_visit[1]
        to_visit <- to_visit[-1]

        if (!(current %in% visited)) {
          visited <- c(visited, current)
          cluster_id[current] <- current_cluster

          # Find all neighbors
          neighbors <- intersection_matrix[[current]]
          neighbors <- neighbors[cluster_id[neighbors] == 0]
          to_visit <- unique(c(to_visit, neighbors))
        }
      }
    }
  }

  # Add cluster_id back to original data table
  stops_dt_clustered <- data.table::copy(stops_dt)
  stops_dt_clustered[, cluster_id := cluster_id]

  # Calculate cluster centroids for reference
  cluster_info <- stops_dt_clustered[, .(
    cluster_lat = mean(stop_lat),
    cluster_lon = mean(stop_lon),
    stops_in_cluster = .N
  ), by = cluster_id]

  stops_dt_clustered <- merge(stops_dt_clustered, cluster_info, by = "cluster_id")

  return(stops_dt_clustered)
}

#' Verify Route Overlap at Cluster
#'
#' Verifies that routes serving a cluster actually intersect at the same
#' street location by parsing street names from stop names and checking
#' for shared streets between routes.
#'
#' @param cluster_stops_dt data.table with columns: cluster_id, unique_stop_id,
#'   stop_name
#' @param stop_times_dt data.table with columns: unique_stop_id, unique_route_id
#'
#' @return data.table with one row per cluster:
#'   \describe{
#'     \item{cluster_id}{Integer. Cluster identifier}
#'     \item{has_overlap}{Logical. TRUE if >=2 routes share street names}
#'     \item{num_routes}{Integer. Total routes serving cluster}
#'     \item{shared_streets}{Character. Comma-separated list of shared streets}
#'     \item{routes_with_overlap}{Integer. Number of routes sharing streets}
#'   }
#'
#' @details
#' This function prevents false positives from spatial clustering by verifying
#' that routes actually intersect at the same location. For example:
#' \itemize{
#'   \item Two parallel north-south routes 200 feet apart would be clustered
#'   \item But their stop names would be "Street1 & Ave A" vs "Street2 & Ave A"
#'   \item Only share "Ave A", so only routes traveling on Ave A would overlap
#'   \item The parallel routes would NOT be counted as overlapping
#' }
#'
#' @section Street Name Parsing:
#' Extracts street names by:
#' \enumerate{
#'   \item Splitting stop names on delimiters: " & ", " at ", " @ ", "/"
#'   \item Trimming whitespace
#'   \item Removing common suffixes: "Station", "Terminal", "Platform", "Stop", "Entrance"
#' }
#'
#' @section Overlap Definition:
#' A cluster has overlap if:
#' \itemize{
#'   \item At least 2 routes serve the cluster
#'   \item At least 1 street name appears in stop names of >=2 routes
#'   \item At least 2 routes mention the shared street(s)
#' }
#'
#' @section Limitations:
#' \itemize{
#'   \item Relies on consistent stop naming conventions
#'   \item May miss overlaps if street names use different abbreviations
#'     (e.g., "Street" vs "St", "Avenue" vs "Ave")
#'   \item Cannot detect semantic equivalence (e.g., "Main Street" vs "Route 66")
#' }
#'
#' @examples
#' \dontrun{
#' # Cluster stops
#' clustered <- cluster_stops_spatial(bus_stops)
#'
#' # Verify route overlap
#' overlap <- verify_route_overlap_at_cluster(
#'   clustered,
#'   all_stop_times
#' )
#'
#' # Find true hubs (clusters with verified overlap)
#' true_hubs <- overlap[has_overlap == TRUE]
#'
#' # Examine a specific cluster
#' overlap[cluster_id == 42]
#' }
#'
#' @export
verify_route_overlap_at_cluster <- function(cluster_stops_dt, stop_times_dt) {
  # For each cluster, extract street names from stop_name field
  # stop_name format is typically "Street1 & Street2" or similar

  cluster_overlap <- cluster_stops_dt[, {
    # Get all stop names in this cluster
    stop_names <- unique(stop_name)

    # Extract street names by splitting on common delimiters
    # Handle patterns like "Jackson & Lotus", "Oak Ave & Davis St"
    street_list <- lapply(stop_names, function(name) {
      # Split on & and other common separators
      streets <- unlist(strsplit(name, " & | at | @ |/"))
      # Trim whitespace
      streets <- trimws(streets)
      # Remove common direction words and clean up
      streets <- gsub("\\s+(Station|Terminal|Platform|Stop|Entrance)$", "", streets, ignore.case = TRUE)
      return(streets)
    })

    # Get unique street names mentioned across all stops in cluster
    all_streets <- unique(unlist(street_list))

    # Get routes serving this cluster
    cluster_stop_ids <- unique(unique_stop_id)
    routes_at_cluster <- stop_times_dt[unique_stop_id %in% cluster_stop_ids,
                                       unique(unique_route_id)]

    # For each route, find which streets its stops mention
    route_streets <- lapply(routes_at_cluster, function(route) {
      route_stop_ids <- stop_times_dt[unique_route_id == route &
                                      unique_stop_id %in% cluster_stop_ids,
                                      unique(unique_stop_id)]
      route_stop_names <- cluster_stops_dt[unique_stop_id %in% route_stop_ids, stop_name]

      route_street_list <- lapply(route_stop_names, function(name) {
        streets <- unlist(strsplit(name, " & | at | @ |/"))
        streets <- trimws(streets)
        streets <- gsub("\\s+(Station|Terminal|Platform|Stop|Entrance)$", "", streets, ignore.case = TRUE)
        return(streets)
      })

      return(unique(unlist(route_street_list)))
    })
    names(route_streets) <- routes_at_cluster

    # Check for street overlap between routes
    # At least 2 routes must share at least one street name
    num_routes <- length(route_streets)
    if (num_routes < 2) {
      return(list(
        has_overlap = FALSE,
        num_routes = num_routes,
        shared_streets = NA_character_,
        routes_with_overlap = 0
      ))
    }

    # Find streets mentioned by multiple routes
    street_route_counts <- table(unlist(route_streets))
    shared_streets <- names(street_route_counts[street_route_counts >= 2])

    # Count how many routes share streets
    routes_with_shared_streets <- sum(sapply(route_streets, function(rs) {
      any(rs %in% shared_streets)
    }))

    list(
      has_overlap = length(shared_streets) > 0 && routes_with_shared_streets >= 2,
      num_routes = num_routes,
      shared_streets = paste(shared_streets, collapse = ", "),
      routes_with_overlap = routes_with_shared_streets
    )
  }, by = cluster_id]

  return(cluster_overlap)
}
