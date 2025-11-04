# Summary Statistics Functions
#
# Functions for generating summary statistics about transit hubs and corridors.
#
# High-Level Functions:
#   - generate_summary_statistics(): Generate all summary statistics
#
# Helper Functions:
#   - None

#' Generate Summary Statistics
#'
#' Calculates summary statistics for bus hubs, rail hubs, and corridors by agency.
#' Returns counts and totals needed for reporting (excludes route/interval averages).
#'
#' @param all_hubs_sf sf object with all transit hubs
#' @param qualifying_corridor_shapes_sf sf object with qualifying corridor route shapes
#' @return List containing:
#'   - bus_hub_summary: Summary statistics for bus hubs by agency (total_stops, unique_clusters)
#'   - corridor_summary: Summary statistics for corridors by agency (total_shapes)
#'   - rail_hub_count: Total number of rail hubs
#'   - total_hubs: Total number of all hubs
#'   - rail_hub_counts: Count of rail hubs by agency
#'   - qualifying_bus_hubs, rail_stops, qualifying_corridor_shapes: Raw data tables
#'   - by_agency: Dynamic structure with hub and corridor stats by agency
#'   - Legacy individual agency stats (cta_hub, pace_hub, etc.) for backward compatibility
#'
#' @examples
#' \dontrun{
#' stats <- generate_summary_statistics(all_hubs_sf, qualifying_corridor_shapes_sf)
#' }
generate_summary_statistics <- function(all_hubs_sf, qualifying_corridor_shapes_sf) {
  cat("\n=== Generating Summary Statistics ===\n\n")

  # Convert sf objects to data.tables for analysis
  all_hubs <- as.data.table(all_hubs_sf)
  qualifying_corridor_shapes <- as.data.table(qualifying_corridor_shapes_sf)

  # Separate bus hubs and rail stops
  qualifying_bus_hubs <- all_hubs[type == "bus_hub"]
  rail_stops <- all_hubs[type == "rail"]

  # Bus hub summary
  bus_hub_summary <- if (nrow(qualifying_bus_hubs) > 0) {
    qualifying_bus_hubs[, .(
      total_stops = .N,
      unique_clusters = uniqueN(cluster_id)
    ), by = agency]
  } else {
    data.table(
      agency = character(0),
      total_stops = integer(0),
      unique_clusters = integer(0)
    )
  }

  # Corridor summary (now counts route shapes instead of stops)
  corridor_summary <- if (nrow(qualifying_corridor_shapes) > 0) {
    qualifying_corridor_shapes[, .(
      total_shapes = .N
    ), by = agency]
  } else {
    data.table(
      agency = character(0),
      total_shapes = integer(0)
    )
  }

  # Count rail hubs
  rail_hub_count <- nrow(rail_stops)
  total_hubs <- nrow(all_hubs)

  # Extract agency-specific stats (with safe defaults for missing agencies)
  get_agency_stats <- function(dt, agency_name) {
    if (nrow(dt[agency == agency_name]) > 0) {
      dt[agency == agency_name]
    } else {
      # Return empty row with NAs
      dt[0]
    }
  }

  # Build dynamic by_agency structure for all agencies
  by_agency <- list()
  all_agency_ids <- get_all_agency_ids()

  for (agency_id in all_agency_ids) {
    by_agency[[agency_id]] <- list(
      hub = get_agency_stats(bus_hub_summary, agency_id),
      corridor = get_agency_stats(corridor_summary, agency_id)
    )
  }

  # Count rail hubs by agency
  rail_hub_counts <- table(rail_stops$agency)

  # Legacy individual agency stats (for backward compatibility)
  cta_hub <- get_agency_stats(bus_hub_summary, "cta")
  pace_hub <- get_agency_stats(bus_hub_summary, "pace")
  metro_stl_hub <- get_agency_stats(bus_hub_summary, "metro_stl")
  cumtd_hub <- get_agency_stats(bus_hub_summary, "cumtd")
  rmtd_hub <- get_agency_stats(bus_hub_summary, "rmtd")

  cta_corridor <- get_agency_stats(corridor_summary, "cta")
  pace_corridor <- get_agency_stats(corridor_summary, "pace")
  metro_stl_corridor <- get_agency_stats(corridor_summary, "metro_stl")
  cumtd_corridor <- get_agency_stats(corridor_summary, "cumtd")
  rmtd_corridor <- get_agency_stats(corridor_summary, "rmtd")

  cat("Summary statistics generated successfully\n")

  return(list(
    # Core summaries
    bus_hub_summary = bus_hub_summary,
    corridor_summary = corridor_summary,
    rail_hub_count = rail_hub_count,
    total_hubs = total_hubs,
    rail_hub_counts = rail_hub_counts,
    qualifying_bus_hubs = qualifying_bus_hubs,
    rail_stops = rail_stops,
    qualifying_corridor_shapes = qualifying_corridor_shapes,

    # NEW: Dynamic by_agency structure
    by_agency = by_agency,

    # Legacy individual agency stats (backward compatibility)
    cta_hub = cta_hub,
    pace_hub = pace_hub,
    metro_stl_hub = metro_stl_hub,
    cumtd_hub = cumtd_hub,
    rmtd_hub = rmtd_hub,
    cta_corridor = cta_corridor,
    pace_corridor = pace_corridor,
    metro_stl_corridor = metro_stl_corridor,
    cumtd_corridor = cumtd_corridor,
    rmtd_corridor = rmtd_corridor
  ))
}
