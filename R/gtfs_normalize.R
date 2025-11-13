#' Read and Normalize GTFS Data
#'
#' Reads GTFS files from an extracted directory and normalizes the data with
#' agency-specific unique identifiers to prevent ID collisions when combining
#' multiple transit agencies.
#'
#' @param agency_name Character. Name of the transit agency (used as prefix for
#'   unique IDs). Examples: "cta", "pace", "metra"
#' @param agency_dir Character. Path to directory containing extracted GTFS files
#'   (typically returned from \code{\link{download_and_extract_gtfs}})
#'
#' @return Named list with 7 data.table elements:
#'   \describe{
#'     \item{stops}{Stop locations with unique_stop_id}
#'     \item{routes}{Route definitions with unique_route_id}
#'     \item{trips}{Trip schedules with unique_trip_id, unique_route_id, and unique_shape_id}
#'     \item{stop_times}{Stop time sequences with unique_trip_id and unique_stop_id}
#'     \item{calendar}{Service calendars with agency field}
#'     \item{calendar_dates}{Service calendar exceptions/additions with agency field}
#'     \item{shapes}{Route geometry with unique_shape_id and point sequences}
#'   }
#'
#' @details
#' This function normalizes GTFS data across agencies by:
#' \enumerate{
#'   \item Reading the core GTFS files: stops.txt, routes.txt, trips.txt,
#'     stop_times.txt, calendar.txt, calendar_dates.txt (if present), and
#'     shapes.txt (if present)
#'   \item Adding \code{agency} field to all tables
#'   \item Creating unique identifiers by prefixing original IDs with agency name:
#'     \itemize{
#'       \item \code{unique_stop_id = "<agency>_<stop_id>"}
#'       \item \code{unique_route_id = "<agency>_<route_id>"}
#'       \item \code{unique_trip_id = "<agency>_<trip_id>"}
#'       \item \code{unique_shape_id = "<agency>_<shape_id>"}
#'     }
#'   \item Handling missing optional fields (location_type, parent_station, direction_id, shape_id)
#'   \item Converting integer64 columns to standard integers to avoid type issues
#'   \item Converting date columns to character format for consistency
#' }
#'
#' If any required file is missing, the function returns an empty data.table with
#' the correct schema instead of failing. This allows graceful handling of
#' incomplete GTFS feeds.
#'
#' @section GTFS Normalization Strategy:
#' The unique ID strategy prevents collisions when combining multiple agencies.
#' For example:
#' \itemize{
#'   \item CTA stop 1234 becomes \code{cta_1234}
#'   \item Pace stop 1234 becomes \code{pace_1234}
#'   \item Both can safely coexist in combined dataset
#' }
#'
#' See \code{docs/gtfs_normalization_strategy.md} for complete details.
#'
#' @section Integer64 Handling:
#' The function converts stop_sequence, start_date, and end_date from integer64
#' to standard types to avoid downstream compatibility issues with data.table
#' operations.
#'
#' @examples
#' \dontrun{
#' # Download and normalize CTA data
#' cta_dir <- download_and_extract_gtfs(
#'   "cta",
#'   "https://www.transitchicago.com/downloads/sch_data/google_transit.zip"
#' )
#' cta_gtfs <- read_normalize_gtfs("cta", cta_dir)
#'
#' # Access individual tables
#' cta_stops <- cta_gtfs$stops
#' cta_routes <- cta_gtfs$routes
#'
#' # Check unique IDs
#' head(cta_stops$unique_stop_id)  # "cta_1234", "cta_1235", ...
#' }
#'
#' @export
read_normalize_gtfs <- function(agency_name, agency_dir) {
  # Read the stops data
  stops_file <- file.path(agency_dir, "stops.txt")
  if (file.exists(stops_file)) {
    stops <- data.table::fread(stops_file)
    stops[, agency := agency_name]

    if (!"location_type" %in% names(stops)) {
      stops[, location_type := NA_integer_]
    }
    if (!"parent_station" %in% names(stops)) {
      stops[, parent_station := NA_character_]
    }

    stops[, stop_id := as.character(stop_id)]
    stops[, unique_stop_id := paste0(agency_name, "_", stop_id)]
  } else {
    stops <- data.table::data.table(
      stop_id = character(),
      stop_name = character(),
      stop_lat = numeric(),
      stop_lon = numeric(),
      location_type = integer(),
      parent_station = character(),
      agency = character(),
      unique_stop_id = character()
    )
  }

  # Read the routes data
  routes_file <- file.path(agency_dir, "routes.txt")
  if (file.exists(routes_file)) {
    routes <- data.table::fread(routes_file)
    routes[, agency := agency_name]
    routes[, route_id := as.character(route_id)]
    routes[, unique_route_id := paste0(agency_name, "_", route_id)]
  } else {
    routes <- data.table::data.table(
      route_id = character(),
      route_type = integer(),
      agency = character(),
      unique_route_id = character()
    )
  }

  # Read trips data
  trips_file <- file.path(agency_dir, "trips.txt")
  if (file.exists(trips_file)) {
    trips <- data.table::fread(trips_file)
    trips[, agency := agency_name]
    trips[, trip_id := as.character(trip_id)]
    trips[, route_id := as.character(route_id)]
    trips[, unique_trip_id := paste0(agency_name, "_", trip_id)]
    trips[, unique_route_id := paste0(agency_name, "_", route_id)]

    # Preserve direction_id if it exists
    if (!"direction_id" %in% names(trips)) {
      trips[, direction_id := NA_integer_]
    }

    # Handle shape_id if it exists (used for route geometry)
    if ("shape_id" %in% names(trips)) {
      trips[, shape_id := as.character(shape_id)]
      trips[, unique_shape_id := paste0(agency_name, "_", shape_id)]
    } else {
      trips[, shape_id := NA_character_]
      trips[, unique_shape_id := NA_character_]
    }
  } else {
    trips <- data.table::data.table(
      trip_id = character(),
      route_id = character(),
      service_id = character(),
      direction_id = integer(),
      shape_id = character(),
      agency = character(),
      unique_trip_id = character(),
      unique_route_id = character(),
      unique_shape_id = character()
    )
  }

  # Read stop_times data
  stop_times_file <- file.path(agency_dir, "stop_times.txt")
  if (file.exists(stop_times_file)) {
    stop_times <- data.table::fread(stop_times_file)
    stop_times[, agency := agency_name]
    stop_times[, trip_id := as.character(trip_id)]
    stop_times[, stop_id := as.character(stop_id)]
    stop_times[, unique_trip_id := paste0(agency_name, "_", trip_id)]
    stop_times[, unique_stop_id := paste0(agency_name, "_", stop_id)]
    # Convert stop_sequence to integer to avoid integer64 issues
    if ("stop_sequence" %in% names(stop_times)) {
      stop_times[, stop_sequence := as.integer(stop_sequence)]
    }
  } else {
    stop_times <- data.table::data.table(
      trip_id = character(),
      stop_id = character(),
      arrival_time = character(),
      departure_time = character(),
      stop_sequence = integer(),
      agency = character(),
      unique_trip_id = character(),
      unique_stop_id = character()
    )
  }

  # Read calendar data
  calendar_file <- file.path(agency_dir, "calendar.txt")
  if (file.exists(calendar_file)) {
    calendar <- data.table::fread(calendar_file)
    calendar[, agency := agency_name]
    # Convert date columns to character to avoid integer64 issues
    if ("start_date" %in% names(calendar)) {
      calendar[, start_date := as.character(start_date)]
    }
    if ("end_date" %in% names(calendar)) {
      calendar[, end_date := as.character(end_date)]
    }
  } else {
    calendar <- data.table::data.table(
      service_id = character(),
      monday = integer(),
      tuesday = integer(),
      wednesday = integer(),
      thursday = integer(),
      friday = integer(),
      saturday = integer(),
      sunday = integer(),
      start_date = character(),
      end_date = character(),
      agency = character()
    )
  }

  # Read calendar_dates data (optional but important for some agencies like CUMTD)
  calendar_dates_file <- file.path(agency_dir, "calendar_dates.txt")
  if (file.exists(calendar_dates_file)) {
    calendar_dates <- data.table::fread(calendar_dates_file)
    calendar_dates[, agency := agency_name]
    # Convert date column to character to avoid integer64 issues
    if ("date" %in% names(calendar_dates)) {
      calendar_dates[, date := as.character(date)]
    }
  } else {
    calendar_dates <- data.table::data.table(
      service_id = character(),
      date = character(),
      exception_type = integer(),
      agency = character()
    )
  }

  # Read shapes data (optional - contains route geometry as lat/lon point sequences)
  shapes_file <- file.path(agency_dir, "shapes.txt")
  if (file.exists(shapes_file)) {
    shapes <- data.table::fread(shapes_file)
    shapes[, agency := agency_name]
    shapes[, shape_id := as.character(shape_id)]
    shapes[, unique_shape_id := paste0(agency_name, "_", shape_id)]

    # Convert shape_pt_sequence to integer to avoid integer64 issues
    if ("shape_pt_sequence" %in% names(shapes)) {
      shapes[, shape_pt_sequence := as.integer(shape_pt_sequence)]
    }

    # Ensure shape_dist_traveled exists (optional field)
    if (!"shape_dist_traveled" %in% names(shapes)) {
      shapes[, shape_dist_traveled := NA_real_]
    }
  } else {
    shapes <- data.table::data.table(
      shape_id = character(),
      shape_pt_lat = numeric(),
      shape_pt_lon = numeric(),
      shape_pt_sequence = integer(),
      shape_dist_traveled = numeric(),
      agency = character(),
      unique_shape_id = character()
    )
  }

  return(list(
    stops = stops,
    routes = routes,
    trips = trips,
    stop_times = stop_times,
    calendar = calendar,
    calendar_dates = calendar_dates,
    shapes = shapes
  ))
}
