#' Identify CTA Rail Stations
#'
#' Extracts CTA rail stations using parent_station and location_type fields
#' from GTFS stops data.
#'
#' @param stops_dt data.table with columns: agency, parent_station, location_type,
#'   unique_stop_id, stop_id, stop_name, stop_lat, stop_lon
#'
#' @return data.table with CTA rail stations (platforms and stations)
#'
#' @details
#' CTA GTFS uses two approaches to identify rail stations:
#' \enumerate{
#'   \item Platforms (location_type=0 or NA) with a parent_station field
#'   \item Stations (location_type=1) without parent_station
#' }
#'
#' Both are considered rail stations and combined.
#'
#' @export
identify_cta_rail_stations <- function(stops_dt) {
  # Platforms with parent stations
  cta_platforms <- stops_dt[
    agency == "cta" &
    (!is.na(parent_station) & parent_station != "") &
    (is.na(location_type) | location_type == 0),
    .(unique_stop_id, stop_id, stop_name, stop_lat, stop_lon, agency)
  ]

  # Station objects (location_type = 1)
  cta_stations <- stops_dt[
    agency == "cta" &
    (!is.na(location_type) & location_type == 1),
    .(unique_stop_id, stop_id, stop_name, stop_lat, stop_lon, agency)
  ]

  # Combine
  rbindlist(list(cta_platforms, cta_stations), fill = TRUE)
}

#' Identify Metra Rail Stations
#'
#' Extracts Metra rail stations, filtering out Wisconsin stations.
#'
#' @param stops_dt data.table with columns: agency, stop_lat, unique_stop_id,
#'   stop_id, stop_name, stop_lon
#' @param max_latitude Numeric. Maximum latitude for Illinois stations (default: 42.5)
#'
#' @return data.table with Metra rail stations in Illinois
#'
#' @details
#' For Metra, all stops are rail stations. However, some Metra lines extend
#' into Wisconsin. The latitude filter (default 42.5°N) excludes Wisconsin
#' stations while keeping all Illinois stations.
#'
#' Wisconsin border is approximately 42.5°N. Setting cutoff there ensures:
#' \itemize{
#'   \item All Illinois Metra stations included
#'   \item Wisconsin stations (Kenosha line) excluded
#' }
#'
#' @export
identify_metra_rail_stations <- function(stops_dt, max_latitude = 42.5) {
  stops_dt[
    agency == "metra" & stop_lat <= max_latitude,
    .(unique_stop_id, stop_id, stop_name, stop_lat, stop_lon, agency)
  ]
}

#' Identify Metro St. Louis MetroLink Stations
#'
#' Extracts MetroLink (light rail) stations from Metro St. Louis GTFS data.
#'
#' @param stops_dt data.table with columns: agency, unique_stop_id, stop_id,
#'   stop_name, stop_lat, stop_lon
#' @param routes_dt data.table with columns: agency, route_type, unique_route_id
#' @param stop_times_dt data.table with columns: agency, unique_route_id, unique_stop_id
#'
#' @return data.table with MetroLink stations
#'
#' @details
#' Metro St. Louis operates both MetroLink (light rail, route_type=2) and
#' MetroBus (bus, route_type=3). This function:
#' \enumerate{
#'   \item Identifies MetroLink routes (route_type = 2)
#'   \item Finds stops served by those routes
#'   \item Returns stop details
#' }
#'
#' MetroLink serves both Missouri and Illinois (including East St. Louis area).
#'
#' @export
identify_metro_stl_metrolink_stations <- function(stops_dt, routes_dt, stop_times_dt) {
  # Get MetroLink route IDs (route_type = 2 for light rail)
  metrolink_routes <- routes_dt[agency == "metro_stl" & route_type == 2, unique_route_id]

  # Get stops served by MetroLink
  metrolink_stop_ids <- stop_times_dt[
    agency == "metro_stl" & unique_route_id %in% metrolink_routes,
    unique(unique_stop_id)
  ]

  # Return stop details
  stops_dt[
    agency == "metro_stl" & unique_stop_id %in% metrolink_stop_ids,
    .(unique_stop_id, stop_id, stop_name, stop_lat, stop_lon, agency)
  ]
}

#' Identify All Rail Stations
#'
#' Combines rail station identification across all agencies.
#'
#' @param stops_dt data.table with GTFS stops from all agencies
#' @param routes_dt data.table with GTFS routes from all agencies
#' @param stop_times_dt data.table with GTFS stop_times from all agencies
#'
#' @return data.table with all rail stations across Illinois, with added column:
#'   \describe{
#'     \item{type}{Character. Always "rail" for these stations}
#'   }
#'
#' @details
#' Identifies rail stations for:
#' \itemize{
#'   \item CTA - L train stations (using parent_station and location_type)
#'   \item Metra - Commuter rail stations (excluding Wisconsin)
#'   \item Metro STL - MetroLink light rail stations
#' }
#'
#' CUMTD and Pace are bus-only agencies and have no rail stations.
#'
#' @examples
#' \dontrun{
#' rail_stations <- identify_all_rail_stations(
#'   all_stops,
#'   all_routes,
#'   all_stop_times
#' )
#'
#' # Count by agency
#' rail_stations[, .N, by = agency]
#' }
#'
#' @export
identify_all_rail_stations <- function(stops_dt, routes_dt, stop_times_dt) {
  # Identify stations by agency
  cta_rail <- identify_cta_rail_stations(stops_dt)
  metra_rail <- identify_metra_rail_stations(stops_dt)
  metro_stl_rail <- identify_metro_stl_metrolink_stations(stops_dt, routes_dt, stop_times_dt)

  # Combine all rail stations
  rail_stops <- unique(rbindlist(
    list(cta_rail, metra_rail, metro_stl_rail),
    fill = TRUE
  ))

  # Add type field
  rail_stops[, type := "rail"]

  return(rail_stops)
}

#' Identify Weekday Service IDs
#'
#' Filters calendar data to find service_ids that operate Monday-Friday.
#'
#' @param calendar_dt data.table with columns: service_id, agency, monday,
#'   tuesday, wednesday, thursday, friday
#'
#' @return data.table with columns: service_id, agency
#'
#' @details
#' Returns service_ids where all weekdays (Mon-Fri) have value 1, indicating
#' service operates all weekdays.
#'
#' This is used to filter trips to weekday peak periods for hub/corridor analysis.
#'
#' @export
identify_weekday_services <- function(calendar_dt) {
  calendar_dt[
    monday == 1 & tuesday == 1 & wednesday == 1 & thursday == 1 & friday == 1,
    .(service_id, agency)
  ]
}

#' Identify Bus Routes
#'
#' Filters routes data to bus routes only (route_type = 3).
#'
#' @param routes_dt data.table with columns: route_type, unique_route_id, agency
#'
#' @return data.table with columns: unique_route_id, agency
#'
#' @details
#' GTFS route_type values:
#' \itemize{
#'   \item 0 = Tram/Light Rail
#'   \item 1 = Subway/Metro
#'   \item 2 = Rail (heavy rail)
#'   \item 3 = Bus
#'   \item 4 = Ferry
#'   ...
#' }
#'
#' This function returns only type 3 (bus) routes.
#'
#' @export
identify_bus_routes <- function(routes_dt) {
  routes_dt[route_type == 3, .(unique_route_id, agency)]
}

#' Get Weekday Bus Trips
#'
#' Filters trips to weekday bus service only.
#'
#' @param trips_dt data.table with columns: service_id, agency, unique_route_id,
#'   unique_trip_id, (optionally direction_id)
#' @param weekday_services data.table from identify_weekday_services()
#' @param bus_routes data.table from identify_bus_routes()
#'
#' @return data.table with weekday bus trips, including direction_id if available
#'
#' @details
#' Performs inner joins to get trips that are:
#' \enumerate{
#'   \item On bus routes (route_type = 3)
#'   \item Operating on weekdays (Mon-Fri service)
#' }
#'
#' Preserves direction_id field if present in original trips data.
#'
#' @export
get_weekday_bus_trips <- function(trips_dt, weekday_services, bus_routes) {
  # Merge with weekday services
  weekday_trips <- merge(trips_dt, weekday_services, by = c("service_id", "agency"))

  # Filter to bus routes
  weekday_bus_trips <- weekday_trips[unique_route_id %in% bus_routes$unique_route_id]

  # Select relevant columns
  if ("direction_id" %in% names(weekday_bus_trips)) {
    result <- weekday_bus_trips[, .(unique_trip_id, unique_route_id, direction_id, agency)]
  } else {
    result <- weekday_bus_trips[, .(unique_trip_id, unique_route_id, agency)]
    result[, direction_id := NA_integer_]
  }

  return(result)
}

#' Get Bus Stops for Clustering
#'
#' Identifies unique bus stops that need clustering based on peak period service.
#'
#' @param am_peak_stops data.table with column: unique_stop_id
#' @param pm_peak_stops data.table with column: unique_stop_id
#' @param all_stops data.table with all stop details
#'
#' @return data.table with bus stops serving either peak period
#'
#' @details
#' Returns stops that appear in AM peak, PM peak, or both.
#' These are the stops that need spatial clustering to identify bus hubs.
#'
#' @export
get_bus_stops_for_clustering <- function(am_peak_stops, pm_peak_stops, all_stops) {
  peak_stop_ids <- unique(c(am_peak_stops$unique_stop_id, pm_peak_stops$unique_stop_id))
  all_stops[unique_stop_id %in% peak_stop_ids]
}

#' Determine Grouping Columns for Metrics
#'
#' Decides which columns to use for grouping based on direction_id availability.
#'
#' @param stop_times_dt data.table that may contain direction_id
#' @param base_cols Character vector. Base grouping columns (e.g., c("cluster_id", "agency"))
#'
#' @return Character vector of grouping columns
#'
#' @details
#' If direction_id exists and has non-NA values, includes it in grouping.
#' Otherwise, uses only base columns.
#'
#' Direction-aware grouping prevents double-counting bidirectional routes.
#'
#' @export
determine_grouping_cols <- function(stop_times_dt, base_cols) {
  has_direction <- "direction_id" %in% names(stop_times_dt) &&
                   sum(!is.na(stop_times_dt$direction_id)) > 0

  if (has_direction) {
    return(c(base_cols[1], "direction_id", base_cols[-1]))
  } else {
    return(base_cols)
  }
}
