# Hub Processing Functions
#
# Complete workflow for identifying transit hubs (rail stations and bus hubs)
# that qualify under the People Over Parking Act.
#
# High-Level Functions:
#   - identify_all_hubs(): Complete hub identification workflow (rail + bus)
#   - identify_rail_hubs(): Identify all rail transit stations
#   - identify_bus_hubs(): Complete bus hub workflow with clustering and verification
#
# Helper Functions:
#   - prepare_peak_stop_times(): Filter stop times to AM/PM peak periods
#   - calculate_bus_hub_metrics(): Calculate frequency metrics and apply qualification
#   - format_hub_diagnostics(): Add departure times, route lists, and directions
#   - format_itime(): Convert ITime to 12-hour format string

#' Convert ITime to 12-Hour Format String
#'
#' Helper function to convert ITime objects to human-readable 12-hour format.
#'
#' @param itime_obj An ITime object (seconds since midnight)
#' @return Character string in format "HH:MM AM/PM"
format_itime <- function(itime_obj) {
  hours <- itime_obj %/% 3600
  minutes <- (itime_obj %% 3600) %/% 60

  # Convert to 12-hour format
  period <- ifelse(hours < 12, "AM", "PM")
  display_hours <- ifelse(hours == 0, 12, ifelse(hours > 12, hours - 12, hours))

  sprintf("%02d:%02d %s", display_hours, minutes, period)
}

#' Identify Rail Transit Hubs
#'
#' Identifies all rail transit stations from CTA, Metra, and Metro St. Louis.
#' For CTA: Uses parent_station and location_type to identify stations/platforms.
#' For Metra: All stops are rail stations (filtered to exclude Wisconsin stations).
#' For Metro STL: Identifies MetroLink light rail stations (route_type=2).
#'
#' @param all_stops Combined stops data.table
#' @param all_routes Combined routes data.table
#' @param all_stop_times Combined stop_times data.table
#' @return data.table of rail stations with type="rail"
#'
#' @examples
#' \dontrun{
#' rail_hubs <- identify_rail_hubs(all_stops, all_routes, all_stop_times)
#' }
identify_rail_hubs <- function(all_stops, all_routes, all_stop_times) {
  # CTA rail stops with parent_station (platforms)
  cta_rail_stops_parents <- all_stops[
    agency == "cta" &
    (!is.na(parent_station) & parent_station != "") &
    (is.na(location_type) | location_type == 0),
    .(unique_stop_id, stop_id, stop_name, stop_lat, stop_lon, agency)
  ]

  # CTA rail stations (location_type = 1)
  cta_rail_stations <- all_stops[
    agency == "cta" &
    (!is.na(location_type) & location_type == 1),
    .(unique_stop_id, stop_id, stop_name, stop_lat, stop_lon, agency)
  ]

  # Metra rail stops (filter out Wisconsin stations at latitude > 42.5)
  metra_rail_stops <- all_stops[
    agency == "metra" & stop_lat <= 42.5,
    .(unique_stop_id, stop_id, stop_name, stop_lat, stop_lon, agency)
  ]

  # Metro St. Louis MetroLink stations (route_type = 2 for light rail)
  metro_stl_rail_route_ids <- all_routes[agency == "metro_stl" & route_type == 2, unique_route_id]
  metro_stl_rail_stop_ids <- all_stop_times[
    agency == "metro_stl" & unique_route_id %in% metro_stl_rail_route_ids,
    unique(unique_stop_id)
  ]
  metro_stl_rail_stops <- all_stops[
    agency == "metro_stl" & unique_stop_id %in% metro_stl_rail_stop_ids,
    .(unique_stop_id, stop_id, stop_name, stop_lat, stop_lon, agency)
  ]

  # Combine all rail stations
  rail_stops <- unique(rbindlist(list(
    cta_rail_stops_parents,
    cta_rail_stations,
    metra_rail_stops,
    metro_stl_rail_stops
  ), fill = TRUE))
  rail_stops[, type := "rail"]

  cat(sprintf("Identified %d rail transit stations\n", nrow(rail_stops)))

  return(rail_stops)
}

#' Prepare Peak Period Stop Times
#'
#' Filters stop times to AM peak (7-9am) and PM peak (4-6pm) periods.
#' Handles times >= 24:00:00 by filtering to same-day service only.
#' Adds direction_id from trips data if available.
#'
#' @param all_stop_times Combined stop_times data.table with arrival_time
#' @param weekday_bus_trips Weekday bus trips with direction_id (if available)
#' @return List with am_peak_bus_stops and pm_peak_bus_stops data.tables
prepare_peak_stop_times <- function(all_stop_times, weekday_bus_trips) {
  # Define peak hours (2 hours each = 120 minutes)
  morning_peak_start <- as.ITime("07:00:00")
  morning_peak_end <- as.ITime("09:00:00")
  evening_peak_start <- as.ITime("16:00:00")
  evening_peak_end <- as.ITime("18:00:00")

  # Process stop times - filter out times >= 24:00:00
  all_stop_times[, arrival_time_hhmmss := substr(arrival_time, 1, 8)]
  all_stop_times[, arrival_hour := as.integer(substr(arrival_time, 1, 2))]

  # Only process times < 24 hours (same-day service)
  all_stop_times_same_day <- all_stop_times[arrival_hour < 24]
  all_stop_times_same_day[, arrival_time_obj := as.ITime(arrival_time_hhmmss, format="%H:%M:%S")]

  # Split into AM and PM
  am_stop_times <- all_stop_times_same_day[
    arrival_time_obj >= morning_peak_start & arrival_time_obj <= morning_peak_end
  ]
  pm_stop_times <- all_stop_times_same_day[
    arrival_time_obj >= evening_peak_start & arrival_time_obj <= evening_peak_end
  ]

  # Add direction_id if available in trips data
  weekday_bus_trips_select <- if ("direction_id" %in% names(weekday_bus_trips)) {
    weekday_bus_trips[, .(unique_trip_id, unique_route_id, direction_id, agency)]
  } else {
    wbt <- weekday_bus_trips[, .(unique_trip_id, unique_route_id, agency)]
    wbt[, direction_id := NA_integer_]
    wbt
  }

  # Merge with bus trips
  am_peak_bus_stops <- if ("direction_id" %in% names(weekday_bus_trips_select)) {
    merge(am_stop_times,
          weekday_bus_trips_select[, .(unique_trip_id, direction_id, agency)],
          by = c("unique_trip_id", "agency"))
  } else {
    am_st <- copy(am_stop_times)
    am_st[, direction_id := NA_integer_]
    am_st
  }

  pm_peak_bus_stops <- if ("direction_id" %in% names(weekday_bus_trips_select)) {
    merge(pm_stop_times,
          weekday_bus_trips_select[, .(unique_trip_id, direction_id, agency)],
          by = c("unique_trip_id", "agency"))
  } else {
    pm_st <- copy(pm_stop_times)
    pm_st[, direction_id := NA_integer_]
    pm_st
  }

  # Filter to only bus trips
  am_peak_bus_stops <- am_peak_bus_stops[unique_trip_id %in% weekday_bus_trips_select$unique_trip_id]
  pm_peak_bus_stops <- pm_peak_bus_stops[unique_trip_id %in% weekday_bus_trips_select$unique_trip_id]

  return(list(
    am_peak_bus_stops = am_peak_bus_stops,
    pm_peak_bus_stops = pm_peak_bus_stops
  ))
}

#' Calculate Bus Hub Metrics and Apply Qualification
#'
#' Calculates AM/PM frequency metrics for bus stop clusters.
#' Applies hub qualification criteria: 2+ routes and <=15 min frequency
#' in EITHER AM or PM peak. Verifies route overlap at qualifying clusters.
#'
#' @param am_peak_bus_stops AM peak stop times with cluster_id
#' @param pm_peak_bus_stops PM peak stop times with cluster_id
#' @param bus_stops_clustered Clustered bus stops with cluster_id
#' @return data.table of qualifying clusters with metrics
calculate_bus_hub_metrics <- function(am_peak_bus_stops, pm_peak_bus_stops, bus_stops_clustered) {
  # Determine if we have direction data
  has_direction <- "direction_id" %in% names(am_peak_bus_stops) &&
                   sum(!is.na(am_peak_bus_stops$direction_id)) > 0

  grouping_cols <- if (has_direction) {
    c("cluster_id", "direction_id", "agency")
  } else {
    c("cluster_id", "agency")
  }

  # Calculate AM peak metrics
  am_routes_at_cluster <- am_peak_bus_stops[, .(
    num_routes_am = uniqueN(unique_route_id)
  ), by = grouping_cols]

  am_trips_at_cluster <- am_peak_bus_stops[, .(
    trips_am = .N
  ), by = grouping_cols]

  am_metrics <- merge(am_routes_at_cluster, am_trips_at_cluster, by = grouping_cols)
  am_metrics[, interval_am := 120 / trips_am]

  # Calculate PM peak metrics
  pm_routes_at_cluster <- pm_peak_bus_stops[, .(
    num_routes_pm = uniqueN(unique_route_id)
  ), by = grouping_cols]

  pm_trips_at_cluster <- pm_peak_bus_stops[, .(
    trips_pm = .N
  ), by = grouping_cols]

  pm_metrics <- merge(pm_routes_at_cluster, pm_trips_at_cluster, by = grouping_cols)
  pm_metrics[, interval_pm := 120 / trips_pm]

  # Combine AM and PM metrics
  all_peak_metrics <- merge(am_metrics, pm_metrics, by = grouping_cols, all = TRUE)

  # Fill NAs
  all_peak_metrics[is.na(num_routes_am), num_routes_am := 0]
  all_peak_metrics[is.na(num_routes_pm), num_routes_pm := 0]
  all_peak_metrics[is.na(trips_am), trips_am := 0]
  all_peak_metrics[is.na(trips_pm), trips_pm := 0]
  all_peak_metrics[is.na(interval_am), interval_am := Inf]
  all_peak_metrics[is.na(interval_pm), interval_pm := Inf]

  # Calculate combined metrics
  all_peak_metrics[, num_routes_total := pmax(num_routes_am, num_routes_pm)]
  all_peak_metrics[, trips_total := trips_am + trips_pm]
  all_peak_metrics[, interval_combined := 240 / trips_total]

  # Apply hub qualification: 2+ routes AND <=15 min frequency in EITHER period
  all_peak_metrics[, qualifies_routes := num_routes_am >= 2 | num_routes_pm >= 2]
  all_peak_metrics[, qualifies_frequency := interval_am <= 15 | interval_pm <= 15]
  all_peak_metrics[, qualifies_hub := qualifies_routes & qualifies_frequency]

  qualifying_clusters <- all_peak_metrics[qualifies_hub == TRUE]

  cat(sprintf("Found %d qualifying bus hub clusters (before overlap verification)\n",
              nrow(qualifying_clusters)))

  # Verify route overlap at qualifying clusters
  all_peak_bus_stops <- rbindlist(list(am_peak_bus_stops, pm_peak_bus_stops))

  cat("Verifying route overlap at qualifying clusters...\n")
  cluster_overlap_results <- verify_route_overlap_at_cluster(
    cluster_stops_dt = bus_stops_clustered[cluster_id %in% qualifying_clusters$cluster_id],
    stop_times_dt = all_peak_bus_stops[cluster_id %in% qualifying_clusters$cluster_id]
  )

  # Filter to only clusters where routes actually overlap
  qualifying_clusters <- merge(
    qualifying_clusters,
    cluster_overlap_results[, .(cluster_id, has_overlap, shared_streets)],
    by = "cluster_id"
  )
  qualifying_clusters <- qualifying_clusters[has_overlap == TRUE]

  cat(sprintf("Found %d qualifying bus hub clusters (after overlap verification)\n",
              nrow(qualifying_clusters)))

  return(list(
    qualifying_clusters = qualifying_clusters,
    has_direction = has_direction
  ))
}

#' Format Hub Diagnostics
#'
#' Adds diagnostic information to qualifying hubs including:
#' - First/last departure times for AM and PM peaks
#' - Route lists (with display names)
#' - Direction information (if available)
#'
#' @param qualifying_clusters Qualifying clusters with metrics
#' @param bus_stops_clustered Clustered bus stops
#' @param am_peak_bus_stops AM peak stop times
#' @param pm_peak_bus_stops PM peak stop times
#' @param all_routes Combined routes data.table
#' @param has_direction Logical indicating if direction_id is available
#' @return data.table of qualifying bus hubs with diagnostic info
format_hub_diagnostics <- function(qualifying_clusters, bus_stops_clustered,
                                   am_peak_bus_stops, pm_peak_bus_stops,
                                   all_routes, has_direction) {
  # Map clusters back to individual stops
  qualifying_stop_ids <- bus_stops_clustered[
    cluster_id %in% qualifying_clusters$cluster_id,
    .(unique_stop_id, cluster_id, agency, stop_id, stop_name, stop_lat, stop_lon)
  ]

  # Add metrics to stops
  if (has_direction) {
    stop_directions <- unique(rbind(
      am_peak_bus_stops[, .(unique_stop_id, direction_id)],
      pm_peak_bus_stops[, .(unique_stop_id, direction_id)]
    ))
    qualifying_stop_ids <- merge(qualifying_stop_ids, stop_directions,
                                 by = "unique_stop_id", allow.cartesian = TRUE)
    qualifying_stop_ids <- merge(qualifying_stop_ids, qualifying_clusters,
                                 by = c("cluster_id", "direction_id", "agency"),
                                 all.x = TRUE)
  } else {
    qualifying_stop_ids <- merge(qualifying_stop_ids, qualifying_clusters,
                                 by = c("cluster_id", "agency"), all.x = TRUE)
  }

  # Calculate departure times
  am_cluster_times <- am_peak_bus_stops[cluster_id %in% qualifying_clusters$cluster_id, .(
    first_departure_am = format_itime(min(arrival_time_obj)),
    last_departure_am = format_itime(max(arrival_time_obj))
  ), by = cluster_id]

  pm_cluster_times <- pm_peak_bus_stops[cluster_id %in% qualifying_clusters$cluster_id, .(
    first_departure_pm = format_itime(min(arrival_time_obj)),
    last_departure_pm = format_itime(max(arrival_time_obj))
  ), by = cluster_id]

  # Calculate route lists and directions
  all_peak_bus_stops <- rbindlist(list(am_peak_bus_stops, pm_peak_bus_stops))
  cluster_routes <- all_peak_bus_stops[cluster_id %in% qualifying_clusters$cluster_id, {
    route_ids <- unique(unique_route_id)
    route_info <- all_routes[unique_route_id %in% route_ids,
                             .(unique_route_id, route_short_name, route_long_name)]

    route_names <- if (nrow(route_info) > 0) {
      sapply(route_info$unique_route_id, function(rid) {
        r <- route_info[unique_route_id == rid]
        if (!is.na(r$route_short_name) && r$route_short_name != "") {
          r$route_short_name
        } else if (!is.na(r$route_long_name)) {
          r$route_long_name
        } else {
          gsub("^[^_]+_", "", rid)
        }
      })
    } else {
      sapply(route_ids, function(rid) gsub("^[^_]+_", "", rid))
    }

    routes_list <- paste(sort(unique(route_names)), collapse = ", ")

    # Get directions if available
    if ("direction_id" %in% names(.SD) && sum(!is.na(direction_id)) > 0) {
      directions <- unique(direction_id[!is.na(direction_id)])
      direction_labels <- sapply(directions, function(d) {
        if (d == 0) "Outbound" else if (d == 1) "Inbound" else as.character(d)
      })
      directions_list <- paste(sort(direction_labels), collapse = ", ")
    } else {
      directions_list <- "N/A"
    }

    list(routes = routes_list, directions = directions_list)
  }, by = cluster_id]

  # Merge all diagnostics
  qualifying_stop_ids <- merge(qualifying_stop_ids, am_cluster_times,
                               by = "cluster_id", all.x = TRUE)
  qualifying_stop_ids <- merge(qualifying_stop_ids, pm_cluster_times,
                               by = "cluster_id", all.x = TRUE)
  qualifying_stop_ids <- merge(qualifying_stop_ids, cluster_routes,
                               by = "cluster_id", all.x = TRUE)

  qualifying_bus_hubs <- qualifying_stop_ids[, .(
    unique_stop_id, stop_id, stop_name, stop_lat, stop_lon, agency,
    cluster_id, num_routes_am, num_routes_pm, num_routes_total,
    trips_am, trips_pm, trips_total,
    interval_am, interval_pm, interval_combined,
    first_departure_am, last_departure_am,
    first_departure_pm, last_departure_pm,
    routes, directions
  )]
  qualifying_bus_hubs[, type := "bus_hub"]

  return(qualifying_bus_hubs)
}

#' Identify Bus Hubs
#'
#' Complete workflow for identifying qualifying bus hubs:
#' 1. Identify weekday bus services and trips
#' 2. Filter to AM/PM peak periods
#' 3. Cluster bus stops spatially (150 ft radius)
#' 4. Calculate frequency metrics
#' 5. Apply qualification criteria (2+ routes, <=15 min frequency)
#' 6. Verify route overlap at intersections
#' 7. Format diagnostic information
#'
#' @param all_stops Combined stops data.table
#' @param all_routes Combined routes data.table
#' @param all_trips Combined trips data.table
#' @param all_stop_times Combined stop_times data.table
#' @param all_calendar Combined calendar data.table
#' @param all_calendar_dates Combined calendar_dates data.table
#' @return data.table of qualifying bus hubs with metrics and diagnostics
#'
#' @examples
#' \dontrun{
#' bus_hubs <- identify_bus_hubs(all_stops, all_routes, all_trips,
#'                                all_stop_times, all_calendar, all_calendar_dates)
#' }
identify_bus_hubs <- function(all_stops, all_routes, all_trips,
                               all_stop_times, all_calendar, all_calendar_dates) {
  cat("\n=== Identifying Bus Hubs ===\n\n")

  # Identify weekday services
  weekday_service <- identify_weekday_services(all_calendar, all_calendar_dates)

  # Get weekday bus trips
  bus_routes <- all_routes[route_type == 3, .(unique_route_id, agency)]
  weekday_bus_trips <- merge(all_trips, weekday_service, by = c("service_id", "agency"))
  weekday_bus_trips <- weekday_bus_trips[unique_route_id %in% bus_routes$unique_route_id]

  cat(sprintf("Found %d weekday bus trips\n", nrow(weekday_bus_trips)))

  # Prepare peak period stop times
  peak_stops <- prepare_peak_stop_times(all_stop_times, weekday_bus_trips)
  am_peak_bus_stops <- peak_stops$am_peak_bus_stops
  pm_peak_bus_stops <- peak_stops$pm_peak_bus_stops

  cat(sprintf("AM peak stops: %d\n", nrow(am_peak_bus_stops)))
  cat(sprintf("PM peak stops: %d\n", nrow(pm_peak_bus_stops)))

  # Get bus stops that need clustering
  bus_stops_for_clustering <- all_stops[
    unique_stop_id %in% c(am_peak_bus_stops$unique_stop_id, pm_peak_bus_stops$unique_stop_id)
  ]

  # Apply spatial clustering (150 ft radius)
  cat("Clustering bus stops (150 ft radius)...\n")
  bus_stops_clustered <- cluster_stops_spatial(bus_stops_for_clustering, cluster_radius_ft = 150)
  cat(sprintf("Created %d clusters from %d bus stops\n",
              uniqueN(bus_stops_clustered$cluster_id),
              nrow(bus_stops_clustered)))

  # Add cluster_id to stop times
  am_peak_bus_stops <- merge(am_peak_bus_stops,
                             bus_stops_clustered[, .(unique_stop_id, cluster_id)],
                             by = "unique_stop_id")
  pm_peak_bus_stops <- merge(pm_peak_bus_stops,
                             bus_stops_clustered[, .(unique_stop_id, cluster_id)],
                             by = "unique_stop_id")

  # Calculate metrics and apply qualification
  hub_results <- calculate_bus_hub_metrics(am_peak_bus_stops, pm_peak_bus_stops,
                                           bus_stops_clustered)
  qualifying_clusters <- hub_results$qualifying_clusters
  has_direction <- hub_results$has_direction

  # Format diagnostics
  qualifying_bus_hubs <- format_hub_diagnostics(
    qualifying_clusters, bus_stops_clustered,
    am_peak_bus_stops, pm_peak_bus_stops,
    all_routes, has_direction
  )

  cat(sprintf("\nTotal qualifying bus hub stops: %d\n", nrow(qualifying_bus_hubs)))

  return(list(
    qualifying_bus_hubs = qualifying_bus_hubs,
    am_peak_bus_stops = am_peak_bus_stops,
    pm_peak_bus_stops = pm_peak_bus_stops
  ))
}

#' Identify All Transit Hubs
#'
#' Complete workflow for identifying all transit hubs (rail + bus).
#' Combines rail stations and qualifying bus hubs into a single sf object.
#'
#' @param all_stops Combined stops data.table
#' @param all_routes Combined routes data.table
#' @param all_trips Combined trips data.table
#' @param all_stop_times Combined stop_times data.table
#' @param all_calendar Combined calendar data.table
#' @param all_calendar_dates Combined calendar_dates data.table
#' @return List containing:
#'   - all_hubs_sf: sf object with all transit hubs (rail + bus)
#'   - am_peak_bus_stops: AM peak bus stops (for corridor processing)
#'   - pm_peak_bus_stops: PM peak bus stops (for corridor processing)
#'
#' @examples
#' \dontrun{
#' hub_results <- identify_all_hubs(all_stops, all_routes, all_trips,
#'                                   all_stop_times, all_calendar, all_calendar_dates)
#' all_hubs_sf <- hub_results$all_hubs_sf
#' }
identify_all_hubs <- function(all_stops, all_routes, all_trips,
                              all_stop_times, all_calendar, all_calendar_dates) {
  # Identify rail hubs
  rail_stops <- identify_rail_hubs(all_stops, all_routes, all_stop_times)

  # Identify bus hubs (returns list with bus hubs and peak stop times)
  bus_hub_results <- identify_bus_hubs(all_stops, all_routes, all_trips,
                                       all_stop_times, all_calendar, all_calendar_dates)

  # Combine all hubs
  all_hubs <- rbindlist(list(rail_stops, bus_hub_results$qualifying_bus_hubs), fill = TRUE)

  # Create spatial object
  all_hubs_sf <- st_as_sf(all_hubs, coords = c("stop_lon", "stop_lat"), crs = 4326)

  # Add agency labels
  all_hubs_sf$agency_name <- factor(
    all_hubs_sf$agency,
    levels = c("cta", "pace", "metra", "metro_stl", "cumtd", "rmtd"),
    labels = c("CTA", "Pace", "Metra", "Metro STL", "MTD", "RMTD")
  )

  cat(sprintf("\n=== Hub Identification Complete ===\n"))
  cat(sprintf("Total hubs: %d (Rail: %d, Bus: %d)\n",
              nrow(all_hubs), nrow(rail_stops), nrow(bus_hub_results$qualifying_bus_hubs)))

  return(list(
    all_hubs_sf = all_hubs_sf,
    am_peak_bus_stops = bus_hub_results$am_peak_bus_stops,
    pm_peak_bus_stops = bus_hub_results$pm_peak_bus_stops
  ))
}
