#' Calculate Peak Period Frequency Metrics
#'
#' Calculates frequency (service interval) for transit routes during peak periods
#' by counting trips and computing average headway.
#'
#' @param stop_times_dt data.table with columns: unique_route_id, (optionally direction_id)
#' @param grouping_cols Character vector. Columns to group by (e.g., c("cluster_id", "direction_id", "agency"))
#' @param peak_duration_minutes Numeric. Duration of peak period in minutes (default: 120 for 2-hour peak)
#' @param period_name Character. Name of period for column naming ("am" or "pm")
#'
#' @return data.table with columns:
#'   \describe{
#'     \item{<grouping_cols>}{Grouping columns passed in}
#'     \item{num_routes_<period>}{Integer. Number of unique routes}
#'     \item{trips_<period>}{Integer. Number of trips during period}
#'     \item{interval_<period>}{Numeric. Average service interval in minutes (peak_duration / trips)}
#'   }
#'
#' @details
#' Frequency is calculated as: \code{interval = peak_duration_minutes / number_of_trips}
#'
#' For example, with a 120-minute peak period:
#' \itemize{
#'   \item 8 trips → 120/8 = 15 minute interval
#'   \item 16 trips → 120/16 = 7.5 minute interval
#'   \item 1 trip → 120/1 = 120 minute interval (very infrequent)
#' }
#'
#' @section Direction Handling:
#' If \code{direction_id} is included in \code{grouping_cols}, frequency is calculated
#' separately per direction. This prevents double-counting bidirectional routes.
#'
#' For example, a route with 4 northbound and 4 southbound trips:
#' \itemize{
#'   \item With direction_id: 120/4 = 30 min each direction
#'   \item Without direction_id: 120/8 = 15 min combined (misleading!)
#' }
#'
#' @examples
#' \dontrun{
#' # Calculate AM peak frequency by cluster
#' am_metrics <- calculate_peak_frequency(
#'   am_peak_stop_times,
#'   grouping_cols = c("cluster_id", "agency"),
#'   peak_duration_minutes = 120,
#'   period_name = "am"
#' )
#'
#' # With direction_id
#' am_metrics <- calculate_peak_frequency(
#'   am_peak_stop_times,
#'   grouping_cols = c("cluster_id", "direction_id", "agency"),
#'   peak_duration_minutes = 120,
#'   period_name = "am"
#' )
#' }
#'
#' @export
calculate_peak_frequency <- function(stop_times_dt, grouping_cols,
                                      peak_duration_minutes = 120,
                                      period_name = "am") {
  # Count unique routes
  routes_col_name <- paste0("num_routes_", period_name)
  routes_at_location <- stop_times_dt[, .(
    count = uniqueN(unique_route_id)
  ), by = grouping_cols]
  data.table::setnames(routes_at_location, "count", routes_col_name)

  # Count trips (stop times)
  trips_col_name <- paste0("trips_", period_name)
  trips_at_location <- stop_times_dt[, .(
    count = .N
  ), by = grouping_cols]
  data.table::setnames(trips_at_location, "count", trips_col_name)

  # Merge and calculate interval
  metrics <- merge(routes_at_location, trips_at_location, by = grouping_cols)

  interval_col_name <- paste0("interval_", period_name)
  metrics[, (interval_col_name) := peak_duration_minutes / get(trips_col_name)]

  return(metrics)
}

#' Combine AM and PM Peak Metrics
#'
#' Merges AM and PM frequency calculations and computes combined metrics.
#'
#' @param am_metrics data.table from calculate_peak_frequency() for AM period
#' @param pm_metrics data.table from calculate_peak_frequency() for PM period
#' @param grouping_cols Character vector. Columns to merge on
#'
#' @return data.table with columns from both periods plus:
#'   \describe{
#'     \item{num_routes_total}{Integer. Maximum of AM or PM route counts}
#'     \item{trips_total}{Integer. Sum of AM + PM trips}
#'     \item{interval_combined}{Numeric. Combined interval (240 min / total trips)}
#'   }
#'
#' @details
#' Handles cases where a location only has service in one peak period by:
#' \itemize{
#'   \item Filling missing route counts with 0
#'   \item Filling missing trip counts with 0
#'   \item Setting missing intervals to Inf (no service)
#' }
#'
#' The combined interval assumes 240 minutes total (120 AM + 120 PM).
#'
#' @section Important:
#' \code{num_routes_total} uses \code{pmax} (not \code{sum}) because a route
#' operating in both periods should count once, not twice.
#'
#' @examples
#' \dontrun{
#' am_metrics <- calculate_peak_frequency(am_stops, c("cluster_id"), 120, "am")
#' pm_metrics <- calculate_peak_frequency(pm_stops, c("cluster_id"), 120, "pm")
#' combined <- combine_am_pm_metrics(am_metrics, pm_metrics, c("cluster_id"))
#' }
#'
#' @export
combine_am_pm_metrics <- function(am_metrics, pm_metrics, grouping_cols) {
  # Full outer join to include locations with only AM or only PM service
  all_metrics <- merge(am_metrics, pm_metrics, by = grouping_cols, all = TRUE)

  # Fill NAs for locations with service in only one period
  if ("num_routes_am" %in% names(all_metrics)) {
    all_metrics[is.na(num_routes_am), num_routes_am := 0]
  }
  if ("num_routes_pm" %in% names(all_metrics)) {
    all_metrics[is.na(num_routes_pm), num_routes_pm := 0]
  }
  if ("trips_am" %in% names(all_metrics)) {
    all_metrics[is.na(trips_am), trips_am := 0]
  }
  if ("trips_pm" %in% names(all_metrics)) {
    all_metrics[is.na(trips_pm), trips_pm := 0]
  }
  if ("interval_am" %in% names(all_metrics)) {
    all_metrics[is.na(interval_am), interval_am := Inf]
  }
  if ("interval_pm" %in% names(all_metrics)) {
    all_metrics[is.na(interval_pm), interval_pm := Inf]
  }

  # Calculate combined metrics
  all_metrics[, num_routes_total := pmax(num_routes_am, num_routes_pm, na.rm = TRUE)]
  all_metrics[, trips_total := trips_am + trips_pm]
  all_metrics[, interval_combined := 240 / trips_total]  # 240 = 120 + 120

  # Handle division by zero
  all_metrics[trips_total == 0, interval_combined := Inf]

  return(all_metrics)
}

#' Apply Hub Qualification Criteria
#'
#' Determines which locations qualify as transit hubs based on route count
#' and frequency thresholds.
#'
#' @param metrics_dt data.table with columns: num_routes_am, num_routes_pm,
#'   interval_am, interval_pm
#' @param min_routes Integer. Minimum routes required (default: 2)
#' @param max_interval_minutes Numeric. Maximum service interval (default: 15)
#'
#' @return data.table with original columns plus:
#'   \describe{
#'     \item{qualifies_routes}{Logical. TRUE if >=min_routes in either AM or PM}
#'     \item{qualifies_frequency}{Logical. TRUE if interval <=max_interval in either AM or PM}
#'     \item{qualifies_hub}{Logical. TRUE if both route and frequency criteria met}
#'   }
#'
#' @details
#' Illinois SB2111 qualification criteria:
#' \enumerate{
#'   \item \strong{Route count}: 2+ routes in at least one peak period (AM OR PM)
#'   \item \strong{Frequency}: 15-minute or better interval in at least one peak (AM OR PM)
#' }
#'
#' The "OR" logic (not "AND") is intentional:
#' \itemize{
#'   \item Captures directional flow (inbound AM, outbound PM)
#'   \item Reflects actual transit usage patterns
#'   \item More inclusive of genuine transit hubs
#' }
#'
#' @section Legislative Context:
#' From SB2111 (People Over Parking Act):
#' \emph{"An intersection of 2 or more bus routes with a combined frequency
#' of service interval of 15 minutes or less during peak commute periods"}
#'
#' "Peak commute periods" interpreted as either AM or PM, not necessarily both.
#'
#' @examples
#' \dontrun{
#' # Standard hub qualification (2 routes, 15 min)
#' qualified <- apply_hub_qualification(all_metrics)
#'
#' # More restrictive (3 routes, 10 min)
#' qualified <- apply_hub_qualification(
#'   all_metrics,
#'   min_routes = 3,
#'   max_interval_minutes = 10
#' )
#'
#' # Count qualifying hubs
#' nrow(qualified[qualifies_hub == TRUE])
#' }
#'
#' @export
apply_hub_qualification <- function(metrics_dt, min_routes = 2, max_interval_minutes = 15) {
  # Make a copy to avoid modifying original
  qualified <- data.table::copy(metrics_dt)

  # Route qualification: min_routes in EITHER AM or PM
  qualified[, qualifies_routes := num_routes_am >= min_routes | num_routes_pm >= min_routes]

  # Frequency qualification: max_interval in EITHER AM or PM
  qualified[, qualifies_frequency := interval_am <= max_interval_minutes | interval_pm <= max_interval_minutes]

  # Overall qualification: must meet BOTH criteria
  qualified[, qualifies_hub := qualifies_routes & qualifies_frequency]

  return(qualified)
}

#' Apply Corridor Qualification Criteria
#'
#' Determines which locations qualify as transit corridors based on frequency.
#'
#' @param metrics_dt data.table with columns: interval_am, interval_pm
#' @param max_interval_minutes Numeric. Maximum service interval (default: 15)
#'
#' @return data.table with original columns plus:
#'   \describe{
#'     \item{qualifies_corridor}{Logical. TRUE if interval <=max_interval in either AM or PM}
#'   }
#'
#' @details
#' Illinois SB2111 corridor qualification:
#' \emph{"A street on which there is one or more bus routes with a combined
#' frequency of bus service interval of 15 minutes or less"}
#'
#' Note: Corridors require only 1+ route (vs 2+ for hubs), but same frequency threshold.
#'
#' @examples
#' \dontrun{
#' # Standard corridor qualification (15 min)
#' qualified <- apply_corridor_qualification(corridor_metrics)
#'
#' # More restrictive (10 min)
#' qualified <- apply_corridor_qualification(
#'   corridor_metrics,
#'   max_interval_minutes = 10
#' )
#' }
#'
#' @export
apply_corridor_qualification <- function(metrics_dt, max_interval_minutes = 15) {
  # Make a copy to avoid modifying original
  qualified <- data.table::copy(metrics_dt)

  # Corridor qualification: max_interval in EITHER AM or PM
  qualified[, qualifies_corridor := interval_am <= max_interval_minutes | interval_pm <= max_interval_minutes]

  return(qualified)
}

#' Filter Peak Period Stop Times
#'
#' Filters stop_times data to only include arrivals during specified peak period.
#'
#' @param stop_times_dt data.table with columns: arrival_time
#' @param peak_start Character or ITime. Start of peak period (e.g., "07:00:00")
#' @param peak_end Character or ITime. End of peak period (e.g., "09:00:00")
#'
#' @return data.table filtered to peak period with added column:
#'   \describe{
#'     \item{arrival_time_obj}{ITime. Parsed arrival time}
#'   }
#'
#' @details
#' Handles GTFS times > 24:00:00 (overnight trips):
#' \itemize{
#'   \item Extracts hour component from arrival_time
#'   \item Filters out times >= 24 hours (next-day service)
#'   \item Only processes same-day service for peak period
#' }
#'
#' This ensures peak periods are in local time (Central Time for Illinois),
#' not wrapped into next calendar day.
#'
#' @examples
#' \dontrun{
#' # Filter to AM peak (7-9 AM)
#' am_stops <- filter_peak_period_stop_times(
#'   all_stop_times,
#'   "07:00:00",
#'   "09:00:00"
#' )
#'
#' # Filter to PM peak (4-6 PM)
#' pm_stops <- filter_peak_period_stop_times(
#'   all_stop_times,
#'   "16:00:00",
#'   "18:00:00"
#' )
#' }
#'
#' @export
filter_peak_period_stop_times <- function(stop_times_dt, peak_start, peak_end) {
  # Make a copy to avoid modifying original
  filtered <- data.table::copy(stop_times_dt)

  # Convert peak times to ITime if needed
  if (is.character(peak_start)) {
    peak_start <- data.table::as.ITime(peak_start)
  }
  if (is.character(peak_end)) {
    peak_end <- data.table::as.ITime(peak_end)
  }

  # Extract hour to filter out times >= 24:00:00 (next-day service)
  if (!"arrival_hour" %in% names(filtered)) {
    filtered[, arrival_time_hhmmss := substr(arrival_time, 1, 8)]
    filtered[, arrival_hour := as.integer(substr(arrival_time, 1, 2))]
  }

  # Only process same-day service (< 24 hours)
  filtered <- filtered[arrival_hour < 24]

  # Parse arrival time
  if (!"arrival_time_obj" %in% names(filtered)) {
    filtered[, arrival_time_obj := data.table::as.ITime(arrival_time_hhmmss, format = "%H:%M:%S")]
  }

  # Filter to peak period
  filtered <- filtered[arrival_time_obj >= peak_start & arrival_time_obj <= peak_end]

  return(filtered)
}


# =============================================================================
# Representative Service Selection Functions
# =============================================================================
# These functions implement the "representative service" approach to avoid
# inflating trip counts when agencies define the same schedule multiple times
# for different dates.
# =============================================================================

#' Get Representative Weekday Service IDs
#'
#' Selects representative weekday service_id(s) for frequency calculation.
#' This avoids inflating trip counts when agencies define the same schedule
#' multiple times for different dates in calendar_dates.txt.
#'
#' IMPORTANT: Some agencies (like PACE) organize services by division/garage,
#' resulting in multiple distinct service_ids that together represent full
#' weekday service. This function returns ALL such services to ensure complete
#' route coverage.
#'
#' @param calendar_dt data.table with calendar.txt data
#' @param calendar_dates_dt data.table with calendar_dates.txt data
#' @param trips_dt data.table with trips to filter (e.g., agency-specific trips)
#' @param target_date Date. Optional specific date to use. If NULL, finds next Tuesday.
#'
#' @return Character vector of service_id(s) to use for frequency calculations.
#'   Returns all weekday services when they represent different route groups,
#'   or a single service when that covers all routes.
#'
#' @details
#' The selection strategy is:
#' \enumerate{
#'   \item If calendar.txt has weekday services (Mon-Fri = 1):
#'     \itemize{
#'       \item If multiple weekday services cover different routes, return ALL of them
#'       \item If they overlap significantly, return the one with most trips
#'     }
#'   \item Otherwise, use calendar_dates.txt to find services running on a
#'         representative Tuesday (or target_date if provided)
#'   \item Last resort: use the most common service_id in trips_dt
#' }
#'
#' This approach ensures we count trips from all route groups while avoiding
#' double-counting when the same schedule is defined multiple times.
#'
#' @section Why Multiple Services?
#' Agencies like PACE organize GTFS by division (West, Northwest, South, etc.).
#' Each division has its own service_id, but they all represent weekday service.
#' Selecting only one would miss routes from other divisions.
#'
#' @examples
#' \dontrun{
#' # Get representative services for PACE
#' pace_trips <- all_trips[agency == "pace"]
#' rep_services <- get_representative_service(
#'   all_calendar[agency == "pace"],
#'   all_calendar_dates[agency == "pace"],
#'   pace_trips
#' )
#'
#' # Filter trips to weekday services (may be multiple)
#' pace_weekday_trips <- pace_trips[service_id %in% rep_services]
#' }
#'
#' @export
get_representative_service <- function(calendar_dt, calendar_dates_dt, trips_dt,
                                        target_date = NULL) {

  # Method 1: Use calendar.txt if it has clear weekday services
  if (nrow(calendar_dt) > 0) {
    weekday_services <- calendar_dt[
      monday == 1 & tuesday == 1 & wednesday == 1 & thursday == 1 & friday == 1
    ]

    if (nrow(weekday_services) > 0) {
      # Get all weekday services that have trips
      services_with_trips <- trips_dt[service_id %in% weekday_services$service_id,
                                      unique(service_id)]

      if (length(services_with_trips) > 0) {
        # Check if multiple services cover different routes (like PACE divisions)
        if (length(services_with_trips) > 1) {
          # Check route overlap between services
          routes_by_service <- trips_dt[service_id %in% services_with_trips,
                                        .(routes = list(unique(route_id))),
                                        by = service_id]

          # Calculate pairwise overlap
          all_routes <- unique(unlist(routes_by_service$routes))

          # Count how many services each route appears in
          route_coverage <- trips_dt[service_id %in% services_with_trips,
                                    .(n_services = uniqueN(service_id)),
                                    by = route_id]

          # If most routes appear in only 1 service, they're distinct divisions
          # Use threshold: if >50% of routes are unique to one service
          unique_routes <- sum(route_coverage$n_services == 1)
          total_routes <- nrow(route_coverage)

          if (unique_routes > total_routes * 0.5) {
            # Services cover different routes - return ALL weekday services
            cat(sprintf("  Found %d weekday services covering different route groups\n",
                        length(services_with_trips)))
            cat(sprintf("  Using ALL weekday services: %s\n",
                        paste(services_with_trips, collapse = ", ")))
            return(services_with_trips)
          }
        }

        # Single service or overlapping services - return the one with most trips
        trip_counts <- trips_dt[service_id %in% services_with_trips, .N, by = service_id]
        best_service <- trip_counts[which.max(N), service_id]
        cat(sprintf("  Using calendar.txt weekday service: %s\n", best_service))
        return(best_service)
      }
    }
  }

  # Method 2: Use calendar_dates.txt - find a representative Tuesday
  if (nrow(calendar_dates_dt) > 0) {
    # Convert date column if needed
    if (!"date_obj" %in% names(calendar_dates_dt)) {
      calendar_dates_dt <- copy(calendar_dates_dt)
      calendar_dates_dt[, date_obj := as.Date(as.character(date), format = "%Y%m%d")]
    }

    # Calculate weekday (1=Mon, 2=Tue, etc. using lubridate convention)
    if (!"weekday" %in% names(calendar_dates_dt)) {
      calendar_dates_dt[, weekday := lubridate::wday(date_obj, week_start = 1)]
    }

    if (is.null(target_date)) {
      # Find the next upcoming Tuesday (or most recent if none upcoming)
      today <- Sys.Date()

      # Find Tuesdays with service (exception_type=1 means service added)
      tuesdays <- calendar_dates_dt[exception_type == 1 & weekday == 2]

      if (nrow(tuesdays) > 0) {
        # Prefer upcoming Tuesday, fall back to most recent
        upcoming_tuesdays <- tuesdays[date_obj >= today]
        if (nrow(upcoming_tuesdays) > 0) {
          target_tuesday <- min(upcoming_tuesdays$date_obj)
        } else {
          target_tuesday <- max(tuesdays$date_obj)
        }

        service_on_tuesday <- tuesdays[date_obj == target_tuesday, service_id]

        cat(sprintf("  Representative Tuesday: %s\n", target_tuesday))
        cat(sprintf("  Service(s) on that day: %s\n", paste(service_on_tuesday, collapse = ", ")))

        # Return all services on that Tuesday (they may cover different routes)
        if (length(service_on_tuesday) >= 1) {
          return(service_on_tuesday)
        }
      }
    } else {
      # Use specific target date
      services_on_date <- calendar_dates_dt[
        exception_type == 1 & date_obj == target_date,
        service_id
      ]

      if (length(services_on_date) > 0) {
        return(services_on_date)
      }
    }

    # Fallback: find service with most weekday dates
    weekday_services <- calendar_dates_dt[exception_type == 1 & weekday >= 1 & weekday <= 5]
    service_weekday_counts <- weekday_services[, .N, by = service_id]
    if (nrow(service_weekday_counts) > 0) {
      best_service <- service_weekday_counts[which.max(N), service_id]
      cat(sprintf("  Fallback: using service with most weekday dates: %s\n", best_service))
      return(best_service)
    }
  }

  # Last resort: use most common service in trips
  trip_counts <- trips_dt[, .N, by = service_id]
  if (nrow(trip_counts) > 0) {
    best_service <- trip_counts[which.max(N), service_id]
    cat(sprintf("  Last resort: using most common service: %s\n", best_service))
    return(best_service)
  }

  warning("Could not determine representative service - returning NULL")
  return(NULL)
}


#' Get Representative Services for All Agencies
#'
#' Applies representative service selection to each agency's data.
#' Returns a lookup table mapping agency -> representative service_id(s).
#'
#' @param all_calendar Combined calendar data.table
#' @param all_calendar_dates Combined calendar_dates data.table
#' @param all_trips Combined trips data.table
#'
#' @return data.table with columns: agency, representative_service_ids (list column)
#'
#' @details
#' For each agency, determines the service_id(s) to use for frequency calculations.
#' Some agencies (like PACE) have multiple weekday services for different route
#' groups - this function returns all of them to ensure complete coverage.
#'
#' @examples
#' \dontrun{
#' rep_services <- get_representative_services_by_agency(
#'   all_calendar, all_calendar_dates, all_trips
#' )
#' print(rep_services)
#' #    agency representative_service_ids
#' # 1:    cta                    weekday
#' # 2:   pace  2025-08-WE-Weekday-01,...  (multiple)
#' # 3:   smtd                   c74850_1
#' }
#'
#' @export
get_representative_services_by_agency <- function(all_calendar, all_calendar_dates, all_trips) {

  agencies <- unique(all_trips$agency)

  result <- rbindlist(lapply(agencies, function(agency_id) {
    cat(sprintf("\nDetermining representative service for %s:\n", toupper(agency_id)))

    agency_calendar <- all_calendar[agency == agency_id]
    agency_calendar_dates <- all_calendar_dates[agency == agency_id]
    agency_trips <- all_trips[agency == agency_id]

    rep_services <- get_representative_service(
      agency_calendar,
      agency_calendar_dates,
      agency_trips
    )

    # Store as list to handle multiple service_ids
    data.table(
      agency = agency_id,
      representative_service_ids = list(rep_services)
    )
  }))

  return(result)
}


#' Filter Trips to Representative Weekday Service
#'
#' Filters trips to only include those running on the representative
#' weekday service(s) for each agency.
#'
#' @param all_trips Combined trips data.table
#' @param representative_services data.table from get_representative_services_by_agency()
#'   with columns: agency, representative_service_ids (list column)
#'
#' @return data.table with trips filtered to representative services
#'
#' @details
#' This is the key function that applies the representative service
#' selection to filter trips before frequency calculation. Some agencies
#' (like PACE) have multiple weekday services covering different route
#' groups - this function includes trips from ALL representative services.
#'
#' @examples
#' \dontrun{
#' rep_services <- get_representative_services_by_agency(
#'   all_calendar, all_calendar_dates, all_trips
#' )
#' weekday_trips <- filter_trips_to_representative_service(all_trips, rep_services)
#' }
#'
#' @export
filter_trips_to_representative_service <- function(all_trips, representative_services) {

  # Build a lookup of all valid (agency, service_id) combinations
  valid_services <- representative_services[, .(
    service_id = unlist(representative_service_ids)
  ), by = agency]

  # Filter trips to only those matching valid agency/service combinations
  filtered_trips <- all_trips[
    paste(agency, service_id) %in% paste(valid_services$agency, valid_services$service_id)
  ]
  
  cat(sprintf("\nFiltered to representative services: %d / %d trips (%.1f%%)\n",
              nrow(filtered_trips), nrow(all_trips),
              100 * nrow(filtered_trips) / nrow(all_trips)))
  
  return(filtered_trips)
}
