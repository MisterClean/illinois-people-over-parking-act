# GTFS Processing Functions
#
# Complete workflow for downloading, normalizing, validating, and combining
# GTFS data from multiple transit agencies.
#
# High-Level Functions:
#   - process_all_gtfs_data(): Complete GTFS workflow for all agencies
#
# Helper Functions:
#   - convert_integer64(): Convert integer64 columns to appropriate types
#   - standardize_calendar_columns(): Standardize calendar table structure
#   - standardize_calendar_dates_columns(): Standardize calendar_dates structure
#   - combine_gtfs_tables(): Combine GTFS tables from multiple agencies
#   - enrich_stop_times(): Add route_type and clean stop times

#' Convert integer64 Columns to Appropriate Types
#'
#' Converts integer64 columns in a data.table to regular integer or character
#' types. Large values that exceed integer.max are converted to character,
#' while smaller values are converted to regular integers.
#'
#' @param dt A data.table with potential integer64 columns
#' @return The modified data.table with integer64 columns converted
#'
#' @examples
#' \dontrun{
#' dt <- convert_integer64(my_gtfs_table)
#' }
convert_integer64 <- function(dt) {
  for (col in names(dt)) {
    if (class(dt[[col]])[1] == "integer64") {
      # Convert to character for large integers (like dates), regular integer for small ones
      max_val <- max(dt[[col]], na.rm = TRUE)
      if (!is.na(max_val) && max_val > .Machine$integer.max) {
        dt[, (col) := as.character(get(col))]
      } else {
        dt[, (col) := as.integer(get(col))]
      }
    }
  }
  return(dt)
}

#' Standardize Calendar Table Columns
#'
#' Ensures all calendar tables have consistent column order and types.
#' Adds missing columns with NA values and converts date columns to character.
#'
#' @param calendar_dt A calendar data.table from GTFS data
#' @return The standardized calendar data.table
standardize_calendar_columns <- function(calendar_dt) {
  standard_calendar_cols <- c("service_id", "monday", "tuesday", "wednesday",
                               "thursday", "friday", "saturday", "sunday",
                               "start_date", "end_date", "agency")

  # Convert date columns to character to avoid integer64 issues
  if ("start_date" %in% names(calendar_dt)) {
    calendar_dt[, start_date := as.character(start_date)]
  }
  if ("end_date" %in% names(calendar_dt)) {
    calendar_dt[, end_date := as.character(end_date)]
  }

  # Ensure all expected columns exist
  for (col in standard_calendar_cols) {
    if (!col %in% names(calendar_dt)) {
      if (col %in% c("monday", "tuesday", "wednesday", "thursday", "friday", "saturday", "sunday")) {
        calendar_dt[, (col) := as.integer(NA)]
      } else if (col %in% c("start_date", "end_date", "service_id", "agency")) {
        calendar_dt[, (col) := as.character(NA)]
      }
    }
  }

  # Reorder columns to standard order
  existing_cols <- intersect(standard_calendar_cols, names(calendar_dt))
  setcolorder(calendar_dt, existing_cols)

  return(calendar_dt)
}

#' Standardize Calendar Dates Table Columns
#'
#' Ensures all calendar_dates tables have consistent column order and types.
#' Adds missing columns with NA values and converts date column to character.
#'
#' @param calendar_dates_dt A calendar_dates data.table from GTFS data
#' @return The standardized calendar_dates data.table
standardize_calendar_dates_columns <- function(calendar_dates_dt) {
  standard_calendar_dates_cols <- c("service_id", "date", "exception_type", "agency")

  # Convert date column to character to avoid integer64 issues
  if ("date" %in% names(calendar_dates_dt)) {
    calendar_dates_dt[, date := as.character(date)]
  }

  # Ensure all expected columns exist
  for (col in standard_calendar_dates_cols) {
    if (!col %in% names(calendar_dates_dt)) {
      if (col == "exception_type") {
        calendar_dates_dt[, (col) := as.integer(NA)]
      } else {
        calendar_dates_dt[, (col) := as.character(NA)]
      }
    }
  }

  # Reorder columns to standard order
  existing_cols <- intersect(standard_calendar_dates_cols, names(calendar_dates_dt))
  setcolorder(calendar_dates_dt, existing_cols)

  return(calendar_dates_dt)
}

#' Combine GTFS Tables from Multiple Agencies
#'
#' Combines stops, routes, trips, stop_times, calendar, calendar_dates, and
#' shapes tables from multiple agencies. Handles integer64 conversion and
#' calendar standardization.
#'
#' @param agency_data_list List of agency GTFS data (each with stops, routes, trips, etc.)
#' @return List with combined tables: all_stops, all_routes, all_trips,
#'   all_stop_times, all_calendar, all_calendar_dates, all_shapes
#'
#' @examples
#' \dontrun{
#' combined <- combine_gtfs_tables(list(cta_data, pace_data, metra_data))
#' }
combine_gtfs_tables <- function(agency_data_list) {
  # Apply integer64 conversion to all datasets
  for (data in agency_data_list) {
    data$stops <- convert_integer64(data$stops)
    data$routes <- convert_integer64(data$routes)
    data$trips <- convert_integer64(data$trips)
    data$stop_times <- convert_integer64(data$stop_times)
    data$calendar <- convert_integer64(data$calendar)
    data$calendar_dates <- convert_integer64(data$calendar_dates)
    data$shapes <- convert_integer64(data$shapes)
  }

  # Combine basic tables
  all_stops <- rbindlist(lapply(agency_data_list, function(x) x$stops),
                         fill = TRUE, use.names = TRUE)
  all_routes <- rbindlist(lapply(agency_data_list, function(x) x$routes),
                          fill = TRUE, use.names = TRUE)
  all_trips <- rbindlist(lapply(agency_data_list, function(x) x$trips),
                         fill = TRUE, use.names = TRUE)
  all_stop_times <- rbindlist(lapply(agency_data_list, function(x) x$stop_times),
                              fill = TRUE, use.names = TRUE)

  # Standardize and combine calendar tables
  for (data in agency_data_list) {
    data$calendar <- standardize_calendar_columns(data$calendar)
    data$calendar_dates <- standardize_calendar_dates_columns(data$calendar_dates)
  }

  all_calendar <- rbindlist(lapply(agency_data_list, function(x) x$calendar),
                            fill = TRUE, use.names = TRUE)
  all_calendar_dates <- rbindlist(lapply(agency_data_list, function(x) x$calendar_dates),
                                  fill = TRUE, use.names = TRUE)

  # Combine shapes tables
  all_shapes <- rbindlist(lapply(agency_data_list, function(x) x$shapes),
                          fill = TRUE, use.names = TRUE)

  return(list(
    all_stops = all_stops,
    all_routes = all_routes,
    all_trips = all_trips,
    all_stop_times = all_stop_times,
    all_calendar = all_calendar,
    all_calendar_dates = all_calendar_dates,
    all_shapes = all_shapes
  ))
}

#' Enrich Stop Times with Route Type and Clean Times
#'
#' Adds route_type to stop_times by joining with trips and routes tables.
#' Also cleans stop times that are >= 24:00:00 by capping them at 23:59:59.
#'
#' @param all_stop_times Combined stop_times data.table
#' @param all_trips Combined trips data.table
#' @param all_routes Combined routes data.table
#' @return Enriched stop_times data.table with route_type column
#'
#' @examples
#' \dontrun{
#' enriched_stop_times <- enrich_stop_times(all_stop_times, all_trips, all_routes)
#' }
enrich_stop_times <- function(all_stop_times, all_trips, all_routes) {
  # Clean up stop times that are > 24:00:00
  all_stop_times[, arrival_time := fifelse(substr(arrival_time, 1, 2) >= "24",
                                            paste0("23:59:59"),
                                            arrival_time)]

  # Add route_type to all_stop_times
  all_stop_times <- merge(
    all_stop_times,
    all_trips[, .(unique_trip_id, unique_route_id, agency)],
    by = c("unique_trip_id", "agency")
  )
  all_stop_times <- merge(
    all_stop_times,
    all_routes[, .(unique_route_id, route_type, agency)],
    by = c("unique_route_id", "agency")
  )

  return(all_stop_times)
}

#' Process All GTFS Data from Multiple Agencies
#'
#' Complete workflow for downloading, normalizing, validating, and combining
#' GTFS data from multiple transit agencies. This is the main orchestration
#' function that replaces the inline logic in the Rmd file.
#'
#' @param agency_configs List of agency configurations, each with name and url.
#'   Example: list(list(name = "cta", url = "https://..."), ...)
#' @param validate Logical indicating whether to run data quality validation (default: TRUE)
#' @return List containing:
#'   - all_stops: Combined stops from all agencies
#'   - all_routes: Combined routes from all agencies
#'   - all_trips: Combined trips from all agencies (with unique_shape_id)
#'   - all_stop_times: Combined and enriched stop_times from all agencies
#'   - all_calendar: Combined calendar data from all agencies
#'   - all_calendar_dates: Combined calendar_dates from all agencies
#'   - all_shapes: Combined route geometry from all agencies
#'   - validation_results: Validation report (if validate = TRUE)
#'   - agency_data: List of individual agency data for reference
#'
#' @examples
#' \dontrun{
#' agency_configs <- list(
#'   list(name = "cta", url = "https://www.transitchicago.com/downloads/sch_data/google_transit.zip"),
#'   list(name = "pace", url = "https://www.pacebus.com/sites/default/files/2025-02/GTFS.zip")
#' )
#' gtfs_data <- process_all_gtfs_data(agency_configs)
#' }
process_all_gtfs_data <- function(agency_configs, validate = TRUE) {
  cat("\n=== Downloading and Processing GTFS Data ===\n\n")

  # Download and extract GTFS data for all agencies
  agency_dirs <- list()
  for (config in agency_configs) {
    cat(sprintf("Downloading %s...\n", toupper(config$name)))
    agency_dirs[[config$name]] <- download_and_extract_gtfs(config$name, config$url)
  }

  # Read and normalize GTFS data for all agencies
  cat("\n=== Reading and Normalizing GTFS Data ===\n\n")
  agency_data <- list()
  for (config in agency_configs) {
    cat(sprintf("Processing %s...\n", toupper(config$name)))
    agency_data[[config$name]] <- read_normalize_gtfs(config$name, agency_dirs[[config$name]])
  }

  # Validate GTFS data quality
  validation_results <- NULL
  if (validate) {
    cat("\n=== GTFS Data Quality Validation ===\n\n")
    validation_results <- validate_all_gtfs(agency_data)
    print_validation_report(validation_results)

    # Check if any critical issues were found
    critical_failures <- sapply(validation_results, function(x) {
      !x$valid && any(!grepl("WARNING", unlist(x[c("structure", "coordinates", "relationships")]$issues)))
    })
    if (any(critical_failures)) {
      warning("Critical data quality issues detected - review validation report above")
    }
  }

  # Combine data from all agencies
  cat("\n=== Combining GTFS Tables ===\n\n")
  combined <- combine_gtfs_tables(agency_data)

  # Enrich stop times with route_type
  cat("Enriching stop times with route information...\n")
  combined$all_stop_times <- enrich_stop_times(
    combined$all_stop_times,
    combined$all_trips,
    combined$all_routes
  )

  cat(sprintf("\nProcessing complete:\n"))
  cat(sprintf("  - Stops: %d\n", nrow(combined$all_stops)))
  cat(sprintf("  - Routes: %d\n", nrow(combined$all_routes)))
  cat(sprintf("  - Trips: %d\n", nrow(combined$all_trips)))
  cat(sprintf("  - Stop times: %d\n", nrow(combined$all_stop_times)))
  cat(sprintf("  - Calendar entries: %d\n", nrow(combined$all_calendar)))
  cat(sprintf("  - Calendar date exceptions: %d\n", nrow(combined$all_calendar_dates)))
  cat(sprintf("  - Shape points: %d\n", nrow(combined$all_shapes)))
  cat(sprintf("  - Unique shapes: %d\n", length(unique(combined$all_shapes$unique_shape_id))))

  # Return combined data with validation results
  return(c(combined, list(
    validation_results = validation_results,
    agency_data = agency_data
  )))
}
