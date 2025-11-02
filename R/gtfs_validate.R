#' Validate GTFS Data Structure
#'
#' Checks that all required GTFS files are present and non-empty.
#'
#' @param gtfs_data Named list with elements: stops, routes, trips, stop_times, calendar
#' @param agency_name Character. Name of agency (for error messages)
#'
#' @return List with:
#'   \describe{
#'     \item{valid}{Logical. TRUE if all checks pass}
#'     \item{issues}{Character vector of issues found}
#'   }
#'
#' @details
#' Checks for:
#' \itemize{
#'   \item All required tables present (stops, routes, trips, stop_times, calendar)
#'   \item Tables are not empty
#'   \item Required fields exist in each table
#' }
#'
#' @export
validate_gtfs_structure <- function(gtfs_data, agency_name) {
  issues <- character()

  # Check required tables exist
  required_tables <- c("stops", "routes", "trips", "stop_times", "calendar")
  for (table in required_tables) {
    if (!table %in% names(gtfs_data)) {
      issues <- c(issues, paste0(table, " table missing"))
    } else if (nrow(gtfs_data[[table]]) == 0) {
      issues <- c(issues, paste0(table, " table is empty"))
    }
  }

  # Check required fields in stops
  if ("stops" %in% names(gtfs_data) && nrow(gtfs_data$stops) > 0) {
    required_stop_fields <- c("stop_id", "stop_name", "stop_lat", "stop_lon")
    missing_fields <- setdiff(required_stop_fields, names(gtfs_data$stops))
    if (length(missing_fields) > 0) {
      issues <- c(issues, paste0("stops missing fields: ", paste(missing_fields, collapse = ", ")))
    }
  }

  # Check required fields in routes
  if ("routes" %in% names(gtfs_data) && nrow(gtfs_data$routes) > 0) {
    required_route_fields <- c("route_id", "route_type")
    missing_fields <- setdiff(required_route_fields, names(gtfs_data$routes))
    if (length(missing_fields) > 0) {
      issues <- c(issues, paste0("routes missing fields: ", paste(missing_fields, collapse = ", ")))
    }
  }

  # Check required fields in trips
  if ("trips" %in% names(gtfs_data) && nrow(gtfs_data$trips) > 0) {
    required_trip_fields <- c("trip_id", "route_id", "service_id")
    missing_fields <- setdiff(required_trip_fields, names(gtfs_data$trips))
    if (length(missing_fields) > 0) {
      issues <- c(issues, paste0("trips missing fields: ", paste(missing_fields, collapse = ", ")))
    }
  }

  # Check required fields in stop_times
  if ("stop_times" %in% names(gtfs_data) && nrow(gtfs_data$stop_times) > 0) {
    required_st_fields <- c("trip_id", "stop_id", "arrival_time", "stop_sequence")
    missing_fields <- setdiff(required_st_fields, names(gtfs_data$stop_times))
    if (length(missing_fields) > 0) {
      issues <- c(issues, paste0("stop_times missing fields: ", paste(missing_fields, collapse = ", ")))
    }
  }

  list(
    valid = length(issues) == 0,
    issues = issues
  )
}

#' Validate GTFS Coordinates
#'
#' Checks that stop coordinates are valid and within Illinois bounds.
#'
#' @param stops data.table with stop_lat and stop_lon columns
#' @param agency_name Character. Name of agency (for error messages)
#'
#' @return List with:
#'   \describe{
#'     \item{valid}{Logical. TRUE if all checks pass}
#'     \item{issues}{Character vector of issues found}
#'     \item{invalid_stops}{data.table of stops with invalid coordinates}
#'   }
#'
#' @details
#' Illinois approximate bounds:
#' \itemize{
#'   \item Latitude: 36.9 to 42.6
#'   \item Longitude: -91.6 to -87.0
#' }
#'
#' Also checks for:
#' \itemize{
#'   \item Coordinates at (0, 0) - common error value
#'   \item Missing/NA coordinates
#'   \item Coordinates outside valid global ranges (±90 lat, ±180 lon)
#' }
#'
#' @export
validate_gtfs_coordinates <- function(stops, agency_name) {
  issues <- character()
  invalid_stops <- list()

  if (nrow(stops) == 0) {
    return(list(valid = TRUE, issues = character(), invalid_stops = stops[0]))
  }

  # Check for missing coordinates
  missing_lat <- sum(is.na(stops$stop_lat))
  missing_lon <- sum(is.na(stops$stop_lon))
  if (missing_lat > 0 || missing_lon > 0) {
    issues <- c(issues, paste0(max(missing_lat, missing_lon), " stops with missing coordinates"))
  }

  # Check for (0, 0) coordinates
  zero_coords <- stops[stop_lat == 0 & stop_lon == 0]
  if (nrow(zero_coords) > 0) {
    issues <- c(issues, paste0(nrow(zero_coords), " stops at (0,0) coordinates"))
    invalid_stops$zero <- zero_coords
  }

  # Check for coordinates outside valid global ranges
  invalid_lat <- stops[stop_lat < -90 | stop_lat > 90]
  if (nrow(invalid_lat) > 0) {
    issues <- c(issues, paste0(nrow(invalid_lat), " stops with invalid latitude (outside ±90)"))
    invalid_stops$lat <- invalid_lat
  }

  invalid_lon <- stops[stop_lon < -180 | stop_lon > 180]
  if (nrow(invalid_lon) > 0) {
    issues <- c(issues, paste0(nrow(invalid_lon), " stops with invalid longitude (outside ±180)"))
    invalid_stops$lon <- invalid_lon
  }

  # Check Illinois bounds (with some buffer for border cases)
  # Illinois: roughly 36.9°N to 42.6°N, 87.0°W to 91.6°W
  outside_il <- stops[
    stop_lat < 36.5 | stop_lat > 43.0 |
    stop_lon < -92.0 | stop_lon > -86.5
  ]
  if (nrow(outside_il) > 0) {
    # This is a warning, not necessarily an error (some agencies serve border areas)
    issues <- c(issues, paste0(
      "WARNING: ", nrow(outside_il),
      " stops outside Illinois bounds (may be OK for border agencies like Metra/Metro STL)"
    ))
    invalid_stops$outside_il <- outside_il
  }

  all_invalid <- unique(do.call(rbind, invalid_stops))

  list(
    valid = !any(grepl("^[^W]", issues)),  # Valid if only warnings, not errors
    issues = issues,
    invalid_stops = if (is.null(all_invalid)) stops[0] else all_invalid
  )
}

#' Validate GTFS Relationships
#'
#' Checks referential integrity between GTFS tables.
#'
#' @param gtfs_data Named list with elements: stops, routes, trips, stop_times, calendar
#' @param agency_name Character. Name of agency (for error messages)
#'
#' @return List with:
#'   \describe{
#'     \item{valid}{Logical. TRUE if all checks pass}
#'     \item{issues}{Character vector of issues found}
#'   }
#'
#' @details
#' Checks for:
#' \itemize{
#'   \item Orphaned trips (trip references non-existent route_id)
#'   \item Orphaned stop_times (references non-existent trip_id or stop_id)
#'   \item Routes with no trips
#'   \item Trips with no stop_times
#'   \item Trips referencing non-existent service_id in calendar
#' }
#'
#' @export
validate_gtfs_relationships <- function(gtfs_data, agency_name) {
  issues <- character()

  # Check trips reference valid routes
  if (nrow(gtfs_data$trips) > 0 && nrow(gtfs_data$routes) > 0) {
    orphaned_trips <- gtfs_data$trips[!unique_route_id %in% gtfs_data$routes$unique_route_id]
    if (nrow(orphaned_trips) > 0) {
      issues <- c(issues, paste0(
        nrow(orphaned_trips), " trips reference non-existent routes"
      ))
    }
  }

  # Check stop_times reference valid trips
  if (nrow(gtfs_data$stop_times) > 0 && nrow(gtfs_data$trips) > 0) {
    orphaned_st_trips <- gtfs_data$stop_times[!unique_trip_id %in% gtfs_data$trips$unique_trip_id]
    if (nrow(orphaned_st_trips) > 0) {
      issues <- c(issues, paste0(
        nrow(orphaned_st_trips), " stop_times reference non-existent trips"
      ))
    }
  }

  # Check stop_times reference valid stops
  if (nrow(gtfs_data$stop_times) > 0 && nrow(gtfs_data$stops) > 0) {
    orphaned_st_stops <- gtfs_data$stop_times[!unique_stop_id %in% gtfs_data$stops$unique_stop_id]
    if (nrow(orphaned_st_stops) > 0) {
      issues <- c(issues, paste0(
        nrow(orphaned_st_stops), " stop_times reference non-existent stops"
      ))
    }
  }

  # Check for routes with no trips
  if (nrow(gtfs_data$routes) > 0 && nrow(gtfs_data$trips) > 0) {
    routes_with_trips <- unique(gtfs_data$trips$unique_route_id)
    routes_no_trips <- gtfs_data$routes[!unique_route_id %in% routes_with_trips]
    if (nrow(routes_no_trips) > 0) {
      issues <- c(issues, paste0(
        nrow(routes_no_trips), " routes have no trips"
      ))
    }
  }

  # Check for trips with no stop_times
  if (nrow(gtfs_data$trips) > 0 && nrow(gtfs_data$stop_times) > 0) {
    trips_with_st <- unique(gtfs_data$stop_times$unique_trip_id)
    trips_no_st <- gtfs_data$trips[!unique_trip_id %in% trips_with_st]
    if (nrow(trips_no_st) > 0) {
      issues <- c(issues, paste0(
        nrow(trips_no_st), " trips have no stop_times"
      ))
    }
  }

  # Check trips reference valid service_id in calendar
  if (nrow(gtfs_data$trips) > 0 && nrow(gtfs_data$calendar) > 0) {
    orphaned_service <- gtfs_data$trips[!service_id %in% gtfs_data$calendar$service_id]
    if (nrow(orphaned_service) > 0) {
      issues <- c(issues, paste0(
        nrow(orphaned_service), " trips reference non-existent service_id"
      ))
    }
  }

  list(
    valid = length(issues) == 0,
    issues = issues
  )
}

#' Generate GTFS Quality Report
#'
#' Runs all validation checks and generates a comprehensive quality report.
#'
#' @param gtfs_data Named list with elements: stops, routes, trips, stop_times, calendar
#' @param agency_name Character. Name of agency
#'
#' @return List with:
#'   \describe{
#'     \item{agency}{Character. Agency name}
#'     \item{valid}{Logical. TRUE if all checks pass}
#'     \item{structure}{Result from validate_gtfs_structure}
#'     \item{coordinates}{Result from validate_gtfs_coordinates}
#'     \item{relationships}{Result from validate_gtfs_relationships}
#'     \item{stats}{List of basic statistics (row counts)}
#'   }
#'
#' @examples
#' \dontrun{
#' cta_dir <- download_and_extract_gtfs("cta", cta_url)
#' cta_gtfs <- read_normalize_gtfs("cta", cta_dir)
#' report <- generate_quality_report(cta_gtfs, "cta")
#' print_validation_report(list(cta = report))
#' }
#'
#' @export
generate_quality_report <- function(gtfs_data, agency_name) {
  structure_check <- validate_gtfs_structure(gtfs_data, agency_name)
  coords_check <- validate_gtfs_coordinates(gtfs_data$stops, agency_name)
  relationships_check <- validate_gtfs_relationships(gtfs_data, agency_name)

  list(
    agency = agency_name,
    valid = structure_check$valid && coords_check$valid && relationships_check$valid,
    structure = structure_check,
    coordinates = coords_check,
    relationships = relationships_check,
    stats = list(
      stops = nrow(gtfs_data$stops),
      routes = nrow(gtfs_data$routes),
      trips = nrow(gtfs_data$trips),
      stop_times = nrow(gtfs_data$stop_times),
      calendar_entries = nrow(gtfs_data$calendar)
    )
  )
}

#' Print Validation Report
#'
#' Pretty-prints validation results for multiple agencies.
#'
#' @param validation_results Named list of validation results (from generate_quality_report)
#'
#' @return NULL (prints to console)
#'
#' @export
print_validation_report <- function(validation_results) {
  cat("\n")
  cat("=" = rep("=", 60), "\n")
  cat("  GTFS DATA QUALITY REPORT\n")
  cat("=" = rep("=", 60), "\n\n")

  for (agency in names(validation_results)) {
    result <- validation_results[[agency]]

    cat("Agency:", toupper(result$agency), "\n")
    cat("-" = rep("-", 60), "\n")

    # Overall status
    status <- if (result$valid) "✓ PASS" else "✗ FAIL"
    cat("Overall Status:", status, "\n\n")

    # Statistics
    cat("Dataset Statistics:\n")
    cat(sprintf("  Stops: %d\n", result$stats$stops))
    cat(sprintf("  Routes: %d\n", result$stats$routes))
    cat(sprintf("  Trips: %d\n", result$stats$trips))
    cat(sprintf("  Stop Times: %d\n", result$stats$stop_times))
    cat(sprintf("  Calendar Entries: %d\n", result$stats$calendar_entries))
    cat("\n")

    # Structure issues
    if (length(result$structure$issues) > 0) {
      cat("Structure Issues:\n")
      for (issue in result$structure$issues) {
        cat(sprintf("  • %s\n", issue))
      }
      cat("\n")
    }

    # Coordinate issues
    if (length(result$coordinates$issues) > 0) {
      cat("Coordinate Issues:\n")
      for (issue in result$coordinates$issues) {
        cat(sprintf("  • %s\n", issue))
      }
      cat("\n")
    }

    # Relationship issues
    if (length(result$relationships$issues) > 0) {
      cat("Relationship Issues:\n")
      for (issue in result$relationships$issues) {
        cat(sprintf("  • %s\n", issue))
      }
      cat("\n")
    }

    if (result$valid) {
      cat("✓ All validation checks passed!\n")
    }

    cat("\n")
  }

  cat("=" = rep("=", 60), "\n\n")
}

#' Validate All GTFS Data
#'
#' Convenience function to validate multiple agencies at once.
#'
#' @param gtfs_list Named list of GTFS data (names are agency names)
#'
#' @return Named list of validation results
#'
#' @export
validate_all_gtfs <- function(gtfs_list) {
  results <- list()
  for (agency in names(gtfs_list)) {
    results[[agency]] <- generate_quality_report(gtfs_list[[agency]], agency)
  }
  results
}
