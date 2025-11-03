# GTFS Feed Exploration Script for New Illinois Transit Agencies
# Purpose: Download and inspect 8 new GTFS feeds to understand their structure
# and identify any quirks before integration into main analysis

library(data.table)

# Source required modules
source("R/agency_metadata.R")
source("R/gtfs_download.R")

# Helper function to safely check file existence and row counts
check_file <- function(dir, filename) {
  filepath <- file.path(dir, filename)
  if (file.exists(filepath)) {
    dt <- fread(filepath)
    return(list(
      exists = TRUE,
      rows = nrow(dt),
      cols = ncol(dt),
      colnames = names(dt)
    ))
  } else {
    return(list(exists = FALSE, rows = 0, cols = 0, colnames = character()))
  }
}

# Helper function to explore a single agency's GTFS feed
explore_agency <- function(agency_id) {
  cat("\n")
  cat("================================================================================\n")
  cat(sprintf("EXPLORING: %s (%s)\n",
              get_agency_display_name(agency_id),
              get_agency_full_name(agency_id)))
  cat("================================================================================\n\n")

  metadata <- get_agency_metadata()[[agency_id]]

  cat(sprintf("URL: %s\n", metadata$url))
  cat(sprintf("Expected color: %s\n\n", metadata$color))

  # Download and extract
  cat("Downloading GTFS feed...\n")
  tryCatch({
    agency_dir <- download_and_extract_gtfs(agency_id, metadata$url)
    cat(sprintf("Downloaded to: %s\n\n", agency_dir))

    # Check all standard GTFS files
    cat("=== FILE STRUCTURE ===\n")

    files_to_check <- c(
      "agency.txt",
      "stops.txt",
      "routes.txt",
      "trips.txt",
      "stop_times.txt",
      "calendar.txt",
      "calendar_dates.txt",
      "shapes.txt"
    )

    for (file in files_to_check) {
      result <- check_file(agency_dir, file)
      if (result$exists) {
        cat(sprintf("✓ %s: %s rows, %s columns\n",
                    file,
                    format(result$rows, big.mark=","),
                    result$cols))
      } else {
        cat(sprintf("✗ %s: MISSING\n", file))
      }
    }

    # Detailed inspection of key files
    cat("\n=== STOPS.TXT ===\n")
    stops_info <- check_file(agency_dir, "stops.txt")
    if (stops_info$exists) {
      stops <- fread(file.path(agency_dir, "stops.txt"))
      cat(sprintf("Total stops: %s\n", format(nrow(stops), big.mark=",")))
      cat(sprintf("Columns: %s\n", paste(names(stops), collapse=", ")))
      cat(sprintf("Sample stop names:\n"))
      print(head(stops$stop_name, 5))

      # Check for location_type and parent_station
      if ("location_type" %in% names(stops)) {
        cat(sprintf("\nLocation types present: %s\n",
                    paste(unique(stops$location_type), collapse=", ")))
      } else {
        cat("\nNo location_type column (OK - will be added during normalization)\n")
      }

      # Check coordinate ranges
      if ("stop_lat" %in% names(stops) && "stop_lon" %in% names(stops)) {
        cat(sprintf("Latitude range: %.4f to %.4f\n",
                    min(stops$stop_lat, na.rm=TRUE),
                    max(stops$stop_lat, na.rm=TRUE)))
        cat(sprintf("Longitude range: %.4f to %.4f\n",
                    min(stops$stop_lon, na.rm=TRUE),
                    max(stops$stop_lon, na.rm=TRUE)))

        # Check for suspicious coordinates
        if (any(stops$stop_lat == 0 | stops$stop_lon == 0, na.rm=TRUE)) {
          cat("⚠ WARNING: Found stops with 0,0 coordinates!\n")
        }
      }
    }

    cat("\n=== ROUTES.TXT ===\n")
    routes_info <- check_file(agency_dir, "routes.txt")
    if (routes_info$exists) {
      routes <- fread(file.path(agency_dir, "routes.txt"))
      cat(sprintf("Total routes: %s\n", format(nrow(routes), big.mark=",")))
      cat(sprintf("Columns: %s\n", paste(names(routes), collapse=", ")))

      if ("route_type" %in% names(routes)) {
        route_types <- table(routes$route_type)
        cat("\nRoute types:\n")
        for (i in seq_along(route_types)) {
          type_code <- names(route_types)[i]
          type_name <- switch(type_code,
                             "0" = "Tram/Light Rail",
                             "1" = "Subway/Metro",
                             "2" = "Rail",
                             "3" = "Bus",
                             "4" = "Ferry",
                             "5" = "Cable Car",
                             "6" = "Gondola",
                             "7" = "Funicular",
                             "Unknown")
          cat(sprintf("  Type %s (%s): %s routes\n",
                      type_code, type_name, route_types[i]))
        }

        # Check for unexpected rail types
        if (any(routes$route_type %in% c(0, 1, 2))) {
          cat("\n⚠ WARNING: Found rail route types! Expected bus-only.\n")
          cat("    Review rail identification logic in R/hub_identification.R\n")
        }
      }

      cat("\nSample route names:\n")
      print(head(routes$route_short_name, 10))
    }

    cat("\n=== TRIPS.TXT ===\n")
    trips_info <- check_file(agency_dir, "trips.txt")
    if (trips_info$exists) {
      trips <- fread(file.path(agency_dir, "trips.txt"))
      cat(sprintf("Total trips: %s\n", format(nrow(trips), big.mark=",")))
      cat(sprintf("Columns: %s\n", paste(names(trips), collapse=", ")))

      if ("direction_id" %in% names(trips)) {
        direction_count <- sum(!is.na(trips$direction_id))
        direction_pct <- 100 * direction_count / nrow(trips)
        cat(sprintf("\ndirection_id: Present in %s/%s trips (%.1f%%)\n",
                    format(direction_count, big.mark=","),
                    format(nrow(trips), big.mark=","),
                    direction_pct))
        if (direction_pct > 0) {
          cat(sprintf("  Direction values: %s\n",
                      paste(sort(unique(trips$direction_id[!is.na(trips$direction_id)])),
                            collapse=", ")))
        }
      } else {
        cat("\n⚠ direction_id: NOT PRESENT (will be set to NA during normalization)\n")
      }
    }

    cat("\n=== CALENDAR.TXT ===\n")
    calendar_info <- check_file(agency_dir, "calendar.txt")
    if (calendar_info$exists) {
      calendar <- fread(file.path(agency_dir, "calendar.txt"))
      cat(sprintf("Total service calendars: %s\n", format(nrow(calendar), big.mark=",")))

      # Check for weekday service
      if (all(c("monday", "tuesday", "wednesday", "thursday", "friday") %in% names(calendar))) {
        weekday_services <- calendar[monday == 1 & tuesday == 1 &
                                     wednesday == 1 & thursday == 1 & friday == 1]
        cat(sprintf("Weekday-only or weekday-inclusive services: %s\n", nrow(weekday_services)))

        if (nrow(weekday_services) == 0) {
          cat("⚠ WARNING: No weekday service found in calendar.txt!\n")
          cat("   Check calendar_dates.txt for service additions.\n")
        }
      }

      # Check date ranges
      if (all(c("start_date", "end_date") %in% names(calendar))) {
        cat(sprintf("Date range: %s to %s\n",
                    min(calendar$start_date),
                    max(calendar$end_date)))
      }
    } else {
      cat("⚠ calendar.txt: MISSING\n")
      cat("   Agency may use calendar_dates.txt exclusively\n")
    }

    cat("\n=== CALENDAR_DATES.TXT ===\n")
    calendar_dates_info <- check_file(agency_dir, "calendar_dates.txt")
    if (calendar_dates_info$exists) {
      calendar_dates <- fread(file.path(agency_dir, "calendar_dates.txt"))
      cat(sprintf("Total calendar exceptions/additions: %s\n",
                  format(nrow(calendar_dates), big.mark=",")))

      if ("exception_type" %in% names(calendar_dates)) {
        exceptions <- table(calendar_dates$exception_type)
        cat("\nException types:\n")
        cat(sprintf("  Type 1 (service added): %s\n",
                    ifelse("1" %in% names(exceptions), exceptions["1"], 0)))
        cat(sprintf("  Type 2 (service removed): %s\n",
                    ifelse("2" %in% names(exceptions), exceptions["2"], 0)))
      }

      if ("date" %in% names(calendar_dates)) {
        cat(sprintf("Date range: %s to %s\n",
                    min(calendar_dates$date),
                    max(calendar_dates$date)))
      }
    } else {
      cat("calendar_dates.txt: Not present (optional)\n")
    }

    cat("\n=== STOP_TIMES.TXT ===\n")
    stop_times_info <- check_file(agency_dir, "stop_times.txt")
    if (stop_times_info$exists) {
      cat(sprintf("Total stop times: %s\n",
                  format(stop_times_info$rows, big.mark=",")))

      # Sample a few records to check time format
      stop_times <- fread(file.path(agency_dir, "stop_times.txt"), nrows = 1000)

      # Check for times >= 24:00:00
      if ("arrival_time" %in% names(stop_times)) {
        times_over_24 <- sum(substr(stop_times$arrival_time, 1, 2) >= "24", na.rm=TRUE)
        if (times_over_24 > 0) {
          cat(sprintf("⚠ Found %s arrival times >= 24:00:00 in sample (will be cleaned)\n",
                      times_over_24))
        }
      }
    }

    cat("\n=== PEAK PERIOD SERVICE CHECK ===\n")
    # This is a simplified check - full analysis happens in main pipeline
    if (calendar_info$exists || calendar_dates_info$exists) {
      cat("✓ Calendar data present - peak period analysis will proceed\n")
      cat("  AM peak: 7:00-9:00 (07:00:00 - 09:00:00)\n")
      cat("  PM peak: 16:00-18:00 (16:00:00 - 18:00:00)\n")
    } else {
      cat("✗ No calendar data found - cannot verify peak period service\n")
    }

    cat("\n=== SUMMARY ===\n")
    critical_files <- c("stops.txt", "routes.txt", "trips.txt", "stop_times.txt")
    critical_exist <- sapply(critical_files, function(f) check_file(agency_dir, f)$exists)

    if (all(critical_exist)) {
      cat("✓ All critical GTFS files present\n")
    } else {
      cat("✗ MISSING CRITICAL FILES:\n")
      missing <- critical_files[!critical_exist]
      for (f in missing) {
        cat(sprintf("  - %s\n", f))
      }
    }

    if (calendar_info$exists || calendar_dates_info$exists) {
      cat("✓ Calendar data present (calendar.txt or calendar_dates.txt)\n")
    } else {
      cat("✗ No calendar data found\n")
    }

    if (routes_info$exists) {
      routes <- fread(file.path(agency_dir, "routes.txt"))
      if ("route_type" %in% names(routes)) {
        if (all(routes$route_type == 3)) {
          cat("✓ Bus-only agency (route_type=3) as expected\n")
        } else {
          cat("⚠ Non-bus route types found - review rail identification logic\n")
        }
      }
    }

    cat("\n")

  }, error = function(e) {
    cat(sprintf("\n✗ ERROR downloading or processing feed:\n"))
    cat(sprintf("  %s\n", e$message))
    cat("\n  This feed may require manual investigation or special handling.\n")
  })
}

# ============================================================================
# MAIN EXPLORATION
# ============================================================================

cat("\n")
cat("################################################################################\n")
cat("# GTFS FEED EXPLORATION FOR 8 NEW ILLINOIS TRANSIT AGENCIES\n")
cat("################################################################################\n")
cat("\n")
cat("This script downloads and inspects each new GTFS feed to identify:\n")
cat("  - File structure and completeness\n")
cat("  - Route types (expecting bus-only)\n")
cat("  - Direction ID availability\n")
cat("  - Calendar structure (calendar.txt vs calendar_dates.txt)\n")
cat("  - Peak period service coverage\n")
cat("  - Any data quality issues or unusual patterns\n")
cat("\n")
cat("New agencies being explored:\n")

# Get metadata for new agencies (last 8 in the list)
all_ids <- get_all_agency_ids()
new_agency_ids <- all_ids[7:14]  # metrolink_quad_cities through gowest

for (i in seq_along(new_agency_ids)) {
  agency_id <- new_agency_ids[i]
  cat(sprintf("%d. %s (%s)\n",
              i,
              get_agency_display_name(agency_id),
              agency_id))
}

cat("\n")
cat("Press Enter to start exploration (or Ctrl+C to cancel)...")
readline()

# Explore each new agency
for (agency_id in new_agency_ids) {
  explore_agency(agency_id)
}

cat("\n")
cat("################################################################################\n")
cat("# EXPLORATION COMPLETE\n")
cat("################################################################################\n")
cat("\n")
cat("Next steps:\n")
cat("1. Review findings above for any critical issues or unusual patterns\n")
cat("2. Document any agency-specific quirks in comments/documentation\n")
cat("3. Proceed with integrating agencies into main analysis pipeline\n")
cat("4. Run full validation via process_all_gtfs_data()\n")
cat("\n")
cat(sprintf("All feeds cached in: %s\n", file.path(getwd(), "gtfs_cache")))
cat("\n")
