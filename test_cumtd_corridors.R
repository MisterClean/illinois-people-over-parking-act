#!/usr/bin/env Rscript
# Focused test script for CUMTD (Champaign-Urbana MTD) corridor detection
#
# This script tests corridor overlap detection using only CUMTD bus data,
# allowing for fast iteration on buffer tolerance and overlap detection logic.
#
# Usage: Rscript test_cumtd_corridors.R [buffer_ft]
#   buffer_ft: Optional buffer tolerance in feet (default: 50)
#
# Examples:
#   Rscript test_cumtd_corridors.R     # Uses 50ft buffer
#   Rscript test_cumtd_corridors.R 30  # Uses 30ft buffer
#   Rscript test_cumtd_corridors.R 75  # Uses 75ft buffer

suppressPackageStartupMessages({
  library(tidyverse)
  library(sf)
  library(data.table)
  library(lubridate)
})

# Get buffer tolerance from command line or use default
args <- commandArgs(trailingOnly = TRUE)
BUFFER_TOLERANCE_FT <- if (length(args) > 0) as.numeric(args[1]) else 50

cat(strrep("=", 80), "\n", sep = "")
cat("CUMTD CORRIDOR DETECTION TEST\n")
cat(strrep("=", 80), "\n", sep = "")
cat(sprintf("Buffer tolerance: %d feet (%.1f meters)\n",
            BUFFER_TOLERANCE_FT, BUFFER_TOLERANCE_FT * 0.3048))
cat(strrep("=", 80), "\n\n", sep = "")

# Source all required modules
cat("Loading R modules...\n")
source("R/agency_metadata.R")
source("R/gtfs_download.R")
source("R/gtfs_normalize.R")
source("R/hub_identification.R")  # For identify_weekday_services
source("R/frequency_calc.R")
source("R/hub_processing.R")
source("R/corridor_processing.R")
source("R/spatial_clustering.R")

# Step 1: Load CUMTD GTFS data
cat("\n[1/7] Loading CUMTD GTFS data...\n")
cumtd_metadata <- get_agency_metadata()$cumtd
cat(sprintf("  Agency: %s (%s)\n", cumtd_metadata$full_name, cumtd_metadata$name))
cat(sprintf("  GTFS URL: %s\n", cumtd_metadata$url))

cumtd_dir <- download_and_extract_gtfs("cumtd", cumtd_metadata$url)
cumtd_data <- read_normalize_gtfs("cumtd", cumtd_dir)

cat(sprintf("  Loaded: %s stops, %s routes, %s trips, %s shapes\n",
            format(nrow(cumtd_data$stops), big.mark = ","),
            format(nrow(cumtd_data$routes), big.mark = ","),
            format(nrow(cumtd_data$trips), big.mark = ","),
            format(nrow(cumtd_data$shapes), big.mark = ",")))

# Step 2: Check for direction_id
cat("\n[2/7] Checking direction_id availability...\n")
has_direction <- "direction_id" %in% names(cumtd_data$trips) &&
                 sum(!is.na(cumtd_data$trips$direction_id)) > 0

if (has_direction) {
  cat(sprintf("  ✓ direction_id found: %d/%d trips have direction data\n",
              sum(!is.na(cumtd_data$trips$direction_id)),
              nrow(cumtd_data$trips)))
  direction_values <- unique(cumtd_data$trips$direction_id[!is.na(cumtd_data$trips$direction_id)])
  cat(sprintf("  Unique directions: %s\n", paste(direction_values, collapse = ", ")))
} else {
  cat("  ⚠️  No direction_id data found - will use default direction 0\n")
  cumtd_data$trips[, direction_id := 0L]
}

# Step 3: Identify weekday services
cat("\n[3/7] Identifying weekday services...\n")
weekday_service <- identify_weekday_services(cumtd_data$calendar, cumtd_data$calendar_dates)
cat(sprintf("  Found %d weekday service IDs\n", nrow(weekday_service)))

# Step 4: Get weekday bus trips (CUMTD is bus-only)
cat("\n[4/7] Filtering to weekday bus trips...\n")
bus_routes <- cumtd_data$routes[route_type == 3]  # Route type 3 = bus
cat(sprintf("  Total bus routes: %d\n", nrow(bus_routes)))

weekday_bus_trips <- merge(
  cumtd_data$trips,
  weekday_service,
  by = c("service_id", "agency")
)
weekday_bus_trips <- weekday_bus_trips[unique_route_id %in% bus_routes$unique_route_id]
cat(sprintf("  Weekday bus trips: %s\n", format(nrow(weekday_bus_trips), big.mark = ",")))

# Enrich stop_times with route information
cat("  Enriching stop_times with route information...\n")
source("R/gtfs_processing.R")  # For enrich_stop_times
enriched_stop_times <- enrich_stop_times(cumtd_data$stop_times, cumtd_data$trips, cumtd_data$routes)

# Step 5: Prepare peak period stop times
cat("\n[5/7] Preparing AM/PM peak stop times...\n")
peak_stops <- prepare_peak_stop_times(enriched_stop_times, weekday_bus_trips)
am_peak_bus_stops <- peak_stops$am_peak_bus_stops
pm_peak_bus_stops <- peak_stops$pm_peak_bus_stops

cat(sprintf("  AM peak (7-9am) stops: %s\n", format(nrow(am_peak_bus_stops), big.mark = ",")))
cat(sprintf("  PM peak (4-6pm) stops: %s\n", format(nrow(pm_peak_bus_stops), big.mark = ",")))

# Step 6: Calculate route-level trip counts
cat("\n[6/7] Calculating route trip counts by direction...\n")
route_trips <- calculate_route_trip_counts(am_peak_bus_stops, pm_peak_bus_stops)

cat(sprintf("  Routes with trip counts: %d\n", nrow(route_trips)))
cat(sprintf("    AM direction 0 total trips: %s\n", format(sum(route_trips$trips_am_dir0), big.mark = ",")))
cat(sprintf("    AM direction 1 total trips: %s\n", format(sum(route_trips$trips_am_dir1), big.mark = ",")))
cat(sprintf("    PM direction 0 total trips: %s\n", format(sum(route_trips$trips_pm_dir0), big.mark = ",")))
cat(sprintf("    PM direction 1 total trips: %s\n", format(sum(route_trips$trips_pm_dir1), big.mark = ",")))

# Step 7: Identify overlapping segments with configurable buffer
cat("\n[7/7] Identifying overlapping corridor segments...\n")
cat(sprintf("  Using buffer tolerance: %d feet\n\n", BUFFER_TOLERANCE_FT))

overlap_results <- identify_overlapping_segments(
  route_trips,
  weekday_bus_trips,   # Has unique_shape_id and direction_id
  cumtd_data$shapes,
  buffer_distance_ft = BUFFER_TOLERANCE_FT
)

qualifying_segments <- overlap_results$qualifying_shapes
qualification_summary <- overlap_results$qualification_summary

# Display results
cat("\n", strrep("=", 80), "\n", sep = "")
cat("RESULTS\n")
cat(strrep("=", 80), "\n\n", sep = "")

cat(sprintf("Total qualifying corridor segments: %d\n", nrow(qualifying_segments)))

if (nrow(qualifying_segments) > 0) {
  multi_route_count <- sum(qualifying_segments$num_routes > 1)
  single_route_count <- sum(qualifying_segments$num_routes == 1)

  cat(sprintf("\nBreakdown:\n"))
  cat(sprintf("  Single-route segments: %d\n", single_route_count))
  cat(sprintf("  Multi-route segments:  %d", multi_route_count))

  if (multi_route_count > 0) {
    cat(" ⭐\n")
  } else {
    cat(" ⚠️\n")
  }

  # Route count distribution
  cat("\nSegments by route count:\n")
  route_counts <- table(qualifying_segments$num_routes)
  for (n in names(sort(route_counts, decreasing = TRUE))) {
    cat(sprintf("  %s routes: %d segments\n", n, route_counts[n]))
  }

  # Frequency distribution
  cat("\nFrequency metrics:\n")
  am_valid <- qualifying_segments$interval_am[qualifying_segments$interval_am < Inf]
  pm_valid <- qualifying_segments$interval_pm[qualifying_segments$interval_pm < Inf]

  if (length(am_valid) > 0) {
    cat(sprintf("  Best AM frequency: %.1f minutes\n", min(am_valid)))
  }
  if (length(pm_valid) > 0) {
    cat(sprintf("  Best PM frequency: %.1f minutes\n", min(pm_valid)))
  }

  # Total corridor length
  if (nrow(qualifying_segments) > 0) {
    total_length_ft <- sum(st_length(st_transform(qualifying_segments, 3435)))
    total_length_mi <- as.numeric(total_length_ft) / 5280
    cat(sprintf("\nTotal corridor length: %.2f miles\n", total_length_mi))
  }

  # Show top multi-route examples
  if (multi_route_count > 0) {
    cat("\n", strrep("-", 80), "\n", sep = "")
    cat("TOP MULTI-ROUTE CORRIDORS\n")
    cat(strrep("-", 80), "\n\n", sep = "")

    multi <- qualifying_segments[qualifying_segments$num_routes > 1, ]
    multi <- multi[order(-multi$num_routes, multi$interval_am), ]

    n_show <- min(10, nrow(multi))
    for (i in 1:n_show) {
      seg <- multi[i, ]
      routes_list <- strsplit(as.character(seg$routes), ";")[[1]]

      cat(sprintf("%d. Direction %d: %d routes\n", i, seg$direction_id, seg$num_routes))
      cat(sprintf("   AM: %d trips (%.1f min frequency)\n",
                  seg$trips_am, seg$interval_am))
      cat(sprintf("   PM: %d trips (%.1f min frequency)\n",
                  seg$trips_pm, seg$interval_pm))

      # Show route IDs
      routes_preview <- if (length(routes_list) > 5) {
        paste(c(routes_list[1:5], "..."), collapse = ", ")
      } else {
        paste(routes_list, collapse = ", ")
      }
      cat(sprintf("   Routes: %s\n\n", routes_preview))
    }
  } else {
    cat("\n⚠️  No multi-route corridor segments found!\n")
    cat("\nPossible reasons:\n")
    cat(sprintf("  - Buffer tolerance (%d ft) may be too small\n", BUFFER_TOLERANCE_FT))
    cat("  - Routes may not overlap geometrically in GTFS shapes\n")
    cat("  - Combined frequencies may not meet 15-minute threshold\n")
    cat("\nTry increasing buffer tolerance:\n")
    cat(sprintf("  Rscript test_cumtd_corridors.R %d\n", BUFFER_TOLERANCE_FT + 25))
  }
} else {
  cat("\n⚠️  No qualifying corridor segments found!\n")
  cat("\nThis could mean:\n")
  cat("  1. No route segments overlap within the buffer tolerance\n")
  cat("  2. Overlapping segments don't meet the 15-minute frequency threshold\n")
  cat("\nTry increasing buffer tolerance or checking input data.\n")
}

cat("\n", strrep("=", 80), "\n", sep = "")
cat("TEST COMPLETE\n")
cat(strrep("=", 80), "\n", sep = "")
