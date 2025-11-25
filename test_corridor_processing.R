#!/usr/bin/env Rscript
# Test script for corridor processing with geometry-based overlap detection
#
# This script tests the corridor identification without running the full notebook.
#
# PREREQUISITES: Run the notebook through the "process_hubs_UPDATED" chunk first
# to ensure all required data objects exist in your workspace.
#
# Usage in RStudio:
#   1. Run notebook chunks up through "process_hubs_UPDATED"
#   2. Source this file: source("test_corridor_processing.R")
#
# Usage from command line:
#   Rscript test_corridor_processing.R

cat("=== Corridor Processing Test Script ===\n\n")

# Load required packages
cat("Loading required packages...\n")
suppressPackageStartupMessages({
  library(tidyverse)
  library(sf)
  library(data.table)
  library(lubridate)
})

# Source all R modules
cat("Sourcing R modules...\n")
source("R/corridor_processing.R")

# Check that required objects exist
required_objects <- c(
  "all_stops", "all_trips", "all_shapes",
  "am_peak_bus_stops", "pm_peak_bus_stops",
  "illinois_boundary"
)

cat("\nChecking for required data objects...\n")
missing_objects <- character()
for (obj in required_objects) {
  if (!exists(obj)) {
    missing_objects <- c(missing_objects, obj)
    cat(sprintf("  ✗ %s - MISSING\n", obj))
  } else {
    cat(sprintf("  ✓ %s\n", obj))
  }
}

if (length(missing_objects) > 0) {
  cat("\n❌ ERROR: Missing required objects!\n")
  cat("Please run the notebook through the 'process_hubs_UPDATED' chunk first.\n")
  cat("Required objects:", paste(missing_objects, collapse = ", "), "\n")
  stop("Cannot proceed without required data objects")
}

cat("\n✓ All required objects found\n\n")

# Print data object sizes for diagnostics
cat("Data object sizes:\n")
cat(sprintf("  all_stops: %s rows\n", format(nrow(all_stops), big.mark = ",")))
cat(sprintf("  all_trips: %s rows\n", format(nrow(all_trips), big.mark = ",")))
cat(sprintf("  all_shapes: %s rows\n", format(nrow(all_shapes), big.mark = ",")))
cat(sprintf("  am_peak_bus_stops: %s rows\n", format(nrow(am_peak_bus_stops), big.mark = ",")))
cat(sprintf("  pm_peak_bus_stops: %s rows\n", format(nrow(pm_peak_bus_stops), big.mark = ",")))

cat("\n", strrep("=", 70), "\n", sep = "")
cat("TESTING CORRIDOR IDENTIFICATION\n")
cat(strrep("=", 70), "\n", sep = "")

# Run corridor identification
cat("\nRunning identify_qualifying_corridors()...\n")

tryCatch({
  corridor_results <- identify_qualifying_corridors(
    all_stops, am_peak_bus_stops, pm_peak_bus_stops,
    all_trips, all_shapes
  )

  cat("\n✓ Corridor identification completed successfully!\n\n")

  # Extract results
  qualifying_corridor_segments_sf <- corridor_results$qualifying_corridor_segments
  corridor_qualification_summary <- corridor_results$qualification_summary

  # Print summary statistics
  cat(strrep("=", 70), "\n", sep = "")
  cat("RESULTS SUMMARY\n")
  cat(strrep("=", 70), "\n\n", sep = "")

  cat(sprintf("Total qualifying corridor segments: %d\n", nrow(qualifying_corridor_segments_sf)))

  if (nrow(qualifying_corridor_segments_sf) > 0) {
    # Analyze by number of routes
    multi_route_count <- sum(qualifying_corridor_segments_sf$num_routes > 1)
    single_route_count <- sum(qualifying_corridor_segments_sf$num_routes == 1)

    cat(sprintf("\nBreakdown by route count:\n"))
    cat(sprintf("  Single-route segments: %d\n", single_route_count))
    cat(sprintf("  Multi-route segments: %d ⭐\n", multi_route_count))

    if (multi_route_count > 0) {
      cat("\n✓ SUCCESS: Multi-route corridors detected!\n")

      # Show examples of multi-route corridors
      cat("\nExample multi-route corridors:\n")
      multi_route_examples <- qualifying_corridor_segments_sf[qualifying_corridor_segments_sf$num_routes > 1, ]
      multi_route_examples <- multi_route_examples[order(-multi_route_examples$num_routes), ]

      # Show top 5
      n_show <- min(5, nrow(multi_route_examples))
      for (i in 1:n_show) {
        seg <- multi_route_examples[i, ]
        cat(sprintf("\n  %d. %s (direction %d)\n", i, seg$agency, seg$direction_id))
        cat(sprintf("     Routes: %d\n", seg$num_routes))
        cat(sprintf("     AM trips: %d (%.1f min frequency)\n",
                    seg$trips_am, seg$interval_am))
        cat(sprintf("     PM trips: %d (%.1f min frequency)\n",
                    seg$trips_pm, seg$interval_pm))

        # Show first few routes
        routes <- strsplit(as.character(seg$routes), ";")[[1]]
        routes_preview <- if (length(routes) > 3) {
          paste(c(routes[1:3], "..."), collapse = ", ")
        } else {
          paste(routes, collapse = ", ")
        }
        cat(sprintf("     Route IDs: %s\n", routes_preview))
      }
    } else {
      cat("\n⚠️  WARNING: No multi-route corridors detected!\n")
      cat("This suggests the overlap detection may need tuning.\n")
    }

    # Frequency distribution
    cat("\n\nFrequency metrics:\n")
    cat(sprintf("  Best AM frequency: %.1f minutes\n",
                min(qualifying_corridor_segments_sf$interval_am[qualifying_corridor_segments_sf$interval_am < Inf])))
    cat(sprintf("  Best PM frequency: %.1f minutes\n",
                min(qualifying_corridor_segments_sf$interval_pm[qualifying_corridor_segments_sf$interval_pm < Inf])))

    # Agency breakdown
    cat("\n\nSegments by agency:\n")
    agency_counts <- table(qualifying_corridor_segments_sf$agency)
    for (agency in names(sort(agency_counts, decreasing = TRUE))) {
      cat(sprintf("  %s: %d segments\n", agency, agency_counts[agency]))
    }

  } else {
    cat("\n❌ ERROR: No qualifying corridors found!\n")
    cat("This suggests an issue with the corridor identification logic.\n")
  }

  cat("\n", strrep("=", 70), "\n", sep = "")
  cat("TEST COMPLETE\n")
  cat(strrep("=", 70), "\n", sep = "")

}, error = function(e) {
  cat("\n❌ ERROR during corridor identification:\n")
  cat(sprintf("   %s\n", e$message))
  cat("\nStack trace:\n")
  print(traceback())
  stop(e)
})
