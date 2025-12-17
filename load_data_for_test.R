#!/usr/bin/env Rscript
# Minimal script to load data needed for corridor testing
# This extracts and runs just the essential code from the notebook

cat("Loading data for corridor testing...\n\n")

# Setup
suppressPackageStartupMessages({
  library(tidyverse)
  library(sf)
  library(data.table)
  library(lubridate)
  library(tidytransit)
  library(httr)
  library(zip)
  library(tigris)
  library(leaflet)
  library(leaflet.extras)
  library(mapview)
  library(kableExtra)
})

# Load all R modules
cat("Sourcing R modules...\n")
source("R/agency_metadata.R")
source("R/gtfs_download.R")
source("R/gtfs_normalize.R")
source("R/gtfs_validate.R")
source("R/spatial_validate.R")
source("R/hub_identification.R")
source("R/spatial_clustering.R")
source("R/frequency_calc.R")
source("R/gtfs_processing.R")
source("R/hub_processing.R")
source("R/corridor_processing.R")
source("R/buffer_processing.R")
source("R/map_creation.R")
source("R/summary_stats.R")

# Download Illinois boundary
cat("\nDownloading Illinois boundary...\n")
options(tigris_use_cache = TRUE)
illinois_boundary <- states(cb = TRUE, year = 2023) %>%
  filter(NAME == "Illinois") %>%
  st_transform(4326)

# Get agency configurations
cat("\nGetting agency configurations...\n")
agency_configs <- get_agency_configs_for_download()

# Process GTFS data
cat("\nProcessing GTFS data (this may take a few minutes)...\n")
agencies_data <- process_agencies_with_tidytransit(agency_configs, validate = TRUE)

# Combine data from all agencies
cat("\nCombining data from all agencies...\n")
combined_tables <- combine_agency_data(agencies_data)

# Extract combined datasets
all_stops <- combined_tables$stops
all_routes <- combined_tables$routes
all_trips <- combined_tables$trips
all_stop_times <- combined_tables$stop_times
all_calendar <- combined_tables$calendar
all_calendar_dates <- combined_tables$calendar_dates
all_shapes <- combined_tables$shapes

# Enrich stop_times with route information
cat("Enriching stop_times with route information...\n")
all_stop_times <- enrich_stop_times(all_stop_times, all_trips, all_routes)

cat(sprintf("Loaded %s stops, %s routes, %s trips, %s shapes\n",
            format(nrow(all_stops), big.mark = ","),
            format(nrow(all_routes), big.mark = ","),
            format(nrow(all_trips), big.mark = ","),
            format(nrow(all_shapes), big.mark = ",")))

# Process hubs to get peak bus stops
cat("\nProcessing transit hubs...\n")
hub_results <- identify_all_hubs(
  all_stops, all_routes, all_trips,
  all_stop_times, all_calendar, all_calendar_dates
)

all_hubs_sf <- hub_results$all_hubs_sf
am_peak_bus_stops <- hub_results$am_peak_bus_stops
pm_peak_bus_stops <- hub_results$pm_peak_bus_stops

cat(sprintf("Identified %d hubs\n", nrow(all_hubs_sf)))
cat(sprintf("AM peak bus stops: %s\n", format(nrow(am_peak_bus_stops), big.mark = ",")))
cat(sprintf("PM peak bus stops: %s\n", format(nrow(pm_peak_bus_stops), big.mark = ",")))

cat("\n✓ Data loaded successfully!\n")
cat("\nYou can now run: source('test_corridor_processing.R')\n")
