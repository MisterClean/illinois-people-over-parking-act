# Quick check of Route 101 service_ids
library(data.table)

# Load the cached PACE GTFS data
pace_zip <- list.files("gtfs_cache", pattern = "pace", full.names = TRUE)
pace_zip <- pace_zip[grepl("\\.zip$", pace_zip)]

if (length(pace_zip) > 0) {
  temp_dir <- tempdir()
  utils::unzip(pace_zip[1], exdir = temp_dir)
  pace_dir <- temp_dir
} else {
  stop("No PACE zip found")
}

# Read trips and routes
trips <- fread(file.path(pace_dir, "trips.txt"))
routes <- fread(file.path(pace_dir, "routes.txt"))

# Find Route 101
cat("=== Route 101 Info ===\n")
route_101 <- routes[route_id == "101" | route_short_name == "101"]
print(route_101)

# Get Route 101 trips
route_101_trips <- trips[route_id == "101"]
cat("\n=== Route 101 Service IDs ===\n")
cat(sprintf("Total Route 101 trips: %d\n", nrow(route_101_trips)))
cat("\nTrips by service_id:\n")
print(route_101_trips[, .N, by = service_id])

# Check the pattern
cat("\n=== Service ID Patterns ===\n")
cat("Route 101 service_ids:\n")
unique_services <- unique(route_101_trips$service_id)
for (s in unique_services) {
  cat(sprintf("  - %s\n", s))
}

cat("\nCompare to selected representative: 2025-12-WE-Weekday-01\n")
cat(sprintf("Match: %s\n", "2025-12-WE-Weekday-01" %in% unique_services))

# Show what routes use the WE service
cat("\n=== Routes using 2025-12-WE-Weekday-01 ===\n")
we_trips <- trips[service_id == "2025-12-WE-Weekday-01"]
we_routes <- unique(we_trips$route_id)
cat(sprintf("Number of routes: %d\n", length(we_routes)))
cat("First 20 routes:\n")
print(head(sort(we_routes), 20))

# Show what service_id patterns exist
cat("\n=== All Service ID Patterns ===\n")
all_services <- unique(trips$service_id)
cat(sprintf("Total unique service_ids: %d\n", length(all_services)))
cat("\nService ID prefixes (e.g., 2025-08-BT, 2025-12-NO):\n")
prefixes <- unique(substr(all_services, 1, 10))
print(sort(prefixes))

# Count trips per service pattern
cat("\n=== Trips by Service Pattern ===\n")
trips_by_service <- trips[, .N, by = service_id]
setorder(trips_by_service, -N)
print(head(trips_by_service, 15))
