# R/ Module Directory

This directory contains modular R functions extracted from the main analysis notebook (`sb2111-people-over-parking.Rmd`). All functions are documented with roxygen2 and designed for reusability.

## Module Organization

### 📥 **Data Acquisition** (`gtfs_download.R`, `gtfs_normalize.R`)

**Purpose**: Download, extract, and normalize GTFS data from multiple transit agencies

**Key Functions**:
- `download_and_extract_gtfs()` - Downloads GTFS ZIP files with caching and error recovery
- `read_normalize_gtfs()` - Reads and normalizes GTFS tables with agency-specific unique IDs

**Usage**:
```r
source("R/gtfs_download.R")
source("R/gtfs_normalize.R")

# Download CTA GTFS
cta_dir <- download_and_extract_gtfs("cta", "https://...")
cta_data <- read_normalize_gtfs("cta", cta_dir)
```

**Key Features**:
- Automatic caching to `gtfs_cache/` directory
- Fallback to cached data if download fails
- Unique ID prefixing (e.g., `cta_1234`, `pace_5678`) to prevent collisions
- Handles integer64 and missing field issues across agencies
- Reads both `calendar.txt` and `calendar_dates.txt` to support different GTFS scheduling approaches

---

### ✅ **Data Validation** (`gtfs_validate.R`, `spatial_validate.R`)

**Purpose**: Validate GTFS data quality and spatial operation integrity

#### GTFS Validation (`gtfs_validate.R`)

**Key Functions**:
- `validate_gtfs_structure()` - Checks required files and fields
- `validate_gtfs_coordinates()` - Validates lat/lon bounds and detects errors
- `validate_gtfs_relationships()` - Checks referential integrity (foreign keys)
- `generate_quality_report()` - Comprehensive validation report
- `print_validation_report()` - Pretty-printed output
- `validate_all_gtfs()` - Batch validation across agencies

**Usage**:
```r
source("R/gtfs_validate.R")

# Validate single agency
report <- generate_quality_report(cta_data, "cta")
print_validation_report(list(cta = report))

# Validate all agencies
all_results <- validate_all_gtfs(list(
  cta = cta_data,
  pace = pace_data,
  metra = metra_data
))
print_validation_report(all_results)
```

**Checks Performed**:
- ✓ All required GTFS files present and non-empty
- ✓ Required fields exist (stop_id, route_id, etc.)
- ✓ Coordinates within valid ranges (lat ±90, lon ±180)
- ✓ Coordinates within Illinois bounds (with warnings for border agencies)
- ✓ Trips reference existing routes
- ✓ Stop_times reference existing trips and stops
- ✓ No orphaned data

#### Spatial Validation (`spatial_validate.R`)

**Key Functions**:
- `validate_geometries()` - Checks and repairs invalid geometries
- `validate_coordinate_transform()` - Verifies CRS transformations
- `validate_buffer_result()` - Validates buffer operations
- `validate_illinois_bounds()` - Checks state boundary compliance

**Usage**:
```r
source("R/spatial_validate.R")

# Validate and repair geometries
stops_sf <- st_as_sf(stops, coords = c("stop_lon", "stop_lat"), crs = 4326)
stops_sf <- validate_geometries(stops_sf, "stops")

# Validate transformation
stops_il <- validate_coordinate_transform(stops_sf, 3435, "stops_to_state_plane")

# Validate buffers
buffers <- st_buffer(stops_il, 2640)  # 1/2 mile
buffers <- validate_buffer_result(buffers, 2640, "ft", "hub_buffers")
```

**Protections**:
- Detects and repairs invalid geometries (self-intersections, etc.)
- Warns if transformations change area unexpectedly
- Verifies buffer distances are correct
- Flags geometries outside Illinois (with tolerance for border cases)

---

### 🗺️ **Spatial Analysis** (`spatial_clustering.R`)

**Purpose**: Cluster nearby transit stops and verify route overlaps

**Key Functions**:
- `cluster_stops_spatial()` - Groups stops within 150ft radius using connected components algorithm
- `verify_route_overlap_at_cluster()` - Verifies routes share street names (prevents false positives)

**Usage**:
```r
source("R/spatial_clustering.R")

# Cluster bus stops
clustered <- cluster_stops_spatial(bus_stops, cluster_radius_ft = 150)

# Check cluster sizes
clustered[, .N, by = cluster_id]

# Verify route overlap at clusters
overlap <- verify_route_overlap_at_cluster(clustered, all_stop_times)
true_hubs <- overlap[has_overlap == TRUE]
```

**Algorithm Details**:
- **Method**: Breadth-first search (BFS) connected components
- **Why BFS?**: Deterministic, guarantees all stops within radius are clustered, simple to verify
- **Why 150ft?**: Balances capturing same-intersection stops vs avoiding parallel-street false positives
- **Street Verification**: Parses stop names to extract street names, counts route overlaps

**Limitations**:
- Relies on consistent stop naming (e.g., "State & Madison")
- May miss overlaps with different street abbreviations ("Street" vs "St")
- Cannot detect semantic equivalence ("Main St" vs "Route 66")

---

### ⏱️ **Frequency Analysis** (`frequency_calc.R`)

**Purpose**: Calculate transit service frequency and apply SB2111 qualification criteria

**Key Functions**:
- `filter_peak_period_stop_times()` - Extract AM/PM peak arrivals
- `calculate_peak_frequency()` - Compute service intervals (headway)
- `combine_am_pm_metrics()` - Merge AM and PM data
- `apply_hub_qualification()` - Apply hub criteria (2+ routes, ≤15 min)
- `apply_corridor_qualification()` - Apply corridor criteria (1+ route, ≤15 min)

**Usage**:
```r
source("R/frequency_calc.R")

# Filter to AM peak (7-9am)
am_stops <- filter_peak_period_stop_times(all_stop_times, "07:00:00", "09:00:00")
pm_stops <- filter_peak_period_stop_times(all_stop_times, "16:00:00", "18:00:00")

# Calculate frequency
am_metrics <- calculate_peak_frequency(am_stops, c("cluster_id", "agency"), 120, "am")
pm_metrics <- calculate_peak_frequency(pm_stops, c("cluster_id", "agency"), 120, "pm")

# Combine periods
all_metrics <- combine_am_pm_metrics(am_metrics, pm_metrics, c("cluster_id", "agency"))

# Apply qualification
qualified <- apply_hub_qualification(all_metrics, min_routes = 2, max_interval_minutes = 15)
hubs <- qualified[qualifies_hub == TRUE]
```

**Key Concepts**:
- **Frequency**: Service interval in minutes = peak_duration / number_of_trips
  - Example: 120 min / 8 trips = 15 minute headway
- **Direction-aware**: If `direction_id` exists, calculates frequency per direction
  - Prevents double-counting bidirectional routes
- **Either/Or Logic**: Qualifies if criteria met in AM **OR** PM (not necessarily both)
  - Captures directional flow patterns (inbound AM, outbound PM)

**SB2111 Criteria**:
- **Hubs**: 2+ routes AND ≤15 min interval (in at least one peak)
- **Corridors**: 1+ route AND ≤15 min interval (in at least one peak)

---

### 🚏 **Hub Identification** (`hub_identification.R`)

**Purpose**: Identify rail stations and process transit hubs per agency

**Key Functions**:

**Rail Station Identification**:
- `identify_cta_rail_stations()` - CTA L train stations (parent_station + location_type logic)
- `identify_metra_rail_stations()` - Metra commuter rail (excludes Wisconsin)
- `identify_metro_stl_metrolink_stations()` - Metro STL light rail
- `identify_all_rail_stations()` - Combines all agencies

**Bus Trip Filtering**:
- `identify_weekday_services()` - Filters to Mon-Fri service_ids (supports both calendar.txt and calendar_dates.txt)
- `identify_weekday_services_from_dates()` - Identifies weekday services from calendar_dates.txt for agencies like CUMTD
- `identify_bus_routes()` - Extracts bus routes (route_type = 3)
- `get_weekday_bus_trips()` - Combines filters for weekday bus trips

**Helper Functions**:
- `get_bus_stops_for_clustering()` - Stops appearing in peak periods
- `determine_grouping_cols()` - Handles direction_id presence/absence

**Usage**:
```r
source("R/hub_identification.R")

# Identify all rail stations
rail_stations <- identify_all_rail_stations(all_stops, all_routes, all_stop_times)
rail_stations[, .N, by = agency]

# Get weekday bus trips (supports both calendar approaches)
weekday_svc <- identify_weekday_services(all_calendar, all_calendar_dates)
bus_routes <- identify_bus_routes(all_routes)
bus_trips <- get_weekday_bus_trips(all_trips, weekday_svc, bus_routes)

# Determine grouping (handles direction_id)
group_cols <- determine_grouping_cols(bus_trips, c("cluster_id", "agency"))
```

**Agency-Specific Logic**:
- **CTA**: Uses GTFS parent_station hierarchy + location_type
- **Metra**: All stops are rail; filters latitude ≤42.5°N (IL/WI border)
- **Metro STL**: Identifies MetroLink (route_type=2) vs MetroBus (route_type=3)
- **CUMTD**: Bus-only agency; uses calendar_dates.txt exclusively for service definition (university schedule complexity)
- **Pace**: Bus-only agency, no rail identification needed

---

## Module Dependencies

```
gtfs_download.R
gtfs_normalize.R
    ↓
gtfs_validate.R (validates normalized data)
    ↓
hub_identification.R (identifies stations/routes)
    ↓
frequency_calc.R (calculates frequencies)
    ↓
spatial_clustering.R (clusters stops)
    ↓
spatial_validate.R (validates spatial operations)
```

**Loading Order** (in `sb2111-people-over-parking.Rmd`):
```r
source("R/gtfs_download.R")
source("R/gtfs_normalize.R")
source("R/gtfs_validate.R")
source("R/spatial_validate.R")
source("R/spatial_clustering.R")
source("R/frequency_calc.R")
source("R/hub_identification.R")
```

---

## Function Naming Conventions

- **Verbs**: Functions start with action verbs
  - `download_*`, `read_*`, `validate_*`, `identify_*`, `calculate_*`, `apply_*`
- **Suffixes**: Indicate scope
  - `*_gtfs`: Operates on GTFS data
  - `*_spatial`: Spatial operations
  - `*_at_cluster`: Operates at cluster level
  - `*_all_*`: Combines multiple agencies

---

## Return Value Conventions

- **Single agency**: data.table
- **Multiple agencies**: Named list of data.tables
- **Validation**: List with `valid` (logical) and `issues` (character vector)
- **Spatial**: sf objects or data.table with geometry columns

---

## Testing

Test files should mirror module structure:
```
tests/testthat/
├── test-gtfs-download.R
├── test-gtfs-normalize.R
├── test-gtfs-validate.R
├── test-spatial-validate.R
├── test-spatial-clustering.R
├── test-frequency-calc.R
├── test-hub-identification.R
└── test-integration.R
```

See testing plan in project documentation.

---

## Common Patterns

### Error Handling

Functions use informative error messages:
```r
if (!is.numeric(cluster_radius_ft) || cluster_radius_ft <= 0) {
  stop("cluster_radius_ft must be a positive number")
}
```

### Data Validation

Many functions validate inputs:
```r
if (!"unique_stop_id" %in% names(stops_dt)) {
  stop("stops_dt must contain unique_stop_id column")
}
```

### Progress Reporting

Long-running functions use `cat()` for progress:
```r
cat(sprintf("Created %d clusters from %d stops\n", n_clusters, n_stops))
```

---

## Documentation Standards

All functions include roxygen2 documentation:
- `@param` - Parameter descriptions with types
- `@return` - Return value structure
- `@details` - Implementation details and rationale
- `@examples` - Usage examples (in `\dontrun{}` blocks)
- `@export` - Export tag for package use

---

## Contributing

When adding new functions:
1. ✓ Add roxygen2 documentation
2. ✓ Follow naming conventions
3. ✓ Include `@examples` section
4. ✓ Explain algorithm choices in `@details`
5. ✓ Add validation for common errors
6. ✓ Write corresponding unit tests
7. ✓ Update this README

---

## Questions?

See project documentation:
- [../CLAUDE.md](../CLAUDE.md) - Project overview and AI assistant guidance
- [../docs/gtfs_normalization_strategy.md](../docs/gtfs_normalization_strategy.md) - GTFS processing details
- [../docs/bus_transit_hub_verification_summary.md](../docs/bus_transit_hub_verification_summary.md) - Hub verification logic
