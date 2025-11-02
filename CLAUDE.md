# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This project performs geospatial analysis to identify areas in Illinois that qualify for parking minimum relief under **Senate Bill 2111 (the "People Over Parking Act")**. The legislation prohibits municipalities from enforcing parking minimums in two types of transit-accessible areas:

- **Transit Hubs**: Within 1/2 mile of stations/stops where 3+ transit routes intersect with 15-minute or better frequency
- **Transit Corridors**: Within 1/8 mile of routes with 15-minute or better frequency

The analysis covers all major transit agencies across Illinois (CTA, Pace, Metra, Metro St. Louis, CUMTD) and produces an interactive HTML map showing qualifying areas.

## Commands

This is an R Markdown-based project with no build system. Execute the analysis in R/RStudio:

```r
# Render the complete analysis (primary command)
rmarkdown::render("sb2111-people-over-parking.Rmd")
```

Or click the "Knit" button in RStudio when viewing `sb2111-people-over-parking.Rmd`.

**Install dependencies:**
```r
required_packages <- c("tidyverse", "sf", "leaflet", "leaflet.extras",
                       "data.table", "zip", "httr", "lubridate", "mapview", "tigris")
install.packages(required_packages)
```

## High-Level Architecture

### Code Structure

The project consists of:
- **Main analysis**: `sb2111-people-over-parking.Rmd` (~1330 lines) - Orchestrates the complete workflow
- **Modular functions**: `R/` directory (7 modules, ~2500 lines) - Reusable, documented functions
- **Testing infrastructure**: `tests/testthat/` - Unit and integration tests (in development)

The main Rmd is now a **thin orchestration layer** that:
1. Sources modular R functions from `R/` directory
2. Runs data quality validation
3. Executes the analysis pipeline
4. Generates interactive visualizations

#### R/ Module Organization

**Data Acquisition & Validation** (4 modules):
- **`gtfs_download.R`** - Download GTFS data with caching
- **`gtfs_normalize.R`** - Normalize GTFS tables across agencies
- **`gtfs_validate.R`** - Validate GTFS data quality (structure, coordinates, relationships)
- **`spatial_validate.R`** - Validate spatial operations (geometries, transformations, buffers)

**Analysis & Processing** (3 modules):
- **`hub_identification.R`** - Identify rail stations and filter bus routes per agency
- **`spatial_clustering.R`** - Cluster stops spatially and verify route overlaps
- **`frequency_calc.R`** - Calculate service frequencies and apply SB2111 criteria

See [R/README.md](R/README.md) for detailed module documentation.

#### Main Rmd Chunks

1. **`setup`** - knitr configuration
2. **`packages`** - Dependency installation and loading
3. **`load_existing_data`** - Source R/ modules
4. **`download_gtfs`** - Download from 5 agencies, normalize, **validate**, combine data
5. **`explore_direction_data`** - Check direction_id availability across agencies
6. **`cluster_stops`** - Load clustering functions (defined in R/spatial_clustering.R)
7. **`process_hubs_UPDATED`** - Identify qualifying transit hubs (rail stations + bus hubs with overlap verification)
8. **`process_corridors_and_buffers`** - Identify corridors and create spatial buffers
9. **`calculate_areas`** - Calculate area coverage in square miles
10. **`create_map`** - Generate interactive Leaflet map with layers
11. **`summary_stats`** - Statistical summaries

### Data Flow

```
GTFS Data (5 agencies: CTA, Pace, Metra, Metro STL, CUMTD)
    ↓ download_and_extract_gtfs() [R/gtfs_download.R]
    ↓ read_normalize_gtfs() [R/gtfs_normalize.R] - reads calendar.txt + calendar_dates.txt
    ↓ ✓ VALIDATION: validate_all_gtfs() [R/gtfs_validate.R]
Combined GTFS tables (stops, routes, trips, stop_times, calendar, calendar_dates)
    ↓ identify_weekday_services() [R/hub_identification.R] - supports both calendar approaches
    ↓ identify_bus_routes() [R/hub_identification.R]
    ↓ filter_peak_period_stop_times() [R/frequency_calc.R]
    ↓ Split analysis by AM/PM peaks (7-9am, 4-6pm)
Transit Hubs (rail + bus)
    ↓ identify_all_rail_stations() [R/hub_identification.R]
    ↓ cluster_stops_spatial() [R/spatial_clustering.R] - 150ft radius
    ↓ verify_route_overlap_at_cluster() [R/spatial_clustering.R] - street name matching
    ↓ calculate_peak_frequency() [R/frequency_calc.R]
    ↓ apply_hub_qualification() [R/frequency_calc.R]
    ↓ ✓ VALIDATION: validate_geometries() [R/spatial_validate.R]
    ↓ Buffer 1/2 mile (2640 ft)
    ↓ ✓ VALIDATION: validate_buffer_result() [R/spatial_validate.R]
Transit Corridors
    ↓ Snap to street network (TIGER/Line)
    ↓ Buffer 1/8 mile (660 ft)
    ↓ apply_corridor_qualification() [R/frequency_calc.R]
Combined Areas
    ↓ ✓ VALIDATION: validate_coordinate_transform() [R/spatial_validate.R]
    ↓ Clip to Illinois boundary
    ↓ ✓ VALIDATION: validate_illinois_bounds() [R/spatial_validate.R]
Interactive Leaflet Map (HTML output)
```

### Data Sources

- **GTFS feeds**: Cached in `gtfs_cache/` directory (5 zip files)
- **Geographic data**: US Census TIGER/Line shapefiles via `tigris` package
- **Output**: Self-contained HTML with embedded Leaflet map

## Key Concepts and Conventions

### GTFS Normalization Strategy

All transit agencies get unique identifiers prefixed with agency name to prevent ID collisions when combining datasets:
- `unique_stop_id`: e.g., `cta_1234`, `pace_5678`
- `unique_route_id`: e.g., `metra_UP-N`, `metro_stl_90`
- `unique_trip_id`: e.g., `cta_trip_1234`, `cumtd_trip_5678`

See [docs/gtfs_normalization_strategy.md](docs/gtfs_normalization_strategy.md) for details.

### Peak Period Split Analysis

Frequency is calculated separately for two peak periods:
- **AM peak**: 7:00-9:00 (120 minutes)
- **PM peak**: 16:00-18:00 (120 minutes)

Areas qualify if criteria are met in **EITHER** period (not both required). This "either" logic ensures areas with strong directional flow (e.g., inbound AM, outbound PM) still qualify.

### Direction-Aware Frequency Calculation

Uses `direction_id` from GTFS when available. Frequency is calculated separately per direction to avoid double-counting bidirectional routes. Formula: `peak_duration_minutes / number_of_trips`

### Bus Hub Clustering and Verification

Bus stops are grouped into "hubs" using two-stage verification:
1. **Spatial clustering**: Groups stops within 150 feet as single cluster (EPSG:3435 for accurate distance calculation)
2. **Street name verification**: Parses stop names to verify routes actually intersect at same street location, preventing false positives from parallel routes

See [docs/bus_transit_hub_verification_summary.md](docs/bus_transit_hub_verification_summary.md) for details.

### Spatial Reference Systems

- **EPSG:3435** (Illinois State Plane East): Used for distance calculations in feet (buffers, clustering)
- **EPSG:4326** (WGS84): Used for web mapping and final Leaflet output
- All buffers are clipped to Illinois state boundary

### Coding Conventions

- **Variable/function naming**: snake_case (`all_stops`, `download_and_extract_gtfs`)
- **GTFS fields**: Preserved from GTFS standard (`stop_id`, `route_type`, `trip_id`)
- **Performance**: Heavy use of `data.table` for large GTFS datasets
- **Error handling**: Falls back to cached GTFS files if download fails

## ast-grep Usage Guide

You run in an environment where ast-grep (`sg`) is available. Use `sg run --lang <language> --pattern '<pattern>' <paths>` for syntax-aware structural matching instead of text-only tools like `rg` or `grep` when searching for code patterns.

## Testing

### Test Infrastructure

The project uses `testthat` for unit and integration testing:

```r
# Run all tests
devtools::test()

# Run specific test file
testthat::test_file("tests/testthat/test-gtfs-validate.R")
```

**Test Coverage** (in development):
- GTFS validation functions
- Spatial clustering algorithms
- Frequency calculations
- Hub identification logic
- Integration tests for full pipeline

See [R/README.md](R/README.md) for testing conventions.

### Running Validation

The analysis automatically runs data quality validation:

```r
# Validation runs automatically when rendering
rmarkdown::render("sb2111-people-over-parking.Rmd")

# Or source modules and run manually
source("R/gtfs_validate.R")
validation_results <- validate_all_gtfs(list(
  cta = cta_data,
  pace = pace_data,
  metra = metra_data,
  metro_stl = metro_stl_data,
  cumtd = cumtd_data
))
print_validation_report(validation_results)
```

Validation catches:
- Missing or corrupted GTFS files
- Invalid coordinates (0,0 or outside Illinois)
- Broken relationships (orphaned trips, missing stops)
- Invalid geometries from spatial operations
- Unexpected CRS transformation errors

## Code Quality Improvements

**Recent Enhancements** (December 2024 - January 2025):

1. **Modularization**: Extracted 2500+ lines of code into 7 reusable R modules
2. **Validation**: Added comprehensive GTFS and spatial validation (11 functions)
3. **Documentation**: 100% roxygen2 coverage on extracted functions
4. **Testing Infrastructure**: Created testthat framework (tests in development)
5. **Error Handling**: Improved error messages and recovery strategies

**Benefits**:
- Code reusability across transit analysis projects
- Automated data quality validation
- Easier debugging and maintenance
- Foundation for automated testing
- Preserved algorithm rationale in documentation

## Additional Documentation

- **[R/README.md](R/README.md)** - Comprehensive module documentation and usage guide
- [docs/gtfs_normalization_strategy.md](docs/gtfs_normalization_strategy.md) - Detailed methodology for GTFS processing
- [docs/bus_transit_hub_verification_summary.md](docs/bus_transit_hub_verification_summary.md) - Bus hub qualification verification
- [README.md](README.md) - Project documentation and context
