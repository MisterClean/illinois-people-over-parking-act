# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This project performs geospatial analysis to identify areas in Illinois that qualify for parking minimum relief under **Senate Bill 2111 (the "People Over Parking Act")**. The legislation prohibits municipalities from enforcing parking minimums in two types of transit-accessible areas:

- **Transit Hubs**: Within 1/2 mile of stations/stops where 3+ transit routes intersect with 15-minute or better frequency
- **Transit Corridors**: Within 1/8 mile of routes with 15-minute or better frequency

The analysis covers 14 transit agencies across Illinois and produces an interactive HTML map showing qualifying areas.

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
                       "data.table", "zip", "httr", "lubridate", "mapview",
                       "tigris", "kableExtra")
install.packages(required_packages)
```

## High-Level Architecture

### Code Structure

The project consists of:
- **Main analysis**: `sb2111-people-over-parking.Rmd` (~606 lines) - Thin orchestration layer
- **Modular functions**: `R/` directory (14 modules, ~4,000 lines) - Reusable, documented functions
- **Testing infrastructure**: `tests/testthat/` - Unit and integration tests (in development)

The main Rmd is a **thin orchestration layer** (54% reduction from original 1,330 lines) that:
1. Sources modular R functions from `R/` directory
2. Calls high-level orchestration functions with simple, readable code
3. Displays results with minimal inline logic

#### R/ Module Organization

**Configuration Module (1)** - Centralized agency configuration:
- **`agency_metadata.R`** - Single source of truth for all 14 transit agencies (display names, colors, GTFS URLs, rail service types, geographic filters)

**Foundation Modules (7)** - Low-level functions for specific tasks:
- **`gtfs_download.R`** - Download GTFS data with caching
- **`gtfs_normalize.R`** - Normalize GTFS tables across agencies
- **`gtfs_validate.R`** - Validate GTFS data quality (structure, coordinates, relationships)
- **`spatial_validate.R`** - Validate spatial operations (geometries, transformations, buffers)
- **`hub_identification.R`** - Identify rail stations and filter bus routes per agency
- **`spatial_clustering.R`** - Cluster stops spatially and verify route overlaps
- **`frequency_calc.R`** - Calculate service frequencies and apply SB2111 criteria

**Orchestration Modules (6)** - High-level workflows combining foundation modules:
- **`gtfs_processing.R`** - Complete GTFS workflow: download, normalize, validate, combine
- **`hub_processing.R`** - Complete hub identification workflow (rail + bus with clustering)
- **`corridor_processing.R`** - Corridor qualification and street buffering
- **`buffer_processing.R`** - Hub/corridor buffer creation and combination
- **`map_creation.R`** - Interactive Leaflet map generation
- **`summary_stats.R`** - Comprehensive summary statistics

See [R/README.md](R/README.md) for detailed module documentation.

#### Main Rmd Chunks (Simplified)

1. **`setup`** - knitr configuration
2. **`packages`** - Dependency installation and loading
3. **`load_existing_data`** - Source all R/ modules
4. **`download_gtfs`** - `process_all_gtfs_data()` - Download, normalize, validate 14 agencies (32 lines, was 145)
5. **`explore_direction_data`** - Check direction_id availability across agencies
6. **`process_hubs_UPDATED`** - `identify_all_hubs()` - Identify all transit hubs (11 lines, was 384)
7. **`process_corridors_and_buffers`** - Identify corridors and create buffers (30 lines, was 143)
8. **`calculate_areas`** - Calculate area coverage in square miles
9. **`create_map`** - `create_interactive_map()` - Generate Leaflet map (19 lines, was 186)
10. **`summary_stats`** - `generate_summary_statistics()` - All summaries (24 lines, was 34)

### Data Flow (Simplified with High-Level Functions)

```
GTFS Data (14 agencies across Illinois)
    ↓
    ↓ process_all_gtfs_data() [R/gtfs_processing.R]
    ↓   ├─ Downloads all agencies
    ↓   ├─ Normalizes tables (unique IDs, calendar handling)
    ↓   ├─ ✓ Validates data quality
    ↓   └─ Combines into unified tables
    ↓
Combined GTFS tables (stops, routes, trips, stop_times, calendar, calendar_dates)
    ↓
    ↓ identify_all_hubs() [R/hub_processing.R]
    ↓   ├─ Identifies rail stations (CTA, Metra, Metro STL)
    ↓   ├─ Identifies weekday services (calendar.txt + calendar_dates.txt)
    ↓   ├─ Filters to AM/PM peak periods (7-9am, 4-6pm)
    ↓   ├─ Clusters bus stops spatially (150ft radius)
    ↓   ├─ Verifies route overlap (street name matching)
    ↓   ├─ Calculates frequency metrics (AM & PM)
    ↓   └─ Applies qualification criteria (2+ routes, ≤15 min)
    ↓
Transit Hubs (rail + qualifying bus hubs)
    ↓
    ├──→ create_hub_buffers() [R/buffer_processing.R]
    │      └─ Creates 1/2 mile (2640 ft) buffers by agency
    │
    └──→ identify_qualifying_corridors() [R/corridor_processing.R]
           ├─ Calculates corridor frequencies (1+ route, ≤15 min)
           └─ create_corridor_buffers()
              ├─ Downloads TIGER/Line street network
              ├─ Snaps stops to nearest streets
              └─ Creates 1/8 mile (660 ft) buffers
    ↓
Hub Buffers + Corridor Buffers
    ↓
    ↓ create_combined_buffers() [R/buffer_processing.R]
    ↓   ├─ Combines all affected areas
    ↓   └─ Clips to Illinois boundary
    ↓
Combined Affected Areas + Hub Points
    ↓
    ↓ create_interactive_map() [R/map_creation.R]
    ↓   ├─ Builds Leaflet map with layers
    ↓   ├─ Adds agency-specific colors & controls
    ↓   └─ Creates rich popups for hubs
    ↓
Interactive HTML Map + Summary Statistics
```

**Key Improvements**:
- Each major step is now a single function call
- All validation happens automatically within orchestration functions
- Notebook reduced from 1,330 lines to 606 lines (54% reduction)
- Complex logic is hidden in well-documented, testable modules

### Data Sources

- **GTFS feeds**: Cached in `gtfs_cache/` directory (14 zip files)
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

**Recent Enhancements** (December 2024 - November 2025):

**Phase 1 - Foundation Modules** (December 2024):
1. **Modularization**: Extracted 2500+ lines of code into 7 reusable R modules
2. **Validation**: Added comprehensive GTFS and spatial validation (11 functions)
3. **Documentation**: 100% roxygen2 coverage on extracted functions
4. **Testing Infrastructure**: Created testthat framework (tests in development)
5. **Error Handling**: Improved error messages and recovery strategies

**Phase 2 - Orchestration & Simplification** (January 2025):
1. **High-Level Workflows**: Created 6 orchestration modules combining foundation functions
2. **Notebook Simplification**: Reduced main Rmd from 1,330 lines to 606 lines (54% reduction)
3. **Code Reusability**: Major processing steps now single function calls
4. **Improved Readability**: Inline logic replaced with well-named function calls
5. **Maintained Functionality**: Full integration test confirms identical output

**Phase 3 - Statewide Expansion** (November 2025):
1. **Agency Expansion**: Added 8 new transit agencies across Illinois (6 → 14 total)
2. **Centralized Configuration**: Created agency_metadata.R module as single source of truth
3. **Statewide Coverage**: Expanded from Chicago metro to comprehensive Illinois coverage

**Total Impact**:
- **14 modules** with ~4,000 lines of documented, reusable code
- **Major chunks simplified**: download_gtfs (-78%), process_hubs (-97%), create_map (-90%)
- All functions fully documented with roxygen2
- Code reusability across transit analysis projects
- Automated data quality validation
- Easier debugging and maintenance
- Foundation for automated testing
- Preserved algorithm rationale in documentation

## Additional Documentation

- **[R/README.md](R/README.md)** - Comprehensive module documentation and usage guide
- [docs/gtfs_normalization_strategy.md](docs/gtfs_normalization_strategy.md) - Detailed methodology for GTFS processing
- [docs/bus_transit_hub_verification_summary.md](docs/bus_transit_hub_verification_summary.md) - Bus hub qualification verification
- [docs/new_agencies_feed_characteristics.md](docs/new_agencies_feed_characteristics.md) - Characteristics of 8 new transit agencies
- [README.md](README.md) - Project documentation and context
