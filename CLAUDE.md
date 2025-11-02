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

This is a **monolithic R Markdown analysis** in a single file (`sb2111-people-over-parking.Rmd`, 1505 lines). The entire workflow is reproducible from one "Knit" command. Code is organized into 11 major chunks:

1. **`setup`** - knitr configuration
2. **`packages`** - Dependency installation and loading
3. **`load_existing_data`** - GTFS download/extraction functions
4. **`download_gtfs`** - Download from 5 agencies, normalize, combine data
5. **`explore_direction_data`** - Check direction_id availability across agencies
6. **`cluster_stops`** - Spatial clustering functions (150ft radius for bus stops)
7. **`process_hubs_UPDATED`** - Identify qualifying transit hubs (rail stations + bus hubs with overlap verification)
8. **`process_corridors_and_buffers`** - Identify corridors and create spatial buffers
9. **`calculate_areas`** - Calculate area coverage in square miles
10. **`create_map`** - Generate interactive Leaflet map with layers
11. **`summary_stats`** - Statistical summaries

### Data Flow

```
GTFS Data (5 agencies: CTA, Pace, Metra, Metro STL, CUMTD)
    ↓ download_and_extract_gtfs()
    ↓ read_normalize_gtfs()
Combined GTFS tables (stops, routes, trips, stop_times, calendar)
    ↓ Filter weekday service + peak hours (AM: 7-9, PM: 4-6)
    ↓ Split analysis by AM/PM peaks
Transit Hubs (rail + bus)
    ↓ cluster_stops_spatial() [150ft radius]
    ↓ verify_route_overlap_at_cluster() [street name matching]
    ↓ Buffer 1/2 mile
Transit Corridors
    ↓ Snap to street network (TIGER/Line)
    ↓ Buffer 1/8 mile
Combined Areas
    ↓ Clip to Illinois boundary
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

## Additional Documentation

- [docs/gtfs_normalization_strategy.md](docs/gtfs_normalization_strategy.md) - Detailed methodology for GTFS processing
- [docs/bus_transit_hub_verification_summary.md](docs/bus_transit_hub_verification_summary.md) - Bus hub qualification verification
- [README.md](README.md) - Project documentation and context
