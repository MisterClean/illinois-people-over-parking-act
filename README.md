# Illinois People Over Parking Act (SB2111) - Transit Access Analysis

This repository contains a comprehensive geospatial analysis that identifies areas in Illinois qualifying for parking minimum relief under **Senate Bill 2111** (the "People Over Parking Act"), signed into law in 2024.

The project combines GTFS transit data from 5 agencies across Illinois with advanced spatial analysis to map transit-accessible areas where municipalities cannot enforce parking minimums.

## What is SB2111?

Senate Bill 2111 prohibits municipalities in Illinois from enforcing parking minimums in two types of transit-accessible areas:

1. **Transit Hubs**: Within 1/2 mile of transit stations/stops where:
   - **3+ fixed-route transit lines intersect**
   - Service operates **at least every 15 minutes** during peak periods (either AM or PM)

2. **Transit Corridors**: Within 1/8 mile of transit routes where:
   - **1+ transit routes operate**
   - Service operates **at least every 15 minutes** during peak periods (either AM or PM)

**Peak Periods** are defined as:
- **AM Peak**: 7:00 AM - 9:00 AM (120 minutes)
- **PM Peak**: 4:00 PM - 6:00 PM (120 minutes)

**"Either" Logic**: Areas qualify if criteria are met in **either** AM **or** PM peak (not necessarily both). This captures directional flow patterns (e.g., heavy inbound service AM, heavy outbound service PM).

---

## Geographic Coverage

This analysis covers **all major transit agencies in Illinois**:

### Chicago Metropolitan Area (6 counties)
- **CTA** (Chicago Transit Authority) - Bus and Rail (L train)
- **Pace** - Suburban Bus
- **Metra** - Commuter Rail

### St. Louis Metropolitan Area, Illinois Side (3 counties)
- **Metro St. Louis** - Bus and MetroLink (light rail)

### Champaign-Urbana (1 county)
- **CUMTD** (Champaign-Urbana Mass Transit District) - Bus

**Total Coverage**: 10 counties, 5 transit agencies, 1000+ routes analyzed

**Important**: All transit buffers are clipped to Illinois state boundaries to comply with the state law's jurisdiction. Stations in Wisconsin (Metra) and Missouri (Metro STL) are excluded.

---

## Project Architecture

This project uses a **modular architecture** with separate, reusable R functions organized by responsibility:

```
illinois-people-over-parking-act/
├── README.md                          # This file
├── CLAUDE.md                          # AI assistant guidance & architecture docs
├── sb2111-people-over-parking.Rmd     # Main analysis orchestration (~1330 lines)
│
├── R/                                 # Modular R functions (7 modules, ~2000 lines)
│   ├── README.md                      # Comprehensive module documentation
│   ├── gtfs_download.R                # Download GTFS data with caching
│   ├── gtfs_normalize.R               # Normalize GTFS across agencies
│   ├── gtfs_validate.R                # Validate GTFS data quality
│   ├── spatial_validate.R             # Validate spatial operations
│   ├── hub_identification.R           # Identify rail stations & bus routes
│   ├── spatial_clustering.R           # Cluster stops & verify overlaps
│   └── frequency_calc.R               # Calculate frequencies & apply criteria
│
├── tests/                             # Testing infrastructure
│   └── testthat/                      # Unit and integration tests
│       └── fixtures/                  # Test data
│
├── docs/                              # Methodology documentation
│   ├── gtfs_normalization_strategy.md
│   └── bus_transit_hub_verification_summary.md
│
└── gtfs_cache/                        # Cached GTFS data (5 zip files)
    ├── cta_gtfs.zip
    ├── pace_gtfs.zip
    ├── metra_gtfs.zip
    ├── metro_stl_gtfs.zip
    └── cumtd_gtfs.zip
```

### Code Organization

**Main Analysis** (`sb2111-people-over-parking.Rmd`):
- Thin orchestration layer that sources R modules
- Runs the complete analysis pipeline
- Generates interactive visualizations
- Produces final HTML output

**Modular Functions** (`R/` directory):
- **Data Acquisition** - Download and normalize GTFS data
- **Data Validation** - Validate GTFS quality and spatial operations
- **Analysis & Processing** - Identify hubs, calculate frequencies, apply criteria

See [R/README.md](R/README.md) for detailed module documentation.

---

## Key Features

### 1. **Comprehensive Data Validation** ✅

The analysis includes automatic data quality validation:

**GTFS Validation**:
- Checks required files and fields exist
- Validates coordinate bounds (detects invalid lat/lon)
- Verifies referential integrity (trips reference valid routes, etc.)
- Generates quality reports for each agency

**Spatial Validation**:
- Validates and repairs invalid geometries
- Verifies coordinate transformations preserve accuracy
- Validates buffer distances are correct
- Checks Illinois boundary compliance

Validation runs automatically and reports issues before they corrupt the analysis.

### 2. **Split Peak Analysis**

Analyzes AM peak (7:00-9:00) and PM peak (4:00-6:00) **separately**, with "either/or" qualification logic:
- Captures directional flow patterns (inbound AM, outbound PM)
- More inclusive of genuine transit accessibility
- Reflects actual transit usage patterns

### 3. **Bus Hub Clustering & Verification**

Two-stage process to identify bus hubs:

**Stage 1: Spatial Clustering**
- Groups bus stops within **150 feet** as a single "hub"
- Uses Illinois State Plane (EPSG:3435) for accurate distance in feet
- Connected components algorithm ensures all nearby stops are clustered

**Stage 2: Route Overlap Verification**
- Parses stop names to extract street names
- Verifies routes actually intersect at same street location
- Prevents false positives from nearby parallel routes

Example: Stops at "State & Madison" and "State & Monroe" are spatially close but on different streets, so they're NOT counted as a hub.

See [docs/bus_transit_hub_verification_summary.md](docs/bus_transit_hub_verification_summary.md) for details.

### 4. **Direction-Aware Frequency Calculation**

Uses GTFS `direction_id` (when available) to calculate frequency separately per direction:
- Prevents double-counting bidirectional routes
- Example: Route with 4 northbound + 4 southbound trips in 2 hours
  - **With direction**: 120 min / 4 trips = 30 min interval (does not qualify)
  - **Without direction**: 120 min / 8 trips = 15 min interval (misleadingly qualifies)

### 5. **Agency-Specific Logic**

Handles differences in GTFS implementation across agencies:

- **CTA**: Uses `parent_station` hierarchy and `location_type` to identify L stations
- **Metra**: All stops are rail stations; filters latitude ≤42.5°N to exclude Wisconsin
- **Metro STL**: Distinguishes MetroLink (light rail, route_type=2) from MetroBus (route_type=3)
- **CUMTD/Pace**: Bus-only agencies

### 6. **Geographic Precision**

- All buffers clipped to Illinois state boundaries
- Uses TIGER/Line street network data for accurate corridor distances
- Distance calculations in feet using Illinois State Plane projection (EPSG:3435)
- Final output in web-friendly WGS84 (EPSG:4326)

---

## Requirements

### R Packages

The notebook automatically installs required packages:

```r
required_packages <- c(
  "tidyverse",        # Data manipulation and visualization
  "sf",              # Spatial data handling
  "leaflet",         # Interactive mapping
  "leaflet.extras",  # Additional Leaflet features
  "data.table",      # Fast data processing for large GTFS files
  "tigris",          # Download Census TIGER/Line shapefiles
  "zip",             # ZIP file handling
  "httr",            # HTTP requests for GTFS downloads
  "lubridate",       # Date/time handling
  "mapview"          # Additional spatial visualization
)
install.packages(required_packages)
```

### System Requirements

- **R**: Version 4.0 or higher recommended
- **RStudio**: Optional, but recommended for rendering R Markdown
- **Internet connection**: For downloading GTFS data and geographic boundaries
- **Memory**: 8GB+ RAM recommended (processing 5 agencies of GTFS data)

---

## Usage

### Quick Start

1. **Clone the repository**:
   ```bash
   git clone https://github.com/MisterClean/illinois-people-over-parking-act.git
   cd illinois-people-over-parking-act
   ```

2. **Open in RStudio**:
   ```r
   # Open the main notebook
   rstudioapi::navigateToFile("sb2111-people-over-parking.Rmd")
   ```

3. **Run the analysis**:
   - Click "Knit" in RStudio, **OR**
   - Run: `rmarkdown::render("sb2111-people-over-parking.Rmd")`

### What Happens During Execution

The analysis will:

1. ✅ **Load R modules** - Sources all functions from `R/` directory
2. ✅ **Download GTFS data** - Downloads from 5 agencies (or uses `gtfs_cache/`)
3. ✅ **Validate data quality** - Runs comprehensive validation checks
4. ✅ **Normalize data** - Creates unique IDs across agencies (e.g., `cta_1234`)
5. ✅ **Identify rail stations** - Agency-specific logic for CTA/Metra/Metro STL
6. ✅ **Filter to weekday peak** - Extracts AM/PM peak period trips
7. ✅ **Cluster bus stops** - Groups stops within 150ft radius
8. ✅ **Verify route overlaps** - Checks street name matching at clusters
9. ✅ **Calculate frequencies** - Computes service intervals per location
10. ✅ **Apply SB2111 criteria** - Identifies qualifying hubs and corridors
11. ✅ **Create buffers** - 1/2 mile hubs, 1/8 mile corridors
12. ✅ **Generate map** - Interactive Leaflet visualization
13. ✅ **Produce output** - Self-contained HTML file

**Estimated runtime**: 5-15 minutes depending on system specs and network speed.

### Output

The notebook generates **`sb2111-people-over-parking.html`**, which contains:

- 🗺️ **Interactive map** with toggleable layers for each agency
- 📊 **Summary statistics** (total area, hubs, corridors by agency)
- 📋 **Data quality reports** (validation results)
- 📖 **Methodology documentation** (embedded in output)
- 📈 **Visualizations** (frequency distributions, cluster sizes, etc.)

---

## Using the R Modules Independently

All functions in `R/` are reusable for other transit analysis projects:

```r
# Load modules
source("R/gtfs_download.R")
source("R/gtfs_normalize.R")
source("R/gtfs_validate.R")
source("R/frequency_calc.R")

# Download GTFS data
cta_dir <- download_and_extract_gtfs(
  "cta",
  "https://www.transitchicago.com/downloads/sch_data/google_transit.zip"
)

# Normalize and validate
cta_data <- read_normalize_gtfs("cta", cta_dir)
validation <- generate_quality_report(cta_data, "cta")
print_validation_report(list(cta = validation))

# Calculate frequencies
am_stops <- filter_peak_period_stop_times(
  cta_data$stop_times,
  "07:00:00",
  "09:00:00"
)
am_metrics <- calculate_peak_frequency(
  am_stops,
  c("stop_id", "agency"),
  120,
  "am"
)
```

See [R/README.md](R/README.md) for comprehensive module documentation with examples.

---

## Methodology

### Data Flow

```
GTFS Data (5 agencies)
    ↓ download_and_extract_gtfs()
    ↓ read_normalize_gtfs()
    ↓ ✓ VALIDATION: validate_all_gtfs()
Combined GTFS tables
    ↓ identify_weekday_services()
    ↓ filter_peak_period_stop_times()
    ↓ Split AM/PM peaks
Transit Hubs (rail + bus)
    ↓ identify_all_rail_stations()
    ↓ cluster_stops_spatial() [150ft]
    ↓ verify_route_overlap_at_cluster()
    ↓ calculate_peak_frequency()
    ↓ apply_hub_qualification()
    ↓ ✓ VALIDATION: validate_geometries()
    ↓ Buffer 1/2 mile
Transit Corridors
    ↓ Snap to TIGER/Line streets
    ↓ Buffer 1/8 mile
    ↓ apply_corridor_qualification()
Combined Areas
    ↓ ✓ VALIDATION: validate_coordinate_transform()
    ↓ Clip to Illinois boundary
Interactive Leaflet Map (HTML)
```

### Data Sources

- **GTFS Feeds**: Downloaded from transit agencies (or cached in `gtfs_cache/`)
  - CTA: https://www.transitchicago.com/downloads/sch_data/
  - Pace: https://www.pacebus.com/
  - Metra: https://schedules.metrarail.com/
  - Metro STL: https://metrostlouis.org/
  - CUMTD: http://developer.cumtd.com/

- **Geographic Boundaries**: Illinois state boundaries via US Census TIGER/Line
- **Street Network**: Road centerlines via TIGER/Line for accurate distance calculations

### GTFS Normalization

All transit agencies receive unique identifiers to prevent ID collisions:
- `unique_stop_id`: e.g., `cta_1234`, `pace_5678`
- `unique_route_id`: e.g., `metra_UP-N`, `metro_stl_90`
- `unique_trip_id`: e.g., `cta_trip_1234`, `cumtd_trip_5678`

This allows combining data from multiple agencies safely.

See [docs/gtfs_normalization_strategy.md](docs/gtfs_normalization_strategy.md) for details.

---

## Data Quality & Validation

The analysis includes comprehensive automatic validation:

### GTFS Validation

Checks for:
- ✅ All required GTFS files present (stops, routes, trips, stop_times, calendar)
- ✅ Required fields exist in each table
- ✅ Coordinates within valid ranges (lat ±90°, lon ±180°)
- ✅ Coordinates within Illinois bounds (with warnings for border agencies)
- ✅ Referential integrity (trips reference valid routes, stop_times reference valid stops/trips)
- ✅ No orphaned data (routes with no trips, trips with no stop_times)

### Spatial Validation

Checks for:
- ✅ Valid geometries (detects and repairs self-intersections, invalid polygons)
- ✅ Accurate coordinate transformations (verifies CRS changes preserve accuracy)
- ✅ Correct buffer distances (validates 150ft, 2640ft, 660ft buffers)
- ✅ Illinois boundary compliance

Validation results are printed during execution and included in the output HTML.

---

## GTFS Cache

The `gtfs_cache/` directory contains cached GTFS data files to:
- **Speed up analysis** (no need to re-download every time)
- **Ensure reproducibility** (consistent data across runs)
- **Enable offline work** (run analysis without internet)

Cached files:
- `cta_gtfs.zip` - CTA (Chicago Transit Authority)
- `pace_gtfs.zip` - Pace Suburban Bus
- `metra_gtfs.zip` - Metra Commuter Rail
- `metro_stl_gtfs.zip` - Metro St. Louis
- `cumtd_gtfs.zip` - CUMTD (Champaign-Urbana)

If cache files are missing, the notebook automatically downloads fresh data from each agency.

---

## Testing

The project uses `testthat` for unit and integration testing:

```r
# Run all tests
devtools::test()

# Run specific test file
testthat::test_file("tests/testthat/test-gtfs-validate.R")
```

**Test coverage** (in development):
- GTFS validation functions
- Spatial clustering algorithms
- Frequency calculations
- Hub identification logic
- Integration tests for full pipeline

See [R/README.md](R/README.md) for testing conventions and examples.

---

## Code Quality

**Recent Enhancements** (December 2024 - January 2025):

1. ✅ **Modularization**: Extracted 2,500+ lines into 7 reusable R modules
2. ✅ **Validation**: Added comprehensive GTFS and spatial validation (11 functions)
3. ✅ **Documentation**: 100% roxygen2 coverage on all 28 extracted functions
4. ✅ **Testing Infrastructure**: Created testthat framework (tests in development)
5. ✅ **Error Handling**: Improved error messages and recovery strategies

**Benefits**:
- Code reusability across transit analysis projects
- Automated data quality validation catches errors before they corrupt analysis
- Easier debugging and maintenance
- Foundation for automated testing
- Algorithm rationale preserved in documentation

---

## Legislative Background

- **Bill**: Illinois Senate Bill 2111 (2024)
- **Common Name**: "People Over Parking Act"
- **Status**: Signed into law
- **Effective Date**: January 1, 2024
- **Purpose**: Reduce housing costs and promote transit-oriented development by eliminating parking minimums near high-quality transit
- **Scope**: Statewide (applies to all municipalities in Illinois)

This legislation is a stronger and more comprehensive version of earlier Chicago-only proposals (HB3256), extending parking minimum relief across the entire state.

**Policy Context**: Parking minimums increase housing costs, reduce walkability, and encourage car dependency. By eliminating these requirements near high-quality transit, SB2111 enables:
- More affordable housing development
- Transit-oriented development
- Reduced car dependency in transit-accessible areas
- More efficient land use near transit

---

## Additional Documentation

- **[R/README.md](R/README.md)** - Comprehensive module documentation and API reference
- **[CLAUDE.md](CLAUDE.md)** - AI assistant guidance and architecture overview
- [docs/gtfs_normalization_strategy.md](docs/gtfs_normalization_strategy.md) - GTFS processing methodology
- [docs/bus_transit_hub_verification_summary.md](docs/bus_transit_hub_verification_summary.md) - Bus hub verification approach

---

## Contributing

Contributions welcome! Please:
1. Fork the repository
2. Create a feature branch
3. Add tests for new functionality
4. Ensure all tests pass: `devtools::test()`
5. Submit a pull request

See [R/README.md](R/README.md) for coding conventions and module structure.

---

## License

This analysis is provided for informational and research purposes. Please verify all findings with official transit schedules and local ordinances before making planning decisions.

The code is available under the MIT License (see LICENSE file).

---

## Contact

- **GitHub Issues**: For bugs, questions, or feature requests
- **Repository**: https://github.com/MisterClean/illinois-people-over-parking-act

---

## Citation

If you use this analysis in research or policy work, please cite:

```
Illinois People Over Parking Act (SB2111) Transit Access Analysis
https://github.com/MisterClean/illinois-people-over-parking-act
```
