# Illinois People Over Parking Act (SB2111) - Transit Access Analysis

This repository contains an R Markdown analysis that identifies areas in Illinois that qualify for parking minimum relief under **Senate Bill 2111** (the "People Over Parking Act"), which was signed into law in 2024.

## What is SB2111?

Senate Bill 2111 prohibits municipalities in Illinois from enforcing parking minimums in two types of transit-accessible areas:

1. **Transit Hubs**: Within 1/2 mile of transit stations/stops where:
   - 3+ fixed-route transit lines intersect
   - Service operates at least every 15 minutes during peak periods (either AM or PM)

2. **Transit Corridors**: Within 1/8 mile of transit routes where:
   - Service operates at least every 15 minutes during peak periods (either AM or PM)

## Geographic Coverage

This analysis covers **all major transit agencies in Illinois**:

### Chicago Metropolitan Area (6 counties)
- **CTA** (Chicago Transit Authority) - Bus and Rail
- **Pace** - Suburban Bus
- **Metra** - Commuter Rail

### St. Louis Metropolitan Area, Illinois Side (3 counties)
- **Metro St. Louis** - Bus and MetroLink (light rail)

### Champaign-Urbana (1 county)
- **CUMTD** (Champaign-Urbana Mass Transit District) - Bus

**Important**: All transit buffers are clipped to Illinois state boundaries to comply with the state law's jurisdiction.

## Interactive Map

The analysis generates an interactive Leaflet map showing:
- Transit hubs (1/2 mile buffers)
- Transit corridors (1/8 mile buffers)
- Individual transit routes and stops
- Route overlap verification for bus hubs

## Methodology

### Key Features

1. **Split Peak Analysis**: Analyzes AM peak (6:00-9:00) and PM peak (16:00-19:00) separately, with "either" logic (qualifies if criteria met in either period)

2. **Bus Hub Clustering**:
   - Groups bus stops within 150 feet as a single "hub"
   - Verifies that qualifying routes actually intersect at the same street location
   - Prevents false positives from nearby parallel routes

3. **Direction-Aware Frequency**:
   - Calculates service frequency separately for each direction
   - Uses the better-performing direction for qualification

4. **Geographic Precision**:
   - All buffers clipped to Illinois state boundaries
   - Uses TIGER/Line street network data for accurate distances

### Data Sources

- **GTFS Data**: Downloaded directly from transit agencies (or from cached copies in `gtfs_cache/`)
- **Geographic Boundaries**: Illinois state boundaries via TIGER/Line (tigris R package)
- **Street Network**: Road centerlines via TIGER/Line for distance calculations

See [docs/gtfs_normalization_strategy.md](docs/gtfs_normalization_strategy.md) and [docs/bus_transit_hub_verification_summary.md](docs/bus_transit_hub_verification_summary.md) for detailed methodology documentation.

## Requirements

### R Packages

The notebook automatically installs required packages:
- `tidyverse` - Data manipulation and visualization
- `sf` - Spatial data handling
- `leaflet` / `leaflet.extras` - Interactive mapping
- `data.table` - Fast data processing
- `tigris` - Download Census TIGER/Line shapefiles
- `zip` / `httr` - File handling and HTTP requests
- `lubridate` - Date/time handling
- `mapview` - Additional spatial visualization

### System Requirements

- R (version 4.0 or higher recommended)
- RStudio (optional, but recommended for rendering R Markdown)
- Internet connection (for downloading GTFS data and geographic boundaries)

## Usage

### Quick Start

1. Clone this repository:
   ```bash
   git clone https://github.com/MisterClean/illinois-people-over-parking-act.git
   cd illinois-people-over-parking-act
   ```

2. Open `sb2111-people-over-parking.Rmd` in RStudio

3. Click "Knit" to generate the analysis (or run `rmarkdown::render("sb2111-people-over-parking.Rmd")`)

The analysis will:
- Download GTFS data from transit agencies (or use cached copies)
- Download Illinois boundaries and street networks
- Process all transit data
- Generate an interactive HTML map with all qualifying areas

### Output

The notebook generates `sb2111-people-over-parking.html`, which contains:
- Interactive map with toggleable layers for each agency
- Summary statistics
- Methodology documentation
- All visualizations

## GTFS Cache

The `gtfs_cache/` directory contains cached GTFS data files to speed up analysis and ensure reproducibility:

- `cta_gtfs.zip` - CTA (Chicago Transit Authority)
- `pace_gtfs.zip` - Pace Suburban Bus
- `metra_gtfs.zip` - Metra Commuter Rail
- `metro_stl_gtfs.zip` - Metro St. Louis
- `cumtd_gtfs.zip` - CUMTD (Champaign-Urbana)

If these files are missing, the notebook will automatically download fresh data from each agency.

## Project Structure

```
illinois-people-over-parking-act/
├── README.md                          # This file
├── sb2111-people-over-parking.Rmd     # Main analysis notebook
├── .gitignore                         # Git ignore patterns
├── docs/                              # Methodology documentation
│   ├── gtfs_normalization_strategy.md
│   └── bus_transit_hub_verification_summary.md
└── gtfs_cache/                        # Cached GTFS data
    ├── cta_gtfs.zip
    ├── pace_gtfs.zip
    ├── metra_gtfs.zip
    ├── metro_stl_gtfs.zip
    └── cumtd_gtfs.zip
```

## Legislative Background

- **Bill**: Illinois Senate Bill 2111 (2024)
- **Status**: Signed into law
- **Purpose**: Reduce housing costs and promote transit-oriented development by eliminating parking minimums near high-quality transit
- **Scope**: Statewide (all municipalities in Illinois)

This is a stronger and more comprehensive version of earlier Chicago-only legislation (HB3256), extending parking minimum relief across the entire state.

## License

This analysis is provided for informational and research purposes. Please verify all findings with official transit schedules and local ordinances before making planning decisions.

## Contact

For questions or issues, please open an issue on GitHub.
