#' Agency Metadata and Configuration
#'
#' Central repository for all transit agency metadata used throughout the
#' People Over Parking Act analysis. This module provides a single source of
#' truth for agency identifiers, display names, colors, GTFS feed URLs, and
#' service characteristics (rail vs. bus).
#'
#' Usage:
#'   - Call get_agency_metadata() to access the full metadata structure
#'   - Use helper functions for specific needs (colors, names, rail status, etc.)
#'   - All agency-specific code should reference this metadata instead of hard-coding
#'
#' Adding New Agencies:
#'   1. Add new entry to AGENCY_METADATA list in get_agency_metadata()
#'   2. Choose unique color (check existing colors for contrast)
#'   3. Specify has_rail and rail_type if applicable
#'   4. Add geographic_filter if needed (e.g., latitude limits, state boundaries)
#'   5. All downstream code will automatically incorporate the new agency

#' Get Complete Agency Metadata
#'
#' Returns comprehensive metadata for all transit agencies included in the analysis.
#' This is the single source of truth for agency configuration.
#'
#' @return Named list where each element is an agency with fields:
#'   \describe{
#'     \item{id}{Internal identifier (lowercase with underscores)}
#'     \item{name}{Display name for maps and summaries}
#'     \item{full_name}{Full official agency name}
#'     \item{url}{GTFS feed URL for download}
#'     \item{color}{Hex color code for mapping}
#'     \item{has_rail}{Logical indicating rail service (TRUE) or bus-only (FALSE)}
#'     \item{rail_type}{Type of rail service if has_rail=TRUE ("cta_l", "commuter_rail", "light_rail")}
#'     \item{geographic_filter}{List specifying geographic filtering (NULL if none needed)}
#'   }
#'
#' @examples
#' metadata <- get_agency_metadata()
#' cta_info <- metadata$cta
#' cta_info$name  # "CTA"
#' cta_info$color # "#009CDE"
#'
#' @export
get_agency_metadata <- function() {
  list(
    # ===== EXISTING AGENCIES (6) =====

    cta = list(
      id = "cta",
      name = "CTA",
      full_name = "Chicago Transit Authority",
      url = "https://www.transitchicago.com/downloads/sch_data/google_transit.zip",
      color = "#009CDE",  # Blue
      has_rail = TRUE,
      rail_type = "cta_l",
      geographic_filter = NULL
    ),

    pace = list(
      id = "pace",
      name = "Pace",
      full_name = "Pace Suburban Bus",
      url = "https://www.pacebus.com/gtfsdownload",
      color = "#814C9E",  # Purple
      has_rail = FALSE,
      rail_type = NULL,
      geographic_filter = NULL
    ),

    metra = list(
      id = "metra",
      name = "Metra",
      full_name = "Metra Commuter Rail",
      url = "https://schedules.metrarail.com/gtfs/schedule.zip",
      color = "#E31837",  # Red
      has_rail = TRUE,
      rail_type = "commuter_rail",
      geographic_filter = list(type = "latitude", max = 42.5)  # Exclude Wisconsin
    ),

    metro_stl = list(
      id = "metro_stl",
      name = "Metro STL",
      full_name = "Metro St. Louis (MetroLink)",
      url = "https://metrostlouis.org/Transit/google_transit.zip",
      color = "#00A651",  # Green
      has_rail = TRUE,
      rail_type = "light_rail",
      geographic_filter = list(type = "state_boundary", state = "IL")  # IL portion only
    ),

    cumtd = list(
      id = "cumtd",
      name = "MTD",
      full_name = "Champaign-Urbana Mass Transit District",
      url = "http://developer.cumtd.com/gtfs/google_transit.zip",
      color = "#FF6600",  # Orange
      has_rail = FALSE,
      rail_type = NULL,
      geographic_filter = NULL
    ),

    rmtd = list(
      id = "rmtd",
      name = "RMTD",
      full_name = "Rockford Mass Transit District",
      url = "https://rmtd.org/rmtdgtfs/GTFS_FILES.zip",
      color = "#FFD700",  # Gold
      has_rail = FALSE,
      rail_type = NULL,
      geographic_filter = NULL
    ),

    # ===== NEW AGENCIES (8) =====

    metrolink_quad_cities = list(
      id = "metrolink_quad_cities",
      name = "MetroLINK",
      full_name = "Rock Island County Metropolitan Mass Transit District (MetroLINK)",
      url = "https://www.metroqc.com/documentcenter/view/404",
      color = "#4169E1",  # Royal Blue
      has_rail = FALSE,
      rail_type = NULL,
      geographic_filter = NULL
    ),

    citylink = list(
      id = "citylink",
      name = "CityLink",
      full_name = "Greater Peoria Mass Transit District (CityLink)",
      url = "https://clk.rideralerts.com/InfoPoint/gtfs-zip.ashx",
      color = "#DC143C",  # Crimson
      has_rail = FALSE,
      rail_type = NULL,
      geographic_filter = NULL
    ),

    smtd = list(
      id = "smtd",
      name = "SMTD",
      full_name = "Sangamon Mass Transit District",
      url = "http://data.smtd.org/gtfs/smtd_gtfs_feed.zip",
      color = "#228B22",  # Forest Green
      has_rail = FALSE,
      rail_type = NULL,
      geographic_filter = NULL
    ),

    dekalb = list(
      id = "dekalb",
      name = "DeKalb Transit",
      full_name = "DeKalb Public Transit",
      url = "https://data.trilliumtransit.com/gtfs/cityofdekalb-il-us/cityofdekalb-il-us.zip",
      color = "#FF4500",  # Orange Red
      has_rail = FALSE,
      rail_type = NULL,
      geographic_filter = NULL
    ),

    connect_transit = list(
      id = "connect_transit",
      name = "Connect Transit",
      full_name = "Bloomington-Normal Connect Transit",
      url = "https://rideconnecttransit.com/gtfs",
      color = "#9370DB",  # Medium Purple
      has_rail = FALSE,
      rail_type = NULL,
      geographic_filter = NULL
    ),

    dpts = list(
      id = "dpts",
      name = "DPTS",
      full_name = "Decatur Public Transit System",
      url = "https://gtfs.remix.com/dpts_decatur_il_us.zip",
      color = "#20B2AA",  # Light Sea Green
      has_rail = FALSE,
      rail_type = NULL,
      geographic_filter = NULL
    ),

    galesburg = list(
      id = "galesburg",
      name = "Galesburg",
      full_name = "Galesburg Transit",
      url = "https://gis.ci.galesburg.il.us/cityofgalesburg-il-us.zip",
      color = "#CD853F",  # Peru (tan/brown)
      has_rail = FALSE,
      rail_type = NULL,
      geographic_filter = NULL
    ),

    gowest = list(
      id = "gowest",
      name = "Go West",
      full_name = "Macomb McDonough County Public Transportation (Go West Transit)",
      url = "https://api.transloc.com/gtfs/wiu.zip",
      color = "#FF1493",  # Deep Pink
      has_rail = FALSE,
      rail_type = NULL,
      geographic_filter = NULL
    )
  )
}

#' Get All Agency IDs
#'
#' Returns vector of all agency identifiers in the order defined in metadata.
#'
#' @return Character vector of agency IDs (e.g., c("cta", "pace", "metra", ...))
#'
#' @examples
#' all_ids <- get_all_agency_ids()
#' for (id in all_ids) {
#'   # Process each agency
#' }
#'
#' @export
get_all_agency_ids <- function() {
  names(get_agency_metadata())
}

#' Get Agency Display Name
#'
#' Maps internal agency ID to human-readable display name.
#'
#' @param agency_id Character. Internal agency identifier (e.g., "cta", "pace")
#' @return Character. Display name (e.g., "CTA", "Pace")
#'
#' @examples
#' get_agency_display_name("cta")        # "CTA"
#' get_agency_display_name("metro_stl")  # "Metro STL"
#'
#' @export
get_agency_display_name <- function(agency_id) {
  metadata <- get_agency_metadata()
  if (agency_id %in% names(metadata)) {
    return(metadata[[agency_id]]$name)
  } else {
    warning(sprintf("Unknown agency ID: %s", agency_id))
    return(agency_id)  # Fallback to ID if not found
  }
}

#' Get Agency Color
#'
#' Returns hex color code for a given agency.
#'
#' @param agency_id Character. Internal agency identifier
#' @return Character. Hex color code (e.g., "#009CDE")
#'
#' @examples
#' get_agency_color("cta")   # "#009CDE" (blue)
#' get_agency_color("pace")  # "#814C9E" (purple)
#'
#' @export
get_agency_color <- function(agency_id) {
  metadata <- get_agency_metadata()
  if (agency_id %in% names(metadata)) {
    return(metadata[[agency_id]]$color)
  } else {
    warning(sprintf("Unknown agency ID: %s", agency_id))
    return("#808080")  # Gray fallback
  }
}

#' Get Rail Agency IDs
#'
#' Returns vector of agency IDs that provide rail service (CTA L, Metra, Metro STL).
#'
#' @return Character vector of rail agency IDs
#'
#' @examples
#' rail_agencies <- get_rail_agencies()  # c("cta", "metra", "metro_stl")
#'
#' @export
get_rail_agencies <- function() {
  metadata <- get_agency_metadata()
  names(metadata)[sapply(metadata, function(x) isTRUE(x$has_rail))]
}

#' Get Agency Color Palette for Leaflet
#'
#' Returns named vector of colors suitable for Leaflet mapping. Names are
#' display names (not IDs) to match Leaflet layer group names.
#'
#' @return Named character vector where names are display names and values are hex colors
#'
#' @examples
#' palette <- get_agency_color_palette()
#' palette["CTA"]   # "#009CDE"
#' palette["Pace"]  # "#814C9E"
#'
#' @export
get_agency_color_palette <- function() {
  metadata <- get_agency_metadata()
  colors <- sapply(metadata, function(x) x$color)
  names(colors) <- sapply(metadata, function(x) x$name)
  return(colors)
}

#' Get Agency Configuration for GTFS Download
#'
#' Returns list of agency configurations formatted for process_all_gtfs_data().
#' Each entry has 'name' (agency ID) and 'url' (GTFS feed URL).
#'
#' @return List of lists, each with name and url fields
#'
#' @examples
#' configs <- get_agency_configs_for_download()
#' # Returns: list(list(name="cta", url="https://..."), ...)
#'
#' @export
get_agency_configs_for_download <- function() {
  metadata <- get_agency_metadata()
  lapply(metadata, function(x) {
    list(name = x$id, url = x$url)
  })
}

#' Get Agency Full Name
#'
#' Returns official full name of agency.
#'
#' @param agency_id Character. Internal agency identifier
#' @return Character. Full official agency name
#'
#' @examples
#' get_agency_full_name("cumtd")  # "Champaign-Urbana Mass Transit District"
#'
#' @export
get_agency_full_name <- function(agency_id) {
  metadata <- get_agency_metadata()
  if (agency_id %in% names(metadata)) {
    return(metadata[[agency_id]]$full_name)
  } else {
    warning(sprintf("Unknown agency ID: %s", agency_id))
    return(agency_id)
  }
}

#' Check if Agency Has Rail Service
#'
#' Returns TRUE if agency provides rail service, FALSE if bus-only.
#'
#' @param agency_id Character. Internal agency identifier
#' @return Logical. TRUE if rail service exists, FALSE otherwise
#'
#' @examples
#' has_rail("cta")    # TRUE
#' has_rail("pace")   # FALSE
#'
#' @export
has_rail <- function(agency_id) {
  metadata <- get_agency_metadata()
  if (agency_id %in% names(metadata)) {
    return(isTRUE(metadata[[agency_id]]$has_rail))
  } else {
    return(FALSE)
  }
}

#' Get Agency Count
#'
#' Returns total number of agencies in the analysis.
#'
#' @return Integer. Total agency count
#'
#' @examples
#' get_agency_count()  # 14
#'
#' @export
get_agency_count <- function() {
  length(get_agency_metadata())
}
