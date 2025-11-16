#' tidytransit Integration Module
#'
#' Provides wrapper functions for processing GTFS data using the tidytransit package
#' while maintaining compatibility with the project's multi-agency normalization
#' strategy (unique ID prefixes, combined data tables).
#'
#' This module replaces the custom GTFS download, reading, and normalization logic
#' with tidytransit's robust, maintained functions while preserving the project's
#' sophisticated analysis logic for spatial clustering, frequency calculations,
#' and hub identification.
#'
#' Key functions:
#'   - process_agencies_with_tidytransit(): Downloads and processes all agencies
#'   - add_agency_identifiers(): Adds unique ID prefixes to prevent collisions
#'   - combine_agency_data(): Merges multi-agency data into unified tables
#'   - convert_to_data_table(): Converts tibbles to data.table for performance

#' Process All Transit Agencies Using tidytransit
#'
#' Downloads and processes GTFS data for all configured transit agencies using
#' the tidytransit package. Handles download, validation, spatial conversion,
#' and agency-specific identifier creation.
#'
#' @param agency_configs Named list of agency configurations from \code{\link{get_agency_metadata}}
#' @param validate Logical. If TRUE (default), runs tidytransit validation and
#'   prints validation reports. Useful for data quality checks.
#'
#' @return Named list where each element corresponds to an agency and contains:
#'   \describe{
#'     \item{data}{tidygtfs object with all GTFS tables (stops, routes, trips, etc.)}
#'     \item{validation}{Validation results from tidytransit (if validate=TRUE)}
#'     \item{config}{Original agency configuration metadata}
#'     \item{success}{Logical indicating if processing succeeded}
#'   }
#'
#' @details
#' For each agency, this function:
#' \enumerate{
#'   \item Downloads GTFS feed using \code{tidytransit::read_gtfs()}
#'   \item Validates GTFS structure (if validate=TRUE)
#'   \item Converts stops to sf POINT geometries and shapes to LINESTRING
#'   \item Adds unique identifiers (agency_id field and unique_*_id columns)
#'   \item Converts tibbles to data.table for performance
#' }
#'
#' Failed downloads are logged with error messages but do not halt processing
#' of other agencies. Check the \code{success} field in returned list.
#'
#' @examples
#' \dontrun{
#' # Process all agencies
#' agency_configs <- get_agency_metadata()
#' agencies <- process_agencies_with_tidytransit(agency_configs, validate = TRUE)
#'
#' # Access individual agency data
#' cta_gtfs <- agencies$cta$data
#' cta_stops <- cta_gtfs$stops
#'
#' # Check which agencies succeeded
#' sapply(agencies, function(a) a$success)
#' }
#'
#' @export
process_agencies_with_tidytransit <- function(agency_configs, validate = TRUE) {
  agencies <- list()

  cat("Processing", length(agency_configs), "transit agencies with tidytransit...\n\n")

  for (agency_id in names(agency_configs)) {
    config <- agency_configs[[agency_id]]

    cat(sprintf("Processing %s (%s)...\n", config$name, agency_id))

    tryCatch({
      # Download and read GTFS using tidytransit
      # Set quiet=FALSE to see download progress
      gtfs <- tidytransit::read_gtfs(config$url, quiet = TRUE)

      # Store validation results if requested
      validation <- NULL
      if (validate) {
        validation <- attr(gtfs, "validation_result")
        if (!is.null(validation) && length(validation) > 0) {
          cat(sprintf("  Validation: %d issues found\n", length(validation)))
        }
      }

      # Convert to spatial objects (stops as POINT, shapes as LINESTRING)
      gtfs <- tidytransit::gtfs_as_sf(gtfs, skip_shapes = FALSE)

      # Convert shapes from points to LINESTRING geometries
      # tidytransit's gtfs_as_sf creates sf points, but we need LINESTRING per shape_id
      if (!is.null(gtfs$shapes) && nrow(gtfs$shapes) > 0) {
        gtfs$shapes <- convert_shapes_points_to_linestrings(gtfs$shapes)
      }

      # Add agency identifiers (unique IDs to prevent collisions)
      gtfs <- add_agency_identifiers(gtfs, agency_id)

      # Convert to data.table for performance-critical operations
      gtfs <- convert_gtfs_to_data_table(gtfs)

      # Store successfully processed agency
      agencies[[agency_id]] <- list(
        data = gtfs,
        validation = validation,
        config = config,
        success = TRUE
      )

      cat(sprintf("  ✓ %s processed successfully\n", config$name))

    }, error = function(e) {
      cat(sprintf("  ✗ Error processing %s: %s\n", config$name, e$message))

      # Store failed agency with error information
      agencies[[agency_id]] <- list(
        data = NULL,
        validation = NULL,
        config = config,
        success = FALSE,
        error = e$message
      )
    })

    cat("\n")
  }

  # Summary
  success_count <- sum(sapply(agencies, function(a) a$success))
  cat(sprintf("Summary: %d/%d agencies processed successfully\n",
              success_count, length(agency_configs)))

  return(agencies)
}

#' Add Agency Identifiers to GTFS Data
#'
#' Adds agency-specific identifiers to all GTFS tables to prevent ID collisions
#' when combining multiple transit agencies. This maintains compatibility with
#' the project's multi-agency normalization strategy.
#'
#' @param gtfs tidygtfs object from \code{tidytransit::read_gtfs()}
#' @param agency_id Character. Internal agency identifier (e.g., "cta", "pace")
#'
#' @return Modified tidygtfs object with added fields:
#'   \itemize{
#'     \item \code{agency} field added to all tables
#'     \item \code{unique_stop_id} = "agency_id_stop_id"
#'     \item \code{unique_route_id} = "agency_id_route_id"
#'     \item \code{unique_trip_id} = "agency_id_trip_id"
#'     \item \code{unique_shape_id} = "agency_id_shape_id" (if shapes exist)
#'   }
#'
#' @details
#' This function replicates the normalization strategy from the original
#' \code{read_normalize_gtfs()} function, ensuring that agency identifiers
#' are unique across the combined dataset.
#'
#' Example: CTA stop "1234" becomes "cta_1234", Pace stop "1234" becomes "pace_1234"
#'
#' @examples
#' \dontrun{
#' gtfs <- tidytransit::read_gtfs("https://example.com/gtfs.zip")
#' gtfs <- add_agency_identifiers(gtfs, "cta")
#' head(gtfs$stops$unique_stop_id)  # "cta_1234", "cta_1235", ...
#' }
#'
#' @export
add_agency_identifiers <- function(gtfs, agency_id) {

  # Add agency field and unique_stop_id to stops
  if (!is.null(gtfs$stops)) {
    gtfs$stops$agency <- agency_id
    gtfs$stops$stop_id <- as.character(gtfs$stops$stop_id)
    gtfs$stops$unique_stop_id <- paste0(agency_id, "_", gtfs$stops$stop_id)

    # Ensure stop_lat and stop_lon columns exist (tidytransit may convert to geometry)
    # If they're missing, extract from geometry column
    if (!"stop_lat" %in% names(gtfs$stops) && inherits(gtfs$stops, "sf")) {
      coords <- sf::st_coordinates(gtfs$stops)
      gtfs$stops$stop_lon <- coords[, "X"]
      gtfs$stops$stop_lat <- coords[, "Y"]
    }

    # Handle optional fields (parent_station, location_type)
    if (!"location_type" %in% names(gtfs$stops)) {
      gtfs$stops$location_type <- NA_integer_
    }
    if (!"parent_station" %in% names(gtfs$stops)) {
      gtfs$stops$parent_station <- NA_character_
    } else {
      # Also prefix parent_station references
      gtfs$stops$parent_station <- ifelse(
        is.na(gtfs$stops$parent_station) | gtfs$stops$parent_station == "",
        NA_character_,
        paste0(agency_id, "_", gtfs$stops$parent_station)
      )
    }
  }

  # Add agency field and unique_route_id to routes
  if (!is.null(gtfs$routes)) {
    gtfs$routes$agency <- agency_id
    gtfs$routes$route_id <- as.character(gtfs$routes$route_id)
    gtfs$routes$unique_route_id <- paste0(agency_id, "_", gtfs$routes$route_id)
  }

  # Add agency field and unique_trip_id, unique_route_id, unique_shape_id to trips
  if (!is.null(gtfs$trips)) {
    gtfs$trips$agency <- agency_id
    gtfs$trips$trip_id <- as.character(gtfs$trips$trip_id)
    gtfs$trips$route_id <- as.character(gtfs$trips$route_id)
    gtfs$trips$unique_trip_id <- paste0(agency_id, "_", gtfs$trips$trip_id)
    gtfs$trips$unique_route_id <- paste0(agency_id, "_", gtfs$trips$route_id)

    # Handle optional direction_id
    if (!"direction_id" %in% names(gtfs$trips)) {
      gtfs$trips$direction_id <- NA_integer_
    }

    # Handle optional shape_id
    if ("shape_id" %in% names(gtfs$trips)) {
      gtfs$trips$shape_id <- as.character(gtfs$trips$shape_id)
      gtfs$trips$unique_shape_id <- ifelse(
        is.na(gtfs$trips$shape_id) | gtfs$trips$shape_id == "",
        NA_character_,
        paste0(agency_id, "_", gtfs$trips$shape_id)
      )
    } else {
      gtfs$trips$shape_id <- NA_character_
      gtfs$trips$unique_shape_id <- NA_character_
    }
  }

  # Add unique identifiers to stop_times
  if (!is.null(gtfs$stop_times)) {
    gtfs$stop_times$agency <- agency_id
    gtfs$stop_times$trip_id <- as.character(gtfs$stop_times$trip_id)
    gtfs$stop_times$stop_id <- as.character(gtfs$stop_times$stop_id)
    gtfs$stop_times$unique_trip_id <- paste0(agency_id, "_", gtfs$stop_times$trip_id)
    gtfs$stop_times$unique_stop_id <- paste0(agency_id, "_", gtfs$stop_times$stop_id)
  }

  # Add agency field to calendar
  if (!is.null(gtfs$calendar)) {
    gtfs$calendar$agency <- agency_id
  }

  # Add agency field to calendar_dates
  if (!is.null(gtfs$calendar_dates)) {
    gtfs$calendar_dates$agency <- agency_id
  }

  # Add unique_shape_id to shapes (if present)
  if (!is.null(gtfs$shapes)) {
    gtfs$shapes$agency <- agency_id
    gtfs$shapes$shape_id <- as.character(gtfs$shapes$shape_id)
    gtfs$shapes$unique_shape_id <- paste0(agency_id, "_", gtfs$shapes$shape_id)
  }

  return(gtfs)
}

#' Convert tidytransit GTFS Object to data.table Format
#'
#' Converts all tables in a tidygtfs object from tibbles to data.table format
#' for improved performance in data-intensive operations (joins, aggregations).
#'
#' @param gtfs tidygtfs object with tibble-formatted tables
#'
#' @return tidygtfs object with data.table-formatted tables
#'
#' @details
#' The original codebase uses data.table extensively for performance-critical
#' operations (frequency calculations, large joins in stop_times). This function
#' maintains performance parity by converting tidytransit's tibbles to data.table.
#'
#' Conversion is done in-place using \code{data.table::setDT()}, which is
#' memory-efficient and fast.
#'
#' @examples
#' \dontrun{
#' gtfs <- tidytransit::read_gtfs("https://example.com/gtfs.zip")
#' gtfs <- convert_gtfs_to_data_table(gtfs)
#' class(gtfs$stops)  # c("data.table", "data.frame")
#' }
#'
#' @export
convert_gtfs_to_data_table <- function(gtfs) {

  # List of standard GTFS tables to convert
  table_names <- c("stops", "routes", "trips", "stop_times",
                   "calendar", "calendar_dates", "shapes", "frequencies",
                   "agency", "transfers", "pathways", "levels",
                   "feed_info", "attributions", "translations")

  for (table_name in table_names) {
    if (!is.null(gtfs[[table_name]]) && nrow(gtfs[[table_name]]) > 0) {
      # Convert sf objects need special handling (keep geometry column)
      if (inherits(gtfs[[table_name]], "sf")) {
        # For sf objects, convert the data portion but keep sf class
        # data.table and sf can coexist
        data.table::setDT(gtfs[[table_name]])
      } else {
        # Regular tibble/data.frame conversion
        data.table::setDT(gtfs[[table_name]])
      }

      # Handle integer64 conversion (common in GTFS files)
      gtfs[[table_name]] <- convert_integer64_columns(gtfs[[table_name]])
    }
  }

  return(gtfs)
}

#' Convert Shapes from POINT to LINESTRING Geometries
#'
#' Converts GTFS shapes.txt from individual sf POINT geometries to LINESTRING
#' geometries grouped by shape_id. This is needed because tidytransit's gtfs_as_sf()
#' creates point geometries, but corridor analysis needs LINESTRING per shape.
#'
#' @param shapes_sf sf object with POINT geometries (one per shape point)
#' @return sf object with LINESTRING geometries (one per shape_id)
#'
#' @details
#' Groups shape points by shape_id and shape_pt_sequence, then creates a
#' LINESTRING geometry for each unique shape_id.
#'
#' @keywords internal
convert_shapes_points_to_linestrings <- function(shapes_sf) {

  # If already LINESTRING, return as-is
  geom_types <- unique(as.character(sf::st_geometry_type(shapes_sf)))
  if ("LINESTRING" %in% geom_types || "MULTILINESTRING" %in% geom_types) {
    return(shapes_sf)
  }

  # Convert to data.table for processing
  shapes_dt <- as.data.table(shapes_sf)

  # Extract coordinates from geometry column
  coords <- sf::st_coordinates(shapes_sf)
  shapes_dt$shape_pt_lon <- coords[, "X"]
  shapes_dt$shape_pt_lat <- coords[, "Y"]

  # Order by shape_id and sequence
  data.table::setorder(shapes_dt, shape_id, shape_pt_sequence)

  # Split by shape_id
  shapes_list <- split(shapes_dt, by = "shape_id")

  # Convert each shape to LINESTRING
  linestrings_list <- lapply(names(shapes_list), function(sid) {
    shape_points <- shapes_list[[sid]]

    # Need at least 2 points for a valid LINESTRING
    if (nrow(shape_points) < 2) {
      return(NULL)
    }

    # Extract coordinates as matrix (lon, lat order for sf)
    coords_matrix <- as.matrix(shape_points[, .(shape_pt_lon, shape_pt_lat)])

    # Create LINESTRING geometry
    linestring <- sf::st_linestring(coords_matrix)

    # Return as data.frame with shape_id
    data.frame(
      shape_id = sid,
      num_points = nrow(shape_points),
      geometry = sf::st_sfc(linestring, crs = 4326)
    )
  })

  # Remove NULL entries (shapes with < 2 points)
  linestrings_list <- linestrings_list[!sapply(linestrings_list, is.null)]

  # Combine into single sf object
  if (length(linestrings_list) > 0) {
    shapes_linestring_sf <- do.call(rbind, linestrings_list)
    shapes_linestring_sf <- sf::st_as_sf(shapes_linestring_sf)
    return(shapes_linestring_sf)
  } else {
    # Return empty sf object if no valid shapes
    empty_sf <- sf::st_sf(
      shape_id = character(),
      num_points = integer(),
      geometry = sf::st_sfc(crs = 4326)
    )
    return(empty_sf)
  }
}

#' Convert integer64 Columns to Appropriate Types
#'
#' Converts integer64 columns in a data.table to regular integer or character
#' types. Large values that exceed integer.max are converted to character,
#' while smaller values are converted to regular integers.
#'
#' @param dt A data.table with potential integer64 columns
#' @return The modified data.table with integer64 columns converted
#'
#' @details
#' GTFS files often have integer64 columns for fields like stop_sequence,
#' start_date, and end_date. This function converts them to more manageable
#' types to avoid downstream compatibility issues with data.table operations.
#'
#' @examples
#' \dontrun{
#' dt <- convert_integer64_columns(my_gtfs_table)
#' }
#'
#' @keywords internal
convert_integer64_columns <- function(dt) {
  for (col in names(dt)) {
    # Skip geometry columns
    if (col == "geometry") next

    if (class(dt[[col]])[1] == "integer64") {
      # Convert to character for large integers (like dates), regular integer for small ones
      max_val <- suppressWarnings(max(dt[[col]], na.rm = TRUE))
      if (!is.na(max_val) && max_val > .Machine$integer.max) {
        dt[, (col) := as.character(get(col))]
      } else {
        dt[, (col) := as.integer(get(col))]
      }
    }
  }
  return(dt)
}

#' Combine GTFS Data from Multiple Agencies
#'
#' Merges GTFS data from multiple agencies into unified tables for cross-agency
#' analysis. Handles missing tables gracefully and preserves spatial geometries.
#'
#' @param agencies_data Output from \code{\link{process_agencies_with_tidytransit}}
#'
#' @return Named list of combined data tables:
#'   \describe{
#'     \item{stops}{All stops from all agencies (sf POINT geometries)}
#'     \item{routes}{All routes from all agencies}
#'     \item{trips}{All trips from all agencies}
#'     \item{stop_times}{All stop times from all agencies}
#'     \item{calendar}{All calendar entries from all agencies}
#'     \item{calendar_dates}{All calendar date exceptions from all agencies}
#'     \item{shapes}{All shape geometries from all agencies (sf LINESTRING)}
#'   }
#'
#' @details
#' This function:
#' \itemize{
#'   \item Combines tables using \code{rbindlist()} for performance
#'   \item Handles missing tables (some agencies may not have calendar.txt)
#'   \item Preserves sf geometries for stops and shapes
#'   \item Maintains unique identifiers across agencies (via unique_*_id fields)
#' }
#'
#' Only successfully processed agencies (success=TRUE) are included in the
#' combined output.
#'
#' @examples
#' \dontrun{
#' agencies_data <- process_agencies_with_tidytransit(get_agency_metadata())
#' combined <- combine_agency_data(agencies_data)
#' nrow(combined$stops)  # Total stops across all agencies
#' }
#'
#' @export
combine_agency_data <- function(agencies_data) {

  cat("Combining data from", length(agencies_data), "agencies...\n")

  # Filter to only successful agencies
  successful_agencies <- agencies_data[sapply(agencies_data, function(a) a$success)]

  if (length(successful_agencies) == 0) {
    stop("No agencies were successfully processed. Cannot combine data.")
  }

  cat(sprintf("Combining data from %d successful agencies\n", length(successful_agencies)))

  # Helper function to combine a specific table across all agencies
  combine_table <- function(table_name, preserve_sf = FALSE) {
    tables_list <- lapply(successful_agencies, function(agency) {
      table <- agency$data[[table_name]]
      if (!is.null(table) && nrow(table) > 0) {
        return(table)
      } else {
        return(NULL)
      }
    })

    # Remove NULL entries
    tables_list <- tables_list[!sapply(tables_list, is.null)]

    if (length(tables_list) == 0) {
      cat(sprintf("  Warning: No data found for %s\n", table_name))
      return(NULL)
    }

    # Combine tables
    if (preserve_sf && inherits(tables_list[[1]], "sf")) {
      # Use sf::st_as_sf and dplyr::bind_rows for sf objects
      combined <- do.call(rbind, tables_list)
    } else {
      # Use data.table::rbindlist for regular tables (fill=TRUE handles missing columns)
      combined <- data.table::rbindlist(tables_list, fill = TRUE)
    }

    cat(sprintf("  Combined %s: %s rows\n", table_name, format(nrow(combined), big.mark = ",")))
    return(combined)
  }

  # Combine each table
  combined_data <- list(
    stops = combine_table("stops", preserve_sf = TRUE),
    routes = combine_table("routes"),
    trips = combine_table("trips"),
    stop_times = combine_table("stop_times"),
    calendar = combine_table("calendar"),
    calendar_dates = combine_table("calendar_dates"),
    shapes = combine_table("shapes", preserve_sf = TRUE)
  )

  cat("\nData combination complete!\n")

  return(combined_data)
}

#' Get Configurations for GTFS Download
#'
#' Wrapper function that retrieves agency metadata configured for GTFS download.
#' This maintains backward compatibility with the original \code{get_agency_configs_for_download()}
#' function name.
#'
#' @return Named list of agency configurations from \code{\link{get_agency_metadata}}
#'
#' @examples
#' \dontrun{
#' configs <- get_agency_configs_for_download()
#' agencies <- process_agencies_with_tidytransit(configs)
#' }
#'
#' @export
get_agency_configs_for_download <- function() {
  return(get_agency_metadata())
}
