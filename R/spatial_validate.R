#' Validate Geometries
#'
#' Checks that sf geometries are valid and attempts to repair invalid ones.
#'
#' @param sf_object sf object to validate
#' @param name Character. Name of the geometry (for error messages)
#' @param repair Logical. If TRUE, attempts to repair invalid geometries using
#'   sf::st_make_valid(). Default: TRUE
#'
#' @return sf object with validated (and possibly repaired) geometries
#'
#' @details
#' Invalid geometries can cause failures in spatial operations. Common issues:
#' \itemize{
#'   \item Self-intersecting polygons
#'   \item Duplicate vertices
#'   \item Ring orientation problems
#'   \item Topology errors
#' }
#'
#' If repair=TRUE and invalid geometries are found:
#' \enumerate{
#'   \item Warns about number of invalid geometries
#'   \item Attempts repair with st_make_valid()
#'   \item Returns repaired geometries
#' }
#'
#' If repair=FALSE and invalid geometries are found:
#' \enumerate{
#'   \item Throws error with details about invalid features
#' }
#'
#' @examples
#' \dontrun{
#' # Validate and repair buffer geometries
#' hub_buffers <- st_buffer(stops_sf, dist = 2640)
#' hub_buffers <- validate_geometries(hub_buffers, "hub_buffers")
#'
#' # Validate without repair (strict mode)
#' corridor_geom <- validate_geometries(
#'   corridors_sf,
#'   "corridors",
#'   repair = FALSE
#' )
#' }
#'
#' @export
validate_geometries <- function(sf_object, name, repair = TRUE) {
  if (!inherits(sf_object, "sf")) {
    stop(paste0(name, " is not an sf object"))
  }

  if (nrow(sf_object) == 0) {
    message(paste0(name, ": empty sf object, skipping validation"))
    return(sf_object)
  }

  invalid <- !sf::st_is_valid(sf_object)
  n_invalid <- sum(invalid)

  if (n_invalid > 0) {
    if (repair) {
      warning(paste0(
        name, ": ", n_invalid, " invalid geometries detected (",
        round(100 * n_invalid / nrow(sf_object), 1),
        "%) - attempting repair with st_make_valid()"
      ))

      # Get validity reasons for reporting
      validity_reasons <- sf::st_is_valid(sf_object, reason = TRUE)
      unique_reasons <- unique(validity_reasons[invalid])

      message(paste0("  Reasons for invalidity: ", paste(unique_reasons, collapse = "; ")))

      # Attempt repair
      sf_object[invalid, ] <- sf::st_make_valid(sf_object[invalid, ])

      # Check if repair succeeded
      still_invalid <- !sf::st_is_valid(sf_object)
      if (any(still_invalid)) {
        warning(paste0(
          name, ": ", sum(still_invalid),
          " geometries still invalid after repair attempt"
        ))
      } else {
        message(paste0("  Successfully repaired all ", n_invalid, " invalid geometries"))
      }

    } else {
      # Strict mode - don't repair, just error
      validity_reasons <- sf::st_is_valid(sf_object, reason = TRUE)
      invalid_reasons <- unique(validity_reasons[invalid])
      stop(paste0(
        name, ": ", n_invalid, " invalid geometries found. ",
        "Reasons: ", paste(invalid_reasons, collapse = "; ")
      ))
    }
  }

  sf_object
}

#' Validate Coordinate Transform
#'
#' Verifies that coordinate system transformation preserves geometry validity
#' and doesn't introduce unexpected distortions.
#'
#' @param sf_object sf object to transform
#' @param target_crs CRS to transform to (can be EPSG code or proj4string)
#' @param name Character. Name for error messages
#' @param check_area Logical. If TRUE, warns if transformation changes total
#'   area by more than 1%. Default: TRUE
#'
#' @return Transformed sf object
#'
#' @details
#' Coordinate transformations can introduce subtle errors:
#' \itemize{
#'   \item Datum shifts may move geometries slightly
#'   \item Projection distortion varies by location
#'   \item Invalid geometries may become valid or vice versa
#' }
#'
#' This function:
#' \enumerate{
#'   \item Validates geometries before transformation
#'   \item Performs transformation
#'   \item Validates geometries after transformation
#'   \item Optionally checks that area hasn't changed dramatically
#' }
#'
#' @examples
#' \dontrun{
#' # Transform stops to Illinois State Plane for accurate distance calc
#' stops_il <- validate_coordinate_transform(
#'   stops_wgs84,
#'   3435,  # EPSG:3435 IL State Plane East
#'   "stops"
#' )
#'
#' # Transform back to WGS84 for web mapping
#' stops_web <- validate_coordinate_transform(
#'   stops_il,
#'   4326,
#'   "stops_web",
#'   check_area = FALSE  # Area check not meaningful across projections
#' )
#' }
#'
#' @export
validate_coordinate_transform <- function(sf_object, target_crs, name,
                                          check_area = FALSE) {
  if (nrow(sf_object) == 0) {
    return(sf::st_transform(sf_object, target_crs))
  }

  # Validate before transform
  sf_object <- validate_geometries(sf_object, paste0(name, " (before transform)"))

  # Record original CRS
  original_crs <- sf::st_crs(sf_object)

  # Calculate area before (if checking)
  if (check_area && sf::st_geometry_type(sf_object)[1] %in% c("POLYGON", "MULTIPOLYGON")) {
    area_before <- sum(sf::st_area(sf_object))
  }

  # Perform transformation
  sf_transformed <- sf::st_transform(sf_object, target_crs)

  # Validate after transform
  sf_transformed <- validate_geometries(sf_transformed, paste0(name, " (after transform)"))

  # Check area change (if requested and applicable)
  if (check_area && exists("area_before")) {
    area_after <- sum(sf::st_area(sf_transformed))

    # Convert to numeric and compare (handling units)
    area_before_num <- as.numeric(area_before)
    area_after_num <- as.numeric(area_after)

    pct_change <- abs((area_after_num - area_before_num) / area_before_num * 100)

    if (pct_change > 1) {
      warning(paste0(
        name, ": coordinate transformation changed total area by ",
        round(pct_change, 1), "% ",
        "(from CRS ", original_crs$epsg, " to ", sf::st_crs(sf_transformed)$epsg, ")"
      ))
    }
  }

  sf_transformed
}

#' Validate Buffer Result
#'
#' Checks that buffer operation produced valid geometries of expected size.
#'
#' @param sf_buffered sf object after buffering
#' @param expected_dist Numeric. Expected buffer distance
#' @param dist_unit Character. Unit of distance ("ft", "m", etc.)
#' @param name Character. Name for error messages
#' @param tolerance Numeric. Allowed deviation from expected distance (as fraction).
#'   Default: 0.05 (5%)
#'
#' @return Validated sf object
#'
#' @details
#' Validates that:
#' \itemize{
#'   \item All geometries are valid polygons/multipolygons
#'   \item Buffer distances are approximately correct
#'   \item No unexpected empty geometries
#' }
#'
#' @examples
#' \dontrun{
#' # Create 1/2 mile (2640 ft) buffer around stops
#' stops_il <- st_transform(stops, 3435)  # IL State Plane uses feet
#' hub_buffers <- st_buffer(stops_il, dist = 2640)
#' hub_buffers <- validate_buffer_result(
#'   hub_buffers,
#'   expected_dist = 2640,
#'   dist_unit = "ft",
#'   name = "hub_buffers"
#' )
#' }
#'
#' @export
validate_buffer_result <- function(sf_buffered, expected_dist, dist_unit, name,
                                   tolerance = 0.05) {
  # Validate geometries
  sf_buffered <- validate_geometries(sf_buffered, name)

  if (nrow(sf_buffered) == 0) {
    warning(paste0(name, ": buffer operation resulted in empty sf object"))
    return(sf_buffered)
  }

  # Check for empty geometries
  empty_geoms <- sf::st_is_empty(sf_buffered)
  if (any(empty_geoms)) {
    warning(paste0(
      name, ": ", sum(empty_geoms),
      " empty geometries after buffering"
    ))
  }

  # Check geometry types
  geom_types <- unique(as.character(sf::st_geometry_type(sf_buffered)))
  expected_types <- c("POLYGON", "MULTIPOLYGON")

  unexpected_types <- setdiff(geom_types, expected_types)
  if (length(unexpected_types) > 0) {
    warning(paste0(
      name, ": unexpected geometry types after buffering: ",
      paste(unexpected_types, collapse = ", ")
    ))
  }

  # Sample a few features and check approximate buffer distance
  # (This is a rough check - actual buffer distance can vary slightly)
  if (nrow(sf_buffered) > 0 && !all(empty_geoms)) {
    # Sample up to 10 non-empty features
    sample_idx <- which(!empty_geoms)
    if (length(sample_idx) > 10) {
      sample_idx <- sample(sample_idx, 10)
    }

    # For circular buffers, approximate radius from area: r = sqrt(A/π)
    areas <- sf::st_area(sf_buffered[sample_idx, ])
    approx_radii <- sqrt(as.numeric(areas) / pi)

    # Check if radii are within tolerance of expected distance
    pct_diffs <- abs((approx_radii - expected_dist) / expected_dist)
    if (any(pct_diffs > tolerance)) {
      warning(paste0(
        name, ": some buffers appear to be ",
        round(mean(pct_diffs) * 100, 1),
        "% different from expected distance of ", expected_dist, " ", dist_unit
      ))
    }
  }

  sf_buffered
}

#' Validate Illinois Bounds
#'
#' Checks that geometries are within (or reasonably near) Illinois state boundaries.
#'
#' @param sf_object sf object to validate
#' @param name Character. Name for error messages
#' @param strict Logical. If TRUE, errors on any features outside Illinois.
#'   If FALSE (default), just warns. Default: FALSE
#' @param buffer_miles Numeric. Buffer around Illinois boundary (in miles) to
#'   allow for border cases. Default: 5
#'
#' @return sf object (unchanged)
#'
#' @details
#' Some transit agencies (Metra, Metro STL) serve areas outside Illinois.
#' By default, this function warns but doesn't error on out-of-state features.
#'
#' Illinois approximate bounds (WGS84):
#' \itemize{
#'   \item North: 42.5°N
#'   \item South: 37.0°N
#'   \item East: -87.5°W
#'   \item West: -91.5°W
#' }
#'
#' @export
validate_illinois_bounds <- function(sf_object, name, strict = FALSE, buffer_miles = 5) {
  if (nrow(sf_object) == 0) {
    return(sf_object)
  }

  # Ensure in WGS84 for lat/lon check
  if (sf::st_crs(sf_object)$epsg != 4326) {
    sf_check <- sf::st_transform(sf_object, 4326)
  } else {
    sf_check <- sf_object
  }

  # Get bounding box
  bbox <- sf::st_bbox(sf_check)

  # Illinois bounds with buffer (convert miles to degrees, ~69 miles per degree latitude)
  buffer_deg <- buffer_miles / 69

  il_bounds <- list(
    north = 42.5 + buffer_deg,
    south = 37.0 - buffer_deg,
    east = -87.5 + buffer_deg,
    west = -91.5 - buffer_deg
  )

  # Check if any features are outside bounds
  outside <- (
    bbox["ymax"] > il_bounds$north ||
    bbox["ymin"] < il_bounds$south ||
    bbox["xmax"] > il_bounds$east ||
    bbox["xmin"] < il_bounds$west
  )

  if (outside) {
    msg <- paste0(
      name, ": geometries extend outside Illinois bounds ",
      "(bbox: ", round(bbox["ymin"], 2), "°N to ", round(bbox["ymax"], 2),
      "°N, ", round(bbox["xmin"], 2), "°W to ", round(bbox["xmax"], 2), "°W)"
    )

    if (strict) {
      stop(msg)
    } else {
      message(paste0("NOTE: ", msg))
      message("  This may be expected for border agencies (Metra, Metro STL)")
    }
  }

  sf_object
}
