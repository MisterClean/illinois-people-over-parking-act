# Buffer Processing Functions
#
# Functions for creating 1/2 mile buffers around transit hubs and combining
# with corridor buffers.
#
# High-Level Functions:
#   - create_hub_buffers(): Create 1/2 mile buffers around hubs by agency
#   - create_combined_buffers(): Combine hub and corridor buffers
#
# Helper Functions:
#   - None

#' Create Hub Buffers
#'
#' Creates 1/2 mile (2640 ft) buffers around transit hubs.
#' Returns buffers separated by agency and a union of all hub buffers.
#' Now dynamically handles all agencies via metadata.
#'
#' @param all_hubs_sf sf object with all transit hubs
#' @param illinois_boundary sf object with Illinois state boundary
#' @return List containing:
#'   - all_hub_areas: Union of all hub buffers (WGS84)
#'   - half_mile_buffers: Individual hub buffers (WGS84)
#'   - per_agency: Named list of agency-specific buffers (not unioned)
#'   - per_agency_union: Named list of agency-specific buffer unions (clipped to IL)
#'   - Legacy fields for backward compatibility: cta_hubs_union, pace_hubs_union, etc.
#'
#' @examples
#' \dontrun{
#' hub_buffers <- create_hub_buffers(all_hubs_sf, illinois_boundary)
#' # Access via: hub_buffers$per_agency_union$cta
#' # Or legacy: hub_buffers$cta_hubs_union
#' }
create_hub_buffers <- function(all_hubs_sf, illinois_boundary) {
  cat("\n=== Creating Hub Buffers ===\n\n")

  # Buffer hubs (1/2 mile = 2640 feet)
  all_hubs_projected <- st_transform(all_hubs_sf, 3435)
  cat("Creating 1/2 mile buffers around hubs...\n")
  half_mile_buffers <- st_buffer(all_hubs_projected, 2640)
  all_hub_areas <- st_union(half_mile_buffers)

  # Transform back to WGS84
  all_hub_areas_wgs84 <- st_transform(all_hub_areas, 4326)
  half_mile_buffers_wgs84 <- st_transform(half_mile_buffers, 4326)

  # Dynamically separate buffers by agency
  cat("Separating buffers by agency...\n")
  unique_agencies <- unique(half_mile_buffers_wgs84$agency)

  per_agency <- list()
  per_agency_union <- list()

  for (agency_id in unique_agencies) {
    agency_buffers <- half_mile_buffers_wgs84[half_mile_buffers_wgs84$agency == agency_id, ]
    per_agency[[agency_id]] <- agency_buffers

    # Create union and clip to Illinois boundary
    if (nrow(agency_buffers) > 0) {
      per_agency_union[[agency_id]] <- st_intersection(
        st_union(agency_buffers),
        illinois_boundary
      )
    } else {
      per_agency_union[[agency_id]] <- st_sfc(crs = 4326)
    }
  }

  cat("Hub buffers created successfully\n")

  # Build return list with new dynamic structure
  result <- list(
    all_hub_areas = all_hub_areas_wgs84,
    half_mile_buffers = half_mile_buffers_wgs84,
    per_agency = per_agency,
    per_agency_union = per_agency_union
  )

  # Add legacy fields for backward compatibility
  # Extract known agencies if they exist
  legacy_agencies <- c("cta", "pace", "metra", "metro_stl", "cumtd", "rmtd")
  for (agency_id in legacy_agencies) {
    field_name <- paste0(agency_id, "_hubs_union")
    if (agency_id %in% names(per_agency_union)) {
      result[[field_name]] <- per_agency_union[[agency_id]]
    } else {
      result[[field_name]] <- st_sfc(crs = 4326)
    }
  }

  return(result)
}

#' Create Combined Buffers
#'
#' Combines hub and corridor buffers into a single geometry representing
#' all areas affected by the People Over Parking Act.
#'
#' @param all_hub_areas_wgs84 sf object with union of all hub buffers
#' @param all_corridors_union_wgs84 sf object with union of all corridor buffers
#' @param illinois_boundary sf object with Illinois state boundary
#' @return sf object with combined affected areas (clipped to Illinois)
#'
#' @examples
#' \dontrun{
#' combined_areas <- create_combined_buffers(
#'   hub_buffers$all_hub_areas,
#'   corridor_buffer,
#'   illinois_boundary
#' )
#' }
create_combined_buffers <- function(all_hub_areas_wgs84,
                                    all_corridors_union_wgs84,
                                    illinois_boundary) {
  cat("\n=== Combining All Affected Areas ===\n\n")

  # Combine all affected areas (hubs + corridors)
  all_affected_areas_combined_raw <- st_union(c(all_hub_areas_wgs84, all_corridors_union_wgs84))

  # Clip to Illinois boundary to exclude Missouri areas
  all_affected_areas_combined <- st_intersection(all_affected_areas_combined_raw, illinois_boundary)

  cat("Combined buffers created successfully\n")

  return(all_affected_areas_combined)
}
