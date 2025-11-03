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
#'
#' @param all_hubs_sf sf object with all transit hubs
#' @param illinois_boundary sf object with Illinois state boundary
#' @return List containing:
#'   - all_hub_areas: Union of all hub buffers (WGS84)
#'   - half_mile_buffers: Individual hub buffers (WGS84)
#'   - cta_hubs_union: CTA hub buffers union (clipped to IL)
#'   - pace_hubs_union: Pace hub buffers union (clipped to IL)
#'   - metra_hubs_union: Metra hub buffers union (clipped to IL)
#'   - metro_stl_hubs_union: Metro STL hub buffers union (clipped to IL)
#'   - cumtd_hubs_union: CUMTD hub buffers union (clipped to IL)
#'   - rmtd_hubs_union: RMTD hub buffers union (clipped to IL)
#'
#' @examples
#' \dontrun{
#' hub_buffers <- create_hub_buffers(all_hubs_sf, illinois_boundary)
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

  # Separate hub buffers by agency
  cat("Separating buffers by agency...\n")
  cta_hub_buffers <- half_mile_buffers_wgs84[half_mile_buffers_wgs84$agency == "cta", ]
  pace_hub_buffers <- half_mile_buffers_wgs84[half_mile_buffers_wgs84$agency == "pace", ]
  metra_hub_buffers <- half_mile_buffers_wgs84[half_mile_buffers_wgs84$agency == "metra", ]
  metro_stl_hub_buffers <- half_mile_buffers_wgs84[half_mile_buffers_wgs84$agency == "metro_stl", ]
  cumtd_hub_buffers <- half_mile_buffers_wgs84[half_mile_buffers_wgs84$agency == "cumtd", ]
  rmtd_hub_buffers <- half_mile_buffers_wgs84[half_mile_buffers_wgs84$agency == "rmtd", ]

  # Union hub buffers by agency and clip to Illinois boundary
  cat("Creating agency-specific buffer unions...\n")
  cta_hubs_union <- if(nrow(cta_hub_buffers) > 0) {
    st_intersection(st_union(cta_hub_buffers), illinois_boundary)
  } else {
    st_sfc(crs = 4326)
  }

  pace_hubs_union <- if(nrow(pace_hub_buffers) > 0) {
    st_intersection(st_union(pace_hub_buffers), illinois_boundary)
  } else {
    st_sfc(crs = 4326)
  }

  metra_hubs_union <- if(nrow(metra_hub_buffers) > 0) {
    st_intersection(st_union(metra_hub_buffers), illinois_boundary)
  } else {
    st_sfc(crs = 4326)
  }

  metro_stl_hubs_union <- if(nrow(metro_stl_hub_buffers) > 0) {
    st_intersection(st_union(metro_stl_hub_buffers), illinois_boundary)
  } else {
    st_sfc(crs = 4326)
  }

  cumtd_hubs_union <- if(nrow(cumtd_hub_buffers) > 0) {
    st_intersection(st_union(cumtd_hub_buffers), illinois_boundary)
  } else {
    st_sfc(crs = 4326)
  }

  rmtd_hubs_union <- if(nrow(rmtd_hub_buffers) > 0) {
    st_intersection(st_union(rmtd_hub_buffers), illinois_boundary)
  } else {
    st_sfc(crs = 4326)
  }

  cat("Hub buffers created successfully\n")

  return(list(
    all_hub_areas = all_hub_areas_wgs84,
    half_mile_buffers = half_mile_buffers_wgs84,
    cta_hubs_union = cta_hubs_union,
    pace_hubs_union = pace_hubs_union,
    metra_hubs_union = metra_hubs_union,
    metro_stl_hubs_union = metro_stl_hubs_union,
    cumtd_hubs_union = cumtd_hubs_union,
    rmtd_hubs_union = rmtd_hubs_union
  ))
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
