# Map Creation Functions
#
# Functions for creating the interactive Leaflet map visualizing transit
# hubs, corridors, and affected areas.
#
# High-Level Functions:
#   - create_interactive_map(): Creates complete Leaflet map with all layers
#
# Helper Functions:
#   - get_agency_color_palette(): Returns color palette for agencies
#   - create_hub_popup_html(): Creates popup HTML for hub markers

#' Get Agency Color Palette
#'
#' Returns the standardized color palette for transit agencies.
#' Now pulls colors from centralized agency metadata.
#'
#' @return Named vector of colors where names are display names and values are hex colors
get_agency_color_palette <- function() {
  metadata <- get_agency_metadata()
  colors <- sapply(metadata, function(x) x$color)
  names(colors) <- sapply(metadata, function(x) x$name)
  return(colors)
}

#' Create Hub Popup HTML
#'
#' Creates formatted HTML popup content for transit hub markers.
#' Includes different information for rail vs bus hubs.
#'
#' @param hub_sf sf object for a single hub (row from all_hubs_sf)
#' @return Character string with HTML popup content
create_hub_popup_html <- function(hub_sf) {
  popup <- paste0(
    "<strong>", hub_sf$stop_name, "</strong><br>",
    "Agency: ", hub_sf$agency_name, "<br>",
    "Type: ", hub_sf$type, "<br>",
    "Stop ID: ", hub_sf$stop_id, "<br>"
  )

  # Add diagnostic info for bus hubs
  if (!is.null(hub_sf$type) && hub_sf$type == "bus_hub" && !is.na(hub_sf$num_routes_total)) {
    popup <- paste0(
      popup,
      "<hr>",
      "<strong>Cluster ID: ", hub_sf$cluster_id, "</strong><br>",
      "<hr>",
      "<strong>Routes:</strong> ", if_else(!is.na(hub_sf$routes), hub_sf$routes, "N/A"), "<br>",
      "<strong>Directions:</strong> ", if_else(!is.na(hub_sf$directions), hub_sf$directions, "N/A"), "<br>",
      "<hr>",
      "<strong>Service Frequency:</strong><br>",
      "Routes (AM/PM/Total): ", hub_sf$num_routes_am, "/", hub_sf$num_routes_pm, "/", hub_sf$num_routes_total, "<br>",
      "Trips (AM/PM/Total): ", hub_sf$trips_am, "/", hub_sf$trips_pm, "/", hub_sf$trips_total, "<br>",
      "Avg Interval (AM/PM): ", round(hub_sf$interval_am, 1), "/", round(hub_sf$interval_pm, 1), " min<br>",
      "Combined Interval: ", round(hub_sf$interval_combined, 1), " min<br>",
      "<hr>",
      "<strong>AM Peak Service (7-9 AM Central):</strong><br>",
      "First Departure: ", if_else(!is.na(hub_sf$first_departure_am), hub_sf$first_departure_am, "N/A"), "<br>",
      "Last Departure: ", if_else(!is.na(hub_sf$last_departure_am), hub_sf$last_departure_am, "N/A"), "<br>",
      "<hr>",
      "<strong>PM Peak Service (4-6 PM Central):</strong><br>",
      "First Departure: ", if_else(!is.na(hub_sf$first_departure_pm), hub_sf$first_departure_pm, "N/A"), "<br>",
      "Last Departure: ", if_else(!is.na(hub_sf$last_departure_pm), hub_sf$last_departure_pm, "N/A")
    )
  }

  return(popup)
}

#' Create Interactive Map
#'
#' Creates a Leaflet map showing transit hubs, corridors, and affected areas.
#' Dynamically handles all agencies via metadata. Includes layer controls and
#' Illinois state boundary outline.
#'
#' @param all_hubs_sf sf object with all transit hubs
#' @param all_affected_areas sf object with combined affected areas
#' @param hub_buffers List from create_hub_buffers() with per_agency_union field
#' @param all_corridors_union sf object with corridor buffers
#' @param illinois_boundary sf object with Illinois state boundary (for outline)
#' @param center_lng Longitude for map center (default: Chicago -87.6079)
#' @param center_lat Latitude for map center (default: Chicago 41.8917)
#' @param zoom Initial zoom level (default: 9)
#' @return Leaflet map object
#'
#' @examples
#' \dontrun{
#' hub_buffers <- create_hub_buffers(all_hubs_sf, illinois_boundary)
#' map <- create_interactive_map(
#'   all_hubs_sf,
#'   all_affected_areas_combined,
#'   hub_buffers,
#'   all_corridors_union_wgs84,
#'   illinois_boundary
#' )
#' }
create_interactive_map <- function(all_hubs_sf,
                                   all_affected_areas,
                                   hub_buffers,
                                   all_corridors_union,
                                   illinois_boundary,
                                   center_lng = -87.6079,
                                   center_lat = 41.8917,
                                   zoom = 9) {
  cat("\n=== Creating Interactive Map ===\n\n")

  # Define color palette for agencies
  agency_colors <- get_agency_color_palette()
  agency_pal <- colorFactor(
    palette = agency_colors,
    domain = all_hubs_sf$agency_name
  )

  # Create base map
  map <- leaflet() %>%
    setView(lng = center_lng, lat = center_lat, zoom = zoom) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%

    # Combined affected areas (default visible)
    addPolygons(
      data = all_affected_areas,
      fillColor = "purple",
      fillOpacity = 0.25,
      weight = 1,
      color = "purple",
      opacity = 0.7,
      group = "All Affected Areas (Hubs + Corridors)"
    )

  # Dynamically add hub buffer layers for each agency
  cat("Adding agency hub buffer layers...\n")
  per_agency_union <- hub_buffers$per_agency_union

  agency_hub_groups <- c()  # Track group names for layer control

  for (agency_id in names(per_agency_union)) {
    agency_buffer <- per_agency_union[[agency_id]]
    agency_name <- get_agency_display_name(agency_id)
    agency_color <- get_agency_color(agency_id)
    group_name <- paste0(agency_name, " Hubs (1/2 mile)")

    # Only add layer if buffer has data
    if (length(agency_buffer) > 0 && !is.null(agency_buffer)) {
      map <- map %>%
        addPolygons(
          data = agency_buffer,
          fillColor = agency_color,
          fillOpacity = 0.4,
          weight = 1,
          color = agency_color,
          opacity = 0.8,
          group = group_name
        )

      agency_hub_groups <- c(agency_hub_groups, group_name)
    }
  }

  # Add corridor and hub points layers
  map <- map %>%
    # Corridor areas (combined)
    addPolygons(
      data = all_corridors_union,
      fillColor = "#FF8C00", # Orange for corridors
      fillOpacity = 0.3,
      weight = 1,
      color = "#FF8C00",
      opacity = 0.6,
      group = "All Corridors (1/8 mile)",
      dashArray = "5,5"
    ) %>%

    # Hub points with popups
    addCircleMarkers(
      data = all_hubs_sf,
      radius = 3,
      color = ~agency_pal(agency_name),
      stroke = FALSE,
      fillOpacity = 0.8,
      group = "Transit Hub Points",
      popup = ~paste0(
        "<strong>", stop_name, "</strong><br>",
        "Agency: ", agency_name, "<br>",
        "Type: ", type, "<br>",
        "Stop ID: ", stop_id, "<br>",
        # Add diagnostic info for bus hubs
        if_else(type == "bus_hub" & !is.na(num_routes_total),
          paste0(
            "<hr>",
            "<strong>Cluster ID: ", cluster_id, "</strong><br>",
            "<hr>",
            "<strong>Routes:</strong> ", if_else(!is.na(routes), routes, "N/A"), "<br>",
            "<strong>Directions:</strong> ", if_else(!is.na(directions), directions, "N/A"), "<br>",
            "<hr>",
            "<strong>Service Frequency:</strong><br>",
            "Routes (AM/PM/Total): ", num_routes_am, "/", num_routes_pm, "/", num_routes_total, "<br>",
            "Trips (AM/PM/Total): ", trips_am, "/", trips_pm, "/", trips_total, "<br>",
            "Avg Interval (AM/PM): ", round(interval_am, 1), "/", round(interval_pm, 1), " min<br>",
            "Combined Interval: ", round(interval_combined, 1), " min<br>",
            "<hr>",
            "<strong>AM Peak Service (7-9 AM Central):</strong><br>",
            "First Departure: ", if_else(!is.na(first_departure_am), first_departure_am, "N/A"), "<br>",
            "Last Departure: ", if_else(!is.na(last_departure_am), last_departure_am, "N/A"), "<br>",
            "<hr>",
            "<strong>PM Peak Service (4-6 PM Central):</strong><br>",
            "First Departure: ", if_else(!is.na(first_departure_pm), first_departure_pm, "N/A"), "<br>",
            "Last Departure: ", if_else(!is.na(last_departure_pm), last_departure_pm, "N/A")
          ),
          ""
        )
      )
    ) %>%

    # Add Illinois state boundary outline (reference line, always visible)
    addPolylines(
      data = illinois_boundary,
      color = "#666666",
      weight = 2,
      opacity = 0.8,
      fill = FALSE
    )

  # Build overlay groups list dynamically
  overlay_groups <- c(
    "All Affected Areas (Hubs + Corridors)",
    agency_hub_groups,
    "All Corridors (1/8 mile)",
    "Transit Hub Points"
  )

  # Build legend colors and labels dynamically
  legend_colors <- c("purple", agency_colors[sapply(names(per_agency_union), get_agency_display_name)], "#FF8C00")
  legend_labels <- c("All Affected Areas",
                     paste0(sapply(names(per_agency_union), get_agency_display_name), " Hubs"),
                     "All Corridors (dashed)")

  # Add layer controls
  map <- map %>%
    addLayersControl(
      baseGroups = c("CartoDB.Positron"),
      overlayGroups = overlay_groups,
      options = layersControlOptions(collapsed = FALSE)
    ) %>%

    # Hide individual layers by default, show only combined
    hideGroup(c(
      agency_hub_groups,
      "All Corridors (1/8 mile)",
      "Transit Hub Points"
    )) %>%

    # Add legend
    addLegend(
      position = "bottomright",
      colors = legend_colors,
      labels = legend_labels,
      opacity = 0.7
    ) %>%

    addFullscreenControl() %>%
    addMeasure(
      position = "bottomleft",
      primaryLengthUnit = "miles",
      primaryAreaUnit = "sqmiles",
      activeColor = "#3D535D",
      completedColor = "#7D4479"
    )

  cat("Interactive map created successfully\n")

  return(map)
}
