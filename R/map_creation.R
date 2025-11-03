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
#'
#' @return Named vector of colors for each agency
get_agency_color_palette <- function() {
  c(
    CTA = "#009CDE",       # Blue
    Pace = "#814C9E",      # Purple
    Metra = "#E31837",     # Red
    "Metro STL" = "#00A651", # Green
    MTD = "#FF6600",       # Orange (CUMTD)
    RMTD = "#FFD700"       # Gold (Rockford)
  )
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
#' Includes layer controls for toggling different agency buffers and features.
#'
#' @param all_hubs_sf sf object with all transit hubs
#' @param all_affected_areas sf object with combined affected areas
#' @param cta_hubs_union sf object with CTA hub buffers
#' @param pace_hubs_union sf object with Pace hub buffers
#' @param metra_hubs_union sf object with Metra hub buffers
#' @param metro_stl_hubs_union sf object with Metro STL hub buffers
#' @param cumtd_hubs_union sf object with CUMTD hub buffers
#' @param rmtd_hubs_union sf object with RMTD hub buffers
#' @param all_corridors_union sf object with corridor buffers
#' @param center_lng Longitude for map center (default: Chicago -87.6079)
#' @param center_lat Latitude for map center (default: Chicago 41.8917)
#' @param zoom Initial zoom level (default: 9)
#' @return Leaflet map object
#'
#' @examples
#' \dontrun{
#' map <- create_interactive_map(
#'   all_hubs_sf,
#'   all_affected_areas_combined,
#'   cta_hubs_union, pace_hubs_union, metra_hubs_union,
#'   metro_stl_hubs_union, cumtd_hubs_union, rmtd_hubs_union,
#'   all_corridors_union_wgs84
#' )
#' }
create_interactive_map <- function(all_hubs_sf,
                                   all_affected_areas,
                                   cta_hubs_union,
                                   pace_hubs_union,
                                   metra_hubs_union,
                                   metro_stl_hubs_union,
                                   cumtd_hubs_union,
                                   rmtd_hubs_union,
                                   all_corridors_union,
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

  # Create the interactive map
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
    ) %>%

    # Hub areas by agency
    addPolygons(
      data = cta_hubs_union,
      fillColor = agency_colors["CTA"],
      fillOpacity = 0.4,
      weight = 1,
      color = agency_colors["CTA"],
      opacity = 0.8,
      group = "CTA Hubs (1/2 mile)"
    ) %>%
    addPolygons(
      data = pace_hubs_union,
      fillColor = agency_colors["Pace"],
      fillOpacity = 0.4,
      weight = 1,
      color = agency_colors["Pace"],
      opacity = 0.8,
      group = "Pace Hubs (1/2 mile)"
    ) %>%
    addPolygons(
      data = metra_hubs_union,
      fillColor = agency_colors["Metra"],
      fillOpacity = 0.4,
      weight = 1,
      color = agency_colors["Metra"],
      opacity = 0.8,
      group = "Metra Hubs (1/2 mile)"
    ) %>%
    addPolygons(
      data = metro_stl_hubs_union,
      fillColor = agency_colors["Metro STL"],
      fillOpacity = 0.4,
      weight = 1,
      color = agency_colors["Metro STL"],
      opacity = 0.8,
      group = "Metro STL Hubs (1/2 mile)"
    ) %>%
    addPolygons(
      data = cumtd_hubs_union,
      fillColor = agency_colors["MTD"],
      fillOpacity = 0.4,
      weight = 1,
      color = agency_colors["MTD"],
      opacity = 0.8,
      group = "MTD Hubs (1/2 mile)"
    ) %>%
    addPolygons(
      data = rmtd_hubs_union,
      fillColor = agency_colors["RMTD"],
      fillOpacity = 0.4,
      weight = 1,
      color = agency_colors["RMTD"],
      opacity = 0.8,
      group = "RMTD Hubs (1/2 mile)"
    ) %>%

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

    # Layer controls
    addLayersControl(
      baseGroups = c("CartoDB.Positron"),
      overlayGroups = c(
        "All Affected Areas (Hubs + Corridors)",
        "CTA Hubs (1/2 mile)",
        "Pace Hubs (1/2 mile)",
        "Metra Hubs (1/2 mile)",
        "Metro STL Hubs (1/2 mile)",
        "MTD Hubs (1/2 mile)",
        "RMTD Hubs (1/2 mile)",
        "All Corridors (1/8 mile)",
        "Transit Hub Points"
      ),
      options = layersControlOptions(collapsed = FALSE)
    ) %>%

    # Hide individual layers by default, show only combined
    hideGroup(c(
      "CTA Hubs (1/2 mile)",
      "Pace Hubs (1/2 mile)",
      "Metra Hubs (1/2 mile)",
      "Metro STL Hubs (1/2 mile)",
      "MTD Hubs (1/2 mile)",
      "RMTD Hubs (1/2 mile)",
      "All Corridors (1/8 mile)",
      "Transit Hub Points"
    )) %>%

    # Add legend
    addLegend(
      position = "bottomright",
      colors = c("purple", agency_colors["CTA"], agency_colors["Pace"],
                 agency_colors["Metra"], agency_colors["Metro STL"],
                 agency_colors["MTD"], agency_colors["RMTD"], "#FF8C00"),
      labels = c("All Affected Areas",
                 "CTA Hubs",
                 "Pace Hubs",
                 "Metra Hubs",
                 "Metro STL Hubs",
                 "MTD Hubs",
                 "RMTD Hubs",
                 "All Corridors (dashed)"),
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
