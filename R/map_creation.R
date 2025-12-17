# Map Creation Functions
#
# Functions for creating the interactive Leaflet map visualizing transit
# hubs, corridors, and affected areas.
#
# High-Level Functions:
#   - create_interactive_map(): Creates complete Leaflet map with all layers
#
# Helper Functions:
#   - get_agency_color_palette(): Returns color palette for agencies (deprecated, kept for compatibility)
#   - create_hub_popup_html(): Creates popup HTML for hub markers (deprecated, kept for compatibility)

#' Get Agency Color Palette (Deprecated)
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

#' Create Hub Popup HTML (Deprecated)
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
#' Uses a colorblind-safe creative palette with detailed transit layers.
#'
#' @param all_hubs_sf sf object with all transit hubs
#' @param all_affected_areas sf object with combined affected areas (hubs + corridors)
#' @param hub_buffers List from create_hub_buffers() with all_hub_areas field
#' @param all_corridors_union sf object with corridor buffers
#' @param qualifying_corridor_segments_sf sf object with individual qualifying corridor segments
#' @param all_shapes_sf sf object with all bus route shapes (LINESTRING geometries)
#' @param all_stops sf object or data.table with all bus stops
#' @param illinois_boundary sf object with Illinois state boundary (for outline)
#' @param center_lng Longitude for map center (default: Chicago -87.6079)
#' @param center_lat Latitude for map center (default: Chicago 41.8917)
#' @param zoom Initial zoom level (default: 9)
#' @return Leaflet map object
#'
#' @details
#' Map layers (colorblind-safe palette):
#' \itemize{
#'   \item Combined Parking Relief (purple #9370DB) - hubs + corridors, default visible
#'   \item Hub Buffers (teal #20B2AA) - 1/2 mile buffers, hidden by default
#'   \item Corridor Buffers (coral #FF6B6B) - 1/8 mile buffers, hidden by default
#'   \item Corridor Segments (crimson #DC143C) - qualifying edges with popups, hidden by default
#'   \item Hub Points (indigo #4B0082) - transit hubs with popups, default visible
#'   \item All Bus Routes (gray #808080) - reference shapes, hidden by default
#'   \item Bus Stops (steel blue #4682B4) - all stops, hidden by default
#' }
#'
#' @examples
#' \dontrun{
#' all_shapes_sf <- convert_shapes_to_linestrings(all_shapes)
#' hub_buffers <- create_hub_buffers(all_hubs_sf, illinois_boundary)
#' map <- create_interactive_map(
#'   all_hubs_sf,
#'   all_affected_areas_combined,
#'   hub_buffers,
#'   all_corridors_union_wgs84,
#'   qualifying_corridor_segments_sf,
#'   all_shapes_sf,
#'   all_stops,
#'   illinois_boundary
#' )
#' }
create_interactive_map <- function(all_hubs_sf,
                                   all_affected_areas,
                                   hub_buffers,
                                   all_corridors_union,
                                   qualifying_corridor_segments_sf,
                                   all_shapes_sf,
                                   all_stops,
                                   illinois_boundary,
                                   center_lng = -87.6079,
                                   center_lat = 41.8917,
                                   zoom = 9) {
  cat("\n=== Creating Interactive Map ===\n\n")

  # Convert stops to sf if needed
  if (!inherits(all_stops, "sf")) {
    cat("Converting stops to sf object...\n")
    all_stops <- st_as_sf(all_stops, coords = c("stop_lon", "stop_lat"), crs = 4326)
  }

  # Colorblind-safe creative palette
  COLOR_PARKING_RELIEF <- "#9370DB"  # Purple - combined parking relief
  COLOR_HUB_BUFFER <- "#20B2AA"      # Teal - hub buffers
  COLOR_CORRIDOR_BUFFER <- "#FF6B6B" # Coral - corridor buffers
  COLOR_CORRIDOR_SEGMENTS <- "#DC143C" # Crimson - corridor segments
  COLOR_HUB_POINTS <- "#4B0082"      # Indigo - hub points
  COLOR_ALL_ROUTES <- "#808080"      # Gray - all routes
  COLOR_BUS_STOPS <- "#4682B4"       # Steel blue - bus stops

  # Create base map
  cat("Building base map layers...\n")
  map <- leaflet() %>%
    setView(lng = center_lng, lat = center_lat, zoom = zoom) %>%
    addProviderTiles(providers$CartoDB.Positron)

  # Layer 1: Combined Parking Relief (hubs + corridors) - DEFAULT VISIBLE, Purple
  cat("Adding Combined Parking Relief layer...\n")
  map <- map %>%
    addPolygons(
      data = all_affected_areas,
      fillColor = COLOR_PARKING_RELIEF,
      fillOpacity = 0.25,
      weight = 1,
      color = COLOR_PARKING_RELIEF,
      opacity = 0.7,
      group = "Combined Parking Relief (Hubs + Corridors)"
    )

  # Layer 2: Combined Hub Buffers - HIDDEN, Teal
  cat("Adding Hub Buffers layer...\n")
  map <- map %>%
    addPolygons(
      data = hub_buffers$all_hub_areas,
      fillColor = COLOR_HUB_BUFFER,
      fillOpacity = 0.3,
      weight = 1,
      color = COLOR_HUB_BUFFER,
      opacity = 0.8,
      group = "Hub Buffers (1/2 mile)"
    )

  # Layer 3: Corridor Buffers - HIDDEN, Coral
  cat("Adding Corridor Buffers layer...\n")
  map <- map %>%
    addPolygons(
      data = all_corridors_union,
      fillColor = COLOR_CORRIDOR_BUFFER,
      fillOpacity = 0.3,
      weight = 1,
      color = COLOR_CORRIDOR_BUFFER,
      opacity = 0.8,
      group = "Corridor Buffers (1/8 mile)"
    )

  # Layer 4: Corridor Segments (individual qualifying edges) - HIDDEN, Crimson
  cat("Adding Corridor Segments layer...\n")
  if (!is.null(qualifying_corridor_segments_sf) && nrow(qualifying_corridor_segments_sf) > 0) {
    map <- map %>%
      addPolylines(
        data = qualifying_corridor_segments_sf,
        color = COLOR_CORRIDOR_SEGMENTS,
        weight = 3,
        opacity = 0.9,
        group = "Corridor Segments (Qualifying)",
        popup = ~paste0(
          "<strong>Qualifying Corridor Segment</strong><br>",
          "Edge: ", from_cluster, " → ", to_cluster, "<br>",
          "Routes: ", num_routes, "<br>",
          "Trips AM: ", trips_am, "<br>",
          "Trips PM: ", trips_pm, "<br>",
          "Interval AM: ", round(interval_am, 1), " min<br>",
          "Interval PM: ", round(interval_pm, 1), " min"
        )
      )
  }

  # Layer 5: Hub Points - DEFAULT VISIBLE, Indigo
  cat("Adding Hub Points layer...\n")
  map <- map %>%
    addCircleMarkers(
      data = all_hubs_sf,
      radius = 4,
      color = COLOR_HUB_POINTS,
      stroke = FALSE,
      fillOpacity = 0.8,
      group = "Hub Points",
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
    )

  # Layer 6: All Bus Routes (shapes) - HIDDEN, Gray
  cat("Adding All Bus Routes layer...\n")
  if (!is.null(all_shapes_sf) && nrow(all_shapes_sf) > 0) {
    map <- map %>%
      addPolylines(
        data = all_shapes_sf,
        color = COLOR_ALL_ROUTES,
        weight = 2,
        opacity = 0.5,
        group = "All Bus Routes",
        popup = ~paste0("Shape ID: ", unique_shape_id, "<br>Agency: ", agency)
      )
  }

  # Layer 7: Bus Stops - HIDDEN, Steel Blue
  cat("Adding Bus Stops layer...\n")
  if (!is.null(all_stops) && nrow(all_stops) > 0) {
    map <- map %>%
      addCircleMarkers(
        data = all_stops,
        radius = 3,
        color = COLOR_BUS_STOPS,
        fillColor = "white",
        fillOpacity = 0.8,
        weight = 1,
        group = "Bus Stops",
        popup = ~paste0("<b>", stop_name, "</b><br>Stop ID: ", stop_id, "<br>Agency: ", agency)
      )
  }

  # Add Illinois state boundary outline (reference line, always visible)
  cat("Adding Illinois boundary...\n")
  map <- map %>%
    addPolylines(
      data = illinois_boundary,
      color = "#666666",
      weight = 2,
      opacity = 0.8,
      fill = FALSE
    )

  # Add layer controls
  cat("Adding layer controls...\n")
  map <- map %>%
    addLayersControl(
      baseGroups = c("CartoDB.Positron"),
      overlayGroups = c(
        "Combined Parking Relief (Hubs + Corridors)",
        "Hub Buffers (1/2 mile)",
        "Corridor Buffers (1/8 mile)",
        "Corridor Segments (Qualifying)",
        "Hub Points",
        "All Bus Routes",
        "Bus Stops"
      ),
      options = layersControlOptions(collapsed = FALSE)
    ) %>%

    # Hide all except combined parking relief and hub points
    hideGroup(c(
      "Hub Buffers (1/2 mile)",
      "Corridor Buffers (1/8 mile)",
      "Corridor Segments (Qualifying)",
      "All Bus Routes",
      "Bus Stops"
    ))

  # Add legend
  cat("Adding legend...\n")
  map <- map %>%
    addLegend(
      position = "bottomright",
      colors = c(COLOR_PARKING_RELIEF, COLOR_HUB_BUFFER, COLOR_CORRIDOR_BUFFER,
                 COLOR_CORRIDOR_SEGMENTS, COLOR_HUB_POINTS, COLOR_ALL_ROUTES, COLOR_BUS_STOPS),
      labels = c(
        "Combined Parking Relief",
        "Hub Buffers (1/2 mi)",
        "Corridor Buffers (1/8 mi)",
        "Corridor Segments",
        "Transit Hubs",
        "All Bus Routes",
        "Bus Stops"
      ),
      title = "Transit-Oriented Areas",
      opacity = 0.7
    ) %>%

    # Add controls
    addFullscreenControl() %>%
    addMeasure(
      position = "bottomleft",
      primaryLengthUnit = "miles",
      primaryAreaUnit = "sqmiles",
      activeColor = "#3D535D",
      completedColor = "#7D4479"
    )

  cat("Interactive map created successfully\n\n")
  cat("Default view: Combined Parking Relief + Hub Points\n")
  cat("7 toggleable layers available\n")

  return(map)
}
