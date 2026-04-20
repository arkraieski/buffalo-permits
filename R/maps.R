build_popup_table <- function(rows) {
  htmltools::tags$div(
    class = "popup-card",
    lapply(seq_len(nrow(rows)), function(i) {
      htmltools::tags$div(
        class = "popup-row",
        htmltools::tags$span(class = "popup-label", rows$label[[i]]),
        htmltools::tags$span(class = "popup-value", rows$value[[i]])
      )
    })
  ) |>
    as.character()
}

crime_popup_html <- function(data) {
  vapply(seq_len(nrow(data)), function(i) {
    rows <- tibble::tibble(
      label = c("Type", "Description", "Address", "When", "Neighborhood", "Case"),
      value = c(
        data$category[[i]] %||% "Unknown",
        data$detail[[i]] %||% "No description",
        data$address[[i]] %||% "Address unavailable",
        format_datetime_label(data$incident_datetime[[i]]),
        data$neighborhood[[i]] %||% "Unknown",
        data$source_id[[i]] %||% "Unknown"
      )
    )

    build_popup_table(rows)
  }, character(1))
}

permit_popup_html <- function(data) {
  vapply(seq_len(nrow(data)), function(i) {
    rows <- tibble::tibble(
      label = c("Permit type", "Address", "Issued", "Value of work", "Fees", "Neighborhood"),
      value = c(
        data$category[[i]] %||% "Unknown",
        data$address[[i]] %||% "Address unavailable",
        format_datetime_label(data$issued[[i]]),
        format_currency_label(data$value[[i]]),
        format_currency_label(data$fees[[i]]),
        data$neighborhood[[i]] %||% "Unknown"
      )
    )

    build_popup_table(rows)
  }, character(1))
}

neighborhood_popup_html <- function(data, count_col, count_label, value_col = NULL, value_label = NULL) {
  vapply(seq_len(nrow(data)), function(i) {
    labels <- c("Neighborhood", count_label)
    values <- c(
      data$neighborhood[[i]],
      format_number_label(data[[count_col]][[i]])
    )

    if (!is.null(value_col) && !is.null(value_label)) {
      labels <- c(labels, value_label)
      values <- c(values, format_currency_label(data[[value_col]][[i]]))
    }

    rows <- tibble::tibble(label = labels, value = values)
    build_popup_table(rows)
  }, character(1))
}

add_basemap_layers <- function(map) {
  map |>
    leaflet::addProviderTiles(
      leaflet::providers$CartoDB.Positron,
      group = "Light"
    ) |>
    leaflet::addProviderTiles(
      leaflet::providers$Esri.WorldImagery,
      group = "Satellite"
    ) |>
    leaflet::addProviderTiles(
      leaflet::providers$Esri.WorldStreetMap,
      group = "Street"
    )
}

make_main_map <- function(site_data) {
  summary_sf <- site_data$neighborhood_summary
  crime_popups <- neighborhood_popup_html(summary_sf, "crime_count", "Crime incidents")
  permit_popups <- neighborhood_popup_html(
    summary_sf,
    "permit_count",
    "Permits",
    value_col = "value_sum",
    value_label = "Declared value"
  )
  demolition_popups <- neighborhood_popup_html(
    summary_sf,
    "demolition_count",
    "Demolition permits"
  )
  crime_labels <- sprintf(
    "%s: %s incidents",
    summary_sf$neighborhood,
    format_number_label(summary_sf$crime_count)
  )
  permit_labels <- sprintf(
    "%s: %s permits",
    summary_sf$neighborhood,
    format_number_label(summary_sf$permit_count)
  )
  crime_point_popups <- crime_popup_html(site_data$crime_sf)
  permit_point_popups <- permit_popup_html(site_data$permit_sf)
  demolition_point_popups <- permit_popup_html(site_data$demolition_permit_sf)
  crime_labels <- lapply(crime_labels, htmltools::HTML)
  permit_labels <- lapply(permit_labels, htmltools::HTML)
  demolition_labels <- lapply(
    sprintf(
      "%s: %s demolition permits",
      summary_sf$neighborhood,
      format_number_label(summary_sf$demolition_count)
    ),
    htmltools::HTML
  )

  crime_values <- summary_sf$crime_count
  permit_values <- summary_sf$permit_count
  demolition_values <- summary_sf$demolition_count

  crime_palette <- leaflet::colorNumeric(
    palette = site_config$palette$crime_fill,
    domain = crime_values,
    na.color = "#d8dbe6"
  )

  permit_palette <- leaflet::colorNumeric(
    palette = site_config$palette$permit_fill,
    domain = permit_values,
    na.color = "#d8dbe6"
  )

  demolition_palette <- leaflet::colorNumeric(
    palette = site_config$palette$demolition_fill,
    domain = demolition_values,
    na.color = "#d8dbe6"
  )

  map <- leaflet::leaflet(
    options = leaflet::leafletOptions(zoomControl = TRUE, minZoom = 10)
  ) |>
    add_basemap_layers() |>
    leaflet::setView(
      lng = unname(site_config$map$center[["lng"]]),
      lat = unname(site_config$map$center[["lat"]]),
      zoom = site_config$map$zoom
    ) |>
    leaflet::addPolygons(
      data = summary_sf,
      group = "Neighborhood crime counts",
      color = "#ffffff",
      weight = 1,
      opacity = 1,
      fillOpacity = 0.68,
      fillColor = ~crime_palette(crime_count),
      popup = crime_popups,
      label = crime_labels
    ) |>
    leaflet::addPolygons(
      data = summary_sf,
      group = "Neighborhood permit counts",
      color = "#ffffff",
      weight = 1,
      opacity = 1,
      fillOpacity = 0.68,
      fillColor = ~permit_palette(permit_count),
      popup = permit_popups,
      label = permit_labels
    ) |>
    leaflet::addPolygons(
      data = summary_sf,
      group = "Neighborhood demolition permits",
      color = "#ffffff",
      weight = 1,
      opacity = 1,
      fillOpacity = 0.72,
      fillColor = ~demolition_palette(demolition_count),
      popup = demolition_popups,
      label = demolition_labels
    ) |>
    leaflet::addCircleMarkers(
      data = site_data$crime_sf,
      group = "Crime incidents",
      radius = 5,
      stroke = TRUE,
      weight = 1,
      color = "#7f1d2d",
      fillColor = site_config$palette$crime_points,
      fillOpacity = 0.9,
      popup = crime_point_popups,
      clusterOptions = leaflet::markerClusterOptions(
        showCoverageOnHover = FALSE,
        spiderfyOnMaxZoom = TRUE
      )
    ) |>
    leaflet::addCircleMarkers(
      data = site_data$permit_sf,
      group = "Permits",
      radius = 5,
      stroke = TRUE,
      weight = 1,
      color = "#164544",
      fillColor = site_config$palette$permit_points,
      fillOpacity = 0.9,
      popup = permit_point_popups,
      clusterOptions = leaflet::markerClusterOptions(
        showCoverageOnHover = FALSE,
        spiderfyOnMaxZoom = TRUE
      )
    ) |>
    leaflet::addCircleMarkers(
      data = site_data$demolition_permit_sf,
      group = "Demolition permits",
      radius = 6,
      stroke = TRUE,
      weight = 1,
      color = "#4c1d95",
      fillColor = site_config$palette$demolition_points,
      fillOpacity = 0.95,
      popup = demolition_point_popups,
      clusterOptions = leaflet::markerClusterOptions(
        showCoverageOnHover = FALSE,
        spiderfyOnMaxZoom = TRUE
      )
    ) |>
    leaflet::addLayersControl(
      baseGroups = c("Light", "Satellite", "Street"),
      overlayGroups = c(
        "Crime incidents",
        "Permits",
        "Demolition permits",
        "Neighborhood crime counts",
        "Neighborhood permit counts",
        "Neighborhood demolition permits"
      ),
      options = leaflet::layersControlOptions(collapsed = FALSE, position = "topright")
    ) |>
    leaflet::hideGroup("Permits") |>
    leaflet::hideGroup("Demolition permits") |>
    leaflet::hideGroup("Neighborhood crime counts") |>
    leaflet::hideGroup("Neighborhood permit counts") |>
    leaflet::hideGroup("Neighborhood demolition permits")

  map
}

make_summary_map <- function(data, value_col, title, palette, value_format = format_number_label) {
  values <- data[[value_col]]
  labels <- sprintf("%s: %s", data$neighborhood, value_format(values))
  pal <- leaflet::colorNumeric(
    palette = palette,
    domain = values,
    na.color = "#d8dbe6"
  )

  popup_rows <- lapply(seq_len(nrow(data)), function(i) {
    tibble::tibble(
      label = c("Neighborhood", title),
      value = c(data$neighborhood[[i]], value_format(data[[value_col]][[i]]))
    )
  })

  leaflet::leaflet(data) |>
    leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) |>
    leaflet::setView(
      lng = unname(site_config$map$center[["lng"]]),
      lat = unname(site_config$map$center[["lat"]]),
      zoom = site_config$map$zoom
    ) |>
    leaflet::addPolygons(
      color = "#ffffff",
      weight = 1,
      fillOpacity = 0.78,
      fillColor = pal(values),
      popup = vapply(popup_rows, build_popup_table, character(1)),
      label = labels
    ) |>
    leaflet::addLegend(
      position = "bottomright",
      pal = pal,
      values = values,
      title = title,
      opacity = 0.85
    )
}
