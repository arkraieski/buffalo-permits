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

build_leaflet_options <- function() {
  leaflet::leafletOptions(
    zoomControl = TRUE,
    minZoom = site_config$map$min_zoom,
    maxZoom = site_config$map$max_zoom
  )
}

build_legend_breaks <- function(values, bins = 7L, symmetric = FALSE) {
  finite_values <- values[is.finite(values)]

  if (length(finite_values) == 0) {
    return(0)
  }

  if (symmetric) {
    limit <- max(abs(finite_values), na.rm = TRUE)

    if (!is.finite(limit) || limit == 0) {
      return(0)
    }

    breaks <- pretty(c(-limit, limit), n = bins)
    breaks <- breaks[breaks >= -limit & breaks <= limit]
    breaks <- sort(unique(c(-limit, breaks, limit)))
    return(breaks)
  }

  value_range <- range(finite_values, na.rm = TRUE)

  if (diff(value_range) == 0) {
    return(value_range[[1]])
  }

  breaks <- pretty(value_range, n = bins)
  breaks <- breaks[breaks >= value_range[[1]] & breaks <= value_range[[2]]]
  sort(unique(c(value_range[[1]], breaks, value_range[[2]])))
}

add_descending_numeric_legend <- function(map, palette, values, title, class_name = "info legend", symmetric = FALSE, formatter = format_number_label) {
  finite_values <- values[is.finite(values)]
  breaks <- build_legend_breaks(values, symmetric = symmetric)
  display_breaks <- rev(breaks)

  leaflet::addLegend(
    map,
    position = "bottomright",
    colors = unname(palette(display_breaks)),
    labels = vapply(display_breaks, formatter, character(1)),
    opacity = 0.85,
    title = title,
    className = class_name
  )
}

make_leaflet_responsive <- function(map) {
  htmlwidgets::onRender(
    map,
    "function(el, x) {
      var map = this;
      function refresh() {
        window.setTimeout(function() {
          map.invalidateSize(true);
        }, 80);
      }
      refresh();
      window.addEventListener('resize', refresh, { passive: true });
      if (window.ResizeObserver) {
        var target = el.closest('.main-map-frame, .support-map-frame') || el.parentElement || el;
        new ResizeObserver(refresh).observe(target);
      }
    }"
  )
}

add_main_map_legends <- function(map, crime_palette, crime_values, permit_palette, permit_values, demolition_palette, demolition_values, crime_yoy_palette, crime_yoy_values) {
  map |>
    add_descending_numeric_legend(
      palette = crime_palette,
      values = crime_values,
      title = "Neighborhood crime counts",
      class_name = "info legend legend-crime"
    ) |>
    add_descending_numeric_legend(
      palette = permit_palette,
      values = permit_values,
      title = "Neighborhood permit counts",
      class_name = "info legend legend-permits"
    ) |>
    add_descending_numeric_legend(
      palette = crime_yoy_palette,
      values = crime_yoy_values,
      title = "Crime YoY count change",
      class_name = "info legend legend-crime-yoy",
      symmetric = TRUE,
      formatter = function(x) {
        sprintf("%s%s", if (x > 0) "+" else "", format_number_label(x))
      }
    )
}

add_main_map_legend_behavior <- function(map) {
  htmlwidgets::onRender(
    map,
    "function(el, x) {
      var map = this;
      var summaryGroups = [
        'Neighborhood crime counts',
        'Neighborhood crime YoY change',
        'Neighborhood permit counts',
        'Neighborhood demolition permits'
      ];

      function refreshSize() {
        window.setTimeout(function() {
          map.invalidateSize(true);
        }, 80);
      }

      function legendMap() {
        return {
          'Neighborhood crime counts': el.querySelector('.legend-crime'),
          'Neighborhood crime YoY change': el.querySelector('.legend-crime-yoy'),
          'Neighborhood permit counts': el.querySelector('.legend-permits'),
          'Neighborhood demolition permits': el.querySelector('.legend-demolition')
        };
      }

      function controlInputs() {
        var labels = el.querySelectorAll('.leaflet-control-layers-overlays label');
        var inputs = {};

        labels.forEach(function(label) {
          var input = label.querySelector('input');
          var span = label.querySelector('span');
          var text = span ? span.textContent : label.textContent;
          var cleanText = (text || '').replace(/^\\s+|\\s+$/g, '');

          if (input && cleanText) {
            inputs[cleanText] = input;
          }
        });

        return inputs;
      }

      function hideAllLegends() {
        var legends = legendMap();
        Object.keys(legends).forEach(function(key) {
          if (legends[key]) {
            legends[key].style.display = 'none';
          }
        });
      }

      function syncLegends() {
        hideAllLegends();
        var activeGroups = map.layerManager.getVisibleGroups();
        var legends = legendMap();

        activeGroups.forEach(function(groupName) {
          if (legends[groupName]) {
            legends[groupName].style.display = 'block';
          }
        });
      }

      function enforceSingleSummary(groupName) {
        if (summaryGroups.indexOf(groupName) === -1) {
          return;
        }

        var inputs = controlInputs();

        summaryGroups.forEach(function(otherGroup) {
          if (otherGroup === groupName) {
            return;
          }

          var layerGroup = map.layerManager.getLayerGroup(otherGroup, false);
          if (layerGroup && map.hasLayer(layerGroup)) {
            map.removeLayer(layerGroup);
          }

          if (inputs[otherGroup]) {
            inputs[otherGroup].checked = false;
          }
        });

        if (inputs[groupName]) {
          inputs[groupName].checked = true;
        }
      }

      function syncSummaryInputs() {
        var inputs = controlInputs();
        var activeGroups = map.layerManager.getVisibleGroups();

        summaryGroups.forEach(function(groupName) {
          if (inputs[groupName]) {
            inputs[groupName].checked = activeGroups.indexOf(groupName) !== -1;
          }
        });
      }

      refreshSize();
      syncLegends();
      syncSummaryInputs();

      window.addEventListener('resize', refreshSize, { passive: true });
      map.on('overlayadd', function(e) {
        var groupName = e.name || map.layerManager.getGroupNameFromLayerGroup(e.layer);
        enforceSingleSummary(groupName);
        syncLegends();
        syncSummaryInputs();
      });
      map.on('overlayremove', function() {
        syncLegends();
        syncSummaryInputs();
      });
      map.on('baselayerchange', refreshSize);

      if (window.ResizeObserver) {
        var target = el.closest('.main-map-frame, .support-map-frame') || el.parentElement || el;
        new ResizeObserver(refreshSize).observe(target);
      }
    }"
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
  crime_yoy_values <- summary_sf$crime_yoy_count_change
  permit_values <- summary_sf$permit_count
  demolition_values <- summary_sf$demolition_count
  has_demolition_data <- any(demolition_values > 0, na.rm = TRUE)

  crime_palette <- leaflet::colorNumeric(
    palette = site_config$palette$crime_fill,
    domain = crime_values,
    na.color = "#d8dbe6"
  )

  crime_yoy_limit <- max(abs(crime_yoy_values), na.rm = TRUE)
  if (!is.finite(crime_yoy_limit) || crime_yoy_limit == 0) {
    crime_yoy_limit <- 1
  }

  crime_yoy_palette <- leaflet::colorNumeric(
    palette = c("#1d4d91", "#f7f7f7", "#c44558"),
    domain = c(-crime_yoy_limit, crime_yoy_limit),
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

  overlay_groups <- c(
    "Crime incidents",
    "Permits",
    "Neighborhood crime counts",
    "Neighborhood crime YoY change",
    "Neighborhood permit counts"
  )

  map <- leaflet::leaflet(
    options = build_leaflet_options()
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
      group = "Neighborhood crime YoY change",
      color = "#ffffff",
      weight = 1,
      opacity = 1,
      fillOpacity = 0.72,
      fillColor = ~crime_yoy_palette(crime_yoy_count_change),
      popup = vapply(seq_len(nrow(summary_sf)), function(i) {
        rows <- tibble::tibble(
          label = c("Neighborhood", "Current crime count", "Prior-year crime count", "YoY count change", "YoY percent change"),
          value = c(
            summary_sf$neighborhood[[i]],
            format_number_label(summary_sf$crime_count[[i]]),
            format_number_label(summary_sf$prior_crime_count[[i]]),
            sprintf(
              "%s%s",
              if (summary_sf$crime_yoy_count_change[[i]] > 0) "+" else "",
              format_number_label(summary_sf$crime_yoy_count_change[[i]])
            ),
            if (is.na(summary_sf$crime_yoy_pct_change[[i]])) {
              "No prior-year baseline"
            } else {
              scales::percent(summary_sf$crime_yoy_pct_change[[i]], accuracy = 0.1)
            }
          )
        )

        build_popup_table(rows)
      }, character(1)),
      label = lapply(
        sprintf(
          "%s: %s%s vs last year",
          summary_sf$neighborhood,
          ifelse(summary_sf$crime_yoy_count_change > 0, "+", ""),
          format_number_label(summary_sf$crime_yoy_count_change)
        ),
        htmltools::HTML
      )
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
    leaflet::addLayersControl(
      baseGroups = c("Light", "Satellite", "Street"),
      overlayGroups = overlay_groups,
      options = leaflet::layersControlOptions(collapsed = TRUE, position = "topright")
    ) |>
    add_main_map_legends(
      crime_palette = crime_palette,
      crime_values = crime_values,
      permit_palette = permit_palette,
      permit_values = permit_values,
      demolition_palette = demolition_palette,
      demolition_values = demolition_values,
      crime_yoy_palette = crime_yoy_palette,
      crime_yoy_values = crime_yoy_values
    ) |>
    leaflet::hideGroup("Permits") |>
    leaflet::hideGroup("Neighborhood crime counts") |>
    leaflet::hideGroup("Neighborhood crime YoY change") |>
    leaflet::hideGroup("Neighborhood permit counts")

  if (has_demolition_data) {
    overlay_groups <- c(overlay_groups[1:2], "Demolition permits", overlay_groups[3:5], "Neighborhood demolition permits")

    map <- leaflet::clearControls(map) |>
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
        overlayGroups = overlay_groups,
        options = leaflet::layersControlOptions(collapsed = TRUE, position = "topright")
      ) |>
      add_main_map_legends(
        crime_palette = crime_palette,
        crime_values = crime_values,
        permit_palette = permit_palette,
        permit_values = permit_values,
        demolition_palette = demolition_palette,
        demolition_values = demolition_values,
        crime_yoy_palette = crime_yoy_palette,
        crime_yoy_values = crime_yoy_values
      ) |>
      leaflet::hideGroup("Permits") |>
      leaflet::hideGroup("Demolition permits") |>
      leaflet::hideGroup("Neighborhood crime counts") |>
      leaflet::hideGroup("Neighborhood crime YoY change") |>
      leaflet::hideGroup("Neighborhood permit counts") |>
      leaflet::hideGroup("Neighborhood demolition permits")
  }

  add_main_map_legend_behavior(map)
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

  map <- leaflet::leaflet(data, options = build_leaflet_options()) |>
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
    add_descending_numeric_legend(
      palette = pal,
      values = values,
      title = title
    )

  make_leaflet_responsive(map)
}
