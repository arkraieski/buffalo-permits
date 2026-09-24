build_daily_counts <- function(data, window) {
  data |>
    st_drop_geometry() |>
    count(event_date, name = "total") |>
    right_join(
      tibble(
        event_date = seq(window$start_date, window$end_date, by = "day")
      ),
      by = "event_date"
    ) |>
    mutate(total = coalesce(total, 0L)) |>
    arrange(event_date)
}

build_daily_totals_alt_text <- function(data, series_name, window_label) {
  peak_row <- data |>
    filter(total == max(total, na.rm = TRUE)) |>
    arrange(event_date) |>
    slice(1)

  low_row <- data |>
    filter(total == min(total, na.rm = TRUE)) |>
    arrange(event_date) |>
    slice(1)

  latest_row <- data |>
    slice_tail(n = 1)

  trend_direction <- case_when(
    nrow(data) < 2 ~ "flat",
    latest_row$total[[1]] > data$total[[1]] ~ "higher at the end of the period",
    latest_row$total[[1]] < data$total[[1]] ~ "lower at the end of the period",
    TRUE ~ "unchanged between the first and last day"
  )

  nonzero_days <- sum(data$total > 0, na.rm = TRUE)
  total_events <- sum(data$total, na.rm = TRUE)
  average_daily <- mean(data$total, na.rm = TRUE)

  paste(
    sprintf(
      "Bar chart of daily %s in Buffalo for %s.",
      series_name,
      window_label
    ),
    sprintf(
      "There are %s total %s across %s days, averaging %s per day.",
      format_number_label(total_events),
      series_name,
      format_number_label(nrow(data)),
      format(round(average_daily, 1), nsmall = 1)
    ),
    sprintf(
      "The highest daily count is %s on %s.",
      format_number_label(peak_row$total[[1]]),
      format_date_label(peak_row$event_date[[1]])
    ),
    sprintf(
      "The lowest daily count is %s on %s.",
      format_number_label(low_row$total[[1]]),
      format_date_label(low_row$event_date[[1]])
    ),
    sprintf(
      "%s of %s days have at least one recorded %s.",
      format_number_label(nonzero_days),
      format_number_label(nrow(data)),
      series_name
    ),
    sprintf(
      "The series ends at %s on %s and is %s.",
      format_number_label(latest_row$total[[1]]),
      format_date_label(latest_row$event_date[[1]]),
      trend_direction
    )
  )
}

make_daily_totals_chart <- function(data, fill_color, y_label) {
  axis_breaks <- seq(min(data$event_date), max(data$event_date), by = "5 days")

  ggplot(data, aes(x = event_date, y = total)) +
    geom_col(width = 0.82, fill = fill_color) +
    scale_x_date(
      breaks = axis_breaks,
      labels = label_date(format = "%b %d"),
      expand = expansion(mult = c(0.01, 0.01))
    ) +
    scale_y_continuous(
      labels = label_number(accuracy = 1),
      expand = expansion(mult = c(0, 0.06))
    ) +
    labs(x = "Date", y = y_label) +
    theme_minimal(base_size = 11) +
    theme(
      plot.background = element_rect(fill = "transparent", color = NA),
      panel.background = element_rect(fill = "transparent", color = NA),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(color = "#dde4ec", linewidth = 0.5),
      axis.line.x = element_line(color = "#b9c4d0", linewidth = 0.5),
      axis.title.x = element_text(size = 10, color = "#44515f", margin = margin(t = 10)),
      axis.text.x = element_text(
        angle = 45,
        hjust = 1,
        vjust = 1,
        size = 8.5,
        color = "#44515f"
      ),
      axis.text.y = element_text(size = 9, color = "#44515f"),
      axis.title.y = element_text(size = 10, color = "#44515f", margin = margin(r = 10)),
      plot.margin = margin(12, 12, 28, 8)
    ) +
    coord_cartesian(clip = "off")
}

format_hour_label <- function(hour) {
  dplyr::case_when(
    hour == 0L ~ "12a",
    hour < 12L ~ paste0(hour, "a"),
    hour == 12L ~ "12p",
    TRUE ~ paste0(hour - 12L, "p")
  )
}

build_crime_hourly_counts <- function(data) {
  data |>
    sf::st_drop_geometry() |>
    dplyr::filter(!is.na(incident_hour)) |>
    dplyr::count(incident_hour, name = "total") |>
    dplyr::right_join(
      tibble::tibble(incident_hour = 0:23),
      by = "incident_hour"
    ) |>
    dplyr::mutate(
      total = dplyr::coalesce(total, 0L),
      hour_label = format_hour_label(incident_hour)
    ) |>
    dplyr::arrange(incident_hour)
}

build_crime_hourly_alt_text <- function(data, window_label) {
  total_events <- sum(data$total, na.rm = TRUE)
  peak_row <- data |>
    dplyr::filter(total == max(total, na.rm = TRUE)) |>
    dplyr::arrange(incident_hour) |>
    dplyr::slice(1)

  quiet_row <- data |>
    dplyr::filter(total == min(total, na.rm = TRUE)) |>
    dplyr::arrange(incident_hour) |>
    dplyr::slice(1)

  paste(
    sprintf("Bar chart of Buffalo crime incidents by hour of day for %s.", window_label),
    sprintf(
      "There are %s total incidents with usable incident times.",
      format_number_label(total_events)
    ),
    sprintf(
      "The busiest hour is %s with %s incidents.",
      peak_row$hour_label[[1]],
      format_number_label(peak_row$total[[1]])
    ),
    sprintf(
      "The quietest hour is %s with %s incidents.",
      quiet_row$hour_label[[1]],
      format_number_label(quiet_row$total[[1]])
    )
  )
}

make_crime_hourly_chart <- function(data) {
  axis_breaks <- seq(0, 23, by = 3)

  ggplot2::ggplot(data, ggplot2::aes(x = incident_hour, y = total)) +
    ggplot2::geom_col(width = 0.82, fill = site_config$palette$crime_points) +
    ggplot2::scale_x_continuous(
      breaks = axis_breaks,
      labels = format_hour_label(axis_breaks),
      expand = ggplot2::expansion(add = c(0.35, 0.35))
    ) +
    ggplot2::scale_y_continuous(
      labels = scales::label_number(accuracy = 1),
      expand = ggplot2::expansion(mult = c(0, 0.08))
    ) +
    ggplot2::labs(x = "Hour of day", y = "Crime incidents") +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = "transparent", color = NA),
      panel.background = ggplot2::element_rect(fill = "transparent", color = NA),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(color = "#dde4ec", linewidth = 0.5),
      axis.line.x = ggplot2::element_line(color = "#b9c4d0", linewidth = 0.5),
      axis.title.x = ggplot2::element_text(size = 10, color = "#44515f", margin = ggplot2::margin(t = 10)),
      axis.text.x = ggplot2::element_text(size = 9, color = "#44515f"),
      axis.text.y = ggplot2::element_text(size = 9, color = "#44515f"),
      axis.title.y = ggplot2::element_text(size = 10, color = "#44515f", margin = ggplot2::margin(r = 10)),
      plot.margin = ggplot2::margin(12, 12, 16, 8)
    ) +
    ggplot2::coord_cartesian(clip = "off")
}

build_top_neighborhood_hourly_pattern <- function(data, top_n = 12L) {
  base <- data |>
    sf::st_drop_geometry() |>
    dplyr::filter(!is.na(incident_hour), !is.na(neighborhood), nzchar(neighborhood))

  if (nrow(base) == 0) {
    return(tibble::tibble(
      neighborhood = character(),
      incident_hour = integer(),
      incidents = integer(),
      neighborhood_total = integer(),
      share = numeric(),
      hour_label = character()
    ))
  }

  top_neighborhoods <- base |>
    dplyr::count(neighborhood, name = "neighborhood_total", sort = TRUE) |>
    dplyr::slice_head(n = top_n)

  tidyr::expand_grid(
    neighborhood = top_neighborhoods$neighborhood,
    incident_hour = 0:23
  ) |>
    dplyr::left_join(
      base |>
        dplyr::semi_join(top_neighborhoods, by = "neighborhood") |>
        dplyr::count(neighborhood, incident_hour, name = "incidents"),
      by = c("neighborhood", "incident_hour")
    ) |>
    dplyr::left_join(top_neighborhoods, by = "neighborhood") |>
    dplyr::mutate(
      incidents = dplyr::coalesce(incidents, 0L),
      share = dplyr::if_else(neighborhood_total > 0, incidents / neighborhood_total, 0),
      hour_label = format_hour_label(incident_hour),
      neighborhood = factor(neighborhood, levels = rev(top_neighborhoods$neighborhood))
    )
}

build_neighborhood_hourly_alt_text <- function(data, window_label) {
  if (nrow(data) == 0) {
    return(sprintf("Heatmap of crime incidents by neighborhood and hour of day for %s. No usable incident times are available.", window_label))
  }

  peak_row <- data |>
    dplyr::filter(share == max(share, na.rm = TRUE)) |>
    dplyr::arrange(neighborhood, incident_hour) |>
    dplyr::slice(1)

  paste(
    sprintf(
      "Heatmap of crime incidents by hour of day for the %s neighborhoods with the most incidents in %s.",
      format_number_label(dplyr::n_distinct(data$neighborhood)),
      window_label
    ),
    "Color shows each hour's share of that neighborhood's incidents, so timing patterns can be compared across neighborhoods.",
    sprintf(
      "The strongest cell is %s at %s, representing %s of that neighborhood's incidents.",
      as.character(peak_row$neighborhood[[1]]),
      peak_row$hour_label[[1]],
      scales::percent(peak_row$share[[1]], accuracy = 1)
    )
  )
}

make_neighborhood_hourly_heatmap <- function(data) {
  axis_breaks <- seq(0, 23, by = 3)

  ggplot2::ggplot(data, ggplot2::aes(x = incident_hour, y = neighborhood, fill = share)) +
    ggplot2::geom_tile(color = "white", linewidth = 0.35) +
    ggplot2::scale_x_continuous(
      breaks = axis_breaks,
      labels = format_hour_label(axis_breaks),
      expand = c(0, 0)
    ) +
    ggplot2::scale_fill_gradient(
      low = site_config$palette$crime_fill[[1]],
      high = site_config$palette$crime_fill[[3]],
      labels = scales::label_percent(accuracy = 1),
      name = "Share"
    ) +
    ggplot2::labs(x = "Hour of day", y = NULL) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = "transparent", color = NA),
      panel.background = ggplot2::element_rect(fill = "transparent", color = NA),
      panel.grid = ggplot2::element_blank(),
      axis.title.x = ggplot2::element_text(size = 10, color = "#44515f", margin = ggplot2::margin(t = 10)),
      axis.text.x = ggplot2::element_text(size = 9, color = "#44515f"),
      axis.text.y = ggplot2::element_text(size = 12, color = "#44515f"),
      legend.position = "bottom",
      legend.title = ggplot2::element_text(size = 9, color = "#44515f"),
      legend.text = ggplot2::element_text(size = 8.5, color = "#44515f"),
      legend.key.width = grid::unit(1.8, "cm"),
      plot.margin = ggplot2::margin(12, 12, 16, 8)
    )
}

build_hourly_crime_chart_bundle <- function(site_data, window_label) {
  crime_hourly_counts <- build_crime_hourly_counts(site_data$crime_sf)
  neighborhood_hourly_pattern <- build_top_neighborhood_hourly_pattern(site_data$crime_sf)

  list(
    citywide = list(
      data = crime_hourly_counts,
      plot = make_crime_hourly_chart(crime_hourly_counts),
      alt_text = build_crime_hourly_alt_text(crime_hourly_counts, window_label)
    ),
    neighborhoods = list(
      data = neighborhood_hourly_pattern,
      plot = make_neighborhood_hourly_heatmap(neighborhood_hourly_pattern),
      alt_text = build_neighborhood_hourly_alt_text(neighborhood_hourly_pattern, window_label)
    )
  )
}

build_daily_chart_bundle <- function(site_data, window_label) {
  crime_daily_counts <- build_daily_counts(site_data$crime_sf, site_data$window)
  permit_daily_counts <- build_daily_counts(site_data$permit_sf, site_data$window)

  list(
    crime = list(
      data = crime_daily_counts,
      plot = make_daily_totals_chart(
        crime_daily_counts,
        fill_color = site_config$palette$crime_points,
        y_label = "Crime incidents"
      ),
      alt_text = build_daily_totals_alt_text(
        crime_daily_counts,
        series_name = "crime incidents",
        window_label = window_label
      )
    ),
    permits = list(
      data = permit_daily_counts,
      plot = make_daily_totals_chart(
        permit_daily_counts,
        fill_color = site_config$palette$permit_points,
        y_label = "Permits issued"
      ),
      alt_text = build_daily_totals_alt_text(
        permit_daily_counts,
        series_name = "permits",
        window_label = window_label
      )
    )
  )
}
