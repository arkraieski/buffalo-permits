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
