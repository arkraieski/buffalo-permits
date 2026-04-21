site_config <- list(
  site = list(
    title = "Buffalo Permits + Crime Tracker",
    description = "A daily-updated dashboard tracking recent permits and crime incidents in Buffalo.",
    site_url = "https://arkraieski.github.io/buffalo-permits/",
    image = "opengraph.png",
    image_alt = "Open Graph preview for the Buffalo Permits and Crime Tracker showing the latest reporting window and key metrics.",
    hero_image = "assets/buffalo-public-domain-photos/aerial-downtown-buffalo-2018.jpg",
    bottom_primary_image = "assets/buffalo-public-domain-photos/aerial-buffalo-central-terminal-2018.jpg",
    bottom_archive_image = "assets/buffalo-public-domain-photos/grain-boats-and-grain-elevators-erie-canal-1943.jpg"
  ),
  timezone = "America/New_York",
  window_days = 30L,
  datasets = list(
    crime = "d6g9-xbgu",
    permits = "9p2d-f3yt",
    neighborhoods = "ekfg-mtu8"
  ),
  data_paths = list(
    neighborhoods = "data/neighborhoods.gpkg"
  ),
  map = list(
    center = c(lng = -78.8784, lat = 42.8864),
    zoom = 12,
    min_zoom = 10,
    max_zoom = 18
  ),
  palette = list(
    crime_points = "#d1495b",
    permit_points = "#2c7c7b",
    demolition_points = "#7c3aed",
    crime_fill = c("#fff4ef", "#f6b89d", "#d1495b"),
    permit_fill = c("#eef8f7", "#90d1c7", "#2c7c7b"),
    demolition_fill = c("#f5f0ff", "#c4b5fd", "#7c3aed")
  ),
  source_urls = list(
    crime = "https://data.buffalony.gov/d/d6g9-xbgu",
    permits = "https://data.buffalony.gov/d/9p2d-f3yt",
    neighborhoods = "https://data.buffalony.gov/d/ekfg-mtu8"
  ),
  network = list(
    request_timeout_seconds = 120,
    max_retries = 3,
    max_retry_wait_seconds = 45
  )
)

ny_now <- function() {
  lubridate::with_tz(Sys.time(), tzone = site_config$timezone)
}

build_reporting_window <- function(reference_time = ny_now(), days = site_config$window_days) {
  end_date <- as.Date(reference_time, tz = site_config$timezone)
  start_date <- end_date - lubridate::days(days - 1L)

  list(
    generated_at = reference_time,
    start_date = start_date,
    end_date = end_date,
    start_datetime = as.POSIXct(
      paste(start_date, "00:00:00"),
      tz = site_config$timezone
    ),
    end_datetime_exclusive = as.POSIXct(
      paste(end_date + 1L, "00:00:00"),
      tz = site_config$timezone
    )
  )
}

shift_reporting_window_years <- function(window, years = -1L) {
  shifted_start_date <- lubridate::`%m+%`(window$start_date, lubridate::years(years))
  shifted_end_date <- lubridate::`%m+%`(window$end_date, lubridate::years(years))
  shifted_generated_at <- lubridate::`%m+%`(
    lubridate::with_tz(window$generated_at, tzone = site_config$timezone),
    lubridate::years(years)
  )

  list(
    generated_at = shifted_generated_at,
    start_date = shifted_start_date,
    end_date = shifted_end_date,
    start_datetime = as.POSIXct(
      paste(shifted_start_date, "00:00:00"),
      tz = site_config$timezone
    ),
    end_datetime_exclusive = as.POSIXct(
      paste(shifted_end_date + 1L, "00:00:00"),
      tz = site_config$timezone
    )
  )
}

format_date_label <- function(x) {
  format(as.Date(x), "%b %d, %Y")
}

format_datetime_label <- function(x) {
  ifelse(
    is.na(x),
    "Unknown",
    format(lubridate::with_tz(x, tzone = site_config$timezone), "%b %d, %Y %I:%M %p")
  )
}

format_number_label <- function(x) {
  scales::comma(x %||% 0)
}

format_currency_label <- function(x) {
  scales::dollar(x %||% 0, accuracy = 1)
}

normalize_name <- function(x) {
  cleaned <- x |>
    as.character() |>
    trimws() |>
    toupper()

  trimws(gsub("[^A-Z0-9]+", " ", cleaned))
}

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) y else x
}

safe_sum <- function(x) {
  if (length(x) == 0) {
    return(0)
  }

  sum(x, na.rm = TRUE)
}

safe_mean <- function(x) {
  values <- x[!is.na(x)]
  if (length(values) == 0) {
    return(NA_real_)
  }

  mean(values)
}

compute_yoy_ratio <- function(current, previous) {
  if (is.na(previous) || previous == 0) {
    return(NA_real_)
  }

  (current - previous) / previous
}

format_yoy_change_label <- function(current, previous) {
  if (is.na(previous) || previous == 0) {
    if (current == 0) {
      return("0.0% YoY")
    }

    return("No YoY")
  }

  delta_ratio <- compute_yoy_ratio(current, previous)
  sprintf(
    "%s%s YoY",
    if (delta_ratio > 0) "+" else if (delta_ratio < 0) "-" else "",
    scales::percent(abs(delta_ratio), accuracy = 0.1)
  )
}

build_kpi_cards <- function(kpis) {
  htmltools::tags$div(
    class = "kpi-grid",
    lapply(seq_len(nrow(kpis)), function(i) {
      row <- kpis[i, ]
      htmltools::tags$article(
        class = "kpi-card",
        htmltools::tags$div(class = "kpi-label", row$label),
        htmltools::tags$div(
          class = "kpi-value-row",
          htmltools::tags$div(class = "kpi-value", row$value),
          if ("delta" %in% names(row) && nzchar(row$delta[[1]])) {
            htmltools::tags$span(
              class = paste("kpi-delta", row$delta_class[[1]] %||% "flat"),
              row$delta[[1]]
            )
          }
        ),
        if ("note" %in% names(row) && nzchar(row$note[[1]])) {
          htmltools::tags$p(
            class = "kpi-note",
            row$note[[1]]
          )
        }
      )
    })
  )
}
