prepare_neighborhoods <- function(neighborhoods_raw) {
  sf::st_geometry(neighborhoods_raw) <- sf::st_geometry(neighborhoods_raw)

  neighborhoods_raw |>
    dplyr::transmute(
      neighborhood = nbhdname,
      neighborhood_key = normalize_name(nbhdname),
      geometry = sf::st_geometry(neighborhoods_raw)
    )
}

validate_required_columns <- function(data, required_columns, dataset_label) {
  missing_columns <- setdiff(required_columns, names(data))
  if (length(missing_columns) > 0) {
    stop(
      sprintf(
        "%s is missing expected columns: %s",
        dataset_label,
        paste(missing_columns, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  invisible(data)
}

parse_socrata_datetime <- function(x) {
  if (inherits(x, "POSIXct")) {
    return(lubridate::with_tz(x, tzone = site_config$timezone))
  }

  if (inherits(x, "Date")) {
    return(as.POSIXct(x, tz = site_config$timezone))
  }

  parsed <- suppressWarnings(
    lubridate::parse_date_time(
      as.character(x),
      orders = c(
        "Ymd HMS", "Ymd HM", "Ymd",
        "ymd HMS", "ymd HM", "ymd",
        "mdY HMS", "mdY HM", "mdY"
      ),
      tz = site_config$timezone,
      quiet = TRUE
    )
  )

  as.POSIXct(parsed, tz = site_config$timezone)
}

attach_neighborhoods <- function(points_sf, neighborhoods) {
  if (nrow(points_sf) == 0) {
    points_sf$joined_neighborhood <- character(0)
    points_sf$neighborhood_key <- character(0)
    return(points_sf)
  }

  joined <- sf::st_join(
    points_sf,
    neighborhoods |> dplyr::select(joined_neighborhood = neighborhood, joined_key = neighborhood_key),
    left = TRUE,
    join = sf::st_within
  )

  missing_idx <- which(is.na(joined$joined_key))
  if (length(missing_idx) > 0) {
    nearest_idx <- sf::st_nearest_feature(joined[missing_idx, ], neighborhoods)
    joined$joined_neighborhood[missing_idx] <- neighborhoods$neighborhood[nearest_idx]
    joined$joined_key[missing_idx] <- neighborhoods$neighborhood_key[nearest_idx]
  }

  joined |>
    dplyr::mutate(
      neighborhood = dplyr::coalesce(joined_neighborhood, neighborhood),
      neighborhood_key = dplyr::coalesce(joined_key, normalize_name(neighborhood))
    ) |>
    dplyr::select(-joined_neighborhood, -joined_key)
}

prepare_crime_data <- function(crime_raw, neighborhoods) {
  points <- crime_raw |>
    dplyr::mutate(
      incident_datetime = parse_socrata_datetime(incident_datetime),
      latitude = readr::parse_number(as.character(latitude)),
      longitude = readr::parse_number(as.character(longitude)),
      neighborhood = dplyr::na_if(trimws(as.character(neighborhood)), ""),
      category = dplyr::na_if(trimws(as.character(incident_type_primary)), ""),
      detail = dplyr::na_if(trimws(as.character(incident_description)), ""),
      address = dplyr::na_if(trimws(as.character(address_1)), "")
    ) |>
    dplyr::filter(!is.na(latitude), !is.na(longitude)) |>
    sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = FALSE) |>
    attach_neighborhoods(neighborhoods) |>
    dplyr::mutate(
      source_id = case_number,
      event_date = as.Date(incident_datetime, tz = site_config$timezone)
    )

  points
}

prepare_permit_data <- function(permits_raw, neighborhoods) {
  points <- permits_raw |>
    dplyr::mutate(
      issued = parse_socrata_datetime(issued),
      latitude = readr::parse_number(as.character(latitude)),
      longitude = readr::parse_number(as.character(longitude)),
      fees = readr::parse_number(as.character(fees)),
      value = readr::parse_number(as.character(value)),
      neighborhood = dplyr::na_if(trimws(as.character(neighborhood)), ""),
      category = dplyr::na_if(trimws(as.character(aptype)), ""),
      detail = dplyr::na_if(trimws(as.character(descofwork)), ""),
      address = dplyr::na_if(trimws(as.character(stname)), "")
    ) |>
    dplyr::filter(!is.na(latitude), !is.na(longitude)) |>
    sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = FALSE) |>
    attach_neighborhoods(neighborhoods) |>
    dplyr::mutate(
      source_id = apno,
      event_date = as.Date(issued, tz = site_config$timezone),
      is_demolition = category == "DEMOLITION"
    )

  points
}

summarize_neighborhood_counts <- function(neighborhoods, points_sf, count_name, value_name = NULL) {
  if (nrow(points_sf) == 0) {
    summary_tbl <- neighborhoods |>
      sf::st_drop_geometry() |>
      dplyr::transmute(
        neighborhood_key,
        count_value = 0,
        secondary_value = 0
      )
  } else {
    base_tbl <- points_sf |>
      sf::st_drop_geometry() |>
      dplyr::count(neighborhood_key, name = "count_value")

    if (!is.null(value_name)) {
      value_tbl <- points_sf |>
        sf::st_drop_geometry() |>
        dplyr::group_by(neighborhood_key) |>
        dplyr::summarise(secondary_value = safe_sum(.data[[value_name]]), .groups = "drop")

      summary_tbl <- dplyr::left_join(base_tbl, value_tbl, by = "neighborhood_key")
    } else {
      summary_tbl <- dplyr::mutate(base_tbl, secondary_value = 0)
    }
  }

  result <- neighborhoods |>
    dplyr::left_join(summary_tbl, by = "neighborhood_key") |>
    dplyr::mutate(
      count_value = dplyr::coalesce(count_value, 0L),
      secondary_value = dplyr::coalesce(secondary_value, 0)
    )

  names(result)[names(result) == "count_value"] <- count_name
  names(result)[names(result) == "secondary_value"] <- if (is.null(value_name)) {
    "secondary_value"
  } else {
    paste0(value_name, "_sum")
  }

  result
}

compute_kpis <- function(crime_sf, permit_sf) {
  tibble::tibble(
    label = c(
      "Crime incidents",
      "Permits issued",
      "Declared permit value",
      "Average permits per day"
    ),
    value = c(
      format_number_label(nrow(crime_sf)),
      format_number_label(nrow(permit_sf)),
      format_currency_label(safe_sum(permit_sf$value)),
      format_number_label(round(nrow(permit_sf) / site_config$window_days, 1))
    ),
    note = c(
      "Reported incidents in the rolling 30-day window.",
      "Issued permits in the same reporting window.",
      "Total declared value of work across 30-day permits.",
      sprintf("Based on a %s-day rolling window.", site_config$window_days)
    )
  )
}

build_site_data <- function() {
  window <- build_reporting_window()
  neighborhoods <- prepare_neighborhoods(fetch_neighborhood_boundaries())
  crime_raw <- fetch_crime_raw(window)
  permits_raw <- fetch_permits_raw(window)

  validate_required_columns(
    crime_raw,
    c(
      "case_number", "incident_datetime", "incident_id", "incident_type_primary",
      "incident_description", "address_1", "latitude", "longitude", "neighborhood"
    ),
    "Crime dataset"
  )

  validate_required_columns(
    permits_raw,
    c(
      "apno", "aptype", "issued", "stname", "fees", "value",
      "descofwork", "latitude", "longitude", "neighborhood"
    ),
    "Permits dataset"
  )

  crime_sf <- prepare_crime_data(crime_raw, neighborhoods)
  permit_sf <- prepare_permit_data(permits_raw, neighborhoods)

  crime_neighborhoods <- summarize_neighborhood_counts(
    neighborhoods,
    crime_sf,
    count_name = "crime_count"
  )

  permit_neighborhoods <- summarize_neighborhood_counts(
    neighborhoods,
    permit_sf,
    count_name = "permit_count",
    value_name = "value"
  )

  demolition_permit_sf <- permit_sf |>
    dplyr::filter(is_demolition)

  demolition_neighborhoods <- summarize_neighborhood_counts(
    neighborhoods,
    demolition_permit_sf,
    count_name = "demolition_count"
  )

  neighborhood_summary <- crime_neighborhoods |>
    dplyr::left_join(
      permit_neighborhoods |>
        sf::st_drop_geometry() |>
        dplyr::select(neighborhood_key, permit_count, value_sum),
      by = "neighborhood_key"
    ) |>
    dplyr::left_join(
      demolition_neighborhoods |>
        sf::st_drop_geometry() |>
        dplyr::select(neighborhood_key, demolition_count),
      by = "neighborhood_key"
    ) |>
    dplyr::mutate(
      permit_count = dplyr::coalesce(permit_count, 0L),
      value_sum = dplyr::coalesce(value_sum, 0),
      demolition_count = dplyr::coalesce(demolition_count, 0L)
    )

  top_crime_types <- crime_sf |>
    sf::st_drop_geometry() |>
    dplyr::count(category, sort = TRUE, name = "n") |>
    dplyr::slice_head(n = 5)

  top_permit_types <- permit_sf |>
    sf::st_drop_geometry() |>
    dplyr::count(category, sort = TRUE, name = "n") |>
    dplyr::slice_head(n = 5)

  list(
    window = window,
    neighborhoods = neighborhoods,
    crime_sf = crime_sf,
    permit_sf = permit_sf,
    demolition_permit_sf = demolition_permit_sf,
    neighborhood_summary = neighborhood_summary,
    crime_neighborhoods = crime_neighborhoods,
    permit_neighborhoods = permit_neighborhoods,
    demolition_neighborhoods = demolition_neighborhoods,
    kpis = compute_kpis(crime_sf, permit_sf),
    top_crime_types = top_crime_types,
    top_permit_types = top_permit_types
  )
}
