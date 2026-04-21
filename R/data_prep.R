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

compute_kpis <- function(crime_sf, permit_sf, prior_crime_sf, prior_permit_sf) {
  current_permit_value <- safe_sum(permit_sf$value)
  prior_permit_value <- safe_sum(prior_permit_sf$value)
  current_avg_permits <- nrow(permit_sf) / site_config$window_days
  prior_avg_permits <- nrow(prior_permit_sf) / site_config$window_days

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
      format_currency_label(current_permit_value),
      format_number_label(round(current_avg_permits, 1))
    ),
    note = c(
      "",
      "",
      "",
      ""
    ),
    delta = c(
      format_yoy_change_label(nrow(crime_sf), nrow(prior_crime_sf)),
      format_yoy_change_label(nrow(permit_sf), nrow(prior_permit_sf)),
      format_yoy_change_label(current_permit_value, prior_permit_value),
      format_yoy_change_label(current_avg_permits, prior_avg_permits)
    ),
    delta_class = c(
      dplyr::case_when(
        nrow(crime_sf) > nrow(prior_crime_sf) ~ "up",
        nrow(crime_sf) < nrow(prior_crime_sf) ~ "down",
        TRUE ~ "flat"
      ),
      dplyr::case_when(
        nrow(permit_sf) > nrow(prior_permit_sf) ~ "up",
        nrow(permit_sf) < nrow(prior_permit_sf) ~ "down",
        TRUE ~ "flat"
      ),
      dplyr::case_when(
        current_permit_value > prior_permit_value ~ "up",
        current_permit_value < prior_permit_value ~ "down",
        TRUE ~ "flat"
      ),
      dplyr::case_when(
        current_avg_permits > prior_avg_permits ~ "up",
        current_avg_permits < prior_avg_permits ~ "down",
        TRUE ~ "flat"
      )
    )
  )
}

build_site_data <- function() {
  window <- build_reporting_window()
  prior_year_window <- shift_reporting_window_years(window, years = -1L)
  neighborhoods <- prepare_neighborhoods(fetch_neighborhood_boundaries())
  crime_raw <- fetch_crime_raw(window)
  permits_raw <- fetch_permits_raw(window)
  prior_crime_raw <- fetch_crime_raw(prior_year_window)
  prior_permits_raw <- fetch_permits_raw(prior_year_window)

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

  validate_required_columns(
    prior_crime_raw,
    c(
      "case_number", "incident_datetime", "incident_id", "incident_type_primary",
      "incident_description", "address_1", "latitude", "longitude", "neighborhood"
    ),
    "Prior-year crime dataset"
  )

  validate_required_columns(
    prior_permits_raw,
    c(
      "apno", "aptype", "issued", "stname", "fees", "value",
      "descofwork", "latitude", "longitude", "neighborhood"
    ),
    "Prior-year permits dataset"
  )

  crime_sf <- prepare_crime_data(crime_raw, neighborhoods)
  permit_sf <- prepare_permit_data(permits_raw, neighborhoods)
  prior_crime_sf <- prepare_crime_data(prior_crime_raw, neighborhoods)
  prior_permit_sf <- prepare_permit_data(prior_permits_raw, neighborhoods)

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
    dplyr::left_join(
      summarize_neighborhood_counts(
        neighborhoods,
        prior_crime_sf,
        count_name = "prior_crime_count"
      ) |>
        sf::st_drop_geometry() |>
        dplyr::select(neighborhood_key, prior_crime_count),
      by = "neighborhood_key"
    ) |>
    dplyr::mutate(
      permit_count = dplyr::coalesce(permit_count, 0L),
      value_sum = dplyr::coalesce(value_sum, 0),
      demolition_count = dplyr::coalesce(demolition_count, 0L),
      prior_crime_count = dplyr::coalesce(prior_crime_count, 0L),
      crime_yoy_count_change = crime_count - prior_crime_count,
      crime_yoy_pct_change = dplyr::if_else(
        prior_crime_count > 0,
        (crime_count - prior_crime_count) / prior_crime_count,
        NA_real_
      )
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
    prior_year_window = prior_year_window,
    neighborhoods = neighborhoods,
    crime_sf = crime_sf,
    permit_sf = permit_sf,
    prior_crime_sf = prior_crime_sf,
    prior_permit_sf = prior_permit_sf,
    demolition_permit_sf = demolition_permit_sf,
    neighborhood_summary = neighborhood_summary,
    crime_neighborhoods = crime_neighborhoods,
    permit_neighborhoods = permit_neighborhoods,
    demolition_neighborhoods = demolition_neighborhoods,
    kpis = compute_kpis(crime_sf, permit_sf, prior_crime_sf, prior_permit_sf),
    top_crime_types = top_crime_types,
    top_permit_types = top_permit_types
  )
}
