build_socrata_request <- function(dataset_id, resource = "csv") {
  httr2::request(sprintf("https://data.buffalony.gov/resource/%s.%s", dataset_id, resource))
}

apply_socrata_app_token <- function(req) {
  app_token <- Sys.getenv("SOCRATA_APP_TOKEN", unset = "")
  if (!nzchar(app_token)) {
    return(req)
  }

  httr2::req_headers(req, "X-App-Token" = app_token)
}

fetch_socrata_csv <- function(dataset_id, select, where, order_by = NULL, limit = 50000) {
  query <- list(
    "$select" = select,
    "$where" = where,
    "$limit" = limit
  )

  if (!is.null(order_by)) {
    query[["$order"]] <- order_by
  }

  req <- build_socrata_request(dataset_id, "csv") |>
    apply_socrata_app_token() |>
    httr2::req_url_query(!!!query) |>
    httr2::req_user_agent("buffalo-permits-crime-tracker/0.1")

  resp <- httr2::req_perform(req)
  httr2::resp_check_status(resp)

  readr::read_csv(
    I(httr2::resp_body_string(resp)),
    show_col_types = FALSE,
    progress = FALSE
  )
}

fetch_neighborhood_boundaries <- function() {
  path <- site_config$data_paths$neighborhoods

  if (!file.exists(path)) {
    stop(
      sprintf(
        "Neighborhood boundary file is missing: %s",
        path
      ),
      call. = FALSE
    )
  }

  sf::read_sf(path, quiet = TRUE) |>
    sf::st_transform(4326)
}

fetch_crime_raw <- function(window = build_reporting_window()) {
  start_iso <- format(window$start_datetime, "%Y-%m-%dT%H:%M:%S")
  end_iso <- format(window$end_datetime_exclusive, "%Y-%m-%dT%H:%M:%S")

  select <- paste(
    "case_number,incident_datetime,incident_id,incident_type_primary,incident_description,",
    "parent_incident_type,address_1,latitude,longitude,neighborhood,council_district,police_district"
  )

  where <- sprintf(
    "incident_datetime >= '%s' AND incident_datetime < '%s'",
    start_iso,
    end_iso
  )

  fetch_socrata_csv(
    dataset_id = site_config$datasets$crime,
    select = select,
    where = where,
    order_by = "incident_datetime DESC",
    limit = 50000
  )
}

fetch_permits_raw <- function(window = build_reporting_window()) {
  start_iso <- format(window$start_datetime, "%Y-%m-%dT%H:%M:%S")
  end_iso <- format(window$end_datetime_exclusive, "%Y-%m-%dT%H:%M:%S")

  select <- paste(
    "apno,aptype,issued,stname,fees,value,descofwork,latitude,longitude,",
    "neighborhood,council_district,police_district"
  )

  where <- sprintf(
    "issued >= '%s' AND issued < '%s'",
    start_iso,
    end_iso
  )

  fetch_socrata_csv(
    dataset_id = site_config$datasets$permits,
    select = select,
    where = where,
    order_by = "issued DESC",
    limit = 50000
  )
}
