build_render_context <- function(site_data) {
  window <- site_data$window
  generated_label <- format(
    lubridate::with_tz(window$generated_at, tzone = site_config$timezone),
    "%b %d, %Y %I:%M %p %Z"
  )
  window_label <- sprintf(
    "%s to %s",
    format_date_label(window$start_date),
    format_date_label(window$end_date)
  )

  crime_neighborhood_rank <- site_data$crime_neighborhoods |>
    sf::st_drop_geometry() |>
    dplyr::arrange(dplyr::desc(crime_count), neighborhood) |>
    dplyr::slice_head(n = 10) |>
    dplyr::transmute(
      Neighborhood = neighborhood,
      `Crime incidents` = format_number_label(crime_count)
    )

  permit_neighborhood_rank <- site_data$permit_neighborhoods |>
    sf::st_drop_geometry() |>
    dplyr::arrange(dplyr::desc(value_sum), neighborhood) |>
    dplyr::slice_head(n = 10) |>
    dplyr::transmute(
      Neighborhood = neighborhood,
      `Declared permit value` = format_currency_label(value_sum),
      Permits = format_number_label(permit_count)
    )

  daily_chart_bundle <- build_daily_chart_bundle(site_data, window_label)

  list(
    site_data = site_data,
    window_label = window_label,
    generated_label = generated_label,
    crime_neighborhood_rank = crime_neighborhood_rank,
    permit_neighborhood_rank = permit_neighborhood_rank,
    daily_chart_bundle = daily_chart_bundle
  )
}

ensure_directory <- function(path) {
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  invisible(path)
}

write_daily_chart_images <- function(daily_chart_bundle, output_dir) {
  ensure_directory(output_dir)

  chart_specs <- list(
    crime = list(filename = "daily-crime-incidents.png"),
    permits = list(filename = "daily-permits-issued.png")
  )

  for (name in names(chart_specs)) {
    plot_bundle <- daily_chart_bundle[[name]]
    chart_path <- file.path(output_dir, chart_specs[[name]]$filename)

    ggplot2::ggsave(
      filename = chart_path,
      plot = plot_bundle$plot,
      device = ragg::agg_png,
      width = 7,
      height = 4.8,
      units = "in",
      dpi = 144,
      bg = "white"
    )

    chart_specs[[name]]$src <- file.path("charts", chart_specs[[name]]$filename)
    chart_specs[[name]]$alt_text <- plot_bundle$alt_text
  }

  chart_specs
}

build_data_table <- function(data, numeric_columns = integer()) {
  htmltools::tags$table(
    htmltools::tags$thead(
      htmltools::tags$tr(
        lapply(names(data), function(name) {
          htmltools::tags$th(scope = "col", name)
        })
      )
    ),
    htmltools::tags$tbody(
      lapply(seq_len(nrow(data)), function(i) {
        htmltools::tags$tr(
          lapply(seq_along(data), function(j) {
            raw_value <- data[[j]][[i]]
            cell_value <- if (length(raw_value) == 0 || is.na(raw_value)) {
              ""
            } else {
              as.character(raw_value)
            }
            classes <- if (j %in% numeric_columns) "text-end" else NULL
            htmltools::tags$td(class = classes, cell_value)
          })
        )
      })
    )
  )
}

build_note_list <- function(items) {
  htmltools::tags$ul(
    lapply(items, function(item) {
      htmltools::tags$li(item)
    })
  )
}

build_responsive_picture <- function(src, alt, img_class, picture_class = NULL, eager = FALSE) {
  htmltools::tags$picture(
    class = picture_class,
    htmltools::tags$source(
      media = "(max-width: 720px)",
      srcset = src
    ),
    htmltools::tags$source(
      media = "(min-width: 721px)",
      srcset = src
    ),
    htmltools::tags$img(
      src = src,
      alt = alt,
      class = img_class,
      loading = if (eager) "eager" else "lazy",
      decoding = "async"
    )
  )
}

build_page_head <- function() {
  canonical_url <- site_config$site$site_url
  og_image_url <- paste0(
    sub("/?$", "/", canonical_url),
    site_config$site$image
  )

  htmltools::tagList(
    htmltools::tags$meta(
      name = "viewport",
      content = "width=device-width, initial-scale=1.0, viewport-fit=cover"
    ),
    htmltools::tags$title(site_config$site$title),
    htmltools::tags$meta(name = "description", content = site_config$site$description),
    htmltools::tags$link(rel = "canonical", href = canonical_url),
    htmltools::tags$meta(property = "og:type", content = "website"),
    htmltools::tags$meta(property = "og:title", content = site_config$site$title),
    htmltools::tags$meta(property = "og:description", content = site_config$site$description),
    htmltools::tags$meta(property = "og:url", content = canonical_url),
    htmltools::tags$meta(property = "og:image", content = og_image_url),
    htmltools::tags$meta(property = "og:image:alt", content = site_config$site$image_alt),
    htmltools::tags$meta(name = "twitter:card", content = "summary_large_image"),
    htmltools::tags$meta(name = "twitter:title", content = site_config$site$title),
    htmltools::tags$meta(name = "twitter:description", content = site_config$site$description),
    htmltools::tags$meta(name = "twitter:image", content = og_image_url),
    htmltools::tags$link(rel = "stylesheet", href = "styles.css")
  )
}

build_site_body <- function(render_context, chart_assets) {
  site_data <- render_context$site_data

  htmltools::tags$body(
    class = "dashboard-page",
    htmltools::tags$div(
      id = "quarto-content",
      htmltools::tags$main(
        class = "content column-page",
        id = "quarto-document-content",
        htmltools::tags$div(
          class = "page-shell",
          htmltools::tags$div(
            class = "masthead",
            htmltools::tags$section(
              class = "masthead-copy",
              htmltools::tags$p(class = "eyebrow", "Buffalo, New York"),
              htmltools::tags$h1("Permits and crime incidents, refreshed daily."),
              htmltools::tags$p(
                class = "masthead-dek",
                "A daily Buffalo dashboard tracking recent permits, crime incidents, and demolition activity."
              ),
              htmltools::tags$div(
                class = "meta-row",
                htmltools::tags$div(
                  class = "meta-item",
                  htmltools::tags$div(class = "meta-term", "Window"),
                  htmltools::tags$div(class = "meta-detail", render_context$window_label)
                ),
                htmltools::tags$div(
                  class = "meta-item",
                  htmltools::tags$div(class = "meta-term", "Updated"),
                  htmltools::tags$div(class = "meta-detail", render_context$generated_label)
                )
              )
            ),
            htmltools::tags$div(
              class = "masthead-visual",
              build_responsive_picture(
                src = site_config$site$hero_image,
                alt = "Aerial view of downtown Buffalo, New York",
                img_class = "masthead-image",
                picture_class = "masthead-picture",
                eager = TRUE
              )
            )
          ),
          htmltools::tags$section(
            class = "section-block",
            htmltools::tags$h2("Key signals from the current 30-day window"),
            build_kpi_cards(site_data$kpis)
          ),
          htmltools::tags$section(
            class = "section-block",
            htmltools::tags$h2("Incidents, permits, and demolition activity in one view"),
            htmltools::tags$div(class = "main-map-frame", make_main_map(site_data))
          ),
          htmltools::tags$section(
            class = "section-block",
            htmltools::tags$h2("Where recent crime counts are concentrated"),
            htmltools::tags$div(
              class = "support-map-frame",
              make_summary_map(
                site_data$crime_neighborhoods,
                value_col = "crime_count",
                title = "30-day crime count",
                palette = site_config$palette$crime_fill,
                value_format = format_number_label
              )
            )
          ),
          htmltools::tags$section(
            class = "section-block",
            htmltools::tags$h2("Daily totals"),
            htmltools::tags$div(
              class = "chart-grid",
              htmltools::tags$section(
                class = "chart-card",
                htmltools::tags$h3("Total crime incidents per day"),
                htmltools::tags$img(
                  src = chart_assets$crime$src,
                  alt = chart_assets$crime$alt_text,
                  class = "chart-image"
                )
              ),
              htmltools::tags$section(
                class = "chart-card",
                htmltools::tags$h3("Total permits per day"),
                htmltools::tags$img(
                  src = chart_assets$permits$src,
                  alt = chart_assets$permits$alt_text,
                  class = "chart-image"
                )
              )
            )
          ),
          htmltools::tags$section(
            class = "section-block",
            htmltools::tags$h2("Neighborhood and category rankings"),
            htmltools::tags$div(
              class = "table-stack",
              htmltools::tags$section(
                class = "table-card",
                htmltools::tags$h3("Neighborhoods with the most crime incidents"),
                build_data_table(render_context$crime_neighborhood_rank, numeric_columns = 2L)
              ),
              htmltools::tags$section(
                class = "table-card",
                htmltools::tags$h3("Neighborhoods with the highest declared permit value"),
                build_data_table(render_context$permit_neighborhood_rank, numeric_columns = c(2L, 3L))
              ),
              htmltools::tags$section(
                class = "table-card",
                htmltools::tags$h3("Most common crime and permit categories"),
                htmltools::tags$div(
                  class = "table-pair",
                  htmltools::tags$div(
                    class = "mini-table",
                    htmltools::tags$h4("Crime categories"),
                    build_data_table(
                      site_data$top_crime_types |>
                        dplyr::mutate(category = dplyr::coalesce(category, "Unknown")) |>
                        dplyr::mutate(n = format_number_label(n)) |>
                        dplyr::rename(Category = category, Incidents = n),
                      numeric_columns = 2L
                    )
                  ),
                  htmltools::tags$div(
                    class = "mini-table",
                    htmltools::tags$h4("Permit types"),
                    build_data_table(
                      site_data$top_permit_types |>
                        dplyr::mutate(category = dplyr::coalesce(category, "Unknown")) |>
                        dplyr::mutate(n = format_number_label(n)) |>
                        dplyr::rename(Type = category, Permits = n),
                      numeric_columns = 2L
                    )
                  )
                )
              )
            )
          ),
          htmltools::tags$section(
            class = "section-block notes-block",
            htmltools::tags$h2("Notes and sources"),
            htmltools::tags$div(
              class = "notes-grid",
              htmltools::tags$section(
                class = "note-card",
                htmltools::tags$h3("Sources"),
                htmltools::tags$p("Data is pulled from Buffalo Open Data during the daily build:"),
                build_note_list(list(
                  htmltools::tags$span(
                    "Crime Incidents: ",
                    htmltools::tags$a(href = site_config$source_urls$crime, site_config$source_urls$crime)
                  ),
                  htmltools::tags$span(
                    "Permits: ",
                    htmltools::tags$a(href = site_config$source_urls$permits, site_config$source_urls$permits)
                  ),
                  htmltools::tags$span(
                    "Neighborhoods: ",
                    htmltools::tags$a(href = site_config$source_urls$neighborhoods, site_config$source_urls$neighborhoods)
                  )
                ))
              ),
              htmltools::tags$section(
                class = "note-card",
                htmltools::tags$h3("Caveats"),
                htmltools::tags$p(
                  sprintf(
                    "The reporting window is calculated in %s and includes today plus the previous %s days. Crime data is preliminary.",
                    site_config$timezone,
                    site_config$window_days - 1L
                  )
                )
              )
            )
          ),
          htmltools::tags$div(
            class = "bottom-photo-composition",
            htmltools::tags$div(
              class = "bottom-photo-primary",
              build_responsive_picture(
                src = site_config$site$bottom_primary_image,
                alt = "Aerial view of Buffalo, New York, with a focus on the historic Buffalo Central Terminal",
                img_class = "bottom-photo-image bottom-photo-image-primary",
                picture_class = "bottom-photo-picture"
              )
            ),
            htmltools::tags$div(
              class = "bottom-photo-archive",
              build_responsive_picture(
                src = site_config$site$bottom_archive_image,
                alt = "Grain boats and grain elevators on the Erie Canal in Buffalo",
                img_class = "bottom-photo-image bottom-photo-image-archive bottom-photo-image-archive-top",
                picture_class = "bottom-photo-picture"
              )
            )
          ),
          htmltools::tags$footer(
            class = "page-footer",
            htmltools::tags$div(
              class = "page-footer-left",
              htmltools::HTML(
                "Built with R, htmltools, and Buffalo Open Data by <a href=\"https://kraieski.dev/\">Alex Kraieski</a>."
              )
            ),
            htmltools::tags$div(
              class = "page-footer-right",
              "Updated daily via GitHub Actions."
            )
          )
        )
      )
    )
  )
}

build_html_document <- function(render_context, chart_assets) {
  htmltools::tagList(
    htmltools::tags$head(build_page_head()),
    build_site_body(render_context, chart_assets)
  )
}

build_og_snapshot <- function(render_context) {
  list(
    title = "Permits and crime incidents, refreshed daily.",
    pageTitle = site_config$site$title,
    description = site_config$site$description,
    windowLabel = render_context$window_label,
    updatedLabel = render_context$generated_label,
    heroSrc = site_config$site$hero_image,
    kpis = lapply(seq_len(min(3L, nrow(render_context$site_data$kpis))), function(i) {
      list(
        label = render_context$site_data$kpis$label[[i]],
        value = render_context$site_data$kpis$value[[i]]
      )
    })
  )
}
