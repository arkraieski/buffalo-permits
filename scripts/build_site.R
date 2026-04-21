#!/usr/bin/env Rscript

library(dplyr)
library(ggplot2)
library(scales)
library(sf)
library(tibble)

## Module load order and ownership:
## - `R/utils.R`: shared config and formatting helpers
## - `R/data_fetch.R`: Socrata and boundary fetchers
## - `R/data_prep.R`: builds the `site_data` object used by the page
## - `R/maps.R`: leaflet widget builders used inside the page render
## - `R/charts.R`: ggplot builders for the static PNG charts
## - `R/page_builder.R`: HTML assembly, chart file writing, and OG snapshot data
source("R/utils.R")
source("R/data_fetch.R")
source("R/data_prep.R")
source("R/maps.R")
source("R/charts.R")
source("R/page_builder.R")

site_dir <- "_site"
chart_dir <- file.path(site_dir, "charts")
snapshot_path <- file.path(site_dir, "og-snapshot.json")
html_path <- file.path(site_dir, "index.html")

## Start from a clean output directory so the deployed artifact mirrors the
## current render instead of carrying stale files forward.
reset_site_output <- function(site_dir) {
  unlink(site_dir, recursive = TRUE, force = TRUE)
  ensure_directory(site_dir)
}

copy_static_assets <- function(site_dir) {
  file.copy("styles.css", file.path(site_dir, "styles.css"), overwrite = TRUE)
  ensure_directory(file.path(site_dir, "assets"))
  file.copy(
    from = list.files("assets", all.files = TRUE, full.names = TRUE, no.. = TRUE),
    to = file.path(site_dir, "assets"),
    recursive = TRUE
  )
}

write_site_snapshot <- function(render_context, snapshot_path) {
  jsonlite::write_json(
    build_og_snapshot(render_context),
    path = snapshot_path,
    auto_unbox = TRUE,
    pretty = TRUE
  )
}

reset_site_output(site_dir)
## `build_site_data()` is defined in `R/data_prep.R`.
site_data <- build_site_data()
## `build_render_context()` and `build_html_document()` are defined in `R/page_builder.R`.
render_context <- build_render_context(site_data)
## `build_daily_chart_bundle()` lives in `R/charts.R`; `write_daily_chart_images()`
## lives in `R/page_builder.R` and turns those plots into files.
chart_assets <- write_daily_chart_images(render_context$daily_chart_bundle, chart_dir)
html_document <- build_html_document(render_context, chart_assets)

## Save the page first so htmlwidgets can emit their dependency bundle.
htmltools::save_html(
  html = html_document,
  file = html_path,
  libdir = "site_libs",
  background = "white"
)

## Then copy static assets and write the structured input for OG generation.
copy_static_assets(site_dir)
write_site_snapshot(render_context, snapshot_path)

message("Rendered site to ", html_path)
