#!/usr/bin/env Rscript

library(dplyr)
library(ggplot2)
library(scales)
library(sf)
library(tibble)

source("R/utils.R")
source("R/data_fetch.R")
source("R/data_prep.R")
source("R/maps.R")
source("R/charts.R")
source("R/site_render.R")

site_dir <- "_site"
chart_dir <- file.path(site_dir, "charts")
snapshot_path <- file.path(site_dir, "og-snapshot.json")
html_path <- file.path(site_dir, "index.html")

unlink(site_dir, recursive = TRUE, force = TRUE)
ensure_directory(site_dir)

site_data <- build_site_data()
render_context <- build_render_context(site_data)
chart_assets <- write_daily_chart_images(render_context$daily_chart_bundle, chart_dir)
html_document <- build_html_document(render_context, chart_assets)

htmltools::save_html(
  html = html_document,
  file = html_path,
  libdir = "site_libs",
  background = "white"
)

file.copy("styles.css", file.path(site_dir, "styles.css"), overwrite = TRUE)
ensure_directory(file.path(site_dir, "assets"))
file.copy(
  from = list.files("assets", all.files = TRUE, full.names = TRUE, no.. = TRUE),
  to = file.path(site_dir, "assets"),
  recursive = TRUE
)

jsonlite::write_json(
  build_og_snapshot(render_context),
  path = snapshot_path,
  auto_unbox = TRUE,
  pretty = TRUE
)

message("Rendered site to ", html_path)
