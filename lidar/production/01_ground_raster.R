#------------------------------------------------------------------------------#
# Ground raster driver — one flight, no tuning or human judgment
#------------------------------------------------------------------------------#
#
# Thin wrapper around build_ground_raster() -- see `dev/worklog.md`,
# 2026-09-15, for the tuning/production driver split this is part of.
# Once a site/flight has a `best_csf` (for `ground_reference.method ==
# "csf"`) or a `ground_reference` decision (for
# `"massgis_plus_floor"`) recorded in `lidar/runs.yml`, this is the
# only script needed to (re)produce its ground raster -- no grid
# search, no MassGIS/photo-DEM comparison report.
#
# Prerequisite workflow:
#   lidar/tuning/01_csf_tuning.R and
#   lidar/tuning/02_ground_reference_selection.R -- record this site's
#   `best_csf` / `ground_reference` in lidar/runs.yml.
#
# Run from the project root in RStudio so relative paths resolve.
#------------------------------------------------------------------------------#

library(lidR)
library(future)
library(progressr)
library(pathtools)
progressr::handlers(global = TRUE)
progressr::handlers("cli")

invisible(lapply(list.files("R/", pattern = "\\.[Rr]$",
                            full.names = TRUE), source))

set_path_scheme("lidar/paths.yml")

site <- "rr" # 2- or 3-character lowercase site code (e.g. "rr", "nor")

# Date string in "yyyy_mm_dd" format identifying which flight to
# produce a ground raster for.
date <- "2022_08_10"

output <- build_ground_raster(site, date)

message("Ground raster ready: ", output)
