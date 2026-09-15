#------------------------------------------------------------------------------#
# Ground reference selection driver — per site
#------------------------------------------------------------------------------#
#
# Combines what used to be two separate scripts — see `dev/worklog.md`,
# 2026-09-15, for the tuning/production driver split this replaces:
#
#   1. `lidar/06_compare_ground_sources.R` — renders
#      `rmd/ground_source_comparison.Rmd`, comparing this site's tuned
#      CSF DTM(s) against orthophoto DEMs and MassGIS.
#   2. `lidar/07_floor_corrected_ground.R` — if MassGIS looks better,
#      estimates the instrument-level "floor" component of the lidar
#      bias (`estimate_floor_bias()`, which excludes the vegetation-
#      contaminated part of the residual distribution) and builds a
#      MassGIS-plus-floor-bias hybrid raster: keeps MassGIS's superior
#      absolute accuracy while still cancelling the UAS point cloud's
#      instrument-level offset.
#
# CSF parameters come from `lidar/runs.yml` (recorded by
# `lidar/tuning/01_csf_tuning.R`), not hand-copied literals — this is
# what kills the drift risk the old scripts had.
#
# This script is data-driven for a site once a ground_reference
# decision is recorded: it renders the comparison report, then checks
# `get_run_config(site)$ground_reference$method` — if
# "massgis_plus_floor" is already recorded, it (re)builds that hybrid
# raster via `build_ground_raster()`; otherwise it stops after the
# report with a reminder to review it and edit `runs.yml`. The same
# script serves the first exploratory run and every later refresh.
#
# Run from the project root in RStudio so relative paths resolve.
#------------------------------------------------------------------------------#

library(pathtools)

invisible(lapply(list.files("R/", pattern = "\\.[Rr]$",
                            full.names = TRUE), source))

set_path_scheme("lidar/paths.yml")

#------------------------------------------------------------------------------#
# Red River
#------------------------------------------------------------------------------#

site <- "rr"

config      <- get_run_config(site)
target_epsg <- config$target_epsg

spring_date <- "2022_05_14"
summer_date <- "2022_08_10"

best_csf_spring <- config$flights[[spring_date]]$best_csf
best_csf_summer <- config$flights[[summer_date]]$best_csf

sources <- c(
   lidar_spring = get_path("ground_raster", c(
      list(site = site, date = spring_date, target_epsg = target_epsg),
      best_csf_spring
   )),
   lidar_summer = get_path("ground_raster", c(
      list(site = site, date = summer_date, target_epsg = target_epsg),
      best_csf_summer
   )),
   photo_spring_hesai = get_path("photo_dem", site = site,
                                 source = "spring_hesai"),
   photo_spring_mica  = get_path("photo_dem", site = site,
                                 source = "spring_mica"),
   photo_summer       = get_path("photo_dem", site = site,
                                 source = "summer"),
   massgis            = get_path("massgis_tile", tile = "19TDG412612")
)

report_ground_sources(sources = sources, site = site)

# Floor-bias diagnostics -- both flights, for human comparison, even
# though only one (per ground_reference.floor_bias_source_flight, once
# recorded) ever feeds the hybrid raster. estimate_floor_bias() and
# build_ground_raster() only ever compute the one that's needed.
lidar_spring_ecp <- readr::read_csv(
   get_path("ground_comparison_ecp_cache", site = site,
            source_name = "lidar_spring"),
   show_col_types = FALSE
)
lidar_summer_ecp <- readr::read_csv(
   get_path("ground_comparison_ecp_cache", site = site,
            source_name = "lidar_summer"),
   show_col_types = FALSE
)

floor_spring <- estimate_floor_bias(lidar_spring_ecp)
floor_summer <- estimate_floor_bias(lidar_summer_ecp)

message(sprintf(
   "Floor bias — spring: %.3f m (n = %d, %d excluded)",
   floor_spring$floor_bias, floor_spring$n, floor_spring$n_excluded
))
message(sprintf(
   "Floor bias — summer: %.3f m (n = %d, %d excluded)",
   floor_summer$floor_bias, floor_summer$n, floor_summer$n_excluded
))

ggplot2::ggsave(
   get_path("floor_bias_plot", site = site, season = "spring"),
   plot_floor_bias(floor_spring, title = paste(site, "spring lidar")),
   width = 6, height = 4
)
ggplot2::ggsave(
   get_path("floor_bias_plot", site = site, season = "summer"),
   plot_floor_bias(floor_summer, title = paste(site, "summer lidar")),
   width = 6, height = 4
)

# Decision check -- build the hybrid raster if it's already decided,
# otherwise remind to record the decision.
ground_reference <- config$ground_reference

if (is.null(ground_reference) || is.null(ground_reference$method)) {
   message("No ground_reference decision recorded yet for '", site, "'. ",
           "Review the comparison report and floor-bias plots above, ",
           "then edit lidar/runs.yml's sites.", site, ".ground_reference.")
} else if (identical(ground_reference$method, "massgis_plus_floor")) {
   message("ground_reference.method = 'massgis_plus_floor' already ",
           "recorded for '", site, "' -- (re)building the hybrid raster.")
   build_ground_raster(site, ground_reference$floor_bias_source_flight)
} else {
   message("ground_reference.method = '", ground_reference$method,
           "' recorded for '", site, "' -- nothing further to build here.")
}

#------------------------------------------------------------------------------#
# Old Town Hill
#------------------------------------------------------------------------------#

# No raw point cloud for this site -- comparison is photo DEMs vs.
# MassGIS only, no lidar/CSF sources. Not yet represented in
# lidar/runs.yml (site rollout tracked separately, dev/rollout_workplan.md).
site <- "oth"
sources <- c(
   may_14_ortho   = get_path("photo_dem", site = site,
                             source = "may_14_ortho"),
   may_02_ortho   = get_path("photo_dem", site = site,
                             source = "may_02_ortho"),
   massgis_346737 = get_path("massgis_tile", tile = "19TCH346737"),
   massgis_346735 = get_path("massgis_tile", tile = "19TCH346735")
)

report_ground_sources(sources = sources, site = site)

#------------------------------------------------------------------------------#
# Welfleet
#------------------------------------------------------------------------------#

# Photo DEMs vs. MassGIS only, same as Old Town Hill above -- this
# site's lidar/CSF ground rasters aren't included in this comparison
# yet (also tracked under the site rollout, dev/rollout_workplan.md).
site <- "wel"
sources <- c(
   may_22_ortho   = get_path("photo_dem", site = site,
                             source = "may_22_ortho"),
   massgis_415636 = get_path("massgis_tile", tile = "19TDG415636"),
   massgis_417636 = get_path("massgis_tile", tile = "19TDG417636")
)

report_ground_sources(sources = sources, site = site)
