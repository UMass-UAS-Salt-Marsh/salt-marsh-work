#------------------------------------------------------------------------------#
# Ground reference selection driver — one generic block, any site
#------------------------------------------------------------------------------#
#
# Combines what used to be two separate scripts — see `dev/worklog.md`,
# 2026-09-15, for the tuning/production driver split this replaces:
#
#   1. `lidar/06_compare_ground_sources.R` — renders
#      `rmd/ground_source_comparison.Rmd`, comparing this site's tuned
#      CSF DTM(s) against orthophoto DEMs and USGS.
#   2. `lidar/07_floor_corrected_ground.R` — if USGS looks better,
#      estimates the instrument-level "floor" component of the lidar
#      bias (`estimate_floor_bias()`, which excludes the vegetation-
#      contaminated part of the residual distribution) and builds a
#      USGS-plus-floor-bias hybrid raster: keeps USGS's superior
#      absolute accuracy while still cancelling the UAS point cloud's
#      instrument-level offset.
#
# A single `site <- "..."` variable (matching every other driver in
# `lidar/tuning/` and `lidar/production/`) drives everything below.
# Which flights/seasons a site has comes entirely from `lidar/runs.yml`
# (via `get_run_config()`); a flight only contributes a lidar
# ground-raster source once its `best_csf` is recorded there, so a
# brand-new site (flight dates entered, tuning not done yet) just
# skips that flight with a message rather than erroring.
# Photo-DEM sources (irregular per site, unrelated to `runs.yml`)
# and the USGS source are both optional: a site with no photo_dem
# overrides, or whose footprint can't resolve USGS tiles (e.g. `oth`,
# which has no site_footprint shapefile on disk), simply contributes
# fewer sources rather than failing.
#
# This script is data-driven for a site once a ground_reference
# decision is recorded: it renders the comparison report, then checks
# `get_run_config(site)$ground_reference$method` — if one of "csf",
# "usgs", or "usgs_plus_floor" is already recorded, it (re)builds that
# ground raster via `build_ground_raster()`; otherwise it stops after
# the report with a reminder to review it and edit `runs.yml`. A site
# with no `runs.yml` entry at all skips that decision step entirely —
# there is no flight or ground_reference to decide on yet.
#
# Run from the project root in RStudio so relative paths resolve.
#------------------------------------------------------------------------------#

library(pathtools)

invisible(lapply(list.files("R/", pattern = "\\.[Rr]$",
                            full.names = TRUE), source))

set_path_scheme("lidar/paths.yml")

site <- "esx" # 2- or 3-character lowercase site code (e.g. "rr", "esx")

#------------------------------------------------------------------------------#
# Resolve this site's run configuration, if any
#------------------------------------------------------------------------------#

config <- tryCatch(get_run_config(site), error = function(e) {
   message("No lidar/runs.yml entry for '", site, "' yet -- comparing ",
           "photo/USGS sources only.")
   list(target_epsg = NULL, flights = list(), ground_reference = NULL)
})
target_epsg <- config$target_epsg

#------------------------------------------------------------------------------#
# Lidar sources -- one per flight with a recorded best_csf
#------------------------------------------------------------------------------#

lidar_sources <- list()
for (date in get_flight_info(site, config)$date) {
   flight <- config$flights[[date]]
   if (is.null(flight$best_csf)) {
      message("No best_csf recorded yet for '", site, "' flight '", date,
              "' -- skipping its lidar source. Run ",
              "lidar/tuning/01_csf_tuning.R and record its winner in ",
              "lidar/runs.yml first.")
      next
   }
   label <- paste0("lidar_", flight$season)
   lidar_sources[[label]] <- get_path("ground_raster", c(
      list(site = site, date = date, target_epsg = target_epsg),
      flight$best_csf
   ))
}
lidar_sources <- unlist(lidar_sources)

#------------------------------------------------------------------------------#
# Photo-DEM sources -- whatever lidar/paths.yml's photo_dem overrides
# record for this site, however many and however named.
#------------------------------------------------------------------------------#

photo_overrides <- yaml::read_yaml("lidar/paths.yml")$paths$photo_dem$overrides
photo_sources <- list()
for (ov in photo_overrides) {
   if (identical(ov$when$site, site)) {
      photo_sources[[ov$when$source]] <- get_path(
         "photo_dem", site = site, source = ov$when$source
      )
   }
}
photo_sources <- unlist(photo_sources)

#------------------------------------------------------------------------------#
# USGS source -- omitted if the site's footprint can't resolve tiles
#------------------------------------------------------------------------------#

usgs_source <- tryCatch(
   c(usgs = merge_usgs_tiles(site)),
   error = function(e) {
      message("merge_usgs_tiles() failed for '", site, "' -- omitting ",
              "the USGS source from this comparison: ", conditionMessage(e))
      character(0)
   }
)

sources <- c(lidar_sources, photo_sources, usgs_source)

report_ground_sources(sources = sources, site = site)

#------------------------------------------------------------------------------#
# Floor-bias diagnostics -- one per season that produced a lidar source,
# for human comparison, even though only one (per
# ground_reference.reference_flight, once recorded) ever feeds the
# hybrid raster if method is "usgs_plus_floor". estimate_floor_bias()
# and build_ground_raster() only ever compute the one that's needed.
#------------------------------------------------------------------------------#

for (label in names(lidar_sources)) {
   season <- sub("^lidar_", "", label)

   ecp <- readr::read_csv(
      get_path("ground_comparison_ecp_cache", site = site,
               source_name = label),
      show_col_types = FALSE
   )
   floor <- estimate_floor_bias(ecp)

   message(sprintf(
      "Floor bias -- %s: %.3f m (n = %d, %d excluded)",
      season, floor$floor_bias, floor$n, floor$n_excluded
   ))

   ggplot2::ggsave(
      get_path("floor_bias_plot", site = site, season = season),
      plot_floor_bias(floor, title = paste(site, season, "lidar")),
      width = 6, height = 4
   )
}

#------------------------------------------------------------------------------#
# Decision check -- build the hybrid raster if it's already decided,
# otherwise remind to record the decision. Skipped entirely for a site
# with no runs.yml entry at all (nothing to decide on yet).
#------------------------------------------------------------------------------#

ground_reference <- config$ground_reference

if (length(config$flights) == 0L && is.null(ground_reference)) {
   message("No lidar/runs.yml entry for '", site, "' -- nothing to decide ",
           "yet. Add a `sites.", site, "` block once this site has a ",
           "recorded flight.")
} else if (is.null(ground_reference) || is.null(ground_reference$method)) {
   message("No ground_reference decision recorded yet for '", site, "'. ",
           "Review the comparison report and floor-bias plots above, ",
           "then edit lidar/runs.yml's sites.", site, ".ground_reference.")
} else if (ground_reference$method %in% c("csf", "usgs", "usgs_plus_floor")) {
   message("ground_reference.method = '", ground_reference$method,
           "' recorded for '", site, "' -- (re)building the ground raster.")
   build_ground_raster(site, ground_reference$reference_flight)
} else {
   message("Unrecognized ground_reference.method '",
           ground_reference$method, "' for '", site, "'.")
}
