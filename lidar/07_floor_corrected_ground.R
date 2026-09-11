#------------------------------------------------------------------------------#
# Floor-bias-corrected ground reference — rr (Red River)
#------------------------------------------------------------------------------#
#
# UAS-derived ground rasters carry a positive bias against ECPs that
# confounds two things: a roughly constant instrument/PPK-level offset
# (~10 cm, tracked as the open Phase 1.6 diagnosis in dev/workplan.md)
# and a vegetation-density-dependent CSF ground-finding error (worse in
# summer than spring). MassGIS's aerial lidar has ~no bias but doesn't
# share that instrument offset with the UAS point cloud, so using it
# directly as ground would leave the *entire* instrument-level offset
# uncancelled in every vegetation-height measurement.
#
# This script estimates just the instrument-level "floor" component of
# the summer lidar bias (via estimate_floor_bias(), which excludes the
# vegetation-contaminated part of the residual distribution) and adds
# it to the MassGIS raster, producing a ground reference that keeps
# MassGIS's superior absolute accuracy while still cancelling the
# instrument offset the way the current spring-DTM-relative approach
# does.
#
# Prerequisite: lidar/06_compare_ground_sources.R has been run for
# `rr` (produces the cached *_ecp.csv files this script reads).
#
# Run from the project root in RStudio so relative paths resolve.
#------------------------------------------------------------------------------#

library(pathtools)

invisible(lapply(list.files("R/", pattern = "\\.[Rr]$",
                            full.names = TRUE), source))

set_path_scheme("lidar/data/paths.yml")

site       <- "rr"
output_dir <- get_path("ground_comparison_report", site = site)

# Must match whatever lidar/02.R used to produce the point cloud this
# raster will be paired with (normalize_height() does a raw coordinate
# lookup against the raster, with no CRS-aware reprojection -- unlike
# sample_dtm()'s ECP sampling -- so a CRS mismatch here silently
# returns NA for every point and falls back to an unbounded knnidw
# search, which is catastrophically slow/memory-hungry. See
# dev/worklog.md 2026-08-24.
target_epsg <- 6491L

# Corrected ground raster is written alongside the other rr summer
# rasters (CSF DTMs etc.) in the E: scratch tree, not in the reports
# directory — it's a large derived raster like those, not a small
# comparison artifact.
summer_raster_dir <- get_path("ground_raster_dir", site = site,
                              date = "2022_08_10", target_epsg = target_epsg)

massgis_path <- get_path("massgis_tile", tile = "19TDG412612")

#------------------------------------------------------------------------------#
# Estimate the floor bias for each flight
#------------------------------------------------------------------------------#

lidar_spring <- readr::read_csv(
   file.path(output_dir, "lidar_spring_ecp.csv"), show_col_types = FALSE
)
lidar_summer <- readr::read_csv(
   file.path(output_dir, "lidar_summer_ecp.csv"), show_col_types = FALSE
)

floor_spring <- estimate_floor_bias(lidar_spring)
floor_summer <- estimate_floor_bias(lidar_summer)

message(sprintf(
   "Floor bias — spring: %.3f m (n = %d, %d excluded)",
   floor_spring$floor_bias, floor_spring$n, floor_spring$n_excluded
))
message(sprintf(
   "Floor bias — summer: %.3f m (n = %d, %d excluded)",
   floor_summer$floor_bias, floor_summer$n, floor_summer$n_excluded
))

ggplot2::ggsave(
   file.path(output_dir, "floor_bias_spring.png"),
   plot_floor_bias(floor_spring, title = paste(site, "spring lidar")),
   width = 6, height = 4
)
ggplot2::ggsave(
   file.path(output_dir, "floor_bias_summer.png"),
   plot_floor_bias(floor_summer, title = paste(site, "summer lidar")),
   width = 6, height = 4
)

#------------------------------------------------------------------------------#
# Build the corrected ground raster: massgis + floor_bias_summer
#------------------------------------------------------------------------------#

massgis_raw <- terra::rast(massgis_path)

# MassGIS BE tiles are published in EPSG:6348 (NAD83(2011)/UTM19N) --
# a different, but equally valid, NAD83(2011) projection than this
# project's EPSG:6491 (Mass State Plane) standard. Reproject so this
# raster shares a CRS with the point cloud it will be paired with in
# lidar/05_veg_heights.R (see target_epsg note above).
massgis_reprojected <- terra::project(massgis_raw,
                                      paste0("EPSG:", target_epsg))

massgis_plus_floor <- massgis_reprojected + floor_summer$floor_bias
corrected_ground_tif <- ensure_parent(get_path(
   "corrected_ground_raster", site = site, date = "2022_08_10",
   target_epsg = target_epsg
))
terra::writeRaster(massgis_plus_floor, corrected_ground_tif,
                   overwrite = TRUE)

message("Corrected ground raster written: ", corrected_ground_tif)
