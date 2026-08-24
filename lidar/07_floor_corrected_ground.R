#------------------------------------------------------------------------------#
# Floor-bias-corrected ground reference — rr (Red River)
#------------------------------------------------------------------------------#
#
# UAS-derived ground rasters carry a positive bias against ECPs that
# confounds two things: a roughly constant instrument/PPK-level offset
# (~10 cm, tracked as the open Phase 1.6 diagnosis in dev/work_plan.md)
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

invisible(lapply(list.files("R/", pattern = "\\.[Rr]$",
                            full.names = TRUE), source))

site       <- "rr"
output_dir <- file.path("lidar/output", paste0(site, "_ground_comparison"))

# Corrected ground raster is written alongside the other rr summer
# rasters (CSF DTMs etc.) in the E: scratch tree, not under
# lidar/output/ — it's a large derived raster like those, not a small
# comparison artifact.
summer_raster_dir <- "E:/uas_scratch/lidar/rr/2022_08_10/zzzraster_epsg6491"

massgis_path <- paste0(
   "X:/scratch/bcompton/LiDAR/be_19TDG412612/be_19TDG412612.tif"
)

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

massgis_plus_floor <- terra::rast(massgis_path) + floor_summer$floor_bias
corrected_ground_tif <- file.path(summer_raster_dir,
                                  "massgis_plus_floor_summer.tif")
terra::writeRaster(massgis_plus_floor, corrected_ground_tif,
                   overwrite = TRUE)

message("Corrected ground raster written: ", corrected_ground_tif)
