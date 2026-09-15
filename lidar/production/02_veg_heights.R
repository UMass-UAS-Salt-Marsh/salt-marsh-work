#------------------------------------------------------------------------------#
# Vegetation height distribution driver
#------------------------------------------------------------------------------#
#
# Produces a multi-band GeoTIFF where each band is a height bin and the
# cell value is the fraction of summer returns in that bin above the
# ground reference.
#
# Was `lidar/05_veg_heights.R` -- see `dev/worklog.md`, 2026-09-15, for
# the tuning/production driver split this replaces. The ground raster
# is no longer a hard prerequisite checked by `file.exists()`: it's
# obtained from `build_ground_raster()`, which reads this site's
# recorded `ground_reference` decision (`lidar/runs.yml`) and builds
# the raster if it isn't already materialized.
#
# Ground reference defaults to whatever `ground_reference.method` is
# recorded for this site in `lidar/runs.yml`. For `rr` that's currently
# the floor-bias-corrected MassGIS raster: MassGIS has near-zero bias
# against ECPs but doesn't share the UAS point cloud's ~10 cm
# instrument-level offset, so it's shifted up by an estimate of that
# offset alone (not the full spring-lidar-DTM bias, which also carries
# a vegetation-dependent CSF ground-finding error). This out-performed
# the legacy spring-lidar-DTM approach against field-measured
# vegetation heights -- see `lidar/tuning/veg_height_check.R` and
# `lidar/readme.md`'s "Canopy-top underestimate..." section.
#
# Prerequisite workflow:
#   1. lidar/tuning/01_csf_tuning.R and
#      lidar/tuning/02_ground_reference_selection.R -- record this
#      site's `best_csf` / `ground_reference` in lidar/runs.yml.
#   2. This script.
#
# The summer cloud need not be re-cleaned: prepare_flight()'s
# clean_and_tile() step skips tiles that already exist from a previous
# run of lidar/tuning/01_csf_tuning.R or this script.
#
# Outputs (under E:/uas_scratch/lidar/<site>/<summer_date>/veg_heights/)
#   * veg_dist_<raster_res>m.tif -- 31-band height distribution raster.
#   * return_counts_<raster_res>m.tif -- single-band raster of how many
#     returns landed in each cell (the denominator behind the
#     fractions above).
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

#------------------------------------------------------------------------------#
# Run-level parameters
#------------------------------------------------------------------------------#

site        <- "rr"
summer_date <- "2022_08_10"

config      <- get_run_config(site, summer_date)
target_epsg <- config$target_epsg

raster_res <- 0.5   # output resolution in metres

# Some rr summer tiles run 6-12 million points per 200x200m tile
# (~275 pts/m^2, last returns) -- rasterize_veg_heights()'s 30-bin
# tabulate()/.bincode() step ran out of memory on those at the
# original chunk_size = 200 default (see dev/worklog.md 2026-08-24).
# 100 m keeps peak per-chunk point count manageable. These stay
# driver-local, separate from runs.yml's prepare_flight() settings,
# because this rasterization step has a different memory footprint.
veg_workers      <- 15
veg_chunk_size   <- 100
veg_chunk_buffer <- 20

#------------------------------------------------------------------------------#
# Step 1: prepare the point cloud, resolve the ground raster
#------------------------------------------------------------------------------#

flight <- prepare_flight(site, summer_date, target_epsg = target_epsg,
                         workers = config$workers,
                         chunk_size = config$chunk_size,
                         chunk_buffer = config$chunk_buffer)

# To compare against the legacy spring-lidar-DTM approach instead,
# resolve the spring flight's ground_raster directly via get_path(),
# combining site/date/target_epsg with its best_csf from
# get_run_config() -- see lidar/tuning/veg_height_check.R for the
# pattern -- rather than calling build_ground_raster() below.
ground_raster <- build_ground_raster(site, summer_date)

output_dir <- get_path("veg_heights_dir", site = site, date = summer_date)
output_tif <- get_path("veg_heights_raster", site = site, date = summer_date,
                       raster_res = raster_res)
count_tif <- get_path("veg_return_count_raster", site = site,
                      date = summer_date, raster_res = raster_res)

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

#------------------------------------------------------------------------------#
# Step 2: rasterize vegetation height distribution
#------------------------------------------------------------------------------#

plan(multisession, workers = veg_workers)

message("Starting vegetation height rasterization ", lubridate::now())
rasterize_veg_heights(
   input        = flight$cleaned_dir,
   dtm          = ground_raster,
   output       = output_tif,
   count_output = count_tif,
   raster_res   = raster_res,
   chunk_size   = veg_chunk_size,
   chunk_buffer = veg_chunk_buffer
)
message("Done ", lubridate::now())

# Step 3: write a shared metadata sidecar for both outputs. bin_breaks
# is recorded explicitly (not just raster_res) because it's the
# rasterize_veg_heights() default here, with no override -- this
# sidecar is the only place it's ever recorded.
write_output_metadata(
   output       = c(output_tif, count_tif),
   created_by   = "rasterize_veg_heights",
   source_cloud = flight$raw_cloud,
   crs          = paste0("EPSG:", target_epsg),
   inputs       = list(cleaned_tiles = flight$cleaned_dir,
                       ground_raster = ground_raster),
   params       = list(raster_res = raster_res,
                       bin_breaks = c(-0.05, seq(0.05, 1, by = 0.05),
                                      seq(1.2, 3.0, by = 0.20), Inf))
)
