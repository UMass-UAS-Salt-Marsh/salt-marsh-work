#------------------------------------------------------------------------------#
# Vegetation height distribution driver
#------------------------------------------------------------------------------#
#
# Produces a multi-band GeoTIFF where each band is a height bin and the
# cell value is the fraction of summer returns in that bin above the
# ground reference.
#
# Ground reference defaults to the floor-bias-corrected MassGIS raster
# (lidar/07_floor_corrected_ground.R): MassGIS has near-zero bias
# against ECPs but doesn't share the UAS point cloud's ~10 cm
# instrument-level offset, so it's shifted up by an estimate of that
# offset alone (not the full spring-lidar-DTM bias, which also carries
# a vegetation-dependent CSF ground-finding error). This out-performed
# the legacy spring-lidar-DTM approach against field-measured
# vegetation heights — see lidar/08_veg_height_validation.R and
# lidar/readme.md's "Canopy-top underestimate..." section. The legacy
# path is kept below as a named alternative for comparison.
#
# Prerequisite workflow:
#   1. lidar/06_compare_ground_sources.R — ground-source comparison +
#      cached ECP samples.
#   2. lidar/07_floor_corrected_ground.R — produces the corrected
#      ground raster.
#   3. This script.
#
# The summer cloud need not be re-cleaned: if zzzcleaned/ tiles already
# exist from a previous run of lidar/02.R, clean_and_tile() skips them.
#
# Outputs (under E:/uas_scratch/lidar/<site>/<summer_date>/zzzheights/)
#   * veg_dist_<raster_res>m.tif — 31-band height distribution raster.
#   * return_counts_<raster_res>m.tif — single-band raster of how many
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

#------------------------------------------------------------------------------#
# Run-level parameters
#------------------------------------------------------------------------------#

workers      <- 25    # parallel workers for plan(multisession)
chunk_size   <- 100   # tile size in meters
chunk_buffer <- 20    # buffer around each chunk in meters

# Some rr summer tiles run 6-12 million points per 200x200m tile
# (~275 pts/m^2, last returns) -- rasterize_veg_heights()'s 30-bin
# tabulate()/.bincode() step ran out of memory on those at the
# original chunk_size = 200 default (see dev/worklog.md 2026-08-24).
# 100 m keeps peak per-chunk point count manageable.

site        <- "rr"
summer_date <- "2022_08_10"   # yyyy_mm_dd; matches lidar/data/paths.yml

# Must match whatever lidar/02.R used to produce the cleaned tiles and
# ground rasters below. 6491 = NAD83(2011) / Massachusetts Mainland
# State Plane, this project's current standard (see CRS.md).
target_epsg <- 6491L

raster_res <- 0.5   # output resolution in metres

plan(multisession, workers = workers)

invisible(lapply(list.files("R/", pattern = "\\.[Rr]$",
                            full.names = TRUE), source))

set_path_scheme("lidar/data/paths.yml")

#------------------------------------------------------------------------------#
# Resolve paths
#------------------------------------------------------------------------------#

summer_cloud <- get_path("raw_lidar", site = site, date = summer_date)

summer_clean_dir <- get_path("cleaned_tiles", site = site, date = summer_date,
                             target_epsg = target_epsg)
output_dir <- get_path("veg_heights_dir", site = site, date = summer_date)
output_tif <- get_path("veg_heights_raster", site = site, date = summer_date,
                       raster_res = raster_res)
count_tif <- get_path("veg_return_count_raster", site = site,
                      date = summer_date, raster_res = raster_res)

# Ground reference for normalization. Default: floor-bias-corrected
# MassGIS raster (lidar/07_floor_corrected_ground.R), stored alongside
# the other rr summer rasters. To compare against the legacy approach
# instead, set:
#   ground_raster <- get_path("ground_raster", site = site,
#      date = "2022_05_14", target_epsg = target_epsg,
#      csf_threshold = 0.01, csf_res = 0.1, csf_rigidness = 2,
#      raster_res = 0.25)
ground_raster <- get_path("corrected_ground_raster", site = site,
                          date = summer_date, target_epsg = target_epsg)

if (!file.exists(ground_raster)) {
   stop("Ground raster not found: ", ground_raster,
        "\nRun lidar/06_compare_ground_sources.R then ",
        "lidar/07_floor_corrected_ground.R to produce it.")
}

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)


#------------------------------------------------------------------------------#
# Run
#------------------------------------------------------------------------------#

# Step 1: clean and tile summer cloud (skip-if-exists).
clean_and_tile(summer_cloud, summer_clean_dir,
               chunk_size = chunk_size, chunk_buffer = chunk_buffer)

# Step 2: rasterize vegetation height distribution.
message("Starting vegetation height rasterization ", lubridate::now())
rasterize_veg_heights(
   input        = summer_clean_dir,
   dtm          = ground_raster,
   output       = output_tif,
   count_output = count_tif,
   raster_res   = raster_res,
   chunk_size   = chunk_size,
   chunk_buffer = chunk_buffer
)
message("Done ", lubridate::now())

# Step 3: write a shared metadata sidecar for both outputs. bin_breaks
# is recorded explicitly (not just raster_res) because it's the
# rasterize_veg_heights() default here, with no override -- this
# sidecar is the only place it's ever recorded.
write_output_metadata(
   output       = c(output_tif, count_tif),
   created_by   = "rasterize_veg_heights",
   source_cloud = summer_cloud,
   crs          = paste0("EPSG:", target_epsg),
   inputs       = list(cleaned_tiles = summer_clean_dir,
                       ground_raster = ground_raster),
   params       = list(raster_res = raster_res,
                       bin_breaks = c(-0.05, seq(0.05, 1, by = 0.05),
                                      seq(1.2, 3.0, by = 0.20), Inf))
)
