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
# dev/work_plan.md Phase 1.7. The legacy path is kept below as a named
# alternative for comparison.
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
#   * veg_dist_<raster_res>m.tif — 30-band height distribution raster.
#
# Run from the project root in RStudio so relative paths resolve.
#------------------------------------------------------------------------------#

library(lidR)
library(future)
library(progressr)
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
summer_date <- "2022-08-10"   # yyyy-mm-dd; used to locate summer clean dir

# Must match whatever lidar/02.R used to produce the cleaned tiles and
# ground rasters below. 6491 = NAD83(2011) / Massachusetts Mainland
# State Plane, this project's current standard (see CRS.md).
target_epsg <- 6491L

# Ground reference for normalization. Default: floor-bias-corrected
# MassGIS raster (lidar/07_floor_corrected_ground.R), stored alongside
# the other rr summer rasters. To compare against the legacy approach
# instead, set:
#   ground_raster <- file.path(
#      "E:/uas_scratch/lidar", site, "2022_05_14",
#      "zzzraster_epsg6491", "csf_th0.01_res0.1_rgd2_0.25m.tif"
#   )
ground_raster <- file.path(
   "E:/uas_scratch/lidar", site, "2022_08_10",
   paste0("zzzraster_epsg", target_epsg), "massgis_plus_floor_summer.tif"
)

raster_res <- 0.5   # output resolution in metres

plan(multisession, workers = workers)

invisible(lapply(list.files("R/", pattern = "\\.[Rr]$",
                            full.names = TRUE), source))


#------------------------------------------------------------------------------#
# Resolve paths from paths.csv
#------------------------------------------------------------------------------#

input_file_paths <- readr::read_csv("lidar/data/paths.csv")
input_file_paths$path <- gsub("\\\\", "/", input_file_paths$path)
input_file_paths$preferred <- as.logical(input_file_paths$preferred)
input_file_paths$path <- gsub("^[[:blank:]/\"\\\\]+|[[:blank:]/\"\\\\]+$",
                              "", input_file_paths$path)

summer_rows <- which(
   input_file_paths$site == site &
      input_file_paths$type == "cloud" &
      input_file_paths$preferred &
      format(input_file_paths$date, "%Y-%m-%d") == summer_date
)
if (length(summer_rows) == 0L) {
   stop("No preferred cloud row found for site=", site,
        " date=", summer_date, " in lidar/data/paths.csv")
}
summer_cloud <- input_file_paths$path[summer_rows[1]]

summer_date_uu <- gsub("-", "_", summer_date, fixed = TRUE)
summer_clean_dir <- file.path("E:/uas_scratch/lidar", site,
                              summer_date_uu,
                              paste0("zzzcleaned_epsg", target_epsg))
output_dir <- file.path("E:/uas_scratch/lidar", site,
                        summer_date_uu, "zzzheights")
output_tif <- file.path(output_dir,
                        paste0("veg_dist_", raster_res, "m.tif"))

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
   raster_res   = raster_res,
   chunk_size   = chunk_size,
   chunk_buffer = chunk_buffer
)
message("Done ", lubridate::now())
