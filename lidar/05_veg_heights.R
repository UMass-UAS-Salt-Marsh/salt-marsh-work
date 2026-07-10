#------------------------------------------------------------------------------#
# Vegetation height distribution driver — two-date approach
#------------------------------------------------------------------------------#
#
# Produces a multi-band GeoTIFF where each band is a height bin and the
# cell value is the fraction of summer returns in that bin above the
# spring-season ground reference.
#
# Prerequisite workflow:
#   1. lidar/02.R with date_filter <- "<spring_date>" — produces spring DTMs.
#   2. lidar/03_evaluate_dtm.R with that date — picks the best spring DTM.
#   3. This script — set spring_dtm below to the chosen DTM path.
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
chunk_size   <- 200   # tile size in metres
chunk_buffer <- 20    # buffer around each chunk in metres

site        <- "rr"
spring_date <- "2022-05-14"   # yyyy-mm-dd; used to locate summer clean dir
summer_date <- "2022-08-10"   # yyyy-mm-dd; used to locate summer clean dir

# Path to the chosen spring DTM — set this after evaluating spring DTMs
# with lidar/03_evaluate_dtm.R.  Placeholder until evaluation is done.
spring_dtm <- file.path(
   "E:/uas_scratch/lidar", site,
   gsub("-", "_", spring_date, fixed = TRUE),
   "zzzraster",
   "csf_th0.01_res0.1_rgd2_0.25m.tif"
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
                              summer_date_uu, "zzzcleaned")
output_dir <- file.path("E:/uas_scratch/lidar", site,
                        summer_date_uu, "zzzheights")
output_tif <- file.path(output_dir,
                        paste0("veg_dist_", raster_res, "m.tif"))

if (!file.exists(spring_dtm)) {
   stop("Spring DTM not found: ", spring_dtm,
        "\nRun lidar/02.R with date_filter <- \"", spring_date,
        "\" then lidar/03_evaluate_dtm.R to select a DTM.")
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
   dtm          = spring_dtm,
   output       = output_tif,
   raster_res   = raster_res,
   chunk_size   = chunk_size,
   chunk_buffer = chunk_buffer
)
message("Done ", lubridate::now())
