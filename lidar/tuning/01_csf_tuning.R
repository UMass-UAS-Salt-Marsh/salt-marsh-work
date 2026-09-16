#------------------------------------------------------------------------------#
# CSF tuning driver — one flight
#------------------------------------------------------------------------------#
#
# Grid-searches Cloth Simulation Filter (CSF) ground-classification
# parameters for one site/flight and scores each candidate DTM against
# elevation control points (ECPs). Combines the tuning grid that used
# to live in `lidar/02.R` with the evaluation that used to live in
# `lidar/03_evaluate_dtm.R` — see `dev/worklog.md`, 2026-09-15, for the
# tuning/production driver split this replaces.
#
# Steps:
#   1. `prepare_flight()` — conditional reprojection + `clean_and_tile()`
#      (both skip-if-exists).
#   2. Ground-rasterization tuning loop. For each row of `csf_grid`,
#      `rasterize_ground()` classifies ground via CSF and interpolates
#      a DTM (`knnidw`). Skips individual DTMs that already exist.
#   3. Sample every candidate DTM at ECP locations (skip-if-exists
#      cache) and render the DTM evaluation report via `report_dtms()`.
#
# This script *produces* `best_csf` — it does not read `runs.yml`.
# Ends with you reviewing the rendered report and hand-editing the
# winning parameter set into `lidar/runs.yml` under this flight's
# `best_csf`.
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
#  Run-level parameters
#------------------------------------------------------------------------------#

# Tune these for the machine and dataset; everything below uses them.
# Memory note: this pipeline is memory-bound, not CPU-bound; tuning
# the worker count and chunk size matters more than core count alone.
workers      <- 10   # parallel workers for plan(multisession)
chunk_size   <- 200  # tile size in meters
chunk_buffer <- 20   # buffer read around each chunk in meters

site <- "rr" # 2- or 3-character lowercase site code (e.g. "rr", "nor")

# Date string in "yyyy_mm_dd" format identifying which flight to tune
# (see the `raw_lidar` entry in lidar/paths.yml).
date <- "2022_08_10"

# Target horizontal CRS for reprojection. 6491 = NAD83(2011) /
# Massachusetts Mainland State Plane, this project's current standard
# (see CRS.md at the project root).
target_epsg <- 6491L

# CSF parameter sets to tune. Output DTM filenames embed the values
# via the `ground_raster` entry in `lidar/paths.yml`.
csf_grid <- data.frame(
   csf_res       = c(0.05, 0.1, 0.1, 0.2),
   csf_threshold = c(0.005, 0.01, 0.06, 0.12),
   csf_rigidness = c(2, 2, 2, 2),
   raster_res    = 0.25
)

tolerances <- c(0.10, 0.20)

if (!grepl("^[[:digit:]]{4}_[[:digit:]]{2}_[[:digit:]]{2}$", date)) {
   stop("Expected yyyy_mm_dd date format for output paths")
}
if (!(site == tolower(site) && nchar(site) >= 2 && nchar(site) <= 3)) {
   stop("Expected 2- or 3-character lowercase site code")
}

#------------------------------------------------------------------------------#
# Step 1: prepare the point cloud
#------------------------------------------------------------------------------#

flight <- prepare_flight(site, date, target_epsg = target_epsg,
                         workers = workers, chunk_size = chunk_size,
                         chunk_buffer = chunk_buffer)

#------------------------------------------------------------------------------#
# Step 2: CSF tuning loop
#------------------------------------------------------------------------------#

csf_results <- data.frame()
for (i in seq_len(nrow(csf_grid))) {
   csf_results[i, ] <- NA
   csf_results$site[i] <- site
   csf_results$date[i] <- date

   params <- as.list(csf_grid[i, , drop = FALSE]) # csf parameters

   for (n in names(params)) {
      csf_results[[n]][i] <- params[[n]]
   }

   output_path <- ensure_parent(get_path(
      "ground_raster", site = site, date = date, target_epsg = target_epsg,
      csf_threshold = params$csf_threshold, csf_res = params$csf_res,
      csf_rigidness = params$csf_rigidness, raster_res = params$raster_res
   ))

   csf_results$dtm[i] <- output_path

   # Full argument list for rasterize_ground
   args <- c(list(input = flight$cleaned_dir,
                  output = output_path,
                  chunk_size = chunk_size,
                  chunk_buffer = chunk_buffer),
             params)

   if (!file.exists(output_path)) {
      message("Starting ground rasterization ", lubridate::now())
      ground <- do.call(rasterize_ground, args)
      message("Done ground rasterization ", lubridate::now())
   } else {
      message("Skipping DTM. ", output_path, " already exists. ")
   }

   # Write metadata for every grid combo, not just the eventual
   # winner -- skip-if-exists, so this also backfills a sidecar for a
   # DTM produced by an earlier run of this script.
   write_output_metadata(
      output       = output_path,
      created_by   = "rasterize_ground",
      source_cloud = flight$raw_cloud,
      crs          = paste0("EPSG:", target_epsg),
      inputs       = list(cleaned_tiles = flight$cleaned_dir),
      params       = params
   )
}

#------------------------------------------------------------------------------#
# Step 3: sample every candidate DTM at ECPs, render evaluation report
#------------------------------------------------------------------------------#

# Warm the per-DTM CSV cache (`<dtm-stem>_ecp.csv`) so report_dtms()
# opens fast. `sample_dtm()` is skip-if-exists; re-running this block
# is cheap. `load_ecp()` cleans, filters, and returns an sf object.
# Logger Array excluded; Berm, EVP, and Training kept.
site_ecp <- load_ecp(get_path("ecp_path"), site = site,
                     target_crs = target_epsg)

for (i in seq_len(nrow(csf_results))) {
   ecp_cache <- get_path(
      "ground_raster_ecp_cache", site = site, date = date,
      target_epsg = target_epsg,
      csf_threshold = csf_results$csf_threshold[i],
      csf_res       = csf_results$csf_res[i],
      csf_rigidness = csf_results$csf_rigidness[i],
      raster_res    = csf_results$raster_res[i]
   )
   sample_dtm(csf_results$dtm[i], site_ecp, output_csv = ecp_cache)
}

report_dtms(
   site        = site,
   date        = date,
   target_epsg = target_epsg,
   tolerances  = tolerances
)

message("Review the DTM evaluation report above, then hand-edit the ",
        "winning CSF parameter set into lidar/runs.yml under ",
        "sites.", site, ".flights.", date, ".best_csf.")
