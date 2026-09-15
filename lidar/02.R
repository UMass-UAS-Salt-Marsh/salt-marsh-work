#------------------------------------------------------------------------------#
# Lidar pipeline driver — one site, full point cloud, CSF tuning
#------------------------------------------------------------------------------#
#
# End-to-end driver for the per-site lidar workflow.  Four steps:
#
#   1. Conditional reprojection.  Source clouds tagged
#      WGS 84 + WGS 84 ellipsoidal heights are reprojected to
#      NAD83(2011) / Massachusetts Mainland State Plane (EPSG:6491)
#      + NAVD 88 (via GEOID18) using `reproject_las()`
#      (PDAL by default; LAStools alternative).  Skipped automatically
#      if the source is already in the target CRS.  See `CRS.md` at
#      the project root for the CRS/datum/geoid standard and
#      `lidar/readme.md` "CRS, datum, and frame shift considerations"
#      for the full geodetic discussion.
#
#   2. Clean & tile (`clean_and_tile()`).  Filters to last return,
#      removes noise via `lidR::sor()`, writes cleaned tiles to
#      `cleaned_epsg<target_epsg>/`.  Skips if the
#      tiles already exist.
#
#   3. Ground-rasterization tuning loop.  For each row of `csf_grid`,
#      calls `rasterize_ground()` to classify ground via Cloth
#      Simulation Filter and interpolate a DTM (`knnidw`).  Outputs go
#      to `ground_rasters_epsg<target_epsg>/csf_th*_res*_rgd*_*m.tif`.
#      Skips individual DTMs that already exist.
#
#   4. (Partial) DTM evaluation against elevation control points.
#      Reads the ECP xlsx, filters to the current site.  The full
#      per-parameter-set comparison via `evaluate_dtm()` is the
#      Phase 1 work scoped in `dev/workplan.md`; the code at the
#      bottom of this file is the current scaffold.
#
# Inputs
#   * `lidar/data/paths.yml` — pathtools scheme: source clouds, ECP
#     file, and every scratch/output path template.
#   * `csf_grid` (declared below) — CSF parameter sets to tune.
#
# Outputs (under `E:/uas_scratch/lidar/<site>/<date>/`)
#   * `reprojected/<basename>_epsg<target_epsg>_navd88.las`
#   * `cleaned_epsg<target_epsg>/*.las` — cleaned, tiled point cloud.
#   * `ground_rasters_epsg<target_epsg>/csf_*.tif` — one DTM per `csf_grid`
#     row.  Directories are namespaced by `target_epsg` so switching
#     the CRS standard doesn't silently reuse or collide with tiles
#     produced under a previous standard.
#
# Run from the project root in RStudio so relative paths resolve.
#------------------------------------------------------------------------------#

library(lidR)
library(future)
library(progressr)
library(pathtools)
progressr::handlers(global = TRUE)
progressr::handlers("cli")

# Memory note.  This pipeline is memory-bound, not CPU-bound;
# tuning the worker count and chunk size matters more than core
# count alone.  Empirical history on this machine:
#   - workers 8 with chunk_size 400 m yielded 9 chunks for NOR.
#     Conservative, leaves memory headroom but slow.
#   - workers 20 with chunk_size 200 m yielded 25 chunks for rr.
#     Runs fast and does not stress memory.
#   - workers 25 with chunk_size 200 m is the current default
#     (roughly one worker per rr chunk).
# If `chunk_size` or `workers` below is changed, reconsider the
# other.


#------------------------------------------------------------------------------#
#  Run-level parameters
#------------------------------------------------------------------------------#

# Tune these for the machine and dataset; everything below uses them.
# See the comment block above for the history behind the worker /
# chunk-size values.
workers      <- 10   # parallel workers for plan(multisession)
chunk_size   <- 200  # tile size in meters
chunk_buffer <- 20   # buffer read around each chunk in meters

site <- "rr" # 2- or 3-character lowercase site code (e.g. "rr", "nor")

# Date string in "yyyy_mm_dd" format identifying which cloud to use
# for this site (see the `raw_lidar` entry in lidar/data/paths.yml).
# rr: "2022_05_14" and "2022_08_10"
date <- "2022_08_10"

# Target horizontal CRS for reprojection.  6491 = NAD83(2011) /
# Massachusetts Mainland State Plane, this project's current standard
# (provisional; see CRS.md at the project root).  Output directories
# below are namespaced by this value so re-running under a different
# standard doesn't collide with or silently reuse prior output.
target_epsg <- 6491L

# CSF (Cloth Simulation Filter) tuning grid.  Each row is one
# ground-classification parameter set that `rasterize_ground()` will
# run; output DTM filenames embed the values via the `ground_raster`
# entry in `lidar/data/paths.yml`.
#
# Reverted to the original 4-parameter grid for the EPSG:6491 re-run
# (2026-08-20) -- keeps this an apples-to-apples check of whether the
# CRS switch changed bias/RMSE, rather than also reopening the
# parameter search.  The 18-run expanded grid (3 resolutions x
# 3 thresholds x 2 rigidness values) was added 2026-05-20 but never
# actually run to completion for `rr`; revisit it separately if the
# original 4 aren't sufficient going forward.
csf_grid <- data.frame(
   csf_res       = c(0.05, 0.1, 0.1, 0.2),
   csf_threshold = c(0.005, 0.01, 0.06, 0.12),
   csf_rigidness = c(2, 2, 2, 2),
   raster_res    = 0.25
)

plan(multisession, workers = workers)


# Source functions
invisible(lapply(list.files("R/", pattern = "\\.[Rr]$",
                            full.names = TRUE), source))

set_path_scheme("lidar/data/paths.yml")

# Validate date and site
if (!grepl("^[[:digit:]]{4}_[[:digit:]]{2}_[[:digit:]]{2}$", date)) {
   stop("Expected yyyy_mm_dd date format for output paths")
}
if (!(site == tolower(site) && nchar(site) >= 2 && nchar(site) <= 3)) {
   stop("Expected 2- or 3-character lowercase site code")
}


# Resolve paths for this analysis (everything under scratch_root is on
# the local RAID; see lidar/data/paths.yml).
paths <- list()
paths$input <- get_path("raw_lidar", site = site, date = date)
paths$ecp   <- get_path("ecp_path")

# Kept separate from `paths$input` (which gets reassigned to the
# reprojected path below, if reprojection happens) so output metadata
# can always trace back to the true original raw cloud.
raw_cloud_path <- paths$input

paths$cleaned_catalog_dir <- get_path(
   "cleaned_tiles", site = site, date = date, target_epsg = target_epsg
)
paths$reprojected_dir <- get_path(
   "reprojected_dir", site = site, date = date
)
paths$reprojected_path <- get_path(
   "reprojected_las", site = site, date = date, target_epsg = target_epsg,
   orig_stem = tools::file_path_sans_ext(basename(paths$input))
)

# Create output dirs
dir.create(paths$cleaned_catalog_dir, recursive = TRUE,
           showWarnings = FALSE)

#------------------------------------------------------------------------------#
# RUN
#------------------------------------------------------------------------------#


# Conditional reprojection — see Step 1 in the file header for
# the rationale.  `reproject_las()` is idempotent (skip-if-exists),
# so re-runs of this script are cheap.
if (las_needs_reprojection(paths$input, target_epsg = target_epsg)) {

   dir.create(paths$reprojected_dir, recursive = TRUE,
              showWarnings = FALSE)
   paths$input <- reproject_las(paths$input, paths$reprojected_path,
                                target_epsg = target_epsg)
}


# Create cleaned tiles - once per site.  Everything else will use these
clean_and_tile(paths$input, paths$cleaned_catalog_dir,
               chunk_size = chunk_size, chunk_buffer = chunk_buffer)


# Find ground with several parameters - output to raster

# Make a raster for each parameter set
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
   args <- c(list(input = paths$cleaned_catalog_dir,
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
   # winners -- skip-if-exists, so this also backfills a sidecar for
   # a DTM produced by an earlier run of this script.
   write_output_metadata(
      output       = output_path,
      created_by   = "rasterize_ground",
      source_cloud = raw_cloud_path,
      crs          = paste0("EPSG:", target_epsg),
      inputs       = list(cleaned_tiles = paths$cleaned_catalog_dir),
      params       = params
   )
}


# -- Phase 1 prep: sample DTMs at ECP locations ------------------------
# Warm the per-DTM CSV cache (`<dtm-stem>_ecp.csv`) at the end of this
# long-running pipeline so the formal evaluation in
# `lidar/03_evaluate_dtm.R` opens fast.  `sample_dtm()` is
# skip-if-exists; re-running this block is cheap.
#
# `load_ecp()` cleans, filters, and returns an sf object.
# Logger Array excluded; Berm, EVP, and Training kept.

site_ecp <- load_ecp(paths$ecp, site = site, target_crs = target_epsg)

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
