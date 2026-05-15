#------------------------------------------------------------------------------#
# Lidar pipeline driver — one site, full point cloud, CSF tuning
#------------------------------------------------------------------------------#
#
# End-to-end driver for the per-site lidar workflow.  Four steps:
#
#   1. Conditional reprojection.  Source clouds tagged
#      WGS 84 / UTM 19N + WGS 84 ellipsoidal heights are reprojected to
#      NAD 83 / UTM 19N + NAVD 88 (via GEOID12B) using `reproject_las()`
#      (PDAL by default; LAStools alternative).  Skipped automatically
#      if the source is already in the target CRS.  See
#      `lidar/readme.md` "CRS, datum, and frame shift considerations"
#      for the full geodetic discussion.
#
#   2. Clean & tile (`clean_and_tile()`).  Filters to last return,
#      removes noise via `lidR::sor()`, writes cleaned tiles to
#      `<base_output>/zzzcleaned/`.  Skips if the tiles already exist.
#
#   3. Ground-rasterization tuning loop.  For each row of `csf_grid`,
#      calls `rasterize_ground()` to classify ground via Cloth
#      Simulation Filter and interpolate a DTM (`knnidw`).  Outputs go
#      to `<base_output>/zzzraster/csf_th*_res*_rgd*_*m.tif`.  Skips
#      individual DTMs that already exist.
#
#   4. (Partial) DTM evaluation against elevation control points.
#      Reads the ECP xlsx, filters to the current site.  The full
#      per-parameter-set comparison via `evaluate_dtm()` is the
#      Phase 1 work scoped in `dev/work_plan.md`; the code at the
#      bottom of this file is the current scaffold.
#
# Inputs
#   * `lidar/data/paths.csv` — table of source clouds and ECP files.
#   * `csf_grid` (declared below) — CSF parameter sets to tune.
#
# Outputs (under `E:/uas_scratch/lidar/<site>/<date>/`)
#   * `reprojected/<basename>_epsg26919_navd88.las`
#   * `zzzcleaned/*.las` — cleaned, tiled point cloud.
#   * `zzzraster/csf_*.tif` — one DTM per `csf_grid` row.
#
# Run from the project root in RStudio so relative paths resolve.
#------------------------------------------------------------------------------#

library(lidR)
library(future)

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
workers      <- 25   # parallel workers for plan(multisession)
chunk_size   <- 200  # tile size in meters
chunk_buffer <- 20   # buffer read around each chunk in meters

site <- "rr" # 2- or 3-character lowercase site code (e.g. "rr", "nor")

# CSF (Cloth Simulation Filter) tuning grid.  Each row is one
# ground-classification parameter set that `rasterize_ground()` will
# run; output DTM filenames embed the values via
# `paths$ground_raster_template`.
csf_grid <- data.frame(
   csf_res       = c(0.05, 0.10, 0.10, 0.20),
   csf_threshold = c(0.005, 0.01, 0.06, 0.12),
   csf_rigidness = 2,
   raster_res    = 0.25
)

plan(multisession, workers = workers)


# Source functions
invisible(lapply(list.files("R/", pattern = "\\.[Rr]$",
                            full.names = TRUE), source))


# Read table of available input files
input_file_paths <- readr::read_csv("lidar/data/paths.csv")
input_file_paths$file_size <- file.size(input_file_paths$path)
input_file_paths$path <- gsub("\\\\", "/", input_file_paths$path)
input_file_paths$preferred <- as.logical(input_file_paths$preferred)

# Filter to preferred files
input_file_paths <- input_file_paths[input_file_paths$preferred, ]

# Cleanup leading and trailing junk
input_file_paths$path <- gsub("^[[:blank:]/\"\\\\]+|[[:blank:]/\"\\\\]+$",
                              "", input_file_paths$path)


# Set paths for this analysis
paths <- list()



# Select input file.  Could loop to process each cloud at each site;
# for now just take the first match.
possible_input_rows <- which(input_file_paths$site == site &
                                input_file_paths$type == "cloud")
input_row <- possible_input_rows[1]

paths$input <- input_file_paths$path[input_row]

date <- input_file_paths$date[input_row] # Extract associated date
date <- gsub("-", "_", date, fixed = TRUE)

# Validate date and site
if (!grepl("^[[:digit:]]{4}_[[:digit:]]{2}_[[:digit:]]{2}$", date)) {
   stop("Expected yyyy_mm_dd date format for output paths")
}
if (!(site == tolower(site) && nchar(site) >= 2 && nchar(site) <= 3)) {
   stop("Expected 2- or 3-character lowercase site code")
}


# output paths (everything under base_output is on the local RAID)
paths$base_output <- file.path("E:/uas_scratch/lidar", site, date)
paths$cleaned_catalog_dir <- file.path(paths$base_output, "zzzcleaned")
paths$ground_raster_template <- file.path(
   paths$base_output,
   paste0("zzzraster/csf_th[csf_threshold]_res[csf_res]",
          "_rgd[csf_rigidness]_[raster_res]m.tif")
)
paths$reprojected_dir <- file.path(paths$base_output, "reprojected")
paths$reprojected_path <- file.path(
   paths$reprojected_dir,
   sub("\\.las$", "_epsg26919_navd88.las",
       basename(paths$input), ignore.case = TRUE)
)


paths$ecp <- input_file_paths$path[
   input_file_paths$site == "all" & input_file_paths$type == "ecp"
][1]


# Create output dirs
dir.create(dirname(paths$ground_raster_template), recursive = TRUE,
           showWarnings = FALSE)
dir.create(paths$cleaned_catalog_dir, recursive = TRUE,
           showWarnings = FALSE)

#------------------------------------------------------------------------------#
# RUN
#------------------------------------------------------------------------------#


# Conditional reprojection — see Step 1 in the file header for
# the rationale.  `reproject_las()` is idempotent (skip-if-exists),
# so re-runs of this script are cheap.
if (las_needs_reprojection(paths$input)) {

   dir.create(paths$reprojected_dir, recursive = TRUE,
              showWarnings = FALSE)
   paths$input <- reproject_las(paths$input, paths$reprojected_path,
                                target_epsg = 26919)
}


# Create cleaned tiles - once per site.  Everything else will use these
clean_and_tile(paths$input, paths$cleaned_catalog_dir,
               chunk_size = chunk_size, chunk_buffer = chunk_buffer)


# Find ground with several parameters - output to raster
# update_path() comes from R/update_path.R.

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


   output_path <- update_path(paths$ground_raster_template, params)

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
}


# Assess errors
# clean_column_names(), clean_dates(), visualize_dtm(), and evaluate_dtm()
# all come from R/.

# Read elevation control points
ecp <- readxl::read_xlsx(paths$ecp) |> clean_column_names()
ecp$date <- clean_dates(ecp$date)
ecp$site <- tolower(ecp$site)

i <- 1

site_dtm <- csf_results$dtm[i]
site_ecp <- ecp[ecp$site == site & !ecp$type %in% "Logger Array", ]
