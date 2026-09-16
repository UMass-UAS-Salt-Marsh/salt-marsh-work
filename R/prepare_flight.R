#' Prepare a flight's point cloud for rasterization
#'
#' Shared first step for every lidar driver: starts the
#' `future::multisession` worker pool, then conditionally reprojects
#' the raw cloud to `target_epsg` (skipped if it's already there — see
#' [las_needs_reprojection()]) and cleans/tiles it via
#' [clean_and_tile()]. Both steps are skip-if-exists, so calling this
#' more than once for the same site/date is cheap.
#'
#' Extracted from `lidar/02.R` and `lidar/05_veg_heights.R`, which
#' duplicated this logic — see `dev/worklog.md`, 2026-09-15.
#'
#' @param site Lidar site code (e.g. `"rr"`).
#' @param date Flight date string in `"yyyy_mm_dd"` format.
#' @param target_epsg Target horizontal CRS EPSG code (e.g. `6491L`).
#' @param workers Parallel workers for `plan(multisession)`.
#' @param chunk_size Tile size in meters for [clean_and_tile()].
#' @param chunk_buffer Buffer read around each chunk in meters for
#'    [clean_and_tile()].
#'
#' @return A list with `cleaned_dir` (path to the cleaned/tiled point
#'    cloud, ready for rasterization) and `raw_cloud` (path to the
#'    original, un-reprojected raw cloud — for callers that need to
#'    record true provenance in [write_output_metadata()]).
#'
#' @examples
#' \dontrun{
#' flight <- prepare_flight("rr", "2022_08_10", target_epsg = 6491L,
#'                          workers = 25, chunk_size = 100,
#'                          chunk_buffer = 20)
#' }
prepare_flight <- function(site, date, target_epsg, workers,
                           chunk_size, chunk_buffer) {

   future::plan(future::multisession, workers = workers)

   raw_cloud <- pathtools::get_path("raw_lidar", site = site, date = date)

   input <- raw_cloud
   if (las_needs_reprojection(input, target_epsg = target_epsg)) {

      reprojected_dir <- pathtools::get_path("reprojected_dir",
                                             site = site, date = date)
      dir.create(reprojected_dir, recursive = TRUE, showWarnings = FALSE)

      reprojected_path <- pathtools::get_path(
         "reprojected_las", site = site, date = date,
         target_epsg = target_epsg,
         orig_stem = tools::file_path_sans_ext(basename(input))
      )
      input <- reproject_las(input, reprojected_path,
                             target_epsg = target_epsg)
   }

   cleaned_dir <- pathtools::get_path("cleaned_tiles", site = site,
                                      date = date, target_epsg = target_epsg)
   dir.create(cleaned_dir, recursive = TRUE, showWarnings = FALSE)

   clean_and_tile(input, cleaned_dir, chunk_size = chunk_size,
                  chunk_buffer = chunk_buffer)

   list(cleaned_dir = cleaned_dir, raw_cloud = raw_cloud)
}
