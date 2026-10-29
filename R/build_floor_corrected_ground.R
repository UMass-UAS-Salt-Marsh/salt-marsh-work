#' Build a floor-bias-corrected USGS ground raster
#'
#' UAS-derived ground rasters carry a positive bias against ECPs that
#' confounds two things: a roughly constant instrument/PPK-level offset
#' and a vegetation-density-dependent CSF ground-finding error (worse
#' in summer than spring — see `lidar/readme.md`'s "Systematic
#' vertical bias..." section). USGS's aerial lidar has ~no bias but
#' doesn't share that instrument offset with the UAS point cloud, so
#' using it directly as ground would leave the *entire* instrument-
#' level offset uncancelled in every vegetation-height measurement.
#'
#' This reprojects `usgs_path` to `target_epsg` and adds
#' `floor_bias` (an instrument-level floor estimate from
#' [estimate_floor_bias()], which excludes the vegetation-contaminated
#' part of the residual distribution) — producing a ground reference
#' that keeps USGS's superior absolute accuracy while still
#' cancelling the instrument offset.
#'
#' Extracted from `lidar/07_floor_corrected_ground.R`, which combined
#' this with tuning-only floor-bias estimation and diagnostic
#' plotting — see `dev/worklog.md`, 2026-09-15. Those steps stay in
#' `lidar/tuning/02_ground_reference_selection.R`; this function is
#' just the raster build, shared with production.
#'
#' @param usgs_path Path to the USGS bare-earth tile (or
#'    already-mosaicked raster) to correct.
#' @param floor_bias Floor bias estimate (meters) to add, e.g.
#'    `estimate_floor_bias(...)$floor_bias`. Pass `0` to use `usgs_path`
#'    as ground unmodified (just reprojected) -- the
#'    `ground_reference.method == "usgs"` case in
#'    [build_ground_raster()], for sites where the UAS-vs-USGS bias
#'    doesn't look like the instrument-level offset this function
#'    otherwise corrects for.
#' @param output Output raster path.
#' @param target_epsg Target horizontal CRS EPSG code (e.g. `6491L`).
#'    USGS tiles are published in EPSG:6348
#'    (NAD83(2011)/UTM19N) — a different, but equally valid,
#'    NAD83(2011) projection than this project's state-plane standard,
#'    so this reprojects to keep the raster in the same CRS as the
#'    point cloud it will be paired with.
#' @param overwrite If `FALSE` (default) and `output` already exists,
#'    skip rebuilding it.
#'
#' @return Path to the output raster (invisibly).
#'
#' @examples
#' \dontrun{
#' build_floor_corrected_ground(
#'    usgs_path = pathtools::get_path("usgs_tile", tile = "19TDG412612"),
#'    floor_bias   = 0.098,
#'    output       = pathtools::get_path("corrected_ground_raster", ...),
#'    target_epsg  = 6491L
#' )
#' }
build_floor_corrected_ground <- function(usgs_path, floor_bias, output,
                                         target_epsg, overwrite = FALSE) {

   output <- ensure_parent(output)

   if (file.exists(output) && !overwrite) {
      message("build_floor_corrected_ground(): output exists, skipping: ",
              output)
      return(invisible(output))
   }

   usgs_raw <- terra::rast(usgs_path)
   usgs_reprojected <- terra::project(usgs_raw,
                                         paste0("EPSG:", target_epsg))
   usgs_plus_floor <- usgs_reprojected + floor_bias

   terra::writeRaster(usgs_plus_floor, output, overwrite = TRUE)

   invisible(output)
}
