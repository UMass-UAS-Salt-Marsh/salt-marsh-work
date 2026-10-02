#' Sink-fill a raster and compute a fill-height raster
#'
#' Fills topographic depressions ("sinks" or "pits") in `input` using
#' WhiteboxTools' `FillDepressions` algorithm (Wang & Liu, 2006), via
#' the `whitebox` R package, and computes a second raster recording
#' how much each cell was raised: `filled - original`, clamped to
#' `>= 0` to absorb floating-point noise.
#'
#' @details
#' WhiteboxTools operates on files on disk, not in-memory objects.
#' When `input` is a `terra::SpatRaster` it is first written to a
#' temporary GeoTIFF; that temp file is removed on exit either way.
#'
#' Requires a working WhiteboxTools installation:
#' `install.packages("whitebox")` followed by a one-time
#' `whitebox::install_whitebox()` to fetch the binary.
#'
#' This function makes no assumption about how `input` was produced.
#' In particular, road/culvert breaching -- burning flow lines under
#' roads so they don't act as dams -- is anticipated future work as a
#' preprocessing step upstream of this function, not implemented here.
#'
#' @param input Either a `terra::SpatRaster` (single-band) or a path
#'    to one.
#' @param output Path for the sink-filled output GeoTIFF.
#' @param fill_height_output Path for the fill-height output GeoTIFF
#'    (`filled - original`, clamped to `>= 0`).
#' @param fix_flats Passed to [whitebox::wbt_fill_depressions()]: add
#'    a small gradient across flat areas so downstream flow-direction
#'    calculations don't stall on them. Default `TRUE`.
#' @param flat_increment Passed to [whitebox::wbt_fill_depressions()]:
#'    elevation increment applied to flats when `fix_flats = TRUE`.
#'    Default `NULL` (WhiteboxTools picks one automatically).
#' @param max_depth Passed to [whitebox::wbt_fill_depressions()]:
#'    maximum depression depth to fill, in the input raster's
#'    z-units. Default `NULL` (no limit -- all depressions are
#'    filled).
#' @param overwrite If `FALSE` (default) and both `output` and
#'    `fill_height_output` already exist, skip recomputing them.
#' @param verbose Print progress messages. Default `TRUE`.
#'
#' @return `c(output, fill_height_output)` (invisibly).
#'
#' @examples
#' \dontrun{
#' ground_raster <- paste0(
#'    "E:/uas_scratch/lidar/rr/2022_05_14/ground_rasters_epsg6491/",
#'    "csf_th0.06_res0.10_rgd2_0.25m.tif"
#' )
#' fill_sinks(
#'    input              = ground_raster,
#'    output             = "E:/uas_scratch/hydro/rr/dtm_filled.tif",
#'    fill_height_output = "E:/uas_scratch/hydro/rr/dtm_fill_height.tif"
#' )
#' }
fill_sinks <- function(
      input,
      output,
      fill_height_output,
      fix_flats      = TRUE,
      flat_increment = NULL,
      max_depth      = NULL,
      overwrite      = FALSE,
      verbose        = TRUE) {

   stopifnot(
      inherits(input, "SpatRaster") ||
         (is.character(input) && length(input) == 1L && file.exists(input)),
      is.character(output), length(output) == 1L,
      grepl("\\.tif$", output, ignore.case = TRUE),
      is.character(fill_height_output), length(fill_height_output) == 1L,
      grepl("\\.tif$", fill_height_output, ignore.case = TRUE)
   )

   if (!requireNamespace("whitebox", quietly = TRUE)) {
      stop("fill_sinks(): the `whitebox` package is required. Install ",
           "it with install.packages(\"whitebox\") and then run ",
           "whitebox::install_whitebox() to fetch the WhiteboxTools ",
           "binary.")
   }
   if (!whitebox::check_whitebox_binary()) {
      stop("fill_sinks(): the WhiteboxTools binary was not found. ",
           "Run whitebox::install_whitebox() to install it.")
   }

   if (file.exists(output) && file.exists(fill_height_output) &&
          !overwrite) {
      message("fill_sinks(): outputs exist, skipping: ", output,
              ", ", fill_height_output)
      return(invisible(c(output, fill_height_output)))
   }

   dir.create(dirname(output), recursive = TRUE, showWarnings = FALSE)
   dir.create(dirname(fill_height_output), recursive = TRUE,
              showWarnings = FALSE)

   if (inherits(input, "SpatRaster")) {
      original <- input
      input_path <- tempfile(fileext = ".tif")
      on.exit(unlink(input_path), add = TRUE)
      terra::writeRaster(original, input_path, overwrite = TRUE)
   } else {
      input_path <- input
      original <- terra::rast(input_path)
   }

   if (verbose) message("fill_sinks(): filling depressions in ", input_path)

   # wbt_fill_depressions() doesn't return a numeric exit code -- with
   # verbose_mode = FALSE it returns a one-line character summary
   # (e.g. "FillDepressions - Elapsed Time: ..."), or "... [did not
   # run]" if WhiteboxTools failed to launch. Either way, the
   # reliable success signal is whether `output` was actually
   # written.
   result <- whitebox::wbt_fill_depressions(
      dem            = input_path,
      output         = output,
      fix_flats      = fix_flats,
      flat_increment = flat_increment,
      max_depth      = max_depth,
      verbose_mode   = FALSE
   )

   if (!file.exists(output)) {
      stop("whitebox::wbt_fill_depressions() failed for input: ",
           input_path, "\n", paste(result, collapse = "\n"))
   }

   filled      <- terra::rast(output)
   fill_height <- terra::clamp(filled - original, lower = 0, values = TRUE)
   terra::writeRaster(fill_height, fill_height_output, overwrite = TRUE)

   if (verbose) {
      message("Sink-filled raster written: ", output)
      message("Fill-height raster written: ", fill_height_output)
   }

   invisible(c(output, fill_height_output))
}
