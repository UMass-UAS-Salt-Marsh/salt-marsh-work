#' Rasterize a top-of-canopy surface from a cleaned point cloud
#'
#' Companion to [rasterize_ground()], for the opposite surface: an
#' absolute-elevation digital surface model (not ground-normalized)
#' giving a high percentile of raw `Z` per pixel. Used to validate
#' vegetation-height accuracy against field-measured heights at ECP
#' locations (via [sample_dtm()]), so it deliberately takes the same
#' cleaned-tile input that the production vegetation-height pipeline
#' ([rasterize_veg_heights()]) uses, rather than an unfiltered cloud —
#' otherwise the validation wouldn't reflect what that pipeline
#' actually delivers.
#'
#' Chunk rasters are written to disk as each chunk completes so that
#' an interruption only loses the current chunk, not the whole
#' surface. They are merged into `output` and cleaned up on success.
#'
#' @param input Directory containing cleaned `.las` tiles
#'   (output of [clean_and_tile()]).
#' @param output Path for the output GeoTIFF.
#' @param top_percentile Percentile (0-1) of per-pixel `Z` values to
#'   use as the canopy-top estimate. Default `1` (max).
#' @param raster_res Output raster resolution in metres. Default
#'   `0.25`.
#' @param verbose Print progress messages. Default `TRUE`.
#' @param chunk_size Tile size for [lidR::LAScatalog] processing (m).
#'   Default `200`.
#' @param chunk_buffer Buffer around each chunk (m). Default `20`.
#'
#' @examples
#' \dontrun{
#' rasterize_canopy_top(
#'    input  = pathtools::get_path("cleaned_tiles", site = "rr",
#'                                 date = "2022_08_10", target_epsg = 6491),
#'    output = pathtools::get_path("canopy_top_raster", site = "rr",
#'                                 date = "2022_08_10", target_epsg = 6491)
#' )
#' }
rasterize_canopy_top <- function(
      input,
      output,
      top_percentile = 1,
      raster_res      = 0.25,
      verbose         = TRUE,
      chunk_size      = 200,
      chunk_buffer    = 20) {

   stopifnot(grepl("\\.tif$", output, ignore.case = TRUE))

   ctg <- lidR::readLAScatalog(input)
   lidR::opt_chunk_size(ctg)   <- chunk_size
   lidR::opt_chunk_buffer(ctg) <- chunk_buffer

   # Write each chunk raster to disk as it completes so an interruption
   # only loses the in-progress chunk, not the whole surface.
   chunk_dir <- paste0(tools::file_path_sans_ext(output), "_chunks")
   dir.create(chunk_dir, recursive = TRUE, showWarnings = FALSE)
   lidR::opt_output_files(ctg) <- file.path(chunk_dir, "{XLEFT}_{YBOTTOM}")

   n_chunks <- nrow(ctg@data)
   if (n_chunks == 0L) stop("Input catalog contains zero chunks.")
   if (verbose) {
      message(sprintf("Catalog contains %s chunks",
                      format(n_chunks, big.mark = ",")))
   }

   top_z <- function(z) {
      as.numeric(stats::quantile(z, probs = top_percentile,
                                 na.rm = TRUE, names = FALSE))
   }

   canopy_top <- lidR::catalog_map(
      ctg, \(las) lidR::pixel_metrics(las, ~top_z(Z), res = raster_res)
   )

   terra::writeRaster(canopy_top, filename = output, overwrite = TRUE)
   unlink(chunk_dir, recursive = TRUE)
   if (verbose) message("Canopy-top raster written: ", output)
   invisible(output)
}
