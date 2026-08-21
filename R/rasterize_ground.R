#' Classify ground and rasterize a cleaned point cloud into a DTM
#'
#' Classifies ground returns in a cleaned tile catalog using the Cloth
#' Simulation Filter (CSF), then interpolates a digital terrain model
#' via k-NN IDW and writes it as a single-band GeoTIFF.
#'
#' Chunk rasters are written to disk as each chunk completes so that
#' an interruption only loses the current chunk, not the whole DTM.
#' They are merged into `output` and cleaned up on success.
#'
#' @param input Directory containing cleaned `.las` tiles
#'   (output of [clean_and_tile()]).
#' @param output Path for the output GeoTIFF.
#' @param csf_threshold CSF `class_threshold` — maximum distance (m)
#'   from the cloth for a point to be classified as ground.
#'   Default `0.01`.
#' @param csf_res CSF `cloth_resolution` — node spacing (m) of the
#'   simulated cloth. Default `0.01`.
#' @param csf_rigidness CSF `rigidness` parameter (1–3). Default `2`.
#' @param iterations Maximum CSF iterations. Default `500`.
#' @param raster_res Output raster resolution in metres. Default `0.25`.
#' @param verbose Print progress messages. Default `TRUE`.
#' @param chunk_size Tile size for [lidR::LAScatalog] processing (m).
#'   Default `200`.
#' @param chunk_buffer Buffer around each chunk (m). Default `20`.
#'
#' @examples
#' \dontrun{
#' rasterize_ground(
#'    input  = "E:/uas_scratch/lidar/rr/2022_08_10/zzzcleaned",
#'    output = paste0("E:/uas_scratch/lidar/rr/2022_08_10/zzzraster/",
#'                    "csf_th0.06_res0.10_rgd2_0.25m.tif"),
#'    csf_threshold = 0.06,
#'    csf_res       = 0.10,
#'    csf_rigidness = 2
#' )
#' }
rasterize_ground <- function(
      input,
      output,
      csf_threshold = 0.01,
      csf_res       = 0.01,
      csf_rigidness = 2,
      iterations    = 500,
      raster_res    = 0.25,
      verbose       = TRUE,
      chunk_size    = 200,
      chunk_buffer  = 20) {

   stopifnot(grepl("\\.tif$", output))

   ctg <- readLAScatalog(input)
   opt_chunk_size(ctg)   <- chunk_size
   opt_chunk_buffer(ctg) <- chunk_buffer

   # Write each chunk raster to disk as it completes so an interruption
   # only loses the in-progress chunk, not the whole DTM.
   chunk_dir <- paste0(tools::file_path_sans_ext(output), "_chunks")
   dir.create(chunk_dir, recursive = TRUE, showWarnings = FALSE)
   opt_output_files(ctg) <- file.path(chunk_dir, "{XLEFT}_{YBOTTOM}")

   n_chunks <- nrow(ctg@data)
   if (n_chunks == 0L) stop("Input catalog contains zero chunks.")
   if (verbose) {
      message(sprintf("Catalog contains %s chunks",
                      format(n_chunks, big.mark = ",")))
   }

   rast_ground <- function(las) {
      ground_class <- classify_ground(
         las,
         last_returns = TRUE,
         algorithm    = csf(
            class_threshold  = csf_threshold,
            cloth_resolution = csf_res,
            rigidness        = csf_rigidness
         )
      )
      rasterize_terrain(
         las       = ground_class,
         res       = raster_res,
         pkg       = "terra",
         algorithm = knnidw(k = 10, p = 2, rmax = 0.5)
      )
   }

   dtm <- catalog_map(ctg, rast_ground)

   terra::writeRaster(dtm, filename = output, overwrite = TRUE)
   unlink(chunk_dir, recursive = TRUE)
}
