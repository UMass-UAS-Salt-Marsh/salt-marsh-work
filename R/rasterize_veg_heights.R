#' Rasterize a vegetation height distribution from a normalised point cloud
#'
#' Normalises summer point-cloud heights against a spring-season reference
#' DTM, bins the heights, and writes a multi-band GeoTIFF.
#' Each band corresponds to one height bin; the cell value is the fraction
#' of all returns in that cell whose normalised height falls in that bin.
#'
#' @param input Directory containing cleaned `.las` tiles
#'   (output of [clean_and_tile()]).
#' @param dtm Path to the single-band spring-season reference DTM `.tif`.
#' @param output Path for the output multi-band `.tif`.
#' @param bin_breaks Numeric vector of break points (metres) defining the
#'   height bins.
#'   Default: 5 cm bins 0–1 m then 20 cm bins 1–3 m (30 bands total).
#' @param raster_res Output raster resolution in metres. Default `0.5`.
#' @param chunk_size Tile size for [lidR::LAScatalog] processing (metres).
#'   Default `200`.
#' @param chunk_buffer Buffer around each chunk (metres). Default `20`.
#' @param verbose Print progress messages. Default `TRUE`.
#' @return Path to the output GeoTIFF (invisibly).
#'
#' @details
#' **Fraction denominator**: cell values are `count_in_bin / total_returns`
#' where the denominator is all returns in the cell (including returns
#' below ground or above the ceiling of `bin_breaks`).
#' Per-band fractions therefore need not sum to 1.
#'
#' **Band names** encode bin edges in centimetres:
#' `h000_005` = 0–5 cm, `h100_120` = 100–120 cm, etc.
#'
#' @examples
#' \dontrun{
#' rasterize_veg_heights(
#'    input  = "E:/uas_scratch/lidar/rr/2022_08_10/zzzcleaned",
#'    dtm    = paste0("E:/uas_scratch/lidar/rr/2022_05_14/zzzraster/",
#'                    "csf_th0.06_res0.10_rgd2_0.25m.tif"),
#'    output = paste0("E:/uas_scratch/lidar/rr/2022_08_10/zzzheights/",
#'                    "veg_dist_0.5m.tif")
#' )
#' }
rasterize_veg_heights <- function(
      input,
      dtm,
      output,
      bin_breaks   = c(seq(0, 1, by = 0.05), seq(1.2, 3.0, by = 0.20)),
      raster_res   = 0.5,
      chunk_size   = 200,
      chunk_buffer = 20,
      verbose      = TRUE) {

   stopifnot(grepl("\\.tif$", output, ignore.case = TRUE))
   stopifnot(file.exists(dtm))

   n_bins    <- length(bin_breaks) - 1L
   low_cm    <- round(bin_breaks[-length(bin_breaks)] * 100)
   high_cm   <- round(bin_breaks[-1L] * 100)
   bin_names <- sprintf("h%03d_%03d", low_cm, high_cm)
   ceiling_ht <- bin_breaks[n_bins + 1L]

   dtm_path <- dtm   # path string — serialises safely to workers

   ctg <- lidR::readLAScatalog(input)
   lidR::opt_chunk_size(ctg)   <- chunk_size
   lidR::opt_chunk_buffer(ctg) <- chunk_buffer
   # Write each chunk raster to disk as it completes so an interruption
   # only loses the in-progress chunk, not the whole output.
   chunk_dir <- paste0(tools::file_path_sans_ext(output), "_chunks")
   dir.create(chunk_dir, recursive = TRUE, showWarnings = FALSE)
   lidR::opt_output_files(ctg) <- file.path(chunk_dir, "{XLEFT}_{YBOTTOM}")

   n_chunks <- nrow(ctg@data)
   if (n_chunks == 0L) stop("Input catalog contains zero chunks.")
   if (verbose) {
      message(sprintf("Catalog contains %s chunks",
                      format(n_chunks, big.mark = ",")))
   }

   compute_heights <- function(las) {
      dtm_r <- terra::rast(dtm_path)
      las   <- lidR::normalize_height(las, dtm_r)

      fracs_fn <- function(z) {
         if (length(z) == 0L) {
            return(setNames(as.list(rep(NA_real_, n_bins)), bin_names))
         }
         z_pos  <- z[z >= 0 & z < ceiling_ht]
         counts <- tabulate(
            .bincode(z_pos, bin_breaks, right = FALSE,
                     include.lowest = TRUE),
            nbins = n_bins
         )
         setNames(as.list(counts / length(z)), bin_names)
      }

      lidR::pixel_metrics(las, ~fracs_fn(Z), res = raster_res)
   }

   result <- lidR::catalog_map(ctg, compute_heights)

   terra::writeRaster(result, filename = output, overwrite = TRUE)
   unlink(chunk_dir, recursive = TRUE)
   if (verbose) message("Vegetation height raster written: ", output)
   invisible(output)
}
