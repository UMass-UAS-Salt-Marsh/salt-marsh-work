#' Sample a DTM at ECP locations and cache the result
#'
#' Bilinearly samples a digital terrain model at each elevation
#' control point's easting / northing,
#' returns the ECP data frame augmented with a `predicted`
#' column,
#' and caches the result to a CSV next to the DTM
#' (skip-if-exists pattern matching
#' [`clean_and_tile()`] and [`reproject_las()`]).
#'
#' The function does **not** compute residuals.
#' `residual` and `abs_residual` are derived inside
#' [`evaluate_dtm()`] from the cache,
#' so `sample_dtm()`'s return value matches the on-disk CSV
#' exactly.
#' Keeping the cache close to raw data and concentrating all
#' elevation derivation in the reporting function makes it
#' easier to iterate on metric definitions without re-sampling
#' rasters.
#'
#' @param dtm Either a `terra::SpatRaster` (single-band) or a
#'    path to a single-band GeoTIFF.
#' @param ecp A data frame or `sf` object of elevation control
#'    points.
#'    Must contain `easting` and `northing` columns
#'    (or, if an `sf` object,
#'    geometry from which coordinates can be extracted).
#'    All other columns are passed through to the output.
#' @param output_csv Path for the cached CSV.
#'    If `NULL` (default) and `dtm` is a path,
#'    derived from `dtm` by swapping the extension for
#'    `_ecp.csv`
#'    (e.g., `foo.tif` → `foo_ecp.csv` next to the DTM).
#'    Required if `dtm` is a `SpatRaster`
#'    (no path to derive from).
#' @param overwrite If `FALSE` (default) and `output_csv`
#'    already exists,
#'    read and return it without sampling
#'    (no `terra::extract()` call).
#'    If `TRUE`, re-sample and overwrite.
#'
#' @return A data frame with all input ECP columns
#'    (geometry dropped if input was `sf`)
#'    plus a `predicted` column.
#'
#' @examples
#' \dontrun{
#' elevations <- sample_dtm(
#'    dtm = paste0(
#'       "E:/uas_scratch/lidar/rr/2022_08_10/zzzraster/",
#'       "csf_th0.005_res0.05_rgd2_0.25m.tif"
#'    ),
#'    ecp = site_ecp
#' )
#' }
sample_dtm <- function(dtm,
                       ecp,
                       output_csv = NULL,
                       overwrite = FALSE) {

   stopifnot(
      is.data.frame(ecp) || inherits(ecp, "sf")
   )

   # Derive output CSV path from `dtm` when not supplied.
   if (is.null(output_csv)) {
      if (!is.character(dtm) || length(dtm) != 1L) {
         stop("sample_dtm(): when `dtm` is not a file path, ",
              "`output_csv` must be supplied.")
      }
      output_csv <- sub("\\.tif$", "_ecp.csv", dtm,
                        ignore.case = TRUE)
   }

   if (file.exists(output_csv) && !overwrite) {
      message("sample_dtm(): output exists, skipping: ", output_csv)
      return(as.data.frame(readr::read_csv(
         output_csv, show_col_types = FALSE
      )))
   }

   # Resolve the raster.
   if (is.character(dtm) && length(dtm) == 1L && file.exists(dtm)) {
      dtm <- terra::rast(dtm)
   }
   if (!inherits(dtm, "SpatRaster") || terra::nlyr(dtm) != 1L) {
      stop("sample_dtm(): `dtm` must be a single-band ",
           "SpatRaster or a path to one.")
   }

   # Extract coordinates.  Accept either an sf object
   # (use the geometry) or a data frame with easting / northing.
   if (inherits(ecp, "sf")) {
      coords <- sf::st_coordinates(ecp)[, 1:2, drop = FALSE]
      out    <- sf::st_drop_geometry(ecp)
      if (!"easting" %in% colnames(out)) {
         out$easting  <- coords[, 1]
         out$northing <- coords[, 2]
      }
      # Convert to SpatVector so terra reprojects to the DTM's CRS
      # before sampling — avoids silent misalignment when the ECP and
      # raster are in different coordinate systems.
      predicted <- terra::extract(
         dtm, terra::vect(ecp), method = "bilinear"
      )[, 2]
   } else {
      if (!all(c("easting", "northing") %in% colnames(ecp))) {
         stop("sample_dtm(): `ecp` must have `easting` and ",
              "`northing` columns (or an sf geometry).")
      }
      coords <- as.matrix(ecp[, c("easting", "northing")])
      colnames(coords) <- c("x", "y")
      out       <- as.data.frame(ecp)
      predicted <- terra::extract(dtm, coords, method = "bilinear")[, 1]
   }
   n_missing <- sum(is.na(predicted))
   if (n_missing > 0L) {
      message("sample_dtm(): ", n_missing,
              " of ", length(predicted),
              " ECPs fell outside the DTM extent ",
              "(predicted = NA).")
   }
   out$predicted <- predicted

   dir.create(dirname(output_csv), recursive = TRUE,
              showWarnings = FALSE)
   readr::write_csv(out, output_csv)

   out
}
