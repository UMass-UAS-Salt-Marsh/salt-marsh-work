#' Reproject a LAS file with LAStools (`las2las` then `lasvdatum`)
#'
#' Two-step horizontal + vertical reprojection of a `.las` file by
#' shelling out to two LAStools binaries.
#' Typically called via [`reproject_las()`] with `method = "lastools"`;
#' kept available as an alternative to the default PDAL workflow so
#' historical output produced by the same recipe can be recreated
#' and audited.
#'
#' Steps:
#'
#' 1. **`las2las64 -target_epsg <n> -force`** —
#'    horizontal reprojection of XY only.
#'    LAStools' `las2las` does not transform Z when changing
#'    horizontal datum;
#'    its `-vertical_*` flags only stamp metadata.
#'    `-force` overrides the
#'    "horizontal datum of source and target incompatible" warning
#'    that LAStools raises when source and target use different
#'    geodetic datums
#'    (here WGS 84 vs NAD 83).
#' 2. **`lasvdatum64 -epsg <n> -vgrid <gtx>`** —
#'    grid-based vertical transformation from ellipsoidal to
#'    orthometric heights using a GTX geoid grid.
#'
#' Intended for converting RESEPI lidar deliveries from
#' WGS 84 / UTM 19N + WGS84 ellipsoidal heights
#' (EPSG:32619 + EPSG:5030)
#' to NAD83 / UTM 19N + NAVD88
#' (EPSG:26919 + EPSG:5703 via GEOID12B).
#'
#' **Caveat — WGS 84 ↔ NAD 83 frame offset.**
#' The GEOID12B grid is referenced to NAD 83 ellipsoidal heights,
#' but `las2las -target_epsg` does not convert Z from WGS 84
#' ellipsoidal to NAD 83 ellipsoidal.
#' In eastern CONUS the WGS 84 ↔ NAD 83 frame-realization offset
#' is ~1–2 m in 3D
#' (plate motion accumulated since the frames were tied at epoch
#' 1997).
#' This LAStools two-step workflow carries that error;
#' the PDAL workflow ([`reproject_las_pdal()`]) does not.
#' Accepted here because it matches the historical UMassAir
#' process and downstream stats fit a residual offset that
#' absorbs most of the bias.
#'
#' See the LAStools READMEs at
#' `C:\\tools\\LAStools\\bin\\las2las_README.md` and
#' `C:\\tools\\LAStools\\bin\\lasvdatum_README.md` for full flag
#' references.
#'
#' @param input Path to the source `.las` file.
#' @param output Path to write the reprojected `.las` file.
#' @param target_epsg Integer EPSG code for the target horizontal
#'    CRS (e.g. `26919` for NAD83 / UTM 19N).
#' @param vgrid Path to the `.gtx` geoid grid file used by
#'    `lasvdatum`.
#'    Default points at the project-stored GEOID12B CONUS tile.
#' @param overwrite If `FALSE` (default) and `output` already
#'    exists, skip both shell calls and return `output`
#'    invisibly.
#'    If `TRUE`, run the transformation and replace the existing
#'    file.
#' @param intermediate Path for the intermediate (post-`las2las`,
#'    pre-`lasvdatum`) LAS file.
#'    Defaults to a `tempfile()` in the same directory as
#'    `output`;
#'    removed on success unless `keep_intermediate = TRUE`.
#' @param las2las Path to the `las2las` (or `las2las64`)
#'    executable.
#' @param lasvdatum Path to the `lasvdatum` (or `lasvdatum64`)
#'    executable.
#' @param keep_intermediate If `TRUE`, leave the intermediate
#'    file on disk after success.
#'    Useful for diagnostics.
#'
#' @return The `output` path, invisibly.
#'
#' @examples
#' \dontrun{
#' reproject_las_lastools(
#'    input = paste0(
#'       "X:/legacy/gdrive/saltmarsh_UAS/UAS Data Collection/",
#'       "Red River/2022/LiDAR/10Aug2022_Low/",
#'       "RESEPI-5FFC59-2022-08-10-20-26-50/clouds/",
#'       "ppk_07Nov2022_cloud_1.las"
#'    ),
#'    output = paste0(
#'       "E:/uas_scratch/lidar/rr/2022_08_10/reprojected/",
#'       "ppk_07Nov2022_cloud_1_epsg26919_navd88.las"
#'    ),
#'    target_epsg = 26919
#' )
#' }
reproject_las_lastools <- function(input,
                                   output,
                                   target_epsg,
                                   vgrid,
                                   overwrite = FALSE,
                                   intermediate = NULL,
                                   las2las =
                                      "C:/tools/LAStools/bin/las2las64.exe",
                                   lasvdatum =
                                      "C:/tools/LAStools/bin/lasvdatum64.exe",
                                   keep_intermediate = FALSE) {

   stopifnot(
      is.character(input), length(input) == 1L,
      is.character(output), length(output) == 1L,
      is.numeric(target_epsg), length(target_epsg) == 1L,
      file.exists(input),
      file.exists(vgrid),
      file.exists(las2las),
      file.exists(lasvdatum)
   )

   target_epsg <- as.integer(target_epsg)

   if (file.exists(output) && !overwrite) {
      message("reproject_las_lastools(): output exists, skipping: ",
              output)
      return(invisible(output))
   }

   dir.create(dirname(output), recursive = TRUE,
              showWarnings = FALSE)

   if (is.null(intermediate)) {
      intermediate <- tempfile(
         pattern = sub("\\.las$", "_xy_", basename(output),
                       ignore.case = TRUE),
         tmpdir = dirname(output),
         fileext = ".las"
      )
   }

   # Step 1: horizontal reprojection (XY only; Z passes through).
   # `shQuote()` is required because base R's `system2()` does not
   # quote args on Windows, and several of the project paths contain
   # spaces (e.g. "UAS Data Collection", "UMassAir User Resources").
   # `-force` tells LAStools to proceed past its "horizontal datum of
   # source and target incompatible" warning, which it raises when
   # source and target use different geodetic datums (here WGS 84
   # vs NAD 83).  See the WGS84<->NAD83 frame caveat in this
   # function's roxygen docs.
   step1 <- system2(
      las2las,
      args = c("-i", shQuote(input),
               "-target_epsg", target_epsg,
               "-o", shQuote(intermediate),
               "-force")
   )
   if (step1 != 0L || !file.exists(intermediate)) {
      stop("`las2las` failed (exit ", step1, ") for input: ",
           input)
   }

   # Step 2: vertical transformation via gtx grid
   # (ellipsoidal -> NAVD88).
   step2 <- system2(
      lasvdatum,
      args = c("-i", shQuote(intermediate),
               "-epsg", target_epsg,
               "-vgrid", shQuote(vgrid),
               "-o", shQuote(output))
   )
   if (step2 != 0L || !file.exists(output)) {
      stop("`lasvdatum` failed (exit ", step2,
           ") for intermediate file: ", intermediate)
   }

   if (!keep_intermediate) {
      unlink(intermediate)
   }

   invisible(output)
}
