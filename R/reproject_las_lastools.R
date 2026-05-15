#' Reproject a LAS file with LAStools (`las2las` then `lasvdatum`)
#'
#' Two-step horizontal + vertical reprojection of a `.las` file by
#' shelling out to two LAStools binaries.
#' Typically called via [`reproject_las()`] with `method = "lastools"`;
#' kept available as an alternative to the default PDAL workflow so
#' historical output produced by the same recipe can be recreated
#' and audited.
#'
#' **Requires a paid LAStools license.**
#' `las2las` is part of LAStools' free / open-source set,
#' but `lasvdatum` is one of the **commercial-only** tools and
#' will fail with `ERROR:license failure` on a free LAStools
#' install.
#' Without a licensed `lasvdatum` this method cannot complete
#' end-to-end —
#' Step 1 succeeds, Step 2 aborts.
#' If a license is not available,
#' use [`reproject_las_pdal()`] instead
#' (the default for [`reproject_las()`]).
#'
#' Steps:
#'
#' 1. **`las2las64 -proj_epsg <source> <target>`** —
#'    horizontal reprojection of XY only,
#'    delegated to PROJ via las2las's "Recommended" `-proj_epsg`
#'    flag.
#'    This is what actually applies the WGS 84 ↔ NAD 83 datum
#'    shift on the horizontal axis;
#'    the older `-target_epsg <target>` flag aborts with a
#'    SERIOUS WARNING for cross-datum reprojection even with
#'    `-force`,
#'    while `-proj_epsg` avoids that branch entirely because
#'    PROJ knows the relationship between the two datums.
#'    Z passes through unchanged
#'    (LAStools' `las2las` does not transform Z when changing
#'    horizontal datum;
#'    its `-vertical_*` flags only stamp metadata).
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
#' **Caveat — vertical WGS 84 ↔ NAD 83 frame offset remains.**
#' Step 1 (PROJ-based) applies the WGS 84 → NAD 83 frame shift to
#' the horizontal axis,
#' but `las2las` still does not transform Z when changing
#' horizontal datum,
#' so the Z values handed to `lasvdatum` are still WGS 84
#' ellipsoidal heights.
#' GEOID12B is referenced to NAD 83 ellipsoidal heights,
#' so the geoid grid is being applied to coordinates from the
#' wrong reference frame.
#' In eastern CONUS the WGS 84 ↔ NAD 83 vertical frame offset
#' is a few cm to ~1 m
#' (plate motion accumulated since the frames were tied at epoch
#' 1997).
#' This LAStools two-step workflow carries that residual vertical
#' error;
#' the PDAL workflow ([`reproject_las_pdal()`]) does not.
#' Accepted here because it matches (and improves on) the
#' historical UMassAir process and downstream stats fit a
#' residual offset that absorbs most of the bias.
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
#' @param source_epsg Integer EPSG code for the source horizontal
#'    CRS.
#'    If `NULL` (default),
#'    read from the LAS header via `lidR::epsg()`.
#'    Pass explicitly if the header is missing or wrong.
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
                                   source_epsg = NULL,
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

   # Read the source horizontal EPSG from the LAS header if the
   # caller did not pass one explicitly.  `-proj_epsg` needs both
   # source and target codes, and reading the header is reliable
   # for the RESEPI/PPK deliveries we work with.
   if (is.null(source_epsg)) {
      source_epsg <- lidR::epsg(lidR::readLASheader(input))
      if (is.na(source_epsg) || is.null(source_epsg)) {
         stop("reproject_las_lastools(): could not read source ",
              "EPSG from `", input, "`; pass `source_epsg` ",
              "explicitly.")
      }
   }
   source_epsg <- as.integer(source_epsg)

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
   # `-proj_epsg <source> <target>` delegates the transformation to
   # PROJ.  This is the "Recommended" path in the las2las README and
   # is what makes the WGS 84 -> NAD 83 horizontal datum shift
   # actually happen.  The older `-target_epsg <target>` flag aborted
   # with a SERIOUS WARNING for incompatible datums even when
   # paired with `-force`; `-proj_epsg` avoids that branch entirely
   # because PROJ knows the relationship between the two datums.
   step1 <- system2(
      las2las,
      args = c("-i", shQuote(input),
               "-proj_epsg", source_epsg, target_epsg,
               "-o", shQuote(intermediate))
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
