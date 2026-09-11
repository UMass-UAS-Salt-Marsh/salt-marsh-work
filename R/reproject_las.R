#' Reproject a LAS file to the project's target CRS
#'
#' Public dispatcher for the project's reprojection workflow.
#' Routes to one of two implementations based on `method`:
#'
#' - **`"pdal"` (default).**
#'   Calls [`reproject_las_pdal()`], which does the full
#'   datum-aware transformation in a single PROJ pipeline:
#'   WGS 84 → NAD 83 frame shift
#'   (via PROJ's bundled NADCON5 / ITRF grids)
#'   plus ellipsoidal → NAVD 88
#'   (via the supplied `.gtx` geoid grid).
#'   Recommended for new output.
#' - **`"lastools"`.**
#'   Calls [`reproject_las_lastools()`], which chains the LAStools
#'   `las2las` (horizontal) and `lasvdatum` (vertical) binaries.
#'   Retained so historical output produced by the established
#'   UMassAir workflow can be recreated and audited.
#'   **Requires a paid LAStools license** — `lasvdatum` is one of
#'   LAStools' commercial-only tools and will fail with
#'   `ERROR:license failure` on a free install.
#'   Also carries a residual WGS 84 ↔ NAD 83 vertical
#'   frame-realization error
#'   (a few cm to ~1 m in eastern CONUS);
#'   see [`reproject_las_lastools()`] for the full caveat.
#'
#' The skip-if-exists short-circuit lives in the dispatcher,
#' so behavior is uniform across methods.
#' Both helpers also honor `overwrite` themselves
#' as a defense-in-depth check.
#'
#' Method-specific arguments
#' (e.g. `pdal` path,
#' `las2las` / `lasvdatum` paths,
#' `intermediate`,
#' `keep_intermediate`)
#' are forwarded through `...`.
#' See the helpers' docs for the full set of options each method
#' accepts.
#'
#' @param input Path to the source `.las` file.
#' @param output Path to write the reprojected `.las` file.
#' @param target_epsg Integer EPSG code for the target horizontal
#'    CRS.
#'    Default `6491` (NAD83(2011) / Massachusetts Mainland State
#'    Plane, meters) — the project's current standard, adopted
#'    (provisionally — see [`CRS.md`](../CRS.md)) 2026-08-20.
#'    Supersedes the historical `26919` (NAD83 / UTM 19N, no
#'    realization) default.
#' @param vgrid Path to the geoid grid file (`.gtx` or GeoTIFF —
#'    PROJ accepts both the same way).
#'    Default is `pathtools::get_path("geoid_grid")`, the
#'    project-stored **GEOID18** CONUS grid (`us_noaa_g2018u0.tif`),
#'    the current NGS standard. See [`CRS.md`](../CRS.md).
#' @param overwrite If `FALSE` (default) and `output` already
#'    exists, return early without calling either helper.
#'    If `TRUE`, run the transformation and replace the existing
#'    file.
#' @param method One of `"pdal"` or `"lastools"`.
#'    Default `"pdal"`.
#' @param ... Forwarded to the chosen helper.
#'    See [`reproject_las_pdal()`] and
#'    [`reproject_las_lastools()`] for accepted args.
#'
#' @return The `output` path, invisibly.
#'
#' @examples
#' \dontrun{
#' # Default: PDAL, datum-aware, single-pass.
#' reproject_las(
#'    input = pathtools::get_path("raw_lidar", site = "rr",
#'                                date = "2022_08_10"),
#'    output = file.path(
#'       pathtools::get_path("reprojected_dir", site = "rr",
#'                           date = "2022_08_10"),
#'       "ppk_07Nov2022_cloud_1_epsg6491_navd88.las"
#'    )
#' )
#'
#' # Reproduce a historical LAStools-produced file.
#' reproject_las(
#'    input = pathtools::get_path("raw_lidar", site = "rr",
#'                                date = "2022_08_10"),
#'    output = file.path(
#'       pathtools::get_path("reprojected_dir", site = "rr",
#'                           date = "2022_08_10"),
#'       "ppk_07Nov2022_cloud_1_lastools.las"
#'    ),
#'    method = "lastools"
#' )
#' }
reproject_las <- function(input,
                          output,
                          target_epsg = 6491L,
                          vgrid = get_path("geoid_grid"),
                          overwrite = FALSE,
                          method = c("pdal", "lastools"),
                          ...) {

   stopifnot(
      is.character(input), length(input) == 1L,
      is.character(output), length(output) == 1L
   )

   method <- match.arg(method)

   if (file.exists(output) && !overwrite) {
      message("reproject_las(): output exists, skipping: ", output)
      return(invisible(output))
   }

   switch(
      method,
      pdal = reproject_las_pdal(
         input = input,
         output = output,
         target_epsg = target_epsg,
         vgrid = vgrid,
         overwrite = overwrite,
         ...
      ),
      lastools = reproject_las_lastools(
         input = input,
         output = output,
         target_epsg = target_epsg,
         vgrid = vgrid,
         overwrite = overwrite,
         ...
      )
   )
}
