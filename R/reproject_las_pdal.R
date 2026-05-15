#' Reproject a LAS file with PDAL in a single PROJ-aware pass
#'
#' Shells out to `pdal pipeline` with an in-memory pipeline JSON
#' that does the full horizontal + vertical transformation in one
#' pass:
#'
#' 1. **WGS 84 → NAD 83 frame shift** is applied via PROJ's
#'    bundled NADCON5 / ITRF transformation grids.
#' 2. **Ellipsoidal → NAVD 88** vertical shift is applied via the
#'    supplied `.gtx` geoid grid
#'    (e.g. `g2012bu0.gtx` for GEOID12B in CONUS).
#'
#' Unlike [`reproject_las_lastools()`],
#' this method eliminates the ~1–2 m WGS 84 ↔ NAD 83
#' frame-realization offset that the LAStools two-step recipe
#' carries because LAStools' `las2las` does not apply a true
#' datum shift to Z.
#' This is the recommended default for new output;
#' the LAStools path is retained only for reproducing historical
#' files.
#'
#' Typically called via [`reproject_las()`] with `method = "pdal"`
#' (the default).
#'
#' **PROJ_DATA isolation.**
#' The geoid grid is found by setting `PROJ_DATA` to the grid's
#' directory.
#' Base R's `system2(env = ...)` is not honored on Windows
#' (the env arg gets passed as a positional argument instead of
#' as an environment variable),
#' so we set `PROJ_DATA` via `Sys.setenv()` for the duration of
#' the PDAL call and restore the previous value on exit.
#' R's PROJ_DATA is therefore set transiently while PDAL runs and
#' restored afterwards;
#' in practice this does not interfere with `sf` / `terra` /
#' `lidR` because those packages cache their PROJ data path at
#' package load and do not re-read `PROJ_DATA` mid-session.
#'
#' **Install location.**
#' PDAL is expected at `pdal` (default
#' `"C:/OSGeo4W/bin/pdal.exe"`,
#' matching the project's chosen OSGeo4W install route).
#' Override via the `pdal` argument if installed elsewhere.
#'
#' @param input Path to the source `.las` file.
#' @param output Path to write the reprojected `.las` file.
#' @param target_epsg Integer EPSG code for the target horizontal
#'    CRS
#'    (e.g. `26919` for NAD83 / UTM 19N).
#'    Used to build the target PROJ string via
#'    `sf::st_crs(target_epsg)$proj4string` plus appended
#'    `+geoidgrids=` and `+vunits=m`.
#' @param vgrid Path to the `.gtx` geoid grid file.
#'    The grid's directory is added to the subprocess's
#'    `PROJ_DATA`, and the grid is referenced in the PROJ string
#'    by basename only.
#' @param target_vertical_epsg Integer EPSG code for the target
#'    vertical CRS,
#'    used to set the output LAS file's `a_srs` compound CRS.
#'    Default `5703` (NAVD88 height).
#' @param overwrite If `FALSE` (default) and `output` already
#'    exists, skip the PDAL call and return `output` invisibly.
#'    If `TRUE`, run the transformation and replace the existing
#'    file.
#' @param pdal Path to the `pdal` executable.
#'    Default `"C:/OSGeo4W/bin/pdal.exe"`.
#' @param proj_data_dir Path to PROJ's data directory
#'    (the one containing `proj.db`).
#'    Default `"C:/OSGeo4W/share/proj"`,
#'    matching the OSGeo4W install layout.
#'    Combined with `dirname(vgrid)` on `PROJ_DATA` so PROJ can
#'    find both its CRS database and the geoid grid.
#'
#' @return The `output` path, invisibly.
#'
#' @examples
#' \dontrun{
#' reproject_las_pdal(
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
#'    target_epsg = 26919,
#'    vgrid = paste0(
#'       "X:/legacy/gdrive/UMassAir User Resources/LASTools/",
#'       "Geoid Transformation GTX Files/geoid12b/g2012bu0.gtx"
#'    )
#' )
#' }
reproject_las_pdal <- function(input,
                               output,
                               target_epsg,
                               vgrid,
                               target_vertical_epsg = 5703L,
                               overwrite = FALSE,
                               pdal = "C:/OSGeo4W/bin/pdal.exe",
                               proj_data_dir =
                                  "C:/OSGeo4W/share/proj") {

   stopifnot(
      is.character(input), length(input) == 1L,
      is.character(output), length(output) == 1L,
      is.numeric(target_epsg), length(target_epsg) == 1L,
      is.numeric(target_vertical_epsg),
      length(target_vertical_epsg) == 1L,
      file.exists(input),
      file.exists(vgrid),
      file.exists(pdal),
      dir.exists(proj_data_dir),
      file.exists(file.path(proj_data_dir, "proj.db"))
   )

   if (!requireNamespace("sf", quietly = TRUE)) {
      stop("reproject_las_pdal(): the `sf` package is required ",
           "to derive a PROJ string from the target EPSG.")
   }
   if (!requireNamespace("jsonlite", quietly = TRUE)) {
      stop("reproject_las_pdal(): the `jsonlite` package is ",
           "required to write the PDAL pipeline JSON.")
   }

   target_epsg <- as.integer(target_epsg)
   target_vertical_epsg <- as.integer(target_vertical_epsg)

   if (file.exists(output) && !overwrite) {
      message("reproject_las_pdal(): output exists, skipping: ",
              output)
      return(invisible(output))
   }

   dir.create(dirname(output), recursive = TRUE,
              showWarnings = FALSE)

   # Build the target PROJ string.  Start from sf's proj4 form of
   # the horizontal EPSG and append the geoid grid by basename;
   # the grid's directory is added to PROJ_DATA below so PROJ can
   # resolve it.
   base_proj <- sf::st_crs(target_epsg)$proj4string
   if (is.na(base_proj)) {
      stop("reproject_las_pdal(): could not resolve EPSG:",
           target_epsg, " via sf::st_crs().")
   }
   out_srs <- sprintf("%s +geoidgrids=%s +vunits=m",
                      base_proj, basename(vgrid))

   pipeline <- list(
      pipeline = list(
         list(type = "readers.las",
              filename = input),
         list(type = "filters.reprojection",
              out_srs = out_srs),
         list(type = "writers.las",
              filename = output,
              a_srs = sprintf("EPSG:%d+%d",
                              target_epsg, target_vertical_epsg))
      )
   )

   pipeline_json <- tempfile(
      pattern = "pdal_pipeline_",
      tmpdir = dirname(output),
      fileext = ".json"
   )
   on.exit(unlink(pipeline_json), add = TRUE)
   writeLines(
      jsonlite::toJSON(pipeline, auto_unbox = TRUE, pretty = TRUE),
      pipeline_json
   )

   # Point PROJ at *both* OSGeo4W's PROJ data directory (which
   # contains `proj.db`, PROJ's main CRS database) and the geoid
   # grid's directory.  `PROJ_DATA` is a `;`-separated search
   # path on Windows; both entries are needed because pointing
   # only at the gtx dir hides `proj.db` and breaks every CRS
   # lookup.
   #
   # Base R's `system2(env = ...)` is not honored on Windows
   # (it gets passed as a positional arg instead of as an
   # environment variable), so we set `PROJ_DATA` via
   # `Sys.setenv()` and restore the previous value on exit.
   # The subprocess inherits R's environment.
   gtx_dir <- normalizePath(dirname(vgrid), winslash = "/",
                            mustWork = TRUE)
   proj_data_dir <- normalizePath(proj_data_dir, winslash = "/",
                                  mustWork = TRUE)
   new_proj_data <- paste(proj_data_dir, gtx_dir,
                          sep = .Platform$path.sep)
   old_proj_data <- Sys.getenv("PROJ_DATA", unset = NA_character_)
   Sys.setenv(PROJ_DATA = new_proj_data)
   on.exit({
      if (is.na(old_proj_data)) {
         Sys.unsetenv("PROJ_DATA")
      } else {
         Sys.setenv(PROJ_DATA = old_proj_data)
      }
   }, add = TRUE)

   exit <- system2(
      pdal,
      args = c("pipeline", shQuote(pipeline_json))
   )

   if (exit != 0L || !file.exists(output)) {
      stop("`pdal pipeline` failed (exit ", exit,
           ") for input: ", input,
           "\nPipeline JSON: ", pipeline_json,
           " (set `overwrite = TRUE` and inspect to debug).")
   }

   invisible(output)
}
