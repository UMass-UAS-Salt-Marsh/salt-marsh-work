#' Write a metadata sidecar for a final pipeline output
#'
#' Records provenance for a *final* pipeline output raster — when it
#' was created, what raw point cloud it ultimately derives from, its
#' CRS, its immediate inputs, and whatever parameters varied it — as a
#' small YAML file next to the output itself. Generic: takes only
#' already-resolved paths and values, with no knowledge of
#' [`pathtools`] or the lidar path scheme.
#'
#' Skip-if-exists, matching [`reproject_las()`], [`clean_and_tile()`],
#' and [`sample_dtm()`] — safe to call unconditionally after a
#' producing step, including one the caller itself skipped because the
#' output already existed.
#'
#' @param output Path to the output file this metadata describes, or
#'    a character vector when several outputs share one sidecar (e.g.
#'    a raster and its companion count raster). The sidecar path is
#'    derived from `output[1]` by swapping its extension for `.yaml`
#'    (same directory, same stem).
#' @param created_by Name of the function that produced `output` (e.g.
#'    `"rasterize_veg_heights"`), or, when the output is built inline
#'    in a driver script rather than by a dedicated function, the
#'    script's path (e.g. `"lidar/07_floor_corrected_ground.R"`).
#' @param source_cloud Path to the original raw point cloud this
#'    output ultimately derives from — the first input to the whole
#'    pipeline, not necessarily this output's immediate upstream file.
#' @param crs The output's CRS, as a string (e.g. `"EPSG:6491"`).
#' @param inputs Named list of the immediate input paths this output
#'    was built from.
#' @param params Named list of the scalar or vector parameters that
#'    actually varied the output (e.g. CSF parameters, bin breaks,
#'    raster resolution). Default `list()`.
#' @param overwrite If `FALSE` (default) and the sidecar already
#'    exists, skip writing it. If `TRUE`, overwrite.
#'
#' @return Path to the sidecar YAML file (invisibly).
#'
#' @examples
#' \dontrun{
#' write_output_metadata(
#'    output       = output_tif,
#'    created_by   = "rasterize_veg_heights",
#'    source_cloud = pathtools::get_path("raw_lidar", site = "rr",
#'                                       date = "2022_08_10"),
#'    crs          = "EPSG:6491",
#'    inputs       = list(cleaned_tiles = summer_clean_dir),
#'    params       = list(raster_res = 0.5)
#' )
#' }
write_output_metadata <- function(output,
                                  created_by,
                                  source_cloud,
                                  crs,
                                  inputs,
                                  params = list(),
                                  overwrite = FALSE) {

   sidecar <- sub("\\.[^.]+$", ".yaml", output[1])

   if (file.exists(sidecar) && !overwrite) {
      message("write_output_metadata(): sidecar exists, skipping: ",
              sidecar)
      return(invisible(sidecar))
   }

   metadata <- list(
      created_at   = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
      created_by   = created_by,
      output       = output,
      crs          = crs,
      source_cloud = source_cloud,
      inputs       = as.list(inputs),
      params       = as.list(params)
   )

   yaml::write_yaml(metadata, sidecar)

   invisible(sidecar)
}
