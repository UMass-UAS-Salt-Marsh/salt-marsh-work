#' Build a USGS tile mosaic for a site
#'
#' Finds which USGS bare-earth tiles intersect a site's footprint
#' (via `usgs_tile_index` / `site_footprint`, resolved through
#' [`pathtools::get_path()`]) and mosaics them into a lightweight
#' `.vrt` via `sf::gdal_utils(util = "buildvrt", ...)` -- a GDAL
#' virtual-raster pointer file, not a materialized merged raster.
#' Decided (2026-09-11, see `dev/rollout_workplan.md`) over
#' `terra::merge()` because sites needing more than one tile (e.g.
#' `wel`) just need the tiles to read as one raster downstream, not a
#' new file written to disk tile-by-tile.
#'
#' Skip-if-exists, matching [`reproject_las()`], [`clean_and_tile()`],
#' and [`build_floor_corrected_ground()`] -- safe to call
#' unconditionally.
#'
#' @param site Lidar site code (e.g. `"wel"`).
#' @param overwrite If `FALSE` (default) and the `.vrt` already
#'    exists, skip rebuilding it. If `TRUE`, rebuild it.
#' @return Path to the `.vrt` mosaic (invisibly).
#'
#' @examples
#' \dontrun{
#' merge_usgs_tiles("wel")
#' }
merge_usgs_tiles <- function(site, overwrite = FALSE) {

   output <- pathtools::ensure_parent(
      pathtools::get_path("usgs_mosaic_vrt", site = site)
   )

   if (file.exists(output) && !overwrite) {
      message("merge_usgs_tiles(): output exists, skipping: ", output)
      return(invisible(output))
   }

   tile_index <- sf::st_read(
      pathtools::get_path("usgs_tile_index"), quiet = TRUE
   )
   footprint <- sf::st_read(
      pathtools::get_path("site_footprint", site = site), quiet = TRUE
   )
   footprint <- sf::st_transform(footprint, sf::st_crs(tile_index))

   hits <- sf::st_intersects(tile_index, footprint, sparse = FALSE)
   tiles <- tile_index$TILENAME[apply(hits, 1, any)]

   if (length(tiles) == 0L) {
      stop("merge_usgs_tiles(): no USGS tiles intersect site '",
           site, "'s footprint.")
   }

   tile_paths <- vapply(
      tiles,
      \(tile) download_usgs_tile(tile),
      character(1L)
   )

   sf::gdal_utils(
      util        = "buildvrt",
      source      = tile_paths,
      destination = output
   )

   inputs <- list(
      usgs_tile_index = pathtools::get_path("usgs_tile_index"),
      site_footprint  = pathtools::get_path("site_footprint", site = site)
   )

   write_output_metadata(
      output     = output,
      created_by = "merge_usgs_tiles",
      crs        = sf::st_crs(tile_index)$input,
      inputs     = inputs,
      params     = list(tiles = tiles),
      overwrite  = overwrite
   )

   invisible(output)
}
