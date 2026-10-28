#' Download a USGS bare-earth tile if it's not already present
#'
#' Looks up a tile's download URL from the `usgs_tile_index`
#' (`DEM_URL` column), downloads its `.zip`, and unzips it into
#' [`pathtools::get_path("usgs_tile", ...)`]'s parent directory if
#' the tile isn't already there. Skip-if-exists, matching
#' [`merge_usgs_tiles()`] and the rest of the pipeline's output
#' functions.
#'
#' Only handles the `.zip` + inner `be_<tile>.tif` layout used by the
#' `2021_CENTRAL_EASTERN_MA` tiles (confirmed for every tile `esx`/
#' `wel` need) -- errors on any other `DEM_URL` extension rather than
#' guessing at the older `.img.bz2` (2015/Sandy) or bare `.tif`
#' (2018_Nantucket) formats the index also contains.
#'
#' @param tile USGS tile id (e.g. `"19TDG412612"`), matching the
#'    `usgs_tile_index`'s `TILENAME` column and the `tile` parameter
#'    of the `usgs_tile` path template.
#' @param overwrite If `FALSE` (default) and the tile's `.tif` already
#'    exists, skip downloading. If `TRUE`, re-download.
#' @return Path to the tile's `.tif` (invisibly).
#'
#' @examples
#' \dontrun{
#' download_usgs_tile("19TDG412612")
#' }
download_usgs_tile <- function(tile, overwrite = FALSE) {

   output <- pathtools::get_path("usgs_tile", tile = tile)

   if (file.exists(output) && !overwrite) {
      message("download_usgs_tile(): tile already present, ",
              "skipping: ", output)
      return(invisible(output))
   }

   tile_index <- sf::st_read(
      pathtools::get_path("usgs_tile_index"), quiet = TRUE
   )
   url <- tile_index$DEM_URL[match(tile, tile_index$TILENAME)]
   if (is.na(url)) {
      stop("download_usgs_tile(): tile '", tile,
           "' not found in usgs_tile_index.")
   }
   if (tools::file_ext(url) != "zip") {
      stop("download_usgs_tile(): tile '", tile, "' has an ",
           "unsupported DEM_URL format ('.", tools::file_ext(url),
           "'); only '.zip' tiles are currently handled.")
   }

   pathtools::ensure_parent(output)
   zip_path <- tempfile(fileext = ".zip")
   on.exit(unlink(zip_path), add = TRUE)

   message("download_usgs_tile(): downloading tile '", tile, "' ",
           "from ", url)
   utils::download.file(url, zip_path, mode = "wb", quiet = TRUE)
   utils::unzip(zip_path, exdir = dirname(output))

   if (!file.exists(output)) {
      stop("download_usgs_tile(): expected '", output,
           "' after unzipping '", url, "' but it's missing.")
   }

   invisible(output)
}
