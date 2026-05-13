#' Map elevation control points on top of an interactive base map
#'
#' Builds a leaflet map showing each ECP location with its elevation
#' as the marker popup. The DTM is used only to source the CRS for
#' interpreting the ECP easting/northing values; the raster itself is
#' not currently rendered.
#'
#' @param dtm Either a `terra::SpatRaster` (single-band) or a path to
#'   a GeoTIFF. Only its CRS is used.
#' @param ecp A data frame of elevation control points with at least
#'   `easting`, `northing`, `elevation`, and `date` columns.
#'
#' @return A `leaflet` map widget, suitable for printing in RStudio's
#'   Viewer or knitting in an Rmd.
#'
#' @examples
#' \dontrun{
#' visualize_dtm("…/csf_th0.01_res0.1_rgd2_0.25m.tif", site_ecp)
#' }
visualize_dtm <- function(dtm, ecp) {

   if (is.character(dtm) && length(dtm) == 1 && file.exists(dtm)) {
      dtm <- terra::rast(dtm)
   }
   if (!inherits(dtm, "SpatRaster") || terra::nlyr(dtm) != 1) {
      stop("Expected dtm or the file it points to, to be a single band raster.")
   }

   coords <- ecp |>
      dplyr::select(x = easting, y = northing, elevation, date) |>
      as.data.frame()

   coords_sf <- sf::st_as_sf(coords, coords = c("x", "y"),
                             crs = terra::crs(dtm))

   wgs84_pts <- sf::st_transform(coords_sf, 4326)

   leaflet::leaflet(data = wgs84_pts) |>
      leaflet::addTiles() |>
      leaflet::addMarkers(popup = ~elevation)
}
