#' Test whether a LAS file is already in the project's target CRS
#'
#' Reads the LAS header and compares the horizontal EPSG (from
#' `lidR::epsg()`) and the vertical CS type (GeoTIFF GeoKey 4096,
#' `VerticalCSTypeGeoKey`,
#' parsed from `header@VLR$GeoKeyDirectoryTag$tags`) against the
#' supplied targets.
#' Returns `TRUE` if **either** axis differs from the target.
#'
#' Used to decide whether [`reproject_las()`] needs to run for a
#' given source file.
#' A `FALSE` return is a strong signal that no reprojection is
#' needed;
#' a `TRUE` return triggers the two-step LAStools reprojection.
#'
#' If the vertical CS geokey is absent from the header, the
#' function treats the vertical CS as unknown and returns `TRUE`
#' (safer to reproject than to silently use a mismatched file).
#'
#' @param input Path to the `.las` file to inspect.
#' @param target_epsg Integer EPSG code for the target horizontal
#'    CRS.
#'    Default `26919` (NAD83 / UTM 19N).
#' @param target_vertical_epsg Integer EPSG code for the target
#'    vertical CRS.
#'    Default `5703` (NAVD88 height).
#' @param quiet If `FALSE` (default), print a one-line message
#'    describing the found-vs-expected pair.
#'
#' @return Logical scalar: `TRUE` if reprojection is needed.
#'
#' @examples
#' \dontrun{
#' las_needs_reprojection(
#'    "E:/uas_scratch/lidar/rr/2022_08_10/reprojected/foo.las"
#' )
#' }
las_needs_reprojection <- function(input,
                                   target_epsg = 26919L,
                                   target_vertical_epsg = 5703L,
                                   quiet = FALSE) {

   stopifnot(
      is.character(input), length(input) == 1L,
      file.exists(input)
   )

   header <- lidR::readLASheader(input)
   horiz <- lidR::epsg(header)

   tags <- header@VLR$GeoKeyDirectoryTag$tags
   vert <- NA_integer_
   if (!is.null(tags)) {
      keys <- vapply(tags, function(t) t$key, integer(1L))
      vals <- vapply(tags, function(t) t[["value offset"]],
                     integer(1L))
      hit <- which(keys == 4096L)
      if (length(hit) == 1L) {
         vert <- vals[hit]
      }
   }

   needs <- (is.na(horiz) || horiz != target_epsg) ||
      (is.na(vert) || vert != target_vertical_epsg)

   if (!quiet) {
      message(
         "las_needs_reprojection(): ",
         "horizontal ",
         if (is.na(horiz)) "<missing>" else horiz,
         " vs target ", target_epsg, "; ",
         "vertical ",
         if (is.na(vert)) "<missing>" else vert,
         " vs target ", target_vertical_epsg, "; ",
         "needs reprojection = ", needs
      )
   }

   needs
}
