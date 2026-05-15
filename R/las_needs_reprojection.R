#' Test whether a LAS file is already in the project's target CRS
#'
#' Reads the LAS header and compares the horizontal and vertical
#' EPSG codes against the supplied targets.
#' Returns `TRUE` if **either** axis differs from the target,
#' or if either cannot be determined from the header.
#'
#' Used to decide whether [`reproject_las()`] needs to run for a
#' given source file.
#' A `FALSE` return is a strong signal that no reprojection is
#' needed;
#' a `TRUE` return triggers reprojection.
#'
#' **Two header formats are supported,**
#' since LAS files can store their CRS either way:
#'
#' 1. **GeoTIFF GeoKey directory** (LAS 1.0 – 1.3,
#'    typical of LAStools / RESEPI output).
#'    The horizontal EPSG comes from key `3072`
#'    (`ProjectedCSTypeGeoKey`)
#'    and the vertical from key `4096`
#'    (`VerticalCSTypeGeoKey`),
#'    both parsed from
#'    `header@VLR$GeoKeyDirectoryTag$tags`.
#' 2. **WKT VLR** (LAS 1.4 with the `WKT` global-encoding bit
#'    set,
#'    typical of PDAL output).
#'    The compound WKT is read from
#'    `header@VLR[["WKT OGC CS"]][["WKT OGC COORDINATE SYSTEM"]]`,
#'    then scanned for `AUTHORITY["EPSG","NNN"]` entries.
#'    A target code is considered present if it appears anywhere
#'    in the WKT;
#'    this is safe in practice because top-level CRS EPSG codes
#'    (e.g. `26919`, `5703`) do not collide with the nested
#'    authority codes used for datums, ellipsoids, primes, and
#'    units.
#'
#' The two methods are tried in order;
#' a missing field in the first method falls through to the
#' second.
#' If neither yields a match, the axis is recorded as missing
#' and `TRUE` is returned
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

   horiz <- NA_integer_
   vert <- NA_integer_

   # Method 1: parse the GeoTIFF GeoKey directory.
   tags <- header@VLR$GeoKeyDirectoryTag$tags
   if (!is.null(tags)) {
      keys <- vapply(tags, function(t) t$key, integer(1L))
      vals <- vapply(tags, function(t) t[["value offset"]],
                     integer(1L))
      hit_h <- which(keys == 3072L)
      if (length(hit_h) == 1L) {
         horiz <- vals[hit_h]
      }
      hit_v <- which(keys == 4096L)
      if (length(hit_v) == 1L) {
         vert <- vals[hit_v]
      }
   }

   # Method 2: scan the WKT VLR for target EPSG authority codes.
   # Only consulted if at least one axis is still unknown.
   if (is.na(horiz) || is.na(vert)) {
      wkt_vlr <- header@VLR[["WKT OGC CS"]]
      wkt <- if (!is.null(wkt_vlr)) {
         wkt_vlr[["WKT OGC COORDINATE SYSTEM"]]
      } else {
         NULL
      }
      if (!is.null(wkt) && nchar(wkt) > 0L) {
         all_auth <- regmatches(
            wkt,
            gregexpr('AUTHORITY\\["EPSG","\\d+"\\]', wkt)
         )[[1]]
         codes <- as.integer(sub(
            'AUTHORITY\\["EPSG","(\\d+)"\\]', "\\1", all_auth
         ))
         if (is.na(horiz) && target_epsg %in% codes) {
            horiz <- target_epsg
         }
         if (is.na(vert) && target_vertical_epsg %in% codes) {
            vert <- target_vertical_epsg
         }
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
