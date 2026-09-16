#' Estimate expected per-worker memory usage for a lidar process
#'
#' Assumes memory usage scales linearly with the number of points a
#' worker holds at once, i.e. with buffered tile area
#' (`(chunk_size + 2 * chunk_buffer)^2`) times point `density`. Scales
#' from a reference calibration point looked up in
#' [worker_memory_reference] by `process`.
#'
#' @param process Name of the process to estimate memory for (e.g.
#'   `"rasterize_ground"`). Used to look up defaults for any `ref_*`
#'   argument left as `NA` in [worker_memory_reference].
#' @param chunk_size Tile size in meters.
#' @param chunk_buffer Buffer around each tile in meters.
#' @param density Point density in points per square meter.
#' @param ref_chunk_size Reference tile size in meters. Default `NA`
#'   (looked up from [worker_memory_reference]).
#' @param ref_chunk_buffer Reference buffer in meters. Default `NA`
#'   (looked up from [worker_memory_reference]).
#' @param ref_density Reference point density. Default `NA` (looked up
#'   from [worker_memory_reference]).
#' @param ref_memory_mb Reference observed peak per-worker memory in
#'   MB. Default `NA` (looked up from [worker_memory_reference]).
#'
#' @return Estimated per-worker memory usage in MB.
#'
#' @examples
#' \dontrun{
#' estimate_worker_memory_mb(
#'    "rasterize_ground", chunk_size = 200, chunk_buffer = 20,
#'    density = 243.65
#' )
#' }
estimate_worker_memory_mb <- function(process, chunk_size, chunk_buffer,
                                      density,
                                      ref_chunk_size   = NA,
                                      ref_chunk_buffer = NA,
                                      ref_density      = NA,
                                      ref_memory_mb    = NA) {

   if (anyNA(c(ref_chunk_size, ref_chunk_buffer, ref_density,
               ref_memory_mb))) {
      ref_row <- worker_memory_reference[
         worker_memory_reference$process == process, , drop = FALSE
      ]
      if (nrow(ref_row) != 1) {
         stop("Expected exactly one worker_memory_reference row for ",
              "process \"", process, "\", found ", nrow(ref_row), ".")
      }
      if (is.na(ref_chunk_size))   ref_chunk_size   <- ref_row$chunk_size
      if (is.na(ref_chunk_buffer)) ref_chunk_buffer <- ref_row$chunk_buffer
      if (is.na(ref_density))      ref_density      <- ref_row$density
      if (is.na(ref_memory_mb))    ref_memory_mb    <- ref_row$memory_mb
   }

   tile_area     <- (chunk_size + 2 * chunk_buffer) ^ 2
   ref_tile_area <- (ref_chunk_size + 2 * ref_chunk_buffer) ^ 2

   ref_memory_mb * (tile_area * density) / (ref_tile_area * ref_density)
}
