#' Check available memory against expected worker memory usage
#'
#' Compares currently available system memory (via
#' [get_available_memory_mb()]) against how much `workers` parallel
#' workers are expected to need, including a safety margin. Stops with
#' an informative error — including the maximum number of workers the
#' currently available memory could support — if there isn't enough.
#'
#' @param workers Number of parallel workers that will be launched.
#' @param expected_worker_memory_mb Expected peak memory usage per
#'   worker, in MB (e.g. from [estimate_worker_memory_mb()]).
#' @param safety_margin Fractional headroom required above the raw
#'   `workers * expected_worker_memory_mb` requirement. Default `0.25`
#'   (25%).
#'
#' @return Invisibly, `TRUE` if the check passes.
#'
#' @examples
#' \dontrun{
#' check_worker_memory(workers = 10, expected_worker_memory_mb = 11498)
#' }
check_worker_memory <- function(workers, expected_worker_memory_mb,
                                safety_margin = 0.25) {

   available_mb <- get_available_memory_mb()
   required_mb  <- workers * expected_worker_memory_mb * (1 + safety_margin)

   if (available_mb < required_mb) {
      max_workers <- floor(
         available_mb / (expected_worker_memory_mb * (1 + safety_margin))
      )
      stop(sprintf(
         paste(
            "Insufficient memory for %d workers at ~%.0f MB/worker",
            "(requires %.0f MB with a %.0f%% safety margin;",
            "%.0f MB currently available).",
            "At most %d worker(s) would fit right now -- lower",
            "`workers` and rerun."
         ),
         workers, expected_worker_memory_mb, required_mb,
         safety_margin * 100, available_mb, max_workers
      ))
   }

   message(sprintf(
      paste(
         "Memory check OK: %d workers x ~%.0f MB = %.0f MB needed",
         "(incl. %.0f%% margin), %.0f MB available."
      ),
      workers, expected_worker_memory_mb, required_mb,
      safety_margin * 100, available_mb
   ))

   invisible(TRUE)
}
