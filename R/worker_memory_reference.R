#' Reference calibration points for [estimate_worker_memory_mb()]
#'
#' One row per process, giving an observed peak per-worker memory
#' (`memory_mb`) at a specific tile geometry (`chunk_size`,
#' `chunk_buffer`) and point density (`density`). [estimate_worker_memory_mb()]
#' scales linearly from these reference points to estimate expected
#' per-worker memory at other parameter combinations.
#'
#' Add a row whenever a new process adopts the memory check.
#' `memory_mb` should reflect an observed peak per-worker memory across
#' all chunks, not a typical/average value.
#'
#' @details
#' Current rows:
#' * `rasterize_ground` — rr spring cleaned catalog, 2026-09-15,
#'   observed peak 11,498 MB across all chunks at `chunk_size = 200`,
#'   `chunk_buffer = 20`, `density = 243.65`.
"worker_memory_reference"

worker_memory_reference <- data.frame(
   process          = c("rasterize_ground"),
   chunk_size       = c(200),
   chunk_buffer     = c(20),
   density          = c(243.65),
   memory_mb        = c(11498),
   stringsAsFactors = FALSE
)
