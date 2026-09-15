#' Read a site's (and optionally a flight's) run configuration
#'
#' Reads `lidar/runs.yml` — the human-edited record of what
#' `lidar/tuning/01_csf_tuning.R` and
#' `lidar/tuning/02_ground_reference_selection.R` decided for a
#' site — and returns it as a plain list. Unlike [`pathtools::get_path()`],
#' this has no global session state to set up first: `runs.yml` is
#' re-read from disk on every call, since it's tiny and nothing calls
#' this from inside a parallel worker.
#'
#' @param site Lidar site code (e.g. `"rr"`).
#' @param date Flight date string in `"yyyy_mm_dd"` format (e.g.
#'    `"2022_08_10"`). If supplied, the returned list gains a
#'    `$flight` entry with that flight's `season` and `best_csf`.
#'    Default `NULL` (site-level configuration only).
#'
#' @return A list with `target_epsg`, `workers`, `chunk_size`,
#'    `chunk_buffer`, `ground_reference`, and `flights` (all flights
#'    recorded for `site`), plus `flight` (that one flight's entry)
#'    when `date` is supplied.
#'
#' @examples
#' \dontrun{
#' get_run_config("rr")
#' get_run_config("rr", date = "2022_08_10")$flight$best_csf
#' }
get_run_config <- function(site, date = NULL) {

   runs_path <- pathtools::get_path("run_config")
   runs <- yaml::read_yaml(runs_path)

   site_config <- runs$sites[[site]]
   if (is.null(site_config)) {
      stop(sprintf(
         paste(
            "No run configuration for site '%s' in %s. Add a",
            "`sites.%s` block (copy an existing site as a template)",
            "before running tuning or production drivers for it."
         ),
         site, runs_path, site
      ))
   }

   if (!is.null(date)) {
      flight <- site_config$flights[[date]]
      if (is.null(flight)) {
         stop(sprintf(
            paste(
               "No flight '%s' recorded for site '%s' in %s. Run",
               "lidar/tuning/01_csf_tuning.R for this flight and",
               "record its best_csf before running production",
               "drivers for it."
            ),
            date, site, runs_path
         ))
      }
      site_config$flight <- flight
   }

   site_config
}
