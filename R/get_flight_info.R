#' Retrieve lidar flight information for a site from runs.yml
#'
#' @param site The target site code. Typically 3 lower case letters
#' @param config list as returned by `get_run_config()` (optional), pass in
#' to save re-parsing the file.
#' @returns A data.frame with \
#' \item{site}{the three digit site code}
#' \item{date}{flight dates as numeral `{year}_{month}_{day}` - as they
#' appear in paths.}
#' \item{season}{The season assigned to the flight in runs.yml}
#' @export
#'
#' @examples
#' \dontrun{
#' get_flight_info("esx")
#' }
get_flight_info <- function(site, config = NULL) {
   if (is.null(config))
      config <- tryCatch(get_run_config(site), error = function(e) {
         message("No lidar/runs.yml entry for '", site, "' yet -- comparing ",
                 "photo/USGS sources only.")
      })

   flight_dates <- names(config$flights)
   seasons <- vapply(config$flights, \(x) x$season, character(1)) |> as.vector()

   data.frame(site = site, date = flight_dates, season = seasons)
}
