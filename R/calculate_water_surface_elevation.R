#' calculate_water_surface_elevation
#'
#' This function processes a single water logger CSV file and calculates water
#'  surface elevation at common high tide times by adding elevation and depth.
#' 
#' @param file Character. Path a CSV file with calibrated water depth.
#' @param common_high_tides Date time vector with identified common high tides
#' @param deployments Data frame with deployment information as returned by 
#' [read_water_logger_deployments()]
#' 
#' @return A data frame with 
#' \item{date_time}{Date and time of record}
#' \item{depth}{} depth, water surface elevation, 
#' logger ID, and elevation.
#'
#' @examples
#' 
calculate_water_surface_elevation <- function(file, common_high_tides, deployments) {
   
   # Extract logger serial number
   logger_id <- substr(basename(file), 1, 8)
   elevation <- deployments$elevation[deployments$serial == logger_id]
   elevation <- elevation[!is.na(elevation)][1]
   
   # Load with headers
   data <- read_water_logger_data(file, deployments)
   
   # Now filter to only common high tides time stamps
   tide_data <- data %>%
      filter(date_time %in% common_high_tides) %>%
      mutate(
         serial = serial,
         elevation = elevation,
         water_surface_elevation = elevation + depth
      )
   
   return(tide_data)
}