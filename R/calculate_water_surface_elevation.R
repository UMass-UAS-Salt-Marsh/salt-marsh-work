#' calculate_water_surface_elevation
#'
#' This function processes a single water logger CSV file, smooths the depth readings,
#' identifies high tide peaks, and calculates water surface elevation at those points.
#' 
#' @param file Character. Path to the CSV file from a water level logger.
#' @param start_date POSIXct. Start of the date-time window to analyze.
#' @param end_date POSIXct. End of the date-time window to analyze.
#' @param quantile Numeric. Quantile threshold for selecting high tide peaks (default: 0.75).
#' @param min_depth Numeric. Minimum valid depth (meters) to include in calculations (default: 0.1).
#'
#' @return A data frame with datetime, depth, water surface elevation, logger ID, and elevation.
#'
#' @examples
#' 
calculate_water_surface_elevation <- function(file, common_high_tides, start_date, end_date, quantile = 0.75, min_depth = 0.1) {
   
   # Extract logger serial number
   logger_id <- substr(basename(file), 1, 8)
   elevation <- deployments$Elevation[deployments$Serial == logger_id]
   elevation <- elevation[!is.na(elevation)][1]
   
   
   # Load with headers
   data <- read_csv(file, col_names = TRUE, skip = 1)
   
   # Datetime column always 2nd column
   datetime_col <- names(data)[2]
   # Depth column by searching for 'depth' in the column names, case insensitive
   depth_col <- grep("depth", names(data), ignore.case = TRUE, value = TRUE)[1]
   
   data <- data %>%
      mutate(
         date_time = parse_date_time(.data[[datetime_col]], orders = "mdy IMS p"),
         depth = as.numeric(.data[[depth_col]])
      ) %>%
      filter(!is.na(date_time), !is.na(depth))
   
   # Now filter to only common high tides time stamps
   tide_data <- data %>%
      filter(date_time %in% common_high_tides) %>%
      mutate(
         logger_id = logger_id,
         elevation = elevation,
         water_surface_elevation = elevation + depth
      )
   
   return(tide_data)
}