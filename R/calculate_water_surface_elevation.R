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
calculate_water_surface_elevation <- function(file, start_date, end_date, quantile = 0.75, min_depth = 0.1) {
   # Extract logger serial number
   logger_id <- substr(basename(file), 1, 8)
   elevation <- deployments$Elevation[deployments$Serial == logger_id]
   elevation <- elevation[!is.na(elevation)][1]
   
   # Read data (deleting extra header)
   data <- read_csv(file, col_names = FALSE, skip = 1, show_col_types = FALSE)[, c(2, 6)]
   
   # Convert datetime and depth
   data <- data %>%
      mutate(
         date_time = mdy_hms(data[[1]]),       # Column  datetime
         depth = as.numeric(data[[2]])       # Column  depth
      ) %>%
      filter(!is.na(date_time), !is.na(depth))
   
   # Filter by high tide window
   filtered_data <- data %>%
      filter(date_time >= start_date & date_time <= end_date)
   
   # Smooth depth with lowess
   bw <- 1 # bandwidth in hours
   lag <- difftime(filtered_data$date_time[2], filtered_data$date_time[1], units="hours") |> as.numeric()
   npoints <- bw * 2 / lag
   f <- npoints / nrow(filtered_data)
   smoothed <- lowess(filtered_data$depth, f = f)
   
   smoothed_depth <- smoothed$y
   before <- dplyr::lag(smoothed_depth, default = -Inf)
   after  <- dplyr::lead(smoothed_depth, default = -Inf)
   peaks  <- which(smoothed_depth > before & smoothed_depth > after)
   threshold <- quantile(smoothed_depth, quantile, na.rm = TRUE)
   valid_peaks <- peaks[smoothed_depth[peaks] > threshold]
   
   # Include nearby points
   focal_index <- outer(valid_peaks, -2:2, FUN = "+") |> as.vector() |> unique()
   focal_index <- focal_index[focal_index >= 1 & focal_index <= nrow(filtered_data)]
   tide_data <- filtered_data[focal_index, ]
   
   # Compute water surface elevation
   tide_data <- tide_data %>%
      mutate(
         logger_id = logger_id,
         elevation = elevation,
         depth = ifelse(depth < min_depth, NA, depth),
         water_surface_elevation = elevation + depth
      )
   
   return(tide_data)
}