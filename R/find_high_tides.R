#' find_high_tides
#' 
#' This function finds the peaks (high tides) of a single file
#'
#' @param file 
#' @param start_date POSIXct. Start of the date-time window to analyze.
#' @param end_date POSIXct. End of the date-time window to analyze.
#' @param min_depth Numeric. Minimum valid depth (meters) to include in calculations (default: 0.1)
#'
#' @returns
#' @export
#'
#' @examples
find_high_tides <- function(file, deployments, min_depth) {
   
   # Load with headers
   data <- read_water_logger_data(file, deployments)
   
   # Identify datetime and depth columns
   # Datetime column always 2nd column
   datetime_col <- names(data)[2]
   # Depth column by searching for 'depth' in the column names, case insensitive
   depth_col <- grep("depth", names(data), ignore.case = TRUE, value = TRUE)[1]
   
   data <- data %>% filter(!is.na(date_time), !is.na(depth))
   
  
   # robust peaks
   peaks <- findpeaks(data$depth, minpeakheight = min_depth, minpeakdistance = 12)
   if (is.null(peaks)) {
      peak_df <- tibble(date_time = as.POSIXct(character()), depth = numeric())
   } else {
      valid_peaks <- peaks[,2]
      
      peak_df <- data[valid_peaks, ] %>%
         mutate(date = as.Date(date_time)) %>%
         group_by(date) %>%
         slice_max(order_by = depth, n = 2) %>%
         ungroup()
   }
   
   # Always return peak datetimes (empty if no peaks)
   peak_datetimes <- peak_df$date_time
   return(peak_datetimes)
}