#' find_high_tides
#'
#' @param file 
#' @param start_date POSIXct. Start of the date-time window to analyze.
#' @param end_date POSIXct. End of the date-time window to analyze.
#' @param quantile Numeric. Quantile threshold for selecting high tide peaks (dont need it so quantile = 0)
#' @param min_depth Numeric. Minimum valid depth (meters) to include in calculations (default: 0.1)
#'
#' @returns
#' @export
#'
#' @examples
find_high_tides <- function(file, start_date, end_date, quantile = 0, min_depth = 0.1) {
   
   # Load with headers
   data <- read_csv(file, col_names = TRUE, skip = 1)
   
   # Identify datetime and depth columns
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
   
   filtered_data <- data %>%
      filter(date_time >= start_date & date_time <= end_date & depth >= min_depth)
   
   # Option 2: No smoothing (Ben advocates for this)
   smoothed_y <- filtered_data$depth
   
   # robust peaks
   peaks <- findpeaks(smoothed_y, minpeakheight = min_depth, minpeakdistance = 12)
   if (is.null(peaks)) {
      peak_df <- tibble(date_time = as.POSIXct(character()), depth = numeric())
   } else {
      valid_peaks <- peaks[,2]
      
      peak_df <- filtered_data[valid_peaks, ] %>%
         mutate(date = as.Date(date_time)) %>%
         group_by(date) %>%
         slice_max(order_by = depth, n = 2) %>%
         ungroup()
   }
   
   # Always return peak datetimes (empty if no peaks)
   peak_datetimes <- peak_df$date_time
   return(peak_datetimes)
}