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
   
   
   
   
   # DEBUG PRINTS here:
   print(paste("Processing file:", basename(file)))
   print("Filtered data preview:")
   print(head(filtered_data))
   print("Depth column structure:")
   print(str(filtered_data$depth))
   
   if (nrow(filtered_data) == 0 || all(is.na(filtered_data$depth))) {
      stop(paste("No valid depth data for", basename(file)))
      return(NULL)
   }
   
   
   
   
   
   
   
   
   # Smooth depth
   bw <- 1 # bandwidth in hours
   lag <- difftime(filtered_data$date_time[2], filtered_data$date_time[1], units = "hours") |> as.numeric()
   npoints <- bw * 2 / lag
   f <- npoints / nrow(filtered_data)
   smoothed <- lowess(filtered_data$depth, f = f)
   
   smoothed_depth <- smoothed$y
   before <- dplyr::lag(smoothed_depth, default = -Inf)
   after  <- dplyr::lead(smoothed_depth, default = -Inf)
   peaks  <- which(smoothed_depth > before & smoothed_depth > after)
   threshold <- quantile(smoothed_depth, quantile, na.rm = TRUE)
   valid_peaks <- peaks[smoothed_depth[peaks] > threshold]
   
   focal_index <- outer(valid_peaks, -2:2, FUN = "+") |> as.vector() |> unique()
   focal_index <- focal_index[focal_index >= 1 & focal_index <= nrow(filtered_data)]
   focal_index <- sort(focal_index)
   tide_data <- filtered_data[focal_index, ]
   
   peak_datetimes <- tide_data$date_time
   return(peak_datetimes)
}