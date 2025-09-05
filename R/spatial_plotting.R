#' spatial_plotting
#'
#' Takes results and visualizes data
#'
#' @param selected_data results from recalibrate_sites
#' @param spatial_file_path defined globally in recalibrate_sites, includes Easting and Northing data 
#' @param start_date POSIXct. Start date-time for filtering
#' @param end_date POSIXct. End date-time for filtering
#' @param all_peak_times returned in recalibrate_sites, contains all peak times from raw data from all logger files
#'
#' @return 3 different graphs: p1 is for spatial logger error, p2 plots 5 common high tide files, and 
#'          p3 plots all files projected against common high tide times (2 day time frame)
#' @export
map_logger_error <- function(selected_data, spatial_file_path) {
   spatial_file <- read_csv(spatial_file_path)
   spatial_file$Serial <- as.character(spatial_file$Serial)
   
   # Join summary table with spatial file
   spatial_logger_error <- selected_data %>%
      left_join(spatial_file, by = c("logger_id" = "Serial")) |>
      filter(!is.na(Eastings), !is.na(Northings))
   
   
   sf <- sf::st_as_sf(spatial_logger_error, coords = c("Eastings", "Northings"), crs = 26919)
   
   latlon <- sf |> sf::st_transform("epsg:4326")
   
   d <- cbind(spatial_logger_error, sf::st_coordinates(latlon))
   
   # Custom palette: neutral at extremes, strong at 0
   pal <- colorNumeric(
      palette = c("grey90", "red", "grey90"), # low, mid, high
      domain  = c(-0.1, 0.1)
   )
      
      p1 <- leaflet(d)|>
         addProviderTiles("OpenStreetMap")|>
         addCircleMarkers(~X, ~Y, radius = ~sd_wse_diff * 100, color  = ~pal(mean_wse_diff), fillOpacity = 0.8)
   
   return(p1)
}


plot_logger_error <- function(selected_data, spatial_file_path) {
   spatial_file <- read_csv(spatial_file_path)
   spatial_file$Serial <- as.character(spatial_file$Serial)
   
   # Join summary table with spatial file
   spatial_logger_error <- selected_data %>%
      left_join(spatial_file, by = c("logger_id" = "Serial"))
   
   # Plot
   p1 <- ggplot(spatial_logger_error, aes(x = Eastings, y = Northings)) +
      geom_point(aes(color = mean_wse_diff, size = sd_wse_diff)) +
      scale_color_viridis_c(option = "turbo") +
      coord_equal() +
      theme_minimal() +
      labs(
         title = "Spatial Distribution of Logger Error",
         x = "Eastings",
         y = "Northings",
         color = "Mean WSE Diff",
         size = "SD WSE Diff"
      )
   
   return(p1)
}

# didn't graph ratio, not sure what I am looking for when I do that????


# Function to plot high tide peaks for selected files
plot_high_tides <- function(selected_files, all_peak_times, start_date, end_date) {
   # Read in and clean raw data
   raw_data_list <- lapply(selected_files, function(file) {
      df <- read_csv(file, col_names = TRUE, skip = 1, show_col_types = FALSE)
      
      # Identify columns
      date_col_index <- names(df)[2]
      depth_col_index <- grep("depth", names(df), ignore.case = TRUE)[1]
      
      df <- df %>%
         mutate(
            date_time = parse_date_time(df[[date_col_index]], orders = "mdy IMS p"),
            depth = as.numeric(df[[depth_col_index]]),
            file = basename(file)
         ) %>%
         filter(date_time >= start_date & date_time <= end_date) %>%
         select(date_time, depth, file)
      
      return(df)
   })
   
   raw_data_df <- bind_rows(raw_data_list)
   
   # Build peak dataframe
   peaks_df <- purrr::map2_df(
      all_peak_times,
      basename(selected_files[seq_along(all_peak_times)]),
      ~ data.frame(
         file = .y,
         date_time = as.POSIXct(.x, origin = "1970-01-01", tz = "America/New_York")
      )
   )
   
   peaks_df <- peaks_df %>%
      left_join(raw_data_df, by = c("file", "date_time"))
   
   # Plot
   p2 <- ggplot(raw_data_df, aes(x = date_time, y = depth)) +
      geom_line(color = "black") +
      geom_point(data = peaks_df, aes(x = date_time, y = depth),
                 color = "blue", size = 3) +
      facet_wrap(~ file, scales = "free_y") +
      labs(
         title = "Water Depth with High Tide Peaks",
         x = "Time",
         y = "Depth (m)"
      ) +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5))
   
   return(p2)
}

plot_all_loggers_with_high_tides <- function(folder_path, common_high_tides,
                                             start_date, end_date) {
   
   start_date <- as.POSIXct("2022-09-15 00:00:00")
   end_date   <- as.POSIXct("2022-09-17 23:59:59")
   
   # List CSV files
   files <- list.files(folder_path, pattern = "\\.csv$", full.names = TRUE)
   
   # Read all logger files
   raw_data_list <- lapply(files, function(file) {
      df <- read_csv(file, col_names = TRUE, skip = 1, show_col_types = FALSE)
      
      date_col_index <- names(df)[2]
      depth_col_index <- grep("depth", names(df), ignore.case = TRUE)[1]
      
      df %>%
         mutate(
            date_time = parse_date_time(df[[date_col_index]], orders = "mdy IMS p"),
            depth = as.numeric(df[[depth_col_index]]),
            file = basename(file)
         ) %>%
         filter(date_time >= start_date & date_time <= end_date) %>%
         select(date_time, depth, file)
   })
   
   raw_data_df <- bind_rows(raw_data_list)
   
   # Convert common high tides to POSIXct if needed
   high_tides_df <- data.frame(
      date_time = as.POSIXct(common_high_tides, origin="1970-01-01", tz="America/New_York")
   )
   
   # Plot
   p3 <- ggplot(raw_data_df, aes(x = date_time, y = depth)) +
      geom_line(color = "black", size = 0.5) +  # thicker raw data line
      geom_vline(data = high_tides_df, aes(xintercept = date_time),
                 color = "red", linetype = "solid", size = 0.1) +  # thin red solid lines
      facet_wrap(~ file, scales = "free_y") +
      labs(title = "All Logger Depths with Common High Tide Marks",
           x = "Time", y = "Depth (m)") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5))
   return(p3)
}