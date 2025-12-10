prep_spatial_data <- function(selected_data, deployments = NULL) {
   
   
   deployments$serial <- as.character(deployments$serial)
   
   deployments <- filter(deployments, !is.na(eastings), !is.na(northings))
   
   d <- selected_data |> 
      group_by(serial) |> 
      summarise(mean_wse_diff = first(mean_wse_diff), 
                sd_wse_diff = first( sd_wse_diff),
                ratio = first(ratio),
                n = first(n)) |>
      ungroup() |>
      filter(!is.na(mean_wse_diff))
   
   
   sf <- sf::st_as_sf(deployments, coords = c("eastings", "northings"), crs = 26919)
   
   latlon <- sf |> 
      sf::st_transform("epsg:4326") |> 
      sf::st_coordinates() |> 
      as.data.frame()
   names(latlon) <- c("lon", "lat")
   latlon$serial <- deployments$serial
   latlon$eastings <- deployments$eastings
   latlon$northings <- deployments$northings
   
   
   d <- left_join(d, latlon, join_by(serial == serial) )
   
   
   return(d)
}



#' spatial_plotting
#'
#' Takes results and visualizes data
#'
#' @param selected_data results from recalibrate_sites
#' @param spatial_file_path defined globally in recalibrate_sites, includes Easting and Northing data 
#' @param deployments table of deployment information (alternative to spatial_file_path)
#' @param opacity opacity used in plotting between 0 (transparent) and 1 (fully opaque)
#' @return 3 different graphs: p1 is for spatial logger error, p2 plots 5 common high tide files, and 
#'          p3 plots all files projected against common high tide times (2 day time frame)
#' @export
map_logger_error <- function(selected_data, deployments,    
                             opacity = 1, max_radius_pixels = 50) {
   
   if(!all(c("lat", "lon") %in% names(d)))
      d <- prep_spatial_data(selected_data, deployments)
   
   # Custom palette: red at low yellow at neutral and blue at high
   magnitude <- max(abs(d$mean_wse_diff))
   
   pal <- colorNumeric(
      palette = c("red", "yellow", "blue"), # low, mid, high
      domain  = c(-magnitude, magnitude)
   )

   
   
   radius_multiplier =  max_radius_pixels  / max(d$sd_wse_diff, na.rm = TRUE) 
   
   d$label <- paste0(d$serial, "<br>", 
                     "Difference:<br>",
                     "  Mean:", signif(d$mean_wse_diff, 2), "<br>", 
                     "  SD:", round(d$sd_wse_diff, 2)) |> lapply(HTML)
   
   
   p1 <- leaflet(d)|>
      addProviderTiles("Esri.WorldImagery")|>  #Esri.WorldImagery USGS.USImagery Stadia.AlidadeSatellite  OpenStreetMap
      addCircleMarkers(~lon, ~lat, 
                       radius = ~sd_wse_diff * max_radius_pixels, 
                       color  = ~pal(mean_wse_diff), 
                       opacity = opacity, 
                       label = ~label) |>
      addLegend_decreasing(pal = pal, values = ~mean_wse_diff, decreasing = TRUE,
                title = "Mean logger difference", opacity = opacity) 
   
   
   # |> 
   #   addLegendSize(values = ~sd_wse_diff,  title = "SD logger diff.", 
   #                 color = "black", shape = "circle", fillColor = "black", 
   #                 opacity = opacity, breaks = 5)
   
   return(p1)
}




plot_logger_error <- function(selected_data, spatial_file_path, deployments) {
   d <- prep_spatial_data(selected_data, spatial_file_path, deployments)
   
   # Plot
   p1 <- ggplot(d, aes(x = Eastings, y = Northings)) +
      geom_point(aes(color = mean_wse_diff, size = sd_wse_diff)) +
      # scale_color_viridis_c(option = "turbo") +
      scale_color_gradient2() + 
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


# Function to plot common high tides for selected files
plot_high_tides <- function(selected_files, common_high_tides, deployments, start = NULL, end = NULL) {
   # Read in and clean raw data
   data_list <- lapply(selected_files, function(x) read_water_logger_data(x, deployments))
   data <- bind_rows(data_list)
   
   if(!is.null(start))
      data <- filter(data, date_time > start)
   
   if(!is.null(start))
      data <- filter(data, date_time < end)
   
   
   serials <- unique(data$serial)
   
   
   common_high_tides
   peaks_df <- expand_grid(serial = serials, date_time = common_high_tides)
      
   # # Build peak dataframe
   # peaks_df <- purrr::map2_df(
   #    all_peak_times, serials,
   #    ~ data.frame(
   #       serial = .y,
   #       date_time = as.POSIXct(.x, origin = "1970-01-01", tz = "America/New_York")
   #    )
   # )
   
   peaks_df <- peaks_df %>%
      left_join(data, by = c("serial", "date_time"))
   
   # Plot
   p2 <- ggplot(data, aes(x = date_time, y = depth)) +
      geom_line(color = "black") +
      geom_point(data = peaks_df, aes(x = date_time, y = depth),
                 color = "blue", size = 3, alpha = 0.5) +
      facet_wrap(~ serial, scales = "free_y") +
      labs(
         title = "Water Depth with High Tide Peaks",
         x = "Time",
         y = "Depth (m)"
      ) +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5))
   
   return(p2)
}

plot_all_loggers_with_high_tides <- function(calibrated_dir, common_high_tides,
                                             start, end, min_depth = 0.1) {
   

   
   # List CSV files
   files <- list.files(calibrated_dir, pattern = logger_data_file_pattern, 
                       full.names = TRUE)
   
   data_list <- lapply(files, function(x) {
      tryCatch(read_water_logger_data(x, deployments),
               error = function(e) return(NULL))})
   
   
   data <- bind_rows(data_list)
   
   if(!is.null(start))
      data <- filter(data, date_time > start)
   
   if(!is.null(start))
      data <- filter(data, date_time < end)
   
   
   # Convert common high tides to POSIXct if needed
   high_tides_df <- data.frame(
      date_time = as.POSIXct(common_high_tides, origin="1970-01-01", tz="America/New_York")
   )
   
   # Plot
   p3 <- ggplot(data, aes(x = date_time, y = depth)) +
      geom_line(color = "black", size = 0.5) +  # thicker raw data line
      geom_vline(data = high_tides_df, aes(xintercept = date_time),
                 color = "red", linetype = "solid", linewidth = 0.1) +  # thin red solid lines
      geom_hline(data = high_tides_df, aes(yintercept = min_depth),
                 color = "blue", linetype = "solid", linewidth = 0.1) +  # blue line at min depth
      facet_wrap(~ serial) + #, scales = "free_y") +
      labs(title = "All Logger Depths with Common High Tide Marks",
           x = "Time", y = "Depth (m)") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5))
   return(p3)
}