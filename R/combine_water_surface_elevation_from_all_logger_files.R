library(dplyr)
library(lubridate)
library(purrr)
library(readr)

# Load deployment metadata
deployments <- read.csv("C:/Users/Ben Philpot/Downloads/15Sep2022_RED_ArrayDataSheet - Sheet3.csv")
deployments$Serial <- as.character(deployments$Serial)

# Set minimum valid depth (meters)
min_depth <- 0.1


# Function to calculate water surface elevation near high tide
calculate_water_surface_elevation <- function(file, quantile = 0.75, min_depth = 0.1) {
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
   start_date <- as.POSIXct("2022-09-15 00:00:00")    # Start Date
   end_date   <- as.POSIXct("2022-10-20 23:59:59")    # End Date
   
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


# List all calibrated files
folder <- "C:/Users/Ben Philpot/OneDrive/Desktop/Water_Logger_Folder/Red_River/Loggers"
files <- list.files(folder, pattern = "\\.csv$", full.names = TRUE)

total_files <- length(files)



results_list <- list()
success_count <- 0

for (i in seq_along(files)) {
   file <- files[[i]]
   message("\n[", i, "/", total_files, "] Processing: ", basename(file))
   
   result <- tryCatch({
      tide_data <- calculate_water_surface_elevation(file, quantile = 0.75, min_depth = min_depth)
      
      # Preview head of each file
      message("→ Rows read: ", nrow(tide_data))
      print(head(tide_data, 3))
      
      success_count <- success_count + 1
      tide_data
   }, error = function(e) {
      message("❌ Failed on file: ", basename(file))
      message("   Error: ", e$message)
      return(NULL)
   })
   
   results_list[[i]] <- result
}



# Combine all successful results
all_results <- bind_rows(results_list)

# Calculate mean and standard deviation for each logger
logger_stats <- all_results %>%
   group_by(logger_id) %>%
   summarize(
      mean_wse = mean(water_surface_elevation, na.rm = TRUE),
      sd_wse = sd(water_surface_elevation, na.rm = TRUE)
   )

# Join these stats back to all_results
all_results <- left_join(all_results, logger_stats, by = "logger_id")

# View results
print(head(all_results))
