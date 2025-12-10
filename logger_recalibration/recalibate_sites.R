library(dplyr)
library(lubridate)
library(purrr)
library(readr)
library(pracma) # This is used to find peaks accurately
library(ggplot2) # This is to plot data
library(tidyverse) # I dont even know what this is for or if it's needed
library(leaflet)
library(sf)


# Source all R files in /R
a <- list.files("R/", pattern = "\\.[Rr]$", full.names = TRUE) |> 
   lapply(source)


# Parameters
folder_path <- "C:/Users/Ben Philpot/OneDrive/Desktop/Water_Logger_Folder/Red_River/Loggers"
deployment_file <- "C:/Users/Ben Philpot/Downloads/15Sep2022_RED_ArrayDataSheet - Sheet3.csv"
spatial_file_path <- "C:/Users/Ben Philpot/OneDrive/Desktop/Water_Logger_Folder/Red_River/15Sep2022_RED_ArrayDataSheet.csv"

# Set minimum valid depth (meters)
min_depth <- 0.1

# Set start date and end date
start_date <- as.POSIXct("2022-09-15 00:00:00")
end_date   <- as.POSIXct("2022-10-05 23:59:59")


results <- find_common_high_tides(folder_path, deployment_file, min_depth = min_depth)

common_high_tides <- results$common_high_tides
selected_files    <- results$selected_files
all_peak_times    <- results$all_peak_times

# Run folder processor
all_results <- assess_water_logger_errors(folder_path, deployment_file, common_high_tides, start_date, end_date)

selected_data <- all_results %>%
   select(date_time, logger_id, depth, elevation, water_surface_elevation, mean_wse, sd_wse, difference_wse, mean_wse_diff, sd_wse_diff, ratio)

# View result
print(head(selected_data))

# This plots spatial logger error (red means mean_wse_diff is closer to 0):
p1 <- map_logger_error(selected_data, spatial_file_path)
print(p1)

# This plots 5 common high tide files:
p2 <- plot_high_tides(selected_files, all_peak_times, start_date, end_date)
print(p2)

# This plots all files projected against common high tide times (2 day time frame):
p3 <- plot_all_loggers_with_high_tides(folder_path, common_high_tides, start_date, end_date)
print(p3)