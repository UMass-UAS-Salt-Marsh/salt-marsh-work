library(dplyr)
library(lubridate)
library(purrr)
library(readr)
library(pracma) # This is used to find peaks accurately
library(ggplot2) # This is to plot data
library(tidyverse) # I dont even know what this is for or if it's needed

# Parameters
folder_path <- "C:/Users/Ben Philpot/OneDrive/Desktop/Water_Logger_Folder/Red_River/Loggers"
deployment_file <- "C:/Users/Ben Philpot/Downloads/15Sep2022_RED_ArrayDataSheet - Sheet3.csv"
spatial_file_path <- "C:/Users/Ben Philpot/OneDrive/Desktop/Water_Logger_Folder/Red_River/15Sep2022_RED_ArrayDataSheet.csv"

# Set minimum valid depth (meters)
min_depth <- 0.1
quantile <- 0

# Set start date and end date
start_date <- as.POSIXct("2022-09-15 00:00:00")
end_date   <- as.POSIXct("2022-10-05 23:59:59")

source("R/find_high_tides.R")
source("R/calculate_water_surface_elevation.R")
source("R/recalibrate_file.R")
source("R/find_common_high_tides.R")
source("R/spatial_plotting.R")

results <- find_common_high_tides(folder_path, deployment_file, start_date, end_date, quantile = 0, min_depth = 0.1)

common_high_tides <- results$common_high_tides
selected_files    <- results$selected_files
all_peak_times    <- results$all_peak_times

# Run folder processor
all_results <- recalibrate_file(folder_path, deployment_file, common_high_tides, start_date, end_date)

selected_data <- all_results %>%
   select(date_time, logger_id, depth, elevation, water_surface_elevation, mean_wse, sd_wse, difference_wse, mean_wse_diff, sd_wse_diff, ratio)

# View result
print(head(selected_data))

# This plots spatial logger error:
p1 <- plot_logger_error(selected_data, spatial_file_path)
print(p1)

# This plots 5 common high tide files:
p2 <- plot_high_tides(selected_files, all_peak_times, start_date, end_date)
print(p2)

# This plots all files projected against common high tide times:
p3 <- plot_all_loggers_with_high_tides(folder_path, common_high_tides, start_date, end_date)
print(p3)

