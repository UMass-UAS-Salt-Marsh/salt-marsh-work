library(dplyr)
library(lubridate)
library(purrr)
library(readr)

# Parameters
folder <- "C:/Users/Ben Philpot/OneDrive/Desktop/Water_Logger_Folder/Red_River/Loggers"
deployment_file <- "C:/Users/Ben Philpot/Downloads/15Sep2022_RED_ArrayDataSheet - Sheet3.csv"

# Set minimum valid depth (meters)
min_depth <- 0.1

# Set start date and end date
start_date <- as.POSIXct("2022-09-15 00:00:00")
end_date   <- as.POSIXct("2022-10-20 23:59:59")


   source("R/calculate_water_surface_elevation.R")

   source("R/process_calibrated_folder_water_logger_elevation.R")


# Run folder processor
all_results <- process_calibrated_folder_water_logger_elevation(folder, deployment_file, start_date, end_date)

# View result
print(head(all_results))