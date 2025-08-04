library(dplyr)
library(lubridate)
library(purrr)
library(readr)

# Parameters
folder_path <- "C:/Users/Ben Philpot/OneDrive/Desktop/Water_Logger_Folder/Red_River/Loggers"
deployment_file <- "C:/Users/Ben Philpot/Downloads/15Sep2022_RED_ArrayDataSheet - Sheet3.csv"

# Set minimum valid depth (meters)
min_depth <- 0.1

# Set start date and end date
start_date <- as.POSIXct("2022-09-15 00:00:00")
end_date   <- as.POSIXct("2022-10-20 23:59:59")

source("R/find_high_tides.R")
source("R/calculate_water_surface_elevation.R")
source("R/recalibrate_file.R")

files <- list.files(folder_path, pattern = "\\.csv$", full.names = TRUE)
selected_files <- files[1:5]

all_peak_times <- lapply(selected_files, function(file) {
   peaks <- find_high_tides(file, start_date, end_date)
   if (length(peaks) > 0) return(peaks) else return(NULL)
})

# Remove NULL entries
all_peak_times <- Filter(Negate(is.null), all_peak_times)

# Combine and filter peak times
all_times <- as.POSIXct(unlist(all_peak_times), origin = "1970-01-01", tz = "America/New_York")
time_counts <- table(all_times)

# Filter to common tides
common_counts <- time_counts[time_counts >= 2]

# Convert the names back using the original timezone of your data
common_high_tides <- as.POSIXct(names(common_counts), tz = "America/New_York")


# Run folder processor
all_results <- recalibrate_file(folder_path, deployment_file, start_date, end_date)

selected_data <- all_results %>%
   select(date_time, logger_id, depth, elevation, water_surface_elevation, mean_wse, sd_wse)

# View result
print(head(selected_data))