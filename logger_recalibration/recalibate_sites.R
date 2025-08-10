library(dplyr)
library(lubridate)
library(purrr)
library(readr)

# Parameters
folder_path <- "C:/Users/Ben Philpot/OneDrive/Desktop/Water_Logger_Folder/Red_River/Loggers"
deployment_file <- "C:/Users/Ben Philpot/Downloads/15Sep2022_RED_ArrayDataSheet - Sheet3.csv"

# Set minimum valid depth (meters)
min_depth <- 0.1
quantile <- 0

# Set start date and end date
start_date <- as.POSIXct("2022-09-15 00:00:00")
end_date <- as.POSIXct("2022-10-05 23:59:59")

source("R/find_high_tides.R")
source("R/calculate_water_surface_elevation.R")
source("R/recalibrate_file.R")

files <- list.files(folder_path, pattern = "\\.csv$", full.names = TRUE)
selected_files <- files[1:5]

selected_files <- file.path(folder_path, c("21410933_cal.csv", "21410929_cal.csv", "21410927_cal.csv", "21410921_cal.csv", "20358395_cal.csv", "21384536_cal.csv"))

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
common_counts <- time_counts[time_counts >= 3]

# Convert the names back using the original timezone of your data
common_high_tides <- as.POSIXct(names(common_counts), tz = "America/New_York")


# Run folder processor
all_results <- recalibrate_file(folder_path, deployment_file, common_high_tides, start_date, end_date)

selected_data <- all_results %>%
   select(date_time, logger_id, depth, elevation, water_surface_elevation, mean_wse, sd_wse)

# View result
print(head(selected_data))


library(ggplot2)

raw_data_list <- lapply(selected_files, function(file) {
   df <- read_csv(file, col_names = TRUE, skip = 1, show_col_types = FALSE)
   
   # Pick column positions
   date_col_index <- names(data)[2]
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

# Step 2: Create data frame of high tide peaks for plotting
peaks_df <- purrr::map2_df(
   all_peak_times,
   basename(selected_files[seq_along(all_peak_times)]),
   ~ data.frame(
      file = .y,
      date_time = as.POSIXct(.x, origin = "1970-01-01", tz = "America/New_York")
   )
)

# Match peak times to depths from raw data
# (This will find the exact matching rows in raw_data_df)
peaks_df <- peaks_df %>%
   left_join(raw_data_df, by = c("file", "date_time"))

# Step 3: Plot
ggplot(raw_data_df, aes(x = date_time, y = depth)) +
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
   theme(
      plot.title = element_text(hjust = 0.5)
   )