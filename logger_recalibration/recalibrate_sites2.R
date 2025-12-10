
# Load R Packages

library(dplyr)
library(readr)
library(lubridate)
library(tidyverse)
library(readr)
library(readxl)
library(VulnToolkit)
library(mblm)
library(purrr)
library(pracma) # This is used to find peaks accurately
library(ggplot2) # This is to plot data
library(leaflet)
library(sf)
library(leaflegend)

# Source all R files in /R
a <- list.files("R/", pattern = "\\.[Rr]$", full.names = TRUE) |> 
   lapply(source)

min_depth <- 0.1

# Read in standard site table with site abbreviation and name
sites <- readr::read_tsv("hydrology/Data/sites.txt", show_col_types = FALSE) |>
   select(site, site_name)

sites$site <- toupper(sites$site)
sites$site[sites$site == "RR"] <- "RED"

selected_sites <- c("RED", "OTH", "WES", "WEL") 
sites <- sites[match(selected_sites, sites$site), ]
sites$deployment_file_name <- c("15Sep2022_RED_Deployments.csv",
                                "OTH_LoggerArray_Pull.csv",
                                "15Aug2022_WES_ArrayDataSheet.csv",
                                "19Oct2022_WEL_Array_withFloodingMetrics.csv")
i <- 3

result <- vector(mode = "list", length = nrow(sites))
# for (i in seq_len(nrow(sites))) {
   site <- sites$site[i]
   site_name <- sites$site_name[i]
   message("Processing ", site, " (", site_name, ") " , i, " of ", nrow(sites), "\n" )
   
   site_data_dir <- file.path("hydrology/Data/", site)
   calibrated_dir <- file.path(site_data_dir, "Calibrated Data")
   deployment_file <- file.path(site_data_dir, sites$deployment_file_name[i])
   
   deployments <- read_water_logger_deployments(deployment_file)
   

   
   results <- find_common_high_tides(calibrated_dir, deployments, min_depth = min_depth)
   
   common_high_tides <- results$common_high_tides
   selected_files    <- results$selected_files
   all_peak_times    <- results$all_peak_times
   
   # Run folder processor
   d <- assess_water_logger_errors(calibrated_dir, deployments, common_high_tides)
   
   # add lat lon and drop sites with NA
   spatial <- prep_spatial_data(d, deployments)
   
   # View result
   print(head(d))
   
   # This plots spatial logger error (red means mean_wse_diff is closer to 0):
   p1 <- map_logger_error(spatial[!is.na(spatial$sd_wse_diff), ], deployments, opacity = .7, max_radius_pixels = 200)
   print(p1)
   
   # This plots 5 common high tide files:
   p2 <- plot_high_tides(selected_files, , deployments)
   print(p2)
   
   # This plots all files projected against common high tide times (2 day time frame):
   start <- min(deployments$date_time_deployed)
   end <-  start + lubridate::make_difftime(2 * 3600 * 24, units = "days")
   
   
   p3 <- plot_all_loggers_with_high_tides(calibrated_dir, common_high_tides, start, end)
   print(p3)
   
# }