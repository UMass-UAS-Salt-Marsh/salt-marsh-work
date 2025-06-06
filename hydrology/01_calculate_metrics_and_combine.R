

library(tidyverse)
library(readr)
library(readxl)
library(VulnToolkit)
library(mblm)

# Source files in R directory
a <- list.files("R/", pattern = "\\.[Rr]$", full.names = TRUE) |> lapply(source)


# Read Site info

# Loop through sites.

# Read in site level data and add site column

# Composite dataset


# Read in standard site table with site abbreviation and name
sites <- readr::read_tsv("hydrology/Data/sites.txt", show_col_types = FALSE) |>
select(site, site_name)

selected_sites <- c("RED", "OTH", "WES", "WEL")
sites <- sites[match(selected_sites, sites$site), ]
sites$deployment_file_name <- c("15Sep2022_RED_Deployments.csv",
                                 "OTH_LoggerArray_Pull.csv",
                                 "15Aug2022_WES_ArrayDataSheet.csv",
                                 "19Oct2022_WEL_Array_withFloodingMetrics.csv")
i <- 4

result <- vector(mode = "list", length = nrow(sites))
for (i in seq_len(nrow(sites))) {
  site <- sites$site[i]
  site_name <- sites$site_name[i]
  message("Processing ", site, " (", site_name, ") " , i, " of ", nrow(sites), "\n" )

  site_data_dir <- file.path("hydrology/Data/", site)
  calibrated_dir <- file.path(site_data_dir, "Calibrated Data")
  deployment_file <- file.path(site_data_dir, sites$deployment_file_name[i])


  im <- calculate_inundation_metrics(calibrated_dir = calibrated_dir,
                                     deployment_file = deployment_file)

  im$Site <- site

  result[[i]] <- im
}

data <- do.call(rbind, result)

readr::write_rds(data, "hydrology/Data/four_sites.Rds")


