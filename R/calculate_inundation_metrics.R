#2025-05-05

#' Calculate inundation metrics from water depth loggers
#'
#' `calculate_inundation_metrics()` will calculate several different
#' metrics summarizing inundation statistics for all the water depth loggers
#' at a site.
#'
#' @param calibrated_dir The path to a directory containing calibrated logger
#' output. These should be excel files with tabular data.  The columns
#' names are cleaned with regular expressions that deletes the first comma
#' and everything after it, replaces blanks with "_", deletes periods,
#' and converts the names to lower case.  Finally, due to variability in the
#' naming of the depth column after applying the above cleaning it converts
#' `"sendor_depth"` or `"sensor_depth_brackish"`, if present, to `"depth"`.
#' Only two columns are used from each file: `"date_time"`, and
#' `"sensor_depth"`.
#'
#' @param deployment_file The path to a CSV file containing information
#' about all the deployments at the site. After reading this file the
#' names are converted to lower case and blanks are replaced with "_".
#' Both `"Serial"` and `"Serial #"` are accepted names for the `"Serial"`
#' column (prior to cleaning). After cleaning the required columns are
#' `"serial"`, `"date_deployed"`, `"time_deployed"`, `"date_pulled"`,
#' `"time_pulled"`, `"northings"`,`"eastings"`, and `"elevation"`.
#'
#' @returns A data frame with columns:
#' \item{`serial_number`}{The logger serial number}
#' \item{`Proportion_Time_Inundated`}{The proportion of time the logger is
#' inundated (depth > 0.02). Calculated using [VulnToolkit::fld.dur()]}
#' \item{`Median_Time_Inundated`}{The median time a logger was inundated in
#' hours. Calculated with [VulnToolkit::dur.events()]}
#' \item{`Median_Inundation_Level`}{The median depth of inundation, this is
#' the median height of the water surface (above sea level) for all times that
#' the logger was inundated. Calculated using [VulnToolkit::fld.depth()].
#' This was previously named `Median_Inundation_Depth`.}
#' \item{`Elevation`}{The elevation of the logger in meters. Taken from the
#' deployments table.}
#' \item{`Type`}{The type of terrain the logger was deployed on -
#' from the deployments table. Will be one of `"Creek"`, `"Ditch"`,
#' `"Edge"`, `"Plat"`, or `"Pond"`}
#' \item{`Percentile_80_Depth`, `Percentile_95_Depth`}{The 80th and 90th
#' percentile of depths recorded by the logger.
#' `Median_Inundation_Level` this is relative to the logger's elevation,
#' not sea level.)}
#' \item{`Wet_75_Percentile_Depth`}{The 75th percentile of the depth excluding
#' times when then logger is not inundated (depth < 0.02). This is relative
#' to the logger not sea level.}
#' \item{ `Easting`, `Northing`}{Logger location.}
#' N
#' A logger depth of 0.02 meters was used as the threshold of inundation in
#' these calculations.
#'
#' @export
#' @examples
calculate_inundation_metrics <- function(calibrated_dir, deployment_file) {
   
   
   calibrated_files <- list.files(path = file.path(calibrated_dir),
                                  pattern = "cal.xlsx", full.names = TRUE)
   
   if (!file.exists(deployment_file)) {
      stop("CSV file with deployment information not found at ",
           deployment_file)
   }
   
   if(!length(calibrated_files) > 1) {
      stop("Expected multiple calibrated logger files. ",
           "Found ", length(calibrated_files),
           " in ", calibrated_dir)
   }
   
   
   # Read deployment data  (AKA elevation data "elev")
   deployments <- read_water_logger_deployments(deployment_file)
   
   # Initialize empty result table
   n <- length(calibrated_files)
   flood_table = tibble('serial_number'=  integer(n),
                        'Proportion_Time_Inundated'= numeric(n),
                        'Median_Time_Inundated' = numeric(n),
                        'Median_Inundation_Level' = numeric(n),
                        'Elevation' = numeric(n),
                        'Type' = character(n),
                        'Percentile_80_Depth' = numeric(n),
                        'Percentile_95_Depth' = numeric(n),
                        'Wet_75_Percentile_Depth' = numeric(n))
   
   # Process calibrated logger files in loop
   message("Processing logger files\n")
   skipped <- numeric(0) # keep track of any skipped  serial numbers
   for (i in seq_along(calibrated_files)) {
      if(i %% 5 == 0)
         cat("\t", round(i / length(calibrated_files)* 100, 1), "%\n")
      
      # Set file path and serial number
      file <- calibrated_files[i]
      
      a <- tryCatch({
         data <- read_water_logger_data(file, deployments)
      },
      error = function(e) e)
      
      if(inherits(a, "error")){
         cat("Skipping", i, " sn: ", serial_number, "\n\t", 
             "error: ", a$message)
         skkipped <- c(skipped, serial_number)
      }
      
      serial_number <- data$serial_number[1]
      
      logger_row <- which(deployments$serial == serial_number)
      logger_elevation <- deployments$elevation[logger_row]
      logger_type <- deployments$type[logger_row]
      
      # Truncate depth at 0 (no negative depths allowed)
      data$depth[data$depth < 0] <- 0
      
      # Define inundation
      data$wet <- data$depth > 0.02
      
      # Calculate height of surface (water level if wet) above sea level
      data$ht_above_sl = data$depth + logger_elevation
      
      ### Calculate metrics ###
      wet_percentile <- quantile(data$depth[data$wet], probs = c(.75))
      eighty_per_depth <- quantile(data$depth, probs = c(.8))
      ninetyfive_per_depth <- quantile(data$depth, probs = c(.95))
      
      # Tide data not used:
      # HL.NL <- HL(level = data$ht_above_sl, time = data$date_time)
      
      at_loggerelev_freq <- fld.dur(logger_elevation + .02, data$ht_above_sl)
      
      timeinund  <- dur.events(elevation = logger_elevation + .02,
                               level = data$ht_above_sl,
                               units = "10 minutes", percentile = 0.5)
      
      
      med_level <- fld.depth(level = data$ht_above_sl,
                             elevation = logger_elevation + .02,
                             percentile = 0.5)
      
      # Save values to table
      flood_table$serial_number[i] <- serial_number
      flood_table$Proportion_Time_Inundated[i] <-  at_loggerelev_freq
      flood_table$Median_Time_Inundated[i] <- timeinund
      flood_table$Median_Inundation_Level[i] <- med_level
      flood_table$Elevation[i] = logger_elevation
      flood_table$Type[i] = logger_type
      flood_table$Percentile_80_Depth[i] =  eighty_per_depth
      flood_table$Percentile_95_Depth[i] = ninetyfive_per_depth
      flood_table$Wet_75_Percentile_Depth[i] = wet_percentile
      
   }  # end loop through calibrated files
   
   
   if (length(skipped) != 0) {
      message(length(skipped), " logger(s) dropped from results. SN:\n\t",
              paste(skipped, collapse = ",\n\t", sep = ""), sep = "")
      
      flood_table <- flood_table[!flood_table$serial_number %in% skipped, , drop = FALSE]
      
   }
   
   deployments2 <- deployments |>
      mutate(serial_number  = as.character(serial)) |>
      select(serial_number,
             Easting = eastings,
             Northing = northings)
   
   
   flood_table$serial_number <- as.character(flood_table$serial_number)
   
   result <- dplyr::left_join(flood_table, deployments2, by = "serial_number" )
   
   
   return(result)
}
