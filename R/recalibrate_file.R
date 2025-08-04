#' process_calibrated_folder_water_logger_elevation
#'
#' Processes a folder of calibrated water logger CSV files, calculating water surface elevation
#' near high tide using a supplied deployment metadata file and date range.
#'
#' @param folder_path Character. Path to folder containing CSV logger files.
#' @param deployment_file Character. Path to the deployment metadata CSV file.
#' @param start_date POSIXct. Start date-time for filtering.
#' @param end_date POSIXct. End date-time for filtering.
#' @param quantile Numeric. Quantile threshold for high tide (default = 0.75).
#' @param min_depth Numeric. Minimum valid depth (meters) (default = 0.1).
#'
#' @return A data frame combining water surface elevation and logger stats for all valid files.
#' @export
recalibrate_file <- function(folder_path, deployment_file, start_date, end_date,
                                                             quantile = 0.75, min_depth = 0.1) {
   # Load deployment metadata
   deployments <- read.csv(deployment_file)
   deployments$Serial <- as.character(deployments$Serial)
   assign("deployments", deployments, envir = .GlobalEnv)  # makes deployments accessible in your function
   
   # List files
   files <- list.files(folder_path, pattern = "\\.csv$", full.names = TRUE)
   total_files <- length(files)
   
   results_list <- list()
   success_count <- 0
   
   for (i in seq_along(files)) {
      file <- files[[i]]
      message("\n[", i, "/", total_files, "] Processing: ", basename(file))
      
      result <- tryCatch({
         tide_data <- calculate_water_surface_elevation(file, common_high_tides, start_date, end_date, quantile, min_depth)
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
   
   all_results <- bind_rows(results_list)
   
   logger_stats <- all_results %>%
      group_by(logger_id) %>%
      summarize(
         mean_wse = mean(water_surface_elevation, na.rm = TRUE),
         sd_wse = sd(water_surface_elevation, na.rm = TRUE)
      )
   
   all_results <- left_join(all_results, logger_stats, by = "logger_id")
   
   
   message("✅ Successfully processed ", success_count, " of ", total_files, " files.")
   return(all_results)
}
