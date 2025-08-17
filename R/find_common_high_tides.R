#' find_common_high_tides
#'
#' @param folder_path 
#' @param deployment_file 
#' @param start_date POSIXct. Start of the date-time window to analyze.
#' @param end_date POSIXct. End of the date-time window to analyze.
#'
#' @returns common high tides and near points
#' @export
#'
#' @examples
find_common_high_tides <- function(folder_path, deployment_file, start_date, end_date, quantile = 0, min_depth = 0.1) {
   
   # Load deployment metadata
   deployments <- read.csv(deployment_file)
   deployments$Serial <- as.character(deployments$Serial)
   assign("deployments", deployments, envir = .GlobalEnv)  # makes deployments accessible in your function
   
   # Step 1: Get the 5 lowest-elevation serials
   lowest_serials <- deployments %>%
      filter(!is.na(Serial), !is.na(Elevation)) %>%
      arrange(Elevation) %>%
      slice(1:5) %>%
      pull(Serial)

   # Step 2: list all files
   files <- list.files(folder_path, pattern = "\\.csv$", full.names = TRUE)

   # Step 3: match by exact serial number at the start of filename
   selected_files <- files[basename(files) %in% paste0(lowest_serials, "_cal.csv")]

   all_peak_times <- lapply(selected_files, function(file) {
     
       peaks <- find_high_tides(file, start_date, end_date, quantile = 0, min_depth = 0.1)
      if (length(peaks) > 0) return(peaks) else return(NULL)
   })

   # Remove NULL entries
   all_peak_times <- Filter(Negate(is.null), all_peak_times)

   # Combine and filter peak times
   all_times <- as.POSIXct(unlist(all_peak_times), origin = "1970-01-01", tz = "America/New_York")
   
   
   
   # Step 4: Cluster times within tolerance
   tol <- 7200  # tolerance in seconds
   clustered <- list()
   used <- rep(FALSE, length(all_times))
   
   for (i in seq_along(all_times)) {
      if (!used[i]) {
         close_times <- which(abs(difftime(all_times[i], all_times, units="secs")) <= tol)
         clustered[[length(clustered)+1]] <- all_times[close_times]
         used[close_times] <- TRUE
      }
   }
   
   # Step 5: Take median of each cluster as official high tide
   official_high_tides <- sapply(clustered, median)
   
   # Step 6: Count how many files contributed to each cluster
   count_in_files <- sapply(clustered, function(cluster) {
      sum(sapply(all_peak_times, function(file_times) any(abs(difftime(file_times, median(cluster), units="secs")) <= tol)))
   })
   
   # Step 7: Keep only tides that appear in >= 3 files
   official_common_high_tides <- official_high_tides[count_in_files >= 3]
   official_common_high_tides <- as.POSIXct(official_common_high_tides, origin = "1970-01-01", tz = "America/New_York")
   official_common_high_tides <- sort(unique(official_common_high_tides))
   
   
   # --- Step 8: Add -2:2 nearpoints directly around official_common_high_tides ---
   common_high_tides <- sort(unique(do.call(c, lapply(official_common_high_tides, function(ht) {
      ht + seq(-2, 2) * 600
   }))))
   common_high_tides <- as.POSIXct(common_high_tides, origin = "1970-01-01", tz = "America/New_York")
   
   
   return(list(
      common_high_tides = common_high_tides,
      selected_files = selected_files,
      all_peak_times = all_peak_times
   ))

}