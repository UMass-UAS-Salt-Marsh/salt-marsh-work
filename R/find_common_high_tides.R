#' find_common_high_tides
#'
#' This function takes the 5 files of lowest elevation, fidns all the local max points, 
#' and takes median high tide time of each cluster to find common high tide times. 
#' This function also adds -2:2 nearpoints.
#' 
#'
#' @param calibrated_dir  
#' @param deployments
#' @param start_date POSIXct. Start of the date-time window to analyze.
#' @param end_date POSIXct. End of the date-time window to analyze.
#'
#' @returns common high tides and near points
#' @export
#'
#' @examples
find_common_high_tides <- function(calibrated_dir, deployments, min_depth,
                                   n_loggers=5, min_count=3) {
   
   
   # Step 1: Get the 5 lowest-elevation serials
   lowest_serials <- deployments %>%
      filter(!is.na(serial), !is.na(elevation)) %>%
      arrange(elevation) %>%
      slice(seq_len(n_loggers)) %>%
      pull(serial)

   
   
   # Step 2: list all files
   files <- list.files(calibrated_dir, pattern = "\\.xlsx$", full.names = TRUE)

   # Step 3: match by exact serial number at the start of filename
   selected_files <- files[basename(files) %in% paste0(lowest_serials, "_cal.xlsx")]

   all_peak_times <- lapply(selected_files, function(file) {
     
       peaks <- find_high_tides(file, deployments, min_depth = min_depth)
      if (length(peaks) > 0) return(peaks) else return(NULL)
   })
   
   # Note the original tz is wrong (UTC) but we are going to use it universally
   # Otherwise things get confusing
   original_tz <- lubridate::tz(all_peak_times[[1]][1])
   
   
   # Remove NULL entries
   all_peak_times <- Filter(Negate(is.null), all_peak_times)

   # Combine and filter peak times
   all_times <- as.POSIXct(unlist(all_peak_times), origin = "1970-01-01", tz = original_tz)
   
   
   # Step 4: Cluster times within tolerance
   tol <- 2400  # tolerance in seconds
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
   
   # Step 6: Count contributing loggers
   count_in_files <- sapply(clustered, function(cluster) {
      sum(sapply(all_peak_times, function(file_times) 
         any(abs(difftime(file_times, median(cluster), units="secs")) <= 900))) # within 15 min
   })
   
   # Step 7: Keep only tides that appear in >= 3 files
   official_common_high_tides <- official_high_tides[count_in_files >= 3]
   official_common_high_tides <- as.POSIXct(official_common_high_tides, origin = "1970-01-01", tz = original_tz)
   official_common_high_tides <- sort(unique(official_common_high_tides))
   
   
   
   # Compute differences in seconds between consecutive tides
   spacing <- diff(as.numeric(official_common_high_tides))
   
   # Define only a minimum acceptable gap (8 hours in seconds)
   min_gap <- 8 * 3600   # 28800 seconds
   
   # Expected semi-diurnal gap (~12h25m in seconds)
   expected_gap <- 12*3600 + 25*60  # 44700 sec
   max_gap_factor <- 1.5             # flag gaps > 1.5× expected
   max_gap <- expected_gap * max_gap_factor
   
   # Flag too-close tides (to drop) and too-large gaps (diagnostic)
   bad_close_idx <- which(spacing < min_gap)
   bad_large_idx <- which(spacing > max_gap)
   
   # Drop the second tide in each too-close pair
   tides_to_drop <- official_common_high_tides[bad_close_idx + 1]
   official_common_high_tides_clean <- setdiff(official_common_high_tides, tides_to_drop)
   
   # Too-close tides (to drop)
   if(length(bad_close_idx) > 0){
      df_close <- data.frame(
         tide = as.character(official_common_high_tides[bad_close_idx + 1]),
         reason = paste0("Too close: ", spacing[bad_close_idx], " sec")
      )
   } else {
      df_close <- data.frame(tide=character(0), reason=character(0))
   }
   
   # Too-large gaps (diagnostic only)
   if(length(bad_large_idx) > 0){
      df_large <- data.frame(
         tide = as.character(official_common_high_tides[bad_large_idx + 1]),
         reason = paste0("Large gap after previous: ", spacing[bad_large_idx], " sec")
      )
   } else {
      df_large <- data.frame(tide=character(0), reason=character(0))
   }
   
   # Combine flagged info
   flagged_info <- rbind(df_close, df_large)
   
   
   
   # --- Step 8: Add -2:2 nearpoints directly around official_common_high_tides ---
   common_high_tides <- sort(unique(do.call(c, lapply(official_common_high_tides_clean, function(ht) {
      ht + seq(-2, 2) * 600 # 600 seconds on each side
   }))))
   
   common_high_tides <- as.POSIXct(common_high_tides, origin = "1970-01-01", tz = original_tz)
  
   
   return(list(
      common_high_tides = common_high_tides,
      selected_files = selected_files,
      all_peak_times = all_peak_times,
      flagged_info = flagged_info,
      official_common_high_tides_clean = official_common_high_tides_clean
   ))

}