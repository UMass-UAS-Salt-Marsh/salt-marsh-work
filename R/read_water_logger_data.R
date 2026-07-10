read_water_logger_data <- function(file, deployments) {
   
   serial_number =  basename(file) |> substring(1, 8)
   
   # Lookup deployment info for this logger
   logger_row <- which(deployments$serial == serial_number)
   
   if (length(logger_row) == 0) {
      stop(serial_number, " is missing from the ",
              "deployments table")
   }
   
   if (length(logger_row) != 1) {
      stop("There should be one and only one row for each logger",
           "in the deployment file", deployment_file, " logger SN:", serial_number)
   }
   
   start <- deployments$date_time_deployed[logger_row]
   end <- deployments$date_time_pulled[logger_row]
  
   
   # Note original script started reading at line
   data <- read_excel(file, range =  cell_limits(c(2, 1), c(NA, 6)),
                      col_names = TRUE,
                      col_types = c("numeric","date","numeric",
                                    "numeric", "numeric", "numeric"))
   
   
   
   
   # Check that serial number in header matches file name SN
   logger_sn_from_header <- gsub("(^.*LGR S/N:[[:blank:]]+)([[:digit:]]+),.*$",
                                 "\\2", names(data)[3], perl = TRUE) |>
      as.numeric()
   stopifnot(serial_number == logger_sn_from_header)
   
   # Clean up column names
   n <- names(data)
   n <- gsub(",.*$", "", n)
   n <- gsub("[[:blank:]]", "_", n)
   n <- gsub("\\.", "", n)
   n <- tolower(n)
   names(data) <- n
   
   # Check for missing dates and try to recover
   # This was added for "Regression/Data/WEL/Calibrated Data/21384665_cal.xlsx"
   #  some cells in that file's data column stored the dates as text
   # Reading the column in as dates converts those cells to NA
   # Here we read the column as text and then convert the text to dates
   # Note this corrupts the proper dates so we only use this process
   # for the problem cells.
   
   if (any(is.na(data$date_time))) {
      na_rows <- which(is.na(data$date_time))
      text_dates <- read_excel(file, range =  cell_limits(c(2, 2), c(NA, 2)),
                               col_names = TRUE,
                               col_types = c("text")) |>
         as.data.frame()
      missing_dates <- text_dates[na_rows, 1] |> lubridate::mdy_hms()
      if(any(is.na(missing_dates))) {
         stop("Something strange is going on in the data column in: ",
              file, " first problem date in row ",  na_rows[1]+2)
      }
      data$date_time[na_rows] <- missing_dates
   }
   if(any(is.na(data$date_time))) stop("Missing date-time values.")
   
   
   # Rename various versions of the depth column to canonical "depth"
   alternative_depth_names <- c("sensor_depth_brackish", "sensor_depth")
   names(data)[names(data) %in% alternative_depth_names] <- "depth"
   
   required_logger_columns <- c("date_time", "depth")
   if(!all(required_logger_columns %in% names(data))) {
      stop("Expected column(s): \"",
           paste(setdiff(required_logger_columns,
                         names(data)), collapse = '", "'),
           '" missing from ', calibrated_files[i])
   }
   
   # Filter to deployed times based on deployment table
   is_deployed <- data$date_time > start & data$date_time < end
   if(sum(is_deployed) < 10)
      stop("Logger appears to have less than 10 observations in the deployment window.")
   data <- data[is_deployed, , drop = FALSE]
   data$is_deployed <- NULL # delete column
   
   data$serial <- serial_number

   return(data)
}