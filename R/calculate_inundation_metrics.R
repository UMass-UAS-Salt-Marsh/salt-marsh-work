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
#' `"depth"` or `"sensor_depth_brackish"`, if present, to `"sensor_depth"`.
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
#' \item{`Serial_Number`}{The logger serial number}
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
  deployments <- read_csv(deployment_file, show_col_types = FALSE)
  if("Serial #" %in% names(deployments))
    deployments <- rename(deployments, Serial = `Serial #`)
  names(deployments) <- gsub("[[:blank:]]", "_", names(deployments)) |>
    tolower()
  required_deployment_cols <- c("serial", "date_deployed", "time_deployed",
                                "date_pulled", "time_pulled", "northings",
                                "eastings", "elevation")
  if (!all(required_deployment_cols %in% names(deployments))) {
    stop("Some expected columns missing from deployment file:",
         paste(setdiff(required_deployment_cols, names(deployments)),
               collapse = ", "))
  }

  deployments <- filter(deployments, !is.na(serial), !is.na(elevation))

  # Reformat deployment date time info
  date_cols <-  c("date_deployed", "date_pulled")
  for (col in date_cols) {

    dates <- deployments[[col]]

    # eg: "21Jul2022"  see WES/15Aug2022_WES_ArrayDataSheet.csv
    if (!is.Date(dates) &&
       grepl("^[[:digit:]]+[[:alpha:]]+[[:digit:]]+$", dates[1])) {
        dates <- lubridate::dmy(dates)
    }

    # month/day/year   e.g. 9/15/2022  used in most files
    if (!is.Date(dates) &&
        grepl("^[[:digit:]]+/[[:digit:]]+/[[:digit:]]+$", dates[1])) {
      dates <- lubridate::mdy(dates)
    }

    if (!lubridate::is.Date(dates)) {
      stop("Dates not processed correctly from ", deployment_file, " : ", col)
    }
    deployments[[col]] <- dates
  }


  deployments$date_time_deployed <-
    paste(deployments$date_deployed, deployments$time_deployed, sep = " ") |>
    lubridate::ymd_hms()
  deployments$date_time_pulled <-
    paste(deployments$date_pulled,deployments$time_pulled, sep = " ") |>
    lubridate::ymd_hms()

  # Initialize empty result table
  n <- length(calibrated_files)
  flood_table = tibble('Serial_Number'=  integer(n),
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
    serialnum =  basename(file) |> substring(1, 8)

    # Lookup deployment info for this logger
    logger_row <- which(deployments$serial == serialnum)

    if (length(logger_row) == 0) {
      warning("Skipping ", serialnum, " it is either missing from the ",
              "deployments table or missing elevation data.")
      flood_table$Serial_Number[i] <- serialnum
      skipped <- c(skipped, serialnum)
      next
    }


    if (length(logger_row) != 1) {
      stop("There should be one and only one row for each logger",
           "in the deployment file", deployment_file, " logger SN:", serialnum)
    }

    start <- deployments$date_time_deployed[logger_row]
    end <- deployments$date_time_pulled[logger_row]
    logger_elevation <- deployments$elevation[logger_row]
    logger_type <- deployments$type[logger_row]

    # Note original script started reading at line
    data <- read_excel(file, range =  cell_limits(c(2, 1), c(NA, 6)),
                       col_names = TRUE,
                       col_types = c("numeric","date","numeric",
                                     "numeric", "numeric", "numeric"))




    # Check that serial number in header matches file name SN
    logger_sn_from_header <- gsub("(^.*LGR S/N:[[:blank:]]+)([[:digit:]]+),.*$",
                                  "\\2", names(data)[3], perl = TRUE) |>
      as.numeric()
    stopifnot(serialnum == logger_sn_from_header)

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


    # Rename various versions of the depth column to canonical "sensor_depth"
    alternative_depth_names <- c("sensor_depth_brackish", "depth")
    names(data)[names(data) %in% alternative_depth_names] <- "sensor_depth"

    required_logger_columns <- c("date_time", "sensor_depth")
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

    # Truncate depth at 0 (no negative depths allowed)
    data$depth <- data$sensor_depth
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
    flood_table$Serial_Number[i] <- serialnum
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

    flood_table <- flood_table[!flood_table$Serial_Number %in% skipped, , drop = FALSE]

  }

  deployments2 <- deployments |>
    mutate(Serial_Number  = as.character(serial)) |>
    select(Serial_Number,
           Easting = eastings,
           Northing = northings)


  flood_table$Serial_Number <- as.character(flood_table$Serial_Number)

  result <- dplyr::left_join(flood_table, deployments2, by = "Serial_Number" )


  return(result)
}
