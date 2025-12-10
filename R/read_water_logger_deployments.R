

#' Read table of water logger deployments
#'
#' @param deployment_file path to file with water logger deployment information
#' 
#' @returns reformatted water logger deployment table 
#' @export
#'
#' @examples
read_water_logger_deployments <- function(deployment_file){
   
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
   
   final_cols <- c( required_deployment_cols[
      !grepl("date|time",  required_deployment_cols)],
                   "date_time_deployed", "date_time_pulled")
                   
   
   
   deployments <- select(deployments, all_of(final_cols))
   
   
   return(deployments)
}