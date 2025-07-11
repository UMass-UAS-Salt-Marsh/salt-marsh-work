if(FALSE) {
   
   #path to Google drive folder (Specific to computer)
   data_store <- "/Users/emily/Library/CloudStorage/GoogleDrive-ekmiller@umass.edu/.shortcut-targets-by-id/0B6-MI-dco6FLWkZmTDZ4MFhRU1k/7. SaltMUAS_share"
   
   calibrated_dir <- file.path(data_store, 
                               "In Situ Data Collection/OTH/HOBO Water Logger/2022 Water Loggers/2022 Array/Calibrated Data")
   
   deployment_file <- file.path(data_store, "In Situ Data Collection/OTH/HOBO Water Logger/2022 Water Loggers/2022 Array/OTH_LoggerArray_Pull.csv")
   
   dem <- file.path(data_store, 
                    "Data_AllSites/Original Raster/UAS Photogrammetry DEM/OTH/27Apr2021_OTH_Low_RGB_DEM.tif")
}


#' Title
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
#' @param dem This path to a TIF file that contains a DEM raster of a
#' specific site. The path to find a DEM is: 
#' `"Data_AllSites/Original Raster/UAS Photogrammetry DEM"`this leads to
#' a collection of site specific folders that contain different DEM rasters. 
#' 
#' (Some folders have a ton of DEMs in them, how do we decide which one to pick????)
#' 
#' @param out_tif Output function to write out the created predicted raster 
#' and put the raster into a folder of your choosing. `"pti"` being the predicted
#' raster and `"result_path"` being the path to the folder of where you want the
#' raster to be located.
#'  
#' @param metric Using the function `"calculate_inundation_metrics"` to calculate 
#' several different metrics summarizing inundation statistics for all the water 
#' depth loggers at a site. The script for `"calculate_inundation_metrics"` is 
#' more heavily explained in the file `"calculate_inundation_metrics.R"` which is
#' found in the salt_marsh_work repository found within the UMass UAS Salt Marsh
#' on Github (https://github.com/UMass-UAS-Salt-Marsh). The path from the link
#' is `"salt_marsh_work/R/calculate_inundation_metrics.R"`. 
#' 
#' 
#' @param data_store This path is to your Google drive folder found on your desktop.
#'
#' @return
#' @export
#'
#' @examples
map_hydrology <- function(calibrated_dir, deployment_file, dem, out_tif, metric) {
   
   data_store <- "/Users/emily/Library/CloudStorage/GoogleDrive-ekmiller@umass.edu/.shortcut-targets-by-id/0B6-MI-dco6FLWkZmTDZ4MFhRU1k/7. SaltMUAS_share"
   calibrated_dir <- file.path(data_store, 
                               "In Situ Data Collection/OTH/HOBO Water Logger/2022 Water Loggers/2022 Array/Calibrated Data")
   deployment_file <- file.path(data_store, "In Situ Data Collection/OTH/HOBO Water Logger/2022 Water Loggers/2022 Array/OTH_LoggerArray_Pull.csv")
   
   metrics <- calculate_inundation_metrics(calibrated_dir, deployment_file)
   stopifnot(metric %in% names(metrics))
   formula <- as.formula(paste0(metric, "~Elevation"))
   model <- lm(formula, metrics)
   #Your chosen DEM site to transform raster to
   dem <- terra::rast("Data_AllSites/Original Raster/UAS Photogrammetry DEM/OTH/27Apr2021_OTH_Low_RGB_DEM.tif")
   names(dem) <- "Elevation"
   #raster prediction creation
   pti <- predict(dem, model)
   out_tif_metric <- writeRaster(pti, result_path)
}



