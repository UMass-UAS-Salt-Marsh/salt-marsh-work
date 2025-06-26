if(FALSE) {
   #path to Google drive folder (Specific to computer)
   data_store <- "/Users/emily/Library/CloudStorage/GoogleDrive-ekmiller@umass.edu/.shortcut-targets-by-id/0B6-MI-dco6FLWkZmTDZ4MFhRU1k/7. SaltMUAS_share"
   
   calibrated_dir <- file.path(data_store, 
                               "In Situ Data Collection/OTH/HOBO Water Logger/2022 Water Loggers/2022 Array/Calibrated Data")
   
   deployment_file <- file.path(data_store, "In Situ Data Collection/OTH/HOBO Water Logger/2022 Water Loggers/2022 Array/OTH_LoggerArray_Pull.csv")
   
   dem <- file.path(data_store, 
                    "Data_AllSites/Original Raster/UAS Photogrammetry DEM/OTH/27Apr2021_OTH_Low_RGB_DEM.tif")
}


map_hydrology <- function(calibrated_dir, deployment_file, dem, out_tif, metric) {
   
   metrics <- calculate_inundation_metrics(calibrated_dir, deployment_file)
   stopifnot(metric %in% names(metrics))
   formula <- as.formula(paste0(metric, "~Elevation"))
   model <- lm(formula, metrics)
   #OTH is being used but any site and DEM can be changed
   dem <- terra::rast("Data_AllSites/Original Raster/UAS Photogrammetry DEM/OTH/27Apr2021_OTH_Low_RGB_DEM.tif")
   names(dem) <- "Elevation"
   pti <- predict(dem, model)
}



