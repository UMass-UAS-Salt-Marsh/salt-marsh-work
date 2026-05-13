# Run on the full point cloud for a single site

library(lidR)
library(future)

# Note this is memory intensive. Processor wise we could go much higher than 
# 8 but  memory not.  This implies to me that we might want to use smaller 
# tiles.  Each tile would require less memory and we could use more processors

# chunk size (tile dim) and buffer are currenty set within 
# tile and clean: 
#   opt_chunk_size(ctg) <- 400
#   opt_chunk_buffer(ctg) <- 20
# And for NOR that result in 9 chunks.  Maybe try 200 and 20.
#  


plan(multisession, workers = 20)

site <- "rr" # "nor"


# Source functions
a <- lapply(list.files("R/", pattern = "\\.[Rr]$", full.names = TRUE), source)


# Read table of available input files
input_file_paths <- readr::read_csv("lidar/data/paths.csv")
input_file_paths$file_size <- file.size(input_file_paths$path)
input_file_paths$path <- gsub("\\\\", "/", input_file_paths$path)
input_file_paths$preferred <- as.logical(input_file_paths$preferred)

# Filter to prefferred files
input_file_paths <- input_file_paths[input_file_paths$preferred, ]

# Cleanup leading and trailing junk
input_file_paths$path <- gsub("^[[:blank:]/\"\\\\]+|[[:blank:]/\"\\\\]+$", "", input_file_paths$path)


# Set paths for this analysis
paths <- list()



# Select input file
possible_input_rows <- (input_file_paths$site == site & input_file_paths$type == "cloud")|> which() 
input_row <- possible_input_rows[1] ## could loop to process each image at each site

paths$input <- input_file_paths$path[input_row]

date <- input_file_paths$date[input_row] # Extract associated date
date <- gsub("-", "_", date, fixed = TRUE)

# Validate date and site 
if(! grepl("^[[:digit:]]{4}_[[:digit:]]{2}_[[:digit:]]{2}$", date))
   stop("Expected yyyy_mm_dd date format for output paths")
if(! site == tolower(site) && nchar(site) <= 3 && nchar(site) >= 2) {
   stop("Expected 2 or 2 character lower case site")
}


# output
paths$base_output <- file.path("E:/uas_scratch/lidar", site, date) # for all output datasets related to this site + date

paths$old_base_output <- file.path("X:/projects/uas/sites", site, "model_output/lidar", date) # for all output datasets related to this site + date
paths$cleaned_catalog_dir <- file.path(paths$base_output, "zzzcleaned")
paths$ground_raster_template <- file.path(paths$base_output, "zzzraster/csf_th[csf_threshold]_res[csf_res]_rgd[csf_rigidness]_[raster_res]m.tif")



paths$ecp  <-  input_file_paths$path[input_file_paths$site == "all" & input_file_paths$type == "ecp"][1]


# Create output dirs
dir.create(dirname(paths$ground_raster_template), recursive = TRUE, showWarnings = FALSE)
dir.create(paths$cleaned_catalog_dir, recursive = TRUE, showWarnings = FALSE)


# Set Cloth Simulation Filter and rasterization parameters to try
csf_par <- data.frame(csf_res =  c(0.05, 0.1, 0.1, 0.20), 
                      csf_threshold = c(.005, 0.01, 0.06, 0.12),
                      csf_rigidness = 2, 
                      raster_res = 0.25) # raster res



#-------------------------------------------------------------------------------#
# RUN
#-------------------------------------------------------------------------------#


# Create cleaned tiles - once per site.  Everything else will use these
clean_and_tile(paths$input, paths$cleaned_catalog_dir, chunk_size = 200, chunk_buffer = 20)


# Find ground with several parameters - output to raster
update_path <- function(path, args){
   # Given a named list of arguments substitute each argument name within
   # square brackets in the path with the argument value
   # eg path = ".[test]."  with args = list(test = "this_test")  will result in ".this_test."
   n <- names(args)
   for(i in seq_along(n)) {
      path <- gsub(paste0("[", n[i], "]"), args[[i]], path, fixed = TRUE )
   }
   path
}
stopifnot(update_path(path = "adad[test]", list(test = "bop")) == "adadbop")


# Make a raster for each parameter set
models <- data.frame()
for(i in seq_len(nrow(csf_par))){
   models[i, ] <- NA
   models$site[i] <- site
   models$date[i] <- date
   
   params <- as.list(csf_par[i, , drop = FALSE]) # csf parameters
   
   for (n in names(params)) {
      models[[n]][i] <- params[[n]]
   }
   
   
   output_raster <- update_path(paths$ground_raster_template, params)
   
   models$dtm <- output_raster
   
   # Full argument list for rasterize_ground
   args <- c(list(input = paths$cleaned_catalog_dir,
                  output = output_raster), 
             params)
   
   if(!file.exists(output_raster)){
      message("Starting ground rasterization ", lubridate::now())
      ground <- do.call(rasterize_ground, args)
      message("Done ground rasterization ", lubridate::now())
      
   } else {
      message("Skipping DTM. ", output_raster, " already exists. ")
   }
}



models <- data.frame()
for(i in seq_len(nrow(csf_par))){
   models[i, ] <- NA
   models$site[i] <- site
   models$date[i] <- date
   
   params <- as.list(csf_par[i, , drop = FALSE]) # csf parameters
   
   for (n in names(params)) {
      models[[n]][i] <- params[[n]]
   }
   
   
   output_raster <- update_path(paths$ground_raster_template, params)
   
   models$dtm[i] <- output_raster
}


clean_column_names <- function(df) {
   colnames(df) <- colnames(df) |>
      gsub("[[:blank:]]+", "_", x = _) |> 
      tolower()
   df
}

clean_dates <- function(dates) {
   if (lubridate::is.Date(dates))
      return(dates)
   
   result <- rep(lubridate::as_date((NA)), length(dates))
   
   # Excel decimal dates
   num <- suppressWarnings(as.numeric(dates))
   sv_num <- !is.na(num)
   result[sv_num] <-  as.Date(num[sv_num], origin = "1899-12-30")
   
   # e.g. 07Aug2024
   sv_dmy <- grepl("^[[:digit:]]{1,2}[[:alpha:]]+[[:digit:]]{2,4}$", dates )
   result[sv_dmy] <- as.Date(dates[sv_dmy], format = "%d%b%Y")
   
   # Warn if some dates not processed
   if (any(is.na(result))) {
      bad_dates <- dates[is.na(result)]
      n_bad <- length(bad_dates)
      warning(n_bad, " dates were not processed. First bad date input: ", bad_dates[1])
   }
   
   return(result)
}


# Assess errors 

# Read elevation control points
ecp <- readxl::read_xlsx(paths$ecp) |> clean_column_names()
ecp$date <- clean_dates(ecp$date)
ecp$site <- tolower(ecp$site)

# ecp <- dplyr::select(ecp, northing, easting, elevation, date, site)

i <- 1


site_dtm <- models$dtm[i]
site_ecp <- ecp[ecp$site == site & ! ecp$type %in% "Logger Array" , ]



visualize_dtm <- function(dtm, ecp) {
   # Visualize GCP with leaflet
   
   if(is.character(dtm) && length(dtm) == 1 && file.exists(dtm)) {
      dtm <- terra::rast(dtm)
   }
   if(!inherits(dtm, "SpatRaster")  || terra::nlyr(dtm) != 1) {
      stop("Expected dtm or the file it points to, to be a single band raster.")
   }
   
   coords <- dplyr::select(ecp, x = easting, y = northing, elevation, date) |> as.data.frame() 
   
   
   coords_sf <- sf::st_as_sf(coords, coords = c("x", "y"), crs = terra::crs(dtm))
   
   # 1. Transform sf object to EPSG:4326 (WGS84) if it isn't already
   wgs84_pts <- sf::st_transform(coords_sf, 4326)
   
   # 2. Create the map
   leaflet::leaflet(data = wgs84_pts) |>
      leaflet::addTiles() |>  # Add default OpenStreetMap tiles
      leaflet::addMarkers(popup = ~elevation) 
   
   
   
}



evaluate_dtm <- function(dtm, ecp) {
   
   if(is.character(dtm) && length(dtm) == 1 && file.exists(dtm)) {
      dtm <- terra::rast(dtm)
   }
   if(!inherits(dtm, "SpatRaster")  && terra::nlyr(dtm) == 1) {
      stop("Expected dtm or the file it points to, to be a single band raster.")
   }
   
   coords <- dplyr::select(ecp, x = easting, y = northing, elevation, date) |> as.data.frame() 
   
   
   
   values <- terra::extract(dtm, coords[ , c("x", "y")], method = "bilinear", ID = FALSE) 
   values <- values[, 1]
   
   cor(values, ecp$elevation, use = "complete")
 
   d <- cbind(ecp, data.frame(predicted = values))
   p <- ggplot(d, aes(y = predicted, x = elevation, color = type)) + geom_point()
   p
   
   offset_model <- lm(predicted ~ 1 + offset(elevation), data = d)
   
   offset <- coef(offset_model)[1] |> as.numeric()
   d$adjusted_pred <- d$predicted - offset
   d$diff <- d$adjusted_pred - d$elevation
   
   plot(dtm)
   
   
   result <- list()
   
   

   
   
   
}









