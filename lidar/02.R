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


# Conditional reprojection: source clouds tagged WGS84/UTM 19N + WGS84
# ellipsoidal heights are reprojected to NAD83/UTM 19N + NAVD88 (via
# GEOID12B) using LAStools (`las2las` then `lasvdatum`).  See
# `R/reproject_las.R` for the caveat on the WGS84<->NAD83 ellipsoid
# offset.  `reproject_las()` is idempotent (skip-if-exists), so re-runs
# of this script are cheap.
if (las_needs_reprojection(paths$input)) {
   paths$reprojected_dir <- file.path(paths$base_output, "reprojected")
   dir.create(paths$reprojected_dir, recursive = TRUE,
              showWarnings = FALSE)
   reprojected_path <- file.path(
      paths$reprojected_dir,
      sub("\\.las$", "_epsg26919_navd88.las",
          basename(paths$input), ignore.case = TRUE)
   )
   paths$input <- reproject_las(paths$input, reprojected_path,
                                target_epsg = 26919)
}


# Create cleaned tiles - once per site.  Everything else will use these
clean_and_tile(paths$input, paths$cleaned_catalog_dir, chunk_size = 200, chunk_buffer = 20)


# Find ground with several parameters - output to raster
# update_path() comes from R/update_path.R.

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


# Assess errors
# clean_column_names(), clean_dates(), visualize_dtm(), and evaluate_dtm()
# all come from R/.

# Read elevation control points
ecp <- readxl::read_xlsx(paths$ecp) |> clean_column_names()
ecp$date <- clean_dates(ecp$date)
ecp$site <- tolower(ecp$site)

i <- 1

site_dtm <- models$dtm[i]
site_ecp <- ecp[ecp$site == site & !ecp$type %in% "Logger Array", ]

