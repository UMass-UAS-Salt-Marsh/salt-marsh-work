

rasterize_ground <- function(input,  # directory with one or more .las if muliple should collectively represent a single point cloud  - note this should already be cleaned.
                             output,   # path to tif file for rasterized output
                             csf_threshold = 0.01,
                             csf_res = 0.01, 
                             csf_rigidness = 2,
                             iterations = 500,
                             raster_res = 0.25) {
   
   
   stopifnot(grepl("\\.tif$", output))
   
   data_dir <- dirname(path)    
   n_las_files <- list.files(data_dir, pattern = "\\.las$") |> length()
   if(n_las_files != 1)
      stop("Expected one and only one .las file in the input directory.")
   
   ctg <- readLAScatalog(data_dir)
   # Set (square) chunk size in meters
   opt_chunk_size(ctg) <- 400
   # And buffer to read around chunk when processing
   opt_chunk_buffer(ctg) <- 20
   
   opt_output_files(ctg) <- file.path(output_dir,  "")  # no ouput it will return ouput
   
   
   
   npts <- length(las@data[[1]])
   if (npts == 0) stop("Input LAS contains zero points.")
   
   if (verbose) message(sprintf("Point cloud contains %s points", format(npts, big.mark = ",")))
   
   
   
   rast_ground <- function(las) {
      
      ground_class <- classify_ground(las, last_returns = TRUE,
                                      algorithm = csf(class_threshold = csf_threshold,
                                                      cloth_resolution = csf_res,
                                                      rigidness = csf_rigidness))
      # plot(ground_class, color = "Classification")
      
      
      raster <- rasterize_terrain(ground_class, res = raster_res, pkg = "terra", alogrithm =  knnidw(k = 10, p = 2, rmax = 0.5))
      
      return(raster)
   }
   
   
   
   dtm <- catalog_map(ctg, rast_ground)
   terra::writeRaster(dtm, filename = output)
   
}