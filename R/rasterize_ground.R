

rasterize_ground <- function(input,  # directory with one or more .las if muliple should collectively represent a single point cloud  - note this should already be cleaned.
                             output,   # path to tif file for rasterized output
                             csf_threshold = 0.01,
                             csf_res = 0.01, 
                             csf_rigidness = 2,
                             iterations = 500,
                             raster_res = 0.25, 
                             verbose = TRUE, 
                             chunk_size = 200, 
                             chunk_buffer = 20) {
   
   
   stopifnot(grepl("\\.tif$", output))
   
 
   ctg <- readLAScatalog(input)
   # Set (square) chunk size in meters
   opt_chunk_size(ctg) <- chunk_size
   # And buffer to read around chunk when processing
   opt_chunk_buffer(ctg) <- chunk_buffer
   
   opt_output_files(ctg) <-  "" # no output files, it will return output
   
   
   
   npts <- length(ctg@data[[1]])
   if (npts == 0) stop("Input catalog contains zero points.")
   
   if (verbose) message(sprintf("Point cloud contains %s chunks (tiles)", format(npts, big.mark = ",")))
   
   
   
   rast_ground <- function(las) {
      
      ground_class <- classify_ground(las, last_returns = TRUE,
                                      algorithm = csf(class_threshold = csf_threshold,
                                                      cloth_resolution = csf_res,
                                                      rigidness = csf_rigidness))
      # plot(ground_class, color = "Classification")
      
      
      raster <- rasterize_terrain(las = ground_class, res = raster_res, pkg = "terra", alogrithm =  knnidw(k = 10, p = 2, rmax = 0.5))
      
      terra::crop(raster, terra::ext(chunk), snap = "out")
      
      return(raster)
   }
   
   
   
   dtm <- catalog_map(ctg, rast_ground)
   
   terra::writeRaster(dtm, filename = output, overwrite = TRUE)
   
}