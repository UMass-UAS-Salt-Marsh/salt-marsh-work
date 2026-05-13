
if(FALSE){
   
   path <- "X:\\projects\\uas\\sites\\nor\\lidar_point_cloud\\2024_10_06_low_hesaixt32\\RESEPI-A84717-2024-10-06-12-46-16\\clouds\\NOR_2024_10_06_low_lidar_pointcloud.las"
   base_output <- "X:/projects/uas/sites/nor/model_output/lidar"
   output_dir <- file.path(base_output, "2024_10_06", "zzzcleaned")
   dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
   

}
#' Cleanup las point clouds
#' 
#' Remove outliers and duplicate hits from each pulse.
#' 
#' This works in tiles to process a single .las point cloud into a
#' number of tiles each of which has had outliers and multiple points removed.
#' 
#' This is the first step in processing the LAS files for analsysis.
#' 
#' Once parameters have been finalized it may make sense to roll this into
#' a single function that does everything we want.  
#' 
#' But while evaluating multiple parameters for the simulated cloth filter
#' it is helpful to not have to rerun this step with each parameter set.
#'
#' @param input  path to a single .las file or a direcory.  If a directory 
#' all las files in the directory will be processed as one data set.
#' @param output_dir Path to the directory for output files one per tile.
#'  name will be "{ID}_{XLEFT}_{YBOTTOM}_clean.las"
#' @param verbose If `TRUE` update user with progress.  `FALSE` will remove
#' some but not all messages.
#' @param chunk_size Set the chunk size (tile dimension) in meters
#' @param chunk_buffer Sets the chunk (tile) buffer in meters
#'
#' @returns
#' @export
#'
#' @examples
clean_and_tile <- function(input, output_dir, verbose = TRUE,  chunk_size = 200, 
                           chunk_buffer = 20, skip_if_output_exists = TRUE){
   
   
   # Check for las files in output dir 
   n_output_tiles  <- list.files(output_dir, pattern = "\\.las$") |> length()
   if (n_output_tiles > 0) {
      
     if (skip_if_output_exists) {
        message("clean_and_tile() output already exists. Skipping dir: ", output_dir)
        return(readLAScatalog(output_dir))
      }
    
     stop(n_output_tiles, " cleaned tiles already exist in: ", output_dir, " Delete to rerun.")  
   }
   
   
   ctg <- readLAScatalog(input)
   # Set (square) chunk size in meters
   opt_chunk_size(ctg) <- chunk_size
   # And buffer to read around chunk when processing
   opt_chunk_buffer(ctg) <- chunk_buffer
   
   opt_output_files(ctg) <- file.path(output_dir,  "{ID}_{XLEFT}_{YBOTTOM}_clean")
   
   
   clean <- function(x) {
      x |>
         filter_last() |>
         classify_noise(algorithm = sor(k=10, m = 10)) |>
         filter_poi(Classification != LASNOISE)
   }
   
   las_clean <- catalog_map(ctg, clean)
   
   return(las_clean)
   
}