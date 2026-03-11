
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
#' @param path to a single .las file but note it must be in a folder with 
#' no other las files as all las files in the folder will be added to the 
#' input catalog
#' @param output_dir Path to the directory for output files one per tile.
#'  name will be "{ID}_{XLEFT}_{YBOTTOM}_clean.las"
#' @param verbose If `TRUE` update user with progress.  `FALSE` will remove
#' some but not all messages.
#'
#' @returns
#' @export
#'
#' @examples
clean_and_tile <- function(path, output_dir, verbose = TRUE){
   
   data_dir <- dirname(path)    
   n_las_files <- list.files(data_dir, pattern = "\\.las$") |> length()
   if(n_las_files != 1)
      stop("Expected one and only one .las file in the input directory.")
   
   ctg <- readLAScatalog(data_dir)
   # Set (square) chunk size in meters
   opt_chunk_size(ctg) <- 400
   # And buffer to read around chunk when processing
   opt_chunk_buffer(ctg) <- 20
   
   opt_output_files(ctg) <- file.path(output_dir,  "{ID}_{XLEFT}_{YBOTTOM}_clean")
   


   npts <- length(las@data[[1]])
   if (npts == 0) stop("Input LAS contains zero points.")
   
   if (verbose) message(sprintf("Point cloud contains %s points", format(npts, big.mark = ",")))
   
   
   clean <- function(x) {
      x |>
         filter_last() |>
         classify_noise(algorithm = sor(k=10, m = 10)) |>
         filter_poi(Classification != LASNOISE)
   }
   
   las_clean <- catalog_map(ctg, clean)
   
   
}