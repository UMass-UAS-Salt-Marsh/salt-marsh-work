#------------------------------------------------------------------------------#
# Vegetation-height validation against field measurements — rr (Red River)
#------------------------------------------------------------------------------#
#
# Compares two candidate ground references for vegetation-height
# calculation against field-measured vegetation height at ECP
# locations (the `veg_height_m` column, populated for all EVP points
# at this site):
#
#   opt2_spring_lidar_ground — current approach (lidar/05_veg_heights.R):
#     summer canopy top minus the spring lidar DTM.
#   opt3_massgis_plus_floor  — proposed approach: summer canopy top
#     minus MassGIS ground shifted up by the estimated instrument-level
#     floor bias (lidar/07_floor_corrected_ground.R).
#
# The floor-bias estimate is fit from DTM-vs-ECP ground residuals
# (07_floor_corrected_ground.R), not from this comparison, so this
# validation is independent of the fitting data.
#
# Prerequisites:
#   1. lidar/06_compare_ground_sources.R (rr) — cached spring lidar DTM
#      sample at ECPs.
#   2. lidar/07_floor_corrected_ground.R — massgis_plus_floor_summer.tif.
#
# Run from the project root in RStudio so relative paths resolve.
#------------------------------------------------------------------------------#

library(lidR)
library(future)
library(progressr)
progressr::handlers(global = TRUE)
progressr::handlers("cli")

library(pathtools)

invisible(lapply(list.files("R/", pattern = "\\.[Rr]$",
                            full.names = TRUE), source))

set_path_scheme("lidar/data/paths.yml")

site        <- "rr"
summer_date <- "2022_08_10"   # yyyy_mm_dd; matches lidar/data/paths.yml
output_dir  <- get_path("ground_comparison_report", site = site)

# Must match whatever lidar/02.R used to produce the cleaned tiles and
# ground rasters below. 6491 = NAD83(2011) / Massachusetts Mainland
# State Plane, this project's current standard (see CRS.md).
target_epsg <- 6491L

ecp_path <- get_path("ecp_path")

spring_dtm_path <- file.path(output_dir, "lidar_spring_ecp.csv")

# Large derived rasters live alongside the other rr summer rasters in
# the E: scratch tree, not in the reports directory.
summer_raster_dir <- get_path("ground_raster_dir", site = site,
                              date = summer_date, target_epsg = target_epsg)
massgis_floor_tif <- get_path("corrected_ground_raster", site = site,
                              date = summer_date, target_epsg = target_epsg)

if (!file.exists(massgis_floor_tif)) {
   stop("Corrected ground raster not found: ", massgis_floor_tif,
        "\nRun lidar/07_floor_corrected_ground.R first.")
}

workers      <- 25
chunk_size   <- 200
chunk_buffer <- 20

summer_clean_dir <- get_path("cleaned_tiles", site = site, date = summer_date,
                             target_epsg = target_epsg)
canopy_top_tif   <- get_path("canopy_top_raster", site = site,
                             date = summer_date, target_epsg = target_epsg)

plan(multisession, workers = workers)

#------------------------------------------------------------------------------#
# Build the canopy-top raster (skip-if-exists)
#------------------------------------------------------------------------------#

if (!file.exists(canopy_top_tif)) {
   rasterize_canopy_top(
      input        = summer_clean_dir,
      output       = canopy_top_tif,
      chunk_size   = chunk_size,
      chunk_buffer = chunk_buffer
   )
} else {
   message("Canopy-top raster exists, skipping: ", canopy_top_tif)
}

#------------------------------------------------------------------------------#
# Sample all three surfaces at the same ECP set
#------------------------------------------------------------------------------#

site_ecp <- load_ecp(ecp_path, site = site)
site_ecp <- site_ecp[tolower(site_ecp$type) %in% "evp", , drop = FALSE]

spring_elev <- sample_dtm(
   dtm        = get_path("ground_raster", site = site, date = "2022_05_14",
                         target_epsg = target_epsg, csf_threshold = 0.01,
                         csf_res = 0.1, csf_rigidness = 2, raster_res = 0.25),
   ecp        = site_ecp,
   output_csv = spring_dtm_path
)
massgis_floor_elev <- sample_dtm(
   dtm        = massgis_floor_tif,
   ecp        = site_ecp,
   output_csv = file.path(output_dir, "massgis_plus_floor_summer_ecp.csv")
)
canopy_elev <- sample_dtm(
   dtm        = canopy_top_tif,
   ecp        = site_ecp,
   output_csv = file.path(output_dir, "canopy_top_summer_ecp.csv")
)

#------------------------------------------------------------------------------#
# Compare predicted vegetation height against field veg_height_m
#------------------------------------------------------------------------------#

veg_height_opt2 <- canopy_elev$predicted - spring_elev$predicted
veg_height_opt3 <- canopy_elev$predicted - massgis_floor_elev$predicted

evaluate_veg_height <- function(predicted_height, label) {
   df <- data.frame(
      elevation = canopy_elev$veg_height_m,
      predicted = predicted_height,
      subclass  = canopy_elev$subclass
   )
   df$residual     <- df$predicted - df$elevation
   df$abs_residual <- abs(df$residual)
   summ            <- summarize_residuals(df)
   summ$option     <- label
   summ
}

veg_height_validation <- rbind(
   evaluate_veg_height(veg_height_opt2, "opt2_spring_lidar_ground"),
   evaluate_veg_height(veg_height_opt3, "opt3_massgis_plus_floor")
)
veg_height_validation <- veg_height_validation[
   , c("option", setdiff(colnames(veg_height_validation), "option"))
]

write.csv(
   veg_height_validation,
   file.path(output_dir, "veg_height_validation.csv"),
   row.names = FALSE
)

message("Vegetation-height validation written: ",
        file.path(output_dir, "veg_height_validation.csv"))
print(veg_height_validation[veg_height_validation$group == "overall",
                            c("option", "n", "mean_bias", "rmse", "mae")])
