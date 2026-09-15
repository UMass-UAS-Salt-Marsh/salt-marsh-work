#------------------------------------------------------------------------------#
# Vegetation-height validation against field measurements — optional
#------------------------------------------------------------------------------#
#
# Was `lidar/08_veg_height_validation.R`; demoted from a required gate
# to an optional, on-demand diagnostic as part of the tuning/production
# driver split — see `dev/worklog.md`, 2026-09-15. Nothing else reads
# its output automatically. Run it by hand for extra confidence in a
# site's `ground_reference` pick, especially once that site's ECPs
# have `veg_height_m` populated.
#
# Compares two candidate ground references for vegetation-height
# calculation against field-measured vegetation height at ECP
# locations (the `veg_height_m` column, populated for all EVP points
# at `rr`):
#
#   opt2_spring_lidar_ground — legacy approach: summer canopy top
#     minus the spring lidar CSF DTM.
#   opt3_massgis_plus_floor  — current default
#     (`lidar/production/01_ground_raster.R`): summer canopy top minus
#     MassGIS ground shifted up by the estimated instrument-level
#     floor bias.
#
# The floor-bias estimate is fit from DTM-vs-ECP ground residuals
# (`lidar/tuning/02_ground_reference_selection.R`), not from this
# comparison, so this validation is independent of the fitting data.
#
# Prerequisites:
#   1. lidar/tuning/02_ground_reference_selection.R (rr) — cached
#      spring lidar DTM sample at ECPs, plus the massgis_plus_floor
#      raster if that's the recorded ground_reference.
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

set_path_scheme("lidar/paths.yml")

site        <- "rr"
spring_date <- "2022_05_14"
summer_date <- "2022_08_10"

config      <- get_run_config(site)
target_epsg <- config$target_epsg
best_csf_spring <- config$flights[[spring_date]]$best_csf

ecp_path <- get_path("ecp_path")

spring_dtm_path <- get_path("ground_comparison_ecp_cache", site = site,
                            source_name = "lidar_spring")

# Large derived rasters live alongside the other rr summer rasters in
# the E: scratch tree, not in the reports directory.
massgis_floor_tif <- get_path("corrected_ground_raster", site = site,
                              date = summer_date, target_epsg = target_epsg)

if (!file.exists(massgis_floor_tif)) {
   stop("Corrected ground raster not found: ", massgis_floor_tif,
        "\nRun lidar/tuning/02_ground_reference_selection.R first.")
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

# top_percentile / raster_res are recorded explicitly even though the
# call above uses rasterize_canopy_top()'s defaults for both --
# canopy_top_raster's path template embeds no parameters, so this
# sidecar is the only place they're ever recorded.
write_output_metadata(
   output       = canopy_top_tif,
   created_by   = "rasterize_canopy_top",
   source_cloud = get_path("raw_lidar", site = site, date = summer_date),
   crs          = paste0("EPSG:", target_epsg),
   inputs       = list(cleaned_tiles = summer_clean_dir),
   params       = list(top_percentile = 1, raster_res = 0.25)
)

#------------------------------------------------------------------------------#
# Sample all three surfaces at the same ECP set
#------------------------------------------------------------------------------#

site_ecp <- load_ecp(ecp_path, site = site)
site_ecp <- site_ecp[tolower(site_ecp$type) %in% "evp", , drop = FALSE]

spring_elev <- sample_dtm(
   dtm = get_path("ground_raster", c(
      list(site = site, date = spring_date, target_epsg = target_epsg),
      best_csf_spring
   )),
   ecp        = site_ecp,
   output_csv = spring_dtm_path
)
massgis_floor_elev <- sample_dtm(
   dtm        = massgis_floor_tif,
   ecp        = site_ecp,
   output_csv = get_path("corrected_ground_raster_ecp_cache", site = site,
                         date = summer_date, target_epsg = target_epsg)
)
canopy_elev <- sample_dtm(
   dtm        = canopy_top_tif,
   ecp        = site_ecp,
   output_csv = get_path("canopy_top_raster_ecp_cache", site = site,
                         date = summer_date, target_epsg = target_epsg)
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

veg_height_validation_csv <- get_path("veg_height_validation_csv", site = site)

write.csv(
   veg_height_validation,
   veg_height_validation_csv,
   row.names = FALSE
)

message("Vegetation-height validation written: ", veg_height_validation_csv)
print(veg_height_validation[veg_height_validation$group == "overall",
                            c("option", "n", "mean_bias", "rmse", "mae")])
