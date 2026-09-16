#' Build (or skip, if present) a flight's production ground raster
#'
#' Given a site/flight already recorded in `lidar/runs.yml` (via
#' [get_run_config()]), produces *the* ground raster for that flight
#' with no grid search or human judgment left — dispatches on
#' `ground_reference$method`:
#'
#' - `"csf"` — runs [rasterize_ground()] once using that flight's
#'   recorded `best_csf`, preparing the cleaned point cloud first via
#'   [prepare_flight()] if needed.
#' - `"massgis_plus_floor"` — builds the floor-bias-corrected MassGIS
#'   raster via [build_floor_corrected_ground()], estimating the floor
#'   bias from `ground_reference$floor_bias_source_flight`'s cached
#'   ECP residuals (written by
#'   `lidar/tuning/02_ground_reference_selection.R`).
#'
#' This is the piece that only exists because of the tuning/production
#' split — see `dev/worklog.md`, 2026-09-15 — giving a way to get a
#' ground raster for a new flight of an already-tuned site without
#' either re-running the whole CSF grid or hand-copying its literals.
#'
#' Called by both `lidar/production/01_ground_raster.R` (as a thin
#' driver wrapper) and `lidar/production/02_veg_heights.R` (as a
#' prerequisite, if not already materialized).
#'
#' @param site Lidar site code (e.g. `"rr"`).
#' @param date Flight date string in `"yyyy_mm_dd"` format.
#' @param overwrite If `FALSE` (default), skip rebuilding an existing
#'    output raster. If `TRUE`, rebuild it.
#'
#' @return Path to the ground raster (invisibly).
#'
#' @examples
#' \dontrun{
#' build_ground_raster("rr", "2022_08_10")
#' }
build_ground_raster <- function(site, date, overwrite = FALSE) {

   config      <- get_run_config(site, date)
   target_epsg <- config$target_epsg
   method      <- config$ground_reference$method

   if (identical(method, "csf")) {

      best_csf <- config$flight$best_csf
      output <- ensure_parent(pathtools::get_path(
         "ground_raster", site = site, date = date, target_epsg = target_epsg,
         csf_threshold = best_csf$csf_threshold, csf_res = best_csf$csf_res,
         csf_rigidness = best_csf$csf_rigidness,
         raster_res = best_csf$raster_res
      ))

      if (file.exists(output) && !overwrite) {
         message("build_ground_raster(): output exists, skipping ",
                 "rasterization: ", output)
         source_cloud <- pathtools::get_path("raw_lidar", site = site,
                                             date = date)
         cleaned_dir <- pathtools::get_path("cleaned_tiles", site = site,
                                            date = date,
                                            target_epsg = target_epsg)
      } else {
         flight <- prepare_flight(site, date, target_epsg = target_epsg,
                                  workers = config$workers,
                                  chunk_size = config$chunk_size,
                                  chunk_buffer = config$chunk_buffer)
         args <- c(list(input = flight$cleaned_dir, output = output,
                        chunk_size = config$chunk_size,
                        chunk_buffer = config$chunk_buffer), best_csf)
         do.call(rasterize_ground, args)
         source_cloud <- flight$raw_cloud
         cleaned_dir  <- flight$cleaned_dir
      }

      write_output_metadata(
         output       = output,
         created_by   = "rasterize_ground",
         source_cloud = source_cloud,
         crs          = paste0("EPSG:", target_epsg),
         inputs       = list(cleaned_tiles = cleaned_dir),
         params       = best_csf
      )

   } else if (identical(method, "massgis_plus_floor")) {

      ground_reference <- config$ground_reference
      massgis_tile <- ground_reference$massgis_tile
      if (length(massgis_tile) != 1) {
         stop("build_ground_raster(): multi-tile massgis_tile ",
              "mosaicking is not yet implemented -- see ",
              "dev/rollout_workplan.md.")
      }
      massgis_path <- pathtools::get_path("massgis_tile", tile = massgis_tile)

      floor_flight <- ground_reference$floor_bias_source_flight
      floor_season <- config$flights[[floor_flight]]$season
      ecp_cache <- pathtools::get_path(
         "ground_comparison_ecp_cache", site = site,
         source_name = paste0("lidar_", floor_season)
      )
      if (!file.exists(ecp_cache)) {
         stop(sprintf(
            paste(
               "No cached ECP samples at %s for the floor-bias source",
               "flight ('%s'). Run",
               "lidar/tuning/02_ground_reference_selection.R for '%s'",
               "first."
            ),
            ecp_cache, floor_flight, site
         ))
      }
      residuals  <- readr::read_csv(ecp_cache, show_col_types = FALSE)
      floor_bias <- estimate_floor_bias(residuals)$floor_bias

      output <- pathtools::get_path("corrected_ground_raster", site = site,
                                    date = date, target_epsg = target_epsg)
      build_floor_corrected_ground(
         massgis_path = massgis_path,
         floor_bias   = floor_bias,
         output       = output,
         target_epsg  = target_epsg,
         overwrite    = overwrite
      )

      write_output_metadata(
         output       = output,
         created_by   = "build_ground_raster",
         source_cloud = pathtools::get_path("raw_lidar", site = site,
                                            date = date),
         crs          = paste0("EPSG:", target_epsg),
         inputs       = list(massgis_tile = massgis_path,
                             floor_bias_ecp_cache = ecp_cache),
         params       = list(floor_bias = floor_bias)
      )

   } else {
      stop("build_ground_raster(): unknown ground_reference.method '",
           method, "' for site '", site, "'.")
   }

   invisible(output)
}
