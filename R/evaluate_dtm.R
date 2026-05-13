#' Score a DTM against elevation control points
#'
#' Bilinearly samples a digital terrain model at each ECP's
#' easting/northing, then reports per-point residuals and summary
#' statistics — overall and broken out by ECP `type`. Vegetation
#' classes are expected to be harder than bare-ground points, so the
#' per-type breakout is the primary diagnostic.
#'
#' The global offset is estimated as the mean residual
#' `mean(predicted - elevation)` over all points. It approximates the
#' systematic vertical bias of the DTM; adjusted residuals subtract
#' it before computing MAE/RMSE so we can see how much of the error
#' is bias vs. noise.
#'
#' @param dtm Either a `terra::SpatRaster` (single-band) or a path to
#'   a GeoTIFF.
#' @param ecp A data frame of elevation control points with at least
#'   `easting`, `northing`, `elevation`, and `type` columns.
#'
#' @return A list with elements:
#' * `points` — per-ECP table with columns `easting`, `northing`,
#'   `type`, `elevation`, `predicted`, `residual` (= predicted − observed),
#'   and `adj_residual` (residual minus the global offset).
#' * `summary` — one row per group (`"overall"` plus each `type`)
#'   with `n`, `bias`, `mae`, `rmse`, `cor`, `adj_bias`, `adj_mae`,
#'   `adj_rmse`. See [summarize_residuals()].
#' * `offset` — the global offset (scalar).
#' * `plots` — named list of `ggplot` objects: `pred_vs_obs`
#'   ([plot_pred_vs_obs()]), `residual_hist` ([plot_residual_hist()]),
#'   `residual_map` ([plot_residual_map()]).
#'
#' @examples
#' \dontrun{
#' result <- evaluate_dtm(dtm_path, site_ecp)
#' result$summary
#' result$plots$pred_vs_obs
#' }
evaluate_dtm <- function(dtm, ecp) {

   if (is.character(dtm) && length(dtm) == 1 && file.exists(dtm)) {
      dtm <- terra::rast(dtm)
   }
   if (!inherits(dtm, "SpatRaster") || terra::nlyr(dtm) != 1) {
      stop("Expected dtm or the file it points to, to be a single band raster.")
   }

   coords <- ecp |>
      dplyr::select(x = easting, y = northing, elevation, type) |>
      as.data.frame()

   predicted <- terra::extract(dtm, coords[, c("x", "y")],
                               method = "bilinear", ID = FALSE)[, 1]

   points <- data.frame(
      easting = coords$x,
      northing = coords$y,
      type = coords$type,
      elevation = coords$elevation,
      predicted = predicted,
      residual = predicted - coords$elevation
   )

   # Global vertical bias.
   # Equivalent to coef(lm(predicted ~ 1 + offset(elevation))).
   offset <- mean(points$residual, na.rm = TRUE)
   points$adj_residual <- points$residual - offset

   summary <- summarize_residuals(points)

   plots <- list(
      pred_vs_obs   = plot_pred_vs_obs(points),
      residual_hist = plot_residual_hist(points),
      residual_map  = plot_residual_map(points, terra::crs(dtm))
   )

   list(points = points, summary = summary, offset = offset, plots = plots)
}
