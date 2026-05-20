#' Evaluate a DTM against elevation control points
#'
#' Pure reporting function: takes the assembled elevations frame
#' from [`sample_dtm()`], computes residuals and a full metric
#' set, and returns summary statistics plus diagnostic plots.
#' Does *not* perform any raster I/O — that step lives in
#' [`sample_dtm()`].
#'
#' Residuals are computed on raw predicted values
#' (no bias-correction applied).
#' The global offset (mean residual) is returned as a diagnostic
#' showing what correction *would* be needed,
#' but it is not subtracted from any metric.
#' `bias_p` in the summary formally tests whether the mean bias
#' is statistically distinguishable from zero
#' (t-test on residuals vs 0).
#'
#' @param elevations Data frame from [`sample_dtm()`]
#'   (or an rbind of several with a `dtm` identifier column).
#'   Must contain `easting`, `northing`, `elevation`,
#'   `predicted`, and `type` columns;
#'   all other columns are passed through to `points`.
#' @param tolerances Numeric vector of absolute-difference
#'   thresholds (metres) for `pct_within_*` columns.
#'   Default `c(0.10, 0.20)`.
#' @param crs Integer EPSG code used to build the sf object for
#'   `residual_map`.
#'   Default `26919` (NAD83 / UTM 19N, the project standard).
#'
#' @return A named list:
#' * `points` — per-point data frame: all `elevations` columns
#'   plus `residual` (= `predicted − elevation`) and
#'   `abs_residual`.
#' * `summary` — one row per class (`"overall"` plus each
#'   `type`).
#'   See [summarize_residuals()] for the column list.
#' * `offset` — scalar mean residual (global offset diagnostic;
#'   not applied to any metric).
#' * `plots` — named list of `ggplot` objects:
#'   `pred_vs_obs`, `residual_hist`, `residual_map`,
#'   `residual_vs_elevation`, `qq_residuals`.
#'
#' @examples
#' \dontrun{
#' elevations <- sample_dtm(dtm_path, site_ecp)
#' result     <- evaluate_dtm(elevations)
#' result$summary
#' result$plots$pred_vs_obs
#' }
evaluate_dtm <- function(elevations,
                         tolerances = c(0.10, 0.20),
                         crs = 26919L) {

   required <- c("easting", "northing", "elevation",
                 "predicted", "type", "subclass")
   missing <- setdiff(required, colnames(elevations))
   if (length(missing) > 0L) {
      stop("evaluate_dtm(): missing required column(s): ",
           paste(missing, collapse = ", "))
   }

   points <- as.data.frame(elevations)
   points$residual     <- points$predicted - points$elevation
   points$abs_residual <- abs(points$residual)

   summary <- summarize_residuals(points, tolerances)

   # Global offset: mean residual (diagnostic only, not applied).
   offset <- mean(points$residual, na.rm = TRUE)

   plots <- list(
      pred_vs_obs           = plot_pred_vs_obs(points),
      residual_hist         = plot_residual_hist(points),
      residual_map          = plot_residual_map(points,
                                                crs = crs),
      residual_vs_elevation = plot_residual_vs_elevation(points),
      qq_residuals          = plot_qq_residuals(points)
   )

   list(points = points, summary = summary,
        offset = offset, plots = plots)
}
