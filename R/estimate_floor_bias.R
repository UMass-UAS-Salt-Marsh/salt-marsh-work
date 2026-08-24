#' Estimate the instrument-level "floor" bias from ECP residuals
#'
#' A DTM-vs-ECP residual (`predicted - elevation`) confounds two
#' additive error sources in this project's UAS-derived ground
#' rasters: a roughly constant instrument/PPK-level vertical offset,
#' and a vegetation-density-dependent CSF ground-finding error that
#' can only push the residual positive (cloth can rest on top of
#' low vegetation but not below true ground). The residual
#' distribution therefore has a "floor" — a lower bound reached by
#' points with little vegetation contamination — with a long tail
#' extending upward as vegetation density increases, plus a small
#' number of points falling *below* the floor from an unrelated,
#' larger error source (e.g. ECP or point-cloud noise).
#'
#' This function estimates that floor: it excludes the below-floor
#' outliers via a MAD threshold, then reports a low percentile of
#' the remaining residuals as the floor-bias estimate — an estimate
#' of the instrument-level component alone, uncontaminated by the
#' vegetation-dependent error that a plain mean or median residual
#' would include.
#'
#' @param points Data frame with `elevation` (ECP truth) and
#'   `predicted` (raster-sampled) columns — the same shape produced
#'   by [sample_dtm()] and consumed by [evaluate_dtm()].
#' @param k MAD multiplier for the below-floor outlier threshold.
#'   Points with `residual < median(residual) - k * mad(residual)`
#'   are excluded before the percentile is taken. Default `3`.
#' @param prob Percentile (0-1) of the outlier-filtered residuals to
#'   report as the floor bias. Default `0.10`.
#'
#' @return A list:
#' * `floor_bias` — the estimated floor, in the same units as
#'   `elevation`/`predicted` (metres for this project).
#' * `n` — number of points the percentile was computed from
#'   (after excluding outliers).
#' * `n_excluded` — number of below-floor outliers excluded.
#' * `residuals` — the full residual vector (`predicted - elevation`,
#'   one per input row), for plotting.
#' * `excluded` — logical vector, `TRUE` for rows excluded as
#'   below-floor outliers.
#'
#' @examples
#' \dontrun{
#' spring <- readr::read_csv(
#'    "lidar/output/rr_ground_comparison/lidar_spring_ecp.csv"
#' )
#' estimate_floor_bias(spring)$floor_bias
#' }
estimate_floor_bias <- function(points, k = 3, prob = 0.10) {

   required <- c("elevation", "predicted")
   missing <- setdiff(required, colnames(points))
   if (length(missing) > 0L) {
      stop("estimate_floor_bias(): missing required column(s): ",
           paste(missing, collapse = ", "))
   }

   residual <- points$predicted - points$elevation
   threshold <- stats::median(residual, na.rm = TRUE) -
      k * stats::mad(residual, na.rm = TRUE)
   excluded <- residual < threshold

   floor_bias <- stats::quantile(residual[!excluded], probs = prob,
                                 na.rm = TRUE, names = FALSE)

   list(
      floor_bias = floor_bias,
      n          = sum(!excluded & !is.na(residual)),
      n_excluded = sum(excluded, na.rm = TRUE),
      residuals  = residual,
      excluded   = excluded
   )
}
