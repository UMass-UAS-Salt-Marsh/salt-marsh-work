#' Residuals vs observed elevation scatter plot
#'
#' Plots predicted-minus-observed residuals against observed
#' elevation, colored by ECP type.
#' A horizontal reference line at zero marks the ideal
#' (no-bias) case.
#' Systematic slope or curves hint at elevation-dependent DTM
#' bias (e.g., the DTM is smoother in low-lying areas than
#' high).
#'
#' @param points Per-point data frame from [evaluate_dtm()],
#'   with `elevation`, `residual`, and `type` columns.
#'
#' @return A `ggplot` object.
plot_residual_vs_elevation <- function(points) {
   ggplot2::ggplot(
      points,
      ggplot2::aes(x = elevation, y = residual, color = type)
   ) +
      ggplot2::geom_hline(yintercept = 0, linetype = "dashed",
                          color = "grey50") +
      ggplot2::geom_point(alpha = 0.6, size = 2) +
      ggplot2::labs(
         x = "Observed elevation (m NAVD 88)",
         y = "Residual (predicted − observed, m)",
         color = "ECP type"
      ) +
      ggplot2::theme_minimal()
}
