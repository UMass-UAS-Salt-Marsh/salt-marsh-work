#' Q-Q plot of DTM residuals
#'
#' Normal quantile-quantile plot of the residual distribution.
#' A straight diagonal indicates roughly normal residuals, which
#' validates using a t-test for [`evaluate_dtm()`]'s `bias_p`.
#' Curvature or heavy tails suggest using the sign test instead.
#'
#' @param points Per-point data frame from [evaluate_dtm()],
#'   with a `residual` column.
#'
#' @return A `ggplot` object.
plot_qq_residuals <- function(points) {
   ggplot2::ggplot(points, ggplot2::aes(sample = residual)) +
      ggplot2::geom_qq(alpha = 0.5) +
      ggplot2::geom_qq_line(color = "firebrick") +
      ggplot2::labs(
         x = "Theoretical quantiles",
         y = "Sample quantiles (residual, m)"
      ) +
      ggplot2::theme_minimal()
}
