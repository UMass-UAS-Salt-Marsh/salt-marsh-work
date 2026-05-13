#' Predicted-vs-observed scatter, colored by ECP type
#'
#' Companion plot for [evaluate_dtm()]. Adds the 1:1 line so visual
#' deviations from perfect prediction are obvious.
#'
#' @param points Per-point data frame from [evaluate_dtm()], with
#'   `elevation`, `predicted`, and `type` columns.
#'
#' @return A `ggplot` object.
plot_pred_vs_obs <- function(points) {
   ggplot2::ggplot(points,
                   ggplot2::aes(x = elevation, y = predicted, color = type)) +
      ggplot2::geom_abline(slope = 1, intercept = 0,
                           linetype = "dashed", color = "grey50") +
      ggplot2::geom_point(alpha = 0.7) +
      ggplot2::labs(x = "Observed elevation (m)",
                    y = "DTM predicted elevation (m)",
                    color = "ECP type") +
      ggplot2::coord_equal() +
      ggplot2::theme_minimal()
}
