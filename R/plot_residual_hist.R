#' Histogram of residuals, faceted by ECP type
#'
#' Companion plot for [evaluate_dtm()]. Uses raw residuals
#' (predicted − observed) so the overall vertical bias is visible as
#' a horizontal shift from zero.
#'
#' @param points Per-point data frame from [evaluate_dtm()], with
#'   `residual` and `type` columns.
#'
#' @return A `ggplot` object.
plot_residual_hist <- function(points) {
   ggplot2::ggplot(points, ggplot2::aes(x = residual, fill = type)) +
      ggplot2::geom_histogram(bins = 30, show.legend = FALSE) +
      ggplot2::geom_vline(xintercept = 0, color = "grey30") +
      ggplot2::facet_wrap(~ type, scales = "free_y") +
      ggplot2::labs(x = "Residual (predicted − observed, m)",
                    y = "Count") +
      ggplot2::theme_minimal()
}
