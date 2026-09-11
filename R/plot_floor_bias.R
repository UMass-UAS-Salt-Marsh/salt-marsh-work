#' Plot a residual histogram with the estimated floor bias marked
#'
#' Diagnostic companion for [estimate_floor_bias()]. Shows the full
#' residual distribution, the excluded below-floor outliers in a
#' different color, and a vertical line at the estimated floor, so
#' the estimate can be sanity-checked visually before it's used to
#' correct a ground raster.
#'
#' @param floor_result Return value of [estimate_floor_bias()].
#' @param title Plot title (e.g. `"rr summer lidar"`). Default `NULL`
#'   (no title).
#'
#' @return A `ggplot` object.
plot_floor_bias <- function(floor_result, title = NULL) {

   df <- data.frame(
      residual = floor_result$residuals,
      excluded = ifelse(floor_result$excluded,
                        "excluded (below floor)", "included")
   )

   p <- ggplot2::ggplot(df, ggplot2::aes(x = residual, fill = excluded)) +
      ggplot2::geom_histogram(bins = 30) +
      ggplot2::geom_vline(xintercept = floor_result$floor_bias,
                          color = "firebrick", linewidth = 0.8) +
      ggplot2::annotate("text", x = floor_result$floor_bias,
                        y = Inf, vjust = 1.5, hjust = -0.1,
                        color = "firebrick",
                        label = sprintf("floor = %.3f m",
                                        floor_result$floor_bias)) +
      ggplot2::labs(x = "Residual (predicted − observed, m)",
                    y = "Count", fill = NULL) +
      ggplot2::theme_minimal()

   if (!is.null(title)) {
      p <- p + ggplot2::labs(title = title)
   }

   p
}
