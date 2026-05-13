#' Spatial map of residuals at each ECP location
#'
#' Companion plot for [evaluate_dtm()]. Diverging-color scale
#' centered on zero so positive and negative bias are visually
#' distinct; symbol shape encodes ECP type.
#'
#' @param points Per-point data frame from [evaluate_dtm()], with
#'   `easting`, `northing`, `residual`, and `type` columns.
#' @param crs Coordinate reference system to attach to the points —
#'   typically `terra::crs(dtm)`.
#'
#' @return A `ggplot` object.
plot_residual_map <- function(points, crs) {
   sf_pts <- sf::st_as_sf(points,
                          coords = c("easting", "northing"),
                          crs = crs)

   lim <- max(abs(points$residual), na.rm = TRUE)

   ggplot2::ggplot(sf_pts) +
      ggplot2::geom_sf(ggplot2::aes(color = residual, shape = type),
                       size = 2) +
      ggplot2::scale_color_gradient2(low = "blue", mid = "white",
                                     high = "red", midpoint = 0,
                                     limits = c(-lim, lim)) +
      ggplot2::labs(color = "Residual (m)", shape = "ECP type") +
      ggplot2::theme_minimal()
}
