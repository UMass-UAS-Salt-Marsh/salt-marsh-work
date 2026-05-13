#' Per-group residual summary table
#'
#' Helper for [evaluate_dtm()]. Computes n, bias, MAE, RMSE, and
#' Pearson correlation overall and within each ECP `type`. The
#' `adj_*` columns use `adj_residual` (residual with the global
#' offset removed), so `adj_bias` for the `"overall"` row is 0 by
#' construction but can be non-zero within a single type.
#'
#' @param points Per-point data frame with `elevation`, `predicted`,
#'   `residual`, `adj_residual`, and `type` columns. Produced by
#'   [evaluate_dtm()].
#'
#' @return Data frame with one row per group (`"overall"` plus each
#'   distinct `type`) and columns `group`, `n`, `bias`, `mae`,
#'   `rmse`, `cor`, `adj_bias`, `adj_mae`, `adj_rmse`.
summarize_residuals <- function(points) {
   group_stats <- function(df, group_label) {
      data.frame(
         group    = group_label,
         n        = sum(!is.na(df$residual)),
         bias     = mean(df$residual, na.rm = TRUE),
         mae      = mean(abs(df$residual), na.rm = TRUE),
         rmse     = sqrt(mean(df$residual^2, na.rm = TRUE)),
         cor      = suppressWarnings(stats::cor(df$predicted, df$elevation,
                                                use = "complete.obs")),
         adj_bias = mean(df$adj_residual, na.rm = TRUE),
         adj_mae  = mean(abs(df$adj_residual), na.rm = TRUE),
         adj_rmse = sqrt(mean(df$adj_residual^2, na.rm = TRUE))
      )
   }

   overall <- group_stats(points, "overall")

   by_type <- points |>
      split(points$type) |>
      lapply(\(df) group_stats(df, unique(df$type))) |>
      do.call(what = rbind)

   rbind(overall, by_type)
}
