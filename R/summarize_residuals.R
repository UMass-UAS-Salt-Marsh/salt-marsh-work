#' Per-group residual summary table
#'
#' Helper for [evaluate_dtm()].
#' Computes the full metric set for the "overall" group and for
#' each distinct ECP `type`.
#' Metrics are computed on raw residuals
#' (no offset correction applied).
#'
#' @param points Per-point data frame from [evaluate_dtm()],
#'   with `elevation`, `predicted`, `residual`, `abs_residual`,
#'   and `type` columns.
#' @param tolerances Numeric vector of absolute-difference
#'   thresholds (metres) for `pct_within_*` columns.
#'   Default `c(0.10, 0.20)`.
#'
#' @return Data frame with one row per group
#'   (`"overall"` plus each distinct `type`) and columns:
#'   `group`, `n`, `mean_bias`, `median_bias`, `mae`, `mad`,
#'   `rmse`, `max_abs`, `q25_abs`, `q75_abs`, `q95_abs`,
#'   `pct_within_*` (one column per tolerance), `slope`,
#'   `bias_p`.
summarize_residuals <- function(points,
                                tolerances = c(0.10, 0.20)) {

   group_stats <- function(df, group_label) {
      r <- df$residual
      a <- df$abs_residual
      n <- sum(!is.na(r))

      ttest_p <- tryCatch(
         stats::t.test(r, mu = 0)$p.value,
         error = function(e) NA_real_
      )
      slope <- tryCatch({
         coef(stats::lm(predicted ~ observed,
                        data = data.frame(
                           predicted = df$predicted,
                           observed  = df$elevation
                        )))[["observed"]]
      }, error = function(e) NA_real_)

      pct <- vapply(tolerances, function(tol) {
         mean(a <= tol, na.rm = TRUE) * 100
      }, numeric(1L))
      names(pct) <- paste0("pct_within_",
                           formatC(tolerances * 100, format = "fg"),
                           "cm")

      base <- data.frame(
         group        = group_label,
         n            = n,
         mean_bias    = mean(r, na.rm = TRUE),
         median_bias  = stats::median(r, na.rm = TRUE),
         mae          = mean(a, na.rm = TRUE),
         mad          = stats::mad(r, na.rm = TRUE),
         rmse         = sqrt(mean(r^2, na.rm = TRUE)),
         max_abs      = max(a, na.rm = TRUE),
         q25_abs      = stats::quantile(a, 0.25, na.rm = TRUE),
         q75_abs      = stats::quantile(a, 0.75, na.rm = TRUE),
         q95_abs      = stats::quantile(a, 0.95, na.rm = TRUE),
         slope        = slope,
         bias_p       = ttest_p,
         stringsAsFactors = FALSE
      )

      cbind(base, as.data.frame(t(pct)))
   }

   overall <- group_stats(points, "overall")

   by_type <- points |>
      split(points$type) |>
      lapply(\(df) group_stats(df, unique(df$type))) |>
      do.call(what = rbind)

   rbind(overall, by_type)
}
