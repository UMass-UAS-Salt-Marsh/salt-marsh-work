#' Render the DTM evaluation report for one site / date
#'
#' Calls [rmarkdown::render()] on `rmd/dtm_evaluation_report.Rmd`
#' with the supplied parameters and writes an HTML report to
#' `eval_dir`.
#'
#' **Prerequisites**: `lidar/03_evaluate_dtm.R` must have been run
#' for the same site and date so that `dtm_eval_summary.csv` and
#' the per-DTM PNG plots exist in `eval_dir`.
#'
#' @param site 2- or 3-character lowercase site code
#'    (e.g. `"rr"`).
#' @param date Date string in `"yyyy_mm_dd"` format
#'    (e.g. `"2022_08_10"`).
#' @param eval_dir Directory containing `dtm_eval_summary.csv`
#'    and the per-DTM PNG plots.
#'    Defaults to `lidar/output/<site>_<date>` relative to the
#'    project root.
#' @param output_file Name of the output HTML file.
#'    Defaults to `dtm_eval_<site>_<date>.html` written into
#'    `eval_dir`.
#' @param open If `TRUE` (default), open the rendered HTML in
#'    the default browser.
#'
#' @return The path to the rendered HTML file, invisibly.
#'
#' @examples
#' \dontrun{
#' report_dtms("rr", "2022_08_10")
#' }
report_dtms <- function(site,
                        date,
                        eval_dir = file.path("lidar/output",
                                             paste0(site, "_",
                                                    date)),
                        output_file = paste0("dtm_eval_",
                                             site, "_",
                                             date, ".html"),
                        open = TRUE) {

   stopifnot(
      is.character(site), length(site) == 1L,
      is.character(date), length(date) == 1L
   )

   if (!requireNamespace("rmarkdown", quietly = TRUE)) {
      stop("report_dtms(): the `rmarkdown` package is required. ",
           "Install it with install.packages('rmarkdown').")
   }

   rmd <- file.path("rmd", "dtm_evaluation_report.Rmd")
   if (!file.exists(rmd)) {
      stop("report_dtms(): Rmd template not found: ", rmd,
           "\nRun from the project root directory.")
   }

   csv_check <- file.path(eval_dir, "dtm_eval_summary.csv")
   if (!file.exists(csv_check)) {
      stop("report_dtms(): summary CSV not found: ", csv_check,
           "\nRun lidar/03_evaluate_dtm.R first.")
   }

   out_path <- rmarkdown::render(
      input       = rmd,
      params      = list(site     = site,
                         date     = date,
                         eval_dir = eval_dir),
      output_file = output_file,
      output_dir  = eval_dir,
      quiet       = TRUE
   )

   message("Report written: ", out_path)

   if (open) {
      utils::browseURL(out_path)
   }

   invisible(out_path)
}
