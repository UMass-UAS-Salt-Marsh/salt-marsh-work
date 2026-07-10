#' Render the DTM evaluation report for one site / date
#'
#' Sources R/ helpers, samples DTMs at ECP locations (skip-if-exists
#' cache), evaluates residuals for each CSF parameter set, and
#' renders a self-contained HTML report via
#' [rmarkdown::render()].
#'
#' **Prerequisites**: DTMs must exist under
#' `<base_output>/zzzraster/` (produced by `lidar/02.R`).
#' Per-DTM ECP cache CSVs are written automatically on the first
#' run and reused on subsequent calls.
#'
#' @param site 2- or 3-character lowercase site code
#'    (e.g. `"rr"`).
#' @param date Date string in `"yyyy_mm_dd"` format
#'    (e.g. `"2022_08_10"`).
#' @param ecp_path Path to the all-sites ECP xlsx.
#' @param base_output Directory containing `zzzraster/` with the
#'    DTM GeoTIFFs.
#'    Defaults to `E:/uas_scratch/lidar/<site>/<date>`.
#' @param tolerances Numeric vector of absolute-difference
#'    thresholds (metres) for `pct_within_*` columns.
#'    Default `c(0.10, 0.20)`.
#' @param ecp_types Character vector of ECP `type` values to
#'    include (case-insensitive).
#'    Default `"Training"` (the survey-grade ground control
#'    points).
#'    Pass `character(0)` to include all non-excluded types.
#' @param eval_dir Directory for report outputs
#'    (`dtm_eval_summary.csv`, HTML).
#'    Defaults to `lidar/output/<site>_<date>` relative to the
#'    project root.
#' @param output_file Name of the output HTML file.
#'    Defaults to `dtm_eval_<site>_<date>.html`.
#' @param open If `TRUE` (default), open the rendered HTML in
#'    the default browser.
#'
#' @return The path to the rendered HTML file, invisibly.
#'
#' @examples
#' \dontrun{
#' report_dtms("rr", "2022_08_10")
#' }
report_dtms <- function(
      site,
      date,
      ecp_path    = paste0(
         "X:/legacy/gdrive/saltmarsh_UAS_native/",
         "In Situ Data Collection/",
         "JoshSurveyPoints_AllSites_One_Sheet.xlsx"
      ),
      base_output = file.path("E:/uas_scratch/lidar", site, date),
      tolerances  = c(0.10, 0.20),
      ecp_types   = "EVP",
      eval_dir    = file.path("lidar/output",
                              paste0(site, "_", date)),
      output_file = paste0("dtm_eval_", site, "_", date, ".html"),
      open        = TRUE) {

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

   out_path <- rmarkdown::render(
      input       = rmd,
      params      = list(site        = site,
                         date        = date,
                         ecp_path    = ecp_path,
                         base_output = base_output,
                         tolerances  = tolerances,
                         ecp_types   = ecp_types,
                         eval_dir    = eval_dir),
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
