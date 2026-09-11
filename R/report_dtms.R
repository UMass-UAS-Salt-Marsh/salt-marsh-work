#' Render the DTM evaluation report for one site / date
#'
#' Sources R/ helpers, samples DTMs at ECP locations (skip-if-exists
#' cache), evaluates residuals for each CSF parameter set, and
#' renders a self-contained HTML report via
#' [rmarkdown::render()].
#'
#' **Prerequisites**: DTMs must exist under `dtm_dir` (produced by
#' `lidar/02.R`).
#' Per-DTM ECP cache CSVs are written automatically on the first
#' run and reused on subsequent calls.
#'
#' @param site 2- or 3-character lowercase site code
#'    (e.g. `"rr"`).
#' @param date Date string in `"yyyy_mm_dd"` format
#'    (e.g. `"2022_08_10"`).
#' @param target_epsg Integer EPSG code the DTMs and ECPs are in.
#'    Default `6491` (NAD83(2011) / Massachusetts Mainland State
#'    Plane — see [`CRS.md`](../CRS.md)).
#'    Used to locate `dtm_dir` and to reproject the ECPs to match
#'    via `load_ecp(target_crs = )`.
#' @param ecp_path Path to the all-sites ECP xlsx.
#'    Defaults to `pathtools::get_path("ecp_path")`.
#' @param dtm_dir Directory containing the DTM GeoTIFFs to evaluate.
#'    Defaults to
#'    `pathtools::get_path("ground_raster_dir", site, date, target_epsg)`.
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
#'    Defaults to `pathtools::get_path("dtm_eval_report", site, date)`
#'    (`../lidar_reports/<site>_<date>`, outside the repo).
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
      target_epsg = 6491L,
      ecp_path    = get_path("ecp_path"),
      dtm_dir     = get_path("ground_raster_dir", site = site, date = date,
                             target_epsg = target_epsg),
      tolerances  = c(0.10, 0.20),
      ecp_types   = "EVP",
      eval_dir    = get_path("dtm_eval_report", site = site, date = date),
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
                         dtm_dir     = dtm_dir,
                         tolerances  = tolerances,
                         ecp_types   = ecp_types,
                         eval_dir    = eval_dir,
                         target_epsg = target_epsg),
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
