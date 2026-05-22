#' Render the ground elevation source comparison report
#'
#' Sources R/ helpers, samples each ground elevation dataset at ECP
#' locations (skip-if-exists cache), evaluates residuals, and renders
#' a self-contained HTML report via [rmarkdown::render()].
#'
#' @param sources Named character vector mapping a short label to the
#'    path of each raster source to compare
#'    (e.g. `c(lidar_spring = "E:/…/csf_th0.01…tif",
#'    massgis = "X:/…/be_tile.tif")`).
#' @param site 2- or 3-character lowercase site code (e.g. `"rr"`).
#' @param ecp_path Path to the all-sites ECP xlsx.
#' @param output_dir Directory for cached per-source ECP CSVs,
#'    `ground_source_comparison.csv`, and the HTML report.
#'    Defaults to `lidar/output/<site>_ground_comparison`.
#' @param tolerances Numeric vector of absolute-difference thresholds
#'    (metres) for `pct_within_*` columns. Default `c(0.10, 0.20)`.
#' @param ecp_types Character vector of ECP `type` values to include
#'    (case-insensitive). Default `"EVP"`.
#' @param output_file Name of the output HTML file.
#'    Defaults to `ground_comparison_<site>.html`.
#' @param open If `TRUE` (default), open the rendered HTML in the
#'    default browser.
#'
#' @return Path to the rendered HTML (invisibly).
#'
#' @examples
#' \dontrun{
#' report_ground_sources(
#'    sources = c(
#'       lidar_spring = paste0("E:/uas_scratch/lidar/rr/2022_05_14/",
#'                             "zzzraster/csf_th0.01_res0.1_rgd2_0.25m.tif"),
#'       massgis      = paste0("X:/scratch/bcompton/LiDAR/",
#'                             "be_19TDG412612/be_19TDG412612.tif")
#'    ),
#'    site = "rr"
#' )
#' }
report_ground_sources <- function(
      sources,
      site,
      ecp_path    = paste0(
         "X:/legacy/gdrive/saltmarsh_UAS_native/",
         "In Situ Data Collection/",
         "JoshSurveyPoints_AllSites_One_Sheet.xlsx"
      ),
      output_dir  = file.path("lidar/output",
                              paste0(site, "_ground_comparison")),
      tolerances  = c(0.10, 0.20),
      ecp_types   = "EVP",
      output_file = paste0("ground_comparison_", site, ".html"),
      open        = TRUE) {

   stopifnot(
      is.character(sources), length(sources) >= 1L,
      !is.null(names(sources)), all(nchar(names(sources)) > 0L),
      is.character(site), length(site) == 1L,
      is.character(ecp_path), length(ecp_path) == 1L
   )

   rmd <- "rmd/ground_source_comparison.Rmd"
   if (!file.exists(rmd)) {
      stop("report_ground_sources(): Rmd not found: ", rmd,
           "\nRun from the project root directory.")
   }

   dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

   out_path <- rmarkdown::render(
      input       = rmd,
      params      = list(
         site       = site,
         sources    = sources,
         ecp_path   = ecp_path,
         output_dir = output_dir,
         tolerances = tolerances,
         ecp_types  = ecp_types
      ),
      output_file = output_file,
      output_dir  = output_dir,
      quiet       = TRUE
   )

   if (open) utils::browseURL(out_path)
   invisible(out_path)
}
