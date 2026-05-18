#' Load and tidy the elevation control points spreadsheet
#'
#' Reads the project's all-sites ECP xlsx
#' (`JoshSurveyPoints_AllSites_One_Sheet.xlsx` by default),
#' cleans column names,
#' parses dates,
#' lowercases site codes,
#' filters out excluded point types,
#' and optionally narrows to a single site.
#' Returns an `sf` object with `easting` / `northing` retained as
#' columns alongside the geometry.
#'
#' **Source CRS is hardcoded** as **EPSG:26919**
#' (NAD83 / UTM 19N) —
#' the CRS the ECP coordinates were collected in for this
#' project.
#' If `target_crs` differs from the source,
#' the geometry is transformed and the `easting` / `northing`
#' columns are updated to match.
#'
#' The xlsx is expected to have at minimum the columns
#' `Easting`, `Northing`, `Elevation`, `Type`, `Site`, `Date`
#' (case-insensitive;
#' [`clean_column_names()`] normalizes to snake_case).
#' All other columns
#' (e.g. `poly_number`, `point_number`, `subclass`, `notes`,
#' `veg_height_cm`, `veg_height_m`, `outside_mask`, the
#' `out_*` flags)
#' are passed through verbatim.
#'
#' Default `exclude_types = "Logger Array"` drops the
#' water-logger array points,
#' which sit above the marsh surface and are unsuitable for DTM
#' evaluation.
#' Berm, EVP, Training, and other class types are kept since
#' all represent real ground elevation.
#'
#' @param path Path to the ECP xlsx.
#' @param target_crs Integer EPSG code for the returned sf
#'    object.
#'    Default `26919` (NAD83 / UTM 19N),
#'    matching the source CRS and the project's DTMs.
#' @param exclude_types Character vector of values in the `type`
#'    column to drop.
#'    Default `"Logger Array"`.
#'    Pass `character(0)` to keep all types.
#' @param site Optional site code to filter to
#'    (case-insensitive;
#'    compared against `tolower(ecp$site)`).
#'    If `NULL` (default), all sites are returned.
#'
#' @return An `sf` object,
#'    one row per surviving ECP,
#'    with `easting` and `northing` columns plus all other
#'    spreadsheet columns and a `POINT` geometry in `target_crs`.
#'
#' @examples
#' \dontrun{
#' rr_ecp <- load_ecp(
#'    path = paste0(
#'       "X:/legacy/gdrive/saltmarsh_UAS_native/",
#'       "In Situ Data Collection/",
#'       "JoshSurveyPoints_AllSites_One_Sheet.xlsx"
#'    ),
#'    site = "rr"
#' )
#' }
load_ecp <- function(path,
                     target_crs = 26919L,
                     exclude_types = "Logger Array",
                     site = NULL) {

   source_crs <- 26919L

   stopifnot(
      is.character(path), length(path) == 1L, file.exists(path),
      is.numeric(target_crs), length(target_crs) == 1L,
      is.character(exclude_types) || is.null(exclude_types),
      is.null(site) || (is.character(site) && length(site) == 1L)
   )

   ecp <- readxl::read_xlsx(path) |> clean_column_names()

   required <- c("easting", "northing", "elevation", "type", "site")
   missing <- setdiff(required, colnames(ecp))
   if (length(missing) > 0L) {
      stop("load_ecp(): missing required column(s) in `", path,
           "`: ", paste(missing, collapse = ", "))
   }

   if ("date" %in% colnames(ecp)) {
      ecp$date <- clean_dates(ecp$date)
   }
   ecp$site <- tolower(ecp$site)

   if (length(exclude_types) > 0L) {
      ecp <- ecp[!ecp$type %in% exclude_types, ]
   }

   if (!is.null(site)) {
      ecp <- ecp[ecp$site == tolower(site), ]
   }

   if (nrow(ecp) == 0L) {
      warning("load_ecp(): no ECPs remain after filtering ",
              "(site = ",
              if (is.null(site)) "<all>" else site,
              ", exclude_types = ",
              paste(exclude_types, collapse = ", "),
              ").")
   }

   ecp_sf <- sf::st_as_sf(
      ecp,
      coords = c("easting", "northing"),
      crs    = source_crs,
      remove = FALSE
   )

   if (as.integer(target_crs) != source_crs) {
      ecp_sf <- sf::st_transform(ecp_sf, crs = as.integer(target_crs))
      coords <- sf::st_coordinates(ecp_sf)
      ecp_sf$easting <- coords[, 1]
      ecp_sf$northing <- coords[, 2]
   }

   ecp_sf
}
