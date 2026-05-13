#' Normalize a data frame's column names to lower snake_case
#'
#' Replaces runs of whitespace in column names with `_` and
#' lowercases everything. Useful for taming hand-edited Excel sheets
#' before downstream joins.
#'
#' @param df A data frame (or tibble).
#'
#' @return `df` with cleaned column names.
#'
#' @examples
#' clean_column_names(data.frame(`First Name` = 1, `Last Name` = 2,
#'                               check.names = FALSE))
clean_column_names <- function(df) {
   colnames(df) <- colnames(df) |>
      gsub("[[:blank:]]+", "_", x = _) |>
      tolower()
   df
}
