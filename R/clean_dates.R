#' Coerce a mixed-format input vectors to `Date`
#'
#' For character input it handles two encoding seen in the ECP 
#' and field-data spreadsheets:
#' * Excel serial-day numerics (e.g. `"45517"` → 2024-08-07).
#'   Origin is `1899-12-30`.
#' * Day-month-year strings with a 3-letter month abbreviation
#'   (e.g. `"07Aug2024"`).
#' Character input that
#' matches neither pattern is assigned `NA` with a warning naming the
#' first offender.
#'
#' Inputs already of class `Date` pass through unchanged.  
#' Date time classes are converted to dates.

#'
#' @param dates A character, `Date`, or  Date Time vector.
#' @return A `Date` vector the same length as `dates`.
#'
#' @examples
#' clean_dates(c("45517", "07Aug2024"))
clean_dates <- function(dates) {
   
   # Handle formal date and date time classes
   if (lubridate::is.timepoint(dates)) {
      if (!lubridate::is.Date(dates)) {
         dates <- lubridate::as_date(dates)
      }
      return(dates)
   }

   result <- rep(lubridate::as_date(NA), length(dates))

   # Excel serial-day numerics.
   num <- suppressWarnings(as.numeric(dates))
   sv_num <- !is.na(num)
   result[sv_num] <- as.Date(num[sv_num], origin = "1899-12-30")

   # Day-month-year strings, e.g. 07Aug2024.
   sv_dmy <- grepl("^[[:digit:]]{1,2}[[:alpha:]]+[[:digit:]]{2,4}$", dates)
   result[sv_dmy] <- as.Date(dates[sv_dmy], format = "%d%b%Y")

   if (any(is.na(result))) {
      bad_dates <- dates[is.na(result)]
      n_bad <- length(bad_dates)
      warning(n_bad, " dates were not processed. First bad date input: ",
              bad_dates[1])
   }

   result
}
