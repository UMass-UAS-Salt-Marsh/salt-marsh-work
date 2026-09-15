#' Get currently available system memory
#'
#' Queries free physical memory via PowerShell's
#' `Get-CimInstance Win32_OperatingSystem`. Windows-only; `wmic` is
#' deprecated/unreliable on this project's machine, hence PowerShell.
#'
#' @return Available memory in MB, as a numeric value.
#'
#' @examples
#' \dontrun{
#' get_available_memory_mb()
#' }
get_available_memory_mb <- function() {
   out_kb <- system2(
      "powershell",
      c("-NoProfile", "-Command",
        "(Get-CimInstance Win32_OperatingSystem).FreePhysicalMemory"),
      stdout = TRUE
   )
   as.numeric(trimws(out_kb)) / 1024
}
