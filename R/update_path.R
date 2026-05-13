#' Substitute bracketed placeholders in a path with named values
#'
#' Given a path containing `[name]` placeholders and a named list of
#' values, replace each placeholder with the corresponding value.
#'
#' Used by the lidar pipeline to build per-parameter output filenames
#' from a single template, e.g.
#' `"…/csf_th[csf_threshold]_res[csf_res]_rgd[csf_rigidness].tif"`.
#'
#' @param path A character string containing `[name]` placeholders.
#' @param args A named list whose names match the placeholders.
#'   Each value is coerced to character via `paste0()`.
#'
#' @return The input `path` with every `[name]` replaced by `args[[name]]`.
#'   Placeholders not present in `args` are left untouched.
#'
#' @examples
#' update_path("dir/csf_th[th]_res[res].tif", list(th = 0.01, res = 0.1))
#' # "dir/csf_th0.01_res0.1.tif"
update_path <- function(path, args) {
   n <- names(args)
   for (i in seq_along(n)) {
      path <- gsub(paste0("[", n[i], "]"), args[[i]], path, fixed = TRUE)
   }
   path
}
