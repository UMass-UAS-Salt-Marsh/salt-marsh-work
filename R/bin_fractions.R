#' Fraction of returns within each height bin
#'
#' Per-cell metric helper for [rasterize_veg_heights()]. Must be a
#' top-level function, not nested inside another function: `lidR`'s
#' `catalog_map()` per-chunk dispatch does not preserve a closure
#' referenced only inside a [lidR::pixel_metrics()] formula, so a
#' nested definition throws `could not find function` at chunk-
#' processing time even when running sequentially.
#'
#' @param z Numeric vector of normalized heights (metres) for one
#'   pixel.
#' @param bin_breaks Numeric vector of bin break points (metres).
#' @param bin_names Character vector of band names, length
#'   `length(bin_breaks) - 1`.
#'
#' @return A named list of length `length(bin_names)`: the fraction
#'   of `z` (out of all of `z`, including values outside the bin
#'   range) falling in each bin.
bin_fractions <- function(z, bin_breaks, bin_names) {
   n_bins <- length(bin_names)

   if (length(z) == 0L) {
      return(setNames(as.list(rep(NA_real_, n_bins)), bin_names))
   }

   ceiling_ht <- bin_breaks[n_bins + 1L]
   z_pos  <- z[z >= 0 & z < ceiling_ht]
   counts <- tabulate(
      .bincode(z_pos, bin_breaks, right = FALSE,
               include.lowest = TRUE),
      nbins = n_bins
   )
   setNames(as.list(counts / length(z)), bin_names)
}
