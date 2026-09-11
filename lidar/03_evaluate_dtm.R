#------------------------------------------------------------------------------#
# DTM evaluation driver — score each CSF parameter set against ECPs
#------------------------------------------------------------------------------#
#
# Sets site-specific parameters and calls report_dtms(), which:
#   - Sources R/ helpers
#   - Loads field-collected elevation control points (ECPs)
#   - Samples each DTM at ECP locations (skip-if-exists cache)
#   - Evaluates residuals per DTM and ECP class
#   - Writes dtm_eval_summary.csv to ../lidar_reports/<site>_<date>/
#   - Renders an HTML report with per-DTM diagnostic plots and a
#     cross-DTM comparison coloured by vegetation height
#
# Run from the project root in RStudio so relative paths resolve.
# DTMs must already exist (run lidar/02.R first).
#------------------------------------------------------------------------------#

library(pathtools)

invisible(lapply(list.files("R/", pattern = "\\.[Rr]$",
                            full.names = TRUE), source))

set_path_scheme("lidar/data/paths.yml")

#------------------------------------------------------------------------------#
# Parameters
#------------------------------------------------------------------------------#
# Target horizontal CRS for the DTMs being evaluated -- must match
# whatever lidar/02.R used to produce them. 6491 = NAD83(2011) /
# Massachusetts Mainland State Plane, this project's current standard
# (see CRS.md at the project root).
target_epsg <- 6491L

runs <- data.frame(site = "rr", date = c("2022_08_10", "2022_05_14"))

tolerances <- c(0.10, 0.20)

#------------------------------------------------------------------------------#
# Render report
#------------------------------------------------------------------------------#
# ecp_path, dtm_dir, and eval_dir all default to pathtools::get_path()
# inside report_dtms() -- nothing to resolve here.

for (i in seq_len(nrow(runs))) {

   site <- runs$site[i]
   date <- runs$date[i]

   report_dtms(
      site        = site,
      date        = date,
      target_epsg = target_epsg,
      tolerances  = tolerances
   )
}
