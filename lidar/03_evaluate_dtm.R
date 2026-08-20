#------------------------------------------------------------------------------#
# DTM evaluation driver — score each CSF parameter set against ECPs
#------------------------------------------------------------------------------#
#
# Sets site-specific parameters and calls report_dtms(), which:
#   - Sources R/ helpers
#   - Loads field-collected elevation control points (ECPs)
#   - Samples each DTM at ECP locations (skip-if-exists cache)
#   - Evaluates residuals per DTM and ECP class
#   - Writes dtm_eval_summary.csv to lidar/output/<site>_<date>/
#   - Renders an HTML report with per-DTM diagnostic plots and a
#     cross-DTM comparison coloured by vegetation height
#
# Run from the project root in RStudio so relative paths resolve.
# DTMs must already exist (run lidar/02.R first).
#------------------------------------------------------------------------------#

invisible(lapply(list.files("R/", pattern = "\\.[Rr]$",
                            full.names = TRUE), source))

#------------------------------------------------------------------------------#
# Parameters
#------------------------------------------------------------------------------#

site <- "rr"
date <- "2022_08_10"
date <- "2022_05_14"

ecp_path <- paste0(
   "X:/legacy/gdrive/saltmarsh_UAS_native/",
   "In Situ Data Collection/",
   "JoshSurveyPoints_AllSites_One_Sheet.xlsx"
)

base_output <- file.path("E:/uas_scratch/lidar", site, date)
tolerances  <- c(0.10, 0.20)

# Target horizontal CRS for the DTMs being evaluated -- must match
# whatever lidar/02.R used to produce them. 6491 = NAD83(2011) /
# Massachusetts Mainland State Plane, this project's current standard
# (see CRS.md at the project root).
target_epsg <- 6491L

#------------------------------------------------------------------------------#
# Render report
#------------------------------------------------------------------------------#

report_dtms(
   site        = site,
   date        = date,
   ecp_path    = ecp_path,
   base_output = base_output,
   tolerances  = tolerances,
   target_epsg = target_epsg
)
