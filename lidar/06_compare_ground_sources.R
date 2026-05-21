#------------------------------------------------------------------------------#
# Ground elevation source comparison driver
#------------------------------------------------------------------------------#
#
# Sets site-specific parameters and renders
# rmd/ground_source_comparison.Rmd, which:
#   - Samples each ground elevation source at ECP locations
#     (skip-if-exists cache in output_dir)
#   - Evaluates residuals per source and ECP class
#   - Writes ground_source_comparison.csv to output_dir
#   - Renders an HTML report with bias/RMSE plots and per-source
#     diagnostic plots
#
# Run from the project root in RStudio so relative paths resolve.
# Spring and summer lidar DTMs must exist first (run lidar/02.R for
# both dates, then lidar/03_evaluate_dtm.R to pick the best CSF set).
#------------------------------------------------------------------------------#

invisible(lapply(list.files("R/", pattern = "\\.[Rr]$",
                            full.names = TRUE), source))

#------------------------------------------------------------------------------#
# Parameters
#------------------------------------------------------------------------------#

site        <- "rr"
spring_date <- "2022_05_14"
summer_date <- "2022_08_10"

ecp_path <- paste0(
   "X:/legacy/gdrive/saltmarsh_UAS_native/",
   "In Situ Data Collection/",
   "JoshSurveyPoints_AllSites_One_Sheet.xlsx"
)

tolerances  <- c(0.10, 0.20)
ecp_types   <- "EVP"
output_dir  <- file.path("lidar/output",
                         paste0(site, "_ground_comparison"))

# Best CSF stems from Phase 1 evaluation (lowest overall RMSE).
# Spring: RMSE 0.174 m, bias +0.165 m
# Summer: RMSE 0.245 m, bias +0.221 m
best_csf_spring <- "csf_th0.01_res0.1_rgd2_0.25m.tif"
best_csf_summer <- "csf_th0.12_res0.2_rgd2_0.25m.tif"

#------------------------------------------------------------------------------#
# Render report
#------------------------------------------------------------------------------#

rmarkdown::render(
   input       = "rmd/ground_source_comparison.Rmd",
   params      = list(
      site            = site,
      spring_date     = spring_date,
      summer_date     = summer_date,
      ecp_path        = ecp_path,
      tolerances      = tolerances,
      ecp_types       = ecp_types,
      output_dir      = output_dir,
      best_csf_spring = best_csf_spring,
      best_csf_summer = best_csf_summer
   ),
   output_file = paste0("ground_comparison_", site, ".html"),
   output_dir  = output_dir,
   quiet       = TRUE
)
