#------------------------------------------------------------------------------#
# Ground elevation source comparison — score all available DEMs against ECPs
#------------------------------------------------------------------------------#
#
# Evaluates multiple ground elevation datasets for a site against field-
# collected elevation control points (ECPs) using the same sample_dtm() /
# evaluate_dtm() stack as the CSF parameter evaluation.
#
# Sources compared (rr):
#   lidar_spring      — spring lidar DTM (best CSF set from Phase 1 eval)
#   lidar_summer      — summer lidar DTM (best CSF set from Phase 1 eval)
#   photo_spring_hesai — spring photogrammetry DEM (HesaiRGB sensor)
#   photo_spring_mica  — spring photogrammetry DEM (Mica sensor)
#   photo_summer       — summer photogrammetry DEM (Mavic)
#   massgis            — MassGIS aerial lidar bare-earth DEM (2021)
#
# Notes:
#   * Photogrammetry DEMs are surface models, not bare-earth.  Their
#     bare-class RMSE will be inflated wherever vegetation was present;
#     that's expected and informative.
#   * The MassGIS tile is from 2021; absolute offsets may reflect both
#     DTM error and real-world accretion since then.
#   * Cache CSVs go to output_dir (not next to the source files,
#     since several live on a read-only X drive share).
#
# Outputs (under lidar/output/<site>_ground_comparison/)
#   * <label>_ecp.csv        — per-point sample cache (one per source)
#   * ground_source_comparison.csv — combined overall + by-class summary
#
# Run from the project root in RStudio so relative paths resolve.
# Spring and summer lidar DTMs must exist first (run lidar/02.R for both
# dates, then lidar/03_evaluate_dtm.R to pick the best CSF set).
#------------------------------------------------------------------------------#

invisible(lapply(list.files("R/", pattern = "\\.[Rr]$",
                            full.names = TRUE), source))

#------------------------------------------------------------------------------#
# Parameters
#------------------------------------------------------------------------------#

site <- "rr"

ecp_path <- paste0(
   "X:/legacy/gdrive/saltmarsh_UAS_native/",
   "In Situ Data Collection/",
   "JoshSurveyPoints_AllSites_One_Sheet.xlsx"
)

tolerances <- c(0.10, 0.20)

output_dir <- file.path("lidar/output", paste0(site, "_ground_comparison"))

# Best CSF stems from Phase 1 evaluation — update these after running
# lidar/03_evaluate_dtm.R for each date.
best_csf_spring <- "csf_th0.06_res0.10_rgd2_0.25m.tif"
best_csf_summer <- "csf_th0.06_res0.10_rgd2_0.25m.tif"

sources <- c(
   lidar_spring = file.path(
      "E:/uas_scratch/lidar", site, "2022_05_14",
      "zzzraster", best_csf_spring
   ),
   lidar_summer = file.path(
      "E:/uas_scratch/lidar", site, "2022_08_10",
      "zzzraster", best_csf_summer
   ),
   photo_spring_hesai = paste0(
      "X:/legacy/gdrive/saltmarsh_UAS/UAS Data Collection/",
      "Red River/Orthos and DEMs 2022/26May2022/Low/",
      "26May2022_RED_Low_HesaiRGB_DEM.tif"
   ),
   photo_spring_mica = paste0(
      "X:/legacy/gdrive/saltmarsh_UAS/UAS Data Collection/",
      "Red River/Orthos and DEMs 2022/26May2022/Low/",
      "26May22_RR_Low_Mica_DEM.tif"
   ),
   photo_summer = paste0(
      "X:/legacy/gdrive/saltmarsh_UAS/UAS Data Collection/",
      "Red River/Orthos and DEMs 2022/10Aug2022/Low/",
      "10Aug22_RR_Low_Mavic_DEM.tif"
   ),
   massgis = paste0(
      "X:/scratch/bcompton/LiDAR/",
      "be_19TDG412612/be_19TDG412612.tif"
   )
)

#------------------------------------------------------------------------------#
# Run
#------------------------------------------------------------------------------#

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# Load ECPs — Training points only (survey-grade bare-ground control).
site_ecp <- load_ecp(ecp_path, site = site)

# Check which sources exist; warn but continue for missing ones.
missing <- !file.exists(sources)
if (any(missing)) {
   warning("The following source files were not found and will be skipped:\n",
           paste0("  ", names(sources)[missing], ": ",
                  sources[missing], collapse = "\n"))
   sources <- sources[!missing]
}
if (length(sources) == 0L) stop("No source files found.")

# Sample each source at ECP locations (skip-if-exists).
message("Sampling ", length(sources), " sources at ECP locations...")
all_elevations <- do.call(rbind, lapply(names(sources), function(label) {
   csv_path <- file.path(output_dir, paste0(label, "_ecp.csv"))
   elev     <- sample_dtm(sources[[label]], site_ecp,
                          output_csv = csv_path)
   elev$source <- label
   elev
}))

# Evaluate each source.
message("Evaluating sources...")
all_summaries <- do.call(rbind, lapply(names(sources), function(label) {
   elev    <- all_elevations[all_elevations$source == label, ]
   result  <- evaluate_dtm(elev, tolerances = tolerances)
   summary <- result$summary
   summary$source <- label
   summary
}))

# Reorder columns so source and group come first.
all_summaries <- all_summaries[
   , c("source", "group",
       setdiff(colnames(all_summaries), c("source", "group")))
]

# Write combined summary.
csv_out <- file.path(output_dir, "ground_source_comparison.csv")
write.csv(all_summaries, csv_out, row.names = FALSE)
message("Summary written: ", csv_out)

# Print overall ranking table.
message("\n=== Ground source ranking — overall RMSE ===")
overall <- all_summaries[all_summaries$group == "overall", ]
rank_cols <- c("source", "rmse", "mean_bias", "mae")
pct_col   <- grep("^pct_within", colnames(overall), value = TRUE)[1]
if (!is.na(pct_col)) rank_cols <- c(rank_cols, pct_col)
overall_rank <- overall[order(overall$rmse), rank_cols]
overall_rank$rmse      <- round(overall_rank$rmse, 3)
overall_rank$mean_bias <- round(overall_rank$mean_bias, 3)
overall_rank$mae       <- round(overall_rank$mae, 3)
if (!is.na(pct_col)) {
   overall_rank[[pct_col]] <- round(overall_rank[[pct_col]], 1)
}
print(overall_rank, row.names = FALSE)
