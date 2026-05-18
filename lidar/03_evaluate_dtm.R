#------------------------------------------------------------------------------#
# DTM evaluation driver — score each CSF parameter set against ECPs
#------------------------------------------------------------------------------#
#
# Evaluates the four DTMs produced by `lidar/02.R` against the
# field-collected elevation control points (ECPs).
# Produces a summary CSV and per-DTM PNG plots for side-by-side
# comparison; prints a bare-class ranking table to the console to
# aid parameter-set selection.
#
# Inputs
#   * DTMs under `E:/uas_scratch/lidar/<site>/<date>/zzzraster/`
#     (produced by `lidar/02.R`).
#   * ECP xlsx (path set in Parameters below).
#   * Per-DTM `*_ecp.csv` cache files next to each DTM, written by
#     `sample_dtm()` at the end of `lidar/02.R`.
#     If they do not exist, `sample_dtm()` creates them here.
#
# Outputs (under `lidar/output/<site>_<date>/`)
#   * `dtm_eval_summary.csv` — one row per (DTM × ECP-class bucket)
#     plus an "overall" row per DTM.  All metric columns from
#     `evaluate_dtm()`.
#   * `<dtm_stem>_<plot_name>.png` — five plots per DTM.
#
# Run from the project root in RStudio so relative paths resolve.
# See `dev/phase1_smoketest.md` for a step-by-step manual walkthrough
# and verification points.
#------------------------------------------------------------------------------#

library(lidR)   # pulls in terra, needed by sample_dtm()

invisible(lapply(list.files("R/", pattern = "\\.[Rr]$",
                            full.names = TRUE), source))

#------------------------------------------------------------------------------#
# Parameters
#------------------------------------------------------------------------------#

site <- "rr"
date <- "2022_08_10"

ecp_path <- paste0(
   "X:/legacy/gdrive/saltmarsh_UAS_native/",
   "In Situ Data Collection/",
   "JoshSurveyPoints_AllSites_One_Sheet.xlsx"
)

tolerances <- c(0.10, 0.20)   # metres; for pct_within_* columns

base_output <- file.path("E:/uas_scratch/lidar", site, date)
output_dir  <- file.path("lidar/output", paste0(site, "_", date))


#------------------------------------------------------------------------------#
# Run
#------------------------------------------------------------------------------#

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# -- Step 1: load ECPs --------------------------------------------------------
message("Loading ECPs for site: ", site)
site_ecp <- load_ecp(ecp_path, site = site)
type_counts <- sort(table(site_ecp$type), decreasing = TRUE)
message("  ", nrow(site_ecp), " ECPs loaded (",
        paste(paste0(names(type_counts), "=", type_counts),
              collapse = ", "),
        ")")

# -- Step 2: list DTMs --------------------------------------------------------
dtm_dir   <- file.path(base_output, "zzzraster")
dtm_paths <- list.files(dtm_dir, pattern = "^csf_.*\\.tif$",
                        full.names = TRUE)
if (length(dtm_paths) == 0L) {
   stop("No DTMs found in: ", dtm_dir,
        "\nRun lidar/02.R first to produce them.")
}
message(length(dtm_paths), " DTMs found in ", dtm_dir)

# -- Step 3: sample DTMs at ECP locations (cache hit in normal use) -----------
message("Sampling DTMs at ECP locations (skip-if-exists)...")
elevations_list <- lapply(dtm_paths, function(d) {
   elev <- sample_dtm(d, site_ecp)
   elev$dtm <- basename(d)
   elev
})
elevations <- do.call(rbind, elevations_list)
message("  Total rows: ", nrow(elevations),
        " (", length(dtm_paths), " DTMs × ", nrow(site_ecp),
        " ECPs)")

# -- Step 4: evaluate each DTM and write outputs ------------------------------
message("Evaluating DTMs...")
all_summaries <- list()

for (d in dtm_paths) {
   stem       <- sub("\\.tif$", "", basename(d), ignore.case = TRUE)
   dtm_elev   <- elevations[elevations$dtm == basename(d), ]
   result     <- evaluate_dtm(dtm_elev, tolerances = tolerances)

   # Tag summary rows with the DTM identifier.
   summary_d          <- result$summary
   summary_d$dtm      <- stem
   all_summaries[[stem]] <- summary_d

   # Write plots.
   for (pname in names(result$plots)) {
      out_png <- file.path(output_dir,
                           paste0(stem, "_", pname, ".png"))
      ggplot2::ggsave(out_png, plot = result$plots[[pname]],
                      width = 8, height = 6, dpi = 150)
   }
   message("  ", stem, " — RMSE (overall): ",
           round(result$summary$rmse[result$summary$group == "overall"],
                 3), " m")
}

# -- Step 5: write combined summary CSV ---------------------------------------
summary_all <- do.call(rbind, all_summaries)
# Reorder columns so dtm and group come first.
summary_all <- summary_all[, c("dtm", "group",
                               setdiff(colnames(summary_all),
                                       c("dtm", "group")))]
csv_path <- file.path(output_dir, "dtm_eval_summary.csv")
write.csv(summary_all, csv_path, row.names = FALSE)
message("Summary written: ", csv_path)

# -- Step 6: console ranking table (bare-class then overall) ------------------
message("\n=== DTM ranking — bare-class RMSE ===")
bare_types <- c("Berm", "Training")
bare_rows  <- summary_all[summary_all$group %in% bare_types, ]
if (nrow(bare_rows) > 0L) {
   # Aggregate across bare types (mean RMSE as a simple proxy).
   bare_rank <- aggregate(
      rmse ~ dtm, data = bare_rows,
      FUN = function(x) round(mean(x, na.rm = TRUE), 3)
   )
   bare_rank <- bare_rank[order(bare_rank$rmse), ]
   print(bare_rank, row.names = FALSE)
} else {
   message("  (no Berm or Training rows in summary)")
}

message("\n=== DTM ranking — overall RMSE ===")
overall_rows <- summary_all[summary_all$group == "overall", ]
rank_cols    <- c("dtm", "rmse", "mean_bias", "mae")
# Append the first pct_within_* column if it exists.
pct_col <- grep("^pct_within", colnames(overall_rows),
                value = TRUE)[1]
if (!is.na(pct_col)) rank_cols <- c(rank_cols, pct_col)
overall_rank <- overall_rows[order(overall_rows$rmse), rank_cols]
overall_rank$rmse      <- round(overall_rank$rmse, 3)
overall_rank$mean_bias <- round(overall_rank$mean_bias, 3)
overall_rank$mae       <- round(overall_rank$mae, 3)
if (!is.na(pct_col)) {
   overall_rank[[pct_col]] <- round(overall_rank[[pct_col]], 1)
}
print(overall_rank, row.names = FALSE)
