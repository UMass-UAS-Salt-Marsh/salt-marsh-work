# Phase 1 smoke test — DTM evaluation against ECPs

Manual end-to-end walkthrough of the DTM evaluation workflow for
site `rr` (Red River), source date `2022-08-10`.
Verifies the pieces fit together before the formal Phase 1 driver
(`lidar/03_evaluate_dtm.R`) is shipped.

Walks through the proposed function names, signatures, and argument
values.
**No code is run from this document — it documents the intended
workflow so a reader (or implementer) can check the shape before
anything is committed.**

## Prerequisites

Confirmed present before the smoke test starts:

- DTMs from the CSF tuning loop in `lidar/02.R`:
  ```
  E:/uas_scratch/lidar/rr/2022_08_10/zzzraster/csf_th*_res*_rgd*_*m.tif
  ```
  (4 files, one per `csf_grid` row.)
- ECP spreadsheet:
  ```
  X:/legacy/gdrive/saltmarsh_UAS_native/In Situ Data Collection/JoshSurveyPoints_AllSites_Meta_Datapoints.xlsx
  ```
- R packages: `lidR`, `sf`, `terra`, `readxl`, `dplyr`, `ggplot2`.
- New helpers in place (the four below are what Phase 1 produces).

New R/ functions Phase 1 introduces:

- `R/load_ecp.R` — read + clean ECP xlsx,
  return sf in target CRS.
- `R/sample_dtm.R` — single-DTM data assembly
  (raster → per-point `predicted`, cached as a sibling CSV).
- `R/evaluate_dtm.R` — pure summary + plotting from assembled
  data;
  computes `residual` / `abs_residual` internally.
- `lidar/03_evaluate_dtm.R` — driver that wires them together;
  the per-DTM CSV cache makes opening the reporting phase fast.

## Step 0 — Open R in the project root

```bash
cd X:/projects/uas/code/salt-marsh-work
# Open salt-marsh-work.Rproj in RStudio so relative paths resolve.
```

## Step 1 — Source helpers and pick site / date

```r
invisible(lapply(list.files("R/", pattern = "\\.[Rr]$",
                            full.names = TRUE), source))

site   <- "rr"
date   <- "2022_08_10"
target_epsg <- 26919L   # NAD83 / UTM 19N (matches the DTMs)
```

## Step 2 — Load ECPs

Proposed signature:

```r
load_ecp(
   path,
   target_crs       = 26919L,                # transform on read
   training_only    = TRUE,                  # filter type == "training"
   exclude_types    = c("Logger Array"),     # always-skip classes
   site             = NULL                   # if non-NULL, filter
)
```

Returns an `sf` object with at minimum:
`geometry, easting, northing, elevation, type, site, date`,
plus any additional ECP columns
(point id, surveyor notes, etc.)
passed through verbatim.

Smoke-test call:

```r
ecp <- load_ecp(
   path = paste0(
      "X:/legacy/gdrive/saltmarsh_UAS_native/",
      "In Situ Data Collection/",
      "JoshSurveyPoints_AllSites_Meta_Datapoints.xlsx"
   ),
   target_crs = target_epsg,
   site       = site
)
```

**Verify:**

- `nrow(ecp)` > 0.
- `sf::st_crs(ecp)$epsg == 26919`.
- `unique(ecp$type)` shows the expected class set for `rr`
  (bare mud, short Spartina, tall Spartina, *Iva*, *Phragmites*, …
  exact list TBD by what the xlsx contains).
- No rows have `type == "Logger Array"`.
- `range(ecp$elevation)` looks sensible for a saltmarsh
  (roughly 0–2 m NAVD 88).

## Step 3 — List the candidate DTMs

```r
dtm_dir <- file.path("E:/uas_scratch/lidar", site, date, "zzzraster")
dtm_paths <- list.files(dtm_dir,
                        pattern = "^csf_.*\\.tif$",
                        full.names = TRUE)
```

**Verify:**

- `length(dtm_paths) == nrow(csf_grid)`
  (4, given the current grid).
- Filenames embed the CSF parameters
  (`csf_th0.005_res0.05_rgd2_0.25m.tif`, etc.).
- `terra::rast(dtm_paths[1])` opens without error;
  CRS reads as EPSG:26919 + EPSG:5703.

## Step 4 — Assemble per-point eval data (one DTM at a time)

Proposed signature:

```r
sample_dtm(
   dtm,                  # file path OR a terra::SpatRaster
   ecp,                  # sf from load_ecp()
   output_csv = NULL,    # cache CSV path; if NULL, derive from `dtm`
                         # by swapping `.tif` -> `_ecp.csv` (sibling
                         # of the DTM)
   overwrite  = FALSE    # skip-if-exists pattern matching
                         # clean_and_tile() and reproject_las()
)
```

**Behavior:**

- If `output_csv` already exists and `overwrite = FALSE`,
  read the CSV and return.
  No `terra::extract()` call.
- Otherwise,
  run `terra::extract()` at the ECP coordinates,
  write the CSV next to the DTM,
  and return.

**CSV cache contents** — the ECP data frame
(geometry dropped,
easting/northing kept as columns)
plus **one additional column**, `predicted`:

```
easting, northing, elevation, type, site, date, ..., predicted
```

No derived columns are added — `residual` and `abs_residual`
are computed inside `evaluate_dtm()`,
so `sample_dtm()`'s output matches the on-disk cache exactly.
That concentrates elevation derivation in the reporting
function and keeps the assembly function's output as close to
raw data as possible.

**Function return** (always, whether served from cache or
fresh) matches the cache columns exactly:

```
easting, northing, elevation, type, site, date, ..., predicted
```

`predicted` comes from `terra::extract()`
(bilinear interpolation, default).

Smoke-test (single DTM):

```r
eval_one <- sample_dtm(
   dtm = dtm_paths[1],
   ecp = ecp
)
```

**Verify:**

- A new CSV exists at
  `E:/uas_scratch/lidar/rr/2022_08_10/zzzraster/csf_*_ecp.csv`
  (sibling of `dtm_paths[1]`,
  same stem with `_ecp.csv` instead of `.tif`).
- `nrow(eval_one) == nrow(ecp)`.
- `eval_one$predicted` has no NAs
  (every ECP falls inside the DTM extent).
- `eval_one` has no `residual` / `abs_residual` columns yet
  (those are added by `evaluate_dtm()` downstream).
- Re-running the same call is fast and emits a "skipping,
  output exists" message
  (or similar);
  the CSV mtime does not change.

## Step 5 — Assemble across all DTMs

Loop the single-DTM call;
each iteration writes its own CSV
(or skips if one exists).
No bundled cache file needed —
the per-DTM CSVs *are* the cache.

```r
elevations <- do.call(
   rbind,
   lapply(dtm_paths, function(d) {
      df <- sample_dtm(d, ecp)
      df$dtm <- basename(d)   # add identifier for the rbind
      df
   })
)
```

**Verify:**

- `length(list.files(dtm_dir, pattern = "_ecp\\.csv$"))` equals
  `length(dtm_paths)` (4).
- `nrow(elevations) == length(dtm_paths) * nrow(ecp)`.
- `unique(elevations$dtm)` has length 4
  (one entry per parameter set).
- Re-running the loop is fast
  (every call hits the CSV cache;
  no `terra::extract()` calls).

**This is the "expensive prep" boundary.**
Below here is cheap and iterable;
the per-DTM CSV cache lets you tweak `evaluate_dtm()`
freely without re-extracting,
*and* the CSVs are human-readable for ad-hoc inspection.

To force re-extraction for a specific DTM
(e.g., the DTM was regenerated with new CSF parameters),
delete its `*_ecp.csv` sibling and re-run,
or pass `overwrite = TRUE` to `sample_dtm()`.

### Where this loop lives in production

This same Step 4 + Step 5 loop runs in **two** places:

1. **At the end of `lidar/02.R`** (added as part of Phase 1).
   `lidar/02.R` already runs for a long time
   (reprojection + clean-and-tile + four CSF rasterizations);
   appending the ECP extraction means the cache is warm by the
   time the production run finishes.
   The existing "Read elevation control points" scaffold at the
   bottom of `lidar/02.R`
   (the partial `site_dtm <- csf_results$dtm[i]` block)
   is replaced by the `load_ecp()` + `sample_dtm()` loop.
2. **At the top of `lidar/03_evaluate_dtm.R`.**
   The same loop runs again,
   but in normal use every call hits the cached CSVs
   (no `terra::extract()`),
   so opening up the reporting phase is essentially instant.
   If `lidar/02.R` was re-run with new CSF parameters
   (deleting the old DTMs and their sibling CSVs),
   the loop in `03.R` rebuilds the cache transparently.

The duplication is intentional:
each driver is self-contained
(can be run from a cold start without depending on the other
having been run),
and the skip-if-exists pattern makes the duplication free in
the common case.

## Step 6 — Run the report (one DTM at a time)

Proposed signature:

```r
evaluate_dtm(
   elevations,          # data.frame from sample_dtm()
                       # (single DTM, or one slice from rbind)
   tolerances = c(0.10, 0.20)   # meters; for pct_within_* columns
)
```

Computes `residual = predicted - elevation` and
`abs_residual = abs(residual)` internally
(they're not present on the input `elevations` frame),
then derives the metrics and plots.
Returns:

```r
list(
   points  = <data.frame>,   # input + residual + abs_residual
   summary = <data.frame>,   # one row per class, plus "overall"
   offset  = <lm>,           # diagnostic only, not applied
   plots   = list(
      pred_vs_obs           = <ggplot>,
      residual_map          = <ggplot or leaflet>,
      residual_hist         = <ggplot>,
      residual_vs_elevation = <ggplot>,
      qq_residuals          = <ggplot>
   )
)
```

`summary` columns
(per `dev/work_plan.md` Phase 1):

```
class, n,
mean_bias, median_bias,
mae, mad, rmse,
max_abs, q25_abs, q75_abs, q95_abs,
pct_within_10cm, pct_within_20cm,
slope, bias_p
```

Smoke-test (first DTM only):

```r
data_one <- elevations[elevations$dtm == basename(dtm_paths[1]), ]
report_one <- evaluate_dtm(data_one)

report_one$summary       # inspect interactively
report_one$plots$pred_vs_obs
report_one$plots$residual_hist
```

**Verify on `report_one$points`:**

- `residual` and `abs_residual` columns now exist
  (added by `evaluate_dtm()`;
  they weren't present on `data_one`).
- `residual` is roughly zero-centered for bare classes;
  systematically positive for tall-veg classes
  (lidar can't see the marsh floor through the canopy).

**Verify on `report_one$summary`:**

- One row per ECP class plus an `overall` row.
- `n` column sums (across non-overall rows) to `nrow(data_one)`.
- `mean_bias` for the `overall` row is **small**
  (< ~10 cm in magnitude is the target;
  systematic bias here would suggest the PDAL reprojection
  still has a residual issue).
- `bias_p` for the `overall` row tells whether that small bias
  is statistically distinguishable from zero
  (small `n` and small bias → big `p`,
  which is fine).
- `rmse` for bare classes < `rmse` for tall-veg classes
  (vegetation-residual error is real,
  not a CSF failure;
  documented in `lidar/readme.md`).
- `slope` near 1.0 across all classes
  (if it diverges,
  flag for investigation).

**Verify on `report_one$plots`:**

- `pred_vs_obs` shows points scattered along the 1:1 line,
  colored by class.
- `residual_hist` is roughly symmetric for bare classes,
  right-skewed for tall-veg classes.
- `residual_map` shows no large spatial cluster of high residuals
  (a cluster would suggest a localized CSF problem).
- `qq_residuals` straight-ish line means residuals are roughly
  normal,
  curve means they aren't
  (informs whether `bias_p` should come from t-test or sign test).

## Step 7 — Run the report across all DTMs and write outputs

```r
output_dir <- file.path("lidar/output", paste0(site, "_", date))
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

reports <- lapply(
   split(elevations, elevations$dtm),
   evaluate_dtm
)

# Stack the per-DTM summaries into one CSV.
summary_all <- do.call(
   rbind,
   Map(function(r, dtm) cbind(dtm = dtm, r$summary),
       reports, names(reports))
)
write.csv(
   summary_all,
   file.path(output_dir, "dtm_eval_summary.csv"),
   row.names = FALSE
)

# Write plots per DTM.
for (dtm in names(reports)) {
   plots <- reports[[dtm]]$plots
   stem  <- sub("\\.tif$", "", dtm)
   for (pname in names(plots)) {
      ggplot2::ggsave(
         filename = file.path(output_dir,
                              paste0(stem, "_", pname, ".png")),
         plot     = plots[[pname]],
         width    = 8, height = 6, dpi = 150
      )
   }
}
```

**Verify:**

- `lidar/output/rr_2022_08_10/dtm_eval_summary.csv` exists,
  has `4 × n_classes` data rows plus 4 overall rows.
- Per-DTM PNGs land in `lidar/output/rr_2022_08_10/`
  (5 plots × 4 DTMs = 20 PNGs).

## Step 8 — Pick the winner

Eyeball `summary_all`,
ranking parameter sets two ways
(per the work plan):

1. **Bare-class RMSE** (and `pct_within_10cm` in bare class) —
   cleanest signal of CSF performance.
2. **All-class RMSE** — useful but biased upward by tall-veg
   vegetation-residual error.

The two ranks should agree on the winning parameter set;
if they disagree,
the bare-class rank wins
(see the rationale in `lidar/readme.md`).

Record the choice and the reasoning in `dev/worklog.md`.

## Iteration zone (the whole point of the split)

If after seeing the report you want to:

- add a new metric column,
- change a tolerance,
- add or tweak a plot,
- adjust binning / facets,

edit `R/evaluate_dtm.R`,
re-source,
and rerun Step 6 + Step 7.
**No re-extraction needed** — the `.rds` cache is reused.
That's the workflow this split is built for.
