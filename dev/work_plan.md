# Work plan — lidar elevation & vegetation strata

## Goals

1. **Elevation (DTM).** Develop and validate an approach for estimating
   bare-earth elevation from each site's lidar point cloud, scored
   against field-collected elevation control points (ECPs).
2. **Vegetation strata.** Produce a multi-band raster characterizing
   above-ground biomass distribution by height bin: 5 cm bins from
   0–1 m, then 20 cm bins from 1–3 m (30 bands total). Per
   `lidar/readme.md`, this is the "dream output."

## Current state (what already exists)

- **Step 0: conditional reprojection** — `R/reproject_las.R`
  dispatcher routes to `reproject_las_pdal()` (PDAL, default,
  working) or `reproject_las_lastools()` (license-gated, currently
  inert on this machine because `lasvdatum` requires a paid
  LAStools license).  `R/las_needs_reprojection.R` decides whether
  to run by reading the LAS header
  (handles both GeoTIFF-keyed and LAS 1.4 WKT-encoded CRSes).
  See `lidar/readme.md` "CRS, datum, and frame shift considerations"
  for the geodetic discussion.
- **Step 1: clean & tile** — `R/clean_and_tile.R` filters to last
  returns, removes noise via `lidR::sor()`, writes cleaned tiles.
  Skips if output tiles already exist.
- **Step 2: rasterize ground** — `R/rasterize_ground.R` runs CSF
  classification with `(class_threshold, cloth_resolution, rigidness)`
  then `rasterize_terrain(knnidw)`. Writes one GeoTIFF per parameter
  set.
- **Driver** — `lidar/02.R` consolidates run-level parameters
  (`workers`, `chunk_size`, `chunk_buffer`, `site`, `csf_grid`) at
  the top, runs the conditional reprojection step, then Step 1
  once, then loops Step 2 over each row of `csf_grid`.  Outputs
  under `E:/uas_scratch/lidar/<site>/<date>/`:
  reprojected LAS in `reprojected/`,
  cleaned tiles in `zzzcleaned/`,
  DTMs in `zzzraster/csf_*.tif`.
- **Step 3 (validation): partial.** `R/evaluate_dtm.R` returns
  `list(points, summary, offset, plots)` with stats overall and
  broken out by ECP `type`.  Not yet looped over the parameter
  grid; `R/load_ecp.R` not yet written; no `lidar/03_*.R` driver.
- **Step 4 (veg strata): not started.**

Phase 0 (initial cleanup) and Phase 0.5 (reprojection) are both
complete; see `dev/worklog.md` entries on 2026-05-13 and 2026-05-15
for the detailed history.
The only Phase 0.5 task still outstanding is the first
end-to-end `lidar/02.R` run with the PDAL reprojection in place,
which is in progress at time of writing.

## Plan

### Phase 1 — finish DTM evaluation (Goal 1)

Site: `rr` (Red River) — keep the current code's target.
ECPs are matched ignoring date
(saltmarsh elevation considered near-static yearly).
Water-logger floor points are not used as ECPs.

End-to-end walkthrough of the intended workflow,
with proposed signatures and verification points,
lives in `dev/phase1_smoketest.md`.
This section is the implementation plan;
the smoke test is the manual run-through.

**Two design decisions worth flagging up front:**

1. **Pivot away from offset-correction.**
   The existing `R/evaluate_dtm.R` was written when the source LAS
   was in the wrong vertical datum,
   so it fit and subtracted a global offset to absorb the ~28 m bias.
   Now that PDAL applies GEOID12B correctly,
   that offset model is no longer needed for bias correction.
   Instead the evaluation **tests** that the residual bias is small
   (and flags it as a finding if not);
   metrics are reported on raw residuals,
   not offset-adjusted ones.
   The offset model itself stays as a diagnostic.

2. **Split into assembly and reporting.**
   The old `evaluate_dtm()` had two responsibilities
   (assemble the DTM-at-ECP data,
   then compute metrics and plots).
   Splitting these gives a fast iteration loop:
   the assembly is the expensive part
   (raster I/O + `terra::extract()`),
   so it caches to a CSV next to each DTM;
   the reporting is fast,
   so changes to metric definitions or plots can be tested
   without re-extracting.
   The two functions become `sample_dtm()` (assembly)
   and `evaluate_dtm()` (reporting),
   pairing cleanly:
   `data <- sample_dtm(dtm, ecp); result <- evaluate_dtm(data)`.

#### Plan items

- [ ] **`R/load_ecp.R`** — single function that reads the ECP xlsx,
  cleans columns,
  parses dates,
  lowercases site codes,
  and filters to `type = "training"`
  (per `lidar/readme.md`).
  Returns an sf object in the cloud's CRS.
  ECPs marked "Logger Array" stay excluded.
  Optional `site` arg subsets to a single site code in one call.

- [ ] **`R/sample_dtm.R`** — pure data assembly.
  Takes one DTM (path or `terra::SpatRaster`) and the ECP sf
  object;
  returns the ECP data frame with **one additional column**,
  `predicted` (no derived columns).
  Caches that same frame to a CSV next to the DTM
  (`<dtm-stem>_ecp.csv`) using the same skip-if-exists pattern as
  `clean_and_tile()` and `reproject_las()`.
  The function's return matches the on-disk cache exactly;
  `residual` and `abs_residual` are computed inside
  `evaluate_dtm()`,
  not here,
  so the assembly stays as close to raw data as possible.
  Signature:
  ```r
  sample_dtm(dtm, ecp, output_csv = NULL, overwrite = FALSE)
  ```

- [ ] **Rework `R/evaluate_dtm.R`** to reporting-only.
  Takes the data frame from `sample_dtm()` (single DTM or rbind of
  several with a `dtm` column).
  Computes `residual = predicted - elevation` and
  `abs_residual = abs(residual)` internally
  (they're not present on the input),
  then derives the metrics and plots and returns a tidy result
  list.
  Drops the data-assembly path and the "subtract offset"
  pre-correction;
  the existing `adj_*` columns go away.

  - **`points`** — per-point table:
    `easting, northing, elevation, predicted, residual, abs_residual, type`
    (input plus the two derived columns).

  - **`summary`** — one row per class (`type`),
    plus an "overall" row.
    Columns:
    - `n` — sample size
      (essential context for every other metric).
    - `mean_bias` — mean residual
      (signed; should be near zero if PDAL got the geoid right).
    - `median_bias` — robust complement to `mean_bias`;
      large divergence flags skewed residuals.
    - `mae` — mean absolute difference.
    - `mad` — median absolute deviation;
      robust counterpart to MAE,
      insensitive to a few outliers in tall-veg classes.
    - `rmse` — root mean squared error;
      standard reference for vertical accuracy and what other
      lidar-vs-ECP comparisons report.
    - `max_abs` — max |Δ|;
      worst case.
    - `q25_abs`, `q75_abs`, `q95_abs` — quantiles of |Δ|;
      characterize central spread and the upper tail without being
      dominated by the single worst point.
    - `pct_within_10cm`, `pct_within_20cm` — fraction of points
      with |Δ| within those tolerances.
      Direct usability statement
      ("82% of bare ECPs within ±10 cm").
    - `slope` — slope from `lm(predicted ~ observed)`;
      should be ~1 if the DTM tracks ECPs cleanly.
      Deviation from 1 hints at compression/stretch in Z
      (e.g., DTM smoother in low areas than high).
    - `bias_p` — p-value from one-sample t-test of residuals vs 0
      (sign test alternative for non-normal residuals;
      pick one,
      document choice).
      Flags whether the bias is statistically distinguishable
      from zero.

  - **`offset`** — kept as a diagnostic only.
    `lm` fit of `predicted ~ 1 + offset(elevation)` summarizing the
    global correction that *would* be applied if we wanted to
    remove the bias.
    Not used to adjust any other metric.

  - **`plots`** — named list:
    - `pred_vs_obs` — scatter of predicted vs observed with the
      1:1 line,
      faceted or colored by `type`.
    - `residual_map` — spatial map of residuals
      (already implemented).
    - `residual_hist` — histogram of residuals,
      faceted by `type`.
    - `residual_vs_elevation` — scatter of residuals against
      observed elevation,
      colored by `type`.
    - `qq_residuals` — Q-Q plot of residuals to flag non-normality
      (matters for choosing between t-test and sign test for
      `bias_p`).

- [ ] **Wire `sample_dtm()` into `lidar/02.R`.**
  Append a `load_ecp()` + per-DTM `sample_dtm()` loop after the
  CSF tuning loop,
  replacing the existing partial "Read elevation control points"
  scaffold at the bottom of the file.
  `lidar/02.R` already runs for a long time;
  appending the extraction means the per-DTM CSV cache is warm by
  the time the production run finishes,
  so the reporting phase opens fast.

- [ ] **`lidar/03_evaluate_dtm.R`** — new driver.
  Re-runs the same `load_ecp()` + `sample_dtm()` loop at the top
  (cache hit in the normal case,
  so it's essentially instant;
  rebuilds the cache transparently if DTMs were regenerated),
  then calls `evaluate_dtm()` on each DTM's data and writes:

  - `lidar/output/<site>_<date>/dtm_eval_summary.csv` — one row
    per `(parameter set × class)`,
    plus an "overall" row per parameter set.
    All summary columns above.
  - Per-DTM PNG plots in the same directory.

  The duplication of the assembly loop between `02.R` and `03.R`
  is intentional:
  each driver is self-contained
  (can be run from a cold start without depending on the other),
  and the skip-if-exists CSV cache makes the duplication free in
  the common case.

- [ ] **Inspection and selection.**
  Inspect the results together before deciding whether to expand
  the parameter search.
  Rank parameter sets two ways:

  1. **By bare-class RMSE** (and by `pct_within_10cm` in bare class).
     Bare ground is what the CSF is actually classifying,
     so this is the cleanest signal.
  2. **By all-class RMSE.**
     Useful but biased upward by tall-veg classes where the lidar
     can't see the marsh floor through the canopy —
     those errors are vegetation-residual,
     not CSF errors.

  Pick the best parameter set per site and record the choice +
  reasoning in `worklog.md`.

#### Saltmarsh-specific notes for the report

- **Tall-veg classes carry confounded error.**
  In *Spartina alterniflora* tall form,
  *Phragmites*,
  *Iva*,
  etc.,
  the "DTM error" is really
  `DTM error + residual vegetation the CSF didn't filter`.
  Errors in those classes will be systematically positive
  (predicted > observed) and depend on point density,
  vegetation density,
  and CSF aggressiveness.
  Annotate so a reader doesn't read
  "20 cm RMSE in tall Spartina" as "the CSF is wrong" —
  the lidar simply can't see the marsh floor through the canopy
  at every pixel.

- **Sample-size context.**
  ECPs in tall-veg classes are typically sparse compared to bare
  or short-veg.
  Always report metrics with `n` next to them so a 20 cm MAE on
  n=5 doesn't read the same as one on n=500.

- **Spatial coverage.**
  Worth a quick look at whether ECPs are representative of the
  marsh area or clustered in one zone —
  if the latter,
  the metrics generalize less than they look.

### Phase 2 — vegetation strata (Goal 2)

Approach: normalize point heights to the chosen DTM, then compute a
multi-band raster of **percent of returns** per height bin per pixel.
Scan density is treated as noise — percent normalizes it out, and with
~300 pts/m² the per-pixel sample sizes are large enough to keep
percent estimates stable. No direct validation dataset; downstream
modeling performance is the pseudo-validation.

- [ ] **`R/rasterize_strata.R`** — new function that:
  - takes the cleaned tile catalog + a DTM raster + bin edges + raster res
  - inside `catalog_map`: subtracts ground elevation (`lidR::normalize_height()`
    with the supplied DTM), drops returns < 0 m or > top bin, then uses
    `pixel_metrics()` with a custom function returning a named numeric
    vector of length `n_bins` = **percent of returns within the pixel
    falling in each bin** (sums to 100% across bands per pixel).
  - writes a multi-band GeoTIFF, one band per bin, plus a sidecar CSV
    of bin edges and band names.
- [ ] **`lidar/04_vegetation_strata.R`** — driver: pick the chosen
  DTM (output of Phase 1), call `rasterize_strata()` for the bin
  scheme in `lidar/readme.md:14–18` (5 cm bins 0–1 m, 20 cm bins
  1–3 m → 30 bands), write outputs to
  `E:/uas_scratch/lidar/<site>/<date>/zzzstrata/`.
- [ ] **Decision point (deferred):** raster resolution. Default to
  0.5 m per readme; revisit producing 0.25 m / 1 m alternatives after
  the first output is in hand.

### Phase 3 — apply across sites

- [ ] Once the per-site workflow is solid for `rr`, run it for other
  sites with ECP coverage. North River (the readme's top priority) is
  on hold until/unless ECPs become available for it.

## Decisions locked in

1. **First site:** `rr` (Red River) — sticking with the current code.
   `nor` deferred: no ECPs available for it.
2. **CSF parameter search:** keep the 4 hand-picked combinations from
   `lidar/02.R:80–83` for the first round; reassess after seeing
   results.
3. **Water-logger floor points as ECPs:** skip.
4. **Date matching ECP↔DTM:** ignore date; compare all site ECPs
   against each DTM.
5. **Strata output units:** percent of returns within the pixel
   (scan density is noise; ~300 pts/m² keeps percent estimates stable
   at 0.5 m pixels).
6. **Strata validation:** no field veg-height dataset; downstream
   model performance is the pseudo-validation.
7. **DTM stats by ECP type:** report overall **and** broken out by
   ECP `type` (vegetation classes are expected to be harder).

## Out of scope (for now)

- Wrapping anything into a formal R package (`DESCRIPTION` / `NAMESPACE`).
- Touching the hydrology or logger-recalibration pipelines.
- Producing the final veg-modeling deliverables that consume these
  rasters — that's a separate downstream project.
