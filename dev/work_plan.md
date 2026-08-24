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

All items below were actually completed back in May 2026 but the
checkboxes were never marked — confirmed against the current
codebase 2026-08-20 (see `dev/worklog.md`).

- [x] **`R/load_ecp.R`** — single function that reads the ECP xlsx,
  cleans columns,
  parses dates,
  lowercases site codes,
  and filters to `type = "training"`
  (per `lidar/readme.md`).
  Returns an sf object in the cloud's CRS.
  ECPs marked "Logger Array" stay excluded.
  Optional `site` arg subsets to a single site code in one call.

- [x] **`R/sample_dtm.R`** — pure data assembly.
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

- [x] **Rework `R/evaluate_dtm.R`** to reporting-only.
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

- [x] **Wire `sample_dtm()` into `lidar/02.R`.**
  Append a `load_ecp()` + per-DTM `sample_dtm()` loop after the
  CSF tuning loop,
  replacing the existing partial "Read elevation control points"
  scaffold at the bottom of the file.
  `lidar/02.R` already runs for a long time;
  appending the extraction means the per-DTM CSV cache is warm by
  the time the production run finishes,
  so the reporting phase opens fast.

- [x] **`lidar/03_evaluate_dtm.R`** — new driver.
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

- [x] **Inspection and selection.**
  Done for the original 4-parameter grid (the later 18-run expanded
  grid in `lidar/02.R` was never actually rasterized for `rr` — only
  the original 4 DTMs exist on disk for either date). Picked by
  **all-class (overall) RMSE**, recorded in
  `lidar/06_compare_ground_sources.R`'s comments but never given its
  own `worklog.md` entry until now (see `dev/worklog.md`, 2026-08-20).

  The plan's preferred **cleanest-signal RMSE** ranking (ECPs in open/
  minimally-vegetated cover, where the CSF has the least vegetation to
  see through and its ground classification is most directly
  comparable to the ECP) was not done separately. Correction to how
  this was first written up here: every ECP measures true
  ground-surface elevation regardless of what's growing on it — there
  is no subset of ECPs that measured "bare ground" as opposed to
  "vegetation." What varies by class is vegetation cover overhead,
  which is what makes some classes a cleaner CSF-vs-ECP comparison than
  others (see "Saltmarsh-specific notes for the report" below).
  `evaluate_dtm()` groups by ECP `subclass`, and for the `type ==
  "EVP"` points actually used in evaluation, `subclass` is a numeric
  code (1-12) with no cover-type label mapping anywhere in this
  codebase, so "which code(s) are open/unvegetated" isn't currently
  answerable without outside documentation (e.g. Josh Ward's thesis or
  the original classification-points spreadsheet). Flagged as a
  gap, not silently skipped.

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

### Phase 1.5 — compare ground elevation sources

Before the spring lidar DTM is used as the ground reference for the
vegetation strata work, compare it against all other available ground
elevation datasets for the site.
The comparison uses the same ECP-based evaluation stack already built
for Phase 1 — no new R/ functions are needed.

#### Goal

Rank the available sources by RMSE and bias against field ECPs,
and record the chosen ground reference + rationale in `worklog.md`.

#### Data sources (rr)

| Label | Path | Notes |
|---|---|---|
| `lidar_spring` | `E:/uas_scratch/lidar/rr/2022_05_14/zzzraster/<best_csf>.tif` | Spring lidar DTM (Phase 1) |
| `lidar_summer` | `E:/uas_scratch/lidar/rr/2022_08_10/zzzraster/<best_csf>.tif` | Summer lidar DTM (Phase 1) |
| `photo_spring_hesai` | `X:/legacy/gdrive/saltmarsh_UAS/UAS Data Collection/Red River/Orthos and DEMs 2022/26May2022/Low/26May2022_RED_Low_HesaiRGB_DEM.tif` | Spring photogrammetry DEM |
| `photo_spring_mica` | `X:/legacy/gdrive/saltmarsh_UAS/UAS Data Collection/Red River/Orthos and DEMs 2022/26May2022/Low/26May22_RR_Low_Mica_DEM.tif` | Spring photogrammetry DEM (alt sensor) |
| `photo_summer` | `X:/legacy/gdrive/saltmarsh_UAS/UAS Data Collection/Red River/Orthos and DEMs 2022/10Aug2022/Low/10Aug22_RR_Low_Mavic_DEM.tif` | Summer photogrammetry DEM |
| `massgis` | `X:/scratch/bcompton/LiDAR/be_19TDG412612/be_19TDG412612.tif` | MassGIS aerial lidar (2021) |

#### Implementation — `lidar/06_compare_ground_sources.R`

A single new driver script.
No new R/ functions — `load_ecp()`, `sample_dtm()`, and `evaluate_dtm()`
already handle any raster source.

Params block:
```r
site       <- "rr"
output_dir <- file.path("lidar/output", paste0(site, "_ground_comparison"))
```

Logic:
1. Source R/ helpers; load ECPs via `load_ecp()`.
2. Define a named character vector `sources` mapping label → path
   (the six rows above; best CSF stems filled in after Phase 1 runs).
3. Loop: for each source, call `sample_dtm()` with an explicit
   `output_csv = file.path(output_dir, paste0(label, "_ecp.csv"))`.
   Using `output_dir` rather than the source raster's own directory
   keeps all comparison cache files together and avoids writing next
   to read-only data on the X drive.
4. `evaluate_dtm()` on each sampled data frame; collect `$summary` rows,
   tag with `source` label.
5. Write combined `ground_source_comparison.csv` to `output_dir`.
6. Print an overall-RMSE ranking table to the console.

#### Notes

- `terra::extract()` inside `sample_dtm()` reprojects the ECP points
  to the raster's CRS on the fly, so CRS mismatches between sources
  are handled automatically.
- The photogrammetry DEMs are surface models (not bare-earth),
  so their bare-class RMSE will be inflated where vegetation was present
  — that's expected and informative.
- The MassGIS tile covers the 2021 acquisition; absolute elevation
  offsets may reflect both DTM error and real-world marsh accretion
  since then.

#### Decision point

Comparison ran for `rr`, `oth`, and `wel`
(`lidar/output/*_ground_comparison/`); results are unambiguous and
consistent: MassGIS beats every UAS-derived source by a wide margin
at all three sites, with near-zero bias
(rr: RMSE 0.044 m / bias +0.003 m vs. best lidar's 0.174 m / +0.165 m).
MassGIS is the pick for `rr`'s ground reference.

**Blocked from actually wiring this into `lidar/05_veg_heights.R`**
until Phase 1.6a (below) resolves the ECP-CRS open question — see
`CRS.md`. Once the point cloud's horizontal target is confirmed
correct, this becomes a straight path swap (MassGIS and the point
cloud will share a horizontal CRS, so no per-raster reprojection
hack is needed).

- [x] Write `lidar/06_compare_ground_sources.R`.
- [x] Run after spring lidar DTMs are evaluated (Phase 1 complete).
- [x] Record decision in `worklog.md` (2026-08-19).
- [x] Update `lidar/05_veg_heights.R`'s ground-reference path —
  unblocked by Phase 1.6a; superseded by Phase 1.7's
  floor-bias-corrected MassGIS raster rather than the raw MassGIS
  tile (see below).

### Phase 1.6 — diagnose UAS vertical bias

#### Background

Ground source comparison (Phase 1.5) showed all UAS-derived datasets
have a consistent +10–16 cm positive bias against ECPs while the
MassGIS aerial lidar tile shows no bias.
This rules out a *vertical* CRS/datum error in the ECPs and points to
the PPK vertical positioning shared by all UAS flights. (A separate,
newly-found *horizontal* CRS question about the ECPs is tracked in
Phase 1.6a below — the two are independent axes and don't contradict
each other.)

Full write-up in `lidar/readme.md` "Systematic vertical bias in
UAS-derived elevation data" and in `CRS.md`.

#### Candidate causes (revised 2026-08-19)

1. ~~Wrong GEOID12B tile~~ — **still not the explanation, confirmed
   empirically.** The processing notes claimed a choice between
   `g2012bu0.gtx` (CONUS) and `g2012bu4.gtx` ("MA coast"); both files
   exist locally (`g2012bu0` is the combined CONUS grid,
   `g2012bu1`–`g2012bu8` are its eight regional sub-tiles, `g2012bu4`
   covering the MA coast as described). Sampled both with `terra` at
   Red River and across a 1°×1° grid around it: identical to the last
   digit everywhere (max |diff| = 0.0 m) — `u0` is `u1`–`u8` merged
   into one file, not an independent surface, so picking one tile over
   the other can't change the result. See `CRS.md` "The geoid issue"
   for the full finding.
2. **Lever arm misconfiguration** — now the leading hypothesis. The
   0.3355 m vertical offset for the RESEPI sensor; if incorrect it
   propagates into every point height.
3. **Outdated geoid model (GEOID12B vs. GEOID18)** — real, but
   probably not the main story. GEOID18 is the current NGS standard
   (adopted as this project's standard in `CRS.md`); typical
   GEOID12B→GEOID18 differences in this region are a few cm, not the
   observed 10–16 cm, so this alone likely won't close the gap — worth
   fixing regardless, just don't expect it to fully explain the bias.

#### Diagnostic steps

- [ ] **Verify the CORS station used and its published height.**
  The example in the notes shows `mawr0390.23o` (a MACORS station).
  Look up the station's published NAD 83(2011) orthometric height from
  NGS and confirm it matches what PCMaster used.
- [ ] **Check the lever arm value** against the RESEPI sensor's
  documented specification; look for a transcription or sign error.
- [ ] **Record the finding and correction** in `worklog.md`.

#### Impact on current work

Vegetation height work (Phase 2) is **not blocked** by the *vertical*
bias — it cancels when summer heights are normalized against the
spring DTM because both carry the same offset.
Absolute elevation deliverables are blocked until this is resolved.
Phase 1.5's ground-reference decision point *is* blocked, but by the
separate horizontal-CRS question in Phase 1.6a, not by this vertical
bias.

### Phase 1.6a — establish and migrate to the correct CRS/geoid standard

Added 2026-08-19. Full research and rationale live in `CRS.md`
(project root) — this section is the execution checklist.

**Standard decided:** NAD83(2011) (not legacy NAD83) for the
horizontal datum, geographic EPSG:6318/6319; projected EPSG:6491
(Massachusetts Mainland State Plane — provisional, decided
2026-08-20 over the EPSG:6348/UTM19N alternative, see `CRS.md`'s
comparison); NAVD88 via **GEOID18** vertically (EPSG:5703, grid
`us_noaa_g2018u0.tif`). Supersedes the historical EPSG:26919 +
GEOID12B pairing.

See `CRS.md` for the full comparison and the NAPGD2022
not-yet-released status. The GEOID12B-tile-mixup theory was
re-examined with the actual local grid files (`g2012bu4.gtx` does
exist, covering the MA coast as the field notes said) and confirmed,
empirically, to be numerically identical to `g2012bu0.gtx` at Red
River — so it still isn't the explanation for the bias, just not for
the reason first claimed. See `CRS.md`'s "geoid issue" section.

**Blocking open question found while scoping this — now resolved.**
`R/load_ecp.R` hardcodes `source_crs <- 26919L` for the ECPs. This was
flagged as blocking because it was asserted rather than derived from
metadata, and if wrong (ECPs actually NAD83(2011), mislabeled as
legacy 26919), switching the point-cloud pipeline's horizontal target
would make `sample_dtm()`'s CRS-aware reprojection apply a spurious
~0.5–1 m shift. Josh Ward's thesis (2026-08-20) confirms the ECPs were
measured in NAD83/UTM19N, NAVD88 — legacy NAD83, exactly matching the
hardcoded value. `R/evaluate_dtm.R` and `R/plot_residual_map.R` inherit
the same, now-confirmed-correct, assumption. See `CRS.md`'s "Open
questions" for detail. No code change needed in `R/load_ecp.R` itself —
it was already right.

- [x] Research current geoid/datum standard; write `CRS.md`.
- [x] Confirm EPSG codes via local PROJ database.
- [x] Re-examine the GEOID12B tile-mixup theory against the actual
  local grid files — `g2012bu4.gtx` exists (MA coast, as originally
  described) but is numerically identical to `g2012bu0.gtx` at Red
  River, so tile choice still isn't the explanation (see Phase 1.6
  above).
- [x] Confirm MassGIS's bare-earth lidar uses GEOID18 (matches our
  vertical standard).
- [x] Resolve the ECP source-CRS question — confirmed via Josh Ward's
  thesis: legacy NAD83/UTM19N, NAVD88 (see above).
- [x] **Pick UTM19N (EPSG:6348) vs. State Plane (EPSG:6491)** for the
  projected-CRS standard — **Massachusetts Mainland State Plane,
  EPSG:6491, decided provisionally 2026-08-20** (see `CRS.md`'s
  comparison).
- [x] Download `us_noaa_g2018u0.tif` (GEOID18 CONUS grid) and update
  `reproject_las()`'s default `vgrid` — vertical-only, independent of
  the ECP question, safe to do now.
- [x] Update horizontal `target_epsg`/`target_crs`/`crs` defaults to
  EPSG:6491 (`lidar/02.R`, `R/reproject_las.R`,
  `R/las_needs_reprojection.R`, `R/load_ecp.R`, `R/evaluate_dtm.R`,
  `R/plot_residual_map.R`); namespaced `zzzcleaned/`/`zzzraster/` by
  `target_epsg` in `lidar/02.R` so a new-CRS run doesn't collide with
  the existing 26919 outputs.
- [ ] Re-run Step 0 (reprojection) + Step 1 (clean & tile) for `rr`
  spring and summer clouds under the new target CRS/geoid.
- [ ] Re-run Phase 1 CSF tuning + DTM evaluation to reconfirm best
  parameters and measure how much the vertical bias actually shrinks.
- [ ] Re-run `lidar/06_compare_ground_sources.R` for `rr` under the
  corrected CRS.
- [ ] Return to the Phase 1.5 decision point above: wire the ground
  reference into `lidar/05_veg_heights.R`.

### Phase 1.7 — floor-bias-corrected ground reference for veg heights

Added 2026-08-24. Resolves the Phase 1.5 decision point using a
refinement of the Phase 1.6 bias finding rather than either extreme
(raw MassGIS, or the legacy spring-lidar-DTM-relative approach).

#### Refined bias decomposition

The UAS ground bias isn't one fixed number — `lidar_spring` = +0.158 m
vs. `lidar_summer` = +0.215 m at `rr` (Phase 1.5). Hypothesis: this is
two additive pieces — a roughly constant instrument/PPK-level offset
(Phase 1.6's still-open diagnosis) plus a CSF ground-finding error that
grows with vegetation density (worse in summer). Confirmed by
`estimate_floor_bias()` (`R/estimate_floor_bias.R`) run against the
cached `lidar_spring_ecp.csv` / `lidar_summer_ecp.csv` residuals
(`lidar/07_floor_corrected_ground.R`):

| | mean bias | floor bias | implied CSF veg-error |
|---|---|---|---|
| spring | 0.158 m | **0.103 m** | 0.055 m |
| summer | 0.215 m | **0.114 m** | 0.101 m |

The floor estimates (0.103 m, 0.114 m) are close — only 1.1 cm apart —
strongly supporting a roughly-constant instrument-level offset, with
the CSF ground-finding error accounting for nearly all of the
spring-to-summer gap in mean bias.

#### Decision

Ground reference for vegetation heights = **MassGIS + floor_bias_summer
(0.114 m)**, written by `lidar/07_floor_corrected_ground.R` to
`lidar/output/rr_ground_comparison/massgis_plus_floor_summer.tif`.
This keeps MassGIS's near-zero absolute bias while still cancelling
the instrument-level offset the legacy spring-DTM-relative approach
relied on — without also inheriting that approach's uncancelled
spring-only CSF ground-finding error (~5.5 cm).

`lidar/05_veg_heights.R`'s `ground_raster` now defaults to this file;
the legacy spring-lidar-DTM path is kept as a commented-out
alternative for comparison.

#### Empirical validation — `lidar/08_veg_height_validation.R`

The ECP dataset has field-measured `veg_height_cm` at all 169 `rr`
summer survey points — independent ground truth not used to fit
`floor_bias_summer`. Built a canopy-top DSM
(`R/rasterize_canopy_top.R`, high-percentile `Z` per pixel from the
same last-return cleaned tiles the production pipeline uses) and
compared predicted vegetation height (canopy top minus each candidate
ground) against `veg_height_m`, n = 164 (5 ECPs fell outside the
spring DTM extent):

| option | mean bias | rmse | pct within 20 cm |
|---|---|---|---|
| opt2 — spring lidar DTM (legacy) | −0.311 m | 0.386 m | 31.7% |
| opt3 — MassGIS + floor bias | −0.270 m | 0.349 m | 46.3% |

(Numbers corrected 2026-08-24 — the canopy-top raster was originally
built from a stale, wrong-CRS cleaned-tile directory; see "Found and
fixed" below. Conclusion unchanged.)

Option 3 wins on every metric, by almost exactly the amount the bias
decomposition predicts (mean-bias gap 0.041 m ≈ spring mean bias
0.158 m − summer floor bias 0.114 m + massgis's own small bias).
**Confirms the Phase 1.7 decision.**

**Found and fixed while returning to this later: a real CRS bug.**
`lidar/08_veg_height_validation.R` (and originally `lidar/05_veg_heights.R`
too) built the canopy-top raster from
`E:/uas_scratch/lidar/rr/2022_08_10/zzzcleaned/` — a stale directory
from 2026-05-15, confirmed via `readLASheader()` to be **EPSG:26919**
(legacy NAD83/UTM19N), not the current EPSG:6491 standard used by
every ground raster in this phase. The correct, current tiles are in
the namespaced `zzzcleaned_epsg6491/` (built 2026-08-21, same `02.R`
run that produced the CSF DTMs). Per `CRS.md`, the two CRSs carry a
real ~0.5-1 m horizontal frame shift — not just a label difference —
so the canopy-top samples were coming from a differently-positioned
point cloud than the ground samples. Fixed by parameterizing both
scripts' clean-tile path on `target_epsg` (matching `lidar/02.R`'s own
convention) and rebuilding the canopy-top raster from the correct
tiles; numbers above are post-fix. The fix mildly *improved* RMSE
(0.397→0.386, 0.363→0.349) as expected from removing spatial-mismatch
noise, and left the bias-decomposition story intact.

**A second CRS bug, found while wiring up Phase 2 below:**
`massgis_plus_floor_summer.tif` was built directly from the MassGIS
BE tile without reprojecting it — MassGIS publishes in **EPSG:6348**
(NAD83(2011)/UTM19N), not this project's EPSG:6491 (Mass State Plane)
standard. `sample_dtm()`'s ECP sampling is CRS-aware (reprojects the
ECPs to match the raster), so this didn't affect any number reported
above. But `lidR::normalize_height()` — used by
`rasterize_veg_heights()` in Phase 2 — does a **raw coordinate lookup
with no reprojection**, so every point silently missed the raster
entirely and fell into an unbounded `knnidw` fallback, which is what
actually caused Phase 2's out-of-memory crashes (see Phase 2 below for
the full chain). Fixed `lidar/07_floor_corrected_ground.R` to
`terra::project()` the MassGIS raster to `target_epsg` before adding
the floor bias; rebuilt `massgis_plus_floor_summer.tif`. Re-ran
`lidar/08_veg_height_validation.R` to confirm: numbers matched the
pre-fix ones to the fourth decimal place, as expected.

**Separate, larger finding surfaced by this validation — cause still
unknown.** Both options underestimate true vegetation height by
~15 cm *before* the ground-reference difference is even applied
(back-calculated: canopy-top error ≈ −0.153 m for both options —
consistent with each other, meaning it's common to both and not a
ground-reference artifact).

First hypothesis — that `clean_and_tile()`'s last-return-only
filtering (used for all inputs in this pipeline) loses the true top of
multi-return pulses through dense canopy — was **checked directly
against the raw point cloud and refuted**: sampled ~328k pulses from a
40×40 m box of the raw `rr` summer cloud
(`.../RESEPI-5FFC59-2022-08-10-20-26-50/clouds/ppk_07Nov2022_cloud_1.las`),
paired first and last returns by `gpstime`, and found `Z_last -
Z_first == 0` **exactly, for every single pulse** (all had
`NumberOfReturns == 2`). First and last returns are byte-identical in
this point cloud — a RESEPI/sensor characteristic, not a per-pulse
range difference — so last-return filtering discards no height
information relative to first returns.

The ~15 cm canopy-top underestimate therefore has some other cause —
candidates not yet checked: `rasterize_canopy_top()`'s 0.25 m pixel
size averaging over/missing the single tallest blade near an ECP
(spatial sampling mismatch vs. a point-measurement field survey),
lidar beam-footprint effects on thin vegetation, or a difference in
what `veg_height_m` actually measures (e.g. tallest nearby stem vs.
exactly at the GPS point). Needs its own investigation before absolute
(not just relative) vegetation-height outputs from this pipeline
should be trusted; not scheduled as its own phase yet.

#### Checklist

- [x] `R/estimate_floor_bias.R`, `R/plot_floor_bias.R`.
- [x] `R/rasterize_canopy_top.R`.
- [x] `lidar/07_floor_corrected_ground.R` — run for `rr`; floor
  biases and corrected raster produced (raster written to
  `E:/uas_scratch/lidar/rr/2022_08_10/zzzraster_epsg6491/`, alongside
  the other rr summer rasters — not under `lidar/output/`).
- [x] `lidar/08_veg_height_validation.R` — run for `rr`; confirms the
  decision against field-measured `veg_height_m`.
- [x] Wire `lidar/05_veg_heights.R` to the corrected ground raster.
- [x] Check whether last-return filtering explains the ~15 cm
  canopy-top underestimate — **refuted** (see above).
- [ ] Investigate the actual cause of the ~15 cm canopy-top
  underestimate — separate from this phase's ground-reference
  decision, still open.

### Phase 2 — vegetation strata (Goal 2)

Approach: normalize point heights to the chosen DTM, then compute a
multi-band raster of **percent of returns** per height bin per pixel.
Scan density is treated as noise — percent normalizes it out, and with
~300 pts/m² the per-pixel sample sizes are large enough to keep
percent estimates stable. No direct validation dataset; downstream
modeling performance is the pseudo-validation.

**Reconciled and completed 2026-08-24.** `R/rasterize_veg_heights.R`
and `lidar/05_veg_heights.R` (built and used all session for Phase
1.7's ground-reference work) already did almost exactly this — same
bin scheme (5 cm bins 0–1 m, 20 cm bins 1–3 m → 30 bands), same 0.5 m
default resolution, same normalize-then-bin approach — under the names
originally planned below as `R/rasterize_strata.R` /
`lidar/04_vegetation_strata.R`. But it had never actually produced
output for `rr`: `E:/uas_scratch/lidar/rr/2022_08_10/zzzheights/`
contained only an empty, stale `veg_dist_0.5m_chunks/` directory dated
2026-05-21. Tracked down why and got it running:

1. **Closure bug (likely the original blocker).** The per-cell metric
   function (`fracs_fn`) was defined *inside* `compute_heights()`, the
   function `lidR::catalog_map()` dispatches per chunk — but
   `catalog_map()`'s dispatch doesn't preserve that nested closure, so
   every chunk failed with `could not find function "fracs_fn"`, even
   running fully sequentially (`plan(sequential)`), so this was never
   a multisession-only issue. Fixed by promoting it to a top-level
   function, `R/bin_fractions.R`, with `bin_breaks`/`bin_names` baked
   into the `pixel_metrics()` formula as literal values via
   `bquote()` rather than closure lookups.
2. **The promoted top-level function still wasn't found in
   `multisession` workers** — `future`'s automatic global-variable
   detection can't see a symbol referenced only inside a quoted
   formula, so a worker process never received `bin_fractions`'s
   definition. Tried embedding the function *object itself* (not a
   symbol) as a literal in the formula via `bquote()` — this broke a
   different way: `pixel_metrics()` deparses its formula to text and
   reconstructs it inside a `data.table` call, and a deparsed
   multi-line function literal doesn't survive that round-trip
   (`unexpected 'if'`). Landed on: keep `bin_fractions` as a plain
   symbol (deparses safely), and have `compute_heights()` source
   `R/bin_fractions.R` itself, guarded by
   `exists("bin_fractions", mode = "function")`, so each worker is
   self-sufficient regardless of what `future` did or didn't ship.
3. **Out-of-memory crashes, root cause the EPSG:6348 bug above.** Once
   the closure bug was fixed, chunks started crashing with
   `std::bad_alloc`. Traced to `lidR::normalize_height()`: given a
   raster, it does a raw coordinate lookup with no CRS-aware
   reprojection (unlike `sample_dtm()`), and since
   `massgis_plus_floor_summer.tif` was in EPSG:6348 while the point
   cloud is EPSG:6491, *every* point missed the raster and fell into
   `normalize_height()`'s fallback — an unbounded-radius `knnidw`
   nearest-neighbor search against the whole raster reinterpreted as
   points, which is where the memory went. Fixed by the CRS fix
   above; after that, only a handful of genuinely-outside-coverage
   points per chunk hit the (now cheap) fallback.
4. Also reduced `chunk_size` from 200 to 100 m — some `rr` summer
   tiles run 6-12 million last-return points per 200×200 m tile
   (~275 pts/m²), and smaller chunks leave more headroom regardless of
   the fixes above.

Ran `lidar/05_veg_heights.R` for `rr` (real multisession config,
`workers = 25`) after all four fixes: completed cleanly, producing
`E:/uas_scratch/lidar/rr/2022_08_10/zzzheights/veg_dist_0.5m.tif` — a
real 30-band, 0.5 m raster (1463×1571 px, EPSG:6491, extent matching
the point cloud). Confirmed sane: per-band values in [0, 1], per-pixel
row sums in [0, 1] (not always 1 — see the open decision below).

- [x] ~~`R/rasterize_strata.R`~~ — superseded by `R/bin_fractions.R` +
  `R/rasterize_veg_heights.R` (same design).
- [x] ~~`lidar/04_vegetation_strata.R`~~ — superseded by
  `lidar/05_veg_heights.R`; output goes to `zzzheights/` rather than
  the originally planned `zzzstrata/` (naming only).
- [x] Run `lidar/05_veg_heights.R` for `rr` — done; real
  `veg_dist_0.5m.tif` confirmed on disk.
- [ ] **Decision point (still open):** the fraction denominator is
  *all* returns in the cell (including below-ground/above-ceiling
  ones), not just in-range returns, so per-pixel bands don't sum to
  100% as this section's design originally specified. Fix to match
  the original design, or accept "all returns" as intentional? Ask
  before changing — this affects every existing output.
- [ ] **Decision point (still open):** raster resolution. Default to
  0.5 m per readme; revisit producing 0.25 m / 1 m alternatives now
  that a real first output is in hand.

### Phase 3 — apply across sites

**Site scoping (2026-08-24):**

| Site | Cloud pair (spring + summer) | ECPs (type=EVP) | Status |
|---|---|---|---|
| `rr` | 2022-05-14 / 2022-08-10 | 169 | Done (Phase 1–1.7) |
| `wel` | 2022-05-20 / 2022-08-02 | 153 | **Ready, not started** — has everything the pipeline needs; not yet run through Steps 1–2 (clean/tile → CSF DTM). Best next candidate. |
| `oth` | none (photogrammetry only) | 145 | **Blocked** — no raw point cloud, so the CSF pipeline can't run here at all; ground-source comparison (photogrammetry + MassGIS only) already done in `lidar/06_compare_ground_sources.R`. |
| `nor` | 2024-05-24 / 2024-10-06 | **0** | Blocked — no ECPs (readme's top-priority site, but can't validate against anything until ECPs exist for it). |
| `peg` | 2024-04-19 / 2024-09-23 | **0** | Blocked — same reason as `nor`; not previously noted in this doc. |

`wel`'s raw clouds are larger than `rr`'s (7.38 GB summer, 5.03 GB
spring, vs. `rr`'s 5.9 GB summer) — Steps 0–2 (reproject, clean & tile,
CSF tuning per date) will take longer and are memory-bound per
`lidar/02.R`'s own notes. `wel` also spans two MassGIS tiles
(`be_19TDG415636.tif`, `be_19TDG417636.tif`, non-overlapping — each
ECP falls in only one), so extending Phase 1.7's floor-bias approach
there needs a `terra::merge()` mosaic step first.

- [ ] Run `wel` through Steps 0–2 (`lidar/02.R`, both 2022 dates),
  Step 3 evaluation, Phase 1.5/1.7 ground comparison + floor-bias
  correction, and Phase 2 veg heights — **on hold**, pending a decision
  on when to run the longer Steps 0–2 pass.
- [ ] `oth`/`nor`/`peg` stay blocked until their respective gaps
  (no cloud; no ECPs; no ECPs) are resolved.

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
