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

Site: `rr` (Red River) — keep the current code's target. ECPs are
matched ignoring date (saltmarsh elevation considered near-static
yearly). Water-logger floor points are not used as ECPs.

- [ ] **`R/load_ecp.R`** — single function that reads the ECP xlsx,
  cleans columns, parses dates, lowercases site codes, and filters to
  `type = "training"` (per readme line 39). Returns an sf object in
  the cloud's CRS. ECPs marked "Logger Array" stay excluded per
  `lidar/02.R:210`.
- [ ] **Finish `evaluate_dtm(dtm, ecp)`** to return a tidy result list:
  - per-point table: `easting, northing, elevation, predicted, residual, type`
  - summary stats: n, bias (mean residual), MAE, RMSE, R², offset
    (from `lm(predicted ~ 1 + offset(elevation))`) — reported overall
    **and** broken out by ECP `type` (vegetation classes are expected
    to be harder than bare surfaces).
  - residual-vs-elevation plot, residual map, optional histogram
- [ ] **`lidar/03_evaluate_dtm.R`** — new driver that loops over the
  4 hand-picked CSF parameter combinations from `lidar/02.R:80–83`,
  calls `evaluate_dtm()` on each, and writes:
  - `lidar/output/<site>_<date>_dtm_eval.csv` — one row per
    (parameter set × ECP-type bucket) with summary stats; plus an
    "overall" row per parameter set.
  - PNG residual plots per parameter set.
- [ ] Inspect the results together before deciding whether to expand
  the parameter search; pick the best parameter set per site and
  record the choice + reasoning in `worklog.md`.

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
