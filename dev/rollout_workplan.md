# Work plan — lidar elevation & vegetation strata

**Stale.**
Written against the pre-refactor driver layout;
see `dev/driver_refactor_plan.md` for the tuning/production split
this will need to be reconciled against.

This file tracks only the **currently active, still-open** work.
For how the pipeline works (stages, functions, data flow), see
[`lidar/ARCHITECTURE.md`](../lidar/ARCHITECTURE.md). For dated
history of what was done, found, and decided, see `worklog.md` /
`worklog-archive.md`.

## Active: site rollout

The pipeline (reprojection through the vegetation height distribution
raster) is implemented and validated end-to-end for `rr` (Red River).
Active work is applying it to the remaining four sites.

| Site | Cloud pair (spring + summer) | ECPs (type=EVP) | Status |
|---|---|---|---|
| `rr` | 2022-05-14 / 2022-08-10 | 169 | Done |
| `wel` | 2022-05-20 / 2022-08-02 | 153 | **Ready, not started** — has everything the pipeline needs; larger clouds than `rr`'s, so Steps 0–2 will take longer (memory-bound). Best next candidate. |
| `oth` | none (photogrammetry only) | 145 | **Blocked** — no raw point cloud; ground-source comparison (photogrammetry + MassGIS only) already done. |
| `nor` | 2024-05-24 / 2024-10-06 | **0** | Blocked — no ECPs to validate against. |
| `peg` | 2024-04-19 / 2024-09-23 | **0** | Blocked — same reason as `nor`. |

`wel` also spans two non-overlapping MassGIS tiles
(`be_19TDG415636.tif`, `be_19TDG417636.tif`), so extending the
floor-bias approach there needs a mosaic step first — decided
(2026-09-11) to build a lightweight `.vrt` via `gdalbuildvrt`
(through `sf::gdal_utils()`) rather than materializing a merged
`terra::merge()` raster.

### `wel` rollout — concrete steps

Decisions locked in with the user (2026-09-11):
- **Execution**: run each stage here (background `Rscript` jobs),
  not handed to the user for RStudio.
- **CSF parameter selection**: pause after DTM evaluation and let the
  user review the report before hardcoding `best_csf_spring`/
  `best_csf_summer` into the downstream scripts (unlike `rr`, where
  lowest-RMSE was picked directly).
- **Multi-tile MassGIS mosaic**: new reusable `R/merge_massgis_tiles.R`
  (one function, `sf::gdal_utils(util = "buildvrt", ...)`), not inlined
  in `07_floor_corrected_ground.R` — multi-tile sites will likely
  recur (e.g. `bar`).

Steps:
- [ ] Add `R/merge_massgis_tiles.R`: takes a vector of tile paths +
  output path, calls `sf::gdal_utils("buildvrt", source = ,
  destination = )`, returns the `.vrt` path via `ensure_parent()`.
- [ ] Add `massgis_mosaic_vrt` template to `lidar/data/paths.yml`
  (leaf under `ground_raster_dir`, alongside `corrected_ground_raster`
  / `canopy_top_raster`).
- [ ] Run `lidar/02.R` for `wel` spring (`2022_05_20`) then summer
  (`2022_08_02`) — reprojection, clean & tile, CSF grid, ECP sampling.
- [ ] Add `wel` rows to `lidar/03_evaluate_dtm.R`'s `runs` data frame;
  run it; **hand the user the two HTML reports
  (`../lidar_reports/wel_2022_05_20/`, `wel_2022_08_02/`) and wait for
  their CSF pick** before continuing.
- [ ] Update `lidar/06_compare_ground_sources.R`'s existing `wel` block
  (currently just `photo_dem` + the two raw MassGIS tiles): add
  `lidar_spring`/`lidar_summer` `ground_raster` entries using the
  user's chosen CSF params (mirroring the `rr` block), and replace the
  two separate MassGIS tile entries with one `massgis` entry built from
  `merge_massgis_tiles()`. Run it.
- [ ] `lidar/07_floor_corrected_ground.R`: generalize the MassGIS
  lookup so a site can name >1 tile (e.g. a small `list(rr = ...,
  wel = c(...))` map at the top), building the mosaic only when a site
  has more than one. Set `site <- "wel"`, `date <- "2022_08_02"`; run.
- [ ] `lidar/08_veg_height_validation.R`: set `site <- "wel"`,
  `summer_date <- "2022_08_02"`, and update the hardcoded spring-DTM
  CSF params/date (currently `"2022_05_14"` + `rr`'s CSF values) to
  `wel`'s chosen spring CSF + `"2022_05_20"`. Run; report the
  opt2-vs-opt3 comparison back before finalizing which ground
  reference `05_veg_heights.R` should use (defaults to opt3, matching
  `rr`'s finding, unless `wel`'s numbers say otherwise).
- [ ] `lidar/05_veg_heights.R`: set `site <- "wel"`,
  `summer_date <- "2022_08_02"`; run to produce the final
  `veg_dist_0.5m.tif` deliverable.
- [ ] Lint any new/changed `.R` files before committing; update
  `dev/worklog.md` with timings/findings as stages complete; leave
  committing to the user's explicit ask.
- [ ] **Identify ground control points for `nor` and `peg`** — both
  currently have zero ECPs, blocking DTM validation and floor-bias
  estimation the same way it worked for the other three sites. Needs
  a source of field-surveyed elevation (and ideally `veg_height_m`)
  points before either site can proceed.
- [ ] `oth` stays blocked until a raw point cloud exists (or a
  decision is made to accept photogrammetry-only for that site).

## Paused

Two investigations are paused in favor of the site rollout above —
not abandoned, just deprioritized. Resuming either doesn't require
reopening this file first; go straight to the doc named.

- **UAS vertical elevation bias** (+10–16 cm vs. ECPs) — doesn't block
  relative vegetation heights (cancels on normalization), only
  absolute elevation deliverables. See `lidar/readme.md`'s "Systematic
  vertical bias..." section for current findings (lever arm cleared,
  boresight calibration is the leading hypothesis) and
  `dev/scan_angle_bias.md` for the next diagnostic step.
- **Canopy-top underestimate** (~15 cm vs. field `veg_height_m`) — see
  `lidar/readme.md`'s "Canopy-top underestimate..." section.

## Vegetation-strata output decisions (open, not blocking)

- [ ] **Fraction denominator.** Currently *all* returns in the cell
  (including below-ground/above-ceiling), so bands don't sum to
  100% — no firm answer yet on whether that's correct; not changing
  without a concrete reason (e.g. a downstream model that assumes
  bands sum to 1).
- [ ] **Raster resolution.** Default is 0.5 m per `lidar/readme.md`;
  revisit producing 0.25 m / 1 m alternatives now that a real first
  output is in hand.

## Out of scope (for now)

- Wrapping anything into a formal R package (`DESCRIPTION` /
  `NAMESPACE`).
- Touching the hydrology or logger-recalibration pipelines.
- Producing the final veg-modeling deliverables that consume these
  rasters — that's a separate downstream project.
