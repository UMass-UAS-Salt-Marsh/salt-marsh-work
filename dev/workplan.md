# Work plan — lidar elevation & vegetation strata

This file tracks only the **currently active, still-open** work.
For how the pipeline works (stages, functions, data flow), see
[`lidar/ARCHITECTURE.md`](../lidar/ARCHITECTURE.md). For dated
history of what was done, found, and decided, see `worklog.md` /
`worklog-archive.md`.

The site-rollout plan is sidelined for now in `dev/rollout_workplan.md`.

## Active: scratch-tree cleanup, path-scheme rework, output metadata, then a clean `rr` re-run

Goal: clean up the real accumulated cruft in `E:/uas_scratch/lidar` and
`../lidar_reports`, move every remaining hardcoded/ad hoc path onto
`pathtools`, settle a few naming-convention questions the current
scheme never resolved, add a metadata sidecar next to each *final*
output raster, then re-run the whole `rr` pipeline cleanly end-to-end
— both to validate the reworked scheme and to have a known-clean
reference output before rolling out to the other sites.

This is a big, multi-part change: Parts A–C (scratch-tree cleanup,
naming-convention rework, migrating every ad hoc path onto
`pathtools`) are **done** — see `dev/worklog.md` (2026-09-11 through
2026-09-14) for full detail on what was decided and changed. Remaining
work is Part D (the metadata sidecar — not yet started) and Part E
(the clean re-run, blocked on D).

**Part A** (worklog 2026-09-11/12): audited and cleaned `rr`'s
scratch tree, 47 GB → 21 GB (deleted pre-EPSG-6491-migration
duplicates, all 8 CSF-tuning DTMs, outputs the re-run will regenerate
anyway; archived current reports to `../lidar_reports/archive/` for
post-re-run comparison — **still open**, carried into Part E: once
the re-run produces fresh reports, prompt to compare against the
archive and delete it if it looks good). `nor`'s entire scratch tree
(~7.4 GB) turned out to be legacy wrong-CRS data and was deleted too
(worklog 2026-09-12/14). `oth`/`wel`/`peg`/legacy `bar` scratch trees
haven't been audited — out of scope here, revisit as the rollout
reaches each site.

**Part B** (worklog 2026-09-12): naming decided and implemented —
keep `target_epsg` in `cleaned_tiles`/`ground_raster_dir` (also record
it in the Part D metadata), keep `date` over `season` as the path key,
drop the `zzz` prefix (`cleaned_tiles` → `cleaned_epsg{target_epsg}`,
`ground_raster_dir` → `ground_rasters_epsg{target_epsg}`,
`veg_heights_dir` → `veg_heights`). `rr`'s on-disk directories renamed
to match.

**Part C** (worklog 2026-09-14): migrated the remaining ad hoc
`file.path()`/`paste0()` paths onto `pathtools` — 11 new `paths.yml`
leaves (`reprojected_las`, `ground_raster_ecp_cache`,
`corrected_ground_raster_ecp_cache`, `canopy_top_raster_ecp_cache`,
`ground_comparison_report_html`, `ground_source_comparison_csv`,
`ground_comparison_ecp_cache`, `floor_bias_plot`,
`veg_height_validation_csv`, `dtm_eval_report_html`,
`dtm_eval_summary_csv`) plus `orig_stem`/`source_name`/`season`
parameters, with every call site updated
(`lidar/02.R`, `R/report_dtms.R`, `R/report_ground_sources.R`,
`lidar/07_floor_corrected_ground.R`, `lidar/08_veg_height_validation.R`,
both Rmd report templates). Verified byte-identical to the old ad hoc
paths (pure refactor). One deliberate exception left as-is:
`rmd/dtm_evaluation_report.Rmd`'s generic per-DTM loop still relies on
`sample_dtm()`'s default `.tif`→`_ecp.csv` derivation rather than an
explicit `get_path()` call, since it iterates over whatever DTMs exist
without individual CSF params in hand — happens to produce the same
filename regardless.

### Part D — Metadata sidecar for final output rasters

Every *final* output raster gets a sidecar file recording its
creation date and the paths of its inputs. Intermediate/derived
artifacts (cleaned tiles, reprojected `.las`, CSF-grid DTMs, ECP
caches, reports) are explicitly **not** required to have one — only:

- `ground_raster` (or at least the "winning" CSF picks, once chosen)
- `corrected_ground_raster`
- `canopy_top_raster`
- `veg_heights_raster`
- `veg_return_count_raster`

Shape (decided, via `yaml::write_yaml()` — already an indirect
dependency through `pathtools`' own scheme-file parsing, so no new
package):
- **Format**: one small YAML file per output, same directory, same
  stem — e.g. `veg_dist_0.5m.tif` → `veg_dist_0.5m.yaml`.
- **Contents**:
  - `created_at` — timestamp.
  - `output` — its own path (so a sidecar is self-describing if it
    ever gets copied/moved independently of context).
  - `crs` — the target EPSG code / CRS used (per Part B.1's decision
    to record this here too).
  - `source_cloud` — path to the *original raw* point cloud this
    output ultimately derives from (i.e. `get_path("raw_lidar", site
    = , date = )` — the very first input to the whole pipeline, not
    just this step's immediate upstream file), so provenance is
    traceable all the way back even from a downstream output like
    `veg_heights_raster` whose immediate input is a cleaned-tiles
    directory, not the raw cloud itself.
  - `inputs` — named list of the *immediate* input paths (e.g. for
    `veg_heights_raster`: cleaned-tiles dir, ground-reference DTM
    path; for `ground_raster`: cleaned-tiles dir; for
    `corrected_ground_raster`: MassGIS tile path + the ECP cache CSVs
    the floor-bias estimate was fit from).
  - `params` — whatever scalar parameters actually varied the output
    (CSF params, bin breaks, raster resolution) — not a full
    session/package-version dump, that's more than what was asked
    for.
- **Mechanism**: a single small generic helper, `R/write_output_metadata.R`
  (not yet created), taking `output`, `source_cloud`, `crs`, `inputs`,
  `params = list()` — writes the YAML, doesn't know about `pathtools`
  or the lidar scheme itself (just takes already-resolved paths).
  **Refinement found while reading the actual producer code**: don't
  call it *from inside* `rasterize_ground()`/`rasterize_canopy_top()`/
  `rasterize_veg_heights()` — those stay generic, reusable
  point-cloud-processing functions with no reason to know about
  provenance bookkeeping (they don't even receive `source_cloud`/`crs`
  as parameters today, and `corrected_ground_raster` has no dedicated
  function at all — it's built inline in `07_floor_corrected_ground.R`).
  Instead call `write_output_metadata()` explicitly right after each
  producing call, **in the 4 driver scripts**: `lidar/02.R` (after
  each `rasterize_ground()` call in the CSF loop — write one for every
  grid combo, not just the winners, for uniformity/simplicity),
  `lidar/07_floor_corrected_ground.R` (after its inline
  `terra::writeRaster()`), `lidar/08_veg_height_validation.R` (after
  `rasterize_canopy_top()`), `lidar/05_veg_heights.R` (after
  `rasterize_veg_heights()` — one shared sidecar for both
  `veg_heights_raster`/`veg_return_count_raster`, so `output` should
  accept a vector of paths, sidecar filename derived from the first).
- [ ] Confirm the 5-output list above (`ground_raster` — all CSF grid
  combos, not just winners — `corrected_ground_raster`,
  `canopy_top_raster`, `veg_heights_raster`, `veg_return_count_raster`)
  is the right scope — nothing more, nothing less.
- [ ] Write `R/write_output_metadata.R` (roxygen-documented, one
  function per file per convention) and wire in the 4 driver-script
  call sites above.

### Part E — Clean `rr` re-run

Once Parts A–D land:
- [ ] Apply the disposition decisions from Part A.
- [ ] Re-run `lidar/02.R` → `03_evaluate_dtm.R` →
  `06_compare_ground_sources.R` → `07_floor_corrected_ground.R` →
  `08_veg_height_validation.R` → `05_veg_heights.R` for `rr`, end to
  end, against the new path scheme.
- [ ] Confirm every output (including the new metadata sidecars)
  lands where expected and looks sane, the same way the return-count-
  raster change was verified (`terra` inspection: band names/counts,
  datatypes, dims).
- [ ] Update `lidar/ARCHITECTURE.md` and the driver scripts' header
  comments to reflect the final path scheme.
- [ ] **Once the re-run's reports exist**, prompt the user to compare
  `../lidar_reports/rr_2022_05_14/`, `rr_2022_08_10/`,
  `rr_ground_comparison/` against their archived counterparts under
  `../lidar_reports/archive/` (see Part A) — if everything looks
  good, delete the `archive/` copies.

