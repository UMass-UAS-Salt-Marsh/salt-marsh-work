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

This is a big, multi-part change: Parts A–D (scratch-tree cleanup,
naming-convention rework, migrating every ad hoc path onto
`pathtools`, and the output metadata sidecar) are **done** — see
`dev/worklog.md` (2026-09-11 through 2026-09-14) for full detail on
what was decided and changed. Remaining work is Part E (the clean
re-run).

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
creation date and the paths of its inputs.
Complete. See log for details.

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

