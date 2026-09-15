# Worklog

Running log of changes made in this repo: code edits, doc edits, config
tweaks, exploratory findings, memory/settings updates, build fiddling.
More detailed than any user-facing changelog — include enough context
that a reader can reconstruct what happened without diffing.

Newest entries on top. Group by `## YYYY-MM-DD — branch <name>`
headers, then `### <short scope>` sub-headers under them. If today's
date and current branch already have a heading, append a new
sub-heading rather than starting a new date block.

Older entries get moved to `worklog-archive.md` once this file gets
long (~500 lines) or the oldest entry is more than ~30 days old. Move
whole date blocks, never split one. Read this file first when recalling
history; consult the archive only if the answer isn't here.

---

## 2026-09-15 — branch lidar

### Added a memory pre-flight check to `lidar/02.R`

Triggered by a real OOM during a Part E clean re-run of `rr`: another
user's Metashape process was contending for memory on the shared
machine, and `rasterize_ground()`'s `catalog_map()` failed on two
chunks (`std::bad_alloc`, "cannot allocate vector of size 238.5 Mb")
without halting the CSF tuning loop, leaving a corrupted DTM
(`csf_th0.005_res0.05_rgd2_0.25m.tif`) plus its metadata sidecar. The
user killed stray processes, deleted the garbage output, and finished
that run manually with `workers` dropped from 20 to 10 (`lidar/02.R`
now reflects `workers = 10`) while this check was being built.

New `R/` helpers (roxygen-documented, no `pathtools` coupling):

* `R/worker_memory_reference.R` — `worker_memory_reference` data
  frame, one calibration row per process (`chunk_size`,
  `chunk_buffer`, `density`, observed peak `memory_mb`). Currently one
  row: `rasterize_ground` at `chunk_size = 200`, `chunk_buffer = 20`,
  `density = 243.65` (rr spring cleaned catalog), peak `11498` MB —
  the largest of several observed values, all below 12,000 MB, from
  the user's manual run. Add a row per process as other scripts adopt
  this check.
* `R/estimate_worker_memory_mb.R` — `estimate_worker_memory_mb(process,
  chunk_size, chunk_buffer, density, ref_* = NA)`. Assumes memory
  scales linearly with buffered tile area (`(chunk_size + 2 *
  chunk_buffer)^2`) times point density, scaling from the reference
  row looked up by `process` (any `ref_*` arg can be overridden
  explicitly, e.g. for testing).
* `R/get_available_memory_mb.R` — free system memory in MB via
  PowerShell's `Get-CimInstance Win32_OperatingSystem` (`wmic` is
  deprecated/unreliable on this Windows Server version).
* `R/check_worker_memory.R` — `check_worker_memory(workers,
  expected_worker_memory_mb, safety_margin = 0.25)`. Hard-stops via
  `stop()` if available memory is below `workers *
  expected_worker_memory_mb * 1.25`; the error also reports the
  maximum `workers` count the current available memory could support
  at that margin, rather than just failing.

Wired into `lidar/02.R` right after `clean_and_tile()` returns (now
captured as `cleaned_ctg` instead of discarded) and before the CSF
tuning loop starts — this is the earliest point where the real
point density is known from the actual cleaned tiles, without a
second catalog read. Verified: `estimate_worker_memory_mb(
"rasterize_ground", chunk_size = 200, chunk_buffer = 20, density =
243.65)` reproduces `11498`; `get_available_memory_mb()` matches
`Get-CimInstance Win32_OperatingSystem` run directly; a deliberately
absurd `check_worker_memory(workers = 999, ...)` call stops with a
correct max-workers figure; `workers = 10` (current `02.R` default)
passes cleanly against current free memory. `lintr::lint()` run on
all 4 new files plus `lidar/02.R` — one indentation hit in
`estimate_worker_memory_mb.R`, fixed.

Not touched: `lidar/05_veg_heights.R` and
`lidar/08_veg_height_validation.R` also use `future::multisession` and
could adopt the same check later; out of scope for this change.

## 2026-09-14 — branch lidar

### Implemented Part D: metadata sidecar for final output rasters

Added `R/write_output_metadata.R` (new, roxygen-documented): takes
already-resolved `output` path(s), `created_by`, `source_cloud`, `crs`,
`inputs`, and `params = list()`, and writes a YAML sidecar next to
`output[1]` (same dir, same stem, `.yaml` extension). Skip-if-exists
like `reproject_las()`/`clean_and_tile()`/`sample_dtm()`, so it's safe
to call unconditionally after a producing step even when the driver
script itself skipped recomputing the raster. Fields, in order:
`created_at`, `created_by` (the producing function's name, or the
driver script's path when the output is built inline with no dedicated
function), `output`, `crs`, `source_cloud`, `inputs`, `params`. Stays
generic — no `pathtools`/lidar-scheme knowledge, per the refinement
noted in `dev/workplan.md`. `yaml` confirmed already installed
(indirect dependency via `pathtools`) — no new package.

Wired into the 4 driver-script call sites named in the workplan, each
call placed after the raster's own skip-if-exists block so a missing
sidecar gets backfilled even against a pre-existing raster:

- `lidar/02.R` — one sidecar per CSF grid combo in the tuning loop
  (all combos, not just winners). Added `raw_cloud_path`, captured
  before `paths$input` gets reassigned to the reprojected path, so
  `source_cloud` always traces back to the true original raw cloud
  regardless of whether reprojection ran.
- `lidar/07_floor_corrected_ground.R` — `inputs` records both
  `lidar_spring_ecp_cache` and `lidar_summer_ecp_cache` even though
  only `floor_summer` feeds the output formula (confirmed with Ethan);
  `created_by` is the script's own path since this output has no
  dedicated producing function.
- `lidar/08_veg_height_validation.R` — after `rasterize_canopy_top()`.
  `params` records `top_percentile`/`raster_res` explicitly even
  though the call uses both as defaults, since `canopy_top_raster`'s
  path template embeds no parameters — this sidecar is the only place
  they're ever recorded.
- `lidar/05_veg_heights.R` — one shared sidecar for
  `veg_heights_raster` + `veg_return_count_raster` (`output` takes
  both paths, sidecar filename derived from the first). `params`
  likewise records the default `bin_breaks` explicitly for the same
  reason as `top_percentile` above.

Verified: `R/write_output_metadata.R` and all 4 edited driver scripts
parse cleanly; manually ran `write_output_metadata()` against dummy
data to confirm the skip-if-exists behavior and YAML shape, and against
the real `rr` summer paths (resolved via `pathtools`, written to a temp
file rather than the real — currently raster-less — `veg_heights/`
directory) to preview the exact sidecar `05_veg_heights.R` will produce
once Part E's re-run happens. `lintr::lint()` clean on all 5 touched
files (one hanging-indent hit in `lidar/05_veg_heights.R` fixed).

### Implemented Part C: migrated every ad hoc path onto pathtools

Fixed a typo in `lidar/readme.md` ("vegitation" -> "vegetation",
introduced in an unrelated stray edit) while here.

Per `dev/workplan.md`'s Part C table. Added 11 new leaves to
`lidar/data/paths.yml` (`reprojected_las`, `ground_raster_ecp_cache`,
`corrected_ground_raster_ecp_cache`, `canopy_top_raster_ecp_cache`,
`ground_comparison_report_html`, `ground_source_comparison_csv`,
`ground_comparison_ecp_cache`, `floor_bias_plot`,
`veg_height_validation_csv`, `dtm_eval_report_html`,
`dtm_eval_summary_csv`) plus three new parameters (`orig_stem`,
`source_name`, `season: [spring, summer]`).

Updated every call site to resolve these via `get_path()` instead of
`file.path()`/`paste0()`: `lidar/02.R` (reprojected-cloud path, and the
per-CSF-grid ECP-cache loop — now passes `output_csv` explicitly
instead of relying on `sample_dtm()`'s default `.tif`->`_ecp.csv`
derivation), `R/report_dtms.R` and `R/report_ground_sources.R`
(`output_file` defaults), `rmd/dtm_evaluation_report.Rmd` and
`rmd/ground_source_comparison.Rmd` (summary CSVs, per-source ECP
caches), `lidar/07_floor_corrected_ground.R` (floor-bias PNGs, the
`lidar_spring`/`lidar_summer` ECP cache reads — also dropped its now-
unused `output_dir` variable), `lidar/08_veg_height_validation.R`
(`corrected_ground_raster`/`canopy_top_raster` ECP caches,
`veg_height_validation.csv`).

Deliberately left `rmd/dtm_evaluation_report.Rmd`'s generic per-DTM
loop relying on `sample_dtm()`'s default derivation (see
`dev/workplan.md` Part C note) — it iterates over whatever `.tif`s
exist in a directory without individual CSF params in hand, and the
default derivation already produces the identical filename.

Verified with a side-by-side script: every migrated leaf reproduces
the *exact* path the old ad hoc code computed (11/11 `identical()`
checks passed) — confirms this is a pure refactor, no behavior change.
`lintr` on all 5 changed `.R` files (fixed two hanging-indent lints
along the way): no lints. Checked for leftover stale cache files
(`_ecp.csv` sidecars under `ground_rasters_epsg6491/`, and any
non-archived `rr_*` report directories) before starting Part D —
found none; everything relevant was already cleared or archived in
the 2026-09-12 disposition pass. Working-tree changes only — not
committed.

### Fixed sites_data_root (uas -> uas_veg rename) and cleared nor's legacy scratch data

User fixed `lidar/data/paths.yml`'s `sites_data_root` root directly:
`X:/projects/uas/sites` -> `X:/projects/uas_veg/sites` (one line,
fixes every path built from it) — the `X:/projects/uas` directory had
been renamed to `X:/projects/uas_veg`, which is why `nor`'s
`raw_lidar` override didn't resolve when checked 2026-09-12 (see that
entry below). Verified afterward: `raw_lidar` now resolves to an
existing file for `nor`/`2024_10_06`, `nor`/`2024_05_24`,
`wel`/`2024_06_01`, `peg`/`2024_04_19`, and `peg`/`2024_09_23` — every
override that used `{@sites_data_root}`.

User also manually deleted `nor`'s entire legacy scratch tree
(`E:/uas_scratch/lidar/nor`, ~7.4 GB — confirmed 2026-09-12 to be
uniformly in the wrong CRS, EPSG:32619 instead of the project's
EPSG:6491 standard) after Claude Code's auto-mode classifier blocked
a `rm -rf` on the whole site directory as "Irreversible Local
Destruction." Confirmed afterward: `nor/` is now an empty leftover
directory (0 bytes), safe to leave or remove.

## 2026-09-12 — branch lidar

### Implemented Part B's zzz-drop rename (paths.yml + rr's directories)

Applied the already-decided `dev/workplan.md` Part B naming change:
dropped the `zzz` prefix from the three affected `lidar/data/paths.yml`
templates —

- `cleaned_tiles`: `zzzcleaned_epsg{target_epsg}` → `cleaned_epsg{target_epsg}`
- `ground_raster_dir`: `zzzraster_epsg{target_epsg}` → `ground_rasters_epsg{target_epsg}`
- `veg_heights_dir`: `zzzheights` → `veg_heights`

(`target_epsg` itself stays, per Part B's decision.) This is a global
scheme change — before touching it, checked whether any *other* site
had real data under the old names that would go unreachable: `nor`
has 30 cleaned tiles + 4 CSF DTMs under `2024_10_06/zzzcleaned`,
`zzzraster` (no `_epsg` suffix at all) and a separate
`2024_10_06__cs400/` experiment directory, but all of that was
*already* unreachable under the pre-rename scheme too (it predates
the `target_epsg`-suffix convention entirely) — so this rename didn't
newly orphan anything. `oth`/`wel`/`peg`/`bar` have no scratch
directories yet. `nor`'s legacy clutter is separately noted in
`dev/workplan.md` Part A as out of scope for now.

Renamed `rr`'s on-disk directories to match, both dates:
`zzzcleaned_epsg6491` → `cleaned_epsg6491` (real content — the kept
cleaned tiles, 20 `.las` files per date) and `zzzraster_epsg6491` →
`ground_rasters_epsg6491` (empty, after yesterday's cleanup);
`2022_08_10/zzzheights` → `2022_08_10/veg_heights` (also empty).
Verified with `pathtools::get_path()` directly afterward:
`cleaned_tiles` for `rr`/`2022_08_10` resolves to the renamed
directory and still finds all 20 tiles — the upcoming clean re-run
will correctly skip `clean_and_tile()` rather than regenerate from
the raw cloud.

### Cleaned up rr's scratch tree (Part A of the path-rework plan)

Per `dev/workplan.md`'s "scratch-tree cleanup, path-scheme rework,
output metadata" plan (drafted 2026-09-11, disposition decided
2026-09-12). Deleted, in order:

1. All 8 CSF-tuning-grid DTMs + their `_ecp.csv` caches, both dates
   (`zzzraster_epsg6491/csf_th*.tif`/`*_ecp.csv`) — ~86 MB. Only 2 of
   the 8 fed anything downstream; cheap to regenerate from the kept
   cleaned tiles.
2. The 4 outputs the upcoming clean re-run will regenerate anyway
   (`massgis_plus_floor_summer.tif`, `canopy_top_summer.tif`,
   `veg_dist_0.5m.tif`, `return_counts_0.5m.tif`) — ~53 MB. Deleted
   outright rather than relying on `overwrite = TRUE`, for a genuinely
   clean slate.
3. Legacy pre-EPSG-6491-migration dead weight: non-`_epsg`-suffixed
   `zzzcleaned/`/`zzzraster/` dirs (both dates), `_epsg26919_navd88.las`
   reprojected copies (both dates), and an orphaned
   `reproject_las_lastools.R` intermediate
   (`*_lastools_xy_49806f281607.las`) — **~27 GB**, the bulk of the
   total. All fully superseded by the `_epsg6491` tree.
4. Legacy `_epsg26919` report directories under `../lidar_reports/`
   (`rr_2022_05_14_epsg26919/`, `rr_2022_08_10_epsg26919/`,
   `rr_ground_comparison_epsg26919/`) — ~8.6 MB.

`rr`'s `E:/uas_scratch/lidar/rr` tree: **47 GB → 21 GB**. The kept
`zzzcleaned_epsg6491/` tiles (both dates) were untouched throughout.

Also **archived** (moved, not deleted) the current reports —
`../lidar_reports/rr_2022_05_14/`, `rr_2022_08_10/`,
`rr_ground_comparison/` — into a new `../lidar_reports/archive/`
subdirectory (same names), so they survive the upcoming clean re-run
for a side-by-side comparison. Once the re-run produces fresh reports
at the original paths, the user is to be prompted to compare against
`archive/` and, if it looks good, delete the archived copies (tracked
in `dev/workplan.md` Part E).

All actions were plain `rm`/`mv` against `E:/uas_scratch/lidar` and
`../lidar_reports` (outside the repo) — nothing under version control
changed.

### Ran lidar/05_veg_heights.R for rr to test the return-count raster

Real end-to-end test (not the synthetic dry run from the earlier
implementation entry below) of the return-count-raster change,
against `rr`'s existing cleaned tiles (104.82M points, 20 files) and
corrected ground raster — both already present, so Step 1
(`clean_and_tile()`) skipped and the run went straight to
`rasterize_veg_heights()`. 20 catalog chunks, ~3.3 minutes wall time
(16:46:40–16:50:00), exit code 0.

Output checked with `terra`: `veg_dist_0.5m.tif` — 31 bands, names
`hn05_005 ... h300_Inf` as expected; `return_counts_0.5m.tif` —
single `n_returns` band, `datatype = "INT4S"`, same grid as the
fraction raster, values 1-8374 returns/cell, no negatives/NAs where
populated. Spot-checked one cell: fractions summed to 1 exactly at
`n_returns = 2`, i.e. both of that cell's returns landed inside the
bin range (expected — nothing below the -5 cm floor or NA-DTM-derived
noise there).

Only warnings were the pre-existing, benign `pathtools`
`when_value_unchecked` ones (unconstrained `date` parameter, same ~20
seen since the pathtools migration) and three `normalize_height()`
"points do not belong in the raster" nearest-neighbor warnings —
unrelated to this change, inherent to DTM normalization.

This overwrote the existing scratch `veg_dist_0.5m.tif` (regenerated
with the new bin scheme + naming) and created `return_counts_0.5m.tif`
for the first time. Scratch-tree output only, nothing under version
control changed by this run.

### Added a per-cell return-count raster to rasterize_veg_heights()

Per the plan in `dev/workplan.md`. `rasterize_veg_heights()` now
writes a second, single-band GeoTIFF (`count_output`, a new required
parameter) recording `n_returns`: how many returns landed in each
cell — the same `length(Z)` value the height-bin fractions are
already divided by. Deliberately a separate file, not an extra band
on the 31-band fraction raster.

**`R/rasterize_veg_heights.R`.** The `pixel_metrics()` formula (built
via `bquote()`) now evaluates
`c(bin_fractions(Z, ...), list(n_returns = length(Z)))` instead of
just `bin_fractions(...)` — added at this call site rather than
inside `bin_fractions()` itself, keeping that function focused on
fractions only. After `catalog_map()` assembles the merged `result`
raster (31 fraction bands + `n_returns`), it's split by band name via
`terra::subset()`: the fraction bands go to `output` as before, and
the `n_returns` band goes to `count_output`, written with
`datatype = "INT4S"` (counts are always non-negative whole numbers,
so an explicit integer type is smaller and unambiguous vs. the
default float). Function now returns `c(output, count_output)`
invisibly instead of just `output`. Updated roxygen docs/`@examples`
to match.

**`lidar/data/paths.yml`.** Added `veg_return_count_raster` template
(`{@veg_heights_dir}/return_counts_{raster_res}m.tif`), alongside the
existing `veg_heights_raster`.

**`lidar/05_veg_heights.R`.** Resolves `veg_return_count_raster` and
passes it as `count_output`; updated the header-comment outputs list
(also fixed a stale "30-band" reference left over from the earlier
height-bin-adjustment task — should have said 31-band since that
change).

**`lidar/ARCHITECTURE.md`.** Updated the vegetation-height-distribution
section (30-band → 31-band, added the count-raster output) and the
Mermaid data-flow diagram (`W` node for the new count raster, added to
the `data` node-class list).

Verified the formula deparse/reparse round-trip, per-pixel metric
evaluation, band splitting by name, and the `INT4S` write against a
synthetic multi-layer `terra::rast()` standing in for the merged
catalog result (matches `bin_names` + `n_returns` naming). `lintr` on
all three changed R files: no lints. Working-tree changes only — not
committed, per instructions not to commit unless asked.

### Adjusted height-strata bins (floor + open-ended top bin)

Changed the default height bins used by `rasterize_veg_heights()` /
`bin_fractions()` to classify the fraction of returns by height
strata, per the plan in `dev/workplan.md` (site-rollout work is
sidelined in `dev/rollout_workplan.md` while this and one other
cleanup task are done first).

**`R/rasterize_veg_heights.R`.** Default `bin_breaks` changed from
`c(seq(0, 1, by = 0.05), seq(1.2, 3.0, by = 0.20))` (30 bins, 0–3 m)
to `c(-0.05, seq(0.05, 1, by = 0.05), seq(1.2, 3.0, by = 0.20), Inf)`
(31 bins, -5 cm–3 m plus an open-ended top bin). The lowered floor
(-5 cm) catches near-zero returns that are slightly negative (ground
jitter/noise close to zero) while still excluding larger negative
values as errors; the new top bin (`3.0` to `Inf`) counts returns
above the old ceiling instead of silently dropping them from every
band's numerator. Band-name generation (`sprintf("h%03d_%03d", ...)`)
couldn't handle a negative edge or `Inf` directly — `sprintf("%03d",
Inf)` errors, and a literal `-` prefix would make the bin name an
invalid, unquoted R name — so replaced it with a per-edge
`format_edge()` helper (`vapply`'d over `low_cm`/`high_cm`) that emits
`n05` for a -5 cm edge and `Inf` for the open-ended edge. Removed an
unused `ceiling_ht <- bin_breaks[n_bins + 1L]` line found dead in the
same block (only `bin_fractions()`'s own copy is actually used).
Updated the function's roxygen docs (`@param bin_breaks`, `@details`)
to match.

**`R/bin_fractions.R`.** Generalized the floor filter from the
hardcoded `z >= 0` to `z >= bin_breaks[1]`, so it respects whatever
floor the caller's `bin_breaks` sets rather than assuming zero.

**Naming convention** (decided with the user, 2026-09-11): edges use
`n` for a negative sign (e.g. `hn05_005`) rather than a literal `-`,
since a `-` would make the resulting list/column name invalid without
backtick-quoting; `n05` is also the same 3-character width as `005`,
so bin names stay visually aligned. Confirmed the first-3/last-3
resolved names: `hn05_005`, `h005_010`, `h010_015`, ...,
`h260_280`, `h280_300`, `h300_Inf`.

Verified interactively (`.bincode()` behavior with `-Inf`/`Inf`-ish
breaks, `format_edge()` outputs, `make.names()` on the full
`bin_names` vector, and `bin_fractions()` against a small sample `z`
vector spanning below-floor/first-bin/last-bin/above-ceiling values)
before linting. `lintr::lint()` on both changed files: no lints.
Working-tree changes only — not committed, per instructions not to
commit unless asked.

### Implemented dev/path_lookup_plan.md (v2, pathtools-based)

Implemented the revised plan (see the "Revised path_lookup_plan.md"
entry below, same day) end to end. Working-tree changes only — not
committed, per instructions not to commit unless asked.

**Config file.** Created `lidar/data/paths.yml`, a `pathtools` path
scheme, replacing `lidar/data/paths.csv` (git rm'd) and
`R/update_path.R` (deleted — zero remaining callers). Beyond the plan's
sketch, added a composition chain (`site_date_root` ->
`{cleaned_tiles, reprojected_dir, ground_raster_dir, veg_heights_dir}`
-> leaves, mirroring `pathtools`'s own `out_root`/`out_dir`/
`scenario_dir` example) and two entries the plan's inventory hadn't
enumerated: `corrected_ground_raster` (`massgis_plus_floor_summer.tif`)
and `canopy_top_raster` (`canopy_top_summer.tif`), both produced by
`lidar/07_floor_corrected_ground.R` and consumed by `05_veg_heights.R`
/ `08_veg_height_validation.R`. Declared `parameters.site: [rr, oth,
wel, nor, peg, bar]` (enumerable now, silences those particular
warnings); `date`/`tile`/`source`/CSF params stay unconstrained per the
plan's open question 2. Dates are now encoded underscored
(`"2022_08_10"`) throughout, including in `raw_lidar`'s `when:`
clauses — this drops the old ISO-hyphenated-to-underscored conversion
step that used to live in `lidar/02.R` and `05_veg_heights.R`, since
every other template already used the underscored form. Validated the
scheme directly against the installed `pathtools` package
(`path_scheme()`, `get_path()`, `explain_path()`, `ensure_parent()`)
before touching any call site; it builds with ~20 benign
`when_value_unchecked` warnings (unconstrained parameters used in
`when:` clauses can't be checked for a raw-vs-formatted mismatch) —
expected, matches the plan's deferral of full parameter enumeration.

**Call-site migration.** Updated every call site the plan's research
docs inventoried: `lidar/02.R`, `03_evaluate_dtm.R`,
`05_veg_heights.R`, `06_compare_ground_sources.R`,
`07_floor_corrected_ground.R`, `08_veg_height_validation.R`;
`R/report_dtms.R` (renamed its `base_output` parameter to `dtm_dir` to
match what it actually is — the `zzzraster_epsg<N>/` directory,
already fully resolved rather than reconstructed inside the Rmd) and
`R/report_ground_sources.R`; `R/reproject_las.R`'s `vgrid` default. Also
found and fixed two roxygen-example-only hardcoded paths not in the
original inventory: `R/estimate_floor_bias.R` and
`R/rasterize_canopy_top.R`. All of `R/las_needs_reprojection.R`,
`R/load_ecp.R`, `R/rasterize_ground.R`, `R/rasterize_veg_heights.R`,
`R/sample_dtm.R`, `R/reproject_las_pdal.R`, `R/reproject_las_lastools.R`
had their `@examples` updated to demonstrate `pathtools::get_path()`
instead of literal strings. Each driver script now does
`library(pathtools); set_path_scheme("lidar/data/paths.yml")` right
after its existing `R/`-sourcing line.

**Rmd templates.** `rmd/dtm_evaluation_report.Rmd` and
`rmd/ground_source_comparison.Rmd`: `ecp_path`/`dtm_dir` (renamed from
`base_output`)/`eval_dir`/`output_dir` params now default to `NULL` in
the YAML header and resolve via `get_path()` with a `%||%` fallback in
the setup chunk (base R's null-coalescing operator, available since R
4.4.0 — this repo runs 4.5.2, and nothing else uses it yet, so no style
inconsistency introduced). `ground_source_comparison.Rmd`'s
standalone-knit fallback `sources` list (the `oth` site) now calls
`get_path("photo_dem", ...)` / `get_path("massgis_tile", ...)` instead
of duplicating literal paths.

**Reports move.** Per the user's explicit choice (this session's
pipeline wasn't re-run — needs `E:` scratch data), copied
`lidar/output/`'s 72 files (54 previously tracked + 18 untracked, some
under stale `_epsg26919`-suffixed directories from before the EPSG:6491
migration) as-is, preserving names, to a new sibling
`../lidar_reports/` directory. Then `git rm -r --cached lidar/output/`
and deleted the now-empty tree from the repo. Nothing was lost; the
stale `_epsg26919` content moved over unchanged rather than being
cleaned up (out of scope for this migration).

**Documentation.** `CLAUDE.md` (project) and `lidar/ARCHITECTURE.md`
rewritten to describe `paths.yml`/`get_path()` instead of
`paths.csv`/`update_path()`; both now note `pathtools`
(<https://github.com/ethanplunkett/pathtools>) as the repo's first
non-CRAN dependency (`remotes::install_github("ethanplunkett/pathtools")`).
`lidar/readme.md`'s ECP filename discrepancy (flagged as open question
1 in the v1 plan) is resolved: every live code path agreed on
`..._One_Sheet.xlsx`, so `readme.md`'s `..._Meta_Datapoints.xlsx` was
corrected to match.

**Cleanup.** Deleted the dead `if (FALSE) {...}` scratch block at the
top of `R/clean_and_tile.R` (per the plan) and, since that file was now
touched, fixed its pre-existing lint issues (trailing whitespace,
indentation, an explicit `return()`, a couple of >80-char lines) —
these predated this session, not introduced by it.

**Verification.** All 19 changed `.R` files are lint-clean
(`lintr::lint()` against the project's `.lintr`) and parse
(`parse()`); both changed `.Rmd` files parse after `knitr::purl()`.
`grep` for `E:/uas_scratch`, `X:/legacy`, `X:/projects/uas/sites`,
`X:/scratch/bcompton`, `lidar/output`, `update_path(` across `lidar/`,
`R/`, `rmd/` returns nothing outside `paths.yml` and a small number of
descriptive (non-executable) header-comment mentions of the `E:`
scratch-tree layout, which also existed pre-migration.

**Not done in this session:** no pipeline stage was actually re-run
(would need `E:` scratch data + `lidR`, `pdal`, etc.); reports will
regenerate fresh under `../lidar_reports/` the next time each driver
script runs. Nothing was committed.

### Revised path_lookup_plan.md to build on the `pathtools` package

Revised `dev/path_lookup_plan.md` to v2, replacing the bespoke
`lookup_paths()` function and custom `roots:`/`templates:`/`registry:`
YAML schema (proposed in v1, 2026-09-02) with the `pathtools` R
package (<https://github.com/ethanplunkett/pathtools>), a side project
now installed locally (version 0.1.0, commit `aa740f4`) that solves
exactly this class of problem: parametric path management from YAML,
with named roots, `{@ref}`-style composition, per-parameter-combination
`overrides:`, and `audit_paths()`/`ensure_parent()` helpers. No code
changes yet — this was a planning-only session; implementation is
still pending in a future session against the revised plan.

Key design decisions locked in with the user before rewriting:
- Config file stays at `lidar/data/paths.yml` (replacing `paths.csv` in
  place), using `pathtools`'s native `paths:`/`parameters:` schema
  rather than a custom one.
- No machine-local overlay file added now — but every root
  (`scratch_root`, `legacy_data_root`, `sites_data_root`,
  `massgis_root`, `reports_root`) is its own top-level entry
  specifically so that adding `lidar/data/paths-local.yml` later (if
  `X:`/`E:` ever differ across machines) is a two-line change, not a
  redesign.
- Superseded `paths.csv` rows (the old `preferred = 0` rows, e.g. an
  earlier `ppk_cloud_1.las` alongside the current
  `ppk_07Nov2022_cloud_1.las` for the same site/date) are kept as YAML
  comments immediately under the entry that replaced them, rather than
  dropped outright or made into resolvable entries.
- `R/update_path.R` (the `[name]`-placeholder substitutor, one real
  call site at `lidar/02.R:235`) is slated for deletion once the
  migration leaves it with zero callers — `pathtools`'s glue-based
  `{name}`/`{@ref}` templating fully supersedes it.
- The `lidar/readme.md` vs. code ECP-filename discrepancy flagged in
  the v1 research notes is resolved: every live, executed code path
  agrees on `..._One_Sheet.xlsx`, so that's what goes into the new
  config; `readme.md`'s `..._Meta_Datapoints.xlsx` was the stale one.

Also newly documented: `pathtools` is the first non-CRAN dependency
this repo has taken on (installed via
`remotes::install_github("ethanplunkett/pathtools")`); the revised plan
flags this needs a mention in the repo's setup docs once implemented.

The reports-move goal (`lidar/output/` → `../lidar_reports/`, outside
git) carries over from v1 unchanged — it's orthogonal to the
`pathtools` adoption.

## 2026-09-02 — branch lidar

### Built three global Claude Code skills for recurring workflow patterns

Discussed which recurring process patterns in this repo (and other R
projects like BirdFlowR) were worth turning into installed skills.
Landed on three that recur consistently enough to automate globally,
at `~/.claude/skills/` (available in every project, not just this
one):

- `lint-changed` — computes the changed-file set (unstaged + staged +
  committed-but-not-yet-merged) and lints only those, dispatching to
  whatever linter a project already has configured (`lintr` for `.R`
  via that project's own `.lintr`; `eslint`/`ruff`/`flake8` if
  configured elsewhere), then fixes hits. Directly codifies the
  "lint before commit" rule in this project's `CLAUDE.md`.
- `new-r-function` — scaffolds `R/<name>.R` with a roxygen doc
  skeleton for the one-function-per-file convention. Detects indent
  width from the target project's `.lintr` (`indentation_linter`) or
  `.Rproj` (`NumSpacesForTab`) rather than hardcoding a width — this
  repo's own config sets 3 spaces, which won't match every project.
  Pipe operator is always `|>` (never `%>%`) by explicit request, not
  auto-detected.
- `worklog-entry` — automates the mechanics of this very convention:
  matching today's date/branch against the top heading, inserting
  new sub-headings above existing ones for the same day, and
  flagging (without acting on its own) when `dev/worklog-archive.md`
  rotation is due.

No global skills existed before this (`~/.claude/skills/` was empty).
Format modeled on an installed plugin skill
(`discord/skills/access/SKILL.md`) for the YAML-frontmatter +
Markdown-body structure. Smoke-tested the underlying logic manually
(git diff computation, `.lintr`/`.Rproj` indent detection, heading-
match against this file) rather than invoking the skills themselves,
since newly created global skills aren't picked up by the Skill tool
mid-session — they'll be available starting next session. Also added
a reference memory (`global_skills.md`) pointing at these so future
sessions in this project know they exist.

## 2026-08-28 — branch lidar

### Wrote up a plan for the scan-angle-correlation boresight test

Follow-up to the lever-arm-clearing finding below. Drafted a plan
(via plan mode) for testing whether the UAS vertical bias correlates
with lidar scan angle — the check that would confirm or weaken the
boresight-calibration hypothesis (see 2026-08-27 entries below).
Saved as `dev/scan_angle_bias.md` rather than the usual
`dev/work_plan.md` at the user's request, since it's a self-contained
sub-plan for one open item rather than the whole pipeline's active
plan; pointed to from `lidar/readme.md`. **Not yet implemented or
approved** — no `R/`/`lidar/` code has been written for this yet.

## 2026-08-27 — branch lidar

### Lever arm cleared: refined value replicates to <1 cm across 6 independent 2022 flights

Follow-up to the finding below. User asked a clarifying question: since
it's the same physical unit on the same UAS, shouldn't offsets be
consistent flight to flight anyway — isn't the real question whether
they're *consistently wrong*, not just consistent? Right, and it split
the two offset types apart usefully:

- The **boresight** (linear/angular) is a static config value copied
  verbatim into every mission's `ppk.pcmp` — it's never re-derived, so
  finding it identical across flights confirms nothing about
  correctness, only that nothing's been tampered with per-flight.
- The **GNSS-INS lever arm**, by contrast, *is* independently re-solved
  per flight by Inertial Explorer's misclosure minimization (see
  above). That gives a real cross-check: if the true physical antenna
  offset is fixed, independent per-flight solves should agree.

Pulled the final refined lever arm from the latest PPK Report in each
of the 6 processed 2022 flights (RR May/Aug, WEL May/Aug, OTH May/Aug):
Z-component values were 387.5, 385.7, 393.9, 388.9, 392.2, 385.2 mm —
mean 388.9 mm, std ~3.2 mm, full range only 8.7 mm across all six,
each converging from the same generic ~335 mm starting guess to
sub-mm final misclosure (0.6-1.0 mm) independently. X/Y components
also cluster, somewhat less tightly (X: 96-117 mm, Y: 47-77 mm).

**Conclusion: the GNSS-INS lever arm is cleared as a source of the
+10-16 cm vertical bias.** Six independent flights across three sites
and two seasons agree to well under 1 cm — an error here would have
to be a real ~38.9 cm mismeasurement that's *itself* wrong at the
sub-cm level, which the observed consistency makes implausible.
Narrows the live candidates to: the boresight calibration (structurally
can't get this kind of cross-check — copied forward, never
re-derived) and the base-station coordinates (same issue — used as
inputs, not re-derived). Both still need either an independent
verification method (scan-angle/range correlation for boresight; an
external coordinate check for the base stations) rather than a
flight-to-flight consistency check, since consistency alone can't
distinguish "right" from "wrong but constant" for either one.

### Traced the UAS vertical bias into the raw PPK/INS processing artifacts; found a likely common factor

User asked what data we'd need to keep diagnosing the +10-16 cm UAS
vertical bias (`lidar/readme.md`, "Systematic vertical bias..."), then
pointed to a specific read-only source folder to dig into it:
`X:\legacy\gdrive\saltmarsh_UAS\UAS Data Collection\Red River\2022\
LiDAR\10Aug2022_Low\RESEPI-5FFC59-2022-08-10-20-26-50`. Treated that
folder (and siblings for other sites/dates, read-only) as the raw
flight archive and inspected the actual PPK/INS project files rather
than relying on the generic tutorial notes doc.

**Lever arm — ruled out for this flight.** The generic tutorial value
`(0.052, 0.063, 0.3355 m)` is *not* what got applied. Inertial
Explorer's `PPK Report *.html` shows it auto-refining the GNSS-antenna
lever arm via IMU/GNSS misclosure minimization, converging from that
generic starting value (18.1 mm misclosure) to a final
`(0.0992, 0.0473, 0.3875 m)` (0.7 mm misclosure) — stored in
`ins/*-gnss.proj`'s `GPS_INS_OFFSETS`. This is a per-flight automatic
refinement, not a fixed/guessed number, so a bad lever-arm entry is not
the explanation, at least for flights where this refinement converges
this cleanly.

**Geoid — confirmed RESEPI's own step is a no-op here.** The
`ppk.pcmp`/`ppk.pcpp` project files (the "Cloud Generator" step that
consumes `SBET.OUT`+`lidar.scan` to build the `.las`) have no
geoid/vdatum settings at all — output stays pure WGS 84 ellipsoidal
height, matching `CRS.md`. Geoid conversion happens in a separate,
later step (`reproject_las()`), already shown negligible (<1 cm,
GEOID12B vs. 18) in the 2026-08-27 CRS.md work above.

**Base station coordinates — probably not a single bad station.**
Checked which CORS station each 2022 site used, via the RINEX
`MARKER NAME`/`APPROX POSITION XYZ` header fields in each mission's
`data/*.22o` file: Red River uses **MACM**, Wellfleet uses **MATU**,
Old Town Hill uses **MASA** — three different stations, each with an
identical, repeated coordinate across separate flight sessions at the
same site (not per-session autonomous-fix noise). Confirmed Inertial
Explorer's `MB_MASTER_POS` in `gnss.proj` for the RR/Aug flight
matches the RINEX header's `APPROX POSITION XYZ` (converted to
lat/lon/h) to sub-mm — i.e., it's using the station's own header
coordinate directly, not an independent per-flight PPP solve.
Cross-checked against the already-computed bias numbers in
`lidar/output/{rr,oth,wel}_ground_comparison/ground_source_comparison.csv`:
the positive bias shows up at comparable magnitude (~0.05-0.24 m) at
all three sites despite three unrelated base stations — a single
mis-surveyed CORS coordinate would need to coincidentally match in
sign and rough magnitude across three independent stations, which is
unlikely. Still unverified independently (no NGS/MACORS datasheet
lookup or independent PPP re-solve done — `WebSearch` isn't available
in this session/model and guessing at NGS/CORS URLs didn't pan out),
so not fully ruled out, just downgraded.

**Most likely common factor found: a single physical sensor unit's
fixed boresight calibration, reused unchanged across all 2022
flights.** Every 2022 RESEPI mission at all three sites (RR, WEL, OTH,
spring and summer) was flown with the same physical unit, serial
`5FFC59` (folder names all `RESEPI-5FFC59-...`), and every mission's
`log.txt` records the *identical* boresight offsets: linear
`(0, 0.060, 0) m`, angular `(yaw 0.02°, pitch -0.06°, roll -0.58°)` —
a static per-unit calibration carried unchanged into every flight's
`ppk.pcmp`, not something re-derived per mission (unlike the GNSS
lever arm above, which *is* refined per flight). This is the one
parameter set common to every biased 2022 dataset regardless of
site/base-station, so it's currently the leading candidate — though
not yet confirmed mechanistically: a roll misalignment of this size
would nominally be expected to produce a scan-angle/range-dependent Z
error rather than a uniform constant offset, so the sign/magnitude
math hasn't been worked through, and it's only "the one thing that's
the same everywhere," not something independently verified against a
calibration spec. Couldn't compare against the 2024 unit (`A84717`,
used at South River/Peggotty/2024 Wellfleet) — those source folders
under `X:\legacy\gdrive\...` haven't been through the Cloud Generator
step yet (no `ins/`, `clouds/`, or `log.txt`), so no calibration
values are available there yet for a same-vs-different-unit
comparison.

**Open next steps, not yet acted on:**
- Get an independent/authoritative height for at least one CORS base
  station (MACM, MATU, or MASA) — NGS/MACORS datasheet if the user has
  access, or an independent PPP re-solve of the RINEX (would need the
  user's sign-off before uploading any RINEX to an external PPP
  service like OPUS/CSRS-PPP).
- Check whether the observed bias correlates with scan angle / range
  from nadir in the raw point cloud (would implicate the boresight
  roll/pitch) vs. staying flat regardless of geometry (would point
  elsewhere) — doable with data already on hand, no new external
  inputs needed.
- Get a factory/field calibration record for unit `5FFC59`'s boresight
  to compare against the `(0, 0.06, 0)`/`(0.02, -0.06, -0.58°)` values
  baked into every 2022 flight.
- Revisit the 2024-unit comparison once those flights are actually
  processed through Cloud Generator.

### Found the NAD83(2011) frame shift isn't actually being applied anywhere yet

Follow-up to the frame-shift hypothesis check below. User asked to add
the ITRF2014→NAD83(2011) vertical-shift number to `CRS.md`'s
frame-shift section. While pinning down the exact number, checked
whether the horizontal shift `CRS.md` already claimed ("EPSG:6348 /
EPSG:6491: PROJ picks a realization-specific transformation pipeline
and actually applies that ~0.5-1 m shift") is actually true for the
project's real data, since the vertical number needed the same source
CRS to be meaningful.

It isn't. `projinfo -s EPSG:32619 -t EPSG:6348 -o PROJ` (EPSG:32619 is
what the RESEPI LAS headers actually declare) returns just the inverse
UTM projection followed by the new one with the ellipsoid swapped
WGS84→GRS80 — no Helmert step, and PROJ marks the operation's own
accuracy as "2 m" (its ballpark-transformation signal). This is
identical, mechanistically, to the already-documented EPSG:26919
no-op case — PROJ treats generic "WGS 84" (the datum ensemble
EPSG:32619/4979 carry) as coincident with NAD83(2011) regardless of
which NAD83(2011) target EPSG is requested. Confirmed with an actual
coordinate: EPSG:32619→EPSG:6491 and EPSG:32619→(EPSG:6491's
proj4string, i.e. what `reproject_las_pdal()` builds as `out_srs`)
land on identical projected X/Y to the last digit.

The real shift only appears when the *source* is tagged with a
specific realization (`projinfo -s EPSG:7912 -t EPSG:6319`, ITRF2014→
NAD83(2011), returns the genuine published 14-parameter time-dependent
Helmert transform). So `reproject_las_pdal()`'s docstring claim of
eliminating the frame-realization offset is not currently true for
any LAS file tagged EPSG:32619 — which is all of them. Every lidar
output reprojected so far to EPSG:6348/6491 has the same
frame-uncorrected coordinates as if it had been left at legacy
EPSG:26919.

Updated `CRS.md`'s "frame-shift issue" section to correct this
(previously-wrong) claim, add the vertical-shift numbers (≈+1.23 m at
the 2022 survey epoch, ≈+1.24 m at NAD83(2011)'s 2010.0 reference
epoch — dominated by the static Helmert, not epoch drift), and flag
the gap as a new open question and a caveat on the practical
checklist. Not fixed in code yet — needs the source CRS re-derived as
a specific realization before reprojection, not just picking a target
EPSG. Left as a flagged, unresolved item rather than starting the fix
without sign-off.

### Checked and ruled out a frame-shift hypothesis for the UAS vertical bias

User asked whether the +10–16 cm UAS elevation bias (see
`lidar/readme.md`, "Systematic vertical bias in UAS-derived elevation
data") could come from shifting latitude/longitude from WGS 84 to
NAD 83 without also shifting the ellipsoidal height, then applying
NAVD 88 geoid subtraction to that unshifted height.

Tested numerically rather than just reasoning about it. Found
`Rscript`, Python, and an OSGeo4W PROJ install (`cs2cs`, `projinfo`,
`cct`, PROJ 9.8.1) already on the machine; installed `pyproj` (pip)
for a reliable epoch-aware transform after a hand-assembled PROJ
pipeline via `cct` gave an implausible ~8 m result (later traced to a
transcription slip, not a real effect — `pyproj`'s
`Transformer.from_crs("EPSG:7912", "EPSG:6319")` gave clean, plausible
numbers that cross-checked against `CRS.md`'s existing ~0.5–1 m
horizontal estimate).

Result: the vertical component of the ITRF2014 → NAD83(2011) frame
shift at Red River is ≈ +1.23 m (survey epoch 2022.6, and ≈ +1.24 m
at NAD83(2011)'s 2010.0 reference epoch — so it's the static
translation/rotation, not epoch drift, that dominates). That's ~10x
the observed bias and the wrong sign (would read ~1.2 m too low, not
~10 cm too high), so this mechanism is ruled out as the explanation.
Also reconfirmed, via `projinfo`, that PROJ treats generic "WGS 84"
(EPSG:4979) → NAD83(2011) as `+proj=noop` — zero shift at all, not
just "near-identity" as `CRS.md` currently phrases it.

Wrote up the ruled-out hypothesis (with numbers) as item 4 under
`lidar/readme.md`'s "Likely causes" list, alongside the existing
lever-arm-error and outdated-geoid-model hypotheses.
