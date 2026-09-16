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

## 2026-09-16 — branch fill-sinks

### Added `fill_sinks()`

Added `R/fill_sinks.R`: sink-fills an input raster (a DTM, though the
function is generic) via `whitebox::wbt_fill_depressions()` and
writes a second fill-height raster (`filled - original`, clamped to
`>= 0`). This is a hydrology-side building block — per instruction it
lands in `R/` only, with no driver, no `pathtools` path-scheme entry,
and no wiring into any workflow yet. Full design/decision log is in
`dev/workplan.md` as of this session; this entry is the summary.

Decided on `whitebox` (WhiteboxTools) over a pure-R alternative
(`topmodel::fill.sinks`) after checking in — whitebox is fast on large
rasters, is the field standard, and its
`wbt_breach_depressions_least_cost()` is the natural fit for the
anticipated road/culvert-breaching work (see the new `dev/backlog.md`
entry below). That breaching step is *not* implemented — `fill_sinks()`
makes no assumption about whether its input has already had roads
burned through.

`input` accepts either a `terra::SpatRaster` or a path to one
(mirrors `sample_dtm()`/`visualize_dtm()`); WhiteboxTools needs a file
on disk, so an in-memory raster is written to a temp GeoTIFF first.
`overwrite = FALSE` skips recomputation if both outputs already
exist, matching `build_ground_raster()`/`build_floor_corrected_ground()`.

One correctness fix during smoke-testing:
`whitebox::wbt_fill_depressions()` does not return a numeric exit
code — with `verbose_mode = FALSE` it returns a one-line character
summary (or `"... [did not run]"` on failure), and with
`verbose_mode = TRUE` it returns WhiteboxTools' entire raw progress
log instead. An initial `identical(exit, 0L)` check was always false
and surfaced the whole progress dump as a spurious error even on
success. Fixed by hardcoding `verbose_mode = FALSE` and checking
`file.exists(output)` as the success signal instead (same pattern
`reproject_las_pdal()` uses for its PDAL subprocess call), with the
returned summary text included in the `stop()` message for debugging
if the file really is missing.

Smoke-tested manually (no `tests/` directory in this repo) against a
real ground raster
(`E:/uas_scratch/lidar/rr/2022_05_14/ground_rasters_epsg6491/csf_th0.06_res0.1_rgd2_0.25m.tif`):
confirmed both outputs are written, fill height is always `>= 0`
(~4.46M of 8.94M cells raised, max fill 3.91 m), in-memory
`SpatRaster` input works via the temp-file path, and `overwrite`/
skip-if-exists behave as intended. Linted clean
(`lintr::lint("R/fill_sinks.R")`).

Also this session: installed the `whitebox` R package (2.4.3, CRAN)
and the WhiteboxTools binary (v2.4.0, via
`whitebox::install_whitebox()`) on this machine — logged in the new
`dev/whitebox_install.md`, including the download source URL and
install date, per instruction not to modify system `PATH` but to
report what could be added. Added a matching `whitebox` note to the
top-level `README.md`'s Dependencies section (small, targeted edit —
`inundation_metrics` docs were deliberately left untouched since
another developer is actively working there). Parked the anticipated
road/culvert flow-line-burning preprocessing step in `dev/backlog.md`
rather than building it now.

Branch note: this work started on a `fill-sinks` branch created off
`main` after the `lidar` branch was merged and pulled.

## 2026-09-15 — branch lidar

### Split lidar drivers into tuning vs. production

Implemented `dev/driver_refactor_plan.md` (moved into `dev/workplan.md`
per the personal `dev/` convention, then implemented against it in the
same session — see that file's "Migration checklist" for the itemized
list, now all checked off except the worklog entry itself). Full
rationale, decision log, and per-stage design lives in
`dev/workplan.md`; this entry is the change summary.

**Problem.** The old numbered drivers (`02.R`, `03_evaluate_dtm.R`,
`06_compare_ground_sources.R`, `07_floor_corrected_ground.R`,
`08_veg_height_validation.R`, `05_veg_heights.R`) conflated two very
different jobs in one sequence: human-in-the-loop tuning (grid-search
CSF parameters, compare ground sources, decide on a ground reference)
and repeatable production (turn an already-tuned site/flight into
final rasters). That made CSF parameters and ground-reference
decisions get hand-copied as literals into multiple driver scripts
with no single source of truth, and made it unclear which scripts
were required gates vs. one-off diagnostics.

**New layout.**

- `lidar/paths.yml` (was `lidar/data/paths.yml`) — moved via `git mv`;
  header comment and local-overlay example paths updated; added a
  `run_config` entry pointing at the new `runs.yml`.
- `lidar/runs.yml` — new, hand-edited (not templated) config file: the
  single source of truth per site for `target_epsg`,
  `workers`/`chunk_size`/`chunk_buffer` (used only by
  `prepare_flight()` — every rasterization step still keeps its own
  driver-local values, since they have different memory footprints),
  each flight's `best_csf`, and the site's `ground_reference`
  decision. Pre-populated with `rr`'s current values.
- `R/get_run_config.R` — reads `runs.yml` fresh from disk every call
  (deliberately not using `pathtools`'s global-scheme-state pattern);
  looks up a site and, optionally, a flight; stops with an actionable
  message if either isn't recorded yet.
- `R/prepare_flight.R` — extracted from `lidar/02.R` /
  `lidar/05_veg_heights.R`'s duplicated conditional-reprojection +
  `clean_and_tile()` logic. Calls `future::plan(multisession, ...)`
  itself.
- `R/build_floor_corrected_ground.R` — extracted from
  `lidar/07_floor_corrected_ground.R`; just the raster build
  (reproject MassGIS, add floor bias), now with skip-if-exists
  behavior the original script never had (it always rebuilt
  unconditionally).
- `R/build_ground_raster.R` — new. Given a site/flight already
  recorded in `runs.yml`, produces its ground raster with no grid
  search or human judgment: dispatches on `ground_reference.method`
  (`"csf"` → `rasterize_ground()` with the recorded `best_csf`;
  `"massgis_plus_floor"` → re-reads the tuning step's *cached* ECP
  residual CSV, re-runs only `estimate_floor_bias()` — cheap — and
  calls `build_floor_corrected_ground()`). Multi-tile MassGIS mosaicking
  is explicitly out of scope; stops with a pointer to
  `dev/rollout_workplan.md` if `massgis_tile` isn't a single value.
- `lidar/tuning/01_csf_tuning.R` — replaces `02.R` +
  `03_evaluate_dtm.R`: `prepare_flight()`, the CSF grid loop, then ECP
  sampling + `report_dtms()`. Ends with a reminder to hand-edit the
  winning `best_csf` into `runs.yml`.
- `lidar/tuning/02_ground_reference_selection.R` — replaces
  `06_compare_ground_sources.R` + `07_floor_corrected_ground.R`. Pulls
  `best_csf` from `get_run_config()` instead of hardcoding it (this is
  what kills the drift risk the old scripts had), renders the ground-
  source comparison report, computes and plots floor-bias diagnostics
  for every candidate source flight, then — if `rr`'s `runs.yml`
  already records `ground_reference.method == "massgis_plus_floor"` —
  calls `build_ground_raster()` to (re)build the hybrid raster;
  otherwise reminds to record the decision. `oth` and `wel` sections
  carried over unchanged (photo-DEM + MassGIS only; not yet
  represented in `runs.yml`, tracked under the site rollout).
- `lidar/tuning/veg_height_check.R` — replaces
  `08_veg_height_validation.R`, demoted from a required gate to an
  optional, on-demand diagnostic per the plan's locked-in decision.
  Also stopped hardcoding the spring flight's CSF literals, pulling
  `best_csf` from `get_run_config()` instead.
- `lidar/production/01_ground_raster.R` — new, thin wrapper around
  `build_ground_raster()`.
- `lidar/production/02_veg_heights.R` — replaces `05_veg_heights.R`;
  no longer hard-stops if a `corrected_ground_raster` doesn't already
  exist — gets it from `build_ground_raster()` instead, building it on
  demand.
- `lidar/00_explore_data.R` (was `01_explore_data.R`) — renamed only,
  no content change; it's scratch exploration, not part of the
  numbered production sequence.
- `rmd/ground_source_comparison.Rmd`, `rmd/dtm_evaluation_report.Rmd`
  — each independently called `set_path_scheme("lidar/data/paths.yml")`
  (they render in their own process, not inheriting the calling
  driver's scheme); missing this would have broken `report_dtms()`/
  `report_ground_sources()` under the new drivers. Updated to
  `lidar/paths.yml`.
- Deleted the six superseded originals listed under "Problem" above.
- Updated `lidar/ARCHITECTURE.md` ("Where things live" and every
  affected "Pipeline stages" entry, plus the mermaid diagram) and the
  root `CLAUDE.md` lidar section (including the `lidar/data/`
  paragraph, since `paths.yml`/`runs.yml` no longer live there) to
  match the new layout.

**Testing.** No formal test suite exists for this pipeline. Smoke-
tested against `rr`'s already-materialized outputs (fully re-run and
verified as of the 2026-09-14 entries below), via ad hoc `Rscript`
calls rather than a full interactive driver run: `get_run_config()`'s
site/flight lookups and both error paths; `build_ground_raster()`'s
`massgis_plus_floor` branch, which correctly identified the existing
`massgis_plus_floor_summer.tif` and skipped rebuilding it;
`prepare_flight()`, which correctly skipped both reprojection and
`clean_and_tile()` against `rr`'s existing reprojected LAS and cleaned
tiles. Did not do a full interactive run of any driver script through
RStudio, and did not exercise the `"csf"` branch of
`build_ground_raster()` or the two `rmd/` report templates end to end.

`lintr::lint()` run on every new/changed `R/` and driver file; fixed
line-length, hanging-indent, and one commented-code hit (reworded a
driver comment from pseudo-code to prose) before this entry.

### Updated ARCHITECTURE.md and driver header comments to the final path scheme

Closed out the dangling `dev/backlog.md` item carried over from
`workplan.md` Part E (see the "Part E complete" entry below, and
2026-09-12/14 for the naming-convention rework itself). Documentation
only, except for one incidental change bundled into the same commit:
`lidar/05_veg_heights.R`'s `workers` parameter was dropped from 25 to
15, unrelated to the path-scheme wording fix.

`lidar/ARCHITECTURE.md`, six spots: the "Where things live" bullet's
list of scratch-tree subdirectories, and the per-step "Output" lines
for Step 0 (reprojection), Step 1 (clean & tile), Step 2 (CSF
ground-rasterization tuning), the floor-bias-corrected ground
reference, and the vegetation-height-distribution deliverable — all
still said `zzzcleaned_epsg<N>/`, `zzzraster_epsg<N>/`, `zzzheights/`
(the pre-Part-B names, dropped 2026-09-12) and, in three spots, a
stale `<base_output>/` prefix left over from before the `pathtools`
migration (2026-09-14). Replaced with the current `paths.yml` names:
`cleaned_epsg<N>/`, `ground_rasters_epsg<N>/`, `veg_heights/`.

`lidar/02.R` header comment, four spots (both the prose description of
steps 2/3 and the "Outputs" list at the bottom): same
`zzzcleaned_epsg<target_epsg>` → `cleaned_epsg<target_epsg>` and
`zzzraster_epsg<target_epsg>` → `ground_rasters_epsg<target_epsg>`
swap, plus dropped the two remaining `<base_output>/` prefixes.

`lidar/05_veg_heights.R` header comment, two spots: the
"summer cloud need not be re-cleaned" note now says
`cleaned_epsg<target_epsg>/` instead of the old bare `zzzcleaned/`,
and the "Outputs (under ...)" line now says `veg_heights/` instead of
`zzzheights/`.

`CLAUDE.md` line 39 (project root, "Data conventions" section) had
the identical stale `zzzcleaned_epsg<N>`/`zzzraster_epsg<N>` wording —
not named in the original backlog item, initially left out of scope
per the user's call when asked, then folded in on a follow-up request
in the same session: `cleaned_epsg<N>/` / `ground_rasters_epsg<N>/`.

Deliberately left untouched: `dev/worklog.md`/`dev/worklog-archive.md`
(dated records of what was true at the time — retroactively editing
old `zzz*` mentions there would misrepresent history), and
`dev/phase1_smoketest.md`/`dev/scan_angle_bias.md` for the same
reason.

Verified: `grep -rn "zzz\b" lidar/ CLAUDE.md` now returns nothing.
`lintr::lint()` on both changed `.R` files (comment-only changes): no
lints. Removed the now-closed item from `dev/backlog.md`. Not
committed, per instructions not to commit unless asked.

### Part E complete: clean `rr` re-run against the reworked path scheme

Full pipeline re-run end-to-end for `rr`
(`lidar/02.R` → `03_evaluate_dtm.R` → `06_compare_ground_sources.R` →
`07_floor_corrected_ground.R` → `08_veg_height_validation.R` →
`05_veg_heights.R`) against the Part B/C path-scheme rework. Manually
run and verified by the user: every output, including the new Part D
metadata sidecars, landed where expected and looked sane.

Compared the fresh reports (`../lidar_reports/rr_2022_05_14/`,
`rr_2022_08_10/`, `rr_ground_comparison/`) against the pre-re-run
archive kept for this purpose (`../lidar_reports/archive/`, see Part A,
worklog 2026-09-11/12) — looked good, so the archive copies were
deleted (7.5 MB, outside git).

This closes out the "scratch-tree cleanup, path-scheme rework, output
metadata, then a clean `rr` re-run" work item tracked in
`workplan.md` (Parts A–E, worklog 2026-09-11 through today). One piece
carried forward rather than closed: updating `lidar/ARCHITECTURE.md`
and the driver header comments to reflect the final path scheme is
still pending — moved to `dev/backlog.md` rather than left dangling in
a cleared `workplan.md`.

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

### Moved the memory pre-flight check into `rasterize_ground()`, made it cross-platform

Discussed moving the check above from `lidar/02.R` into
`rasterize_ground()` itself — cleaner (the function already has, or
can cheaply get, everything the check needs), protects every future
call site automatically, and re-checks on each CSF-grid iteration
instead of once up front. Decided to do it, and to make
`get_available_memory_mb()` cross-platform at the same time since this
pipeline may eventually run on a Linux/SLURM cluster in addition to
Windows.

* `R/rasterize_ground.R` — new `check_memory = TRUE` parameter (after
  `chunk_buffer`). When `TRUE`, computes `lidR::density(ctg)` right
  after the catalog is read and chunk options are set, estimates
  expected per-worker memory via `estimate_worker_memory_mb(process =
  "rasterize_ground", ...)`, and calls `check_worker_memory()` against
  `future::nbrOfWorkers()` — not a `workers` argument, since the
  function can introspect the caller's active `plan()` directly
  (avoids drift between a driver script's `workers` variable and the
  plan actually in effect). `@details` note added: the check assumes
  `future::plan()` is already configured by the caller, same
  assumption `catalog_map()` itself makes.
* `R/get_available_memory_mb.R` — split into `get_available_memory_mb()`
  (dispatches on `.Platform$OS.type`), `get_available_memory_mb_win()`
  (unchanged PowerShell/CIM logic), `get_available_memory_mb_nix()`
  (reads `/proc/meminfo`'s `MemAvailable` — the reclaimable-cache-aware
  figure, not the more pessimistic `MemFree`), and
  `get_slurm_memory_ceiling_mb()` (returns `NA` unless `SLURM_JOB_ID`
  is set, otherwise `SLURM_MEM_PER_NODE` or `SLURM_MEM_PER_CPU *
  SLURM_CPUS_PER_TASK`, used to cap the Linux figure to the job's
  actual allocation on a shared node). Windows/Linux helper names
  shortened to `_win`/`_nix` (not `_windows`/`_linux`) to fit lintr's
  30-character name limit. Kept all four functions in one file despite
  the one-function-per-file convention — they're tightly coupled
  single-purpose helpers behind one public entry point and are
  meaningless individually; only the public function gets a full
  roxygen block. Direct cgroup usage/limit accounting (the most
  precise number under SLURM, but cgroup path/version-dependent) is
  deliberately out of scope — no real cluster to verify it against.
* `lidar/02.R` — reverted the `clean_and_tile()` call to not capture
  its return value, and removed the now-redundant driver-level
  memory-check block between it and the CSF loop (each
  `rasterize_ground()` call now does this itself, only for grid rows
  that actually run).

Verified: `estimate_worker_memory_mb()`/`check_worker_memory()` sanity
checks re-run unchanged (still reproduce `11498` and a correct
max-workers stop message); `get_available_memory_mb()` still dispatches
to the Windows branch here and returns a plausible value; the Linux
`/proc/meminfo` grep/gsub logic verified against a fabricated
`MemAvailable:` line; `get_slurm_memory_ceiling_mb()` verified against
`Sys.setenv()`-simulated `SLURM_JOB_ID`/`SLURM_MEM_PER_NODE` and the
`SLURM_MEM_PER_CPU`/`SLURM_CPUS_PER_TASK` fallback, plus the
no-`SLURM_JOB_ID` `NA` case; `formals(rasterize_ground)` confirms
`check_memory = TRUE` is in place. No real Linux/SLURM machine
available to test the dispatch live. `lintr::lint()` run on all three
touched files — one `object_length_linter` hit (`_windows`/`_linux`
names), fixed by shortening as noted above. No full pipeline re-run
done as part of this change.

