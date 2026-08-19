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

## 2026-08-19 — branch main

### Sync CLAUDE.md and .gitignore with the hydrology reorg

Pulled commits `75531b7`..`6f37698` (2026-07-10, done on another machine)
that renamed `hydrology/` to `inundation_metrics/`, cleaned up
`logger_recalibration/` (`recalibrate_sites2.R` → `recalibrate_sites.R`
as the canonical script, old drafts moved to `logger_recalibration/old/`),
and committed the inundation-metrics data that used to be gitignored.
None of this had been reflected in `CLAUDE.md` or `dev/worklog.md`.

`CLAUDE.md`: replaced all `hydrology/` path references with
`inundation_metrics/`; updated the `logger_recalibration/` section to
describe `recalibrate_sites.R` + `recalibration_report.Rmd` as the
current redundant pair (per the new `logger_recalibration/README.md`)
instead of "confirm with the user which is current"; updated the data
conventions bullet — inundation-metrics data is now committed, only
`Calibrated Data/*cal.xlsx` stays gitignored.

`.gitignore`: `/hydrology/*.html` → `/inundation_metrics/*.html` (was
stale, matched nothing after the rename).

`dev/work_plan.md` is unaffected — the reorg only touched
hydrology/logger_recalibration, not lidar, so Phase 1.5/1.6 status
there still holds.

## 2026-05-22 — branch main

### Document UAS vertical bias and diagnostic plan

All UAS-derived datasets (spring/summer lidar and photogrammetry) show
+10–16 cm positive bias against ECPs; MassGIS aerial lidar shows no
bias, confirming ECPs and CRS assumptions are correct.
Bias almost certainly originates in PPK vertical positioning (same base
station/pipeline shared across all UAS flights).
Two candidate causes: wrong GEOID12B tile (`g2012bu0.gtx` vs
`g2012bu4.gtx`) or lever arm misconfiguration.
Vegetation height work is unaffected — bias cancels when normalizing
summer against spring DTM.

`lidar/readme.md`: added "Systematic vertical bias in UAS-derived
elevation data" section with findings, PPK workflow summary, candidate
causes, and implications.
`dev/work_plan.md`: added Phase 1.6 with concrete diagnostic steps.

## 2026-05-21 — branch main

### Switch evaluation to spring date; default ecp_types to EVP

Spring cloud (rr/2022-05-14) finished processing; pivoting evaluation
to that date.

`lidar/03_evaluate_dtm.R`: date set to `"2022_05_14"`.

`R/report_dtms.R`: default `ecp_types` changed from `"Training"` to
`"EVP"` to match the point type used in active evaluation runs.

`rmd/dtm_evaluation_report.Rmd`: default params updated to spring date
and `base_output`; added a filter of `all_elevations` to `ecp_types`
after sampling (previously the cross-DTM plot could include non-EVP
points even when `ecp_types = "EVP"`); full metrics table now sorted
with "overall" row first.

`lidar/05_veg_heights.R`: `spring_dtm` placeholder updated to
`csf_th0.01_res0.1_rgd2_0.25m.tif`.

## 2026-05-20 — branch main

### Keep chunk-dir approach in rasterize_ground / rasterize_veg_heights

Empirically confirmed that output TIFs were not appearing on disk until
the last few minutes of a multi-hour run, ruling out output-buffering
as the sole explanation.

Most likely cause: with `opt_output_files = ""` and
`plan(multisession)`, `catalog_map()` returns a lazy or deferred
`terra::SpatRaster` — terra's C++ raster objects are not natively
serializable across process boundaries, so the main process receives a
reference whose underlying GDAL writes are not forced until memory
pressure or GC, which happens to coincide with the end of the run.

The chunk-dir approach (setting `opt_output_files` to a real path)
sidesteps this: workers write chunk TIFs to shared disk directly, file
paths are trivially serializable, and `catalog_map()` returns a
file-backed SpatRaster with no deferred I/O. Keeping this in place to
observe behavior on the next full run.

### Enable progressr for real-time catalog_map() progress

With `plan(multisession)`, worker stdout/stderr is buffered in separate
R processes and only surfaces when futures resolve — so all
`catalog_map()` progress appeared at the end of each run rather than
as chunks completed.
Fix: add `library(progressr)`, `progressr::handlers(global = TRUE)`,
and `progressr::handlers("cli")` to both `lidar/02.R` and
`lidar/05_veg_heights.R`.
lidR integrates with progressr natively in `catalog_map()`, so no
changes to `rasterize_ground.R` or `rasterize_veg_heights.R` were
needed.

### Expand CSF grid search in lidar/02.R

Replaced the initial 4-run grid with an 18-run expanded search.
Previous best had the largest cloth_resolution (0.20) and
class_threshold (0.12), suggesting the cloth needed to be coarser
and more permissive.

Discussion: threshold of 0.50 (original expansion) rejected — in
short saltmarsh (20–50 cm canopy) a half-meter threshold would
classify vegetation returns as ground.
Settled on threshold ∈ {0.08, 0.12, 0.20}: 0.08 probes whether
the current best is already too permissive, 0.12 is the known-best
reference, 0.20 is the upper bound kept ecologically defensible.

New grid: `expand.grid(csf_res = c(0.20, 0.30, 0.50),
csf_threshold = c(0.08, 0.12, 0.20), csf_rigidness = c(2, 3),
raster_res = 0.25)` — 18 parameter sets.
Existing DTMs (res=0.20, th=0.12, rgd=2) will be skipped on re-run
via the skip-if-exists check in `rasterize_ground()`.

Also: `date_filter` set to `"2022-05-14"` (spring cloud) for the
current run.

### Add lidar/06_compare_ground_sources.R

New driver that evaluates all available ground elevation datasets for
a site against ECPs in a single pass, using the existing `sample_dtm()`
/ `evaluate_dtm()` stack.

Sources compared for rr: two lidar DTMs (spring + summer, best CSF
set each), two spring photogrammetry DEMs (HesaiRGB and Mica sensors),
one summer photogrammetry DEM (Mavic), and the MassGIS 2021 aerial
lidar bare-earth tile.

Params block sets `best_csf_spring` and `best_csf_summer` stems
(placeholders for now; update after spring lidar eval is complete).
Cache CSVs written to `lidar/output/rr_ground_comparison/` so nothing
is written next to source files on the X drive.
Missing source files warn and are skipped rather than halting the run.
Outputs `ground_source_comparison.csv` and prints an overall RMSE
ranking table.

Also added Phase 1.5 section to `dev/work_plan.md` describing the
comparison goal, data sources, and implementation approach.

## 2026-05-19 — branch main

### Two-date vegetation height distribution — initial implementation

Phase 1 evaluation showed all CSF parameter sets fail to find clean
ground in the late-summer salt marsh cloud.
Pivot to two-date approach: use spring (pre-vegetation) cloud as the
ground reference DTM, characterise summer cloud returns above that floor.

**`lidar/02.R`** — added `date_filter` param.
Added `date_filter <- NULL` to the params block (5 lines below `site`).
When non-NULL, filters the preferred paths to the specified date string
before selecting the cloud row.
Set `date_filter <- "2022-05-14"` to process the rr spring cloud.

**`R/rasterize_veg_heights.R`** — new function.
`rasterize_veg_heights(input, dtm, output, bin_breaks, raster_res, ...)`.
Uses `catalog_map()`: for each chunk, normalises heights against the
spring DTM via `normalize_height()`, then calls `pixel_metrics()` with
a per-cell histogram function.
Bin breaks default: 5 cm steps 0–1 m + 20 cm steps 1–3 m = 30 bands.
Cell value = fraction of total returns (including below-ground) in the bin.
Band names encode edges in cm: `h000_005`, `h100_120`, etc.
Writes multi-band GeoTIFF; returns output path invisibly.

**`lidar/05_veg_heights.R`** — new workflow driver.
Params block: `site`, `spring_date`, `summer_date`, `spring_dtm`,
`raster_res`.
Mirrors `02.R` structure: sources R/, loads paths.csv, validates inputs.
Step 1: `clean_and_tile()` on summer cloud (skip-if-exists).
Step 2: `rasterize_veg_heights()` → writes to
`E:/uas_scratch/lidar/<site>/<summer_date>/zzzheights/veg_dist_<res>m.tif`.
`spring_dtm` placeholder points to `csf_th0.06_res0.10_rgd2_0.25m.tif`;
update after running spring DTM evaluation.

User workflow:
1. `02.R` with `date_filter <- "2022-05-14"` → spring DTMs
2. `03_evaluate_dtm.R` with spring date → pick best DTM
3. `05_veg_heights.R` with chosen `spring_dtm` path → height raster

### Merge evaluation pipeline into Rmd report; add cross-DTM plot

Refactored the three-file evaluation stack so the Rmd is the single
source of computation and plots, instead of a passive viewer of
pre-saved PNGs.

**`rmd/dtm_evaluation_report.Rmd`** — complete rewrite.

- Setup chunk now runs the full pipeline:
  source R/ helpers, `load_ecp()`, `sample_dtm()` per DTM
  (skip-if-exists cache hit in normal use),
  `evaluate_dtm()` per DTM, write `dtm_eval_summary.csv`.
  Pre-saving PNGs is gone; plots render inline from the
  `evaluate_dtm()` return values.
- New params: `ecp_path`, `base_output`, `tolerances`,
  `ecp_types`.
  `ecp_types` filters the loaded ECPs before sampling
  (case-insensitive `%in%`).
  Default `["EVP"]` — set in the Rmd YAML; `report_dtms()`
  default is `"Training"`.
- New **Cross-DTM comparison** section: faceted predicted
  vs. observed for all DTMs on a common scale, coloured by
  `veg_height_cm` on a log10 scale.
  Zeros floored to `vhc_floor = 0.01` cm so log scale is
  defined; NAs rendered as grey50.
  Falls back to an uncoloured version if `veg_height_cm`
  column is absent.
- Per-DTM plots section: tabbed by DTM stem, 5 plots each,
  rendered from `results[[stem]]$plots` named list.

**`R/report_dtms.R`** — updated signature.

- Added `ecp_path`, `base_output`, `tolerances`, `ecp_types`
  params with project-standard defaults.
- All four passed through to `rmarkdown::render()` params.
- Dropped pre-flight CSV check (Rmd now creates the CSV).

**`lidar/03_evaluate_dtm.R`** — simplified to param block
  + `report_dtms()` call.
  All computation and PNG-writing removed; those responsibilities
  now live in the Rmd.

Pending: lint `R/report_dtms.R` and `lidar/03_evaluate_dtm.R`
then commit (blocked on Rscript not being on PATH in this shell).

## 2026-05-15 — branch main

### `lidar/02.R` cleanup — header, params block, bug fixes, renames

Two commits' worth of housekeeping on `lidar/02.R` while the
end-to-end pipeline run was in flight,
plus a `CLAUDE.md` policy tweak.

**Linting policy** (`f1cd5a1 Defer linting to commit time`).
Updated the Linting subsection of `CLAUDE.md` to make explicit
what the existing wording implied:
defer `lintr::lint()` to the pre-commit step instead of running
it after every edit.
Mid-development iterations skip lintr;
the final pre-commit pass lints any files with changed R code,
fixes hits, and only then commits.

**Refactor + bug fixes** (`a70a415 Refactor lidar/02.R …`).

- Added a file-level header documenting the four-step pipeline
  (conditional reprojection, clean + tile, CSF tuning loop,
  partial DTM evaluation),
  inputs, outputs, and cross-references to `lidar/readme.md`
  (geodetic discussion) and `dev/work_plan.md` (Phase 1 scope).
- Consolidated run-level parameters at the top:
  `workers`, `chunk_size`, `chunk_buffer`, `site`,
  and `csf_grid`
  (renamed from the less clear `csf_par`).
  All four downstream references to the old name updated.
- Wired `workers` through `plan(multisession, ...)`,
  and `chunk_size` + `chunk_buffer` through both
  `clean_and_tile()` and `rasterize_ground()`.
  `rasterize_ground()` had been silently inheriting its own
  defaults rather than honoring the top-of-file values.
- Rewrote the historical memory / workers comment block as
  prose so it's more useful to a reader and no longer
  triggers `lintr::commented_code_linter` false positives.
- Bug fix: `models$dtm <- output_raster` inside the
  rasterization loop was assigning the same path to every row
  on every iteration,
  so after the loop `models$dtm` was just the last iteration's
  path repeated for all rows.
  Fixed to `models$dtm[i] <- output_raster`.
  The duplicate second `models <- data.frame()` loop further
  down had been a workaround for this;
  it's now deleted.
- Bug fix: `if(! site == tolower(site) && nchar(site) <= 3 && nchar(site) >= 2)`
  was mis-parenthesized — `!` bound to the equality check
  only — so only uppercase-AND-short codes triggered the stop.
  Re-parenthesized to
  `if (!(site == tolower(site) && nchar(site) >= 2 && nchar(site) <= 3))`
  and the error-message typo
  ("2 or 2 character") corrected.
- Dead code: `paths$old_base_output` set but never referenced.
  Removed.
- Dedup: the inline reprojection paragraph that duplicated
  Step 1 in the new file header was trimmed to a one-line
  pointer.
- Style sweep on lines this commit was already touching:
  trailing whitespace stripped,
  long lines broken to ≤80 chars,
  banner separator widths corrected,
  `if(` and `for(` → `if (` / `for (`,
  `paths$ecp` double spacing around `<-` fixed,
  trailing blank lines at EOF removed.
  File now lints clean.

**Identifier renames** (`cf0afad Rename for clarity …`).

- `models` → `csf_results`
  (the variable is the results table indexed by CSF parameter
  row,
  not fitted statistical models).
- `output_raster` → `output_path` in the tuning loop
  (it's a file path, not a raster object).
- `a <- lapply(list.files("R/"), source)` →
  `invisible(lapply(...))`
  (drop the throwaway `a` binding;
  `invisible()` suppresses the return value directly).

No behavior change in the rename commit.

### Phase 0.5 wrap-up — PDAL works, LAStools blocked by licensing

First end-to-end testing of the reprojection helpers after the
2026-05-13 restructure into a PDAL-default dispatcher.
Standalone test of `reproject_las()` against the
rr 2022-08-10 source
(`X:/legacy/.../ppk_07Nov2022_cloud_1.las`).

**PDAL path: works end-to-end after four fixes.**

1. **`system2(env = ...)` is dropped on Windows.**
   Our first PDAL run errored with
   `(PDAL Error) Command 'proj_data=x:/legacy/gdrive/umassair' not recognized` —
   base R's `system2` does not honor `env` on Windows;
   the `PROJ_DATA=<dir>` string was passed as a positional
   argument and PDAL tried to interpret it as a subcommand
   name.
   Fixed in `R/reproject_las_pdal.R` by replacing the `env`
   arg with `Sys.setenv()` + `on.exit()` restore.
   The transient PROJ_DATA mutation in R's own env does not
   interfere with `sf` / `terra` / `lidR` because those
   packages cache their PROJ data dir at load.
2. **PROJ_DATA needed both OSGeo4W's data dir AND the gtx
   dir.**
   Second run failed with
   `proj_create_from_database: Cannot find proj.db`.
   Pointing PROJ_DATA at only the geoid grid directory hid
   `proj.db` (PROJ's main CRS database).
   Fixed by setting PROJ_DATA to a `;`-separated path:
   `C:/OSGeo4W/share/proj` (where `proj.db` lives in the
   OSGeo4W install) plus `dirname(vgrid)`.
   Added a new `proj_data_dir` arg to
   `reproject_las_pdal()`
   (default `"C:/OSGeo4W/share/proj"`)
   with a `dir.exists()` + `file.exists(.../proj.db)`
   guard.
3. **OSGeo4W binaries blocked by Mark-of-the-Web (MOTW).**
   Pre-test pain: `gdal.exe` and other OSGeo4W binaries
   threw "For your protection your administrator is not
   allowing access" on launch.
   Initially looked like AppLocker
   (the effective policy via
   `Get-AppLockerPolicy -Effective -Xml` was almost empty
   except for a Copilot deny rule),
   but turned out to be MOTW.
   `Get-ChildItem C:\OSGeo4W -Recurse | Unblock-File` from
   an elevated PowerShell cleared most;
   a few stragglers needed per-binary right-click → Unblock.
   Standalone `pdal --version` works from a plain PowerShell
   (no OSGeo4W shell needed),
   so no env-wrapper batch file is required.
4. **`las_needs_reprojection()` did not understand LAS 1.4
   WKT-encoded CRSes.**
   After the PDAL run succeeded,
   re-running the check on its own output reported
   `needs reprojection = TRUE` —
   because the function only parsed the GeoTIFF GeoKey
   directory (`header@VLR$GeoKeyDirectoryTag$tags`),
   which is empty in PDAL output.
   PDAL writes LAS 1.4 with the global-encoding `WKT` bit set
   and the CRS in a VLR named `WKT OGC CS`
   (record ID 2112).
   Fixed in `R/las_needs_reprojection.R` by trying the GeoKey
   directory first
   (covers RESEPI / LAStools sources)
   then falling through to a WKT scan that looks for the
   target `AUTHORITY["EPSG","NNN"]` codes in the WKT VLR.
   Safe because top-level CRS EPSG codes
   (26919 horizontal, 5703 vertical)
   don't collide with nested datum/ellipsoid/unit authorities.

PDAL test now produces
`E:/uas_scratch/lidar/rr/2022_08_10/reprojected/ppk_07Nov2022_cloud_1_epsg26919_navd88.las`.

**LAStools path: Step 1 fixed, Step 2 blocked by licensing.**

- **Step 1 fix.** Original `las2las64 -target_epsg <n> -force`
  recipe was aborting with
  `SERIOUS WARNING: horizontal datum of source and target incompatible`
  even with `-force`.
  Replaced with **`las2las64 -proj_epsg <source> <target>`**,
  which delegates the transformation to PROJ and avoids the
  LAStools-native datum incompatibility branch entirely
  (the las2las README marks `-proj_epsg` as "Recommended").
  Added an optional `source_epsg` arg to
  `reproject_las_lastools()`;
  if omitted, the source EPSG is read from the LAS header
  via `lidR::epsg()`.
  Re-running with `-proj_epsg 32619 26919` produces a clean
  intermediate LAS.
- **Step 2 blocker.** `lasvdatum64` is one of LAStools'
  **commercial-only** tools and exits with
  `ERROR:license failure` on a free LAStools install.
  Without a paid license the LAStools two-step workflow
  cannot complete end-to-end on this machine.
  The historical UMassAir workflow must have used a
  licensed install.

**Documentation updates:**

- `R/reproject_las_lastools.R` — roxygen now opens with a
  bold "Requires a paid LAStools license" callout pointing
  to `reproject_las_pdal()` as the working alternative.
  Step 1 description updated to reflect the `-proj_epsg`
  switch.
  Vertical caveat softened from
  "~1–2 m in 3D" to
  "a few cm to ~1 m vertical residual" since the horizontal
  frame shift is now applied properly via PROJ.
- `R/reproject_las.R` (dispatcher) — same license callout
  on the `"lastools"` method bullet.
- `R/reproject_las_pdal.R` — new `proj_data_dir` parameter
  documented;
  PROJ_DATA-isolation paragraph rewritten to reflect the
  Sys.setenv approach
  (since `system2(env=...)` is unreliable on Windows).
- `lidar/readme.md` — LAStools install location
  (`C:\tools\LAStools\bin\`) and a license-requirement
  callout added.
  Small wording fixes (double space, PDAL acronym
  expansion).
- `CLAUDE.md` — committed earlier today
  (`f1cd5a1 Defer linting to commit time`):
  defer `lintr::lint()` to the pre-commit step rather than
  after every edit.

**Conclusion:**
PDAL is the working method.
The LAStools method is left in place,
documented as license-gated and currently inert on this
machine.
End-to-end `lidar/02.R` run with the new PDAL path is the
next concrete step.

## 2026-05-13 — branch main

### Phase 0.5 — conditional LAS reprojection via LAStools

Per `dev/work_plan.md` Phase 0.5.
Discovered while preparing to re-run the CSF tuning grid that the
rr 2022-08-10 source LAS is tagged horizontally as EPSG:32619
(WGS 84 / UTM 19N) and vertically as EPSG:5030 (WGS 84
ellipsoidal heights) —
which would have introduced a ~28 m systematic offset against the
NAVD88 ECPs and made CSF tuning meaningless.

**Tooling notes captured for the project memory:**

- LAStools' `las2las` *cannot* consume a `.gtx` geoid grid.
  Its `-vertical_navd88` flag only stamps a metadata key;
  it does not transform Z.
  Confirmed by scanning `C:\tools\LAStools\bin\las2las_README.md`
  for `geoid|gtx|navd|vertical|elevation_offset` —
  the vertical flags are all metadata-only.
- `lasvdatum` (also part of LAStools, at
  `C:\tools\LAStools\bin\lasvdatum64.exe`) does the actual gtx
  grid-based vertical transformation.
  Its README's first example matches our exact use case:
  `lasvdatum64 -i in.laz -epsg 26917 -vgrid g2012bu0.gtx -o out.laz`,
  "convert from (UTM17 + NAD83 ellipsoidal)
  to (UTM17 + NAVD88 Geoid12B)."
- The GEOID12B `.gtx` files live at
  `X:\legacy\gdrive\UMassAir User Resources\LASTools\Geoid Transformation GTX Files\geoid12b\`.
  `g2012bu0.gtx` is the CONUS tile (24–58° N, 230–300° E) and
  covers all four saltmarsh sites.

**New code:**

- `R/reproject_las.R` — wrapper that chains the two LAStools
  binaries via `system2()`:
  `las2las64 -target_epsg <n>` for horizontal,
  then `lasvdatum64 -epsg <n> -vgrid <gtx>` for vertical
  (forward direction:
  ellipsoidal → NAVD88).
  Skip-if-exists when `overwrite = FALSE`,
  matching the `clean_and_tile()` pattern.
  Roxygen docs note the WGS84 ↔ NAD83 ellipsoid offset
  (~1–2 m in eastern CONUS) that this two-step workflow
  implicitly accepts.
- `R/las_needs_reprojection.R` — header check.
  Reads horizontal EPSG via `lidR::epsg()` and parses
  `header@VLR$GeoKeyDirectoryTag$tags` for GeoTIFF key 4096
  (`VerticalCSTypeGeoKey`).
  Returns `TRUE` if either axis differs from the target
  (`26919` + `5703` by default),
  or if the vertical key is absent.

**Workflow integration:**

- `lidar/02.R` — inserted a conditional block between the `RUN`
  banner and `clean_and_tile()` that calls
  `las_needs_reprojection(paths$input)` and,
  if needed,
  reprojects to
  `<base_output>/reprojected/<basename>_epsg26919_navd88.las`
  and reassigns `paths$input` to that file.
  No manual toggle —
  source clouds already in the target CRS pass through unchanged.
- `paths.csv` deliberately left as the stable source-of-truth;
  reprojected files are workflow cache only,
  on the local RAID.

**Linting:** both new files pass `lintr::lint()` clean under the
project `.lintr`.

**Environment note:**
`Rscript` was not on PATH;
user is adding `C:\Program Files\R\R-4.5.2\bin\x64` to the system
PATH manually for all users.
For this session I called `Rscript.exe` by full path
to run lintr.

### Phase 0.5 fix — `shQuote()` path args for LAStools on Windows

First end-to-end run of `lidar/02.R` against the rr 2022-08-10
source failed at Step 1 of `reproject_las()` with LAStools
reporting `no input specified`.
Root cause:
base R's `system2()` on Windows does **not** quote args
containing spaces.
The project paths contain spaces in several places
(`UAS Data Collection`, `Red River`,
and the gtx grid's `UMassAir User Resources`),
so LAStools saw the input path truncated at the first space
and the rest of the path as stray positional args.

Fix in `R/reproject_las.R`:
wrap `input`, `intermediate`, `vgrid`, and `output` in
`shQuote()` inside the two `system2(args = …)` vectors.
Added a code comment explaining why `shQuote()` is needed so
future readers don't try to "clean it up."
Re-lint clean.

### Phase 0.5 fix — add `-force` to las2las for WGS84→NAD83

Second run got past the arg-quoting issue but `las2las`
emitted "SERIOUS WARNING: horizontal datum of source and
target incompatible" and aborted before writing the
intermediate file.
LAStools flags any reprojection between datums it considers
different
(here WGS 84 / UTM 19N → NAD 83 / UTM 19N)
and requires `-force` to proceed.
This is exactly the WGS84 ↔ NAD83 frame caveat already
documented in the function's roxygen
(~1–2 m offset in eastern CONUS,
accepted by the historical UMassAir workflow).

Fix in `R/reproject_las.R`:
added `-force` to the `las2las` `args` vector,
with a code comment cross-referencing the roxygen caveat
so the choice is auditable.
`lasvdatum` was not affected
(it operates on the already-relabeled file
and does not raise the same warning).
Re-lint clean.

### Phase 0.5 redesign — PDAL as default, LAStools as alternative

Discussion of the WGS 84 ↔ NAD 83 frame error revealed that the
~1–2 m offset is real but only matters for absolute NAVD 88
deliverables;
for CSF parameter tuning the offset model in `evaluate_dtm()`
absorbs it.
User chose hybrid option (c):
make PDAL the default
(no frame error;
proper PROJ-pipeline transformation in one pass)
and retain the LAStools path explicitly for reproducing
historical output.
PDAL install route is OSGeo4W
(user has had bad past experiences with multiple PROJ
installations conflicting on the same system,
and OSGeo4W provides a coordinated stack).
Subprocess isolation
(R never loads PDAL's PROJ;
PDAL never loads R's PROJ)
mitigates the conflict risk regardless.

**Restructure into dispatcher + two helpers:**

- `R/reproject_las.R` — public dispatcher.
  Signature
  `reproject_las(input, output, target_epsg = 26919, vgrid, overwrite = FALSE, method = c("pdal", "lastools"), ...)`.
  Skip-if-exists short-circuit lives here so it applies
  uniformly across methods;
  method-specific args
  (`pdal`, `las2las`, `lasvdatum`, `intermediate`,
  `keep_intermediate`)
  pass through `...` to the chosen helper.
- `R/reproject_las_pdal.R` — new.
  Writes a small PDAL pipeline JSON
  (readers.las → filters.reprojection → writers.las)
  to a tempfile,
  then calls `pdal pipeline <json>` via `system2()` with
  `PROJ_DATA = dirname(vgrid)` set on the subprocess only
  (no system-wide env var changes,
  so R's PROJ stays unaffected).
  Target PROJ string is built as
  `sf::st_crs(target_epsg)$proj4string` plus
  `+geoidgrids=<basename(vgrid)> +vunits=m`.
  Output gets `a_srs = "EPSG:<target_epsg>+5703"`
  (compound CRS for NAD83/UTM 19N + NAVD88 height).
  Default `pdal` path is `"C:/OSGeo4W/bin/pdal.exe"`.
  Requires `sf` (already a transitive dep via lidR)
  and `jsonlite`
  (CRAN, may need install).
- `R/reproject_las_lastools.R` — moved from the previous
  `R/reproject_las.R`,
  function renamed
  `reproject_las()` → `reproject_las_lastools()`.
  Logic unchanged
  (`shQuote()` on path args,
  `-force` on `las2las`),
  but the WGS84↔NAD83 frame caveat in the roxygen now
  explicitly states that `reproject_las_pdal()` does not
  carry this error
  and is the recommended path for new output.

`lidar/02.R` needed no change —
its call to `reproject_las()` now picks up the new PDAL
default automatically.

User will install PDAL via OSGeo4W tomorrow and test then.
Code shipped today is untested against a real PDAL
install;
PDAL pipeline JSON syntax,
`out_srs` PROJ string construction,
and `a_srs` compound-EPSG behavior may need iteration.
All three files lint clean under the project `.lintr`.

### Phase 0 cleanup — extract lidar helpers into R/, fix bugs

Per `dev/work_plan.md` Phase 0.
Worked through the four tracked tasks (#1–#4).

**Extracted from `lidar/02.R` into one-function-per-file R/ modules:**

- `R/update_path.R` — bracketed-placeholder substitution.
  Dropped the inline `stopifnot()` self-test;
  the example covers it.
- `R/clean_column_names.R` — lowercase + space-to-underscore.
- `R/clean_dates.R` — handles Excel serial dates and DMonY strings.
- `R/visualize_dtm.R` — leaflet map of ECPs.
  Cleaned up styling but kept the existing behavior
  (which only renders the ECPs, not the DTM —
  the name is aspirational).
- `R/evaluate_dtm.R` — finished out: returns `list(points, summary, offset, plots)`.
  Fixed the `&& nlyr == 1` bug
  (should have been `|| nlyr != 1`,
  matching `visualize_dtm()`'s validation).
  Stats are reported overall and broken out by ECP `type`,
  with `adj_*` columns showing residuals after subtracting the global offset.
- `R/summarize_residuals.R` — helper called by `evaluate_dtm()`.
- `R/plot_pred_vs_obs.R`, `R/plot_residual_hist.R`, `R/plot_residual_map.R` —
  the three plot helpers, one per file per the new convention.

**Bug fixes in existing files:**

- `R/clean_and_tile.R:48` — `ouput_dir` typo corrected.
- `R/rasterize_ground.R:44` — `alogrithm` typo corrected.
  lidR was silently falling back to its default (`tin()`);
  the intended `knnidw(k=10, p=2, rmax=0.5)` is now actually applied.
  **Existing DTMs on disk were produced with `tin()` and will need re-rasterization before Phase 1 evaluation.**
- `R/rasterize_ground.R:46` — removed
  `terra::crop(raster, terra::ext(chunk), snap = "out")`,
  which referenced an undefined `chunk` and discarded its result.
  Dead code.
- `lidar/02.R` — removed the inline definitions of the five extracted helpers;
  callers now use the `R/` versions sourced at script start.

**Linting setup (per the new convention):**

- Added top-level `.lintr` with two project deviations from lintr defaults:
  `indentation_linter(indent = 3L)` to match the project's 3-space indent,
  and `object_usage_linter = NULL` to suppress the false-positive avalanche from NSE column references in dplyr/ggplot2 and from cross-file function calls (lintr lints one file at a time and doesn't know the rest of `R/` will be sourced together).
- All nine new R files lint clean under this config.
- Pre-existing lint hits in `clean_and_tile.R`, `rasterize_ground.R`, and `lidar/02.R` on lines this commit didn't touch are left for a future dedicated cleanup commit.

### Adopt BirdFlowR-inspired conventions + start NEWS.md

After reviewing BirdFlowR's `.github/CONTRIBUTING.md`, folded four of
its conventions into the `CLAUDE.md` Style section:

- **Lint new code** with `lintr::lint("R/path/to/file.R")` before
  committing; do not lint otherwise-unchanged files.
- **One function per file** in `R/`, filename matches function name.
  Extract reusable helpers out of workflow scripts into their own
  files.
- **Markdown syntax in roxygen comments** — backticks, `[text](url)`,
  etc. — so when the eventual package migration enables
  `Roxygen: list(markdown = TRUE)` the existing comments render
  correctly with no rewrites.
- **Semantic line breaks** in markdown (`CLAUDE.md`, `NEWS.md`,
  `dev/*.md`, READMEs) — one line per sentence or clause. Apply on
  new and edited content; reflow existing prose opportunistically,
  not retroactively.

Also restructured the Style section into subsections ("Coding style",
"File organization", "Linting", "Markdown", "NEWS.md", "Branching")
and rewrote the existing prose with semantic line breaks.

Branching policy: work on `main` for now; revisit if collaboration
grows. No issue-first requirement for proposed changes.

Created top-level `NEWS.md` as the user-facing changelog. Format
matches BirdFlowR (setext `===` date headings, asterisk bullets with
4-space hanging indent, backticks around function names), but uses
`YYYY-MM-DD` headings instead of version numbers since this repo
isn't versioned. First entry covers today's project-setup work and
the recent `lidar/02.R` restructuring commits.

Relationship to `dev/worklog.md`: NEWS is terser (highlights only);
worklog stays as the file-by-file dev record.

### Adopt tidyverse style guide (with project deviations)

Expanded the `## Style` section in `CLAUDE.md` to point at the
[tidyverse style guide](https://style.tidyverse.org/) as the project's
baseline. Documented three project-specific deviations that win when
they conflict with tidyverse:

- 3-space indent (per `salt-marsh-work.Rproj` and existing `R/` files,
  not the tidyverse default of 2).
- Base R `|>` pipe, not magrittr `%>%`.
- Roxygen-style function docs preserved even though the repo isn't a
  package, to keep the eventual package migration cheap.

Also enumerated the most load-bearing tidyverse rules inline (naming,
assignment, strings, spacing, line length, comments, function
structure, `TRUE`/`FALSE` over `T`/`F`, etc.) so they're visible
without leaving CLAUDE.md. No existing files were reformatted — new
code will adopt the style, and old code gets reformatted when
otherwise edited.

### Draft lidar work plan

Wrote `dev/work_plan.md` covering the two lidar goals: (1) DTM
estimation validated against ECPs and (2) a multi-band
percent-of-returns vegetation-strata raster. Phase 0 = small
cleanups + helper extraction from `lidar/02.R` into `R/`; Phase 1 =
finish `evaluate_dtm()` and add `lidar/03_evaluate_dtm.R`; Phase 2 =
add `R/rasterize_strata.R` and `lidar/04_vegetation_strata.R`;
Phase 3 = roll out to other sites with ECP coverage.

Locked-in decisions: stay on `rr` (no ECPs for `nor`); keep the 4
hand-picked CSF combos for first round; skip logger floor points; no
date matching; strata output is percent of returns; no field veg
validation (downstream model is pseudo-validation); DTM stats reported
overall + by ECP `type`.

### Add CLAUDE.md

Wrote `CLAUDE.md` at the repo root documenting the repository's nature
(exploratory R, not a package), the two pipelines (hydrology and lidar)
and their key driver scripts and shared functions, data conventions
(including the site-code casing discrepancy: `RED` ↔ `rr`), and the
expected ad-hoc workflow (source `R/*.R`, run numbered scripts from
project root). Used `init` skill instructions; no other code touched.

### Set up dev/ convention

Opted this repo into the `dev/` worklog convention per the user's
global instructions. Created:

- `dev/work_plan.md` — placeholder header, no active plan.
- `dev/worklog.md` — this file.

No `.Rbuildignore` / `MANIFEST.in` / equivalent exclusion was needed —
this repo is not a package and has no published-artifact build step,
so `dev/` is simply tracked in git alongside the rest of the source.
