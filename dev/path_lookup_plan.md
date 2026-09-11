# Plan: adopt `pathtools` for path config, reports moved out of the repo

**Status: proposed, not yet implemented. v2 — revised 2026-09-11 to build
on the `pathtools` package instead of a bespoke `lookup_paths()`.**
Written up for review before any code changes.
Intended to be implemented in a fresh session against this document.

## Context

v1 of this plan (see git history) proposed a bespoke `lookup_paths()`
function backed by a hand-rolled `roots:`/`templates:`/`registry:` YAML
schema. Since v1 was written, a new R package, `pathtools`
(<https://github.com/ethanplunkett/pathtools>), was built as a
side project specifically to solve this class of problem — parametric
path management for data-analysis workflows, YAML-configured, with
named roots, `{@ref}`-style composition, per-parameter-combination
overrides, and existence auditing. It's installed on this machine.
This revision replaces the custom function/schema with `pathtools`
directly; the underlying motivation and most of the "what to migrate"
inventory carry over unchanged from v1.

Two related problems, first surfaced 2026-09-02:

**1. Report output is bloating the repo, and the bloat compounds.**
`lidar/ARCHITECTURE.md` currently describes `lidar/output/<site>_*/` as
"small, committed-size artifacts," but that's no longer true. Each
rendered ground-comparison or DTM-evaluation HTML is a self-contained
R Markdown document, ~2.0-2.7 MB each. Worse, the output paths bake in
parameters — e.g. a directory named `rr_ground_comparison_epsg26919` —
so a parameter change (CRS standard, CSF grid, ECP source) produces a
*new* directory alongside the old one instead of overwriting it. Old
versions never get pruned; they just accumulate in git history. `.git`
is already 165 MB and the tracked/untracked content under
`lidar/output/` alone is 21 MB on disk. This will keep getting worse
every time a parameter changes.

**2. Path construction is scattered and duplicated, with no single
source of truth.** Only two real path-lookup mechanisms exist today,
and both are narrow:
- `R/update_path.R` — a pure string-template substitutor (`[name]` →
  value). It has exactly one real call site, `lidar/02.R:235`, building
  per-CSF-parameter-set DTM filenames.
- `lidar/data/paths.csv` — a registry of raw input clouds and ECP
  files only (`site`, `type`, `date`, `path`, `preferred`), read and
  filtered in `lidar/02.R` and `lidar/05_veg_heights.R`.

Everything else — the scratch-tree root (`E:/uas_scratch/lidar/...`),
the reports root (`lidar/output/...`), the ECP spreadsheet path, the
MassGIS reference tiles, the geoid grid file — is hardcoded via inline
`file.path()`/`paste0()` calls repeated across roughly a dozen files.
The ECP spreadsheet path alone is hardcoded identically in at least six
places. This duplication isn't hypothetical risk — this research
already turned up a live inconsistency: `lidar/readme.md` documents the
ECP file as `..._Meta_Datapoints.xlsx`, while every actual default
argument and `paths.csv` use `..._One_Sheet.xlsx`. **Resolved**: every
piece of live, executed code agrees on `..._One_Sheet.xlsx`; that's the
real file, and `readme.md` gets corrected to match as part of this
migration.

**The ask:** stop committing `dev/run_logs/` (done — already in
`.gitignore`), move all report/raster/CSV output outside the repo to
`../lidar_reports/`, and replace the scattered path construction with
`pathtools`, backed by one config file.

## Goals

1. All report/raster/CSV output currently written under `lidar/output/`
   instead lands under `../lidar_reports/` — a sibling directory to the
   repo, entirely outside git.
2. One YAML file, read by `pathtools::path_scheme()`, is the single
   source of truth for every path that's currently hardcoded or
   duplicated: the scratch root, the new reports root, the ECP file,
   the MassGIS tiles, the geoid grid, and the raw-cloud input registry
   that `paths.csv` currently holds.
3. All path resolution goes through `pathtools::get_path()` (and
   `ensure_parent()` before writes, `audit_paths()` where existence
   reporting is useful). No more inline
   `file.path("E:/uas_scratch/lidar", site, date, ...)` repeated across
   files, and no bespoke path-lookup code of our own to maintain.
4. Existing behavior is preserved: site/date-based cloud selection,
   CSF per-parameter raster naming.
5. `R/update_path.R` is retired — `pathtools`'s glue-based `{name}` /
   `{@ref}` templating fully supersedes it.

## Non-goals

- **`inundation_metrics/Data/sites.txt`** and its hand-duplicated
  `deployment_file_name` vectors — a similar problem, but a separate
  pipeline and a separate config. Not touched here; a candidate for a
  follow-up plan later.
- **Shrinking `.git`'s existing 165 MB** by rewriting history to drop
  the large reports already committed. This plan stops tracking them
  going forward. Reclaiming space already spent would be a separate,
  explicit, destructive operation.
- **Cleanup of already-gitignored stray files** (a stray `Thumbs.db`
  under `lidar/output/rr_2022_08_10_epsg26919/`, leftover top-level
  `.html` renders) — harmless, not blocking, worth a separate pass.
- **A machine-local overlay file** (`pathtools` supports layering a
  second YAML over the first specifically for this). Not set up now —
  see Design §1 for why the base file is still structured so adding one
  later is a two-line change.

## Design

### 0. New dependency: `pathtools`

Not on CRAN — installed from GitHub
(`ethanplunkett/pathtools`, currently at commit `aa740f4`, package
version 0.1.0). This is the first non-CRAN dependency this repo has
taken on (`lidR`, `VulnToolkit`, `yaml`, etc. are all CRAN). Document
the install command wherever the repo's setup steps live (currently
just prose in `README.md`, since there's no `DESCRIPTION` to declare it
formally):
```r
remotes::install_github("ethanplunkett/pathtools")
```
Since `pathtools` is itself actively developed by the same person doing
this migration, no version pinning beyond "whatever's installed" is
proposed — revisit if `pathtools` breaking changes ever bite.

### 1. Config file — `lidar/data/paths.yml`

Replaces `lidar/data/paths.csv` in place (same directory, same role as
"the one committed config file under `lidar/data/`" per `CLAUDE.md`).
One YAML file, `pathtools`'s native `paths:` (+ optional `parameters:`)
schema — no custom `roots:`/`templates:`/`registry:` split; those
distinctions are just entries in the same flat `paths:` map, some
referencing others via `{@name}`.

Sketch (abbreviated — see §4 for the full call-site inventory this
needs to cover):

```yaml
paths:

  # --- Roots -----------------------------------------------------------
  # Every root gets its own entry so that a future machine-local overlay
  # (not set up now — see "Non-goals") only ever needs to redefine these
  # few lines, never anything downstream that references them.
  scratch_root:      "E:/uas_scratch/lidar"
  reports_root:      "../lidar_reports"       # relative -> repo root
  legacy_data_root:  "X:/legacy/gdrive"
  sites_data_root:   "X:/projects/uas/sites"
  massgis_root:      "X:/scratch/bcompton/LiDAR"

  # --- Constants ---------------------------------------------------------
  geoid_grid: "{@legacy_data_root}/UMassAir User Resources/LASTools/Geoid Transformation GTX Files/geoid18/us_noaa_g2018u0.tif"
  ecp_path:   "{@legacy_data_root}/saltmarsh_UAS_native/In Situ Data Collection/JoshSurveyPoints_AllSites_One_Sheet.xlsx"
  # Superseded per-site legacy ECP sheets (paths.csv preferred=0 rows) —
  # never selected by any live code path; kept here only as a record of
  # what used to exist, not as a resolvable entry.
  # oth: "{@legacy_data_root}/.../<old oth-specific ECP file>"
  # rr:  "{@legacy_data_root}/.../<old rr-specific ECP file>"
  # wel: "{@legacy_data_root}/.../<old wel-specific ECP file>"

  # --- Raw input clouds: fully enumerated (no general template — the
  # --- 2022 vs. 2024+ naming conventions are irregular per lidar/readme.md)
  raw_lidar:
    overrides:
      - when: {site: rr, date: 2022-08-10}
        path: "{@legacy_data_root}/saltmarsh_UAS/UAS Data Collection/Red River/2022/LiDAR/10Aug2022_Low/RESEPI-5FFC59-2022-08-10-20-26-50/clouds/ppk_07Nov2022_cloud_1.las"
      # Superseded (paths.csv preferred=0): an earlier processing pass
      # of the same site/date.
      # - path: ".../clouds/ppk_cloud_1.las"
      - when: {site: nor, date: 2024-10-06}
        path: "{@sites_data_root}/nor/lidar_point_cloud/2024_10_06_low_hesaixt32/RESEPI-A84717-2024-10-06-12-46-16/clouds/NOR_2024_10_06_low_lidar_pointcloud.las"
      # ... one override per current paths.csv row that's actually
      # preferred == 1 today; superseded rows kept as comments per-entry
      # the same way, immediately under the row they were replaced by.
    type: file

  # --- Scratch-tree templates -------------------------------------------
  cleaned_tiles:
    template: "{@scratch_root}/{site}/{date}/zzzcleaned_epsg{target_epsg}"
    type: dir
  reprojected_dir:
    template: "{@scratch_root}/{site}/{date}/reprojected"
    type: dir
  ground_raster:
    template: "{@scratch_root}/{site}/{date}/zzzraster_epsg{target_epsg}/csf_th{csf_threshold}_res{csf_res}_rgd{csf_rigidness}_{raster_res}m.tif"
    type: file
  veg_heights_raster:
    template: "{@scratch_root}/{site}/{date}/zzzheights/veg_dist_{raster_res}m.tif"
    type: file

  # --- Reports-root templates --------------------------------------------
  ground_comparison_report:
    template: "{@reports_root}/{site}_ground_comparison"
    type: dir
  dtm_eval_report:
    template: "{@reports_root}/{site}_{date}"
    type: dir

  # --- Third-party reference rasters --------------------------------------
  massgis_tile:
    template: "{@massgis_root}/be_{tile}/be_{tile}.tif"
    type: file

  # --- Orthophoto-derived ground DEMs: fully enumerated, one override
  # --- per (site, source) combination currently hardcoded in
  # --- lidar/06_compare_ground_sources.R.
  photo_dem:
    overrides:
      - when: {site: rr, source: spring_hesai}
        path: "{@legacy_data_root}/saltmarsh_UAS/UAS Data Collection/Red River/Orthos and DEMs 2022/26May2022/Low/26May2022_RED_Low_HesaiRGB_DEM.tif"
      # ... remaining rr/oth/wel rows from lidar/06_compare_ground_sources.R
    type: file

parameters:
  # Left unconstrained (declared, no `values:`) for now — enumerating
  # every valid site/date isn't necessary for pathtools to catch typos
  # in a *declared* parameter name; a firmer `values:` list can be added
  # once the Phase 3 site rollout (dev/workplan.md) settles.
  site:
  date:
  target_epsg:
  csf_threshold:
  csf_res:
  csf_rigidness:
  raster_res:
  tile:
  source:
```

Notes on decisions already made:
- **No local overlay file for now.** Roots are still each their own
  entry (`scratch_root`, `legacy_data_root`, etc.), specifically so
  that if `X:`/`E:` ever differ across machines, adding
  `lidar/data/paths-local.yml` with just those few lines and passing
  `path_scheme(c("lidar/data/paths.yml", "lidar/data/paths-local.yml"))`
  is a two-line change, not a redesign.
- **Superseded registry rows are kept as YAML comments**, immediately
  under the entry that replaced them, so the historical alternative is
  still visible to a reader without being a live, resolvable path.
- **`raw_lidar`/`photo_dem` are "fully enumerated"** (`pathtools` term
  for an entry with no `template:`, defined only by `overrides:`) —
  this is the correct modeling for genuinely irregular per-site
  filenames; an unmatched lookup is an error, not a silent fallback.

### 2. Setup per driver script

Each of `lidar/02.R`, `03_evaluate_dtm.R`, `05_veg_heights.R`,
`06_compare_ground_sources.R`, `07_floor_corrected_ground.R`,
`08_veg_height_validation.R` gets, right after the existing
`R/`-sourcing line:

```r
library(pathtools)
set_path_scheme("lidar/data/paths.yml")
```

This is a small amount of repetition across ~6 scripts, but it matches
the existing convention of repeating the `a <- lapply(list.files("R/",
...))` sourcing line at the top of every driver script — consistent
with, not a departure from, current practice.

### 3. Reports move — `lidar/output/` → `../lidar_reports/`

Unchanged from v1:
- `reports_root` resolves relative to the working directory at lookup
  time (per `pathtools`'s own documented relative-path semantics),
  which for this repo is always the repo root — consistent with how
  the existing Rmds already anchor themselves (`setwd(here::here())`).
- `git rm -r --cached lidar/output/` to stop tracking the 54 files
  currently committed there. Regenerate fresh at the new location
  (recommended — fully reproducible) vs. move as-is is still an open
  question for the implementer (§ below).
- Remove the now-empty `lidar/output/` tree from the repo.
- Nothing new needed in `.gitignore` — `../lidar_reports` is outside
  the repo tree entirely.

### 4. Migration — call sites to update

Same inventory as v1, each now becoming a `get_path()` /
`ensure_parent()` / `audit_paths()` call instead of a
`lookup_paths()` call:

**Input resolution** (replaces direct `paths.csv` reads):
- `lidar/02.R:122-128` (`read_csv`, `preferred` coercion, filter) →
  `get_path("raw_lidar", site = site, date = date)`.
- `lidar/05_veg_heights.R:98` (inline `preferred` filter) → same.
- ECP default arguments everywhere below → `get_path("ecp_path")`
  (no parameters needed — it's a constant).

**Scratch-tree paths:**
- `lidar/02.R:167-182` (`base_output`, `cleaned_catalog_dir`,
  `ground_raster_template`, `reprojected_dir`, `reprojected_path`) and
  `:235` (the `update_path()` call itself) → `get_path("cleaned_tiles",
  ...)`, `get_path("ground_raster", ...)`, `get_path("reprojected_dir",
  ...)`. The per-CSF-parameter-set loop becomes a direct
  `get_path("ground_raster", site=, date=, target_epsg=,
  csf_threshold=, csf_res=, csf_rigidness=, raster_res=)` call — no
  intermediate template + substitution step.
- `lidar/03_evaluate_dtm.R:57` (`base_output`).
- `lidar/05_veg_heights.R:64-75` (`ground_raster`, including the
  commented-out legacy alternative) and `:108-114` (`summer_clean_dir`,
  `output_dir`, `output_tif`).
- `lidar/06_compare_ground_sources.R:30-102` (the `sources` list for
  all three sites) → `get_path("ground_raster", ...)` for the
  UAS-derived DTMs, `get_path("massgis_tile", tile = ...)` and
  `get_path("photo_dem", site = , source = )` for the reference
  rasters.
- `lidar/07_floor_corrected_ground.R:47-52` (`summer_raster_dir`,
  `massgis_path`) and `:103-106` (`corrected_ground_tif`, wrapped in
  `ensure_parent()` before `terra::writeRaster()`).
- `lidar/08_veg_height_validation.R:46-115` (ECP path,
  `summer_raster_dir`, `massgis_floor_tif`, `canopy_top_tif`, spring
  DTM path).
- `R/report_dtms.R`'s `ecp_path`/`base_output` default arguments.
- `R/reproject_las.R`'s `vgrid` default argument;
  `R/reproject_las_pdal.R`'s `vgrid` default (same geoid grid path,
  duplicated) → both become `get_path("geoid_grid")`.

**Reports-root paths:**
- `R/report_dtms.R`'s `eval_dir` default argument →
  `get_path("dtm_eval_report", site = , date = )`.
- `R/report_ground_sources.R`'s `output_dir` default argument →
  `get_path("ground_comparison_report", site = )`.
- `lidar/07_floor_corrected_ground.R:32` and
  `lidar/08_veg_height_validation.R:39` (`output_dir`).
- `rmd/dtm_evaluation_report.Rmd`'s `params:` YAML defaults (`ecp_path`,
  `base_output`, `eval_dir`).
- `rmd/ground_source_comparison.Rmd`'s `params:` YAML default
  (`ecp_path`) and its `output_dir`/`[site]` substitution (`:14, 39` —
  becomes redundant once `get_path()` does this centrally) and its
  standalone-knit fallback `sources` list (`:46-58`, duplicates
  `lidar/06_compare_ground_sources.R`'s `oth` block).

**Documentation-only hardcoded paths** (roxygen `@examples`, not
executed) — update to demonstrate `pathtools::get_path()` usage
instead of literal strings: `R/las_needs_reprojection.R:61`,
`R/load_ecp.R:66`, `R/rasterize_ground.R:30-32`,
`R/rasterize_veg_heights.R:34-37`, `R/sample_dtm.R:50`,
`R/reproject_las_lastools.R:111-117`, `R/reproject_las_pdal.R:85-97`.

**`R/update_path.R`:** delete the file entirely once the migration
above leaves it with zero call sites (confirm with
`grep -rn "update_path(" R/ lidar/` first). Also drop its mentions from
`lidar/ARCHITECTURE.md`.

**Lowest priority, not executed:** `R/clean_and_tile.R:2-10`'s dead
`if (FALSE) {...}` scratch block — update or delete, doesn't affect
behavior either way.

### 5. Documentation updates

- `CLAUDE.md` (project) — rewrite the "Output directories" bullet and
  the `lidar/data/` gitignore bullet in "Data conventions" to describe
  `paths.yml`/`pathtools::get_path()` instead of
  `paths.csv`/`update_path()`. Note the new `pathtools` GitHub
  dependency somewhere appropriate (README or a new "Dependencies"
  note).
- `lidar/ARCHITECTURE.md` — rewrite "Where things live" (the
  `paths.csv` description, the `update_path()` mention, and the
  now-false "small, committed-size artifacts" claim) to describe
  `paths.yml`/`pathtools` and `reports_root`.
- `lidar/readme.md` — fix the ECP filename to
  `..._One_Sheet.xlsx` (resolved above).
- `dev/worklog.md` — log the migration once done, per the existing
  convention.

## Open questions for the implementer

1. **Existing `lidar/output/` content: regenerate vs. move?**
   Regenerating is cleaner (proves the new system works end-to-end)
   but needs `E:/uas_scratch` inputs to still be present; moving is
   faster and needs neither. Recommend regenerating at least the `rr`
   site (fastest, already fully validated per `dev/workplan.md`) and
   moving the rest, unless the implementing session finds moving
   everything is preferable at the time.
2. **`parameters:` `values:` enumeration.** The sketch above leaves
   every parameter unconstrained (declared, no allowed-values list).
   Worth adding a firm `site: [rr, oth, wel, nor, peg, ...]` list once
   convenient — not required for this migration to work, since
   `pathtools` only uses it for optional typo-catching validation.
3. **Fold in `inundation_metrics/Data/sites.txt` now, or later?**
   Recommend later — see "Non-goals."

## Verification (once implemented)

- Re-run `lidar/02.R` through `lidar/08_veg_height_validation.R`
  end-to-end for at least one site; output (DTM rasters, report
  numbers) matches pre-migration results, with every path coming from
  `pathtools::get_path()`.
- `grep -rn "E:/uas_scratch\|X:/legacy\|X:/projects/uas/sites\|X:/scratch/bcompton\|lidar/output" lidar/ R/ rmd/`
  returns nothing outside `paths.yml` and roxygen examples demonstrating
  `get_path()` usage — historical mentions in `dev/worklog*.md`/`NEWS.md`
  are expected and left alone.
- `grep -rn "update_path(" R/ lidar/ rmd/` returns nothing;
  `R/update_path.R` no longer exists.
- New reports land under `../lidar_reports/<site>_*/`; nothing
  reappears under a resurrected `lidar/output/`.
- `git status` after a full pipeline re-run shows no new large files
  staged in the repo.
- `pathtools::path_scheme("lidar/data/paths.yml")` builds with no
  warnings; `summary()` on it shows the expected entry/override counts
  with nothing unexpected flagged.
