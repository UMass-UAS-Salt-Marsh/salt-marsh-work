# Driver refactor plan — tuning vs. production split

Status: **draft, awaiting review.**
Nothing in this file has been implemented yet.

This is a separate plan from `dev/workplan.md`,
which currently tracks the in-flight per-site rollout
(manual steps, mid-execution).
Keeping them separate on purpose so this refactor doesn't
collide with that in-progress work.
When to actually apply this refactor —
before, after, or interleaved with the rest of the rollout —
is a sequencing call for the user, not assumed here.

## Problems this solves

1. **Driver files don't run in file-name order.**
   Today's real execution order is
   `02.R` → `03_evaluate_dtm.R` → `06_compare_ground_sources.R` →
   `07_floor_corrected_ground.R` → `08_veg_height_validation.R` →
   `05_veg_heights.R` — the final deliverable (`05`) runs last
   despite the lowest number.
2. **The CSF tuning grid (`02.R`'s loop) produces rasters that mostly
   aren't the ground reference actually used downstream** — for `rr`,
   the winner is a MassGIS + floor-bias hybrid (`07`), not any CSF DTM.
   That fork is currently implicit, buried in comments.
3. **Run parameters are re-declared per file** — `site`, `target_epsg`,
   dates, `workers`/`chunk_size`/`chunk_buffer`, and especially the
   `best_csf_spring`/`best_csf_summer` values, which get hand-copied
   between `06`, `07`, `08`, and `05` (e.g. `08_veg_height_validation.R`
   lines 112–115 re-types `06`'s CSF literals by hand). High drift risk
   when rolling out a new site.

## Design overview

Split the pipeline into two phases, matching how the work actually
happens:

- **Tuning** — exploratory, per-flight or per-site, ends with a human
  reviewing a report and recording a decision. Not something we
  automate end-to-end; the checkpoint is intentional.
- **Production** — mechanical, no judgment calls left once tuning's
  decisions are recorded. Safe to collapse into single-call functions.

```
lidar/
  tuning/
     01_csf_tuning.R                 # per flight
     02_ground_reference_selection.R # per site
     veg_height_check.R              # optional, on-demand (see below)
  production/
     01_ground_raster.R              # per flight
     02_veg_heights.R                # per flight
  paths.yml   # existing, MOVED here from lidar/data/
  runs.yml    # NEW — per-site/flight run config (see below), sits
              # alongside paths.yml rather than under data/
  data/
     RedRiver_11May2022.csv   # unaffected, stays put
     ... (raw/scratch inputs, gitignored except the above)
  R/
     prepare_flight.R      # NEW — shared reproject + clean_and_tile
     ... (existing R/ functions, signatures adjusted where noted)
  00_explore_data.R         # renamed from 01_explore_data.R; still
                             # scratch-only, outside the tuning/production
                             # sequence
```

Old → new mapping:

| Old | New | Scope |
|---|---|---|
| `02.R` (reprojection + clean_and_tile + CSF grid) | `tuning/01_csf_tuning.R` | per flight |
| `03_evaluate_dtm.R` | folded into `tuning/01_csf_tuning.R` (grid + scoring are one step) | per flight |
| `06_compare_ground_sources.R` | `tuning/02_ground_reference_selection.R` | per site |
| `07_floor_corrected_ground.R` | folded into `tuning/02_ground_reference_selection.R` (the hybrid build is part of *deciding* the reference) | per site |
| `08_veg_height_validation.R` | `tuning/veg_height_check.R`, demoted — see below | per site, optional |
| (new) | `production/01_ground_raster.R` | per flight |
| `05_veg_heights.R` | `production/02_veg_heights.R` | per flight |
| `01_explore_data.R` | `00_explore_data.R` | n/a |

Numbering is now only meaningful *within* each phase — "tuning before
production" is implied by the directory name, so we don't need a
single global sequence across six-plus files.

## The config file: `lidar/runs.yml`

Central handoff between tuning's decisions and production's inputs —
this is what kills the copy-pasted-CSF-literals problem. Shape,
pre-populated with `rr`'s current values as the template to copy for
a new site:

```yaml
sites:
  rr:
     target_epsg: 6491
     workers: 25
     chunk_size: 100
     chunk_buffer: 20
     flights:
        "2022_05_14":
           season: spring
           best_csf: {csf_threshold: 0.01, csf_res: 0.1,
                      csf_rigidness: 2, raster_res: 0.25}
        "2022_08_10":
           season: summer
           best_csf: {csf_threshold: 0.12, csf_res: 0.2,
                      csf_rigidness: 2, raster_res: 0.25}
     ground_reference:
        method: massgis_plus_floor   # or: csf
        massgis_tile: "19TDG412612"  # or a list, for multi-tile sites
        floor_bias_source_flight: "2022_08_10"
```

- **Written by a human**, not by a helper function, at least for now
  (per the user's answer — automated config-writing is future work,
  not part of this refactor).
- `massgis_tile` accepts a single id or a list, so it composes with the
  multi-tile mosaic work already planned for `wel` in
  `dev/rollout_workplan.md` (`merge_massgis_tiles()`) without
  contradicting it.
- Lives at `lidar/runs.yml`, not under `lidar/data/` — checked
  `lidar/data/.gitignore` and it only excludes `*.lax`/`*.las`/`*.xls`/
  `*.xlsx` by extension, so `paths.yml` was never actually blanket-
  ignored there; moving both YAMLs up to `lidar/` needs no gitignore
  change either way, it just sidesteps the question entirely.

### How it's read — and why differently from `paths.yml`

Checked `pathtools`'s installed source to get this right rather than
guessing. `set_path_scheme()` / `get_path()` use genuine **global
session state**: `set_path_scheme("lidar/paths.yml")` (called once per
driver, right after sourcing `R/`) stores the scheme in a
package-internal environment; every later `get_path(...)` call reads
it back implicitly via `path_scheme_current()` — no scheme argument
needed at each call site. `get_path()` does accept an explicit
`path_scheme = ` override, and `pathtools` calls out exactly why: a
parallel worker (`future`/`targets`) doesn't inherit the main
process's global state, so code that must call `get_path()` *inside* a
worker needs the scheme passed in. Our drivers never hit this — every
path is resolved in the main process before `plan(multisession, ...)`,
and only plain values (a raster path, a tile directory) go into the
parallelized functions (`clean_and_tile()`, `rasterize_ground()`);
nothing inside a chunk callback calls `get_path()` itself.

For `runs.yml` I'd deliberately **not** copy that global-state pattern.
`R/get_run_config.R` (`get_run_config(site, date = NULL)`) just reads
`lidar/runs.yml` fresh on every call — it's tiny, sub-millisecond —
located via a new `run_config` entry in `paths.yml` (so `paths.yml`
stays the one place that says "where do files live"), and indexes into
`sites[[site]]` / `flights[[date]]`. No `set_run_config()`, no second
piece of mutable global state to keep in sync or forget to pass into a
worker. Each driver calls it once near the top and immediately
destructures the result into plain locals (`workers`, `chunk_size`,
`best_csf`, ...) — the same shape drivers already use today, and the
same reason the parallel-worker gotcha never comes up for config
either: nothing parallelized reads it directly.

## Shared prep step: `R/prepare_flight.R`

Both phases need a cleaned, tiled point cloud before anything else.
Today that logic (conditional reprojection + `clean_and_tile()`) is
duplicated between `02.R` and `05_veg_heights.R`. Extract it to one
function, `prepare_flight(site, date, target_epsg, workers,
chunk_size, chunk_buffer)`, returning the cleaned-tile directory path.
Both `tuning/01_csf_tuning.R` and `production/01_ground_raster.R` /
`production/02_veg_heights.R` call it; it stays skip-if-exists, so no
behavior change, just de-duplication.

## Stage-by-stage plan

### `tuning/01_csf_tuning.R` — per flight

Grid-search CSF parameters and score them against ECPs in one step
(today's `02.R` grid loop + `03_evaluate_dtm.R`, combined since they're
really one activity: search, then look at the report, then decide).
Reads `site`/`date`/grid from a script-level input (not `runs.yml` —
there's nothing to look up yet, this *produces* `best_csf`). Ends with
you reviewing the DTM evaluation report and hand-editing
`best_csf` into `runs.yml` for that flight.

### `tuning/02_ground_reference_selection.R` — per site

Compares each flight's `best_csf` DTM (from Tuning-1) against ortho
photo DEMs and MassGIS (today's `06`), and — if MassGIS looks better —
estimates the floor bias and builds the hybrid raster (today's `07`).
Ends with you hand-editing `ground_reference` into `runs.yml` for that
site.

### `tuning/veg_height_check.R` — optional, on demand

This is today's `08_veg_height_validation.R`. Per your read that it
feels less essential: I'd keep the capability (it's real, already-built
validation of the `rr` ground-reference choice against field
`veg_height_m`) but **demote it from a required gate to an optional
diagnostic** — no number prefix, not part of the `01`/`02` sequence,
not blocking the `ground_reference` decision. Run it by hand when you
want extra confidence in a new site's pick, especially once ECPs there
have `veg_height_m` populated. Nothing reads its output automatically.
If you'd rather drop it entirely, that's also fine — say so and I'll
leave the functions in `R/` unused rather than deleting working code,
and skip creating this file.

### `production/01_ground_raster.R` — per flight

New. Given a site+date already present in `runs.yml`, produce *the*
ground raster for that flight with no grid search: reads
`ground_reference.method` and dispatches —

- `"csf"` → `rasterize_ground()` once, using that flight's `best_csf`.
- `"massgis_plus_floor"` → build the hybrid raster the way `07` does
  today, generically (site's `massgis_tile` + the floor bias estimated
  from `floor_bias_source_flight`'s cached ECP residuals from
  Tuning-2).

This is the piece that only exists because of the phase split — today
there's no way to get a ground raster for a *new* flight of an
already-tuned site without either re-running the whole grid or
hand-copying CSF literals.

### `production/02_veg_heights.R` — per flight

Today's `05_veg_heights.R`, unchanged in spirit: build the canopy-top
raster, normalize against `production/01`'s ground raster, write the
height-distribution GeoTIFF. Stops reading CSF literals directly —
gets the ground raster path from `production/01`'s output (or calls it
if not already materialized).

## Decisions locked in with the user (2026-09-15)

1. **Config is human-written**, not helper-written, at least
   initially. Ship `runs.yml` pre-populated with `rr`'s current values
   as the template to copy for a new site.
2. **`08`/`veg_height_check.R` is demoted to optional/on-demand**, not
   deleted — see above. Open to dropping it entirely if you'd rather,
   once you see it as a standalone file.
3. **CSF tuning runs per flight until further notice.** If consensus
   parameters emerge across flights/sites, they're expected to become
   global defaults at that point — not building that mechanism now,
   just not designing anything that would block it later (e.g.
   `best_csf` staying keyed under `flights.<date>` rather than
   something harder to collapse).

## Open items to resolve during implementation (low risk, not blocking review)

- Exact function signatures for the refactored `select_ground_reference()`
  / hybrid-raster-building logic pulled out of `06`/`07` — I haven't
  inspected `R/report_ground_sources.R`, `R/estimate_floor_bias.R`, etc.
  line-by-line yet; expect minor signature adjustments once I do.
- Whether `production/01_ground_raster.R`'s two method branches are one
  file with an `if` or two small functions dispatched from one driver
  — implementation detail, no behavior difference.
- `lint-changed` pass on every new/touched file before commit, per
  usual convention — not run yet since nothing's written.

## Out of scope for this refactor

- Making CSF parameters actually global across flights (item 3 above
  — future work, contingent on evidence).
- Automated config-writing helpers.
- Collapsing Tuning into one orchestrated function — the human
  checkpoint is intentional and stays.
- The `wel`/`nor`/`peg` site rollout itself — tracked separately in
  `dev/workplan.md` / `dev/rollout_workplan.md`; this refactor changes
  *where* that rollout's steps live, not what they are, and sequencing
  the two is left to the user.

## Migration steps (checklist, once this plan is approved)

- [ ] Move `lidar/data/paths.yml` → `lidar/paths.yml`; update every
      driver's `set_path_scheme("lidar/data/paths.yml")` call to
      `set_path_scheme("lidar/paths.yml")`.
- [ ] Create `lidar/runs.yml`, pre-populated with `rr`'s current
      parameters (from `06_compare_ground_sources.R` and
      `07_floor_corrected_ground.R`).
- [ ] Add a `run_config` entry to `paths.yml` pointing at
      `lidar/runs.yml`.
- [ ] Add `R/get_run_config.R`.
- [ ] Add `R/prepare_flight.R`; update call sites.
- [ ] Create `lidar/tuning/01_csf_tuning.R` from `02.R` + `03_evaluate_dtm.R`.
- [ ] Create `lidar/tuning/02_ground_reference_selection.R` from `06` + `07`.
- [ ] Create `lidar/tuning/veg_height_check.R` from `08` (or drop, per
      your call once you see it isolated).
- [ ] Create `lidar/production/01_ground_raster.R` (new).
- [ ] Create `lidar/production/02_veg_heights.R` from `05_veg_heights.R`.
- [ ] Rename `01_explore_data.R` → `00_explore_data.R`.
- [ ] Delete the superseded originals once the new files are verified
      against `rr`'s existing outputs (skip-if-exists means this should
      be a no-op re-run, not a re-derivation).
- [ ] Update `lidar/ARCHITECTURE.md` (stage list, file references,
      mermaid diagram) to match.
- [ ] Update the root `CLAUDE.md` lidar section, which currently
      documents `lidar/data/paths.yml` as the scheme location.
- [ ] `lintr::lint()` every new/changed file before committing.
- [ ] Worklog entry summarizing the refactor.
