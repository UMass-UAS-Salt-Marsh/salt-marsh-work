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

## 2026-08-24 — branch lidar

### Documentation reorganization: new `lidar/ARCHITECTURE.md`, trimmed `dev/work_plan.md`

`dev/work_plan.md` had grown to 788 lines mixing the active plan with
full phase-by-phase history and design rationale that already
duplicated `worklog.md`/`worklog-archive.md`. Split the durable
"how the pipeline works" content into a new `lidar/ARCHITECTURE.md`
(conceptual workflow + implementation, one subsection per pipeline
stage naming the driver script and `R/` functions involved, plus a
mermaid data-flow diagram from raw LAS through to the final 30-band
veg-height-distribution raster), scoped to the lidar pipeline only
per user direction — the hydrology and logger-recalibration
pipelines are out of scope for this doc.

Rewrote `dev/work_plan.md` down to just the still-open items: the
Phase 1.6 vertical-bias diagnostics (CORS station height, lever-arm
check — both untouched), the Phase 1.7 canopy-top ~15 cm
underestimate investigation, Phase 2's two open decision points
(fraction denominator, output resolution), and the Phase 3 site
rollout table. Confirmed every completed phase's narrative already
had worklog coverage before cutting it, so nothing was lost. Also
resolved four stale unchecked Phase 1.6a checkboxes (re-run Steps
0–2 / Phase 1 / ground comparison under the corrected CRS, wire the
result into vegetation heights) — Phase 1.7 and Phase 2 already did
this work; the checkboxes just hadn't been ticked.

Other small follow-ons: `lidar/readme.md`'s stale 4-step "Approach"
section (didn't reflect the current 8-driver pipeline) now points to
`ARCHITECTURE.md` instead; root `README.md` and `CLAUDE.md` each got
a one-line pointer to `lidar/ARCHITECTURE.md`. Deleted two untracked
stray files found while exploring: `Saltmarsh_CRS.md` (an old,
superseded draft of `CRS.md`, different content on the same topic)
and `Rplots.pdf` (an accidental R plotting artifact at the repo
root).

### Phase 1.7: floor-bias-corrected ground reference for veg heights

Resolved the Phase 1.5 decision point (which ground source to use for
vegetation heights) by refining the Phase 1.6 bias finding rather than
picking one of the two extremes discussed (raw MassGIS vs. the legacy
spring-lidar-DTM-relative approach). See `dev/work_plan.md` Phase 1.7
for the full write-up; summary here.

**Bias decomposition.** The UAS ground bias (Phase 1.6) isn't one
fixed number — spring mean bias +0.158 m vs. summer +0.215 m at `rr`.
Hypothesis: a roughly-constant instrument/PPK-level offset plus a
vegetation-density-dependent CSF ground-finding error (worse in
summer). New `estimate_floor_bias()` (`R/estimate_floor_bias.R`)
isolates the former by excluding below-floor outliers (MAD threshold)
then taking a low percentile of the remaining DTM-vs-ECP residuals.
Run against the cached `lidar_spring_ecp.csv`/`lidar_summer_ecp.csv`
via new `lidar/07_floor_corrected_ground.R`: floor bias = 0.103 m
(spring), 0.114 m (summer) — only 1.1 cm apart, strongly supporting
the constant-instrument-offset hypothesis, with the CSF error
accounting for nearly all the remaining spring→summer bias gap
(0.055 m spring, 0.101 m summer).

**Decision**: ground = MassGIS + 0.114 m (the summer floor bias),
written to
`lidar/output/rr_ground_comparison/massgis_plus_floor_summer.tif`.
Wired into `lidar/05_veg_heights.R` as the new default `ground_raster`
(replacing the hardcoded `spring_dtm` variable); legacy path kept as a
commented alternative.

**Empirical validation** (new `lidar/08_veg_height_validation.R` +
`R/rasterize_canopy_top.R`, a new top-of-canopy DSM function
analogous to `R/rasterize_ground.R`): compared predicted vegetation
height against the ECP dataset's field-measured `veg_height_m`
(independent of the fitting data — all 169 `rr` summer EVP points have
it). Option 3 (MassGIS + floor) beat the legacy spring-DTM approach on
every metric (mean bias −0.263 m vs. −0.305 m; RMSE 0.363 m vs.
0.397 m; 49.4% vs. 34.8% of points within 20 cm), by almost exactly
the margin the bias decomposition predicts.

**New finding, not yet addressed**: both options underestimate true
vegetation height by ~15 cm *before* the ground-reference difference —
common to both, so not a ground-reference artifact. Likely cause: the
canopy-top DSM (and the production `veg_dist_*.tif` raster) reads the
same last-return-only cleaned tiles from `clean_and_tile()` used
throughout this pipeline, and a last return can sit well below the
top of a multi-return pulse through dense marsh vegetation. This is a
bigger error source than the ground-bias issue just fixed. Logged as
an open item in Phase 1.7's checklist — needs its own investigation
before absolute (not just relative) vegetation heights from
`lidar/05_veg_heights.R` should be trusted.

**Files**: new `R/estimate_floor_bias.R`, `R/plot_floor_bias.R`,
`R/rasterize_canopy_top.R`, `lidar/07_floor_corrected_ground.R`,
`lidar/08_veg_height_validation.R`; edited `lidar/05_veg_heights.R`
(ground-reference wiring). All new `R/` files pass `lintr::lint()`
clean. Both new driver scripts were actually run for `rr` (not just
written) — see numbers above and
`lidar/output/rr_ground_comparison/veg_height_validation.csv`.

### Checked and refuted the last-return canopy-top hypothesis

Before investigating the ~15 cm canopy-top underestimate found above
any further, checked the premise directly: sampled a 40×40 m box from
the raw `rr` summer point cloud (~328k pulses,
`.../ppk_07Nov2022_cloud_1.las`), paired first/last returns by
`gpstime`. Result: every pulse has `NumberOfReturns == 2`, and
`Z_last - Z_first == 0` exactly for all of them — first and last
returns are byte-identical. So `clean_and_tile()`'s last-return
filtering isn't discarding any height information, and isn't the
explanation. Corrected the Phase 1.7 write-up in `dev/work_plan.md`
accordingly; the ~15 cm underestimate's actual cause is still open.

### Moved large derived rasters out of `lidar/output/`

`massgis_plus_floor_summer.tif` (~18 MB) and `canopy_top_summer.tif`
(~10 MB) were initially written to `lidar/output/rr_ground_comparison/`
alongside the small CSV/HTML comparison artifacts, but that directory
isn't gitignored and these are large derived rasters like the CSF
DTMs, not comparison-report artifacts. Moved both to
`E:/uas_scratch/lidar/rr/2022_08_10/zzzraster_epsg6491/`, next to the
other `rr` summer rasters, and updated the three scripts that
reference them (`lidar/07_floor_corrected_ground.R`,
`lidar/08_veg_height_validation.R`, `lidar/05_veg_heights.R`)
accordingly. The small `*_ecp.csv` caches stay in
`lidar/output/rr_ground_comparison/` as before.

### Phase 3 site scoping + Phase 2 reconciliation; found and fixed a CRS bug in the veg-height scripts

Asked to start Phase 3, researched site data first: only `wel` has
paired 2022 spring/summer clouds + ECPs (153 EVPs) ready to extend the
`rr` workflow to; `oth` has ECPs (145) but no raw point cloud at all
(photogrammetry-only); `nor` and `peg` have paired 2024 clouds but zero
ECPs each (matches `nor`'s existing documented blocker; `peg`'s wasn't
previously noted). Documented in `dev/work_plan.md` Phase 3. `wel`'s
clouds are larger than `rr`'s and would take a longer, memory-bound run
per `lidar/02.R`'s own notes — user asked to hold off on actually
running it for now.

User then clarified they meant **Phase 2** (vegetation strata), not
Phase 3. Checked its real status: `R/rasterize_veg_heights.R` /
`lidar/05_veg_heights.R` already implement almost exactly what Phase 2
planned (same bin scheme, same 0.5 m default resolution) under
different names than originally planned
(`R/rasterize_strata.R`/`lidar/04_vegetation_strata.R`) — but two things
were genuinely open: the fraction denominator doesn't match the
original "sums to 100%" design (uses all returns, not just in-range
ones — flagged as an open decision, not changed yet), and the script
had never actually produced output for `rr` (an empty, stale
`veg_dist_0.5m_chunks/` directory from 2026-05-21 was the only trace).
Reconciled in `dev/work_plan.md` Phase 2.

**Found a real bug while preparing to run it.** `lidar/05_veg_heights.R`
and `lidar/08_veg_height_validation.R` both read summer cleaned tiles
from `E:/uas_scratch/lidar/rr/2022_08_10/zzzcleaned/` — confirmed via
`readLASheader()` + `sf::st_crs()` to be **EPSG:26919** (legacy
NAD83/UTM19N), a stale directory from 2026-05-15. Every ground raster
used in Phase 1.7 (CSF DTMs, `massgis_plus_floor_summer.tif`) was built
from the current, correctly-named `zzzcleaned_epsg6491/` (EPSG:6491,
built 2026-08-21 by the same `lidar/02.R` run). Per `CRS.md` these two
CRSs carry a real ~0.5-1 m horizontal frame shift, not just a label
difference — so the canopy-top raster built earlier this session for
`lidar/08_veg_height_validation.R` was sampling a differently-positioned
point cloud than the ground rasters it was compared against.

Fixed both scripts by adding a `target_epsg` variable (matching
`lidar/02.R`'s own convention) and building the clean-tile path from it
instead of a bare `"zzzcleaned"` literal. Deleted the stale
`canopy_top_summer.tif` and its cached `_ecp.csv`, re-ran
`lidar/08_veg_height_validation.R`: numbers shifted slightly (opt2 mean
bias −0.305→−0.311 m, opt3 −0.263→−0.270 m; RMSE improved slightly in
both, as expected from removing spatial-mismatch noise) but the
Phase 1.7 conclusion is unchanged — option 3 still wins by essentially
the same margin. Updated `dev/work_plan.md` Phase 1.7 with corrected
numbers and this fix. Both files pass `lintr::lint()` clean after the
edit.

### Phase 2 completed: fixed a closure bug, a second CRS bug, and OOM crashes to get real veg-height output for rr

Continuing from the Phase 2 reconciliation above, tried to actually
run `lidar/05_veg_heights.R` and hit three separate, unrelated bugs
before it produced real output. Full details in `dev/work_plan.md`
Phase 2 (and Phase 1.7 for the second CRS bug, since it originated
there); summary here.

1. `R/rasterize_veg_heights.R`'s per-cell metric function (`fracs_fn`)
   was defined inside `compute_heights()`, which `catalog_map()`
   dispatches per chunk — every chunk failed with `could not find
   function "fracs_fn"`, reproduced even with `plan(sequential)`, so
   this was never a multisession-only bug and is almost certainly why
   Phase 2 never worked before. Fixed by promoting it to a top-level
   function (`R/bin_fractions.R`) with `bin_breaks`/`bin_names` baked
   into the `pixel_metrics()` formula via `bquote()`.
2. Promoting it to top-level fixed sequential execution but not
   `multisession` — `future`'s automatic global-detection doesn't see
   a symbol referenced only inside a formula. Tried embedding the
   function object itself as a literal via `bquote()`; that broke
   differently, since `pixel_metrics()` deparses the formula to text
   and reconstructs it inside a `data.table` call, and a deparsed
   multi-line function literal doesn't survive that round-trip
   (`unexpected 'if'`). Settled on: keep `bin_fractions` as a plain
   symbol, and have `compute_heights()` `source("R/bin_fractions.R")`
   itself (guarded by an `exists()` check) so each worker is
   self-sufficient.
3. Once chunks started actually running, they crashed with
   `std::bad_alloc`. Root cause: `massgis_plus_floor_summer.tif` was
   in EPSG:6348 (MassGIS's native CRS) while the point cloud is
   EPSG:6491 — `lidR::normalize_height()` does a raw coordinate lookup
   with no reprojection (unlike `sample_dtm()`), so every point missed
   the raster and fell into an unbounded-radius `knnidw` fallback.
   Fixed in `lidar/07_floor_corrected_ground.R` (see Phase 1.7 in
   `dev/work_plan.md`); re-ran `lidar/08_veg_height_validation.R`
   afterward and confirmed the Phase 1.7 numbers were unaffected
   (`sample_dtm()`'s CRS-aware sampling had already handled this
   correctly — only `normalize_height()`'s raw lookup was broken).
4. Also dropped `chunk_size` from 200 to 100 m in
   `lidar/05_veg_heights.R` — some `rr` summer tiles run 6-12 million
   last-return points per 200×200 m tile, and smaller chunks leave
   more memory headroom generally.

Also found and killed ~25 orphaned `Rscript.exe` processes still
running since 2026-08-21 (leftover `future::multisession` workers from
an interrupted earlier run, per user confirmation) — not the root
cause of the OOM crashes (that was the CRS bug above) but were
consuming memory alongside everything else while debugging.

After all four fixes, ran `lidar/05_veg_heights.R` for `rr` with its
real config (`workers = 25`, multisession): completed cleanly.
`E:/uas_scratch/lidar/rr/2022_08_10/zzzheights/veg_dist_0.5m.tif` now
exists — 30 bands, 1463×1571 px, EPSG:6491, values in [0,1]. Deleted
the stale pre-2026-05-21 `veg_dist_0.5m_chunks/` directory and my own
test artifacts. New file `R/bin_fractions.R`; edited
`R/rasterize_veg_heights.R`, `lidar/05_veg_heights.R`,
`lidar/07_floor_corrected_ground.R`. All pass `lintr::lint()` clean.

Left open (in `dev/work_plan.md` Phase 2): whether to fix the
fraction-denominator behavior (all returns vs. in-range returns only)
to match the original "sums to 100%" design — asked, not yet answered.

### Fraction-denominator question: no firm answer, not changing for now

Asked the user directly. Answer: not sure which denominator is
*correct*, but doesn't dislike the current ("all returns") behavior
either. Leaving `R/bin_fractions.R` as-is; updated the Phase 2 decision
point in `dev/work_plan.md` to record this as a leaning, not a final
call — revisit if a concrete reason to change surfaces (e.g. a
downstream model that assumes bands sum to 1).
