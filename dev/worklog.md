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
