# Worklog archive

Older entries moved out of `dev/worklog.md` once that file gets long
(~500 lines) or its oldest entry is more than ~30 days old. Same
newest-on-top format as `worklog.md`; whole `## YYYY-MM-DD — branch
<name>` blocks are moved here, never split. Read `worklog.md` first
when recalling history; consult this file only if the answer isn't
there.

---

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

## 2026-08-20 — branch lidar

### Prep for the Phase 1.6a re-run: fix stale zzzraster paths, archive old output

Before re-running `lidar/02.R` under the new EPSG:6491 standard,
audited whether the downstream evaluation/comparison drivers would
read the right output. They wouldn't have, in two places:

- `rmd/dtm_evaluation_report.Rmd:40` hardcoded
  `file.path(params$base_output, "zzzraster")` — the old,
  un-namespaced directory name. Would have silently found the old
  26919 DTMs (still on disk) instead of the new 6491 ones, rather than
  erroring. Added a `target_epsg` param (default `6491`) threaded
  through `R/report_dtms.R` -> the Rmd, used to build
  `zzzraster_epsg<target_epsg>/` and passed to `load_ecp()`/
  `evaluate_dtm()`'s CRS args explicitly. `lidar/03_evaluate_dtm.R`
  now sets `target_epsg <- 6491L` explicitly too, matching
  `lidar/02.R`'s style.
- `lidar/06_compare_ground_sources.R:29,33` hardcoded `"zzzraster"`
  for the `lidar_spring`/`lidar_summer` source paths. Same fix: added
  a local `target_epsg <- 6491L` and built the path via
  `paste0("zzzraster_epsg", target_epsg)`.

`R/report_ground_sources.R`/`rmd/ground_source_comparison.Rmd` needed
no fix — `load_ecp()` there already has no `target_crs` override, so
it just picks up the new `6491` default; `sample_dtm()` reprojects the
ECPs to match each source raster's own CRS regardless, so comparing
DTMs and orthos in different CRSs continues to work correctly.

**Archived pre-existing rr output that would otherwise be overwritten**
(these three directories are not, and won't be, EPSG-namespaced —
namespacing every report driver's output felt like more churn than
warranted for CSV/PNG/HTML artifacts that are cheap to regenerate;
archiving the one prior run was simpler):

```
git mv lidar/output/rr_2022_05_14        lidar/output/rr_2022_05_14_epsg26919
git mv lidar/output/rr_2022_08_10        lidar/output/rr_2022_08_10_epsg26919
git mv lidar/output/rr_ground_comparison lidar/output/rr_ground_comparison_epsg26919
```

New runs of `lidar/03_evaluate_dtm.R` / `lidar/06_compare_ground_sources.R`
will write to the now-freed plain names
(`lidar/output/rr_2022_05_14/`, etc.), keeping the 26919-era results
available for comparison rather than lost.

Not fixed / intentionally left alone: `lidar/05_veg_heights.R:46`
still points at the old `zzzraster/csf_th0.01...` DTM — out of scope,
since that file is paused pending the Phase 1.5 ground-reference
decision (see below in this plan), not part of this re-run.

All touched R/lidar/Rmd files lint clean.

### Phase 1 bookkeeping: check off completed items, record CSF choice

Noticed (prompted by the user) that every checkbox under Phase 1 in
`dev/work_plan.md` was still unchecked, even though the work is done —
`R/load_ecp.R`, `R/sample_dtm.R`, the `evaluate_dtm()` assembly/
reporting split, the `lidar/02.R` wiring, and `lidar/03_evaluate_dtm.R`
all exist and match the plan's spec. Checked them off.

**"Inspection and selection" backfilled here since it never got its
own entry.** The 4-parameter CSF grid (`csf_th0.005/0.01/0.06/0.12`)
is the only one actually rasterized for `rr` — the 18-run expanded
grid added to `lidar/02.R` on 2026-05-20 was never run to completion
for this site (`E:/uas_scratch/lidar/rr/*/zzzraster/` has exactly 4
`csf_*.tif` files per date, matching the original grid, not 18).
Ranked by overall RMSE from the actual eval CSVs:

- **Spring (2022-05-14):** `csf_th0.01_res0.1_rgd2_0.25m` wins,
  RMSE 0.174 m (next-best 0.179 m).
- **Summer (2022-08-10):** `csf_th0.12_res0.2_rgd2_0.25m` wins,
  RMSE 0.245 m (next-best 0.250 m).

Both match what `lidar/06_compare_ground_sources.R`'s `best_csf_*`
comments already say — this just gives that choice a proper record.

**Gap, not silently resolved — and a correction to how I first framed
it.** The plan calls for ranking by the cleanest-signal class first
(secondary check via all-class RMSE). I initially wrote this up as
"bare-class RMSE" and went looking for "which ECPs measured bare
ground" — the user corrected that: **no ECP measures bare ground as
distinct from vegetated ground.** Every ECP is a ground-surface
elevation measurement, full stop; the CSF's whole job is to find that
same ground surface in the lidar returns even where vegetation sits
above it. What varies by ECP class is the vegetation cover overhead,
which determines how directly comparable the CSF's ground
classification is to the ECP at that point — cleanest where there's
least vegetation to see through, most confounded (systematically
positive residuals) under tall Spartina/Phragmites/Iva, per the
plan's own "Saltmarsh-specific notes for the report."
`evaluate_dtm()` groups by ECP `subclass`, and for `type == "EVP"`
(the point type actually used — see 2026-05-21 below) `subclass` is a
bare numeric code (1-12) with no cover-type mapping anywhere in this
codebase or its data files. Cross-checked `type`/`subclass` in the ECP
xlsx itself (via `load_ecp()`) and confirmed there's no legend to
resolve "which code(s) are open/unvegetated" without an outside source
(Josh Ward's thesis, or the original classification-points spreadsheet
in `X:/legacy/gdrive/2022_23_field_data/Classification Points/`). Went
with all-class RMSE only; flagging this as an open gap in
`dev/work_plan.md` rather than fabricating a cover-type ranking.

### ECP source-CRS question resolved via Josh Ward's thesis

The blocking open question from yesterday — whether `R/load_ecp.R`'s
hardcoded `source_crs <- 26919L` for the ECPs is actually correct, or
whether the ECPs are secretly NAD83(2011) mislabeled as legacy NAD83 —
is resolved. The user checked Josh Ward's thesis (who collected the
GCPs/ECPs): states they were measured in NAD83/UTM19N, NAVD88 — legacy
NAD83, matching the hardcoded value exactly. No code change needed;
`R/load_ecp.R` was already correct, and `sample_dtm()`'s CRS-aware
reprojection has been doing the right thing all along.

Updated `CRS.md` (moved this from "blocking open question" to
resolved, updated the "Historical / legacy usage" section to cite the
thesis as a confirmed fact rather than an assumption), `R/load_ecp.R`'s
roxygen doc (cites the thesis), and `dev/work_plan.md`'s Phase 1.6a
checklist. The geoid model used to compute the ECPs' NAVD88 heights
(GEOID12B vs. GEOID18) isn't stated in the thesis and remains a
separate, lower-priority open question.

**Only remaining blocker on flipping `target_epsg` defaults:** the
UTM19N (EPSG:6348) vs. Massachusetts Mainland State Plane (EPSG:6491)
pick, still open in `CRS.md`.

### Provisionally picked Mass State Plane; flipped target_epsg defaults

User's call: proceed with **EPSG:6491 (NAD83(2011) / Massachusetts
Mainland State Plane)** as the projected-CRS standard, provisionally.
Updated `CRS.md` (resolved the "two options" comparison into a
decision, with the UTM19N writeup kept for reference) and
`dev/work_plan.md`'s Phase 1.6a to match.

With both open questions resolved (ECP-CRS this morning, projected-CRS
pick just now), flipped every hardcoded `26919` default to `6491`:

- `R/reproject_las.R` (`target_epsg`), `R/las_needs_reprojection.R`
  (`target_epsg`), `R/load_ecp.R` (`target_crs` — `source_crs` stays
  `26919L`, confirmed correct for the ECPs), `R/evaluate_dtm.R` (`crs`,
  used only to label points for `residual_map`), `R/plot_residual_map.R`
  (`crs`, same). `R/reproject_las_pdal.R` has no default of its own
  (required arg) — updated its example to `6491` instead.
- `lidar/02.R`: added an explicit `target_epsg <- 6491L` to the
  run-level parameters block rather than relying on scattered function
  defaults; threaded it through `las_needs_reprojection()`,
  `reproject_las()`, and `load_ecp(..., target_crs = target_epsg)`.
  **Also namespaced `zzzcleaned/` and `zzzraster/` by
  `_epsg<target_epsg>`** (and the reprojected LAS filename suffix,
  which already varied by target) — without this,
  `clean_and_tile()`'s skip-if-exists check (purely
  "does the output dir have any `.las` files", independent of what
  produced them — see `R/clean_and_tile.R`) would have silently reused
  the existing `26919`-CRS cleaned tiles on the next run instead of
  re-cleaning under the new target, defeating the whole point of the
  switch.
- `R/reproject_las_lastools.R`'s example (`target_epsg = 26919`) was
  **not** changed — that path exists specifically to reproduce
  historical LAStools output, so it should keep pointing at the
  historical CRS. `R/spatial_plotting.R`'s hardcoded `crs = 26919` was
  also left alone — unrelated hydrology/logger-recalibration code, out
  of scope.

All seven touched R/lidar files lint clean. Not run end-to-end this
session — `dev/work_plan.md`'s remaining Phase 1.6a items (re-run
Step 0/1 for `rr`, re-run Phase 1 CSF tuning + evaluation, re-run
`06_compare_ground_sources.R`, then finally wire the ground reference
into `05_veg_heights.R`) are still open, to be triggered when ready.

## 2026-08-19 — branch lidar

### Phase 1.5 decision point revisited; CRS/geoid standard investigation

Picked back up the Phase 1.5 loose end: `lidar/06_compare_ground_sources.R`
had already been run for `rr`, `oth`, and `wel` (results on disk under
`lidar/output/*_ground_comparison/`), but the plan's final step — record
the chosen ground reference and update `lidar/05_veg_heights.R` — was
never done. Results are unambiguous and consistent across all three
sites: MassGIS beats every UAS-derived source by a wide margin, with
near-zero bias (rr: RMSE 0.044 m / bias +0.003 m vs. best lidar's
0.174 m / +0.165 m; oth and wel show the same pattern).

**Found a real correctness gap before wiring MassGIS in.** `rr`'s point
cloud is reprojected to EPSG:26919 (legacy NAD83, no realization) by
`lidar/02.R`; the MassGIS raster is EPSG:6348 (NAD83(2011)/UTM19N) —
confirmed via `terra::crs()`. `lidar/readme.md`'s own CRS section
already documents this as a real ~0.5–1 m horizontal frame shift for
this region. Read `lidR:::normalize_height.LAS` (used by
`rasterize_veg_heights()`) and confirmed it samples a raster at the
LAS's raw X/Y via `raster_value_from_xy()` with **no CRS check or
reprojection** — plugging MassGIS in as-is would have silently
mis-sampled by up to ~1 m.

Rather than patch around this with a one-off raster reprojection, the
user asked to establish an actual CRS/datum/geoid standard for the
project and a migration path, since more sites are coming. Spent this
session researching that instead of touching code. New file
**`CRS.md`** (project root) documents the findings and decisions in
full; summary of what's new:

- **GEOID18 is the current NGS standard geoid model for CONUS** (since
  ~2019, before our 2022 flights and MassGIS's 2023-processed lidar).
  This project's pipeline (`R/reproject_las_pdal.R`) still defaults to
  GEOID12B (`g2012bu0.gtx`).
- **NAPGD2022/GEOID2022 (the next-gen NSRS replacement) is NOT yet
  officially released** — confirmed by fetching NGS's own site today;
  NAD83/NAVD88 remain official, FGCS approval vote targeted
  early-to-mid 2026, several-month transition to follow. Too early to
  target; current standard is NAD83(2011) + NAVD88/GEOID18.
- **The Phase 1.6 "wrong GEOID12B tile" hypothesis is very likely
  wrong.** The original field notes claimed a choice between
  `g2012bu0.gtx` (CONUS) and `g2012bu4.gtx` ("around MA coast"). Checked
  PROJ's actual grid catalog (`cdn.proj.org`): GEOID12B's real regional
  files are `g2012ba0`/`g2012bg0`/`g2012bh0`/`g2012bp0`/`g2012bs0` (AK,
  Guam, HI, PR/VI, American Samoa) plus a single unified `g2012bu0` for
  all of CONUS. **There is no `g2012bu4`.** So the observed +10–16 cm
  UAS bias isn't a tile mixup — `g2012bu0.gtx` (what we already use) was
  always the correct, only CONUS tile. GEOID12B → GEOID18 differences in
  this region are typically a few cm, not 10+, so switching geoid models
  is correct practice but won't by itself close that bias gap. The
  lever-arm-misconfiguration hypothesis (Phase 1.6 item 2) is now the
  leading candidate.
- **GEOID18's CONUS grid is available as a ready GeoTIFF** from PROJ's
  own CDN — `https://cdn.proj.org/us_noaa_g2018u0.tif` (verified
  reachable, 16.7 MB) — no need to deal with NGS's native `.bin`/`.asc`
  distribution. PROJ accepts GeoTIFF grids the same way as `.gtx` via
  `+geoidgrids=`.
- Confirmed EPSG codes via the local PROJ database
  (`C:/OSGeo4W/share/proj/proj.db`) rather than guessing: NAD83(2011)
  geographic = 6318 (2D) / 6319 (3D); NAD83(2011) UTM19N = 6348;
  NAD83(2011) Massachusetts Mainland (State Plane) = 6491 (meters) /
  6492 (ftUS); legacy equivalents 4269 / 26919 / 26986 / 2249.
- User's call on the go-forward projected standard: **Massachusetts
  Mainland State Plane, NAD83(2011), EPSG:6491** (not UTM19N), for
  consistency with most MassGIS/state data products — though the
  specific MassGIS lidar bare-earth tiles used in the Phase 1.5
  comparison are themselves in UTM19N/EPSG:6348, so that convention
  isn't perfectly uniform even within MassGIS's own catalog.

**New, higher-priority finding while writing this up:** `R/load_ecp.R`
hardcodes `source_crs <- 26919L`, documented as "the CRS the ECP
coordinates were collected in" — asserted, not derived from any
metadata (`JoshSurveyPoints_AllSites_README.txt` has no datum info).
`R/evaluate_dtm.R` and `R/plot_residual_map.R` inherit the same
26919-as-project-standard assumption. If the ECPs were actually
collected in NAD83(2011) (very plausible for modern RTK/GNSS survey
gear) and mislabeled as legacy 26919, then `sample_dtm()`'s
CRS-aware reprojection (`sf::st_transform(ecp, crs = raster_crs)`,
added back on 2026-05-xx) would apply a real ~0.5–1 m transform to
coordinates that don't need one — a **new**, currently-undetected
error, not a fix. Flagged as the top-priority open item in `CRS.md`;
**not flipping any horizontal `target_epsg` default in this codebase
until it's resolved.** The vertical geoid fix (GEOID12B → GEOID18) has
no such dependency and is being adopted regardless.

Also note: `WebSearch` returned a hard API error all session ("tool
type not supported for this model"); research relied on `WebFetch`
against known URLs (several NGS/mass.gov pages blocked or 404'd) plus
direct `curl` from Bash, which worked fine for reaching
`cdn.proj.org` and most of `geodesy.noaa.gov`.

Next: acquire the GEOID18 grid and update `reproject_las()`'s default
`vgrid` (safe, vertical-only change); leave horizontal `target_epsg`
defaults untouched pending the ECP-CRS question; `dev/work_plan.md`'s
Phase 1.5/1.6 sections rewritten to match (see that file — not
duplicating the full plan text here).

Followed through on the safe part: downloaded
`us_noaa_g2018u0.tif` (GEOID18 CONUS grid) to
`X:/legacy/gdrive/UMassAir User Resources/LASTools/Geoid Transformation
GTX Files/geoid18/` (with a `README.txt` noting source/date, per the
existing `geoid12b/` convention), smoke-tested it with a real PDAL
`filters.reprojection` pipeline (`+geoidgrids=us_noaa_g2018u0.tif`,
exit 0 — GeoTIFF grids work identically to `.gtx`), and updated
`R/reproject_las.R` / `R/reproject_las_pdal.R`'s default `vgrid` and
docs accordingly. `target_epsg` defaults left untouched as planned.
Both files lint clean.

### Correction: the GEOID12B tile-mixup theory was debunked for the
### wrong reason — and the wrong theory, twice over

The user reported two things that overturned part of the above in one
message: (1) MassGIS confirmed their bare-earth lidar uses GEOID18; and
(2) `g2012bu4.gtx` **does** exist locally, at
`.../Geoid Transformation GTX Files/geoid12b/g2012bu4.gtx` — directly
contradicting this session's earlier claim that it didn't.

**MassGIS/GEOID18:** resolves one of `CRS.md`'s open questions outright
— updated there. Matches our adopted vertical standard exactly.

**`g2012bu4.gtx`:** listing the `geoid12b/` directory shows why the
earlier PROJ-CDN-based check missed it — `g2012bu0.gtx` (34 MB) is the
**combined CONUS grid**, and `g2012bu1.gtx`–`g2012bu8.gtx` (~4.9 MB
each) are its **eight regional sub-tiles**, the same packaging pattern
GEOID18 uses. PROJ's public CDN only mirrors the combined `u0` grid for
GEOID12B, not the individual sub-tiles, so the earlier "no g2012bu4
exists" conclusion was simply wrong — checking one catalog and treating
its absence as proof of nonexistence, when a local copy was sitting in
the repo's own resource tree the whole time. `g2012bu4` covers
40–58°N, 281–300°E (79–60°W) — genuinely "around the MA coast," exactly
as the original field notes described. The field notes were right;
last session's correction of them was wrong.

**But re-running the actual diagnostic (now possible, since both files
are local) reaches the same practical conclusion as before, on solid
footing this time.** Loaded `g2012bu0.gtx` and `g2012bu4.gtx` with
`terra::rast()` and sampled the undulation value at Red River
(41.668°N, 289.957°E) and across a 1°×1° grid around it:
**identical to the last digit everywhere tested (max |diff| = 0.0 m).**
`u0` is literally `u1`–`u8` merged into one file, not an independently
re-derived surface, so picking one tile over the other cannot produce
a different result at any given point. Also sampled GEOID18 at the
same point: −28.097 m vs. GEOID12B's −28.104 m, a 0.7 cm difference.
Net effect on the Phase 1.6 diagnosis: **unchanged** — tile choice
still isn't the explanation for the +10–16 cm bias, and GEOID18 still
won't close that gap on its own — but now demonstrated empirically
against the actual local grids rather than inferred from an incomplete
external catalog. Updated `CRS.md`, `dev/work_plan.md` (Phase 1.6
candidate-causes list), and `lidar/readme.md` to reflect the corrected
reasoning.

**Process note for next time:** should have listed the local
`geoid12b/` directory before concluding a named file "doesn't exist" —
checking a remote catalog is not the same as checking what's actually
on disk, especially in a resource tree this project already owns.

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
