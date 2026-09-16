# Plan: test whether the UAS vertical bias correlates with lidar scan angle

**Status: paused, 2026-09-11.** Proposed, not yet implemented, and
deprioritized behind the site rollout (`dev/workplan.md`) — the bias
doesn't block relative vegetation heights, only absolute elevation.
Still valid to resume as-is when bias work picks back up. Written up
for review before any code changes — see `dev/worklog.md` for how
this fits into the broader vertical-bias investigation, and
`lidar/readme.md`'s "Systematic vertical bias..." section for the
underlying finding.

## Context

We're diagnosing the +10-16 cm positive vertical bias in UAS lidar
ground elevations (`lidar/readme.md`, "Systematic vertical bias...").
Prior work this session cleared the GNSS-INS lever arm (six
independent 2022 flights converge on the same Z lever arm to <1 cm)
and previously cleared the geoid model (<1 cm effect) and the
ITRF->NAD83(2011) frame shift (wrong order of magnitude and sign).
The remaining live candidate is the RESEPI unit's **boresight
calibration** — a fixed linear+angular offset
(`(0, 0.06, 0) m`, `(yaw 0.02°, pitch -0.06°, roll -0.58°)`) that's
copied unchanged into every 2022 flight's project file rather than
re-derived per flight, so — unlike the lever arm — flight-to-flight
consistency can't tell us whether it's *correct*. A geometry-dependent
check can: if the bias comes from a boresight misalignment, it should
vary with scan angle (which of the sensor's 32 laser channels fired);
if it comes from something translational (base-station coordinate, a
GNSS/tropo effect), it should be flat across scan angle.

This plan covers building that check using data already on disk — no
new external inputs needed.

## Data already confirmed available

- `E:/uas_scratch/lidar/rr/2022_05_14/zzzcleaned_epsg6491/*.las` and
  `.../2022_08_10/zzzcleaned_epsg6491/*.las` — RR spring and summer
  point clouds, already last-return-only + denoised
  ([`clean_and_tile()`](../R/clean_and_tile.R)) and already reprojected
  to EPSG:6491 + NAVD88 (same frame `load_ecp()` produces). Confirmed
  each tile carries a `ScanAngle` field (LAS 1.4 point format 7,
  degrees, range ~20-64° observed).
- `lidar/output/rr_ground_comparison/lidar_spring_ecp.csv` /
  `lidar_summer_ecp.csv` — the exact ECP sets (169 / 168 points) and
  `easting`/`northing`/`elevation` (EPSG:6491/NAVD88) already used to
  compute the published 0.158 m / 0.215 m overall bias figures. Using
  these instead of re-deriving from `load_ecp()` guarantees this
  analysis is apples-to-apples with the existing numbers.

**Caveat carried into the analysis and its writeup**: the RESEPI
Hesai XT32 is a continuously-rotating 32-channel scanner, not an
oscillating-mirror system, and `ScanAngle` here is always positive
(no observed negative values across a full flight) — it reads as
"which fixed-elevation channel fired," not a signed left/right
cross-track angle. A pure roll misalignment classically produces an
antisymmetric left/right signature that an unsigned angle can't show
directly; pitch/channel-angle miscalibration would show more directly
as a function of this angle. So: a clear trend vs. `ScanAngle`
implicates the boresight broadly; a flat result doesn't fully clear a
pure-roll effect, and that nuance needs to be stated explicitly in the
output rather than over-claiming either way.

## Plan

1. **New helper: `R/extract_scan_angle_residuals.R`**
   `extract_scan_angle_residuals(ctg, ecp, radius = 0.5)`:
   - `ctg`: a `LAScatalog` (or path to the tile directory).
   - `ecp`: data frame with `easting`, `northing`, `elevation` (the
     cached `lidar_spring_ecp.csv`-style frame).
   - For each ECP, `lidR::clip_circle()` all points within `radius`
     metres, and emit one row per point: an ECP identifier
     (`point_number`/`subclass`), the point's `Z` and `ScanAngle`,
     `residual = Z - ecp$elevation`, and distance from the ECP center.
   - Returns one long data frame across all ECPs (not per-ECP
     summaries) so the regression/plot step pools points properly.
   - `radius = 0.5 m` default: large enough to catch multiple scan
     channels/passes per ECP (RESEPI's 32 channels + repeated
     rotations should give good angle diversity within half a metre)
     while staying small enough that local marsh microtopography
     shouldn't dominate the residual. Noted as tunable, not fixed.

2. **New helper: `R/plot_residual_vs_scan_angle.R`**
   `plot_residual_vs_scan_angle(points)` — scatter of `residual` vs.
   `ScanAngle`, `geom_smooth()` (loess, not forced-linear, since a
   pure roll effect could be non-monotonic given the unsigned angle
   caveat above) plus the linear fit overlaid for reference, zero
   reference line — same visual style as
   [`plot_residual_vs_elevation()`](../R/plot_residual_vs_elevation.R).

3. **New driver: `lidar/09_scan_angle_bias.R`** (follows the
   `lidar/07_floor_corrected_ground.R` pattern: source `R/`, read the
   cached ECP CSVs, run the new helper, save plots + a summary table
   into `lidar/output/rr_ground_comparison/`):
   - Run for **both** spring and summer clouds (spring is the primary
     read — minimal vegetation means last-return ≈ true ground, so a
     scan-angle trend there is least confounded by canopy hits; summer
     is a secondary check — if the same trend shows through the
     vegetation noise that's reinforcing, if it vanishes that's
     itself informative about return statistics, not necessarily
     exculpatory).
   - For each: fit `lm(residual ~ ScanAngle)`, report slope
     (m per degree), R², p-value; also bin into ~5° bins and report
     per-bin mean/median/SE for the plot and for eyeballing
     non-monotonic patterns the linear slope would miss.
   - Write `scan_angle_bias_spring.png` / `_summer.png` and a
     `scan_angle_bias_summary.csv` (one row per flight: slope, R²,
     p-value, n points, n ECPs) to `lidar/output/rr_ground_comparison/`.
   - `message()` a plain-language readout of slope/significance for
     each flight, same style as `07_floor_corrected_ground.R`.

## Verification

- Run `lidar/09_scan_angle_bias.R` from the project root in RStudio
  (per repo convention).
- Sanity check the extracted point counts per ECP are non-trivial
  (tens to low hundreds within a 0.5 m radius) and that `ScanAngle`
  values within a single ECP's neighborhood actually span a
  reasonable range (i.e., the radius is doing its job of catching
  multiple channels) — if not, radius may need to grow.
- Read the resulting plots/slope stats and interpret per the caveat
  above: a significant slope or a clear bin-wise trend → boresight
  strengthened as the leading hypothesis, worth pursuing a factory
  calibration record next; flat/insignificant with no bin-wise
  pattern either → boresight weakens, redirect toward the base-station
  coordinate check instead.
- `lintr::lint()` the two new `R/` files before committing (per
  `CLAUDE.md`); no changes to already-touched files so no drive-by
  linting.
