# Work plan — lidar elevation & vegetation strata

This file tracks only the **currently active, still-open** work.
For how the pipeline works (stages, functions, data flow), see
[`lidar/ARCHITECTURE.md`](../lidar/ARCHITECTURE.md). For dated
history of what was done, found, and decided, see `worklog.md` /
`worklog-archive.md`.

## Active: site rollout

The pipeline (reprojection through the vegetation height distribution
raster) is implemented and validated end-to-end for `rr` (Red River).
Active work is applying it to the remaining four sites.

| Site | Cloud pair (spring + summer) | ECPs (type=EVP) | Status |
|---|---|---|---|
| `rr` | 2022-05-14 / 2022-08-10 | 169 | Done |
| `wel` | 2022-05-20 / 2022-08-02 | 153 | **Ready, not started** — has everything the pipeline needs; larger clouds than `rr`'s, so Steps 0–2 will take longer (memory-bound). Best next candidate. |
| `oth` | none (photogrammetry only) | 145 | **Blocked** — no raw point cloud; ground-source comparison (photogrammetry + MassGIS only) already done. |
| `nor` | 2024-05-24 / 2024-10-06 | **0** | Blocked — no ECPs to validate against. |
| `peg` | 2024-04-19 / 2024-09-23 | **0** | Blocked — same reason as `nor`. |

`wel` also spans two non-overlapping MassGIS tiles
(`be_19TDG415636.tif`, `be_19TDG417636.tif`), so extending the
floor-bias approach there needs a `terra::merge()` mosaic step first.

- [ ] Run `wel` through Steps 0–2, DTM evaluation, ground comparison +
  floor-bias correction, and veg heights.
- [ ] **Identify ground control points for `nor` and `peg`** — both
  currently have zero ECPs, blocking DTM validation and floor-bias
  estimation the same way it worked for the other three sites. Needs
  a source of field-surveyed elevation (and ideally `veg_height_m`)
  points before either site can proceed.
- [ ] `oth` stays blocked until a raw point cloud exists (or a
  decision is made to accept photogrammetry-only for that site).

## Paused

Two investigations are paused in favor of the site rollout above —
not abandoned, just deprioritized. Resuming either doesn't require
reopening this file first; go straight to the doc named.

- **UAS vertical elevation bias** (+10–16 cm vs. ECPs) — doesn't block
  relative vegetation heights (cancels on normalization), only
  absolute elevation deliverables. See `lidar/readme.md`'s "Systematic
  vertical bias..." section for current findings (lever arm cleared,
  boresight calibration is the leading hypothesis) and
  `dev/scan_angle_bias.md` for the next diagnostic step.
- **Canopy-top underestimate** (~15 cm vs. field `veg_height_m`) — see
  `lidar/readme.md`'s "Canopy-top underestimate..." section.

## Vegetation-strata output decisions (open, not blocking)

- [ ] **Fraction denominator.** Currently *all* returns in the cell
  (including below-ground/above-ceiling), so bands don't sum to
  100% — no firm answer yet on whether that's correct; not changing
  without a concrete reason (e.g. a downstream model that assumes
  bands sum to 1).
- [ ] **Raster resolution.** Default is 0.5 m per `lidar/readme.md`;
  revisit producing 0.25 m / 1 m alternatives now that a real first
  output is in hand.

## Out of scope (for now)

- Wrapping anything into a formal R package (`DESCRIPTION` /
  `NAMESPACE`).
- Touching the hydrology or logger-recalibration pipelines.
- Producing the final veg-modeling deliverables that consume these
  rasters — that's a separate downstream project.
