# Work plan — lidar elevation & vegetation strata

This file tracks only the **currently active, still-open** work.
For how the pipeline works (stages, functions, data flow), see
[`lidar/ARCHITECTURE.md`](../lidar/ARCHITECTURE.md). For dated
history of what was done, found, and decided, see `worklog.md` /
`worklog-archive.md`.

## Goals

1. **Elevation (DTM).** Bare-earth elevation from each site's lidar
   point cloud, validated against field elevation control points
   (ECPs).
2. **Vegetation strata.** A multi-band raster of above-ground
   biomass distribution by height bin (5 cm bins 0–1 m, 20 cm bins
   1–3 m, 30 bands total) — see `lidar/readme.md`'s "dream output."

## Current state

Both goals are implemented and validated end-to-end for `rr` (Red
River). In order:

- **Reprojection, clean & tile, CSF DTM tuning, ECP evaluation** —
  done for `rr` spring and summer under the current CRS standard
  (EPSG:6491 + GEOID18; see `CRS.md`). Best CSF parameter sets
  picked by overall RMSE.
- **Ground-source comparison** — done for `rr`, `oth`, `wel`.
  MassGIS aerial lidar wins by a wide margin at all three; picked as
  the ground-reference source.
- **Floor-bias-corrected ground reference** — done for `rr`.
  MassGIS + an estimated instrument-level offset (not the full
  spring-DTM bias) is the ground reference `lidar/05_veg_heights.R`
  uses by default.
- **Vegetation-height validation** — done for `rr`. The
  floor-bias-corrected reference beats the legacy spring-DTM-relative
  approach on every metric against field-measured `veg_height_m`.
- **Vegetation height distribution raster** — done for `rr`; a real
  30-band `veg_dist_0.5m.tif` exists and looks sane (per-band values
  and per-pixel row sums both in [0, 1]).

Two CRS bugs were found and fixed along the way (stale
pre-EPSG:6491 cleaned tiles; an unreprojected MassGIS raster causing
`normalize_height()` OOM crashes) — see `dev/worklog.md`, 2026-08-24,
for the full account.

## Open items

### Diagnose the remaining UAS vertical bias (was Phase 1.6)

All UAS-derived ground sources still show a positive bias against
ECPs that MassGIS doesn't; GEOID-tile mixups have been ruled out
empirically (`CRS.md`, "the geoid issue"). Leading hypothesis: a
lever-arm misconfiguration in the RESEPI sensor's PPK processing.
Not blocking — vegetation heights are unaffected because the bias is
common to spring and summer and cancels on normalization — but
absolute elevation deliverables need it resolved.

- [ ] Verify the CORS station used and its published NAD83(2011)
  orthometric height against what PCMaster used.
- [ ] Check the lever-arm value against the RESEPI sensor's
  documented specification.
- [ ] Record the finding and correction in `worklog.md`.

### Canopy-top underestimate (was Phase 1.7, open item)

Both candidate ground references underestimate true vegetation
height by ~15 cm *before* the ground-reference choice matters at all
— common to both, so not a ground-reference artifact. The
last-return-filtering hypothesis was checked directly against the
raw point cloud and refuted (first/last returns are byte-identical
for every pulse in the `rr` summer cloud). Cause still unknown;
candidates not yet checked: `rasterize_canopy_top()`'s 0.25 m pixel
averaging vs. a point-measurement field survey, lidar beam-footprint
effects on thin vegetation, or a difference in what `veg_height_m`
actually measures. Needs its own investigation before absolute (not
relative) vegetation heights from this pipeline should be trusted.

- [ ] Investigate the ~15 cm canopy-top underestimate.

### Vegetation-strata output decisions (was Phase 2, open items)

- [ ] **Fraction denominator.** Currently *all* returns in the cell
  (including below-ground/above-ceiling), so bands don't sum to
  100% — no firm answer yet on whether that's correct; not changing
  without a concrete reason (e.g. a downstream model that assumes
  bands sum to 1).
- [ ] **Raster resolution.** Default is 0.5 m per `lidar/readme.md`;
  revisit producing 0.25 m / 1 m alternatives now that a real first
  output is in hand.

### Site rollout (Phase 3)

| Site | Cloud pair (spring + summer) | ECPs (type=EVP) | Status |
|---|---|---|---|
| `rr` | 2022-05-14 / 2022-08-10 | 169 | Done |
| `wel` | 2022-05-20 / 2022-08-02 | 153 | **Ready, not started** — has everything the pipeline needs; larger clouds than `rr`'s, so Steps 0–2 will take longer (memory-bound). Best next candidate. |
| `oth` | none (photogrammetry only) | 145 | **Blocked** — no raw point cloud; ground-source comparison (photogrammetry + MassGIS only) already done. |
| `nor` | 2024-05-24 / 2024-10-06 | **0** | Blocked — no ECPs to validate against (readme's top-priority site otherwise). |
| `peg` | 2024-04-19 / 2024-09-23 | **0** | Blocked — same reason as `nor`. |

`wel` also spans two non-overlapping MassGIS tiles
(`be_19TDG415636.tif`, `be_19TDG417636.tif`), so extending the
floor-bias approach there needs a `terra::merge()` mosaic step
first.

- [ ] Run `wel` through Steps 0–2, DTM evaluation, ground comparison +
  floor-bias correction, and veg heights — on hold, pending a
  decision on when to run the longer Steps 0–2 pass.
- [ ] `oth`/`nor`/`peg` stay blocked until their respective gaps (no
  cloud; no ECPs; no ECPs) are resolved.

## Decisions locked in

1. **First site:** `rr` (Red River).
2. **CSF parameter search:** the 4 hand-picked combinations in
   `lidar/02.R`'s `csf_grid`; reassess if they prove insufficient.
3. **Water-logger floor points as ECPs:** skip.
4. **Date matching ECP↔DTM:** ignore date; compare all site ECPs
   against each DTM.
5. **Strata output units:** percent of returns within the pixel.
6. **Strata validation:** field `veg_height_m` at ECPs (see
   "Vegetation-height validation" above), plus downstream model
   performance as pseudo-validation.
7. **DTM stats by ECP type:** report overall **and** broken out by
   ECP `type`.
8. **Ground reference for vegetation heights:** MassGIS +
   floor-bias offset (see "Current state" above).

## Out of scope (for now)

- Wrapping anything into a formal R package (`DESCRIPTION` /
  `NAMESPACE`).
- Touching the hydrology or logger-recalibration pipelines.
- Producing the final veg-modeling deliverables that consume these
  rasters — that's a separate downstream project.
