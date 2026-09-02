# Coordinate reference systems, datums, and geoids

This file is the single place documenting which horizontal datum,
vertical datum, geoid model, and map projection this project uses,
why, and how that differs from what earlier UAS data collection used.
Read this before adding a new site or a new data source with its own
stated CRS.

Background and the specific Red River bias investigation live in
[`lidar/readme.md`](lidar/readme.md);
this file is the project-wide standard, not a lidar-specific writeup.

## Current standard (adopt for all new work)

### Geographic (unprojected / spherical) coordinates

- **Horizontal datum:** NAD83(2011) — geographic 2D, **EPSG:6318**
  (3D ellipsoidal: **EPSG:6319**).
- **Vertical datum:** NAVD88 orthometric height, **EPSG:5703**,
  computed via the **GEOID18** hybrid geoid model
  (grid `us_noaa_g2018u0.tif`, CONUS coverage).

### Projected coordinates

**Decision (provisional, 2026-08-20): Massachusetts Mainland State
Plane, EPSG:6491.** Both options are documented below for reference;
State Plane is the one to use in new code. Revisit if this turns out
to be the wrong call in practice.

#### UTM Zone 19
- **Horizontal:** NAD83(2011) / UTM zone 19,  **EPSG:6348**
- **Vertical:** same as above — NAVD88 via GEOID18, **EPSG:5703**.

#### Mass State Plane
- **Horizontal:** NAD83(2011) / Massachusetts Mainland (State Plane),
  **EPSG:6491** (meters).
  A US-survey-feet variant exists as **EPSG:6492** if a data source
  requires feet, but default to meters for anything this project
  produces.
- **Vertical:** same as above — NAVD88 via GEOID18, **EPSG:5703**.
- There is no single pre-registered EPSG code for the horizontal +
  vertical compound;
  construct it the same way the code already does for UTM
  (`R/reproject_las_pdal.R`'s `a_srs = "EPSG:<horiz>+<vert>"`),
  e.g. `EPSG:6491+5703`.


#### Comparison

**State Plane advantages**
* People understand state boundaries so are likely to use the right 
state plane projection in each state.
* MassGIS and most Massachusetts state
data products publish in Massachusetts Mainland State Plane, and
aligning with that convention makes cross-referencing state layers
(parcels, wetlands, other MassGIS rasters) simpler.
One caveat worth flagging: the MassGIS lidar bare-earth tiles used in
the Phase 1.5 ground-source comparison
(`lidar/output/*_ground_comparison/`) are themselves published in
**NAD83(2011) / UTM zone 19N (EPSG:6348)**, not State Plane — confirmed
via `terra::crs()` on `be_19TDG412612.tif`.
So "MassGIS's convention" is not perfectly uniform across their own
products/
* Mass Marsh project is Massachusetts project

**UTM Zone 19 advantages**
* all of coastal MA, NH, ME, and RI are in 
zone 19, but Zone 19 splits the CT coast with most of the state in Zone 18. 
- **Vertical:** same as above — NAVD88 via GEOID18, **EPSG:5703**


Regardless of which we pick we should use NAD83(2011), not legacy NAD83.

## Historical / legacy usage (what this project used before)

Historically the DEP UAS project has used several projections but generally
seems to have used legacy NAD83 as opposed to NAD83(2011).

- **Horizontal:** **EPSG:26919** — NAD83 / UTM zone 19N, **no
  realization specified**.
- **Vertical:** NAVD88 (EPSG:5703) via **GEOID12B**
  (grid `g2012bu0.gtx`).

Both are outdated relative to current NGS practice, for different
reasons (below).

**Confirmed, not just assumed, for the field ECPs.** Josh Ward's
thesis (who collected the GCPs/ECPs) states they were measured in
NAD83/UTM19N, NAVD88 — legacy NAD83, not NAD83(2011). This matches
`R/load_ecp.R`'s hardcoded `source_crs <- 26919L` exactly, so that
assumption is correct, not just asserted (see the former "blocking
open question" below, now resolved). The geoid model used to compute
the ECPs' NAVD88 heights isn't stated in the thesis and remains open.

## The frame-shift issue: why "NAD83" alone isn't enough

NAD83 is not one fixed set of coordinates — it's a family of
*realizations* that have drifted apart over time because the North
American tectonic plate moves (~25 mm/yr) and NGS has periodically
re-adjusted the reference network (HARN, NSRS2007, the 2011
readjustment, etc.).
"NAD83" with no realization specified (as in EPSG:26919 or plain
EPSG:4269) resolves, in PROJ, to an early-1980s-epoch realization.
**NAD83(2011)** is the modern, GPS-era readjustment and is what
survey-grade GNSS equipment and current NGS products actually use.

The RESEPI LAS headers tag the source cloud as **EPSG:32619**
("WGS 84 / UTM zone 19N") — the generic WGS 84 *datum ensemble*, not
a specific realization like ITRF2014 or WGS 84(G2139).
That labeling turns out to matter more than which target EPSG code is
picked:

- **EPSG:26919** (legacy NAD83, no realization) as the target:
  `projinfo -s EPSG:32619 -t EPSG:26919` returns a literal
  `+proj=noop` — not just "close to" one.
  This *sounds* convenient but means the frame shift is silently
  **not applied at all**, and downstream comparisons against
  genuinely NAD83(2011)-referenced data (survey ECPs, MassGIS
  rasters) carry a real, uncorrected offset in this region.
- **EPSG:6348 / EPSG:6491** (NAD83(2011)) as the target: **checked
  2026-08-27, and this does not fix it either**, as long as the
  source stays tagged EPSG:32619. `projinfo -s EPSG:32619 -t
  EPSG:6348 -o PROJ` returns:
  ```
  +proj=pipeline
    +step +inv +proj=utm +zone=19 +ellps=WGS84
    +step +proj=utm +zone=19 +ellps=GRS80
  ```
  — just the inverse UTM projection followed by the new one, with the
  reference ellipsoid swapped from WGS84 to GRS80. No Helmert /
  frame-shift step at all, and PROJ labels the operation's own
  accuracy as "2 m" — its standard signal for a *ballpark*
  transformation (the WGS 84 datum ensemble's stated accuracy is wide
  enough that PROJ treats generic "WGS 84" as coincident with
  NAD83(2011) and skips the real shift, exactly as for EPSG:26919
  above). Confirmed with an actual coordinate too: transforming a
  Red River-area point via `EPSG:32619 → EPSG:6491` and via
  `EPSG:32619 → <EPSG:6491's proj4string>` (what
  `reproject_las_pdal()` currently builds as `out_srs`) land on
  **identical** projected coordinates to the last digit.
  This means the claim previously made here — and
  `reproject_las_pdal()`'s docstring's claim of "eliminates the
  ~1–2 m WGS 84 ↔ NAD 83 frame-realization offset" — is **not
  currently true** for LAS files tagged EPSG:32619, regardless of
  which NAD83(2011) target EPSG is passed in.
- **A source tagged with a specific realization** is what actually
  triggers the real shift. `projinfo -s EPSG:7912 -t EPSG:6319`
  (ITRF2014 → NAD83(2011)) picks the genuine, time-dependent
  14-parameter Helmert transformation NGS publishes for this pair,
  not a ballpark op. WGS 84(G2139) — RESEPI's actual GNSS frame per
  the field notes — is defined to track ITRF2014 to within a few cm,
  so EPSG:7912 is a reasonable stand-in for testing purposes.
  **Getting the real shift applied in practice therefore requires
  retagging or re-deriving the source CRS as a specific realization
  before reprojecting** — switching only the *target* EPSG, which is
  all `reproject_las_pdal()`/`reproject_las_lastools()` currently do,
  is not sufficient.

### Magnitude of the real shift (computed 2026-08-27)

Using `pyproj`/PROJ (ITRF2014 → NAD83(2011), EPSG:7912 → EPSG:6319)
at Red River (41.668°N, −70.043°W):

- **Horizontal:** ≈ 1.15 m north, ≈ 0.45 m east at the 2022 survey
  epoch (≈ 1.2 m combined) — consistent with the "~0.5–1 m" figure
  this file previously cited, so that estimate was directionally
  reasonable even though, per above, it wasn't actually being applied
  by the current code.
- **Vertical (ellipsoidal height):** ≈ +1.23 m at the 2022 survey
  epoch, and essentially the same (≈ +1.24 m) at NAD83(2011)'s
  2010.0 reference epoch — so this is dominated by the static
  translation/rotation between the frames, not by plate-motion drift
  accumulated since 2010.

This vertical component was checked against the observed +10–16 cm
UAS elevation bias (see [`lidar/readme.md`](lidar/readme.md),
"Systematic vertical bias in UAS-derived elevation data") and ruled
out as its explanation: it's an order of magnitude too large, and the
wrong sign (omitting it would make heights read ~1.2 m too *low*, not
~10 cm too high). It's a real, separate correction from the
GEOID12B/18 undulation difference discussed below — that one is only
a few cm here.

### Practical consequence

This offset is small enough to be invisible to CSF ground
classification (a local point-cloud algorithm, not a coordinate
matcher) — which is why Phase 1's CSF parameter tuning was fine using
26919. It matters as soon as a raster or point cloud in the new
standard's frame is compared pixel-for-pixel against one still in the
old, unrealized frame — e.g., sampling a MassGIS DTM (EPSG:6348) at a
lidar point's raw X/Y coordinates.
That exact case is what surfaced this issue: `lidR::normalize_height()`
extracts raster values at a LAS point's X/Y with **no CRS check or
reprojection** (confirmed by reading `lidR:::normalize_height.LAS`),
so feeding it a DTM in a different horizontal frame than the point
cloud silently mis-samples by however large the frame shift is. Given
the finding above, that mis-sample risk currently applies to *every*
lidar output produced so far, including ones already reprojected to
EPSG:6348/6491 — the ballpark op means they never actually left the
generic-WGS84 frame.

## The geoid issue: GEOID12B vs. GEOID18

Separately from the horizontal frame, converting ellipsoidal GNSS
heights to NAVD88 orthometric heights requires a geoid model.
**GEOID18** has been NGS's current hybrid geoid model for CONUS since
2019 — before this project's 2022 UAS flights and MassGIS's
2023-processed lidar. GEOID12B is superseded.

One clarification worth recording because it changes how we should
read the observed elevation bias: the original field processing notes
(`lidar/readme.md`, "PPK processing workflow") describe choosing
between two GEOID12B grid files, `g2012bu0.gtx` (labeled CONUS) and
`g2012bu4.gtx` (labeled "around MA coast"), implying a possible
tile-selection mistake.

`g2012bu0.gtx`(34 MB) is the **combined CONUS grid**, and 
`g2012bu1.gtx` through`g2012bu8.gtx` (~4.9 MB each) are its 
**eight regional sub-tiles** —
the same pattern GEOID18 uses (`g2018u0` = "all grids 1–8 for CONUS
combined", per NGS's GEOID18 readme). `g2012bu4` covers 40–58°N,
281–300°E (79–60°W) — the northeast US, including Cape Cod. 

**But this doesn't change the conclusion, empirically using both
`g2012bu0.gtx` and `g2012bu4.gtx` locally with `terra` and sampling
the undulation value at Red River (41.668°N, 289.957°E / −70.043°E) 
and across a 1°×1° grid around it results in 
**identical results to the last digit at every point tested (max difference
0.0 m).** `u0` is literally the sub-tiles merged into one file, not a
different geoid surface, so which one you pick does not matter
numerically. 

The observed +10–16 cm bias in lidar data (see `lidar/readme.md`) is
**not** explained by a GEOID12B tile choice — not because the "MA
coast" tile doesn't exist (it does, and the field notes' description
of it was accurate), but because it's numerically indistinguishable
from the CONUS tile at this location.

Also sampled **GEOID18** (`us_noaa_g2018u0.tif`) at the same Red River
point: **−28.097 m vs. GEOID12B's −28.104 m — a 0.7 cm difference.**
This confirms switching geoid models is correct practice going forward
but, as suspected, doesn't come close to explaining a 10–16 cm bias.

GEOID18's CONUS grid is available as a ready-to-use GeoTIFF from
PROJ's own grid distribution — `us_noaa_g2018u0.tif`
(`https://cdn.proj.org/us_noaa_g2018u0.tif`) — rather than NGS's native
`.bin`/`.asc` format. PROJ accepts GeoTIFF vertical grids the same way
as legacy `.gtx` via `+geoidgrids=`, so it's a drop-in replacement for
the `vgrid` argument already used by `reproject_las_pdal()`.

A local copy is stored at: 
`X:\legacy\gdrive\UMassAir User Resources\LASTools\Geoid Transformation GTX Files\geoid18\us_noaa_g2018u0.tif`

## What comes next (2022/2023 NSRS modernization)

NGS plans to eventually replace NAD83 and NAVD88 entirely with a new
reference frame and gravimetric geoid
(NATRF2022 / NAPGD2022 / GEOID2022), citing known bias and tilt in
NAVD88. As of this document (checked against NGS's own site), **this
has not been officially adopted** — NAD83/NAVD88 remain the official
NSRS datums, with an FGCS approval vote targeted for "early-to-mid 2026"
and a multi-month transition process to follow.
Revisit this file once that adoption actually happens; until then,
NAD83(2011) + NAVD88/GEOID18 is the right target, not NAPGD2022.

## Open questions for LIDAR processing

- ~~What CRS are Josh's field ECPs actually in?~~ — **resolved
  2026-08-19.** Josh Ward's thesis states the GCPs/ECPs were measured
  in NAD83/UTM19N, NAVD88 — legacy NAD83 (no realization), matching
  `R/load_ecp.R`'s hardcoded `source_crs <- 26919L` exactly.
  This was flagged as the highest-priority open question because
  `sample_dtm()` reprojects the ECPs' `sf` geometry to match whatever
  raster it's sampling (`sf::st_transform(ecp, crs = raster_crs)`),
  and that transform is only correct if the *starting* label (26919)
  is true — if the ECPs had actually been NAD83(2011) coordinates
  mislabeled as legacy 26919, reprojecting them to a genuinely
  NAD83(2011) raster would have applied a spurious ~0.5–1 m shift.
  Now confirmed the label is correct, so this risk doesn't apply:
  `sample_dtm()`'s reprojection has been doing the right thing all
  along, and switching the point-cloud pipeline's horizontal target
  to NAD83(2011)/EPSG:6491 is safe with no change needed to
  `R/load_ecp.R`'s `source_crs` (only its `target_crs` default moves).
  `R/evaluate_dtm.R` and `R/plot_residual_map.R` also hardcode
  `crs = 26919L`; same reasoning applies, no longer flagged as risky.
- ~~Which geoid model did MassGIS use~~ — **resolved 2026-08-19: GEOID18**,
  confirmed directly by the user. This matches our vertical standard
  exactly, so no vertical correction is needed when comparing MassGIS
  against future GEOID18-based products (it already didn't matter for
  the Phase 1.5 comparison itself, since GEOID12B vs. GEOID18 is only
  ~0.7 cm at Red River — see above — but it's good to have confirmed
  rather than assumed).
- **Which geoid model were the field ECPs
  (`JoshSurveyPoints_AllSites_*.xlsx`) computed with?**
  Same access problem as above — would need to ask the surveyor or
  check the GNSS receiver's configuration.
- **Open, 2026-08-27: does reprojecting to EPSG:6348/6491 actually
  apply the frame shift for any lidar output produced so far?** No —
  see "The frame-shift issue" above. Because the source LAS files are
  tagged EPSG:32619 (the generic WGS 84 ensemble, not a specific
  realization), PROJ resolves EPSG:32619 → EPSG:6348/6491 as a
  ballpark ellipsoid swap with no real Helmert shift, identically to
  EPSG:32619 → EPSG:26919. This means every lidar output reprojected
  so far via `reproject_las_pdal()`/`reproject_las_lastools()` —
  including any already labeled EPSG:6348/6491 — has *not* actually
  received the ~1.2 m horizontal + ~1.2 m vertical frame correction,
  contrary to what the target EPSG label implies and what
  `reproject_las_pdal()`'s docstring currently claims. Fixing this
  needs the *source* CRS re-derived as a specific realization (e.g.
  ITRF2014, EPSG:7912, or whatever WGS 84(G2139)'s actual EPSG code
  turns out to be) before reprojection, not just picking a good
  target EPSG. Not yet fixed in code — flagging here so it isn't
  lost; see `dev/workplan.md` / `dev/worklog.md` for whether this has
  moved to an active task.

## Practical checklist for new code

- Reproject point clouds and rasters to **EPSG:6491** (Massachusetts
  Mainland State Plane — the provisional decision above) horizontally,
  plus **EPSG:5703** vertically via the **GEOID18** grid — never legacy
  EPSG:26919/4269 or GEOID12B for new work. EPSG:6348 (UTM 19N) remains
  an acceptable fallback where a specific downstream tool or data
  source expects it, as long as it's NAD83(2011) and not legacy NAD83.
  This has now been rolled into the pipeline's `target_epsg` defaults
  (`lidar/02.R`, `R/reproject_las.R`, `R/load_ecp.R`, `R/evaluate_dtm.R`,
  `R/plot_residual_map.R`, `R/las_needs_reprojection.R`) — see
  `dev/worklog.md`, 2026-08-20.
  **Caveat added 2026-08-27: setting `target_epsg` alone does not
  currently apply the real frame shift** — see the open question
  above. Until the source-CRS side is fixed, treat EPSG:6348/6491
  output from this pipeline as no more frame-corrected than legacy
  EPSG:26919 output.
- Don't assume two rasters/point clouds are pixel-aligned just because
  both claim "NAD83" — check the specific EPSG code (realization
  matters) and reproject explicitly rather than relying on downstream
  functions to do it for you.
  Several functions in this codebase do **not** check CRS before
  sampling (`lidR::normalize_height()` is one confirmed example) — the
  caller is responsible for making inputs match.
