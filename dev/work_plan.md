# Work plan — lidar elevation & vegetation strata

## Goals

1. **Elevation (DTM).** Develop and validate an approach for estimating
   bare-earth elevation from each site's lidar point cloud, scored
   against field-collected elevation control points (ECPs).
2. **Vegetation strata.** Produce a multi-band raster characterizing
   above-ground biomass distribution by height bin: 5 cm bins from
   0–1 m, then 20 cm bins from 1–3 m (30 bands total). Per
   `lidar/readme.md`, this is the "dream output."

## Current state (what already exists)

- **Step 1: clean & tile** — `R/clean_and_tile.R` filters to last
  returns, removes noise via `sor`, writes cleaned tiles. Skips if
  output exists.
- **Step 2: rasterize ground** — `R/rasterize_ground.R` runs CSF
  classification with `(class_threshold, cloth_resolution, rigidness)`
  then `rasterize_terrain(knnidw)`. Writes one GeoTIFF per parameter
  set.
- **Driver** — `lidar/02.R` reads `lidar/data/paths.csv`, picks the
  preferred cloud for `site = "rr"` (currently hardcoded), runs Step 1
  once, then loops Step 2 over 4 CSF parameter combinations
  (`lidar/02.R:80–83`). 4 DTMs land in
  `E:/uas_scratch/lidar/<site>/<date>/zzzraster/csf_th*_res*_rgd*_*m.tif`.
- **Step 3 (validation): partial.** `evaluate_dtm()` and
  `visualize_dtm()` are drafted inline in `lidar/02.R:214–281` but
  not yet extracted to `R/`, not finished (`evaluate_dtm()` builds an
  offset model and a residual column but returns nothing), and not yet
  looped over the parameter grid.
- **Step 4 (veg strata): not started.** Nothing yet.

## Plan

### Phase 0 — quick cleanups (before adding new work)

- [ ] Extract `evaluate_dtm()` and `visualize_dtm()` from `lidar/02.R`
  into `R/evaluate_dtm.R` and `R/visualize_dtm.R`. Keep roxygen-style
  headers for eventual package migration.
- [ ] Also extract the `update_path()`, `clean_column_names()`, and
  `clean_dates()` helpers (currently inline at `lidar/02.R:97–194`)
  into `R/`. Small, reusable, no reason to keep inline.
- [ ] Fix the typo at `R/clean_and_tile.R:48` (`ouput_dir` → `output_dir`).
- [ ] Fix the `&&` → `&` bug at `R/rasterize_ground.R` (lines 248 in
  the draft `evaluate_dtm`) and the unused `chunk` reference at
  `rasterize_ground.R:46` (it crops to `ext(chunk)` but `chunk` isn't
  in scope inside `catalog_map`'s function — likely a leftover; either
  pass `las` or drop the crop).

### Phase 0.5 — reproject source LAS into the analysis CRS

The current Red River source (`rr` 2022-08-10,
`ppk_07Nov2022_cloud_1.las`)
is tagged horizontally as **EPSG:32619** (WGS 84 / UTM 19N)
and vertically as **EPSG:5030** (WGS 84 ellipsoid heights).
The ECPs and downstream analyses use NAD83 / UTM 19N (EPSG:26919)
horizontally and NAVD88 vertically.
Without reprojection,
the DTM-vs-ECP residuals will carry a ~28 m systematic offset
from the geoid separation
plus the small NAD83↔WGS84 horizontal shift,
making CSF parameter tuning meaningless.

**Target CRS:**
- Horizontal: **EPSG:26919** (NAD83 / UTM 19N)
- Vertical: **NAVD88** via the **GEOID12B** grid
  (`g2012bu0.gtx`, the CONUS tile covering 40–58° N, 281–300° E;
  Red River at ~41.7° N, ~290° E falls in this tile).
- Grid path: `X:\legacy\gdrive\UMassAir User Resources\LASTools\Geoid Transformation GTX Files\geoid12b\g2012bu0.gtx`.

**Tooling — locked in: hybrid (PDAL default, LAStools alternative).**

Two reprojection paths are supported via a dispatcher with
`method = c("pdal", "lastools")`,
default `"pdal"`.

**Method `"pdal"` (default, recommended for new output).**
PDAL with a PROJ pipeline that does the full datum-aware
transformation in a single pass:

- horizontal: WGS 84 → NAD 83 with the WGS84↔NAD83 frame shift
  applied
  (PROJ's bundled NADCON5 / ITRF grids),
- vertical: ellipsoidal → NAVD 88 via the GEOID12B `.gtx` grid.

The PROJ string for the target is built as
`sf::st_crs(target_epsg)$proj4string` plus
`+geoidgrids=<basename(vgrid)> +vunits=m`,
and the grid is found by setting `PROJ_DATA` to the gtx
directory in the `pdal` subprocess only
(no system-wide env var changes,
so R's own PROJ stays unaffected).
This eliminates the WGS84↔NAD83 frame-realization error
(~1–2 m in eastern CONUS)
that the LAStools workflow carries.

PDAL will be installed via **OSGeo4W**.
The user prefers OSGeo4W over conda because they have had bad
experiences with multiple PROJ installations conflicting on
the same system.
Subprocess isolation
(R never loads PDAL's PROJ;
PDAL never loads R's PROJ)
mitigates this risk regardless,
but OSGeo4W is the chosen route.
Expected install location: `C:\OSGeo4W\bin\pdal.exe`.
The user will install PDAL tomorrow and we will test then.

**Method `"lastools"` (alternative; kept for reproducibility).**
The two-step LAStools workflow described below.
Retained so we can recreate and understand historical output
produced by the established UMassAir process,
which used this approach:

1. **`las2las64 -target_epsg 26919 -force`** —
   horizontal reprojection of XY only.
   `-force` overrides LAStools' "horizontal datum of source and
   target incompatible" warning
   (raised because LAStools does not apply a datum shift
   between WGS 84 and NAD 83).
   Z passes through unchanged.
2. **`lasvdatum64 -epsg 26919 -vgrid g2012bu0.gtx`** —
   grid-based vertical transformation from ellipsoidal to
   NAVD 88 orthometric heights.
   `lasvdatum`'s own README confirms this exact use case:
   "convert from (UTM17 + NAD83 ellipsoidal)
   to (UTM17 + NAVD88 Geoid12B)."

**Known caveat for `method = "lastools"`:**
the GEOID12B grid is referenced to NAD 83 ellipsoidal heights,
but `las2las -target_epsg` does not convert Z from WGS 84
ellipsoidal to NAD 83 ellipsoidal.
In eastern CONUS the WGS 84 ↔ NAD 83 frame-realization offset
is ~1–2 m in 3D
(plate-motion drift since the two reference frames were tied
at epoch 1997).
The LAStools workflow carries this error;
`method = "pdal"` does not.

**Implementation:**
the shell calls are wrapped in three R files
that live under `R/`
and are called from `lidar/02.R`:

- [x] **`R/reproject_las.R`** — public dispatcher:

   ```r
   reproject_las(
      input,
      output,
      target_epsg = 26919L,
      vgrid       = "<g2012bu0.gtx path>",
      overwrite   = FALSE,
      method      = c("pdal", "lastools"),
      ...
   )
   ```

   Dispatches to one of two private helpers based on `method`.
   `...` is forwarded to the helper so its method-specific args
   (e.g. `pdal` path, `las2las`/`lasvdatum` paths,
   `intermediate`, `keep_intermediate`) are accessible without
   cluttering the dispatcher's signature.
   Skip-if-exists short-circuit lives in the dispatcher,
   so it applies uniformly across methods.

- [x] **`R/reproject_las_pdal.R`** — PDAL helper:

   ```r
   reproject_las_pdal(
      input, output, target_epsg, vgrid, overwrite,
      pdal = "C:/OSGeo4W/bin/pdal.exe"
   )
   ```

   Writes a small PDAL pipeline JSON to a tempfile
   (readers.las → filters.reprojection → writers.las),
   calls `pdal pipeline <json>` via `system2()`,
   and sets `PROJ_DATA = dirname(vgrid)` on the subprocess only
   so PROJ finds the `.gtx` grid by basename.
   Output gets `a_srs = "EPSG:<target_epsg>+5703"`
   (compound CRS for NAD83/UTM 19N + NAVD88 height).

- [x] **`R/reproject_las_lastools.R`** — LAStools helper
   (renamed from the existing one-file implementation;
   logic preserved):

   ```r
   reproject_las_lastools(
      input, output, target_epsg, vgrid, overwrite,
      intermediate      = NULL,
      las2las           = "C:/tools/LAStools/bin/las2las64.exe",
      lasvdatum         = "C:/tools/LAStools/bin/lasvdatum64.exe",
      keep_intermediate = FALSE
   )
   ```

   Calls the two binaries via `system2()` with `shQuote()` on
   path args and `-force` on the `las2las` step
   (override the WGS 84 ↔ NAD 83 incompatibility warning).
   Roxygen documents the WGS84↔NAD83 frame caveat that this
   method carries.

- [x] **`R/las_needs_reprojection.R`** — small helper that reads
   the LAS header and returns `TRUE` if either the horizontal or
   vertical CRS doesn't match the project targets:

   ```r
   las_needs_reprojection(
      input,
      target_epsg          = 26919,
      target_vertical_epsg = 5703   # NAVD88 height
   )
   ```

   Reads via `lidR::readLASheader()` for the horizontal EPSG and
   parses the GeoTIFF GeoKey directory in the header VLR for the
   `VerticalCSTypeGeoKey` (key 4096) to get the vertical EPSG.
   Returns `FALSE` only when both match; otherwise `TRUE`.
   Prints a one-line message describing the found-vs-expected pair
   so the workflow log shows why reprojection was triggered.

- [x] **Workflow integration in `lidar/02.R`** — automatic, no
   manual toggle.
   After `paths$input` is selected from `paths.csv` and before
   `clean_and_tile()` runs, insert:

   ```r
   if (las_needs_reprojection(paths$input)) {
      paths$reprojected_dir <- file.path(paths$base_output,
                                         "reprojected")
      dir.create(paths$reprojected_dir, recursive = TRUE,
                 showWarnings = FALSE)
      reprojected <- file.path(
         paths$reprojected_dir,
         sub("\\.las$", "_epsg26919_navd88.las",
             basename(paths$input), ignore.case = TRUE)
      )
      paths$input <- reproject_las(paths$input, reprojected,
                                   target_epsg = 26919)
   }
   ```

   `reproject_las()` itself skips when the output already exists
   (idempotent), so re-running the script is cheap.
   To force a re-reprojection, the user passes
   `overwrite = TRUE` or deletes the cached file manually.

- **`paths.csv` is left untouched** — it remains the stable
   record of original sources.
   The reprojected file is treated as a workflow cache,
   not a primary input,
   and lives only on the local RAID.

**Status — both methods are currently untested.**

Code is written and lints clean,
but neither path has produced a reprojected LAS end-to-end yet:

- **`method = "lastools"`** got partway through testing earlier
  in the day.
  Fixed two issues uncovered by partial runs
  (Windows arg-quoting via `shQuote()`,
  then the `-force` flag for the WGS 84 ↔ NAD 83 datum
  incompatibility warning),
  but never reached a clean run that produced a final output
  file before we restructured around PDAL as the default.
  Treat as "should work but unverified."
- **`method = "pdal"`** is brand new and has never run —
  PDAL is not yet installed.
  PDAL pipeline JSON syntax,
  the `out_srs` PROJ string construction,
  and the compound `a_srs = "EPSG:26919+5703"` may need
  iteration after the first real run.

**Install / setup tasks for tomorrow before testing:**

- [ ] Install PDAL via OSGeo4W.
   Expected install location: `C:\OSGeo4W\bin\pdal.exe`
   (override via the `pdal` arg if installed elsewhere).
- [ ] `install.packages("jsonlite")` in R if not already
   installed
   (used by `reproject_las_pdal()` to write the pipeline JSON).
- [ ] Add `C:\Program Files\R\R-4.5.2\bin\x64` to **System** PATH
   so `Rscript` is available from any shell
   (carried over from earlier in the session;
   user is doing this manually for all-users scope).

**Test plan for tomorrow:**

- [ ] First end-to-end run with `method = "pdal"` (default).
   Run `lidar/02.R`;
   the conditional reprojection block auto-triggers because the
   rr 2022-08-10 source is in EPSG:32619 + EPSG:5030.
   Expected output:
   `E:/uas_scratch/lidar/rr/2022_08_10/reprojected/ppk_07Nov2022_cloud_1_epsg26919_navd88.las`,
   then the re-cleaned tiles and four DTMs in the new CRS.
- [ ] Verify the PDAL output by reading the file's header
   (CRS keys updated to EPSG:26919 + EPSG:5703,
   min/max Z dropped by ~28 m from the geoid separation,
   ~1–2 m frame-realization correction applied to XY too).
- [ ] Smoke-test the LAStools path
   (`method = "lastools"`)
   on the same input,
   writing to a separate output path
   (e.g. `*_lastools.las`)
   so the two outputs can be diffed.
   This is the first clean end-to-end run for `lastools` and
   verifies the `shQuote()` + `-force` fixes,
   and gives us a baseline diff for the
   WGS 84 ↔ NAD 83 frame error
   (PDAL output minus LAStools output ≈ the frame offset).
- [ ] **Stale artifact cleanup already handled** by the user;
   no automatic deletion in the driver.

**Other sites deferred.**
Only rr 2022-08-10 is in scope for this phase per the user's
direction; other sites' clouds will be reprojected one at a time
as Phase 3 reaches them.
The same `reproject_las()` function will be reused;
only the `vgrid` may need to change for sites that fall outside the
`g2012bu0.gtx` tile footprint
(per `geoid12b.inf`,
the CONUS lower-48 tile g2012bu0 covers 24–58° N, 230–300° E,
which includes all four saltmarsh sites,
so the default should hold).

### Phase 1 — finish DTM evaluation (Goal 1)

Site: `rr` (Red River) — keep the current code's target. ECPs are
matched ignoring date (saltmarsh elevation considered near-static
yearly). Water-logger floor points are not used as ECPs.

- [ ] **`R/load_ecp.R`** — single function that reads the ECP xlsx,
  cleans columns, parses dates, lowercases site codes, and filters to
  `type = "training"` (per readme line 39). Returns an sf object in
  the cloud's CRS. ECPs marked "Logger Array" stay excluded per
  `lidar/02.R:210`.
- [ ] **Finish `evaluate_dtm(dtm, ecp)`** to return a tidy result list:
  - per-point table: `easting, northing, elevation, predicted, residual, type`
  - summary stats: n, bias (mean residual), MAE, RMSE, R², offset
    (from `lm(predicted ~ 1 + offset(elevation))`) — reported overall
    **and** broken out by ECP `type` (vegetation classes are expected
    to be harder than bare surfaces).
  - residual-vs-elevation plot, residual map, optional histogram
- [ ] **`lidar/03_evaluate_dtm.R`** — new driver that loops over the
  4 hand-picked CSF parameter combinations from `lidar/02.R:80–83`,
  calls `evaluate_dtm()` on each, and writes:
  - `lidar/output/<site>_<date>_dtm_eval.csv` — one row per
    (parameter set × ECP-type bucket) with summary stats; plus an
    "overall" row per parameter set.
  - PNG residual plots per parameter set.
- [ ] Inspect the results together before deciding whether to expand
  the parameter search; pick the best parameter set per site and
  record the choice + reasoning in `worklog.md`.

### Phase 2 — vegetation strata (Goal 2)

Approach: normalize point heights to the chosen DTM, then compute a
multi-band raster of **percent of returns** per height bin per pixel.
Scan density is treated as noise — percent normalizes it out, and with
~300 pts/m² the per-pixel sample sizes are large enough to keep
percent estimates stable. No direct validation dataset; downstream
modeling performance is the pseudo-validation.

- [ ] **`R/rasterize_strata.R`** — new function that:
  - takes the cleaned tile catalog + a DTM raster + bin edges + raster res
  - inside `catalog_map`: subtracts ground elevation (`lidR::normalize_height()`
    with the supplied DTM), drops returns < 0 m or > top bin, then uses
    `pixel_metrics()` with a custom function returning a named numeric
    vector of length `n_bins` = **percent of returns within the pixel
    falling in each bin** (sums to 100% across bands per pixel).
  - writes a multi-band GeoTIFF, one band per bin, plus a sidecar CSV
    of bin edges and band names.
- [ ] **`lidar/04_vegetation_strata.R`** — driver: pick the chosen
  DTM (output of Phase 1), call `rasterize_strata()` for the bin
  scheme in `lidar/readme.md:14–18` (5 cm bins 0–1 m, 20 cm bins
  1–3 m → 30 bands), write outputs to
  `E:/uas_scratch/lidar/<site>/<date>/zzzstrata/`.
- [ ] **Decision point (deferred):** raster resolution. Default to
  0.5 m per readme; revisit producing 0.25 m / 1 m alternatives after
  the first output is in hand.

### Phase 3 — apply across sites

- [ ] Once the per-site workflow is solid for `rr`, run it for other
  sites with ECP coverage. North River (the readme's top priority) is
  on hold until/unless ECPs become available for it.

## Decisions locked in

1. **First site:** `rr` (Red River) — sticking with the current code.
   `nor` deferred: no ECPs available for it.
2. **CSF parameter search:** keep the 4 hand-picked combinations from
   `lidar/02.R:80–83` for the first round; reassess after seeing
   results.
3. **Water-logger floor points as ECPs:** skip.
4. **Date matching ECP↔DTM:** ignore date; compare all site ECPs
   against each DTM.
5. **Strata output units:** percent of returns within the pixel
   (scan density is noise; ~300 pts/m² keeps percent estimates stable
   at 0.5 m pixels).
6. **Strata validation:** no field veg-height dataset; downstream
   model performance is the pseudo-validation.
7. **DTM stats by ECP type:** report overall **and** broken out by
   ECP `type` (vegetation classes are expected to be harder).

## Out of scope (for now)

- Wrapping anything into a formal R package (`DESCRIPTION` / `NAMESPACE`).
- Touching the hydrology or logger-recalibration pipelines.
- Producing the final veg-modeling deliverables that consume these
  rasters — that's a separate downstream project.
