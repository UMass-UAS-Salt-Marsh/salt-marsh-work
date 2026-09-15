# Lidar pipeline architecture

This document explains how the lidar pipeline works: the conceptual
workflow, and which script and function implements each step.
It is scoped to the lidar pipeline only —
the hydrology (`inundation_metrics/`) and
`logger_recalibration/` pipelines are separate and not covered here.

## How this relates to other docs

To avoid duplicating (and then diverging from) the content below,
other lidar docs stick to their own lane:

- **This document** — conceptual workflow and implementation:
  what each stage does, which script and function does it,
  how data flows between stages.
- [`lidar/readme.md`](readme.md) — the project goal ("dream output"),
  site priorities, raw data source locations,
  and the CRS/vertical-bias investigation narrative.
- [`CRS.md`](../CRS.md) (project root) — the project-wide
  CRS/datum/geoid standard, decided independently of any one
  pipeline stage.
- [`dev/workplan.md`](../dev/workplan.md) — the currently active,
  still-open work items.
- [`dev/worklog.md`](../dev/worklog.md) /
  [`dev/worklog-archive.md`](../dev/worklog-archive.md) — dated
  history of what was done and why, including findings and bugs
  fixed along the way.

## Conceptual overview

For each site, the pipeline starts from two raw point clouds
collected in the same year: a **spring** flight (minimal vegetation,
used as the ground reference) and a **summer** flight (full
vegetation, used for the height signal). From those it produces:

1. A **DTM** (digital terrain model) — bare-earth elevation,
   validated against field-collected elevation control points
   (ECPs).
2. A **vegetation-height distribution raster** — a multi-band GeoTIFF
   where each pixel holds the fraction of summer lidar returns
   falling in each of 30 height bins above the ground reference
   (5 cm bins 0–1 m, then 20 cm bins 1–3 m).

Along the way, the ground reference used for (2) turned out not to
be the site's own spring DTM: DTM validation showed all UAS-derived
sources carry a positive elevation bias against ECPs (spring lidar
least of all, but still biased), while a MassGIS aerial-lidar tile
for the same area does not. The pipeline therefore builds a hybrid
ground reference — MassGIS elevation, shifted by an estimated
instrument-level offset — and validates that choice against
field-measured vegetation heights before using it. See "Pipeline
stages" below for how, and `dev/worklog.md` (2026-08-24) for the
full investigation.

## Where things live

- **`lidar/tuning/*.R`** — driver scripts for the human-in-the-loop,
  exploratory half of the pipeline: grid-searching CSF parameters,
  comparing ground sources, and recording the decisions that come out
  of that (`best_csf`, `ground_reference`) into `lidar/runs.yml`. Not
  numbered as a strict sequence — `01_csf_tuning.R` and
  `02_ground_reference_selection.R` are typically run in order per
  flight, but `veg_height_check.R` is an on-demand diagnostic, not a
  required gate.
- **`lidar/production/*.R`** — thin, data-driven driver scripts for
  the repeatable half: once a site/flight's tuning decisions are
  recorded in `runs.yml`, these reproduce its ground raster and
  vegetation-height outputs with no grid search or human judgment
  left, via `build_ground_raster()`.
- **`R/*.R`** — one function per file (matching filename), shared
  across the whole repo, not just lidar. Drivers source them all
  via `lapply(list.files("R/", ...), source)`. `prepare_flight()`,
  `get_run_config()`, `build_ground_raster()`, and
  `build_floor_corrected_ground()` are the functions that bridge
  tuning and production — see `dev/worklog.md`, 2026-09-15, for the
  driver split that introduced them.
- **`rmd/*.Rmd`** — parameterized R Markdown report templates,
  rendered by `report_dtms()` and `report_ground_sources()` rather
  than knitted directly.
- **`lidar/paths.yml`** — a `pathtools` path scheme: every raw
  input cloud/ECP path (fully enumerated — the 2022/2024+ naming
  conventions are too irregular for a template), plus every
  scratch/output path template (`cleaned_tiles`, `ground_raster`,
  `dtm_eval_report`, etc.). Drivers resolve paths via
  `pathtools::get_path("<entry>", site = , date = , ...)` rather than
  hardcoding them. Moved from `lidar/data/paths.yml` as part of the
  tuning/production driver split — see `dev/worklog.md`, 2026-09-15.
- **`lidar/runs.yml`** — hand-edited, not templated: the single
  source of truth for each site's `best_csf` per flight and its
  `ground_reference` decision, read via `get_run_config(site, date)`.
  Tuning drivers produce the values that go in it; production drivers
  (and `build_ground_raster()`) only ever read it.
- **`../lidar_reports/<site>_*/`** — a sibling directory to the repo,
  entirely outside git: cached per-DTM/per-source ECP-sample CSVs and
  rendered HTML reports (`ground_comparison_report`,
  `dtm_eval_report` entries in `paths.yml`).
- **`E:/uas_scratch/lidar/<site>/<date>/`** — large derived rasters
  and point-cloud tiles, not committed. Namespaced by
  `target_epsg` so a CRS-standard change doesn't collide with prior
  output: `reprojected/`, `cleaned_epsg<N>/`,
  `ground_rasters_epsg<N>/`, `veg_heights/`.

Every driver currently hardcodes `site` (and often `date`) as a
top-of-file parameter rather than looping over sites — see
`dev/workplan.md` for the site-by-site rollout status.

## Pipeline stages

Each stage below names the driver, the `R/` functions it calls, and
its inputs/outputs. Function names link to their roxygen docs in
`R/`.

### Steps 0-1 — prepare the flight (conditional reprojection, clean & tile)

Source clouds arrive in WGS 84 (horizontal) + WGS 84 ellipsoidal
height (vertical); everything downstream needs the project's target
CRS (`CRS.md`: NAD83(2011) / Massachusetts Mainland State Plane,
EPSG:6491, + NAVD88 via GEOID18). `prepare_flight()` bundles the two
steps every driver needs before it can touch point-cloud data:
`las_needs_reprojection()` checks the LAS header and skips
reprojection if the source is already in the target CRS (otherwise
`reproject_las()` dispatches to `reproject_las_pdal()` (default) or
`reproject_las_lastools()`, which needs a paid `lasvdatum` license —
see `lidar/readme.md`); then `clean_and_tile()` filters to last
return, removes noise via `lidR::sor()`, and writes cleaned tiles
using `lidR::LAScatalog` + `catalog_map()`. Both steps are
skip-if-exists, so `prepare_flight()` is safe to call from every
driver that needs a flight's cleaned tiles, not just the first one.

- **Called from:** `lidar/tuning/01_csf_tuning.R`,
  `lidar/production/01_ground_raster.R` (`"csf"` ground-reference
  method only), and `lidar/production/02_veg_heights.R` — each call
  after the first for a given site/flight is a fast no-op.
- **Input:** raw LAS resolved via `get_path("raw_lidar", site = , date = )`
  from `lidar/paths.yml`.
- **Output:** `reprojected/*_epsg<N>_navd88.las`,
  `cleaned_epsg<N>/*.las`.

### Step 2 — CSF ground rasterization (DTM tuning grid)

For each row of a small hand-picked CSF (Cloth Simulation Filter)
parameter grid, `rasterize_ground()` classifies ground points and
interpolates a DTM via k-NN IDW, writing one GeoTIFF per parameter
set. `get_path("ground_raster", ...)` resolves the output filename
from the parameter values, per the `ground_raster` template in
`lidar/paths.yml`.

- **Driver:** `lidar/tuning/01_csf_tuning.R` (main loop).
- **Input:** cleaned tiles (Steps 0-1).
- **Output:** `ground_rasters_epsg<N>/csf_th<...>_res<...>_rgd<...>_<res>m.tif`,
  one per grid row.

### Step 3 — DTM evaluation against ECPs

`load_ecp()` reads and tidies the all-sites ECP spreadsheet (filters
to `type = "training"`, drops "Logger Array" points, returns an
`sf` object in the target CRS). For each candidate DTM,
`sample_dtm()` bilinearly samples elevation at every ECP location
(caching to a CSV next to the DTM), then `evaluate_dtm()` computes
residuals and a full metric set (bias, RMSE, MAE, quantiles,
slope, `bias_p`) broken out overall and by ECP `type`, plus
diagnostic plots (`plot_pred_vs_obs()`, `plot_residual_map()`,
`plot_residual_hist()`, `plot_residual_vs_elevation()`,
`plot_qq_residuals()`, via `summarize_residuals()`).

- **Driver:** `lidar/tuning/01_csf_tuning.R` (final section) →
  `report_dtms()`.
- **Report:** `rmd/dtm_evaluation_report.Rmd`.
- **Output:** `../lidar_reports/<site>_<date>/dtm_eval_summary.csv` +
  HTML report + PNG plots.
- **Human step:** review the rendered report, then hand-edit the
  winning CSF parameter set into `lidar/runs.yml`, under
  `sites.<site>.flights.<date>.best_csf`. Nothing downstream reads
  this driver's output automatically — `runs.yml` is the handoff.

Two matching decisions worth knowing when reading or extending this
step: ECPs are matched to a DTM by site only, ignoring date — every
site ECP is compared against each of that site's DTMs regardless of
survey date. And water-logger locations are deliberately excluded as
ECPs, even though a fixed logger-to-floor offset could in principle
convert them into additional elevation points.

### Ground-source comparison

Before committing to a ground reference for vegetation heights, the
site's `best_csf` DTMs (read from `runs.yml` via `get_run_config()`,
not hand-copied literals) are compared against every other available
ground elevation source for the site — photogrammetry DEMs and a
MassGIS aerial-lidar bare-earth tile — using the same `sample_dtm()`
+ `evaluate_dtm()` stack, just pointed at different rasters.

- **Driver:** `lidar/tuning/02_ground_reference_selection.R` (first
  section) → `report_ground_sources()`.
- **Report:** `rmd/ground_source_comparison.Rmd`.
- **Finding:** MassGIS beats every UAS-derived source by a wide
  margin at every site tested, with near-zero bias.
- **Output:** `../lidar_reports/<site>_ground_comparison/` (per-source
  ECP CSVs, `ground_source_comparison.csv`, HTML report).

### Floor-bias-corrected ground reference

MassGIS's near-zero bias makes it the accuracy winner, but it
doesn't share the UAS point cloud's own instrument-level vertical
offset — using it unmodified would leave that offset uncancelled in
every vegetation-height measurement. `estimate_floor_bias()` isolates
just the instrument-level component of the UAS bias from the cached
DTM-vs-ECP residuals (excluding vegetation-contaminated outliers;
`plot_floor_bias()` is its diagnostic companion), and that offset is
added to the (CRS-reprojected) MassGIS raster via
`build_floor_corrected_ground()`.

- **Driver:** `lidar/tuning/02_ground_reference_selection.R` (second
  section), which computes and plots the floor-bias estimate for
  every candidate source flight, then — if `ground_reference.method
  == "massgis_plus_floor"` is already recorded for the site in
  `runs.yml` — calls `build_ground_raster()` to (re)build the hybrid
  raster. If no decision is recorded yet, it stops after the report
  with a reminder to review it and edit `runs.yml`.
- **Human step:** review the ground-source comparison report and
  floor-bias plots, then hand-edit `ground_reference` (`method`,
  `massgis_tile`, `floor_bias_source_flight`) into `runs.yml` under
  `sites.<site>`.
- **Input:** cached ECP-sample CSVs from the ground-source
  comparison step.
- **Output:** `ground_rasters_epsg<N>/massgis_plus_floor_summer.tif`.

### Ground raster (production replay)

Once a site/flight's tuning decisions are recorded, `production/`
drivers never re-run a grid search or re-derive a decision — they
call `build_ground_raster()`, which reads `runs.yml` via
`get_run_config()` and dispatches on `ground_reference.method`: for
`"csf"` it re-runs `rasterize_ground()` once with the recorded
`best_csf` (preparing the cleaned cloud first via `prepare_flight()`
if needed); for `"massgis_plus_floor"` it re-reads the *cached* ECP
CSV already written by the tuning step above, re-runs only
`estimate_floor_bias()` (cheap), and calls
`build_floor_corrected_ground()`. Either branch is skip-if-exists.

- **Driver:** `lidar/production/01_ground_raster.R` (also called
  internally by `lidar/production/02_veg_heights.R`).
- **Output:** the ground raster for the requested site/flight — same
  path templates as the tuning steps above produce.

### Vegetation-height validation (optional diagnostic)

Because ECPs also carry a field-measured `veg_height_m`, the
floor-bias-corrected ground reference can be validated directly
against ground truth rather than by construction alone.
`rasterize_canopy_top()` builds a top-of-canopy DSM (a high
percentile of raw `Z` per pixel, analogous to `rasterize_ground()`
but not ground-normalized) from the summer cleaned tiles; predicted
vegetation height (canopy top minus each candidate ground) is
compared against `veg_height_m` via `sample_dtm()` +
`summarize_residuals()`.

This step is demoted to an optional, on-demand diagnostic, not a
required gate — nothing else in the pipeline reads its output
automatically. Run it by hand for extra confidence in a site's
`ground_reference` pick.

- **Driver:** `lidar/tuning/veg_height_check.R`.
- **Output:** `../lidar_reports/<site>_ground_comparison/veg_height_validation.csv`.

### Vegetation height distribution (final deliverable)

`rasterize_veg_heights()` normalizes summer point heights against
the chosen ground raster (obtained from `build_ground_raster()`, not
a hardcoded prerequisite check) and bins them per pixel via
`bin_fractions()` (a top-level per-cell metric function — it must not
be nested, since `lidR`'s chunked dispatch doesn't preserve closures
referenced only inside a `pixel_metrics()` formula), writing a
31-band GeoTIFF where each band is the fraction of returns in that
height bin, plus a second single-band GeoTIFF recording how many
returns landed in each cell (`n_returns`, the denominator the
fractions are divided by).

- **Driver:** `lidar/production/02_veg_heights.R`.
- **Input:** summer cleaned tiles (Steps 0-1) + the ground raster
  (via `build_ground_raster()`).
- **Output:** `veg_heights/veg_dist_<res>m.tif`,
  `veg_heights/return_counts_<res>m.tif`.

## Data flow

```mermaid
flowchart TD
    A1[/"Spring LAS"/]
    A2[/"Summer LAS"/]
    A1 --> B0
    A2 --> B0
    B0["reproject_las()"]
    B0 --> C1[/"Reprojected spring LAS"/]
    B0 --> C2[/"Reprojected summer LAS"/]

    C1 --> D1
    C2 --> D2
    D1["clean_and_tile()"]
    D2["clean_and_tile()"]
    D1 --> E1[/"Cleaned spring tiles"/]
    D2 --> E2[/"Cleaned summer tiles"/]

    ECP[/"ECP xlsx<br/>elevation + veg_height_m"/]
    LE["load_ecp()"]
    ECP --> LE

    E1 --> F
    E2 --> F
    F["rasterize_ground()<br/>per CSF parameter set<br/>(lidar/tuning/01_csf_tuning.R)"]
    F --> G1[/"Candidate spring DTMs"/]
    F --> G2[/"Candidate summer DTMs"/]

    G1 --> J
    G2 --> J
    LE --> J
    J["sample_dtm() + evaluate_dtm()<br/>(lidar/tuning/01_csf_tuning.R)"]
    K@{ shape: doc, label: "DTM evaluation report<br/>(rmd/dtm_evaluation_report.Rmd)" }
    J --> K
    K --> RUNS[("lidar/runs.yml<br/>best_csf")]

    OTHER[/"Photogrammetry DEMs +<br/>MassGIS bare-earth tile"/]
    G1 --> M
    G2 --> M
    OTHER --> M
    LE --> M
    RUNS --> M
    M["sample_dtm() + evaluate_dtm()<br/>via report_ground_sources()<br/>(lidar/tuning/02_ground_reference_selection.R)"]
    N@{ shape: doc, label: "Ground-source comparison report<br/>(rmd/ground_source_comparison.Rmd)" }
    M --> N

    M --> O
    OTHER --> P
    O["estimate_floor_bias()<br/>(lidar/tuning/02_ground_reference_selection.R)"]
    O --> P[/"Floor-bias-corrected ground raster<br/>massgis_plus_floor_summer.tif"/]
    N --> RUNS2[("lidar/runs.yml<br/>ground_reference")]
    RUNS2 --> O

    RUNS2 --> BGR
    RUNS --> BGR
    BGR["build_ground_raster()<br/>(lidar/production/01_ground_raster.R)"]
    BGR --> P

    E2 --> Q
    Q["rasterize_canopy_top()"]
    Q --> R[/"Summer canopy-top DSM"/]

    R --> S
    P --> S
    LE --> S
    S["sample_dtm() + summarize_residuals()<br/>(lidar/tuning/veg_height_check.R,<br/>optional diagnostic)"]
    S --> T[/"Veg-height validation CSV vs<br/>field veg_height_m"/]

    E2 --> U
    BGR --> U
    U["rasterize_veg_heights()<br/>+ bin_fractions() per pixel<br/>(lidar/production/02_veg_heights.R)"]
    U --> V[/"veg_dist_0.5m.tif<br/>31-band height distribution<br/>(final deliverable)"/]
    U --> W[/"return_counts_0.5m.tif<br/>per-cell return count"/]

    classDef process fill:#dbe9f6,stroke:#4a77a8,color:#1a2b3c
    classDef data fill:#fff3cd,stroke:#b8860b,color:#4a3b00
    classDef report fill:#e1f5e6,stroke:#2f855a,color:#1a3b28

    class B0,D1,D2,LE,F,J,M,O,Q,S,U,BGR process
    class A1,A2,C1,C2,E1,E2,ECP,G1,G2,OTHER,P,R,T,V,W,RUNS,RUNS2 data
    class K,N report
```

Rectangles are functions/scripts; parallelograms are data artifacts
(point clouds, tiles, rasters, cached CSVs); the document-shaped,
green boxes are the two rendered HTML reports.

## See also

- [`lidar/readme.md`](readme.md) — goals, site priorities, data
  source locations, CRS/bias background.
- [`CRS.md`](../CRS.md) — the project-wide CRS/datum/geoid standard.
- [`dev/workplan.md`](../dev/workplan.md) — active open items.
- [`dev/worklog.md`](../dev/worklog.md) — dated history and findings.
