# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Repository nature

Exploratory R code for the Saltmarsh project. **Not an R package** — there is no `DESCRIPTION`, `NAMESPACE`, or `tests/` directory, and nothing is installed. Code is intended to migrate into a formal package once it matures (per `README.md`), so when adding functions try to keep them package-compatible (roxygen docs, no side effects at source time, no hardcoded absolute paths inside function bodies).

## How code is loaded and run

There is no build, install, lint, or test command. Function files in `R/` are sourced ad-hoc by workflow scripts using:

```r
a <- lapply(list.files("R/", pattern = "\\.[Rr]$", full.names = TRUE), source)
```

Workflow scripts live under topic subdirectories (`hydrology/`, `lidar/`, `logger_recalibration/`) and are numbered (`01_…`, `02_…`) to indicate execution order. Run them from the project root in RStudio (open `salt-marsh-work.Rproj`) so relative paths like `"hydrology/Data/sites.txt"` resolve. `.Rmd` files use `here::here()` and `setwd(here::here())` in their setup chunks for the same reason.

## High-level architecture

Two largely independent pipelines that share the `R/` function library:

### Hydrology pipeline (`hydrology/`)
Processes water-depth logger time series into per-logger inundation metrics, then fits regressions of inundation vs. logger elevation.

- `01_calculate_metrics_and_combine.R` — for each of four sites (RED, OTH, WES, WEL) reads calibrated logger Excel files plus a deployment CSV, calls `calculate_inundation_metrics()`, and writes the combined result to `hydrology/Data/four_sites.Rds`.
- `02_hydrology_regression_and_plots.Rmd` — consumes `four_sites.Rds`, produces scatter plots and a per-site/per-response linear model table (`Model_Coefficients.csv`).
- Key shared functions: `calculate_inundation_metrics()` (orchestrator), `read_water_logger_data()` (handles header serial-number check, text-date recovery, and the various depth column names — `sensor_depth`, `sensor_depth_brackish`), `read_water_logger_deployments()` (handles both `Serial` and `Serial #`, and both `dmy` and `mdy` date formats encountered across sites).
- Logger inundation threshold is hardcoded at `depth > 0.02` m. Metrics use `VulnToolkit::fld.dur()`, `dur.events()`, `fld.depth()`.

### Lidar pipeline (`lidar/`)
Processes LAS point clouds into ground rasters (DTMs) for a single site at a time, evaluating multiple Cloth Simulation Filter (CSF) parameter sets.

- `01_explore_data.R` — interactive exploration on a single chunk; not part of the production flow.
- `02.R` — production-style driver. Reads `lidar/data/paths.csv` (a table of available input clouds and elevation control point files, columns include `site`, `type`, `date`, `path`, `preferred`), picks the preferred cloud for a target `site`, runs `clean_and_tile()` once, then loops over a small grid of CSF parameters calling `rasterize_ground()` and writes per-parameter `.tif` files.
- `R/clean_and_tile.R` — Step 1: filter to last return, classify+drop noise via `lidR::sor`, write cleaned tiles. Skips automatically if output tiles already exist.
- `R/rasterize_ground.R` — Step 2: classify ground with `csf(class_threshold, cloth_resolution, rigidness)`, then `rasterize_terrain()` with `knnidw` into a single GeoTIFF.
- Both functions use `lidR::LAScatalog` + `catalog_map()` to process in tiles (`chunk_size`/`chunk_buffer` defaults 200/20 m). The full pipeline is memory-bound, not CPU-bound — be cautious with `future::plan(multisession, workers = N)` (see comments in `lidar/02.R`).
- Output directories: cleaned tiles → `E:/uas_scratch/lidar/<site>/<date>/zzzcleaned/`; DTMs → `…/zzzraster/csf_th<...>_res<...>_rgd<...>_<res>m.tif`. The `paths$ground_raster_template` uses `[name]` placeholders substituted by `update_path()`.
- See `lidar/readme.md` for the dream output (multi-band veg-height-distribution raster), site priorities, and important file-layout notes (2022 LAS files live under `X:/legacy/gdrive/saltmarsh_UAS/…`; 2024+ under `X:/projects/uas/sites/<site>/lidar_point_cloud/…`).

### `logger_recalibration/`
Separate exploratory scripts by another collaborator for finding common high tides across loggers and producing recalibration diagnostics. Uses the same `R/` helpers (`find_high_tides`, `find_common_high_tides`, `assess_water_logger_errors`, `spatial_plotting`). `recalibate_sites.R` and `recalibrate_sites2.R` are alternate drafts — confirm with the user which is current before editing.

## Data conventions

- Site codes are 3-letter lowercase for lidar (`rr`, `nor`, `bar`, `wel`) and 3-letter uppercase for hydrology (`RED`, `OTH`, `WES`, `WEL`). They are not always the same code for the same site — `RED` ↔ `rr` (Red River). The canonical mapping table is `hydrology/Data/sites.txt`.
- Hydrology data folders (`hydrology/Data/<SITE>/Calibrated Data/*cal.xlsx` plus a per-site deployment CSV) are gitignored — never committed.
- `lidar/data/` is also gitignored except for `paths.csv` and `RedRiver_11May2022.csv` (a small GCP file).
- Elevation control points live in `X:/legacy/gdrive/saltmarsh_UAS_native/In Situ Data Collection/JoshSurveyPoints_AllSites_Meta_Datapoints.xlsx` (use rows with `type = "training"`).

## Style

- The project uses base R pipe (`|>`), not magrittr `%>%`. Indent is 3 spaces (per `salt-marsh-work.Rproj`).
- Function docs are roxygen-style even though the repo isn't a package — preserve this so the eventual package migration is cheap.
