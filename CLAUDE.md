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

## Style and workflow

### Coding style

Follow the [tidyverse style guide](https://style.tidyverse.org/)
with the project-specific deviations noted below.
When the style guide and this section conflict, this section wins.

**Project-specific deviations:**

- **Indent is 3 spaces**, not 2 (per `salt-marsh-work.Rproj`).
  Applies to function bodies, continuation lines, function-call argument alignment, and nested blocks.
- **Pipe is base R `|>`**, not magrittr `%>%`.
  New code must use `|>`; do not introduce `%>%`.
- **Function docs are roxygen-style** (`#'` comments above each function)
  even though the repo isn't a package —
  preserve this so the eventual package migration is cheap.
  Include `@param`, `@return`, and `@examples` where useful.
  Use **Markdown formatting** inside roxygen (backticks for code, `[text](url)` for links, etc.) —
  when the package migration happens we'll enable `Roxygen: list(markdown = TRUE)` in `DESCRIPTION` and the docs render correctly with no rewrites.

**Tidyverse rules to apply (highlights):**

- **Names**: `snake_case` for variables and functions;
  nouns for variables, verbs for functions.
  Avoid dots in object names (reserve `.` for S3 dispatch).
- **Assignment**: `<-`, not `=`.
  `=` is only for function-argument matching.
- **Strings**: double quotes (`"x"`) unless the string contains a `"`.
- **Spacing**: spaces around infix operators (`<-`, `=`, `==`, `+`, etc.), after commas, around `|>`.
  No space before a comma; no space inside parentheses except after a comma.
  One blank line between functions; never more than two consecutive blank lines.
- **Line length**: keep lines ≤ 80 characters.
  Break long function calls so each argument sits on its own line, aligned to the opening paren.
- **Comments**: use complete sentences starting with a capital letter when explaining *why*;
  short lower-case fragments are fine for section dividers.
  Don't restate what the code already says.
- **Function structure**: when an argument list exceeds one line, put each argument on its own line.
  Default values are encouraged where they aid readability.
- **Return values**: rely on R's implicit return for the final expression;
  reserve explicit `return()` for early exits.
- **Booleans**: `TRUE` / `FALSE`, never `T` / `F`.
- **Numeric literals**: use `1L` for integers when integer type matters;
  use scientific notation (`1e6`) for large round numbers.

The full tidyverse guide is at <https://style.tidyverse.org/>.
If a rule there isn't repeated above, follow it.

### File organization

- **One function per file** in `R/`, with the filename matching the function name (per BirdFlowR convention).
  When a workflow script (`lidar/02.R`, etc.) accumulates inline helpers that are reusable,
  extract each into its own file under `R/`.

### Linting

- Defer linting until just before commit, not after every edit.
  Mid-development iterations skip lintr;
  when the work is ready to commit,
  run `lintr::lint("R/path/to/file.R")` on each file with new or changed R code,
  fix any hits,
  and only then commit.
- Do **not** lint files that this commit didn't touch — drive-by lint cleanups belong in their own commit.

### Markdown

- Use **semantic line breaks** — one line per sentence or clause —
  in `CLAUDE.md`, `NEWS.md`, `dev/*.md`, and READMEs.
  Diff-friendly and makes review easier.
  Apply to new and edited content;
  existing prose reflows opportunistically when otherwise touched, not retroactively.

### NEWS.md

- Top-level `NEWS.md` is the user-facing changelog.
  When a noteworthy change ships, add bullets to the **top** of the file under a new date heading.
- Format follows BirdFlowR's `NEWS.md`:
  setext-style date heading (`YYYY-MM-DD` underlined with `===`),
  a blank line, asterisk bullets with 4-space hanging indent,
  backticks around function names.
- `NEWS.md` is terser than `dev/worklog.md` —
  log file-by-file detail in the worklog,
  surface only highlights in `NEWS.md`.

### Branching

Work on `main` for now;
feature branches are unnecessary at the current scale.
Revisit if collaboration grows.
