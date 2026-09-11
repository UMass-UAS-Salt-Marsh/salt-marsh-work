# Research notes: current path/config lookup system

Raw findings from an Explore agent, gathered 2026-09-02 as background
for `dev/path_lookup_plan.md`.
Kept verbatim (lightly reformatted) as reference detail that didn't
all fit in the plan document itself.

## 1. `R/update_path.R` — the function itself

Full contents (`R/update_path.R`, 26 lines):

```r
#' Substitute bracketed placeholders in a path with named values
#'
#' Given a path containing `[name]` placeholders and a named list of
#' values, replace each placeholder with the corresponding value.
#'
#' Used by the lidar pipeline to build per-parameter output filenames
#' from a single template, e.g.
#' `"…/csf_th[csf_threshold]_res[csf_res]_rgd[csf_rigidness].tif"`.
#'
#' @param path A character string containing `[name]` placeholders.
#' @param args A named list whose names match the placeholders.
#'   Each value is coerced to character via `paste0()`.
#'
#' @return The input `path` with every `[name]` replaced by `args[[name]]`.
#'   Placeholders not present in `args` are left untouched.
#'
#' @examples
#' update_path("dir/csf_th[th]_res[res].tif", list(th = 0.01, res = 0.1))
#' # "dir/csf_th0.01_res0.1.tif"
update_path <- function(path, args) {
   n <- names(args)
   for (i in seq_along(n)) {
      path <- gsub(paste0("[", n[i], "]"), args[[i]], path, fixed = TRUE)
   }
   path
}
```

**Signature:** `update_path(path, args)` — `path` a single character
string, `args` a named list (or single-row data.frame coerced via
`as.list()`).

**Mechanism:** iterates over `names(args)`, and for each name `n[i]`
does a *literal* (`fixed = TRUE`) `gsub()` of the substring
`"[n[i]]"` (the name wrapped in literal square brackets) for
`args[[i]]` (coerced to character by `gsub`'s internal coercion,
effectively like `paste0`). It does not use regex-style bracket
matching — the brackets are just literal characters being
matched/replaced, not a capture syntax. There is no validation that
every placeholder in `path` got replaced; placeholders whose name
isn't a key in `args` are silently left as literal `[name]` text in
the output.

**Output:** the same string with substitutions applied.

This function itself does no file I/O and does not know about
`paths.csv` — it's a pure string templating helper. It was extracted
out of `lidar/02.R` on 2026-05-13 (per `NEWS.md` line 26 and
`dev/worklog-archive.md` line 987) along with `evaluate_dtm()`,
`visualize_dtm()`, etc., during a "Lidar Phase 0 cleanup."

## 2. `lidar/data/paths.csv`

Full contents (15 data rows + header), `lidar/data/paths.csv`:

```
site, type, date, path, preferred
```

Columns: `site` (lowercase 2-3 letter code, or the literal value
`"all"`/`"oth"` etc.), `type` (`cloud` or `ecp`), `date` (ISO
`yyyy-mm-dd`, blank for `ecp` rows), `path` (Windows path, often
wrapped in extra literal double-quotes and leading/trailing
whitespace — e.g. `" ""X:\legacy\...\ppk_07Nov2022_cloud_1.las"""`),
`preferred` (`1`/`0`).

Representative rows:
- `type = cloud`, 2022 legacy layout:
  `rr, cloud, 2022-08-10," ""X:\legacy\gdrive\saltmarsh_UAS\UAS Data Collection\Red River\2022\LiDAR\10Aug2022_Low\RESEPI-5FFC59-2022-08-10-20-26-50\clouds\ppk_07Nov2022_cloud_1.las""",1`
  — `preferred = 1`, and an alternate row for the same site/date with
  a different processing-date filename (`ppk_cloud_1.las`),
  `preferred = 0`.
- `type = cloud`, 2024 layout:
  `nor,cloud, 2024-10-06," ""X:\projects\uas\sites\nor\lidar_point_cloud\2024_10_06_low_hesaixt32\RESEPI-A84717-2024-10-06-12-46-16\clouds\NOR_2024_10_06_low_lidar_pointcloud.las""",1`
- `type = ecp`, no date, shared across all sites:
  `all, ecp, ,"""X:\legacy\gdrive\saltmarsh_UAS_native\In Situ Data Collection\JoshSurveyPoints_AllSites_One_Sheet.xlsx""",1`,
  plus per-site legacy `ecp` rows for `oth`, `rr`, `wel` all with
  `preferred = 0`.

There is **no `ground_raster_template` column** in `paths.csv` —
that name refers to a `paths$ground_raster_template` list entry that
`lidar/02.R` *constructs at runtime* (see §3), not to anything stored
in the CSV. `CLAUDE.md` line 39 ("The `paths$ground_raster_template`
uses `[name]` placeholders substituted by `update_path()`") is
describing that runtime list element, not a CSV column.

**How `preferred` is used:** In `lidar/02.R` line 128, right after
reading the CSV, the whole table is filtered:
`input_file_paths <- input_file_paths[input_file_paths$preferred, ]`
(after coercing `preferred` to logical at line 125). So non-preferred
rows (the "0" rows, e.g. earlier/duplicate processing runs, or
per-site legacy ECP files superseded by the combined `all` sheet) are
dropped entirely before any site/date matching happens, and only
preferred rows are ever selected. `lidar/05_veg_heights.R` does the
same filter inline in its `which(...)` condition (line 98:
`input_file_paths$preferred &`) rather than pre-filtering the table.

## 3. Every call site of `update_path(`

A repo-wide grep for `update_path(` in `*.R` finds exactly **one
real call site** (plus the roxygen example and an architecture-doc
mention, which aren't code):

- `R/update_path.R:18` — inside the `@examples` doc comment (not a
  call site).
- `lidar/ARCHITECTURE.md:122-123` — prose reference, not code.
- `lidar/02.R:219` — a comment: `# update_path() comes from
  R/update_path.R.`
- **`lidar/02.R:235`** — the actual call.

Surrounding context (`lidar/02.R`, lines 218-253):

```r
# Find ground with several parameters - output to raster
# update_path() comes from R/update_path.R.

# Make a raster for each parameter set
csf_results <- data.frame()
for (i in seq_len(nrow(csf_grid))) {
   csf_results[i, ] <- NA
   csf_results$site[i] <- site
   csf_results$date[i] <- date

   params <- as.list(csf_grid[i, , drop = FALSE]) # csf parameters

   for (n in names(params)) {
      csf_results[[n]][i] <- params[[n]]
   }

   output_path <- update_path(paths$ground_raster_template, params)

   csf_results$dtm[i] <- output_path

   # Full argument list for rasterize_ground
   args <- c(list(input = paths$cleaned_catalog_dir,
                  output = output_path,
                  chunk_size = chunk_size,
                  chunk_buffer = chunk_buffer),
             params)

   if (!file.exists(output_path)) {
      message("Starting ground rasterization ", lubridate::now())
      ground <- do.call(rasterize_ground, args)
      message("Done ground rasterization ", lubridate::now())
   } else {
      message("Skipping DTM. ", output_path, " already exists. ")
   }
}
```

**What it's constructing:** the per-CSF-parameter-set DTM output
filename. `paths$ground_raster_template` is built earlier in the
same file (lines 171-176):

```r
paths$ground_raster_template <- file.path(
   paths$base_output,
   paste0("zzzraster_epsg", target_epsg,
          "/csf_th[csf_threshold]_res[csf_res]",
          "_rgd[csf_rigidness]_[raster_res]m.tif")
)
```

where `paths$base_output <- file.path("E:/uas_scratch/lidar", site,
date)` (line 167). So the template is e.g.
`E:/uas_scratch/lidar/rr/2022_08_10/zzzraster_epsg6491/csf_th[csf_threshold]_res[csf_res]_rgd[csf_rigidness]_[raster_res]m.tif`,
and each loop iteration's call to
`update_path(paths$ground_raster_template, params)` (where `params`
is one row of `csf_grid`, e.g. `list(csf_res=0.1, csf_threshold=0.01,
csf_rigidness=2, raster_res=0.25)`) substitutes in that iteration's
parameter values to get one concrete DTM path such as
`.../csf_th0.01_res0.1_rgd2_0.25m.tif`.

No other pipeline stage (`03_evaluate_dtm.R`, `05_veg_heights.R`,
`06_compare_ground_sources.R`, `07_floor_corrected_ground.R`,
`08_veg_height_validation.R`) calls `update_path()`. Cleaned-tile
directories, DTM-evaluation output dirs, and report output dirs are
all built with plain `file.path()`/`paste0()` string concatenation
instead (e.g. `lidar/02.R:168-182` for
`cleaned_catalog_dir`/`reprojected_path`; `lidar/05_veg_heights.R:108-114`
for `summer_clean_dir`/`output_dir`/`output_tif`).

## 4. Other path-lookup/config mechanisms

**`inundation_metrics/Data/sites.txt`** — tab-separated
(`readr::read_tsv`), header: `site  site_name  share  footprint
transects  standard`. Full contents (10 sites: sor, bar, peg, wel,
rr, oth, ess, nor, wes). Example row: `rr\tRed River\t\tOriginal
Vector/Site Footprint/RR_100ac_Mask_27Mar23.shp\tRR_Site_Polygon_Layer.shp\tOriginal
Raster/UAS Orthomosaic/<SITE>/26May22_RR_Low_Mica_Ortho.tif`.

Only the `site` and `site_name` columns are ever consumed by code
(confirmed by grep — `footprint`, `transects`, `standard`, `share`,
and the `<SITE>` placeholder syntax inside the `standard` column do
not appear anywhere else in any `.R`/`.Rmd` file). It's read
identically in four places, always immediately followed by an
uppercasing + `RR`→`RED` remap and a hardcoded `selected_sites`
filter:
- `inundation_metrics/01_calculate_metrics_and_combine.R:23-34`
- `inundation_metrics/02_hydrology_regression_and_plots.Rmd:18-22`
- `logger_recalibration/recalibrate_sites.R:27-35`
- `logger_recalibration/recalibration_report.Rmd:46-54`

Representative pattern (`01_calculate_metrics_and_combine.R:22-34`):
```r
sites <- readr::read_tsv("inundation_metrics/Data/sites.txt", show_col_types = FALSE) |>
   select(site, site_name)
sites$site <- toupper(sites$site)
sites$site[sites$site == "RR"] <- "RED"
selected_sites <- c("RED", "OTH", "WES", "WEL")
sites <- sites[match(selected_sites, sites$site), ]
sites$deployment_file_name <- c("15Sep2022_RED_Deployments.csv",
                                 "OTH_LoggerArray_Pull.csv",
                                 "15Aug2022_WES_ArrayDataSheet.csv",
                                 "19Oct2022_WEL_Array_withFloodingMetrics.csv")
```
Note `deployment_file_name` is a hand-maintained parallel vector, not
a `sites.txt` column — a second, inline logical-name→filename mapping
layered on top of the table, position-matched to `selected_sites`,
duplicated verbatim across all four files.

`CLAUDE.md:50` documents this as: "Site codes are 3-letter lowercase
for lidar (`rr`, `nor`, `bar`, `wel`) and 3-letter uppercase for
hydrology (`RED`, `OTH`, `WES`, `WEL`). They are not always the same
code for the same site — `RED` ↔ `rr` (Red River). The canonical
mapping table is `inundation_metrics/Data/sites.txt`."

**Other hardcoded/ad hoc path spots (no lookup table at all):**
- `lidar/01_explore_data.R:9` — `paths$gcp <- "lidar/data/RedRiver_11May2022.csv"`
  (literal string, not looked up from `paths.csv`), read at line 151
  via `readr::read_csv(paths$gcp)`.
- `lidar/05_veg_heights.R:64-75` — `ground_raster` is a hand-built
  `file.path("E:/uas_scratch/lidar", site, "2022_08_10",
  paste0("zzzraster_epsg", target_epsg),
  "massgis_plus_floor_summer.tif")`, with the *legacy alternative*
  path given only as a commented-out example (lines 68-71), not
  selected via any config/lookup mechanism — it's edited by hand.
- No `config.yml`/`.json`/`options()`-based config exists anywhere in
  the repo (checked via search for other CSV/config files feeding
  path lookups — the only CSV that plays a routing/lookup role is
  `lidar/data/paths.csv`; the many other `.csv` files under
  `lidar/output/` and `inundation_metrics/Data/` are pipeline
  *data/output* artifacts, not path/config registries).

## 5. `lidar/ARCHITECTURE.md` and `lidar/readme.md` on file layout / paths.csv

**`lidar/ARCHITECTURE.md`**, "Where things live" section (lines
56-82):
- `lidar/data/paths.csv` — "registry of available input point clouds
  and ECP files (columns: `site`, `type`, `date`, `path`,
  `preferred`). Drivers look up their input row here rather than
  hardcoding paths." (lines 67-70)
- `lidar/output/<site>_*/` — "small, committed-size artifacts: cached
  per-DTM/per-source ECP-sample CSVs and rendered HTML reports."
  (lines 71-73)
- `E:/uas_scratch/lidar/<site>/<date>/` — "large derived rasters and
  point-cloud tiles, not committed. Namespaced by `target_epsg` so a
  CRS-standard change doesn't collide with prior output:
  `reprojected/`, `zzzcleaned_epsg<N>/`, `zzzraster_epsg<N>/`,
  `zzzheights/`." (lines 74-78)
- "Every driver currently hardcodes `site` (and often `date`) as a
  top-of-file parameter rather than looping over sites — see
  `dev/workplan.md`'s Phase 3 for the site-by-site rollout status."
  (lines 80-82)
- Step 2 stage description explicitly ties `update_path()` to the CSF
  output-filename templating: "`update_path()` fills in the output
  filename template from the parameter values." (lines 122-123), with
  output pattern
  `zzzraster_epsg<N>/csf_th<...>_res<...>_rgd<...>_<res>m.tif` (line
  127).
- Step 0 (reprojection) input: "raw LAS from `lidar/data/paths.csv`"
  (line 102).

**`lidar/readme.md`**, "Files" section (lines 37-46) documents the
two LAS naming/layout schemes referenced by `paths.csv`:
```
####LAS files - two schemes
* 2022 -- SCan date in folder path, date in file is processing date example:
"X:\legacy\gdrive\saltmarsh_UAS\UAS Data Collection\Red River\2022\LiDAR\10Aug2022_Low\RESEPI-5FFC59-2022-08-10-20-26-50\clouds\ppk_07Nov2022_cloud_1.las"
* 2024 - Not in legacy projects/uas/sites/[site]/lidar_point_cloud/  example path:
"X:\projects\uas\sites\bar\lidar_point_cloud\2024_06_02_low_hesaixt32\RESEPI-A84717-2024-06-02-16-57-55\clouds\BAR_PointCloud_02Jun2024.las"
```
It also documents the ECP file location (lines 38-40):
`X:\legacy\gdrive\saltmarsh_UAS_native\In Situ Data Collection\JoshSurveyPoints_AllSites_Meta_Datapoints.xlsx`
("Use type = \"training\"") and a second, extract-values variant xlsx
— these correspond to the `type = ecp` rows in `paths.csv` (note the
`readme.md` xlsx filename, `..._Meta_Datapoints.xlsx`, differs from
the `..._One_Sheet.xlsx` filename actually referenced in the current
`paths.csv` row for `all/ecp` — the two docs/data disagree on the
exact ECP filename in current use).

`lidar/readme.md` does not mention `update_path()` or the `[name]`
placeholder mechanism at all — that documentation lives only in
`R/update_path.R`'s roxygen block, `lidar/ARCHITECTURE.md`, and
`CLAUDE.md:39`.

**`CLAUDE.md`** adds two more concrete facts worth noting for
completeness: line 39 — "Output directories: cleaned tiles →
`E:/uas_scratch/lidar/<site>/<date>/zzzcleaned/`; DTMs →
`…/zzzraster/csf_th<...>_res<...>_rgd<...>_<res>m.tif`. The
`paths$ground_raster_template` uses `[name]` placeholders substituted
by `update_path()`." — and line 52 — "`lidar/data/` is also
gitignored except for `paths.csv` and `RedRiver_11May2022.csv` (a
small GCP file)," confirming `paths.csv` is the one lidar-data file
deliberately tracked in git as the source of truth (also stated in
`dev/worklog-archive.md:847`: "`paths.csv` deliberately left as the
stable source-of-truth; reprojected files are workflow cache only, on
the local RAID.").
