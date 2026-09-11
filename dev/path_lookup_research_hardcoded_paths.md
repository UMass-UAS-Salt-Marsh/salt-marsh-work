# Research notes: hardcoded paths and report-output locations

Raw findings from an Explore agent, gathered 2026-09-02 as background
for `dev/path_lookup_plan.md`.
Kept verbatim (lightly reformatted) as reference detail that didn't
all fit in the plan document itself.

Repo root: `X:\projects\uas_veg\code\salt-marsh-work`

---

## 1. Hardcoded absolute paths

Scope covered: `R/*.R`, `lidar/*.R`, `lidar/*.Rmd` (none exist — the
pipeline's Rmds live in `rmd/*.Rmd` instead, noted where relevant),
`inundation_metrics/*.R`, `inundation_metrics/*.Rmd`,
`logger_recalibration/*.R`, `logger_recalibration/*.Rmd`.

**`inundation_metrics/` and `logger_recalibration/` contain zero
hardcoded absolute paths and zero `here::here()`/`setwd()` calls of
the `X:`/`E:` kind** — all their paths are relative
(`inundation_metrics/Data/...`) — **except**:
- `inundation_metrics/02_hydrology_regression_and_plots.Rmd:11` and
  `:132` — `setwd(here::here())`
- `logger_recalibration/recalibration_report.Rmd:28` — `root.dir <-
  here::here()` then `knitr::opts_knit$set(root.dir = root.dir)`

Everything else below is in `R/` or `lidar/`.

### `R/clean_and_tile.R` (lines 2-10, inside `if(FALSE){...}` scratch block, not executed)
```r
if(FALSE){
   path <- "X:\\projects\\uas\\sites\\nor\\lidar_point_cloud\\2024_10_06_low_hesaixt32\\RESEPI-A84717-2024-10-06-12-46-16\\clouds\\NOR_2024_10_06_low_lidar_pointcloud.las"
   base_output <- "X:/projects/uas/sites/nor/model_output/lidar"
   output_dir <- file.path(base_output, "2024_10_06", "zzzcleaned")
   dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
}
```
Kind: raw LAS input (2024+ site convention) + scratch/working output
dir example.

### `R/las_needs_reprojection.R:61` (roxygen `@examples`)
```r
#' las_needs_reprojection(
#'    "E:/uas_scratch/lidar/rr/2022_08_10/reprojected/foo.las"
#' )
```
Kind: reprojected LAS scratch path, doc example only.

### `R/load_ecp.R:66` (roxygen `@examples`)
```r
#' rr_ecp <- load_ecp(
#'    path = paste0(
#'       "X:/legacy/gdrive/saltmarsh_UAS_native/",
#'       "In Situ Data Collection/",
#'       "JoshSurveyPoints_AllSites_One_Sheet.xlsx"
#'    ),
```
Kind: elevation control point (ECP) spreadsheet, doc example.

### `R/rasterize_ground.R:30-32` (roxygen `@examples`)
```r
#' rasterize_ground(
#'    input  = "E:/uas_scratch/lidar/rr/2022_08_10/zzzcleaned",
#'    output = paste0("E:/uas_scratch/lidar/rr/2022_08_10/zzzraster/",
#'                    "csf_th0.06_res0.10_rgd2_0.25m.tif"),
```
Kind: cleaned-tile input + DTM raster output, doc example.

### `R/rasterize_veg_heights.R:34-37` (roxygen `@examples`)
```r
#' rasterize_veg_heights(
#'    input  = "E:/uas_scratch/lidar/rr/2022_08_10/zzzcleaned",
#'    dtm    = paste0("E:/uas_scratch/lidar/rr/2022_05_14/zzzraster/",
#'                    "csf_th0.06_res0.10_rgd2_0.25m.tif"),
#'    output = paste0("E:/uas_scratch/lidar/rr/2022_08_10/zzzheights/",
#'                    "veg_dist_0.5m.tif")
```
Kind: cleaned-tile input, DTM, veg-height raster output, doc example.

### `R/report_dtms.R` — **live default argument values**, not just doc
- Line 21 (doc): `#'    Defaults to \`E:/uas_scratch/lidar/<site>/<date>\`.`
- Lines 53-58 (actual function default arguments):
```r
report_dtms <- function(
      site,
      date,
      ecp_path    = paste0(
         "X:/legacy/gdrive/saltmarsh_UAS_native/",
         "In Situ Data Collection/",
         "JoshSurveyPoints_AllSites_One_Sheet.xlsx"
      ),
      base_output = file.path("E:/uas_scratch/lidar", site, date),
      ...
```
Kind: `ecp_path` = elevation control points; `base_output` = DTM
scratch tree root (per-site/date, contains `zzzraster_epsg<...>/`).

### `R/report_ground_sources.R` — also **live default argument values**
- Lines 9-10, 31, 34 (doc examples): `E:/…/csf_th0.01…tif`,
  `X:/…/be_tile.tif`, full paths
  `E:/uas_scratch/lidar/rr/2022_05_14/zzzraster_epsg6491/csf_th0.01_res0.1_rgd2_0.25m.tif`,
  `X:/scratch/bcompton/LiDAR/be_19TDG412612/be_19TDG412612.tif`.
- Lines 43-46 (actual function default argument):
```r
report_ground_sources <- function(
      sources,
      site,
      ecp_path    = paste0(
         "X:/legacy/gdrive/saltmarsh_UAS_native/",
         "In Situ Data Collection/",
         "JoshSurveyPoints_AllSites_One_Sheet.xlsx"
      ),
```
Kind: ECP spreadsheet default. (The `output_dir` default argument,
`file.path("lidar/output", paste0(site, "_ground_comparison"))`, is
in the same signature block, just below this excerpt.)

### `R/reproject_las.R` — lines 71-95
```r
#' reproject_las(
#'    input = "X:/.../ppk_07Nov2022_cloud_1.las",
#'    output = paste0(
#'       "E:/uas_scratch/lidar/rr/2022_08_10/reprojected/",
#'       "ppk_07Nov2022_cloud_1_epsg6491_navd88.las"
```
and, as a **live default argument** (line 92):
```r
reproject_las <- function(input,
                          output,
                          target_epsg = 6491L,
                          vgrid = paste0(
                             "X:/legacy/gdrive/UMassAir User",
                             " Resources/LASTools/Geoid",
                             " Transformation GTX Files/",
                             "geoid18/us_noaa_g2018u0.tif"
                          ),
```
Kind: raw LAS input / reprojected LAS scratch output (doc);
vertical-datum geoid grid file (live default).

### `R/reproject_las_lastools.R:111-117` and `R/reproject_las_pdal.R:85-97` (roxygen `@examples`)
Both show a raw-LAS input path under `X:/legacy/gdrive/saltmarsh_UAS/UAS
Data Collection/Red River/2022/LiDAR/10Aug2022_Low/...` and a
reprojected-output path under
`E:/uas_scratch/lidar/rr/2022_08_10/reprojected/...`.
`reproject_las_pdal.R:96` also documents the `vgrid` geoid-grid path
under `X:/legacy/gdrive/UMassAir User Resources/LASTools/Geoid
Transformation GTX Files/geoid18/us_noaa_g2018u0.tif`.

### `R/sample_dtm.R:50` (roxygen `@examples`)
```r
#' elevations <- sample_dtm(
#'    dtm = paste0(
#'       "E:/uas_scratch/lidar/rr/2022_08_10/zzzraster/",
#'       "csf_th0.005_res0.05_rgd2_0.25m.tif"
#'    ),
```
Kind: DTM raster input, doc example.

### `lidar/02.R` (production driver) — **live, executed code**
- Line 38 (comment): `# Outputs (under \`E:/uas_scratch/lidar/<site>/<date>/\`)`
- Line 167:
```r
paths$base_output <- file.path("E:/uas_scratch/lidar", site, date)
```
Kind: scratch/working root for cleaned tiles + CSF DTMs for the run.

### `lidar/03_evaluate_dtm.R` (production driver) — **live, executed code**
- Lines 34-38:
```r
ecp_path <- paste0(
   "X:/legacy/gdrive/saltmarsh_UAS_native/",
   "In Situ Data Collection/",
   "JoshSurveyPoints_AllSites_One_Sheet.xlsx"
)
```
Kind: ECP spreadsheet input.
- Line 57 (inside the `for` loop):
```r
base_output <- file.path("E:/uas_scratch/lidar", site, date)
```
Kind: source of DTMs to be evaluated (passed to
`report_dtms(base_output = ...)`). Comment at line 55-56 clarifies:
`# Note base_output is for output data it doesn't define / # where
the report is generated which is currently within  ./lidar/output/`.

### `lidar/05_veg_heights.R` — **live, executed code**
- Line 30 (comment): `# Outputs (under
  E:/uas_scratch/lidar/<site>/<summer_date>/zzzheights/)`
- Lines 69-71 (comment showing an alternative, commented-out
  ground_raster path):
```r
#   ground_raster <- file.path(
#      "E:/uas_scratch/lidar", site, "2022_05_14",
#      "zzzraster_epsg6491", "csf_th0.01_res0.1_rgd2_0.25m.tif"
#   )
```
- Lines 72-75 (live):
```r
ground_raster <- file.path(
   "E:/uas_scratch/lidar", site, "2022_08_10",
   paste0("zzzraster_epsg", target_epsg), "massgis_plus_floor_summer.tif"
)
```
- Lines 108-114 (live):
```r
summer_clean_dir <- file.path("E:/uas_scratch/lidar", site,
                              summer_date_uu,
                              paste0("zzzcleaned_epsg", target_epsg))
output_dir <- file.path("E:/uas_scratch/lidar", site,
                        summer_date_uu, "zzzheights")
output_tif <- file.path(output_dir,
                        paste0("veg_dist_", raster_res, "m.tif"))
```
Kind: ground-reference raster input, cleaned-tile input,
veg-height-distribution raster output — all on the `E:` scratch tree.

### `lidar/06_compare_ground_sources.R` — **live, executed code**, 3 site blocks (rr, oth, wel)
- Lines 30-37 (rr, lidar sources):
```r
sources <- c(
   lidar_spring = file.path(
      "E:/uas_scratch/lidar", site, "2022_05_14",
      paste0("zzzraster_epsg", target_epsg), best_csf_spring
   ),
   lidar_summer = file.path(
      "E:/uas_scratch/lidar", site, "2022_08_10",
      paste0("zzzraster_epsg", target_epsg), best_csf_summer
   ),
```
- Lines 39-57 (rr, external/reference sources):
```r
photo_spring_hesai = paste0(
   "X:/legacy/gdrive/saltmarsh_UAS/UAS Data Collection/",
   "Red River/Orthos and DEMs 2022/26May2022/Low/",
   "26May2022_RED_Low_HesaiRGB_DEM.tif"
),
...
massgis = paste0(
   "X:/scratch/bcompton/LiDAR/",
   "be_19TDG412612/be_19TDG412612.tif"
)
```
- Lines 72-83 (oth site):
  `X:/legacy/gdrive/saltmarsh_UAS/UAS Data Collection/Old Town
  Hill/...` orthophoto DEMs, and `massgis_346737 =
  "X:/scratch/bcompton/LiDAR/be_19TCH346737.tif"`, `massgis_346735 =
  "X:/scratch/bcompton/LiDAR/be_19TCH346735.tif"`.
- Lines 96-102 (wel site):
  `X:/legacy/gdrive/saltmarsh_UAS/UAS Data Collection/Wellfleet
  Bay/...` orthophoto DEM, and `massgis_415636 =
  "X:/scratch/bcompton/LiDAR/be_19TDG415636.tif"`, `massgis_417636 =
  "X:/scratch/bcompton/LiDAR/be_19TDG417636.tif"`.

Kind: UAS-derived DTM comparison inputs (own pipeline output on `E:`
scratch) plus third-party reference ground rasters (MassGIS
bare-earth tiles, drone-orthophoto DEMs on the `X:` legacy
Google-Drive mount).

### `lidar/07_floor_corrected_ground.R` — **live, executed code**
- Lines 43-48 (comment + code):
```r
# Corrected ground raster is written alongside the other rr summer
# rasters (CSF DTMs etc.) in the E: scratch tree, not under
# lidar/output/ — it's a large derived raster like those, not a small
# comparison artifact.
target_epsg <- 6491L
summer_raster_dir <- file.path("E:/uas_scratch/lidar", site, "2022_08_10",
                               paste0("zzzraster_epsg", target_epsg))
```
- Lines 50-52:
```r
massgis_path <- paste0(
   "X:/scratch/bcompton/LiDAR/be_19TDG412612/be_19TDG412612.tif"
)
```
- Line 103-106: writes `corrected_ground_tif <-
  file.path(summer_raster_dir, "massgis_plus_floor_summer.tif")` via
  `terra::writeRaster(...)`.

Kind: MassGIS reference raster input, corrected ground raster output
(on `E:` scratch).

### `lidar/08_veg_height_validation.R` — **live, executed code**
- Lines 46-50: same ECP xlsx path as `03_evaluate_dtm.R`
  (`X:/legacy/gdrive/saltmarsh_UAS_native/In Situ Data
  Collection/JoshSurveyPoints_AllSites_One_Sheet.xlsx`).
- Lines 55-60 (comment + code):
```r
# Large derived rasters live alongside the other rr summer rasters in
# the E: scratch tree, not under lidar/output/.
summer_raster_dir <- file.path("E:/uas_scratch/lidar", site, summer_date_uu,
                               paste0("zzzraster_epsg", target_epsg))
massgis_floor_tif <- file.path(summer_raster_dir,
                               "massgis_plus_floor_summer.tif")
```
- Lines 100-103: `spring_elev <- sample_dtm(dtm =
  file.path("E:/uas_scratch/lidar", site, "2022_05_14",
  paste0("zzzraster_epsg", target_epsg),
  "csf_th0.01_res0.1_rgd2_0.25m.tif"), ...)`.

Kind: ECP input, corrected-ground raster and spring-DTM inputs on
`E:` scratch.

### `rmd/dtm_evaluation_report.Rmd` (rendered by `report_dtms()`, invoked from `lidar/03_evaluate_dtm.R`) — YAML `params:` defaults, lines 11-18:
```yaml
params:
  site:        "rr"
  date:        "2022_05_14"
  ecp_path:    "X:/legacy/gdrive/saltmarsh_UAS_native/In Situ Data Collection/JoshSurveyPoints_AllSites_One_Sheet.xlsx"
  base_output: "E:/uas_scratch/lidar/rr/2022_05_14"
  eval_dir:    "lidar/output/rr_2022_05_14"
  tolerances:  [0.10, 0.20]
  ecp_types:   ["EVP"]
  target_epsg: 6491
```
Also line 26: `setwd(here::here())`.

### `rmd/ground_source_comparison.Rmd` (rendered by `report_ground_sources()`, invoked from `lidar/06_compare_ground_sources.R`) — YAML `params:` default, line 13:
```yaml
ecp_path:   "X:/legacy/gdrive/saltmarsh_UAS_native/In Situ Data Collection/JoshSurveyPoints_AllSites_One_Sheet.xlsx"
```
Also line 25: `setwd(here::here())`, and lines 46-58 (fallback
default `sources` for standalone-knit, `oth` site): same 4
orthophoto/MassGIS paths under
`X:/legacy/gdrive/saltmarsh_UAS/...` and
`X:/scratch/bcompton/LiDAR/be_19TCH34673{5,7}.tif` as in
`lidar/06_compare_ground_sources.R`.

### Data file: `lidar/data/paths.csv` (committed — one of the 2 non-gitignored files under `lidar/data/`)
Every row's `path` column is a hardcoded absolute `X:\...` path — the
raw LAS clouds (`X:\legacy\gdrive\saltmarsh_UAS\...` for 2022 sites,
`X:\projects\uas\sites\<site>\lidar_point_cloud\...` for 2024+ sites)
and the ECP xlsx files
(`X:\legacy\gdrive\saltmarsh_UAS_native\...`,
`X:\legacy\gdrive\2022_23_field_data\Classification Points\Organized
Data\...`). This is the table `lidar/02.R:122` reads via
`readr::read_csv("lidar/data/paths.csv")` to resolve `paths$input`
and `paths$ecp`.

### `R/update_path.R`
No hardcoded path, but relevant to path *construction*: substitutes
`[name]` placeholders in a path template (e.g.
`paths$ground_raster_template` from `lidar/02.R:171-176`) with values
from a named list — this is how per-CSF-parameter-set output
filenames are built
(`csf_th[csf_threshold]_res[csf_res]_rgd[csf_rigidness]_[raster_res]m.tif`).

---

## 2. Output directory / file path construction (report/raster/CSV)

### `lidar/02.R:167-182` (live pipeline paths, all executed)
```r
paths$base_output <- file.path("E:/uas_scratch/lidar", site, date)
paths$cleaned_catalog_dir <- file.path(
   paths$base_output, paste0("zzzcleaned_epsg", target_epsg)
)
paths$ground_raster_template <- file.path(
   paths$base_output,
   paste0("zzzraster_epsg", target_epsg,
          "/csf_th[csf_threshold]_res[csf_res]",
          "_rgd[csf_rigidness]_[raster_res]m.tif")
)
paths$reprojected_dir <- file.path(paths$base_output, "reprojected")
paths$reprojected_path <- file.path(
   paths$reprojected_dir,
   sub("\\.las$", paste0("_epsg", target_epsg, "_navd88.las"),
       basename(paths$input), ignore.case = TRUE)
)
```
Then per-CSF-set raster path at line 235: `output_path <-
update_path(paths$ground_raster_template, params)`.

### `lidar/03_evaluate_dtm.R:57` → passed into `report_dtms()`
```r
base_output <- file.path("E:/uas_scratch/lidar", site, date)
```
`report_dtms()` itself (see below) defaults `eval_dir` to
`lidar/output/<site>_<date>` — that's where the CSV/HTML actually
land, distinct from `base_output`.

### `R/report_dtms.R:50-65` (function signature — this is the canonical construction point)
```r
report_dtms <- function(
      site,
      date,
      ecp_path    = paste0(
         "X:/legacy/gdrive/saltmarsh_UAS_native/",
         "In Situ Data Collection/",
         "JoshSurveyPoints_AllSites_One_Sheet.xlsx"
      ),
      base_output = file.path("E:/uas_scratch/lidar", site, date),
      tolerances  = c(0.10, 0.20),
      ecp_types   = "EVP",
      eval_dir    = file.path("lidar/output",
                              paste0(site, "_", date)),
      output_file = paste0("dtm_eval_", site, "_", date, ".html"),
      target_epsg = 6491L,
      open        = TRUE) {
```
and the render call, `R/report_dtms.R:77-96`:
```r
   rmd <- file.path("rmd", "dtm_evaluation_report.Rmd")
   ...
   out_path <- rmarkdown::render(
      input       = rmd,
      params      = list(site        = site,
                         date        = date,
                         ecp_path    = ecp_path,
                         base_output = base_output,
                         tolerances  = tolerances,
                         ecp_types   = ecp_types,
                         eval_dir    = eval_dir,
                         target_epsg = target_epsg),
      output_file = output_file,
      output_dir  = eval_dir,
      quiet       = TRUE
   )
```
Inside the rendered Rmd, `rmd/dtm_evaluation_report.Rmd:88-90` writes
the CSV:
```r
dir.create(params$eval_dir, recursive = TRUE, showWarnings = FALSE)
csv_path <- file.path(params$eval_dir, "dtm_eval_summary.csv")
write.csv(all_summary, csv_path, row.names = FALSE)
```

### `R/report_ground_sources.R:40-53` (function signature)
```r
report_ground_sources <- function(
      sources,
      site,
      ecp_path    = paste0(
         "X:/legacy/gdrive/saltmarsh_UAS_native/",
         "In Situ Data Collection/",
         "JoshSurveyPoints_AllSites_One_Sheet.xlsx"
      ),
      output_dir  = file.path("lidar/output",
                              paste0(site, "_ground_comparison")),
      tolerances  = c(0.10, 0.20),
      ecp_types   = "EVP",
      output_file = paste0("ground_comparison_", site, ".html"),
      open        = TRUE) {
```
Render call, `R/report_ground_sources.R:62-83`:
```r
   rmd <- "rmd/ground_source_comparison.Rmd"
   ...
   dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
   out_path <- rmarkdown::render(
      input       = rmd,
      params      = list(
         site       = site,
         sources    = sources,
         ecp_path   = ecp_path,
         output_dir = output_dir,
         tolerances = tolerances,
         ecp_types  = ecp_types
      ),
      output_file = output_file,
      output_dir  = output_dir,
      quiet       = TRUE
   )
```
Inside the Rmd, `rmd/ground_source_comparison.Rmd:14, 39, 82, 85,
111`:
```yaml
output_dir: "lidar/output/[site]_ground_comparison"
```
```r
output_dir <- gsub("[site]", site, x = output_dir, fixed = TRUE)
...
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
...
csv_path <- file.path(output_dir, paste0(label, "_ecp.csv"))   # per-source ECP cache
...
csv_out <- file.path(output_dir, "ground_source_comparison.csv")
```

### `lidar/05_veg_heights.R:108-114`
```r
summer_clean_dir <- file.path("E:/uas_scratch/lidar", site,
                              summer_date_uu,
                              paste0("zzzcleaned_epsg", target_epsg))
output_dir <- file.path("E:/uas_scratch/lidar", site,
                        summer_date_uu, "zzzheights")
output_tif <- file.path(output_dir,
                        paste0("veg_dist_", raster_res, "m.tif"))
```

### `lidar/07_floor_corrected_ground.R:32, 103-106`
```r
output_dir <- file.path("lidar/output", paste0(site, "_ground_comparison"))
...
corrected_ground_tif <- file.path(summer_raster_dir,
                                  "massgis_plus_floor_summer.tif")
terra::writeRaster(massgis_plus_floor, corrected_ground_tif, overwrite = TRUE)
```
(note: `output_dir` here is for reading cached ECP CSVs and writing
`floor_bias_{spring,summer}.png`, lines 78 & 82; the corrected raster
itself goes to `summer_raster_dir` on `E:`, per the comment at lines
43-46.)

### `lidar/08_veg_height_validation.R:39, 52, 59-60, 74, 109-115, 148`
```r
output_dir  <- file.path("lidar/output", paste0(site, "_ground_comparison"))
spring_dtm_path <- file.path(output_dir, "lidar_spring_ecp.csv")
...
massgis_floor_tif <- file.path(summer_raster_dir, "massgis_plus_floor_summer.tif")
...
canopy_top_tif   <- file.path(summer_raster_dir, "canopy_top_summer.tif")
...
massgis_floor_elev <- sample_dtm(dtm = massgis_floor_tif, ecp = site_ecp,
   output_csv = file.path(output_dir, "massgis_plus_floor_summer_ecp.csv"))
canopy_elev <- sample_dtm(dtm = canopy_top_tif, ecp = site_ecp,
   output_csv = file.path(output_dir, "canopy_top_summer_ecp.csv"))
...
write.csv(veg_height_validation, file.path(output_dir, "veg_height_validation.csv"), row.names = FALSE)
```

### Rmd `output_dir`/`knit_root_dir` YAML settings — full search result
Only `rmd/ground_source_comparison.Rmd:14`
(`output_dir: "lidar/output/[site]_ground_comparison"`, a `params:`
entry, not an rmarkdown `output_dir`/`knit_root_dir` YAML field) has
anything resembling this. Neither `rmd/dtm_evaluation_report.Rmd` nor
`rmd/ground_source_comparison.Rmd` nor
`logger_recalibration/recalibration_report.Rmd` nor
`inundation_metrics/02_hydrology_regression_and_plots.Rmd` set an
actual `output: html_document: output_dir:` or `knit_root_dir:` YAML
key — the real output directory in both lidar Rmds is controlled
entirely via the `output_dir =` / `output_dir  =` argument passed to
`rmarkdown::render()` from `R/report_dtms.R:94` /
`R/report_ground_sources.R:81` (`eval_dir` / `output_dir`), shown
above. Both Rmds instead use `setwd(here::here())` in their setup
chunk (`rmd/dtm_evaluation_report.Rmd:26`,
`rmd/ground_source_comparison.Rmd:25`) to fix the working directory
to the project root so their own relative `file.path("R/", ...)`
sourcing and `params$eval_dir` / `output_dir` paths resolve.

### `inundation_metrics/01_calculate_metrics_and_combine.R:58`
```r
readr::write_rds(data, "inundation_metrics/Data/four_sites.Rds")
```
### `inundation_metrics/02_hydrology_regression_and_plots.Rmd:133`
```r
readr::write_csv(display_table, "inundation_metrics/Data/Model_Coefficients.csv")
```
Both relative to project root, no `E:`/`X:` involvement.

---

## 3. Large committed report/csv/raster-diagnostic outputs

### `lidar/06_compare_ground_sources.R`
- Invocation: not `rmarkdown::render()` itself — it sources `R/` then
  calls `report_ground_sources(sources = sources, site = site)`
  three times (rr, oth, wel; lines 61, 86, 105), which internally
  calls `rmarkdown::render()` on `rmd/ground_source_comparison.Rmd`
  (see item 2 above).
- Current output landing spot:
  `lidar/output/<site>_ground_comparison/ground_comparison_<site>.html`
  (+ `ground_source_comparison.csv`, per-source `<label>_ecp.csv`
  caches).
- `lidar/06_compare_ground_sources.html` (top-level, 2,664,189 bytes,
  last modified 2025-05-21) — this file is **not tracked by git**
  (`git ls-files lidar/06_compare_ground_sources.html` returns
  nothing) and **is matched by `.gitignore:53` (`/lidar/*.html`)**
  per `git check-ignore -v`. It's a leftover artifact from before the
  script was refactored to render into
  `rmd/ground_source_comparison.Rmd` +
  `lidar/output/<site>_ground_comparison/` (per git log: `5f9fafd
  Convert ground source comparison to Rmd report`, `ea9163a Refactor
  ground source comparison to function + parameterized Rmd`).

### `lidar/output/<site>_ground_comparison/` (rr, oth, wel, plus legacy `rr_ground_comparison_epsg26919`)
These ARE currently tracked/committed in git. `git ls-files
lidar/output` returns 54 tracked files, e.g.:
```
lidar/output/oth_ground_comparison/ground_comparison_oth.html
lidar/output/rr_ground_comparison_epsg26919/ground_comparison_rr.html   (2,664,176 bytes on disk)
lidar/output/wel_ground_comparison/ground_comparison_wel.html
```
`git status --short` (at research time) showed
`lidar/output/oth_ground_comparison/ground_comparison_oth.html` and
`lidar/output/wel_ground_comparison/ground_comparison_wel.html` as
modified (` M`), and a whole new untracked
`lidar/output/rr_ground_comparison/` directory (rendered under the
new un-suffixed naming, target_epsg 6491, vs. the older tracked
`_epsg26919` directory) plus untracked
`lidar/output/rr_2022_05_14/` and `lidar/output/rr_2022_08_10/` (the
`03_evaluate_dtm.R` outputs). Individual on-disk sizes for the
ground-comparison / dtm-eval HTMLs run ~2.0-2.7 MB each; `du -sh
lidar/output` = **21M** total for the currently tracked+untracked
content.

### `lidar/output/rr_2022_*/` (dtm_eval reports) — produced by `03_evaluate_dtm.R`
- Invocation: `lidar/03_evaluate_dtm.R` sources `R/`, builds `runs <-
  data.frame(site = "rr", date = c("2022_08_10", "2022_05_14"))`, and
  for each row calls `report_dtms(site=, date=, ecp_path=,
  base_output=, tolerances=, target_epsg=)` (lines 59-66), which is
  the same `rmarkdown::render()`-based function described in item 2.
- Landing spot:
  `lidar/output/<site>_<date>/dtm_eval_<site>_<date>.html` +
  `dtm_eval_summary.csv`. At research time on disk / tracked:
  `lidar/output/rr_2022_05_14_epsg26919/` (tracked, HTML 2,268,271
  bytes) and `lidar/output/rr_2022_08_10_epsg26919/` (tracked — also
  contains per-DTM diagnostic `.png`s and a stray `Thumbs.db`, HTML
  2,266,875 bytes); the newer un-suffixed
  `lidar/output/rr_2022_05_14/` and `lidar/output/rr_2022_08_10/`
  directories existed on disk but were untracked (`??` in `git
  status`).

### `03_evaluate_dtm.R` / `R/report_dtms.R`
Covered above — `report_dtms()` is the sole path-construction+render
point; `03_evaluate_dtm.R` is purely a driver that loops site/date
combos and calls it.

### `R/report_ground_sources.R`
Covered above — sole path-construction+render point for the
ground-source comparison; called by `lidar/06_compare_ground_sources.R`.

### Other adjacent gitignored/leftover HTML artifacts found during this check
`rmd/dtm_evaluation_report.html` (2,266,802 bytes),
`rmd/ground_source_comparison.html` (2,362,756 bytes),
`rmd/oth_ground_source_comparison.html` (2,362,756 bytes) — all
present on disk under `rmd/`, all untracked and matched by
`.gitignore:54` (`/rmd/*.html`) per `git check-ignore -v`. These
appear to be leftovers from rendering the Rmds directly in place
(their default `output_dir` is now routed to `lidar/output/...` via
the `report_*()` wrapper functions instead).

`inundation_metrics/02_hydrology_regression_and_plots.html` (716,571
bytes) **is tracked** (`git ls-files` confirms it), and is not
matched by the `/inundation_metrics/*.html` gitignore rule at that
path (git tracks it regardless — once a file is tracked, a
later-added gitignore rule doesn't untrack it without `git rm
--cached`).

---

## 4. `.gitignore` and CLAUDE.md "Data conventions"

### Relevant `.gitignore` (root) lines, verbatim (at research time)
```
# Calibrated hydrology files
inundation_metrics/Data/*/Calibrated*

# Raw pipeline run logs (large, narrated in dev/worklog.md instead)
/dev/run_logs/
```
```
# Rendered Rmarkdown
/inundation_metrics/*.html
/lidar/*.html
/rmd/*.html
/logger_recalibration/*.html
```
`grep -n -i "lidar" .gitignore` (root file) returns **only**
`/lidar/*.html` — **`lidar/output/` is not mentioned anywhere in the
root `.gitignore`**, so nothing under it (recursively) is excluded by
that file. The root `.gitignore` also has no `lidar/data` entry.

There is a **separate, nested** `lidar/data/.gitignore` (not the root
one), verbatim:
```
*.lax
*.las
*.xls
*.xlsx
```
Contents of `lidar/data/` at research time: `.gitignore`, `Readme.md`,
`RedRiver_11May2022.csv`, `paths.csv` — all 4 tracked in git. Note the
nested gitignore pattern set (`*.lax/*.las/*.xls/*.xlsx`) doesn't
literally implement "everything except `paths.csv` and
`RedRiver_11May2022.csv`" — it's a filetype-based ignore; the
described "everything else gitignored" behavior in `CLAUDE.md` holds
in practice only because the untracked raw data that lives in that
directory locally (LAS/LAX point clouds, XLS/XLSX ECP sheets) happens
to match those four extensions.

### `CLAUDE.md` "Data conventions" section, verbatim (lines 48-54)
```
## Data conventions

- Site codes are 3-letter lowercase for lidar (`rr`, `nor`, `bar`, `wel`) and 3-letter uppercase for hydrology (`RED`, `OTH`, `WES`, `WEL`). They are not always the same code for the same site — `RED` ↔ `rr` (Red River). The canonical mapping table is `inundation_metrics/Data/sites.txt`.
- Inundation-metrics data (`inundation_metrics/Data/`) is committed, including per-site deployment CSVs — only the raw `Calibrated Data/*cal.xlsx` files stay gitignored.
- `lidar/data/` is also gitignored except for `paths.csv` and `RedRiver_11May2022.csv` (a small GCP file).
- Elevation control points live in `X:/legacy/gdrive/saltmarsh_UAS_native/In Situ Data Collection/JoshSurveyPoints_AllSites_Meta_Datapoints.xlsx` (use rows with `type = "training"`).
```
`CLAUDE.md` does **not** mention `lidar/output/` anywhere in the
"Data conventions" section (or elsewhere in the file) — no
gitignore/commit policy is documented there for it, consistent with
the root `.gitignore` also being silent on it.
