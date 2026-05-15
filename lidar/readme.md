# Saltmarshes

The goal is to process lidar point clouds into raster files for use with
vegetation modeling.

The Lidar data is all on X drive.

## Dream output

Raster corresponding roughly to final cells

0.25, 0.5, or 1 meter cells. Start with 0.5 meter cells.

Floor (DEM)
Veg Height Raster
Distribution of vegetation heights from point cloud
 * Fixed height bins with percentiles of hits in each bin. 5 cm bins up to 1 meter
after 1 meter switch to 20 cm up to 3m. Multiband geoTIFF.


## Priorities
* North River would be best (lidar 2024, veg from 2023 and 2025)
* Red River second (lidar 2022 -- veg from 2023)
* Peggoty second if dates are resolved (lidar 2024, veg 2025)
* Wellfleet other contender (4th)

## Ground control points

* Height GCP:Additional elevation control points that josh collected for
Old Town Hill, Wellfleet, and Red River.
`X:\legacy\gdrive\saltmarsh_UAS_native\In Situ Data Collection\JoshSurveyPoints_AllSites_Meta_Datapoints.xlsx`
* Vegetation locations - good to use

* Caution with water loggers as the location is the location of the logger not the floor, offset is standard so can subtract to get floor. (Subtract approximately 10 mm - need to get measurement from device). This would give us 40 to 60 additional points.
* Don't use lidar / ortho GCP's as they are above marsh on a plastic platform

### Files
#### Elevation control points
"X:\legacy\gdrive\saltmarsh_UAS_native\In Situ Data Collection\JoshSurveyPoints_AllSites_Meta_Datapoints.xlsx". -- Use type = "training"
"X:\legacy\gdrive\saltmarsh_UAS_native\In Situ Data Collection\JoshSurveyPoints_AllSites_Meta_ExtractValues.xlsx"

####LAS files - two schemes
* 2022 -- SCan date in folder path, date in file is processing date example:
"X:\legacy\gdrive\saltmarsh_UAS\UAS Data Collection\Red River\2022\LiDAR\10Aug2022_Low\RESEPI-5FFC59-2022-08-10-20-26-50\clouds\ppk_07Nov2022_cloud_1.las"
* 2024 - Not in legacy projects/uas/sites/[site]/lidar_point_cloud/  example path:
"X:\projects\uas\sites\bar\lidar_point_cloud\2024_06_02_low_hesaixt32\RESEPI-A84717-2024-06-02-16-57-55\clouds\BAR_PointCloud_02Jun2024.las"



## Aproach

We need to use the LAScatalog feature of lidR to work in tiles.

Step 1:  clean_and_tile()
   Filter to last hit
   Remove outliers
   Remove Buffer
   Write to new set of tiles
   
Step 2: 
   Run on output from step 1
   Find and export ground for a number of parameter sets and output as raster
   
Step 3 
   Compare to Elev. Control Points and evaluate performance
   
Step 4
   Use chosen models to generate raster with pcts in each bin.



## CRS, datum, and frame shift considerations

The pipeline reprojects every source `.las`
from the RESEPI delivery's native WGS 84 reference frame
to the NAD 83 / NAVD 88 frame used by the field-collected ECPs
and the broader saltmarsh project.
What sounds like a one-line coordinate transformation
is actually three intertwined geodetic shifts,
each of which different tools handle differently.
This section documents them so future maintainers
know what to expect.

### Source CRS

RESEPI / PPK clouds arrive tagged in the LAS header GeoTIFF keys as:

- Horizontal: **EPSG:32619** — WGS 84 / UTM zone 19N
- Vertical: **EPSG:5030** — WGS 84 ellipsoidal heights

"WGS 84" here means the realization shipped with the GNSS-corrected
post-processing
(approximately WGS 84(G2139),
the realization in use since 2021).
Z values are ellipsoidal heights,
which for Cape Cod sit roughly 28 m above the local geoid.

### Target CRS

Field survey points (ECPs) and downstream deliverables use:

- Horizontal: **EPSG:26919** (legacy NAD 83) or
  **EPSG:6348** (NAD 83(2011)) — both NAD 83 / UTM zone 19N
- Vertical: **EPSG:5703** — NAVD 88 orthometric heights

NAVD 88 heights are what surveyors actually measure
("how far above mean sea level");
they're computed from ellipsoidal heights by subtracting a geoid
undulation,
which for Red River is approximately −28 m
(the geoid sits ~28 m *below* the WGS 84 ellipsoid in that area,
so NAVD 88 height ≈ ellipsoid height + 28 m).

### The three shifts that need to happen

1. **Horizontal projection math.**
   Both source and target use UTM zone 19N,
   so the in-zone projection math is essentially identity.
   This part is trivial.
2. **Horizontal datum / frame shift.**
   WGS 84 and NAD 83 are different *reference frames*,
   not just different ellipsoids.
   They were tied at epoch 1997 but have since drifted apart
   due to plate-tectonic motion of the North American plate
   (~25 mm/yr).
   For a 2022 lidar acquisition compared against NAD 83(2011)
   coordinates,
   the frame divergence is roughly 0.5–1 m in 3D in eastern CONUS.
   This is the "frame error" the project notes repeatedly.
3. **Ellipsoidal → orthometric vertical.**
   Even after a frame shift,
   ellipsoidal heights are not directly comparable to surveyed
   NAVD 88 heights.
   A geoid model is required:
   for Cape Cod that's **GEOID12B**,
   distributed as the `.gtx` grid file `g2012bu0.gtx`
   (the CONUS lower-48 tile,
   covering 24–58° N, 230–300° E).

### How each reprojection method handles them

The `reproject_las()` dispatcher offers two methods,
which handle these three shifts differently:

| Shift | LAStools method | PDAL method |
|---|---|---|
| (1) UTM projection math | `las2las -proj_epsg` (via PROJ) | `filters.reprojection` (via PROJ) |
| (2) Horizontal frame | `las2las -proj_epsg` (via PROJ; needs realization-specific target EPSG to actually apply) | `filters.reprojection` (same caveat) |
| (3) Vertical geoid | `lasvdatum -vgrid <gtx>` (paid license required) | `filters.reprojection` with `+geoidgrids=` appended to the PROJ string |

Neither method explicitly handles a WGS 84 → NAD 83 *ellipsoidal*
Z conversion;
both rely on the geoid grid to handle vertical transparently.
The vertical shift is correct in both methods so long as the geoid
model is correct.
Empirically,
GEOID12B applied at Red River produces a ~+28 m shift,
matching expectation.

The horizontal frame shift,
however,
depends on what realization the *target* EPSG code names:

- **EPSG:26919** (the code currently used in `lidar/02.R`)
  is legacy NAD 83 with no specified realization.
  PROJ's default WGS 84 → legacy-NAD 83 pipeline is effectively
  a near-identity transformation,
  so the horizontal frame shift is **not** applied.
- **EPSG:6348** (NAD 83(2011) / UTM 19N) is realization-specific.
  PROJ would then pick an ITRF → NAD 83(2011) Helmert + grid-based
  transformation pipeline,
  applying the ~0.5–1 m frame shift.

The first PDAL test (2026-05-15) confirmed this empirically:
target was `EPSG:26919`,
output XY differed from source by < 5 mm
(precision-level rounding,
not a real shift);
vertical shifted ~28 m as expected.

### Practical implications

- **CSF parameter tuning (Phase 1).**
  `evaluate_dtm()` fits a residual offset that absorbs any
  constant systematic Z bias.
  A horizontal frame error of < 1 m is invisible to CSF,
  which is a local point-cloud algorithm
  not a coordinate matcher.
  So for parameter ranking,
  the current `EPSG:26919` target is adequate.
- **Absolute NAVD 88 deliverables.**
  Anything that ships a DTM for downstream models expecting
  "real" NAD 83(2011) + NAVD 88 coordinates needs the proper
  frame shift applied.
  Switch the target to **EPSG:6348** and re-verify the output
  XY actually moves
  (and the WKT / EPSG tags update correctly).
- **LAStools licensing.**
  The LAStools method needs a paid `lasvdatum` license to
  complete Step 2.
  Without one,
  use the PDAL method
  (the default for `reproject_las()`).



## LAS Reprojection System Requirements

There are two pathways for .las reprojection built into `reproject_las()`

1. **LASTOOLS** this is the method that has historically been used on the UAS
project. It uses `las2las` followed by `lasvdatum` to reproject and then 
add the vertical datum.

**Requires a paid LAStools license to run end-to-end.** `las2las` is part
of LAStools' free / open-source set, but `lasvdatum` is commercial-only
and exits with `ERROR:license failure` on a free install. On this machine
Step 1 (`las2las`) runs cleanly with `-proj_epsg`, but Step 2
(`lasvdatum`) cannot complete without a license. Use the PDAL method
instead unless a LAStools license becomes available.

lastools was installed following these instructions: 
`X:\system\software\LAStools_README.txt`
The binaries live at `C:\tools\LAStools\bin\` and were added to the system
PATH, making them available directly via the command line. The R code
invokes them by full path from that location regardless (overridable via
the `las2las` / `lasvdatum` arguments to `reproject_las()`).


2. **pdal** (Point Data Abstraction Library) Does this in one step without
introducing the errors in the LASTOOLS approach.  

Pdal was installed via OSGeo4W:
"X:/system/software/osgeo4w-setup.exe"

The installation was problematic but pdal does work. Nothing was added to
the system PATH; the PDAL tools are invoked from R using their full paths.








