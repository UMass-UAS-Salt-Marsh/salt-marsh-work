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
   
   
   





