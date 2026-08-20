This repository contains my initial exploratory work for the Saltmarsh project.
As code matures I will likely move it into a formal R package.

See [`CRS.md`](CRS.md) for the project's coordinate reference system,
datum, and geoid standard.

The structure is as follows:
* `R/`  function code.  Source the `.R` files here to define functions.
*  `inundation_metrics`  Calculate hydrology inundation metrics and perform regression against elevation at
  four sites. Required data is included in git repository.
* `lidar` Process lidar point clouds into elevation and height of returns.
* `logger_recalibration` - Recalibrate logger data by assuming a uniform water elevation at high tides