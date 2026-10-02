This repository contains my initial exploratory work for the Saltmarsh project.
As code matures I will likely move it into a formal R package.

See [`CRS.md`](CRS.md) for the project's coordinate reference system,
datum, and geoid standard.

## Dependencies

Most dependencies are on CRAN and installed the usual way. One
exception: the lidar pipeline requires
[`pathtools`](https://github.com/ethanplunkett/pathtools), which is
not on CRAN and must be installed from GitHub:

```r
remotes::install_github("ethanplunkett/pathtools")
```

Sink-filling (`R/fill_sinks.R`) uses the
[`whitebox`](https://cran.r-project.org/package=whitebox) package,
which is on CRAN but also needs a one-time binary fetch:

```r
install.packages("whitebox")
whitebox::install_whitebox()
```

See [`dev/whitebox_install.md`](dev/whitebox_install.md) for a log of
this install.

The structure is as follows:
* `R/`  function code.  Source the `.R` files here to define functions.
*  `inundation_metrics`  Calculate hydrology inundation metrics and perform regression against elevation at
  four sites. Required data is included in git repository.
* `lidar` Process lidar point clouds into elevation and height of returns.
  See [`lidar/ARCHITECTURE.md`](lidar/ARCHITECTURE.md) for how the
  pipeline works.
* `logger_recalibration` - Recalibrate logger data by assuming a uniform water elevation at high tides