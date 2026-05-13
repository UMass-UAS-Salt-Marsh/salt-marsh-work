2026-05-13
==========

*   Lidar Phase 0 cleanup:
    extracted `evaluate_dtm()`, `visualize_dtm()`, `update_path()`,
    `clean_column_names()`, `clean_dates()`, and three plot helpers
    out of `lidar/02.R` into one-function-per-file modules under `R/`.
    Finished `evaluate_dtm()` so it returns per-point residuals plus
    summary stats broken out by ECP `type`.

*   Fixed two silent bugs in `R/rasterize_ground.R`:
    `alogrithm` typo meant lidR was falling back to its default
    interpolator instead of the intended `knnidw`,
    and a `terra::crop(..., terra::ext(chunk))` line referenced an
    undefined `chunk` and discarded its result.
    **Existing DTMs were produced with the default `tin()` algorithm
    and will need re-rasterization before Phase 1 evaluation.**

*   Added `.lintr` configured for the project's 3-space indent and
    with `object_usage_linter` disabled
    (false-positive-prone in NSE-heavy tidyverse code without a `NAMESPACE`).

*   Initial project documentation:
    added `CLAUDE.md` describing the repo's two pipelines (hydrology and lidar) and conventions,
    and a `dev/` directory (`work_plan.md`, `worklog.md`) for active plans and detailed development history.

*   Adopted the [tidyverse style guide](https://style.tidyverse.org/) with
    project-specific deviations (3-space indent, base R `|>` pipe, roxygen comments retained pre-package).
    Added BirdFlowR-inspired conventions:
    lint new code with `lintr::lint()`,
    one function per file with matching filename,
    Markdown syntax inside roxygen comments,
    semantic line breaks in markdown.

*   Started `NEWS.md` (this file) as the user-facing project changelog.

*   Drafted `dev/work_plan.md` for the lidar pipeline:
    DTM evaluation against elevation control points (Phase 1)
    and a multi-band percent-of-returns vegetation-strata raster (Phase 2).

*   Lidar code restructure (commits `809ea21`, `debe9f1`):
    `lidar/02.R` now runs `clean_and_tile()` and `rasterize_ground()`
    over the full point cloud in tiles for a single site;
    `visualize_dtm()` split out from `evaluate_dtm()` (both still drafted inline in `lidar/02.R`,
    extraction into `R/` pending in Phase 0 of the work plan).
