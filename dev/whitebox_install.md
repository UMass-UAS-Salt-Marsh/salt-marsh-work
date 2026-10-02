# WhiteboxTools installation log

A record of when and how the `whitebox` R package and the
WhiteboxTools binary were installed on this machine, for
`fill_sinks()` (`R/fill_sinks.R`).
Not a general install-anywhere guide -- just what was done, when, and
from where, in case it needs auditing or repeating on another
machine.

## 2026-09-16

- **R package**: `whitebox` 2.4.3, from CRAN
  (<https://cloud.r-project.org>), via
  `install.packages("whitebox", repos = "https://cloud.r-project.org")`.
- **WhiteboxTools binary**: v2.4.0, fetched via
  `whitebox::install_whitebox()`, which downloaded
  <https://www.whiteboxgeo.com/WBT_Windows/WhiteboxTools_win_amd64.zip>
  and extracted it to
  `C:/Users/plunkett/AppData/Roaming/R/data/R/whitebox/WBT/whitebox_tools.exe`.
- Verified with `whitebox::check_whitebox_binary()` (returned `TRUE`)
  and `whitebox::wbt_version()`
  (`"WhiteboxTools v2.4.0 (c) Dr. John Lindsay 2017-2023"`).
- No system environment variables were changed as part of this
  install.
  `whitebox::wbt_init()` records the exe path itself (via
  `wbt_default_path()` / an internal package option), so R doesn't
  need `PATH` updated for `fill_sinks()` or anything else that calls
  `whitebox::wbt_*()` functions to work.

## Optional: add to `PATH` for manual/CLI use

Nothing in this repo needs this -- it's only useful if you want to
run `whitebox_tools.exe` directly from a shell, outside R.
If you want that, add this directory to your user `PATH` yourself:

```
C:\Users\plunkett\AppData\Roaming\R\data\R\whitebox\WBT
```
