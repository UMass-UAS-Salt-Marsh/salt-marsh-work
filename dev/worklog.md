# Worklog

Running log of changes made in this repo: code edits, doc edits, config
tweaks, exploratory findings, memory/settings updates, build fiddling.
More detailed than any user-facing changelog — include enough context
that a reader can reconstruct what happened without diffing.

Newest entries on top. Group by `## YYYY-MM-DD — branch <name>`
headers, then `### <short scope>` sub-headers under them. If today's
date and current branch already have a heading, append a new
sub-heading rather than starting a new date block.

Older entries get moved to `worklog-archive.md` once this file gets
long (~500 lines) or the oldest entry is more than ~30 days old. Move
whole date blocks, never split one. Read this file first when recalling
history; consult the archive only if the answer isn't here.

---

## 2026-05-13 — branch main

### Adopt BirdFlowR-inspired conventions + start NEWS.md

After reviewing BirdFlowR's `.github/CONTRIBUTING.md`, folded four of
its conventions into the `CLAUDE.md` Style section:

- **Lint new code** with `lintr::lint("R/path/to/file.R")` before
  committing; do not lint otherwise-unchanged files.
- **One function per file** in `R/`, filename matches function name.
  Extract reusable helpers out of workflow scripts into their own
  files.
- **Markdown syntax in roxygen comments** — backticks, `[text](url)`,
  etc. — so when the eventual package migration enables
  `Roxygen: list(markdown = TRUE)` the existing comments render
  correctly with no rewrites.
- **Semantic line breaks** in markdown (`CLAUDE.md`, `NEWS.md`,
  `dev/*.md`, READMEs) — one line per sentence or clause. Apply on
  new and edited content; reflow existing prose opportunistically,
  not retroactively.

Also restructured the Style section into subsections ("Coding style",
"File organization", "Linting", "Markdown", "NEWS.md", "Branching")
and rewrote the existing prose with semantic line breaks.

Branching policy: work on `main` for now; revisit if collaboration
grows. No issue-first requirement for proposed changes.

Created top-level `NEWS.md` as the user-facing changelog. Format
matches BirdFlowR (setext `===` date headings, asterisk bullets with
4-space hanging indent, backticks around function names), but uses
`YYYY-MM-DD` headings instead of version numbers since this repo
isn't versioned. First entry covers today's project-setup work and
the recent `lidar/02.R` restructuring commits.

Relationship to `dev/worklog.md`: NEWS is terser (highlights only);
worklog stays as the file-by-file dev record.

### Adopt tidyverse style guide (with project deviations)

Expanded the `## Style` section in `CLAUDE.md` to point at the
[tidyverse style guide](https://style.tidyverse.org/) as the project's
baseline. Documented three project-specific deviations that win when
they conflict with tidyverse:

- 3-space indent (per `salt-marsh-work.Rproj` and existing `R/` files,
  not the tidyverse default of 2).
- Base R `|>` pipe, not magrittr `%>%`.
- Roxygen-style function docs preserved even though the repo isn't a
  package, to keep the eventual package migration cheap.

Also enumerated the most load-bearing tidyverse rules inline (naming,
assignment, strings, spacing, line length, comments, function
structure, `TRUE`/`FALSE` over `T`/`F`, etc.) so they're visible
without leaving CLAUDE.md. No existing files were reformatted — new
code will adopt the style, and old code gets reformatted when
otherwise edited.

### Draft lidar work plan

Wrote `dev/work_plan.md` covering the two lidar goals: (1) DTM
estimation validated against ECPs and (2) a multi-band
percent-of-returns vegetation-strata raster. Phase 0 = small
cleanups + helper extraction from `lidar/02.R` into `R/`; Phase 1 =
finish `evaluate_dtm()` and add `lidar/03_evaluate_dtm.R`; Phase 2 =
add `R/rasterize_strata.R` and `lidar/04_vegetation_strata.R`;
Phase 3 = roll out to other sites with ECP coverage.

Locked-in decisions: stay on `rr` (no ECPs for `nor`); keep the 4
hand-picked CSF combos for first round; skip logger floor points; no
date matching; strata output is percent of returns; no field veg
validation (downstream model is pseudo-validation); DTM stats reported
overall + by ECP `type`.

### Add CLAUDE.md

Wrote `CLAUDE.md` at the repo root documenting the repository's nature
(exploratory R, not a package), the two pipelines (hydrology and lidar)
and their key driver scripts and shared functions, data conventions
(including the site-code casing discrepancy: `RED` ↔ `rr`), and the
expected ad-hoc workflow (source `R/*.R`, run numbered scripts from
project root). Used `init` skill instructions; no other code touched.

### Set up dev/ convention

Opted this repo into the `dev/` worklog convention per the user's
global instructions. Created:

- `dev/work_plan.md` — placeholder header, no active plan.
- `dev/worklog.md` — this file.

No `.Rbuildignore` / `MANIFEST.in` / equivalent exclusion was needed —
this repo is not a package and has no published-artifact build step,
so `dev/` is simply tracked in git alongside the rest of the source.
