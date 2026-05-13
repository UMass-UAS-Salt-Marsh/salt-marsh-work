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
