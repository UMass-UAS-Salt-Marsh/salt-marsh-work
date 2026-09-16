# Backlog

Issues and tasks that are known/worth doing but aren't part of the
currently active plan in `workplan.md`. Not scheduled, not
prioritized in any particular order — just a place to park things so
they aren't forgotten or re-discovered from scratch. Move an item
into `workplan.md` when it becomes active work, then delete it from
here (the worklog is the durable record of what happened).

- **Extend the memory pre-flight check to other memory-bound
  functions.** `rasterize_ground()` got a memory check on 2026-09-15
  (see `worklog.md`), keyed by a `process` row in
  `worker_memory_reference`. `clean_and_tile()` and
  `rasterize_veg_heights()` are the other two `future::multisession`
  / `catalog_map()`-based functions most likely to hit the same
  OOM failure mode. Needs profiling first — each needs its own
  observed peak-memory calibration point (`chunk_size`,
  `chunk_buffer`, `density`, `memory_mb`) added as a new
  `worker_memory_reference` row before `check_worker_memory()` can be
  wired in for that process.

- **check output pixel alignment**  make sure it snaps to origin

- **Burn flow lines under roads before sink-filling.** `fill_sinks()`
  (`R/fill_sinks.R`, added 2026-09-16) fills whatever raster it's
  given, with no assumption about roads/culverts/bridges. Real DTMs
  will have places where a road or driveway crosses a channel and
  reads as a dam, producing an unrealistic upstream fill. Needs a
  preprocessing function that lowers the DTM along mapped
  culvert/bridge flow lines (a vector layer isn't identified yet)
  before it reaches `fill_sinks()`.
  `whitebox::wbt_breach_depressions_least_cost()` is worth evaluating
  as an alternative/complement to hand-burning, since it's built for
  exactly this kind of obstacle-breaching.