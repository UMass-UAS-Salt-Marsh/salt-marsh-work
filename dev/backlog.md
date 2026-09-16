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