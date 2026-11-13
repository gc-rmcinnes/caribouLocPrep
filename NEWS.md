# caribouLocPrep (development version)

- `filter_clusters()` no longer drops the first location of each animal, or the first location after a gap of more than `window_hours`: one location on its own is not a cluster. A cluster is now a run of locations that stays within `radius_m` for at least `window_hours`, or, with the new `min_points`, holds at least that many locations (for tracks with few locations per day).
- `filter_fast_roundtrips()` now uses `amt::flag_roundtrips()`, as hsaCEtool does (arguments `delta` and `epsilon`, defaults 100000 and 10, replace `speed_threshold_m_per_hr` and `ratio_threshold`). It no longer drops an animal's first and last locations when the step next to them is fast.
- Tests (`tests/testthat`, run with `Rscript tests/run-tests.R`) on small made-up locations, run on GitHub Actions by `.github/workflows/test-module.yaml`.

- The `studyArea_juris_<.studyAreaName>.rds` file uploaded to Drive now holds each jurisdiction's area packed with `terra::wrap()`; read it with `lapply(readRDS(file), terra::unwrap)`. Files uploaded before this change cannot be read back in a new R session and need to be made again.

# caribouLocPrep 0.0.1 (13 March 2025)

- initial module version
