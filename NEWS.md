# caribouLocPrep (development version)

- The `studyArea_juris_<.studyAreaName>.rds` file uploaded to Drive now holds each jurisdiction's area packed with `terra::wrap()`; read it with `lapply(readRDS(file), terra::unwrap)`. Files uploaded before this change cannot be read back in a new R session and need to be made again.

# caribouLocPrep 0.0.1 (13 March 2025)

- initial module version
