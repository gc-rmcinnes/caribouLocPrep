## Small made-up caribou tracks for the tests. No real locations: the coordinates are invented
## metres in EPSG:3978, chosen only to sit inside the box downloadDataAndHarmonize() keeps.

t0 <- as.POSIXct("2020-01-01", tz = "UTC")

## One animal's track: a point every `hours` hours, `step_m` metres east of the one before
make_track <- function(id = "a", n = 6, hours = 4, step_m = 1000, x0 = 100000, y0 = 200000) {
  data.table(
    id = id,
    datetime = t0 + 3600 * hours * (seq_len(n) - 1),
    x = x0 + step_m * (seq_len(n) - 1),
    y = y0
  )
}
