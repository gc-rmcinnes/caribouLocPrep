## Drop fast out-and-back spikes: a point whose steps in and out are both fast and that comes back
## near where it started. Uses amt::flag_roundtrips(), as hsaCEtool's `roundtrips` step does; delta
## and epsilon are amt's (squared displacement rate, m^2/s). The first and last points of each animal
## have only one step, so they are never spikes and are kept.
filter_fast_roundtrips <- function(dt, delta = 100000, epsilon = 10) {

  before <- nrow(dt)
  dt <- copy(dt)
  setorder(dt, id, datetime)

  dt[, roundtrip := amt::flag_roundtrips(
    amt::make_track(.SD, x, y, datetime), delta = delta, epsilon = epsilon
  )$fast_roundtrip_, by = id]

  # remove flagged points
  dt <- dt[is.na(roundtrip) | !roundtrip]

  after <- nrow(dt)
  removed <- before - after

  message(sprintf("[roundtrip filter] Removed %s points (%.2f%%)",
                  removed, 100 * removed/before))

  dt[, roundtrip := NULL]

  return(dt)
}
