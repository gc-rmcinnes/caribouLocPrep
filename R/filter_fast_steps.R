filter_fast_steps <- function(dt, speed_threshold_m_per_hr) {

  before <- nrow(dt)

  dt <- copy(dt)
  setorder(dt, id, datetime)

  dt[, `:=`(
    dx = x - shift(x),
    dy = y - shift(y),
    dt_hr = as.numeric(difftime(datetime, shift(datetime), units="hours"))
  ), by=id]

  dt[, dist_m := sqrt(dx^2 + dy^2)]
  dt[, speed_mph := dist_m / dt_hr]

  # Keep only acceptable speeds (plus first rows of each id)
  dt <- dt[is.na(speed_mph) | speed_mph <= speed_threshold_m_per_hr]

  after <- nrow(dt)
  removed <- before - after

  message(sprintf("[speed filter] Removed %s points (%.2f%%)",
                  removed, 100 * removed / before))

  dt[, c("dx","dy","dt_hr","dist_m","speed_mph") := NULL]

  return(dt)
}
