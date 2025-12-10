filter_fast_roundtrips <- function(dt, speed_threshold_m_per_hr = 12000, ratio_threshold = 0.25) {

  before <- nrow(dt)
  dt <- copy(dt)
  setorder(dt, id, datetime)

  # previous step metrics
  dt[, prev_dx  := x - shift(x),       by=id ]
  dt[, prev_dy  := y - shift(y),       by=id ]
  dt[, prev_dtH := as.numeric(difftime(datetime, shift(datetime), units="hours")), by=id ]
  dt[, prev_dist := sqrt(prev_dx^2 + prev_dy^2)]
  dt[, prev_speed := prev_dist / prev_dtH]

  # next step metrics
  dt[, next_dx  := shift(x, type="lead") - x, by=id ]
  dt[, next_dy  := shift(y, type="lead") - y, by=id ]
  dt[, next_dtH := as.numeric(difftime(shift(datetime, type="lead"), datetime, units="hours")), by=id ]
  dt[, next_dist := sqrt(next_dx^2 + next_dy^2)]
  dt[, next_speed := next_dist / next_dtH]

  # optional: round-trip midpoint collapses back near original
  dt[, full_dx := shift(x, type="lead") - shift(x), by=id ]
  dt[, full_dy := shift(y, type="lead") - shift(y), by=id ]
  dt[, full_dist := sqrt(full_dx^2 + full_dy^2)]

  # ROUND TRIP CONDITION:
  # both legs are fast AND full distance is much smaller
  dt[, roundtrip :=
       prev_speed > speed_threshold_m_per_hr &
       next_speed > speed_threshold_m_per_hr &
       full_dist < (prev_dist + next_dist) * ratio_threshold
  ]

  # remove flagged points
  dt <- dt[roundtrip == FALSE]

  after <- nrow(dt)
  removed <- before - after

  message(sprintf("[roundtrip filter] Removed %s points (%.2f%%)",
                  removed, 100 * removed/before))

  dt[, c("prev_dx","prev_dy","prev_dtH","prev_dist","prev_speed",
         "next_dx","next_dy","next_dtH","next_dist","next_speed",
         "full_dx","full_dy","full_dist","roundtrip") := NULL]

  return(dt)
}
