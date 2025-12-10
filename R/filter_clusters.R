filter_clusters <- function(dt, window_hours = 48, radius_m = 50) {

  before <- nrow(dt)

  dt <- copy(dt)
  setorder(dt, id, datetime)

  dt[, tsec := as.numeric(datetime)]
  window_sec <- window_hours * 3600

  dt[, cluster := FALSE]

  # Process per animal
  for (animal in unique(dt$id)) {

    d <- dt[id == animal]
    n <- nrow(d)
    if (n < 3) next

    left <- 1L

    # these track bounding box
    min_x <- d$x
    max_x <- d$x
    min_y <- d$y
    max_y <- d$y

    for (right in seq_len(n)) {

      # Expand right pointer, shrink left pointer as needed
      while (d$tsec[right] - d$tsec[left] > window_sec) {
        left <- left + 1L
      }

      # Current window [left, right]
      xw <- d$x[left:right]
      yw <- d$y[left:right]

      # bbox
      dx <- max(xw) - min(xw)
      dy <- max(yw) - min(yw)
      diag <- sqrt(dx^2 + dy^2)

      if (diag < radius_m) {
        # mark whole window as cluster points
        dt[id == animal &
             tsec >= d$tsec[left] & tsec <= d$tsec[right],
           cluster := TRUE]
      }
    }
  }

  # remove cluster points
  dt <- dt[cluster == FALSE]

  after <- nrow(dt)
  removed <- before - after

  message(sprintf("[cluster filter] Removed %s points (%.2f%%)",
                  removed, 100 * removed / before))

  dt[, c("cluster", "tsec") := NULL]

  return(dt)
}
