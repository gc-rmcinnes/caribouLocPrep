## Drop clusters: runs of consecutive points of one animal that all fit in a box whose diagonal is
## less than radius_m (e.g. a dropped collar or a dead animal). A run counts as a cluster when it
## lasts at least window_hours, or, if min_points is given, when it holds at least min_points points;
## either rule is enough. Use min_points for tracks with few fixes per day, and window_hours = NA to
## use min_points alone. A single point is never a cluster.
filter_clusters <- function(dt, window_hours = 48, radius_m = 50, min_points = NA) {

  before <- nrow(dt)

  dt <- copy(dt)
  setorder(dt, id, datetime)

  dt[, cluster := flag_clusters(as.numeric(datetime), x, y, radius_m = radius_m,
                                min_sec = window_hours * 3600, min_points = min_points),
     by = id]

  # remove cluster points
  dt <- dt[cluster == FALSE]

  after <- nrow(dt)
  removed <- before - after

  message(sprintf("[cluster filter] Removed %s points (%.2f%%)",
                  removed, 100 * removed / before))

  dt[, cluster := NULL]

  return(dt)
}

## TRUE for the points of one animal (sorted by time `tsec`, in seconds) in a cluster
flag_clusters <- function(tsec, x, y, radius_m, min_sec, min_points) {
  n <- length(tsec)
  flag <- logical(n)
  j <- 1L
  for (i in seq_len(n)) {
    # the run from i ends at j: the points i..j fit in the box. A run from i - 1 that reached j
    # holds the run from i, so j never moves back
    j <- max(i, j)
    min_x <- min(x[i:j])
    max_x <- max(x[i:j])
    min_y <- min(y[i:j])
    max_y <- max(y[i:j])
    while (j < n) {
      nx <- c(min(min_x, x[j + 1L]), max(max_x, x[j + 1L]))
      ny <- c(min(min_y, y[j + 1L]), max(max_y, y[j + 1L]))
      if (sqrt(diff(nx)^2 + diff(ny)^2) >= radius_m) break
      min_x <- nx[1]; max_x <- nx[2]; min_y <- ny[1]; max_y <- ny[2]
      j <- j + 1L
    }
    long <- !is.na(min_sec) && tsec[j] - tsec[i] >= min_sec
    many <- !is.na(min_points) && j - i + 1L >= min_points
    if (j > i && (long || many)) flag[i:j] <- TRUE
  }
  flag
}
