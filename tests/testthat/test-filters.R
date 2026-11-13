testthat::local_edition(3)

test_that("filter_fast_steps() drops the point at the end of a too-fast step", {
  d <- make_track(n = 4)
  d[3:4, x := x + 100000] # a 101 km step in 4 h, then on as before
  expect_message(out <- filter_fast_steps(d, speed_threshold_m_per_hr = 12000), "Removed 1 points")
  expect_equal(out$datetime, d$datetime[-3])
  expect_named(out, names(d))
})

test_that("filter_fast_steps() looks at each animal on its own", {
  ## b's first point is far from a's last one, but that is not a step
  d <- rbind(make_track("a", n = 3), make_track("b", n = 3, x0 = 900000))
  out <- suppressMessages(filter_fast_steps(d, speed_threshold_m_per_hr = 12000))
  expect_equal(nrow(out), 6)
})

test_that("filter_fast_steps() does not change its input", {
  d <- make_track(n = 4)
  d[3, x := x + 100000]
  before <- copy(d)
  suppressMessages(filter_fast_steps(d, speed_threshold_m_per_hr = 12000))
  expect_identical(d, before)
})

test_that("filter_fast_roundtrips() drops a fast out-and-back spike", {
  d <- make_track(n = 5)
  d[3, x := x + 60000] # out 60 km and back within 8 h
  expect_message(out <- filter_fast_roundtrips(d), "Removed 1 points")
  expect_equal(out$datetime, d$datetime[-3])
  expect_named(out, names(d))
})

test_that("filter_fast_roundtrips() keeps a fast move that does not come back", {
  d <- make_track(n = 5, step_m = 60000) # 60 km every 4 h, always east
  out <- suppressMessages(filter_fast_roundtrips(d))
  expect_equal(nrow(out), 5)
})

test_that("filter_clusters() drops points that stay within radius_m for a stretch of time", {
  ## a moving track, then 16 points 2 m apart over 60 h (e.g. a dropped collar)
  moving <- make_track(n = 4)
  still <- make_track(n = 16, step_m = 2, x0 = 104000)
  still[, datetime := datetime + 3600 * 16]
  d <- rbind(moving, still)
  out <- suppressMessages(filter_clusters(d, window_hours = 48, radius_m = 50))
  expect_false(any(out$datetime %in% still$datetime))
  expect_equal(out$datetime, moving$datetime)
  expect_named(out, names(d))
})

test_that("filter_clusters() keeps every point of a moving track", {
  d <- make_track(n = 6)
  out <- suppressMessages(filter_clusters(d, window_hours = 48, radius_m = 50))
  expect_equal(out, d)
})

test_that("filter_clusters() keeps a lone point after a gap longer than window_hours", {
  d <- make_track(n = 6)
  d[4:6, datetime := datetime + 3600 * 100]
  out <- suppressMessages(filter_clusters(d, window_hours = 48, radius_m = 50))
  expect_equal(out, d)
})

test_that("filter_clusters() keeps a short stop, shorter than window_hours", {
  ## 4 points 2 m apart over 12 h, between moving stretches
  d <- make_track(n = 10)
  d[4:7, x := x[4] + 2 * (0:3)]
  out <- suppressMessages(filter_clusters(d, window_hours = 48, radius_m = 50))
  expect_equal(out, d)
})

test_that("filter_clusters() with min_points also drops a run of that many points", {
  d <- make_track(n = 10)
  d[4:7, x := x[4] + 2 * (0:3)]
  out <- suppressMessages(filter_clusters(d, window_hours = 48, radius_m = 50, min_points = 4))
  expect_equal(out$datetime, d$datetime[-(4:7)])
  ## window_hours = NA: the number of points alone
  out <- suppressMessages(filter_clusters(d, window_hours = NA, radius_m = 50, min_points = 5))
  expect_equal(out, d)
})

test_that("filter_fast_roundtrips() keeps the first and last points, even after a fast step", {
  d <- make_track(n = 3, step_m = 0)
  d[2, x := x + 60000] # a spike: the points around it are an animal's first and last
  out <- suppressMessages(filter_fast_roundtrips(d))
  expect_equal(out$datetime, d$datetime[-2])
})
