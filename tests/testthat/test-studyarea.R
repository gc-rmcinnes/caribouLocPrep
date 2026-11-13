testthat::local_edition(3)

two_jurisdictions <- function() {
  rbind(
    make_track("a", n = 5)[, jurisdiction := "sk"],
    make_track("b", n = 5, x0 = 600000)[, jurisdiction := "mb"]
  )
}

test_that("studyareaFullextent() makes one polygon around every point, buffered", {
  d <- two_jurisdictions()
  sa <- studyareaFullextent(d, rangeBuffer = 10000)
  expect_length(sa, 1)
  expect_s3_class(sa, "sfc_POLYGON")
  expect_equal(sf::st_crs(sa), sf::st_crs(3978))

  pts <- sf::st_as_sf(d, coords = c("x", "y"), crs = 3978)
  expect_true(all(sf::st_within(pts, sa, sparse = FALSE)))
  ## the buffer: each point is at least about rangeBuffer from the edge
  edge <- sf::st_cast(sa, "LINESTRING")
  expect_true(all(as.numeric(sf::st_distance(pts, edge)) > 9900))
})

test_that("studyareaByJurisdiction() makes one buffered polygon per jurisdiction", {
  d <- two_jurisdictions()
  sa <- studyareaByJurisdiction(d, rangeBuffer = 10000)
  expect_named(sa, c("sk", "mb"))
  for (j in names(sa)) {
    expect_length(sa[[j]], 1)
    expect_s3_class(sa[[j]], "sfc_POLYGON")
    expect_equal(sf::st_crs(sa[[j]]), sf::st_crs(3978))
    pts <- sf::st_as_sf(d[jurisdiction == j], coords = c("x", "y"), crs = 3978)
    expect_true(all(sf::st_within(pts, sa[[j]], sparse = FALSE)))
  }
  ## the two areas are 500 km apart, so they do not touch
  expect_false(sf::st_intersects(sa$sk, sa$mb, sparse = FALSE)[1, 1])
})

test_that("studyareaByJurisdiction() leaves out locations with no jurisdiction", {
  d <- two_jurisdictions()
  d[1, jurisdiction := NA]
  expect_named(studyareaByJurisdiction(d, rangeBuffer = 10000), c("sk", "mb"))
})

test_that("spaceless() removes spaces from column names", {
  expect_named(spaceless(data.frame(`a b` = 1, c = 2, check.names = FALSE)), c("ab", "c"))
})
