testthat::local_edition(3)

## made-up raw locations for two jurisdictions, as the dataPrep_*() functions return them
fixture_boo <- function() {
  sk <- rbind(
    make_track("1", n = 6),
    make_track("1", n = 1),          # the same time as the first point: a duplicate
    make_track("2", n = 4, x0 = 150000) # 4 points: too few to keep
  )
  mb <- rbind(
    make_track("3", n = 6, x0 = 600000),
    make_track("4", n = 6, x0 = 5000000) # outside the box of possible coordinates
  )
  list(SK = sk, MB = mb)
}

test_that("downloadDataAndHarmonize() harmonizes columns and drops duplicates and short tracks", {
  out <- suppressMessages(downloadDataAndHarmonize(c("SK", "MB"), fixture_boo()))
  expect_named(out, c("id", "jurisdiction", "pop", "subpop", "datetime", "x", "y"))
  expect_type(out$id, "character")
  expect_equal(out[, .N, keyby = id], data.table(id = c("1", "3"), N = 6L, key = "id"))
  expect_equal(unique(out[, .(id, jurisdiction, pop)]),
               data.table(id = c("1", "3"), jurisdiction = c("sk", "mb"), pop = c("SK1", "Range")))
  expect_false(anyDuplicated(out, by = c("id", "datetime")) > 0)
})

test_that("downloadDataAndHarmonize() uses only the jurisdictions asked for", {
  out <- suppressMessages(downloadDataAndHarmonize("SK", fixture_boo()))
  expect_equal(unique(out$jurisdiction), "sk")
})
