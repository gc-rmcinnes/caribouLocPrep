testthat::local_edition(3)

## simInit() runs .inputObjects(); spades() with `events` runs only the events named, so the save
## event (an upload to Drive) never runs here. `boo` is always supplied, so nothing is downloaded.

## simInit() attaches every package in the module's reqdPkgs
skip_unless_reqdPkgs <- function() {
  for (pkg in c("SpaDES.core", "ggplot2", "data.table", "readxl", "fs", "tidyr", "purrr", "janitor",
                "raster", "sf", "googledrive", "sfheaders", "amt", "Require", "reproducible", "move2",
                "dplyr", "terra", "rnaturalearth")) {
    skip_if_not_installed(pkg)
  }
}

## A module path holding this module, temporary SpaDES paths, and, at the end of the calling test,
## the packages, options and terra temporary folder simInit() changes put back as they were.
local_spades_sandbox <- function(env = parent.frame()) {
  attached <- search()
  withr::defer(
    for (p in setdiff(search(), attached)) detach(p, character.only = TRUE),
    envir = env
  )
  opts <- options()
  withr::defer(
    {
      added <- setdiff(names(options()), names(opts))
      options(opts)
      options(stats::setNames(vector("list", length(added)), added))
    },
    envir = env
  )
  terraTmp <- terra::terraOptions(print = FALSE)$tempdir
  withr::defer(terra::terraOptions(tempdir = terraTmp), envir = env)
  options(spades.useRequire = FALSE, reproducible.useCache = FALSE, spades.moduleCodeChecks = FALSE)

  mods <- withr::local_tempdir(.local_envir = env)
  file.symlink(normalizePath(testthat::test_path("..", "..")), file.path(mods, "caribouLocPrep"))
  list(modulePath = mods, other = withr::local_tempdir(.local_envir = env))
}

run_simInit <- function(paths, objects, params = list()) {
  suppressMessages(SpaDES.core::simInit(
    times = list(start = 2025, end = 2025),
    params = list(caribouLocPrep = utils::modifyList(
      list(jurisdiction = c("SK", "MB"), rangeBuffer = 10000, .studyAreaName = "test",
           .useCache = FALSE),
      params
    )),
    modules = "caribouLocPrep",
    paths = list(
      modulePath = paths$modulePath,
      inputPath = paths$other,
      outputPath = paths$other,
      cachePath = paths$other,
      scratchPath = paths$other
    ),
    objects = objects
  ))
}

test_that("the module's metadata parses", {
  skip_unless_reqdPkgs()
  paths <- local_spades_sandbox()

  md <- SpaDES.core::moduleMetadata(module = "caribouLocPrep", path = paths$modulePath)
  expect_contains(md$inputObjects$objectName, "boo")
  expect_contains(md$outputObjects$objectName, c("caribouLoc", "studyAreaCaribou", "studyArea_juris"))
  expect_contains(md$parameters$paramName, c("jurisdiction", "rangeBuffer", "outputFolderID"))
})

test_that("the module makes the cleaned locations and the study areas from a supplied boo", {
  skip_unless_reqdPkgs()
  paths <- local_spades_sandbox()
  boo <- list(SK = make_track("1", n = 6), MB = make_track("3", n = 6, x0 = 600000))

  sim <- run_simInit(paths, list(boo = boo))
  expect_identical(sim$boo, boo)
  sim <- suppressMessages(SpaDES.core::spades(
    sim, events = c("init", "downloadData", "createFullExtent", "createJurisdictionExtents")
  ))

  expect_s3_class(sim$caribouLoc, "data.table")
  expect_setequal(sim$caribouLoc$id, c("1", "3"))
  expect_s4_class(sim$studyAreaCaribou, "SpatVector")
  expect_equal(nrow(sim$studyAreaCaribou), 1)
  expect_named(sim$studyArea_juris, c("sk", "mb"))
  for (sa in sim$studyArea_juris) expect_s4_class(sa, "SpatVector")
})

test_that("init stops, saying how to fix it, when MoveBank data are asked for with no login", {
  skip_unless_reqdPkgs()
  paths <- local_spades_sandbox()

  sim <- run_simInit(paths, list(boo = list(YT = make_track())),
                     params = list(jurisdiction = "YT", MoveBankUser = "", MoveBankPass = ""))
  expect_error(suppressMessages(SpaDES.core::spades(sim, events = "init")),
               "Movebank credentials were not found")
})
