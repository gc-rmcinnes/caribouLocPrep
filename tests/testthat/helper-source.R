## Load the packages the module's R/ functions call without `::` (reqdPkgs) and those functions.
for (pkg in c("data.table", "sf", "dplyr")) {
  suppressPackageStartupMessages(library(pkg, character.only = TRUE))
}
for (f in list.files(testthat::test_path("..", "..", "R"), pattern = "[.]R$", full.names = TRUE)) {
  source(f, local = environment())
}
