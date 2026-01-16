studyareaByJurisdiction <- function(dat.clean,
                                    rangeBuffer,
                                    jurisCol = "jurisdiction",
                                    targetCRS = 3978) {

  # points as sf
  pts <- sf::st_as_sf(dat.clean, coords = c("x", "y"), remove = FALSE) |>
    sf::st_set_crs(sf::st_crs(targetCRS))

  juris <- unique(pts[[jurisCol]])
  juris <- juris[!is.na(juris)]

  studyAreas_juris <- setNames(vector("list", length(juris)), as.character(juris))

  for (j in names(studyAreas_juris)) {
    pts_j <- pts[pts[[jurisCol]] == j, ]

    studyAreas_juris[[j]] <-
      sf::st_convex_hull(
        sf::st_union(
          sf::st_buffer(pts_j, dist = rangeBuffer)
        )
      )
  }

  studyAreas_juris
}
