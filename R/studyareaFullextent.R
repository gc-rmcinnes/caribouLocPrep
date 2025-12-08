studyareaFullextent <- function(dat.clean,
                                rangeBuffer,
                                targetCRS = 3978){
  # create study area and buffered study area ----
  sfcoords<- dat.clean%>%st_as_sf(coords = c('x','y'))%>%
    st_set_crs(st_crs(targetCRS))

  ## A i.e., 100 km buffer around points to get an idea of extent of study area
  ## A convex hull of all the polygons creates a single polygon for the simulation
  studyArea.buff <- st_buffer(sfcoords, dist = rangeBuffer)
  studyArea.sf <- st_as_sf(studyArea.buff)
  studyArea.union <- st_union(studyArea.sf)
  studyArea.ch <- sf::st_convex_hull(studyArea.union)

  return(studyArea.ch)
  ## save buffered study area ----
}
