createGlobalSA <- function(studyArea_juris, jurisdictions) {

  juris <- toupper(jurisdictions)
  # Subset polygons
  sa_subset <- studyArea_juris[juris]
  sa_combined <- do.call(rbind, sa_subset)
  # Union touching polygons
  sa_union <- sf::st_union(sa_combined)
  # Split disconnected components
  sa_blobs <- sf::st_cast(sa_union, "POLYGON")
  sa_blobs <- sf::st_as_sf(sa_blobs)

  # Determine jurisdictions per blob
  blob_list <- lapply(seq_len(nrow(sa_blobs)), function(i) {

    blob_geom <- sa_blobs[i, ]

    intersects <- sf::st_intersects(blob_geom, sa_combined, sparse = FALSE)
    juris_in_blob <- juris[which(colSums(intersects) > 0)]

    blob_name <- paste0("globalSA_", paste(juris_in_blob, collapse = ""))

    blob_geom$blobName <- blob_name
    blob_geom
  })

  do.call(rbind, blob_list)
}

