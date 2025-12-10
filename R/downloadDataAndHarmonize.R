downloadDataAndHarmonize <- function(jurisdiction, boo){ # TODO More informative variable names is good practice
  #harmonize the jurisdictions provided and remove erroneous points
  dat.all <- list()
  colnames <- c(BC = "Population_Unit", SK = "SK1", MB = "Range", YT = "Yukon")
  subpops <- list(BC = NA, SK = NA, MB = NA, NT = "habitat", YT = NA)
  #data cleaning function
  rmDupsKeepGT1HarmColumns <- function(dt, jur) {
    dt[,any(duplicated(datetime)), by = id]
    dt <- unique(dt, by = c('id', 'datetime'))
    dt[,Npts := .N, by = .(id)]
    dt <- dt[Npts>4] # remove those with less than 4 point
    dt[,.(id, jurisdiction = tolower(jur), pop = colnames[jur], subpop = subpops[jur], datetime, x, y)]
  }
  dat.all <- Map(jur = jurisdiction, function(jur) {
    dt <- boo[[jur]]
    rmDupsKeepGT1HarmColumns(dt, jur)
  })
  dat.bind <- do.call("rbind", dat.all)
  dat.bind[, id := as.character(id)]

  # Filter unrealistic speeds
  dat.bind <- filter_fast_steps(dat.bind, speed_threshold_m_per_hr = 12000)

  # Filter long-term clusters
  dat.bind <- filter_clusters(dat.bind, window_hours = 48, radius_m = 50)

  # Filter fast round-trips
  dat.bind <- filter_fast_roundtrips(dat.bind, speed_threshold_m_per_hr = 12000, ratio_threshold = 0.25)

  ### remove crazy points in Russia (??) ----
  # TODO I would crop out anything out of Canada with a Canada shapefile as
  # default. This shapefile can be which any shapefile a user provides.
  # This way you keep just the data you want for a specific region (i.e., what
  # if I just want a part of Edehzie in NWT?) Suggestion for existing shapefiles for Canada:
  # canadaBounds <- rnaturalearth::ne_countries(country = "Canada", returnclass = "sv")
  dat.clean <- dat.bind[complete.cases(x,y, datetime)&between(x, -3894480, 4432820)&between(y, -1229436, 4329766)]
  # Save clean data ----
  return(dat.clean)
}
