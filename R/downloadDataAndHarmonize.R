downloadDataAndHarmonize <- function(jurisdiction, boo){

  # Harmonize the jurisdictions provided and remove erroneous points

  dat.all <- list()

  # Updated mappings
  colnames <- c(BC = "Population_Unit", SK = "SK1", MB = "Range", YT = "Yukon", NT = "habitat")

  subpops <- list(BC = NA,SK = NA,MB = NA,ON =NA,NT = "habitat",YT = NA,NTYT = "Territories"
  )

  # Detect NT + YT presence and replace with NTYT
  has_NT <- "NT" %in% jurisdiction
  has_YT <- "YT" %in% jurisdiction

  jur_clean <- jurisdiction

  if (has_NT && has_YT) {
    jur_clean <- setdiff(jur_clean, c("NT", "YT"))
    jur_clean <- c(jur_clean, "NTYT")
  }

  # Data cleaning function
  rmDupsKeepGT1HarmColumns <- function(dt, jur) {
    dt[, any(duplicated(datetime)), by = id]
    dt <- unique(dt, by = c('id', 'datetime'))
    dt[, Npts := .N, by = .(id)]
    dt <- dt[Npts > 4]

    dt[, .(id,jurisdiction = tolower(jur),pop = colnames[jur],subpop = subpops[[jur]],datetime,x,y)]
  }

  # Apply cleaning
  dat.all <- Map(jur = jur_clean, function(jur) {

    dt <- boo[[jur]]
    out <- rmDupsKeepGT1HarmColumns(dt, jur)

    return(out)
  })

  dat.bind <- do.call("rbind", dat.all)

  # Ensure id is character
  dat.bind[, id := as.character(id)]

  # Filter unrealistic speeds
  dat.bind <- filter_fast_steps(dat.bind, speed_threshold_m_per_hr = 12000)

  # Filter long-term clusters
  dat.bind <- filter_clusters(dat.bind, window_hours = 48, radius_m = 50)

  # Filter fast round-trips
  dat.bind <- filter_fast_roundtrips(
    dat.bind,
    speed_threshold_m_per_hr = 12000,
    ratio_threshold = 0.25
  )

  # Remove erroneous coordinates
  dat.clean <- dat.bind[
    complete.cases(x, y, datetime) &
      between(x, -3894480, 4432820) &
      between(y, -1229436, 4329766)]

  return(dat.clean)
}
