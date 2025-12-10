spaceless <- function(x) {colnames(x) <- gsub(" ", "", colnames(x));x}

#i think breaking these out into individual scripts would work well and have cleaner module code
dataPrep_SK <- function(dPath =dpath) {
  ### Input data ----
  sk <- dPath

  # list of files
  ls_sk <- as.character(dir_ls(path = sk, regexp = "Telemetry"))

  temp <- data.table()
  temp <- rbindlist(lapply(seq(1:length(ls_sk)), function(i){
    temp[,.(file= ls_sk[[i]],
            data = list(setDT(read_excel(ls_sk[[i]], sheet = 'TelemetryData', skip = 3))))]
  }))

  temp$file

  #### gather just needed data ####
  colnames(temp[1]$data[[1]])
  str(temp[1]$data[[1]]$`Sample Date                              (yyyy-mm-dd)`)
  spaceless(temp[1]$data[[1]])

  prep_sk <- lapply(ls_sk, function(ll){
    temp[file == ll,.(file,
                      spaceless(data[[1]][,.SD, .SDcols = names(data[[1]]) %like% 'Sample|Latitude|Longitude|UTM|Datum|Individual|Comments & !Sensitive']))]
  })
  #standardize the column names
  colnames(prep_sk[[1]])
  newnames <- c('file','date','time','datum','lat','long','UTMzone','northing','easting','id')
  for (dd in 1:length(ls_sk)) {
    setnames(prep_sk[[dd]], old = colnames(prep_sk[[dd]]), new = newnames)
    prep_sk[[dd]][,`:=`(date = as.IDate(convert_to_date(date)))]
    prep_sk[[dd]][,time:=as.ITime(as.character(time)) ]
    prep_sk[[dd]][,datetime:= as.POSIXct(paste(date,time, sep = ' '))]
    prep_sk[[dd]][,lat:= as.numeric(lat)]
    prep_sk[[dd]][,long:= as.numeric(long)]
  }

  #check for complete entries and remove any incomplete data
  dat_sk <- rbindlist(prep_sk)
  dat_sk <- dat_sk[complete.cases(long,lat, datetime)]
  dat_sk <- dat_sk[long<0&lat>0]
  # convert from long/lat to NAD83/Canada Atlas Lambert (need units to be m)
  crs <- CRS(st_crs(4326)$wkt)
  outcrs <- st_crs(3978)
  sfboo <- st_as_sf(dat_sk, coords = c('long', 'lat'),
                    crs = crs)
  outboo <- st_transform(sfboo, outcrs)
  booSK <- setDT(sfheaders::sf_to_df(outboo, fill = T))

  return(booSK)
}

dataPrep_BC <- function(dPath=dPath, bc_layer) {
  ### Input data ----
  dat <- data.table(as.data.frame(bc_layer),geom(bc_layer))
  dat <- dat[!(is.na(WLH_ID)) & WLH_ID != 'None',.(id = WLH_ID, Pop_Unt, datetime = Date, x, y)]
  dat[, datetime := as.POSIXct(datetime, format = "%Y-%m-%d %H:%M:%OS %z", tz = "UTC")]
  #check for complete entries and remove any incomplete data
  dat_cleaner <- dat[complete.cases(x,y, datetime)]

  ### convert from long/lat to NAD83/Canada Atlas Lambert (need units to be m)
  crs <- st_crs(4326)$wkt
  outcrs <- st_crs(3978)

  sfboo <- st_as_sf(dat_cleaner, coords = c('x', 'y'),
                    crs = crs)
  outboo <- st_transform(sfboo, outcrs)
  booBCnc <- setDT(sfheaders::sf_to_df(outboo, fill = T))

  ### EXPLORE ----
  # check if all observations are complete
  all(complete.cases(booBCnc[,.(x,y, datetime)]))

  # check for duplicated time stamps
  booBCnc[,any(duplicated(datetime)), by = id]

  # We have some duplicated time stamps, these need to be removed prior to creating a track.
  DT <- unique(booBCnc, by = c('id', 'datetime'))
  # remove entries that have large time gaps
  # DTclean <- DT[
  #   order(id, datetime),
  #   .SD[is.na(shift(datetime)) | difftime(datetime, shift(datetime), units = "hours") <= 13],
  #   by = id]

  booBC <- DT

  return(booBC)
}

dataPrep_NT <- function(loginStored, herds) {

  habi <- data.table(habs = c('dehcho', 'north.slave', 'sahtu', 'sahtu2020', 'south.slave'),
                    herd = c('Dehcho Boreal Woodland Caribou', 'North Slave Boreal Caribou',
                             'Sahtu Boreal Woodland Caribou', 'Sahtu Boreal Woodland Caribou (2020)',
                             'South Slave Boreal Woodland Caribou'))
  hab <- habi[herd %in% herds, habs]
  study <- list(384182382,469002388,151021631,1902733508,149370498)

  # make a list of all data from Movebank
  nt.move <- list()
  for (ds in 1:length(study)) {
    nt.move[[ds]] <- Cache(move2::movebank_download_study, study = study[ds],
                                           login = loginStored, removeDuplicatedTimestamps=TRUE)
  }

  nt <- rbindlist(lapply(1:length(hab), function(hh){
    # First, let's work with the specific move2 object.
    moveTable <- nt.move[[ds]]

    # Move the track-level attributes to the event level
    # This will add all columns from the track data to the event data.
    flatMoveTable <- mt_as_event_attribute(moveTable, names(mt_track_data(moveTable)))

    # The result is still an sf object. To convert it to a data.table,
    # we should also extract the coordinates from the geometry column.
    # Then we can drop the geometry and convert to a data.table.

    # Extract coordinates into separate columns
    coords <- st_coordinates(flatMoveTable)
    flatMoveTable$x <- coords[, "X"] # Longitude
    flatMoveTable$y <- coords[, "Y"] # Latitude

    # Drop the geometry column
    flatDF <- st_drop_geometry(flatMoveTable)

    # Convert the resulting data.frame to a data.table
    flatDT <- as.data.table(flatDF)
    tb <- data.table(area = hab[[hh]])
    tb[,dat := list(setDT(flatDT))]

    return(tb)
  }), use.names = TRUE)

  # # gathering just the columns needed
  #TODO This seems a memory demanding way to drop unused and rename columns.
  # I would re-write it using data.table original functions as you are anyway
  # dropping all the rest of the information
  nt.long <- rbindlist(lapply(1:length(hab), function(hh){
    ntD <- nt$dat[[hh]][,.(area = hab[[hh]],
                    habitat,
                    id=tag_id,
                    location.long=x,
                    location.lat=y,
                    datetime = timestamp)]
    return(ntD)
  }))

  # convert from long/lat to NAD83/Canada Atlas Lambert (need units to be m)
  crs <- CRS(st_crs(4326)$wkt)
  outcrs <- st_crs(3978)

  sfboo <- st_as_sf(nt.long, coords = c('location.long', 'location.lat'),
                    crs = crs)
  outboo <- st_transform(sfboo, outcrs)
  booNT <- setDT(sfheaders::sf_to_df(outboo, fill = TRUE))
  return(booNT)
}

dataPrep_YT <- function(loginStored) {
  ### Input data ----
  dat <- Cache(move2::movebank_download_study, study_id = "Boreal Caribou - Yukon Collars",
               login = loginStored, removeDuplicatedTimestamps=TRUE, progress = TRUE)
  ### Prep data ----
  dat <- data.table(dat)[, c("x", "y") := as.data.frame(st_coordinates(geometry))]

  dat_cleaner <- dat[complete.cases(x,y, timestamp)]
  data.table::setnames(dat_cleaner, "timestamp", "datetime")
  ### convert from long/lat to NAD83/Canada Atlas Lambert (need units to be m)
  crs <- st_crs(4326)$wkt
  outcrs <- st_crs(3978)

  sfboo <- st_as_sf(dat_cleaner, coords = c('x', 'y'),
                    crs = crs)
  outboo <- st_transform(sfboo, outcrs)
  boo_notclean <- setDT(sfheaders::sf_to_df(outboo, fill = T))

  ### standarize names and columns ----
  booYT <- boo_notclean[, .(id = individual_local_identifier, datetime, x, y, argos_altitude, gps_fix_type_raw)]

  ### EXPLORE ----
  # check if all observations are complete
  booComplete <- all(complete.cases(booYT[,.(x,y, datetime)]))
  return(booYT)
}

dataPrep_MB <- function(dPath = dPath) {
  ### Input data ----
  raw.mb <- dPath
  #unzip all of the .gdb files
  MI_Berens <- st_read(file.path(raw.mb, 'CaribouGPSData_AtikakiBerens.gdb.zip'), "MI_Berens_Caribou_2020APR02_CLEAN_NAD83_Z14")
  BERENS_RND <- st_read(file.path(raw.mb, 'CaribouGPSData_AtikakiBerens.gdb.zip'), "BERENS_RND_GPS_2001_to_2004_complete_NAD83Z14")
  BLDVN <- st_read(file.path(raw.mb, 'CaribouGPSData_AtikakiBerens.gdb.zip'), "BLDVN_2000_to_2014_complete_NAD83Z14")
  ATIKO <- st_read(file.path(raw.mb, 'CaribouGPSData_AtikakiBerens.gdb.zip'), "ATIKO_2000_to_2014_complete_NAD83Z14")

  interlake <- st_read(file.path(raw.mb, 'CaribouGPSData_Interlake.gdb.zip'), "Interlake_Total_Jan2021")

  MI_NWH <- st_read(file.path(raw.mb, 'CaribouGPSdata_Molson.gdb.zip'), "MI_NWH_Caribou_Telemetry2020APR02_Clean_NAD83")
  charron1 <- st_read(file.path(raw.mb, 'CaribouGPSdata_Molson.gdb.zip'), "CharronLK_GPS_MBHYDRO_Q2_2010_072020")
  charron2 <- st_read(file.path(raw.mb, 'CaribouGPSdata_Molson.gdb.zip'), "CharronLK_GPS_MBHYDRO_Q4_2010_022021")

  # imperial doesn't have enough data for an SSA, it's just VHF
  MBhydro <- st_read(file.path(raw.mb, 'CaribouGPSdata_NaosapReed.gdb.zip'), "MBHYDRO_Q2_2010_072020")
  naosap1 <- st_read(file.path(raw.mb, 'CaribouGPSdata_NaosapReed.gdb.zip'), "Naosap_Reed_Total_NAD83_July2018")
  naosap2 <- st_read(file.path(raw.mb, 'CaribouGPSdata_NaosapReed.gdb.zip'), "gps_naosap_2002_06")
  kississing <- st_read(file.path(raw.mb, 'CaribouGPSdata_NaosapReed.gdb.zip'), "Kississing_Total_NAD83_July2018")
  #imperial <- st_read(file.path(raw.mb, 'CaribouGPSdata_NaosapReed.gdb.zip'), "Imperial_NAD83_2011")
  naosap3 <- st_read(file.path(raw.mb, 'CaribouGPSdata_NaosapReed.gdb.zip'), "NaosapReed_MBHYDRO_Q4_2010_022021")

  flintstone <- st_read(file.path(raw.mb, 'CaribouGPSdata_Owl_Flintstone.gdb.zip'), "Owl_Flintstone_1995_2018_complete_WGS84")

  wimwap1<- st_read(file.path(raw.mb, 'CaribouGPSData_PartridgeCrop.gdb.zip'), "Wim_Wap_GPS_MBHydro_Q2_2010_072020")
  wimwap2 <- st_read(file.path(raw.mb, 'CaribouGPSData_PartridgeCrop.gdb.zip'), "Wim_Wap_GPS_MBHYDRO_Q4_2010_022021")
  harding1 <- st_read(file.path(raw.mb, 'CaribouGPSData_PartridgeCrop.gdb.zip'), "Harding_GPS_MBHydro_Q2_2010_072020")
  harding2 <- st_read(file.path(raw.mb, 'CaribouGPSData_PartridgeCrop.gdb.zip'), "Harding_GPS_MBHYDRO_Q4_2010_022021")
  wheadon1 <- st_read(file.path(raw.mb, 'CaribouGPSData_PartridgeCrop.gdb.zip'), "Wheadon_GPS_MBHydro_Q2_2010_072020")
  wheadon2 <- st_read(file.path(raw.mb, 'CaribouGPSData_PartridgeCrop.gdb.zip'), "Wheadon_Total_NAD83_Nov2018_Final")
  wheadon3 <- st_read(file.path(raw.mb, 'CaribouGPSData_PartridgeCrop.gdb.zip'), "Wheadon_GPS_MBHYDRO_Q4_2010_022021")

  bog1<- st_read(file.path(raw.mb, 'CaribouGPSdata_TheBog.gdb.zip'), "TheBog_MH_2010_072020")
  bog2 <- st_read(file.path(raw.mb, 'CaribouGPSdata_TheBog.gdb.zip'), "TheBog_MBHYDRO_Q4_2010_022021")

  william <- st_read(file.path(raw.mb, 'CaribouGPSdata_Wabowden.gdb.zip'), "William_Lake_Total_NAD83_Dec2019")
  wabowden1 <- st_read(file.path(raw.mb, 'CaribouGPSdata_Wabowden.gdb.zip'), "Wabowden_GPS_MH_Q2_2010_072020")
  wabowden2 <- st_read(file.path(raw.mb, 'CaribouGPSdata_Wabowden.gdb.zip'), "Wabowden_UHF_relocations_2009to2012_NAD83")
  wabowden3 <- st_read(file.path(raw.mb, 'CaribouGPSdata_Wabowden.gdb.zip'), "Wabowden_GPS_MBHYDRO_Q4_2010_022021")

  kamuchawie <- st_read(file.path(raw.mb, 'CaribouGPSData_Kamuchawie.gdb.zip'), "Positions2025Jun12_Kamuchawie")

  ### Prep data ----
  ### convert to NAD83/Canada Atlas Lambert (need units to be m and consistent across)
  outcrs <- st_crs(3978)

  out_MI_Berens <- st_transform(MI_Berens, outcrs)
  out_BERENS_RND <- st_transform(BERENS_RND, outcrs)
  out_BLDVN <- st_transform(BLDVN, outcrs)
  out_ATIKO <- st_transform(ATIKO, outcrs)

  out_interlake <- st_transform(interlake, outcrs)

  out_MI_NWH <- st_transform(MI_NWH, outcrs)
  out_charron1 <- st_transform(charron1, outcrs)
  out_charron2 <- st_transform(charron2, outcrs)

  out_MBhydro <- st_transform(MBhydro, outcrs)
  out_naosap1 <- st_transform(naosap1, outcrs)
  out_naosap2 <- st_transform(naosap2, outcrs)
  out_naosap3 <- st_transform(naosap3, outcrs)
  out_kississing <- st_transform(kississing, outcrs)
  #out_imperial <- st_transform(imperial, outcrs)

  out_flintstone <- st_transform(flintstone, outcrs)

  out_wimwap1 <- st_transform(wimwap1, outcrs)
  out_wimwap2 <- st_transform(wimwap2, outcrs)
  out_harding1 <- st_transform(harding1, outcrs)
  out_harding2 <- st_transform(harding2, outcrs)
  out_wheadon1 <- st_transform(wheadon1, outcrs)
  out_wheadon2 <- st_transform(wheadon2, outcrs)
  out_wheadon3 <- st_transform(wheadon3, outcrs)

  out_bog1 <- st_transform(bog1, outcrs)
  out_bog2 <- st_transform(bog2, outcrs)

  out_william <- st_transform(william, outcrs)
  out_wabowden1 <- st_transform(wabowden1, outcrs)
  out_wabowden2 <- st_transform(wabowden2, outcrs)
  out_wabowden3 <- st_transform(wabowden3, outcrs)

  out_kamuchawie <- st_transform(kamuchawie, outcrs)


  # checking for right formats and grabbing what need
  DT_MI_Berens <- setDT(sfheaders::sf_to_df(out_MI_Berens, fill = T))
  DT_MI_Berens <- DT_MI_Berens[,.(id = Animal_ID, Fix_Status, Range,
                                  datetime = as.POSIXct(paste(paste(GMT_Year, GMT_Month, GMT_Day, sep = '-'),
                                                              paste(GMT_Hour, GMT_Minute, sep = ':'), sep = ' '),
                                                        tz = 'gmt', format = '%Y-%m-%d %H:%M'),
                                  x, y)]
  # TODO what is time tz for BERENS_RND, BLDVN?
  DT_BERENS_RND <- setDT(sfheaders::sf_to_df(out_BERENS_RND, fill = T))
  DT_BERENS_RND <- DT_BERENS_RND[,.(id = ANIMAL_ID, Fix_Status = FIX_STATUS, Range = RANGE,
                                    datetime = as.POSIXct(paste(paste(YEAR, MONTH, DAY, sep = '-'),
                                                                TIME, sep = ' '),
                                                          format = '%Y-%m-%d %H:%M:%OS'),
                                    x, y)]
  DT_BLDVN <- setDT(sfheaders::sf_to_df(out_BLDVN, fill = T))
  DT_BLDVN <-DT_BLDVN[,.(id = ANIMAL_ID, Fix_Status = FIX_STATUS, Range = RANGE,
                         datetime = as.POSIXct(paste(gsub( " .*$", "", DATE ),
                                                     TIME, sep = ' '),
                                               format = '%Y-%m-%d %H:%M:%OS'),
                         x, y)]
  DT_ATIKO <- setDT(sfheaders::sf_to_df(out_ATIKO, fill = T))
  DT_ATIKO <- DT_ATIKO[,.(id = ANIMAL_ID, Fix_Status = FIX_STATUS, Range = RANGE,
                          datetime = as.POSIXct(paste(paste(YEAR, MONTH, DAY, sep = '-'),
                                                      TIME, sep = ' '),
                                                format = '%Y-%m-%d %H:%M:%OS'),
                          x, y)]


  DT_interlake <- setDT(sfheaders::sf_to_df(out_interlake, fill = T))
  DT_interlake <- DT_interlake[,.(id = Animal_ID, Fix_Status, Range,
                                  datetime = as.POSIXct(paste(paste(Year, Month, Day, sep = '-'),
                                                              paste(Hour, Minute, Second, sep = ':'), sep = ' '),
                                                        format = '%Y-%m-%d %H:%M:%OS', tz = 'America/Chicago'),
                                  x, y)]

  DT_MI_NWH <- setDT(sfheaders::sf_to_df(out_MI_NWH, fill = T))
  DT_MI_NWH <- DT_MI_NWH[,.(id = Animal_ID, Fix_Status, Range = RANGE,
                            datetime = as.POSIXct(paste(paste(GMT_Year, GMT_Month, GMT_Day, sep = '-'),
                                                        paste(GMT_Hour, GMT_Minute, sep = ':'), sep = ' '),
                                                  tz = 'gmt', format = '%Y-%m-%d %H:%M'),
                            x, y)]
  DT_charron1 <- setDT(sfheaders::sf_to_df(out_charron1, fill = T))
  # what is the tz for Charron 1 and 2?
  DT_charron1 <- DT_charron1[,.(id = AnimalID, Fix_Status = NAV, Range = CaptureRan,
                                datetime = as.POSIXct(paste(paste(Year, Month, Day, sep = '-'),
                                                            paste(Hour, 00, 00, sep = ':'), sep = ' '),
                                                      format = '%Y-%m-%d %H:%M:%OS'),
                                x, y)]
  DT_charron2 <- setDT(sfheaders::sf_to_df(out_charron2, fill = T))
  DT_charron2 <- DT_charron2[,.(id = AnimalID, Fix_Status = NAV, Range = CaptureRan,
                                datetime = as.POSIXct(paste(paste(Year, Month, Day, sep = '-'),
                                                            paste(Hour, 00, 00, sep = ':'), sep = ' '),
                                                      format = '%Y-%m-%d %H:%M:%OS'),
                                x, y)]

  DT_MBhydro <- setDT(sfheaders::sf_to_df(out_MBhydro, fill = T))
  DT_MBhydro <- DT_MBhydro[,.(id = AnimalID, Fix_Status = NAV, Range = CaptureRan,
                              datetime = as.POSIXct(paste(paste(Year, Month, Day, sep = '-'),
                                                          paste(Hour, 00, 00, sep = ':'), sep = ' '),
                                                    format = '%Y-%m-%d %H:%M:%OS'),
                              x, y)]
  DT_naosap1 <- setDT(sfheaders::sf_to_df(out_naosap1, fill = T))
  DT_naosap1 <- DT_naosap1[,.(id = Animal_ID, Fix_Status = NumSats, Range,
                              datetime = as.POSIXct(paste(paste(Year, match(Month, month.name), sprintf("%02d", Day), sep = '-'),
                                                          paste(sprintf("%02d", Hour), sprintf("%02d", Minute), 00, sep = ':'), sep = ' '),
                                                    format = '%Y-%m-%d %H:%M:%OS'),
                              x, y)]
  DT_naosap2 <- setDT(sfheaders::sf_to_df(out_naosap2, fill = T))
  DT_naosap2 <- DT_naosap2[,.(id = UNIQUE_ID, Fix_Status = FIX_STATUS, Range = RANGE,
                              datetime = as.POSIXct(paste(paste(YEAR, MONTH, DAY, sep = '-'),
                                                          paste(HOUR, MINUTE, SECOND, sep = ':'), sep = ' '),
                                                    format = '%Y-%m-%d %H:%M:%OS'),
                              x, y)]
  DT_naosap3 <- setDT(sfheaders::sf_to_df(out_naosap3, fill = T))
  DT_naosap3 <- DT_naosap3[,.(id = AnimalID, Fix_Status = NAV, Range = CaptureRan,
                              datetime = as.POSIXct(paste(paste(Year, Month, Day, sep = '-'),
                                                          paste(Hour, 00, 00, sep = ':'), sep = ' '),
                                                    format = '%Y-%m-%d %H:%M:%OS'),
                              x, y)]
  DT_kississing <- setDT(sfheaders::sf_to_df(kississing, fill = T))
  DT_kississing <- DT_kississing[,.(id = Animal_ID, Fix_Status = NumSats, Range,
                                    datetime = as.POSIXct(paste(paste(Year, match(Month, month.name), sprintf("%02d", Day), sep = '-'),
                                                                paste(sprintf("%02d", Hour), sprintf("%02d", Minute), 00, sep = ':'), sep = ' '),
                                                          format = '%Y-%m-%d %H:%M:%OS'),
                                    x=-1*x, y)]
  # Don't use imperial, not enough info for SSA
  #DT_imperial <- setDT(sfheaders::sf_to_df(out_imperial, fill = T))

  DT_flintstone <- setDT(sfheaders::sf_to_df(out_flintstone, fill = T))
  DT_flintstone <- DT_flintstone[,.(id = ANIMAL_ID, Fix_Status = FIX_STATUS, Range,
                                    datetime = as.POSIXct(paste(paste(YEAR, MONTH, DAY, sep = '-'),
                                                                TIME, sep = ' '),
                                                          format = '%Y-%m-%d %H:%M:%OS'),
                                    x, y)]

  DT_wimwap1 <- setDT(sfheaders::sf_to_df(out_wimwap1, fill = T))
  DT_wimwap1 <- DT_wimwap1[,.(id = AnimalID, Fix_Status = NAV, Range = CaptureRan,
                              datetime = as.POSIXct(paste(paste(Year, Month, Day, sep = '-'),
                                                          paste(Hour, 00, 00, sep = ':'), sep = ' '),
                                                    format = '%Y-%m-%d %H:%M:%OS'),
                              x, y)]
  DT_wimwap2 <- setDT(sfheaders::sf_to_df(out_wimwap2, fill = T))
  DT_wimwap2 <- DT_wimwap2[,.(id = AnimalID, Fix_Status = NAV, Range = CaptureRan,
                              datetime = as.POSIXct(paste(paste(Year, Month, Day, sep = '-'),
                                                          paste(Hour, 00, 00, sep = ':'), sep = ' '),
                                                    format = '%Y-%m-%d %H:%M:%OS'),
                              x, y)]
  DT_harding1 <- setDT(sfheaders::sf_to_df(out_harding1, fill = T))
  DT_harding1 <- DT_harding1[,.(id = AnimalID, Fix_Status = NAV, Range = CaptureRan,
                                datetime = as.POSIXct(paste(paste(Year, Month, Day, sep = '-'),
                                                            paste(Hour, 00, 00, sep = ':'), sep = ' '),
                                                      format = '%Y-%m-%d %H:%M:%OS'),
                                x, y)]
  DT_harding2 <- setDT(sfheaders::sf_to_df(harding2, fill = T))
  DT_harding2 <- DT_harding2[,.(id = AnimalID, Fix_Status = NAV, Range = CaptureRan,
                                datetime = as.POSIXct(paste(paste(Year, Month, Day, sep = '-'),
                                                            paste(Hour, 00, 00, sep = ':'), sep = ' '),
                                                      format = '%Y-%m-%d %H:%M:%OS'),
                                x, y)]
  DT_wheadon1 <- setDT(sfheaders::sf_to_df(out_wheadon1, fill = T))
  DT_wheadon1 <- DT_wheadon1[,.(id = AnimalID, Fix_Status = NAV, Range = CaptureRan,
                                datetime = as.POSIXct(paste(paste(Year, Month, Day, sep = '-'),
                                                            paste(Hour, 00, 00, sep = ':'), sep = ' '),
                                                      format = '%Y-%m-%d %H:%M:%OS'),
                                x, y)]
  DT_wheadon2 <- setDT(sfheaders::sf_to_df(out_wheadon2, fill = T))
  DT_wheadon2 <- DT_wheadon2[,.(id = Animal_ID, Fix_Status = NumSats, Range,
                                datetime = as.POSIXct(paste(paste(Year, match(Month, month.name), sprintf("%02d", Day), sep = '-'),
                                                            paste(sprintf("%02d", Hour), sprintf("%02d", Minute), 00, sep = ':'), sep = ' '),
                                                      format = '%Y-%m-%d %H:%M:%OS'),
                                x=x, y)]
  DT_wheadon3 <- setDT(sfheaders::sf_to_df(out_wheadon3, fill = T))
  DT_wheadon3 <- DT_wheadon3[,.(id = AnimalID, Fix_Status = NAV, Range = CaptureRan,
                                datetime = as.POSIXct(paste(paste(Year, Month, Day, sep = '-'),
                                                            paste(Hour, 00, 00, sep = ':'), sep = ' '),
                                                      format = '%Y-%m-%d %H:%M:%OS'),
                                x, y)]

  DT_bog1 <- setDT(sfheaders::sf_to_df(out_bog1, fill = T))
  DT_bog1 <- DT_bog1[,.(id = AnimalID, Fix_Status = NAV, Range = CaptureRan,
                        datetime = as.POSIXct(paste(paste(Year, Month, Day, sep = '-'),
                                                    paste(Hour, 00, 00, sep = ':'), sep = ' '),
                                              format = '%Y-%m-%d %H:%M:%OS'),
                        x, y)]
  DT_bog2 <- setDT(sfheaders::sf_to_df(out_bog2, fill = T))
  DT_bog2 <- DT_bog2[,.(id = AnimalID, Fix_Status = NAV, Range = CaptureRan,
                        datetime = as.POSIXct(paste(paste(Year, Month, Day, sep = '-'),
                                                    paste(Hour, 00, 00, sep = ':'), sep = ' '),
                                              format = '%Y-%m-%d %H:%M:%OS'),
                        x, y)]

  DT_william <- setDT(sfheaders::sf_to_df(william, fill = T))
  DT_william <- DT_william[,.(id = Animal_ID, Fix_Status = NumSats, Range,
                              datetime = as.POSIXct(paste(paste(Year, match(Month, month.name), sprintf("%02d", Day), sep = '-'),
                                                          paste(sprintf("%02d", Hour), sprintf("%02d", Minute), 00, sep = ':'), sep = ' '),
                                                    format = '%Y-%m-%d %H:%M:%OS'),
                              x=-1 *x, y)]
  DT_wabowden1 <- setDT(sfheaders::sf_to_df(out_wabowden1, fill = T))
  DT_wabowden1 <- DT_wabowden1[,.(id = AnimalID, Fix_Status = NAV, Range = CaptureRan,
                                  datetime = as.POSIXct(paste(paste(Year, Month, Day, sep = '-'),
                                                              paste(Hour, 00, 00, sep = ':'), sep = ' '),
                                                        format = '%Y-%m-%d %H:%M:%OS'),
                                  x, y)]
  DT_wabowden2 <- setDT(sfheaders::sf_to_df(out_wabowden2, fill = T))
  DT_wabowden2 <- DT_wabowden2[,.(id = Unique_id, Fix_Status = NAV, Range = Population,
                                  datetime = as.POSIXct(paste(paste(Year, sprintf("%02d", Month), sprintf("%02d", Day), sep = '-'),
                                                              paste(sprintf("%02d", Hour), sprintf("%02d", Minute), sprintf("%02d", Second), sep = ':'), sep = ' '),
                                                        format = '%Y-%m-%d %H:%M:%OS'),
                                  x, y)]
  DT_wabowden3 <- setDT(sfheaders::sf_to_df(out_wabowden3, fill = T))
  DT_wabowden3 <- DT_wabowden3[,.(id = AnimalID, Fix_Status = NAV, Range = CaptureRan,
                                  datetime = as.POSIXct(paste(paste(Year, sprintf("%02d", Month), sprintf("%02d", Day), sep = '-'),
                                                              paste(sprintf("%02d", Hour), 00, 00, sep = ':'), sep = ' '),
                                                        format = '%Y-%m-%d %H:%M:%OS'),
                                  x, y)]

  DT_kamuchawie <- setDT(sfheaders::sf_to_df(out_kamuchawie, fill = T))
  DT_kamuchawie <- DT_kamuchawie[,.(id = Collar_ID, Fix_Status = Fix_Type, Range = 'Kamuchawie',
                                    datetime = as.POSIXct(Acq__Time__UTC_, format = '%Y-%m-%d %H:%M:%OS'),
                                    x, y)]

  mb.dt <- rbind(DT_ATIKO, DT_BERENS_RND, DT_BLDVN, DT_bog1, DT_bog2, DT_charron1, DT_charron2,
                 DT_flintstone, DT_harding1, DT_harding2, DT_interlake, DT_kississing, DT_MBhydro,
                 DT_MBhydro, DT_MI_Berens, DT_MI_NWH, DT_naosap1, DT_naosap2, DT_naosap3,
                 DT_wabowden1, DT_wabowden2, DT_wabowden3, DT_wheadon1, DT_wheadon2, DT_wheadon3,
                 DT_william, DT_wimwap1, DT_wimwap2, DT_kamuchawie)

  # right now quick and dirty way to deal with outliers
  dat_cleaner <- mb.dt[complete.cases(x,y, datetime)]
  booMB <- dat_cleaner

  return(booMB)
}

dataPrep_ON <- function(dPath = dPath) {
  raw.on <- dPath

  on.pts <- st_read(file.path(raw.on, 'BorealCaribou_NHICMedSensitive2025.gdb.zip'), "Species_TRK_Pts")

  outcrs <- st_crs(3978)

  out_on.pts <- st_transform(on.pts, outcrs)

  on.dt <- setDT(sfheaders::sf_to_df(out_on.pts, fill = T))

  #this needs to be ont custom
  on.dt <- on.dt[,.(id = IDENT, LOCATION_FIX_TYPE, Range = "Ontario", datetime = as.POSIXct(MONITOR_DATETIME,
                                                                                            tz = 'gmt', format = '%Y-%m-%d %H:%M'),
                    x, y)]

  dat_cleaner <- on.dt[complete.cases(x,y, datetime)]
  # check for duplicated time stamps
  dat_cleaner[,any(duplicated(datetime)), by = id]
  booON <- dat_cleaner

  return(booON)

}
