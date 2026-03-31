spaceless <- function(x) {colnames(x) <- gsub(" ", "", colnames(x));x}

#i think breaking these out into individual scripts would work well and have cleaner module code
dataPrep_SK <- function(dPath =dpath) {
  ### Input data
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
  ### Input data
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
    nt.move[[ds]] <- Cache(move2::movebank_download_study, study = as.integer(study[ds]),
                                           login = loginStored, removeDuplicatedTimestamps=TRUE)
  }

  nt <- rbindlist(lapply(1:length(hab), function(hh){
    # First, let's work with the specific move2 object.
    moveTable <- nt.move[[hh]]

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
  booNT[, id := as.character(id)]
  return(booNT)
}

dataPrep_YT <- function(loginStored) {
  ### Input data
  dat <- Cache(move2::movebank_download_study, study_id = "Boreal Caribou - Yukon Collars",
               login = loginStored, removeDuplicatedTimestamps=TRUE, progress = TRUE)
  ### Prep data
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

  ### standarize names and columns
  booYT <- boo_notclean[, .(id = individual_local_identifier, datetime, x, y, argos_altitude, gps_fix_type_raw)]
  booYT[, id := as.character(id)]

  ### EXPLORE
  # check if all observations are complete
  booComplete <- all(complete.cases(booYT[,.(x,y, datetime)]))
  return(booYT)
}

dataPrep_MB <- function(dPath = dPath) {
  ### input data
  raw.mb <- dPath
  outcrs <- st_crs(3978)
  # Read and transform all input layers

  layer_defs <- list(
    MI_Berens   = c("CaribouGPSData_AtikakiBerens.gdb.zip","MI_Berens_Caribou_2020APR02_CLEAN_NAD83_Z14"),
    BERENS_RND  = c("CaribouGPSData_AtikakiBerens.gdb.zip","BERENS_RND_GPS_2001_to_2004_complete_NAD83Z14"),
    BLDVN       = c("CaribouGPSData_AtikakiBerens.gdb.zip","BLDVN_2000_to_2014_complete_NAD83Z14"),
    ATIKO       = c("CaribouGPSData_AtikakiBerens.gdb.zip","ATIKO_2000_to_2014_complete_NAD83Z14"),

    interlake   = c("CaribouGPSData_Interlake.gdb.zip","Interlake_Total_Jan2021"),

    MI_NWH      = c("CaribouGPSdata_Molson.gdb.zip","MI_NWH_Caribou_Telemetry2020APR02_Clean_NAD83"),
    charron1    = c("CaribouGPSdata_Molson.gdb.zip","CharronLK_GPS_MBHYDRO_Q2_2010_072020"),
    charron2    = c("CaribouGPSdata_Molson.gdb.zip","CharronLK_GPS_MBHYDRO_Q4_2010_022021"),

    MBhydro     = c("CaribouGPSdata_NaosapReed.gdb.zip","MBHYDRO_Q2_2010_072020"),
    naosap1     = c("CaribouGPSdata_NaosapReed.gdb.zip","Naosap_Reed_Total_NAD83_July2018"),
    naosap2     = c("CaribouGPSdata_NaosapReed.gdb.zip","gps_naosap_2002_06"),
    naosap3     = c("CaribouGPSdata_NaosapReed.gdb.zip","NaosapReed_MBHYDRO_Q4_2010_022021"),
    kississing  = c("CaribouGPSdata_NaosapReed.gdb.zip","Kississing_Total_NAD83_July2018"),

    flintstone  = c("CaribouGPSdata_Owl_Flintstone.gdb.zip","Owl_Flintstone_1995_2018_complete_WGS84"),

    wimwap1     = c("CaribouGPSData_PartridgeCrop.gdb.zip","Wim_Wap_GPS_MBHydro_Q2_2010_072020"),
    wimwap2     = c("CaribouGPSData_PartridgeCrop.gdb.zip","Wim_Wap_GPS_MBHYDRO_Q4_2010_022021"),
    harding1    = c("CaribouGPSData_PartridgeCrop.gdb.zip","Harding_GPS_MBHydro_Q2_2010_072020"),
    harding2    = c("CaribouGPSData_PartridgeCrop.gdb.zip","Harding_GPS_MBHYDRO_Q4_2010_022021"),
    wheadon1    = c("CaribouGPSData_PartridgeCrop.gdb.zip","Wheadon_GPS_MBHydro_Q2_2010_072020"),
    wheadon2    = c("CaribouGPSData_PartridgeCrop.gdb.zip","Wheadon_Total_NAD83_Nov2018_Final"),
    wheadon3    = c("CaribouGPSData_PartridgeCrop.gdb.zip","Wheadon_GPS_MBHYDRO_Q4_2010_022021"),

    bog1        = c("CaribouGPSdata_TheBog.gdb.zip","TheBog_MH_2010_072020"),
    bog2        = c("CaribouGPSdata_TheBog.gdb.zip","TheBog_MBHYDRO_Q4_2010_022021"),

    william     = c("CaribouGPSdata_Wabowden.gdb.zip","William_Lake_Total_NAD83_Dec2019"),
    wabowden1   = c("CaribouGPSdata_Wabowden.gdb.zip","Wabowden_GPS_MH_Q2_2010_072020"),
    wabowden2   = c("CaribouGPSdata_Wabowden.gdb.zip","Wabowden_UHF_relocations_2009to2012_NAD83"),
    wabowden3   = c("CaribouGPSdata_Wabowden.gdb.zip","Wabowden_GPS_MBHYDRO_Q4_2010_022021"),

    kamuchawie  = c("CaribouGPSData_Kamuchawie.gdb.zip","Positions2025Jun12_Kamuchawie")
  )

  sf_list <- vector("list", length(layer_defs))
  names(sf_list) <- names(layer_defs)

  for (nm in names(layer_defs)) {
    f <- file.path(raw.mb, layer_defs[[nm]][1])
    lyr <- layer_defs[[nm]][2]
    sf_list[[nm]] <- st_transform(st_read(f, lyr, quiet = TRUE), outcrs)
  }

  # Convert to data.table

  dt_list <- lapply(sf_list, \(x) setDT(sfheaders::sf_to_df(x, fill = TRUE)))

  # checking for right formats and grabbing what need

  DT_MI_Berens <- dt_list$MI_Berens[,.(id = Animal_ID, Fix_Status, Range,
                                       datetime = as.POSIXct(paste(paste(GMT_Year,GMT_Month,GMT_Day,sep='-'),
                                                                   paste(GMT_Hour,GMT_Minute,sep=':'),sep=' '),
                                                             tz='gmt', format='%Y-%m-%d %H:%M'), x,y)]

  DT_BERENS_RND <- dt_list$BERENS_RND[,.(id = ANIMAL_ID, Fix_Status = FIX_STATUS, Range = RANGE,
                                         datetime = as.POSIXct(paste(paste(YEAR,MONTH,DAY,sep='-'), TIME, sep=' '),
                                                               format='%Y-%m-%d %H:%M:%OS'), x,y)]

  DT_BLDVN <- dt_list$BLDVN[,.(id = ANIMAL_ID, Fix_Status = FIX_STATUS, Range = RANGE,
                               datetime = as.POSIXct(paste(gsub(" .*$","",DATE), TIME, sep=' '),
                                                     format='%Y-%m-%d %H:%M:%OS'), x,y)]

  DT_ATIKO <- dt_list$ATIKO[,.(id = ANIMAL_ID, Fix_Status = FIX_STATUS, Range = RANGE,
                               datetime = as.POSIXct(paste(paste(YEAR,MONTH,DAY,sep='-'), TIME, sep=' '),
                                                     format='%Y-%m-%d %H:%M:%OS'), x,y)]

  DT_interlake <- dt_list$interlake[,.(id = Animal_ID, Fix_Status, Range,
                                       datetime = as.POSIXct(paste(paste(Year,Month,Day,sep='-'),
                                                                   paste(Hour,Minute,Second,sep=':'),sep=' '),
                                                             format='%Y-%m-%d %H:%M:%OS', tz='America/Chicago'), x,y)]

  DT_MI_NWH <- dt_list$MI_NWH[,.(id = Animal_ID, Fix_Status, Range = RANGE,
                                 datetime = as.POSIXct(paste(paste(GMT_Year,GMT_Month,GMT_Day,sep='-'),
                                                             paste(GMT_Hour,GMT_Minute,sep=':'),sep=' '),
                                                       tz='gmt', format='%Y-%m-%d %H:%M'), x,y)]

  # repeated pattern datasets
  nav_sets <- c("charron1","charron2","MBhydro","naosap3","wimwap1","wimwap2",
                "harding1","harding2","wheadon1","wheadon3","bog1","bog2",
                "wabowden1","wabowden3")

  for (nm in nav_sets) {
    assign(paste0("DT_",nm),
           dt_list[[nm]][,.(id = AnimalID, Fix_Status = NAV, Range = CaptureRan,
                            datetime = as.POSIXct(paste(paste(Year,Month,Day,sep='-'),
                                                        paste(Hour,0,0,sep=':'),sep=' '),
                                                  format='%Y-%m-%d %H:%M:%OS'), x,y)])
  }

  DT_naosap1 <- dt_list$naosap1[,.(id = Animal_ID, Fix_Status = NumSats, Range,
                                   datetime = as.POSIXct(paste(paste(Year,match(Month,month.name),sprintf("%02d",Day),sep='-'),
                                                               paste(sprintf("%02d",Hour),sprintf("%02d",Minute),0,sep=':'),sep=' '),
                                                         format='%Y-%m-%d %H:%M:%OS'), x,y)]

  DT_naosap2 <- dt_list$naosap2[,.(id = UNIQUE_ID, Fix_Status = FIX_STATUS, Range = RANGE,
                                   datetime = as.POSIXct(paste(paste(YEAR,MONTH,DAY,sep='-'),
                                                               paste(HOUR,MINUTE,SECOND,sep=':'),sep=' '),
                                                         format='%Y-%m-%d %H:%M:%OS'), x,y)]

  DT_kississing <- dt_list$kississing[,.(id = Animal_ID, Fix_Status = NumSats, Range,
                                         datetime = as.POSIXct(paste(paste(Year,match(Month,month.name),sprintf("%02d",Day),sep='-'),
                                                                     paste(sprintf("%02d",Hour),sprintf("%02d",Minute),0,sep=':'),sep=' '),
                                                               format='%Y-%m-%d %H:%M:%OS'), x=-1*x, y)]

  DT_flintstone <- dt_list$flintstone[,.(id = ANIMAL_ID, Fix_Status = FIX_STATUS, Range,
                                         datetime = as.POSIXct(paste(paste(YEAR,MONTH,DAY,sep='-'), TIME, sep=' '),
                                                               format='%Y-%m-%d %H:%M:%OS'), x,y)]

  DT_wheadon2 <- dt_list$wheadon2[,.(id = Animal_ID, Fix_Status = NumSats, Range,
                                     datetime = as.POSIXct(paste(paste(Year,match(Month,month.name),sprintf("%02d",Day),sep='-'),
                                                                 paste(sprintf("%02d",Hour),sprintf("%02d",Minute),0,sep=':'),sep=' '),
                                                           format='%Y-%m-%d %H:%M:%OS'), x,y)]

  DT_william <- dt_list$william[,.(id = Animal_ID, Fix_Status = NumSats, Range,
                                   datetime = as.POSIXct(paste(paste(Year,match(Month,month.name),sprintf("%02d",Day),sep='-'),
                                                               paste(sprintf("%02d",Hour),sprintf("%02d",Minute),0,sep=':'),sep=' '),
                                                         format='%Y-%m-%d %H:%M:%OS'), x=-1*x, y)]

  DT_wabowden2 <- dt_list$wabowden2[,.(id = Unique_id, Fix_Status = NAV, Range = Population,
                                       datetime = as.POSIXct(paste(paste(Year,sprintf("%02d",Month),sprintf("%02d",Day),sep='-'),
                                                                   paste(sprintf("%02d",Hour),sprintf("%02d",Minute),sprintf("%02d",Second),sep=':'),sep=' '),
                                                             format='%Y-%m-%d %H:%M:%OS'), x,y)]

  DT_kamuchawie <- dt_list$kamuchawie[,.(id = Collar_ID, Fix_Status = Fix_Type, Range = 'Kamuchawie',
                                         datetime = as.POSIXct(Acq__Time__UTC_, format='%Y-%m-%d %H:%M:%OS'), x,y)]

  # Bind and clean

  mb.dt <- rbindlist(mget(ls(pattern="^DT_")), use.names = TRUE, fill = TRUE)

  dat_cleaner <- mb.dt[complete.cases(x,y,datetime)]
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
