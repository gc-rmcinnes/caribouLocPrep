defineModule(sim, list(
  name = "caribouLocPrep",
  description = paste0("This module downloads and cleansup caribou collar data",
                       "from several provinces and Territories in Canada."),
  keywords = c("Caribou","Location",""),
  authors = c(person("Julie", "Tuner", email = "", role = c("aut", "cre")),
              person("Rory", "McInnes", email = "", role = c("aut", "cre")),
              person("Eliot", "McIntire", email = "", role = c("aut", "cre")),
              person("Tati", "Micheletti", email = "tati.micheletti@gmail.com",
                     role = c("aut"))),
  childModules = character(0),
  version = list(caribouLocPrep = "0.0.0.9000"),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("NEWS.md", "README.md", "caribouLocPrep.Rmd"),
  reqdPkgs = c("SpaDES.core (>= 2.1.5.9002)",
               "ggplot2", 'data.table', 'readxl', 'fs', 'tidyr',
               'purrr', 'janitor', 'raster','sf','googledrive', 'sfheaders', 'amt', 'Require',
               'reproducible', 'move2','dplyr', 'terra', "rnaturalearth"
  ), # TODO Double check you REALLY use all packages. Simplify as much as possible!
  parameters = bindrows(
    defineParameter("urlToBCDataFolder", "character",
                    "https://drive.google.com/drive/u/0/folders/1spylo1XDqYO1SmvRXfzhIo3v6rsSryZs", NA, NA,
                    desc = "The URL to a folder where BC data is kept"),
    defineParameter("urlToSKDataFolder", "character",
                    "https://drive.google.com/drive/folders/18jdIk_62S73PRe1ZtX0JXxp1pxxbFPaX", NA, NA,
                    desc = "The URL to a folder where SK data is kept"),
    defineParameter("urlToMBDataFolder", "character",
                    "https://drive.google.com/drive/folders/1hdTIcpiMueewC1DZsJE1gW8R8mdG7CkF", NA, NA,
                    desc = "The URL to a folder where MB data is kept"),
    defineParameter("urlToONDataFolder", "character",
                    "https://drive.google.com/drive/u/0/folders/1pivxW9sNOvoVNecsDH-rTuqP6WyI1XTF", NA, NA,
                    desc = "The URL to a folder where ON data is kept"),
    defineParameter("NWTMovebankID", "character",
                    "", NA, NA,
                    desc = "The MoveBank study ID where NTW is kept"),
    defineParameter("YTMovebankID", "character",
                    "3023885998", NA, NA,
                    desc = "The MoveBank study ID where YT data is kept"),
    defineParameter("MoveBankUser", "character",
                    Sys.getenv("MOVEBANK_USERNAME"), NA, NA,
                    desc = "The MoveBank user ID to access studies stored on MoveBank"),
    defineParameter("MoveBankPass", "character",
                    Sys.getenv("MOVEBANK_PASSWORD"), NA, NA,
                    desc = "The MoveBank user password to access studies stored on MoveBank"),
    defineParameter("jurisdiction", "character",c("BC","SK","MB", "ON", "NT", "YT"),
                    desc = "A list of jurisdictions to run"),
    defineParameter("herdNT", "character",
                    c('Dehcho Boreal Woodland Caribou', 'North Slave Boreal Caribou',
                      'Sahtu Boreal Woodland Caribou', 'Sahtu Boreal Woodland Caribou (2020)',
                      'South Slave Boreal Woodland Caribou'),
                    desc = "A list of specific herds from NT to download data from"),
    defineParameter(".plots", "character", "screen", NA, NA,
                    "Used by Plots function, which can be optionally used here"),
    defineParameter(".plotInitialTime", "numeric", start(sim), NA, NA,
                    "Describes the simulation time at which the first plot event should occur."),
    defineParameter(".plotInterval", "numeric", NA, NA, NA,
                    "Describes the simulation time interval between plot events."),
    defineParameter(".saveInitialTime", "numeric", NA, NA, NA,
                    "Describes the simulation time at which the first save event should occur."),
    defineParameter(".saveInterval", "numeric", NA, NA, NA,
                    "This describes the simulation time interval between save events."),
    defineParameter(".studyAreaName", "character", NA, NA, NA,
                    "Human-readable name for the study area used - e.g., a hash of the study",
                    "area obtained using `reproducible::studyAreaName()`"),
    ## .seed is optional: `list('init' = 123)` will `set.seed(123)` for the `init` event only.
    defineParameter(".seed", "list", list(), NA, NA,
                    "Named list of seeds to use for each event (names)."),
    defineParameter(".useCache", "logical", FALSE, NA, NA,
                    "Should caching of events or module be used?"),
    defineParameter("rangeBuffer", "numeric", 100000, NA, NA,
                    paste0("What buffer do you want for getting the extent of",
                           "collars chosen? Defaults to 100km (in meters)"))
  ),
  inputObjects = bindrows(
    #expectsInput("objectName", "objectClass", "input object description", sourceURL, ...),
    expectsInput(objectName = "boo", objectClass = "list",
                 desc = "List of data.table objects with raw caribou GPS information, per province", sourceURL = NA),
    expectsInput(objectName = "caribouLoc", objectClass = "list",
                 desc = "List of data.table objects with processed caribou GPS information, per province", sourceURL = NA)
  ),
  outputObjects = bindrows(
    #createsOutput("objectName", "objectClass", "output object description", ...)
    createsOutput(objectName = "caribouLoc", objectClass = "data.table",
                  desc = "Harmonized and cleaned caribou locations of all jurisdictions provided"),
    # createsOutput(objectName = "boo", objectClass = "list",
    #               desc = paste0("List of data.table objects with caribou GPS",
    #                             "information, per province, created in",
    #                             ".inputObjects if not provided")),
    createsOutput(objectName = "studyArea", objectClass = "SpatVector",
                  desc = "a single polygon derived from the full extent of caribou locations")

  )
))

doEvent.caribouLocPrep = function(sim, eventTime, eventType) {
  switch(
    eventType,
    init = {
      browser()
      # Test if user has movebank info IF it is running for data that is there
      if (any("YT" %in% Par$jurisdiction,
              "NT" %in% Par$jurisdiction)){
        if (Par$MoveBankUser == "" || Par$MoveBankPass == "") {
          errorMessage <- paste(
            "Error: You requested data hosted in MoveBank, but Movebank credentials were not found.",
            "To solve this, please follow these steps in your R console:",
            "1. Install the 'usethis' package if you haven't already:",
            "   install.packages('usethis')",
            "2. Run the following command. It will open a special file named '.Renviron' for editing:",
            "   usethis::edit_r_environ()",
            "3. Add the following two lines to the '.Renviron' file, replacing the placeholders",
            "   with your actual Movebank username and password:",
            "   MOVEBANK_USERNAME=\"your_username_here\"",
            "   MOVEBANK_PASSWORD=\"your_password_here\"",
            "4. Save the '.Renviron' file and **IMPORTANT: Restart your R session**.",
            "   (In RStudio: Session > Restart R or Ctrl+Shift+F10)",
            "After restarting, re-run this script.",
            "ALTERNATIVELY, simply pass both `MoveBankPass` and `MoveBankUser` as parameters to the module.",
            sep = "\n"
          )
          stop(errorMessage)
        }
      }
      # sim <- scheduleEvent(sim, time(sim), "caribouLocPrep", "downloadData")
      # sim <- scheduleEvent(sim, time(sim), "caribouLocPrep", "createFullExtent")

    },
    downloadData = {
      # run data harmonization
      sim$caribouLoc <- downloadDataAndHarmonize(jurisdiction = Par$jurisdiction,
                                                 boo = sim$boo)
    },
    createFullExtent = {
      # create study area buffered
      sim$studyArea <- Cache(studyareaFullextent, dat.clean = sim$caribouLoc,
                             rangeBuffer = Par$rangeBuffer)
      sim$studyArea <- terra::vect(sim$studyArea)
    },
    warning(noEventWarning(sim))
  )
  return(invisible(sim))
}

.inputObjects <- function(sim) {
  dPath <- asPath(getOption("reproducible.destinationPath", dataPath(sim)), 1)
  message(currentModule(sim), ": using dataPath '", dPath, "'.")
  #run the jurisdictional data prep scripts for the supplied jurisdictions
  if (!suppliedElsewhere("boo", sim = sim)) {
    sim$boo <- list()
    if ("BC" %in% Par$jurisdiction) {
      #download just the .gdb file from google drive
      layers <- list("Regional_Telemetry_20211026", "KMB_Local_Telemetry_20211026")
      telemDataZipFile <- "telem_data_request_20211026.gdb.zip"
      bc_layers <- list()
      bc_layers <- Map(layer = layers, function(layer) {
        prepInputs(url = Par$urlToBCDataFolder,
                   targetFile = telemDataZipFile, useCache = FALSE,
                   layer = layer,
                   destinationPath = dPath, fun = terra::vect(x = targetFile, layer = layer))
      })
      sim$boo[["BC"]] <- prepInputs(destinationPath = dPath,
                                    fun = dataPrep_BC(dPath=dPath, bc_layers, layers))
    }
    if ("SK" %in% Par$jurisdiction){
      #download the spreadsheets of points
      sim$boo[["SK"]] <- prepInputs(url = Par$urlToSKDataFolder,
                                    destinationPath = dPath,
                                    fun = dataPrep_SK(dPath = dPath))
    }
    if ("MB" %in% Par$jurisdiction){
      #download the .gdb files (there are many, this prepInputs takes time)
      sim$boo[["MB"]] <- prepInputs(url = Par$urlToMBDataFolder,
                                    destinationPath = dPath,
                                    archive = NA,
                                    fun = dataPrep_MB(dPath = dPath))
    }
    if ("ON" %in% Par$jurisdiction){
      #download the ontario data
      ontario_zip <- "BorealCaribou_NHICMedSensitive2025.gdb.zip"
      sim$boo[["ON"]] <- prepInputs(url = Par$urlToONDataFolder,
                                    targetFile = ontario_zip,
                                    destinationPath = dPath,
                                    archive = NA,
                                    fun = dataPrep_ON(dPath = dPath))
    }
    if ("YT" %in% Par$jurisdiction){
      #the login required to access data on Move Bank for YT and NWT
      loginStored <- move2::movebank_store_credentials(username=Par$MoveBankUser,
                                                       password=Par$MoveBankPass)
      #use movebank to access the data
      message("Access to YT data requires a Move Bank account, ensure you have collaboration rights to the study")
      sim$boo[["YT"]] <- dataPrep_YT(loginStored)
    }
    if ("NT" %in% Par$jurisdiction){
      #the login required to access data on Move Bank for YT and NWT
      loginStored <- move2::movebank_store_credentials(username=Par$MoveBankUser,
                                                       password=Par$MoveBankPass)
      #use movebank to access the data
      message("Access to NT data requires a Move Bank account, ensure you have collaboration rights to the study")
      sim$boo[["NT"]] <- Cache(dataPrep_NT, loginStored, herds = Par$herdNT)
    }
  }
  return(invisible(sim))
}
