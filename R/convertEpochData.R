convertEpochData = function(datadir = c(), outputdir = c(), data_format = "raw", tz = "Europe/London", epochSize = 60) {
  
  fnames = dir(datadir,full.names = TRUE,pattern = "[.]csv")
  chartime2iso8601 = function(x,tz){
    POStime = as.POSIXlt(as.numeric(as.POSIXlt(x, tz)), origin = "1970-1-1", tz)
    POStimeISO = strftime(POStime, format = "%Y-%m-%dT%H:%M:%S%z")
    return(POStimeISO)
  }
  POSIXtime2iso8601 = function(x, tz) {
    POStime = as.POSIXlt(x,tz) #turn to right timezone
    POStime_z = chartime2iso8601(as.character(POStime),tz) #change format
    return(POStime_z)
  }
  
  if (data_format == "ukbiobank_csv") {
    deviceName = "Axivity"
    monn = "axivity"
    monc = 4
    dformc = 4
    dformn = "cwa"
    sf = 100
    epochSize = 5
  } else if (length(grep(pattern = "actiwatch", x = data_format, ignore.case = TRUE)) > 0) {
    deviceName = "Actiwatch"
    monn = "actiwatch"
    monc = 7
    dformc = 99
    dformn = "epochdata"
    sf = 100 # <= EXTRACT FROM FILE?
  }
  
  # create generic template data
  C = list(cal.error.end = 0, cal.error.start = 0)
  C$scale = c(1, 1, 1)
  C$offset = c(0, 0, 0)
  C$tempoffset =  c(0, 0, 0)
  C$QCmessage = "Autocalibration not done"
  C$npoints = 0
  C$nhoursused = 0
  C$use.temp = TRUE
  M = list(filecorrupt = FALSE, filetooshort = FALSE,
           metalong = data.frame(
             timestamp = rep(0, 3),
             nonwearscore = rep(0, 3),
             clippingscore = rep(0, 3),
             lightmean = rep(0, 3),
             lightpeak = rep(0, 3),
             temperaturemean = rep(0, 3),
             EN = rep(0, 3)
           ),
           metashort = data.frame(timestamp = rep(0, 3),
                                  ENMO = rep(0, 3)), 
           wday = 0,
           wdayname = "Sunday",
           windowsizes = c(5, 900, 3600),
           bsc_qc = data.frame(A = 1:3,
                               B = 1:3))
  
  dummyheader = data.frame(uniqueSerialCode = 0, 
                           frequency = 100,
                           start = "1980-01-01 19:08:00",
                           device = deviceName,
                           firmwareVersion = "unknown",
                           block = 0)
  dummyheader = t(dummyheader)
  colnames(dummyheader) = "value"
  I = list(
    header = dummyheader,
    monc = monc,
    monn = monn,
    dformc = dformc,
    dformn = dformn,
    sf = sf,
    filename = "unknown")
  
  filefoldername = NA
  for (i in 1:length(fnames)) { # loop over files
    # filename
    fname = basename(fnames[i])
    I$filename = filename_dir = fname
    if (data_format == "ukbiobank_csv") {
      # read data
      D = utils::read.table(file = fnames[i],
                     header = TRUE,
                     sep = ",")
      header = as.character(read.table(file = fnames[i], header = FALSE, nrows = 1, sep = ",")[1, 1])
      # extract date/timestamp from fileheader
      # header = as.character(colnames(D)[1])
      timestamp = unlist(strsplit(header," - "))[2]
      # define start time of the 15 minute intervals
      # like in the default GGIR version
      timestamp_POSIX = as.POSIXlt(timestamp, tz = tz)
      
    } else if (length(grep(pattern = "actiwatch", x = data_format, ignore.case = TRUE)) > 0) {
      if (data_format == "actiwatch_csv") {
        # ! Assumptions that timeseries start before line 150
        ind = 150
        testraw = data.table::fread(input = fnames[i],
                                    header = FALSE, sep = ",", skip = ind,
                                    nrows = 1, data.table = TRUE)
        # ! Assumption that first column are the epoch numbers
        delta = 1 - testraw$V1
        ind = ind + delta
        D = data.table::fread(input = fnames[i],
                                 header = FALSE, sep = ",", skip = ind)
        # ! Assumption that column names are present 2 lines prior to timeseries
        colnames = data.table::fread(input = fnames[i],
                                     header = FALSE, sep = ",",
                                     skip = ind - 2, nrows = 1)
        colnames(D) = as.character(colnames)[1:ncol(D)]
        # ! Assumptions about columns names
        colnames(D) = gsub(pattern = "datum|date", replacement = "date", x = colnames(D), ignore.case = TRUE)
        colnames(D) = gsub(pattern = "tijd|time", replacement = "time", x = colnames(D), ignore.case = TRUE)
        colnames(D) = gsub(pattern = "activiteit|activity", replacement = "activity", x = colnames(D), ignore.case = TRUE)
        
        timestamp_POSIX = as.POSIXct(x = paste(D$date[1], D$time[1], sep = " "), format = "%d-%m-%Y %H:%M:%S", tz = tz)
        D = D[, "activity"]
      }
    }
    Sys.setlocale("LC_TIME", "C") # set language to English
    quartlystart = (ceiling(as.numeric(timestamp_POSIX) / 900)) * 900
    
    ts_15min_POSIX = as.POSIXlt(quartlystart, tz = tz, origin = "1970-01-01") # POSIX time
    M$wdayname = weekdays(x = ts_15min_POSIX, abbreviate = FALSE) # extract weekday as day name
    M$wday = ts_15min_POSIX$wday # extract weekday as a daynumber
    
    # calculate time difference relative to original data
    # to know how much epoch to ignore at the beginning of the file
    deltastart = as.numeric(difftime(ts_15min_POSIX, timestamp_POSIX,units = "sec") / 5)
    # shorten the data (object D) accordingly
    D = D[(deltastart + 1):nrow(D),]
    #create timeseries for metashort
    
    timeseries_shortEpoch_num = seq(quartlystart, quartlystart + ((nrow(D) - 1) * epochSize), by = epochSize)
    timeseries_15min_num = seq(quartlystart, quartlystart + ((nrow(D) - 1) * epochSize), by = 900)
    timeseries_shortEpoch_8601 = POSIXtime2iso8601(as.POSIXlt(timeseries_shortEpoch_num, tz = tz,
                                                        origin = "1970-01-01"),
                                             tz = tz)
    timeseries_15min_8601 = POSIXtime2iso8601(as.POSIXlt(timeseries_15min_num, tz = tz, origin = "1970-01-01"),
                                              tz = tz)
    M$metashort = data.frame(timestamp = timeseries_shortEpoch_8601,ENMO = D[1:nrow(D),1]/1000,stringsAsFactors = FALSE)
    LML = length(timeseries_15min_8601)
    if (data_format == "ukbiobank_csv") {
      #Collapse second column of input data to use as non-wear score
      imp = D[, 2]
    } else if (length(grep(pattern = "actiwatch", x = data_format, ignore.case = TRUE)) > 0) {
      imp = unlist(D[, 1])
    }
    navalues = which(is.na(imp) == TRUE)
    if (length(navalues) > 0) imp[navalues] = 1
    
    
    if (data_format == "ukbiobank_csv") {
      # Take 15 minute mean of UK Biobank based invalid data indicater per 5 seconds
      imp2 = cumsum(imp)
      imp3 = diff(imp2[seq(1, length(imp2), by = ((60/epochSize) * 15))]) / ((60/epochSize) * 15) # rolling mean
      imp4 = round(imp3 * 3) # create three level nonwear score from it, not really necessary for GGIR, but helps to retain some of the information
    } else if (length(grep(pattern = "actiwatch", x = data_format, ignore.case = TRUE)) > 0) {
      # Using rolling 60 minute sum to indicate whether it is nonwear
      imp2 = zoo::rollapply(imp, width = 3600 / epochSize, FUN = sum, fill = 0)
      imp4 = imp2
      imp4[which(imp2 > 0)] = 0
      imp4[which(imp2 == 0)] = 3 # If rolling average is zero then consider it nonwear
      imp5 = cumsum(imp4)
      step = (60/epochSize) * 15
      imp4 = diff(imp5[seq(1, length(imp5) + step, by = step)]) / step # rolling mean
      if (length(imp4) > length(timeseries_15min_8601)) imp4 = imp4[1:length(timeseries_15min_8601)]
    }
    
    if (length(imp4) < LML) {
      imp4 = c(imp4, rep(0, LML - length(imp4)))
    }
    # create data.frame for metalong, note that light and temperature are just set at zero
    M$metalong = data.frame(timestamp = timeseries_15min_8601,nonwearscore = imp4, #rep(0,LML)
                            clippingscore = rep(0,LML), lightmean = rep(0,LML),
                            lightpeak = rep(0,LML), temperaturemean = rep(0,LML), EN = rep(0,LML))
    
    # Save these files as new meta-file
    save(M, C, I, filename_dir, filefoldername,
         file = paste0(outputdir, "/meta_", fname, ".RData"))
  }
}