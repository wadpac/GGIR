convertEpochData = function(datadir = c(), studyname = c(), outputdir = c(),
                            params_general = c()) {
  # Function to convert externally derived epoch data
  # to a format that allows GGIR to process them as if they were part 1 output.
  
  # Capture current local settings
  Syslocale = Sys.getenv("LC_TIME")
  Sys.setlocale("LC_TIME", "C") # set language to English
  tz = params_general[["desiredtz"]]
  epSizeShort = params_general[["windowsizes"]][1]
  epSizeLong = params_general[["windowsizes"]][2]
  # Identify input data file extensions
  if (dir.exists(datadir) == FALSE) {
    stop("\nWhen working with external data, argument datadir is expected to be a directory")
  }
  fnames_csv = dir(datadir,full.names = TRUE,pattern = "[.]csv")
  fnames_awd = dir(datadir,full.names = TRUE,pattern = "[.]awd|[.]AWD")
  if (length(fnames_csv) > 0 & length(fnames_awd) > 0) {
    stop("Do not mix csv and awd files in the same data directory")
  } else {
    if (length(fnames_awd) > 0) {
      if (params_general[["dataFormat"]] != "actiwatch_awd") {
        stop("Specified dataFormat does not match the data")
      }
      fnames = fnames_awd
    } else {
      if (params_general[["dataFormat"]] == "actiwatch_awd") {
        stop("Specified dataFormat does not match the data")
      }
      fnames = fnames_csv
    }
  }
  #-------------
  # Create output folder, normally with raw data g.part1 would do this:
  filelist = isfilelist(datadir)
  # create output directory if it does not exist
  if (filelist == TRUE) {
    if (length(studyname) == 0) {
      studyname = "mystudy"
      stop('\nError: studyname not specified in part1. Needed for analysing lists of files')
    } else {
      outputfolder = paste0("/output_", studyname)
    }
  } else {
    outputfolder = unlist(strsplit(datadir, "/"))
    outputfolder = paste0("/output_",outputfolder[length(outputfolder)])
  }
  if (!file.exists(paste0(outputdir, outputfolder))) {
    dir.create(file.path(outputdir, outputfolder))
    dir.create(file.path(outputdir, outputfolder, "meta"))
    dir.create(file.path(outputdir, paste0(outputfolder,"/meta"), "basic"))
    dir.create(file.path(outputdir, outputfolder, "results"))
    dir.create(file.path(outputdir, paste0(outputfolder, "/results"), "QC"))
  }
  
  outputdir = paste0(outputdir, outputfolder) #where is output stored?
  
  #============
  # Based on knowledge about data format
  # we can already assign monitor names and codes
  # dataformat names and codes
  # and sample rate
  if (params_general[["dataFormat"]] == "ukbiobank_csv") {
    deviceName = "Axivity"
    monn = "axivity"
    monc = 4
    dformc = 4
    dformn = "cwa"
    sf = 100
    epSizeShort = 5
  } else if (length(grep(pattern = "actiwatch", x = params_general[["dataFormat"]], ignore.case = TRUE)) > 0) {
    deviceName = "Actiwatch"
    monn = "actiwatch"
    monc = 99
    dformc = 99
    dformn = "epochdata"
    sf = 100 # <= EXTRACT FROM FILE?
  }
  # Before we look inside the epoch files we can already create templates
  # with dummy data
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
                                  accmetric = rep(0, 3)), 
           wday = 0,
           wdayname = "Sunday",
           windowsizes = params_general[["windowsizes"]],
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
    filename = "unknown",
    deviceSerialNumber = "unknown")
  
  filefoldername = NA
  
  
  detectQuote = function(fn, ind) {
    # From experience we know that on some machine the quotes in the files are
    # poorly recognised, to catch this first try to check whether this is the case:
    quote = "\""
    try(expr = {Dtest = data.table::fread(input = fn,
                                          header = FALSE, sep = ",", skip = ind,
                                          nrows = 20, quote = quote)}, silent = TRUE)
    if (length(Dtest) == 0) {
      quote = ""
    } else {
      if (nrow(Dtest) <= 1) {
        quote = "" 
      }
    }
    return(quote)
  }
  
  for (i in 1:length(fnames)) { # loop over all epoch files
    # filename
    fname = basename(fnames[i])
    
    outputFileName = paste0(outputdir, "/meta/basic/meta_", fname, ".RData")
    
    skip = TRUE
    if (params_general[["overwrite"]] == TRUE) {
      skip = FALSE
    } else {
      if (!file.exists(outputFileName)) {
        skip = FALSE
      }
    }
    if (skip == FALSE) {
      I$filename = filename_dir = fname
      
      if (params_general[["dataFormat"]] == "ukbiobank_csv") {
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
      } else if (length(grep(pattern = "actiwatch", x = params_general[["dataFormat"]], ignore.case = TRUE)) > 0) {
        if (params_general[["dataFormat"]] == "actiwatch_csv") {
          # ! Assumptions that timeseries start before line 150
          ind = 150
          quote = detectQuote(fn = fnames[i], ind = ind)
          testraw = data.table::fread(input = fnames[i],
                                      header = FALSE, sep = ",", skip = ind,
                                      nrows = 1, data.table = TRUE, quote = quote)
          # ! Assumption that first column are the epoch numbers
          delta = 1 - testraw$V1
          ind = ind + delta
          D = data.table::fread(input = fnames[i], sep = ",", skip = 500, nrows = 10, quote = quote)
          
          # ! Assumption that column names are present 2 lines prior to timeseries
          colnames = data.table::fread(input = fnames[i],
                                       header = FALSE, sep = ",",
                                       skip = ind - 2, nrows = 1, quote = quote)

          colnames(D) = as.character(colnames)[1:ncol(D)]
          # ! Assumptions about columns names
          colnames(D) = gsub(pattern = "datum|date", replacement = "date", x = colnames(D), ignore.case = TRUE)
          colnames(D) = gsub(pattern = "tijd|time", replacement = "time", x = colnames(D), ignore.case = TRUE)
          colnames(D) = gsub(pattern = "activiteit|activity", replacement = "ZCY", x = colnames(D), ignore.case = TRUE)
          
          timestamp_POSIX = as.POSIXct(x = paste(D$date[1:4], D$time[1:4], sep = " "), format = "%d-%m-%Y %H:%M:%S", tz = tz)
          epSizeShort = mean(diff(as.numeric(timestamp_POSIX)))
          if (is.na(epSizeShort)) {
            timestamp_POSIX = as.POSIXct(x = paste(D$date[1:4], D$time[1:4], sep = " "), format = "%d/%m/%Y %H:%M:%S", tz = tz)
            epSizeShort = mean(diff(as.numeric(timestamp_POSIX)))
          }

          if (epSizeShort != params_general[["windowsizes"]][1]) {
            stop(paste0("\nThe short epoch size as specified by the user as the first value of argument windowsizes (",
                 params_general[["windowsizes"]][1],
                 " seconds) does NOT match the short epoch size we see in the data (", epSizeShort),
                 " seconds). Please correct.", call. = FALSE)
          }
          timestamp_POSIX = timestamp_POSIX[1]
          D = D[, "ZCY"]
          if (quote == "") D$ZCY = as.numeric(D$ZCY)
        } else if (params_general[["dataFormat"]] == "actiwatch_awd") {
          # ! Assumption that first data row equals the first row with 3 columns
          ind = 0
          
          quote = detectQuote(fn = fnames[i], ind = 50)
          NC = 1
          while (NC >= 3) {
            
            testraw = data.table::fread(input = fnames[i],
                                      header = FALSE, sep = ",", skip = ind,
                                      nrows = 1, data.table = TRUE, quote = quote)
            NC = ncol(testraw)
            if (NC >= 3) {
              break()
            } else {
              ind = ind + 1
            }
          }
          D = data.table::fread(input = fnames[i],
                                header = FALSE, sep = ",", skip = ind, quote = quote)
          D = D[,1]
          colnames(D)[1] = "ZCY"
          header = data.table::fread(input = fnames[i],
                                header = FALSE, sep = ",", nrows =  7, quote = quote)
          # Get epoch size
          optionalEpochs = data.frame(code = c("1", "2", "4", "8", "20", "81", "C1", "C2"),
                                      size = c(15, 30, 60, 120, 300, 2, 5, 10))
          epSizeShort = optionalEpochs$size[which(optionalEpochs$code == as.character(header[4]))]
          # Get starttime 
          timestamp_POSIX = as.POSIXct(x = paste(header[2], header[3], sep = " "),
                                       format = "%d-%b-%Y %H:%M", tz = tz)
          if (is.na(timestamp_POSIX) == TRUE) {
            timestamp_POSIX = as.POSIXct(x = paste(header[2], header[3], sep = " "),
                                         format = "%d/%b/%Y %H:%M", tz = tz)
          }
          if (epSizeShort != params_general[["windowsizes"]][1]) {
            stop(paste0("\nThe short epoch size as specified by the user as the first value of argument windowsizes (",
                        params_general[["windowsizes"]][1],
                        " seconds) does NOT match the short epoch size we see in the data (", epSizeShort),
                 " seconds). Please correct.", call. = FALSE)
          }
          if (quote == "") D$ZCY = as.numeric(D$ZCY)
        }
      }
      
      
      quartlystart = (ceiling(as.numeric(timestamp_POSIX) / epSizeLong)) * epSizeLong
      
      ts_longEp_POSIX = as.POSIXlt(quartlystart, tz = tz, origin = "1970-01-01") # POSIX time
      M$wdayname = weekdays(x = ts_longEp_POSIX, abbreviate = FALSE) # extract weekday as day name
      M$wday = ts_longEp_POSIX$wday # extract weekday as a daynumber
      
      if (length(ts_longEp_POSIX) > 2) {
        # calculate time difference relative to original data
        # to know how much epoch to ignore at the beginning of the file
        deltastart = as.numeric(difftime(ts_longEp_POSIX, timestamp_POSIX,units = "sec") / 5)
        # shorten the data (object D) accordingly
        D = D[(deltastart + 1):nrow(D),]
      }
      #create timeseries for metashort
      time_shortEp_num = seq(quartlystart, quartlystart + ((nrow(D) - 1) * epSizeShort), by = epSizeShort)
      time_longEp_num = seq(quartlystart, quartlystart + ((nrow(D) - 1) * epSizeShort), by = epSizeLong)
      
      if (length(time_longEp_num) * 15 > length(time_shortEp_num)) {
        time_longEp_num = time_longEp_num[1:floor(length(time_shortEp_num) / (epSizeLong/epSizeShort))]
        time_shortEp_num = time_shortEp_num[1:(length(time_longEp_num) * (epSizeLong/epSizeShort))]
      }
      starttime = as.POSIXlt(time_longEp_num[1], origin = "1970-1-1", tz = tz)
      time_shortEp_8601 = POSIXtime2iso8601(x = as.POSIXlt(time_shortEp_num, tz = tz,
                                                                origin = "1970-01-01"),
                                                     tz = tz)
      time_longEp_8601 = POSIXtime2iso8601(x = as.POSIXlt(time_longEp_num, tz = tz, origin = "1970-01-01"),
                                                tz = tz)
      M$metashort = data.frame(timestamp = time_shortEp_8601,
                               accmetric = D[1:length(time_shortEp_8601),1],stringsAsFactors = FALSE)
      LML = length(time_longEp_8601)
      if (params_general[["dataFormat"]] == "ukbiobank_csv") {
        names(M$metashort)[2] = "LFENMO"
        #Collapse second column of input data to use as non-wear score
        imp = D[, 2]
        M$metashort$LFENMO = M$metashort$LFENMO/1000
      } else if (length(grep(pattern = "actiwatch", x = params_general[["dataFormat"]], ignore.case = TRUE)) > 0) {
        imp = unlist(D[, 1])
      }
      navalues = which(is.na(imp) == TRUE)
      if (length(navalues) > 0) imp[navalues] = 1
      
      
      if (params_general[["dataFormat"]] == "ukbiobank_csv") {
        # Take long epoch mean of UK Biobank based invalid data indicater per 5 seconds
        imp2 = cumsum(imp)
        imp3 = diff(imp2[seq(1, length(imp2),
                             by = ((60/epSizeShort) * (epSizeLong/60)))]) / ((60/epSizeShort) * (epSizeLong/60)) # rolling mean
        imp4 = round(imp3 * 3) # create three level nonwear score from it, not really necessary for GGIR, but helps to retain some of the information
      } else if (length(grep(pattern = "actiwatch", x = params_general[["dataFormat"]], ignore.case = TRUE)) > 0) {
        # Using rolling 60 minute sum to indicate whether it is nonwear
        imp2 = zoo::rollapply(imp, width = (1*3600) / epSizeShort, FUN = sum, fill = 0)
        imp4 = imp2
        imp4[which(imp2 > 0)] = 0
        imp4[which(imp2 == 0)] = 3 # If rolling average is zero then consider it nonwear
        imp5 = cumsum(imp4)
        step = (60/epSizeShort) * (epSizeLong/60)
        imp4 = diff(imp5[seq(1, length(imp5) + step, by = step)]) / step # rolling mean
        if (length(imp4) > length(time_longEp_8601)) imp4 = imp4[1:length(time_longEp_8601)]
      }
      
      if (length(imp4) < LML) {
        imp4 = c(imp4, rep(0, LML - length(imp4)))
      }
      # create data.frame for metalong, note that light and temperature are just set at zero
      M$metalong = data.frame(timestamp = time_longEp_8601,nonwearscore = imp4, #rep(0,LML)
                              clippingscore = rep(0,LML), lightmean = rep(0,LML),
                              lightpeak = rep(0,LML), temperaturemean = rep(0,LML), EN = rep(0,LML))
      
      # update weekday name and number based on actual data
      M$wdayname = weekdays(x = starttime, abbreviate = FALSE)
      M$wday = as.POSIXlt(starttime)$wday + 1
      
      # Save these files as new meta-file
      save(M, C, I, filename_dir, filefoldername,
           file = outputFileName)
      Sys.setlocale("LC_TIME", Syslocale) # set language to English
    }
  }
}