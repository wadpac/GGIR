convertEpochData = function(datadir = c(), metadatadir = c(),
                            params_general = c(), verbose = TRUE) {
  
  # Function to convert externally derived epoch data
  # to a format that allows GGIR to process them as if they were part 1 output.
  
  # Declare local funcions:
  matAggregate = function(mat, step) {
    # Aggregate matrix mat by taking over step number of rows
    # as sum unless column names is sleep or nonwear in that case
    # we take the rounded mean.
    mat = rbind(rep(0, ncol(mat)), mat)
    cumsum2 = function(x) {
      x = cumsum(ifelse(is.na(x), 0, x)) + x*0
      return(x)
    }
    mat = apply(mat, 2, cumsum2)
    mat = mat[seq(1, nrow(mat), by = step), , drop = FALSE]
    mat = apply(mat, 2, diff)
    # Correct non incremental variables
    for (niv in c("sleep", "ExtSleep", "light", "nonwear")) {
      if (niv %in% colnames(D)) D[, niv] = round(D[, niv] / step)
    }
    return(mat)
  }

  checkEpochMatch = function(desiredEpochSize, epSizeShort) {
    # Check whether desired and derived epoch size match
    if (!is.null(desiredEpochSize) && epSizeShort != desiredEpochSize) {
      stop(paste0("\nThe short epoch size as specified by the user (",
                  desiredEpochSize, " seconds) does NOT match the short",
                  " epoch size we see in the data (", epSizeShort,
                  " seconds). Please correct."), call. = FALSE)
    }
    return()
  }
  #-----------------------------------------------------------
  
  
  tz = params_general[["desiredtz"]]
  epSizeShort = params_general[["windowsizes"]][1]
  epSizeLong = params_general[["windowsizes"]][2]
  epSizeNonWear = params_general[["windowsizes"]][3]
  myfun = NULL
  # Identify input data file extensions
  if (dir.exists(datadir) == FALSE) {
    stop("\nWhen working with external data, argument datadir is expected to be a directory")
  }
  fnames_csv = dir(datadir, full.names = TRUE,pattern = "[.]csv")
  fnames_awd = dir(datadir, full.names = TRUE,pattern = "[.]awd|[.]AWD")
  fnames_xls = dir(datadir, full.names = TRUE,pattern = "[.]xls")
  if (length(which(c(length(fnames_csv), length(fnames_awd), length(fnames_xls)) != 0)) > 1) {
    stop("Do not mix csv and awd files in the same data directory")
  } else {
    if (length(fnames_awd) > 0) {
      if (params_general[["dataFormat"]] != "actiwatch_awd") {
        stop("Specified dataFormat does not match the data")
      }
      fnames = fnames_awd
    } else if (length(fnames_csv) > 0) {
      if (params_general[["dataFormat"]] == "actiwatch_awd") {
        stop("Specified dataFormat does not match the data")
      }
      fnames = fnames_csv
    } else if (length(fnames_xls) > 0) {
      if (params_general[["dataFormat"]] != "sensewear_xls") {
        stop("Specified dataFormat does not match the data")
      }
      fnames = fnames_xls
    }
  }
  #-------------
  # Create output folder, normally with raw data g.part1 would do this:
  if (!file.exists(metadatadir)) {
    dir.create(metadatadir)
    dir.create(file.path(metadatadir, "meta"))
    dir.create(file.path(metadatadir, "meta", "basic"))
    dir.create(file.path(metadatadir, "results"))
    dir.create(file.path(metadatadir, "results", "QC"))
  }
  
  #============
  # Based on knowledge about data format
  # we can already assign monitor names and codes
  # dataformat names and codes
  # and sample rate
  actiwatchData = length(grep(pattern = "actiwatch", x = params_general[["dataFormat"]], ignore.case = TRUE)) > 0
  if (params_general[["dataFormat"]] == "ukbiobank_csv") {
    deviceName = "Axivity"
    monn = "axivity"
    monc = 4
    dformc = 4
    dformn = "cwa"
    sf = 100
    epSizeShort = 5
  } else if (params_general[["dataFormat"]] == "actigraph_csv") {
    deviceName = "ActiGraph"
    monn = "actigraph"
    monc = 3
    dformc = 2
    dformn = "csv"
    sf = 0 # extract from file header
    epSizeShort = 0 # extract from file header
  } else if (actiwatchData == TRUE) {
    deviceName = "Actiwatch"
    monn = "actiwatch"
    monc = 99
    dformc = 99
    dformn = "epochdata"
    sf = 100 # <= EXTRACT FROM FILE?
  } else if (params_general[["dataFormat"]] == "sensewear_xls") {
    deviceName = "Sensewear"
    monn = "sensewear"
    monc = 98
    dformc = 98
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
                           start = "", # if relevant, this will be filled later on in code
                           device = deviceName,
                           firmwareVersion = "unknown",
                           block = 0)
  dummyheader = t(dummyheader)
  colnames(dummyheader) = "value"
  dummyheader = as.data.frame(dummyheader)
  I = list(
    header = dummyheader,
    monc = monc,
    monn = monn,
    dformc = dformc,
    dformn = dformn,
    sf = sf,
    filename = "unknown",
    deviceSerialNumber = "unknown")

  I_bu = I # backup version of I (for now only used in actigraph_csv)
  for (i in 1:length(fnames)) { # loop over all epoch files
    # include verbose info
    if (verbose == TRUE) {
      if (i  == 1) {
        cat(paste0("\nP1 file: ", i))
      } else {
        cat(paste0(" ", i))
      }
    }
    # filename
    fname = basename(fnames[i])
    
    outputFileName = paste0(metadatadir, "/meta/basic/meta_", fname, ".RData")
    
    skip = TRUE
    if (params_general[["overwrite"]] == TRUE) {
      skip = FALSE
    } else {
      if (!file.exists(outputFileName)) {
        skip = FALSE
      }
    }
    if (skip == FALSE) {
      I = I_bu
      I$filename = filename_dir = fname
      if (params_general[["dataFormat"]] == "ukbiobank_csv") {
        # read data
        D = data.table::fread(input = fnames[i],
                              header = TRUE,
                              data.table = FALSE,
                              sep = ",")
        header = as.character(data.table::fread(input = fnames[i], header = FALSE, nrows = 1, data.table = FALSE, sep = ",")[1, 1])
        # extract date/timestamp from fileheader
        timestamp = unlist(strsplit(header," - "))[2]
        timestamp_POSIX = as.POSIXlt(timestamp, tz = tz)
        D = list(data = D, epochSize = epSizeShort, startTime = timestamp_POSIX)
      } else if (params_general[["dataFormat"]] == "sensewear_xls") {
        # read data
        D = as.data.frame(readxl::read_excel(path = fnames[i], col_types = "text"))
        # Convert timestamps which is read as Excel timestamp
        timestamp_POSIX = format(as.POSIXct(as.numeric(D[, grep(pattern = "Time", x = colnames(D))]) * (60*60*24),
                                            origin = "1899-12-30", tz = "GMT"))
        timestamp_POSIX = as.POSIXct(timestamp_POSIX, tz = tz)
        
        D = D[, c("METs", "Step Counter", "Sleep")]
        colnames(D) = c("ExtAct", "ExtStep", "ExtSleep")
        D$ExtAct = as.numeric(D$ExtAct)
        D$ExtStep = as.numeric(D$ExtStep)
        D$ExtSleep = as.numeric(D$ExtSleep)
        epochSize = difftime(timestamp_POSIX[2], timestamp_POSIX[1], 
                             units = "secs")
        epSizeShort = as.numeric(epochSize)
        timestamp_POSIX = timestamp_POSIX[1]
        D = list(data = D, epochSize = epSizeShort, startTime = timestamp_POSIX)
      } else if (params_general[["dataFormat"]] == "actigraph_csv") {
        D = GGIRread::readActiGraphCount(filename = fnames[i], 
                           timeformat = params_general[["extEpochData_timeformat"]],
                           desiredtz = params_general[["desiredtz"]],
                           configtz = params_general[["configtz"]],
                           timeformatName = "extEpochData_timeformat")
        
        # Rename to align with GGIR metric naming
        cnd = colnames(D$data)
        colnames(D$data)[which(cnd == "x")] = "NeishabouriCount_x"
        colnames(D$data)[which(cnd == "y")] = "NeishabouriCount_y"
        colnames(D$data)[which(cnd == "z")] = "NeishabouriCount_z"
        colnames(D$data)[which(cnd == "vm")] = "NeishabouriCount_vm"
      } else if (length(grep(pattern = "actiwatch", x = params_general[["dataFormat"]], ignore.case = TRUE)) > 0) {
        D = GGIRread::readActiwatchCount(filename = fnames[i], 
                               timeformat = params_general[["extEpochData_timeformat"]],
                               desiredtz = params_general[["desiredtz"]],
                               configtz = params_general[["configtz"]],
                               timeformatName = "extEpochData_timeformat")
        # Rename to align with GGIR metric naming
        colnames(D$data)[which(colnames(D$data) == "counts")] = "ZCY"
        extraVars  = grep(pattern = "light|nonwear", x = colnames(D$data))
        if (length(extraVars) > 0) {
          # split the extraVars
          D_extraVars = D$data[, extraVars, drop = FALSE]
          D$data = D$data[, -extraVars, drop = FALSE]
        }
      } else if (params_general[["dataFormat"]] == "phb_xlsx") {
        # phb = Philips Health Band
        
        # fnames = dir(inputPath, recursive = FALSE, full.names = TRUE, pattern = "[.]xlsx")
        # fileOverview = data.frame(filename = fnames)
        # extractID = function(x) {
        #   x = basename(x)
        #   x = gsub(pattern = "sleep_wake", replacement = "sleepwake", x = tolower(x))
        #   ID = unlist(strsplit(x, "_"))[2]
        #   return(ID)
        # }
        # fileOverview$ID = unlist(lapply(fileOverview$filename, FUN = extractID))
        # 
        # uids = unique(fileOverview$ID)
        # for (uid in uids) {
        # filesForThisPerson = fileOverview$filename[which(fileOverview$ID == uid)]
      }
      if ("sleep" %in% colnames(D$data)) colnames(D$data)[which(colnames(D$data) == "sleep")] = "ExtSleep"
      if ("steps" %in% colnames(D$data)) colnames(D$data)[which(colnames(D$data) == "steps")] = "ExtStep"
      
      # Convert lists to objects as expected by code below
      timestamp_POSIX = D$startTime
      epSizeShort = D$epochSize
      D = D$data
      #-----------------------------------
      # Aggregate epochs if desired epoch length is larger
      desiredEpochSize = params_general[["windowsizes"]][1]
      if (!is.null(desiredEpochSize)) {
        if (desiredEpochSize > epSizeShort) {
          step = desiredEpochSize %/% epSizeShort

          D = matAggregate(D, step)
          epSizeShort = epSizeShort * step
        }
        checkEpochMatch(desiredEpochSize, epSizeShort)
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
        D = D[(deltastart + 1):nrow(D), drop = FALSE]
      }
      # Define compatible metashort and metalong dimensions
      LMS = nrow(D)
      LML = LMS * epSizeShort / epSizeLong
      expected_LML = floor(LML)
      if (expected_LML != LML) {
        # this means LMS and LML are not compatible
        # keep only until last epSizeLong available
        # redefine metashort length
        LMS = expected_LML * epSizeLong / epSizeShort
        D = D[1:LMS, , drop = FALSE]
        LML = expected_LML
      }
      #create timeseries for metashort
      time_shortEp_num = seq(quartlystart, quartlystart + ((nrow(D) - 1) * epSizeShort), by = epSizeShort)
      time_longEp_num = seq(quartlystart, quartlystart + ((nrow(D) - 1) * epSizeShort), by = epSizeLong)
      if (LML * 15 > LMS) {
        time_longEp_num = time_longEp_num[1:floor(length(time_shortEp_num) / (epSizeLong/epSizeShort))]
        time_shortEp_num = time_shortEp_num[1:(length(time_longEp_num) * (epSizeLong/epSizeShort))]
      }
      starttime = as.POSIXlt(time_longEp_num[1], origin = "1970-1-1", tz = tz)
      time_shortEp_8601 = POSIXtime2iso8601(x = as.POSIXlt(time_shortEp_num, tz = tz,
                                                           origin = "1970-01-01"),
                                            tz = tz)
      time_longEp_8601 = POSIXtime2iso8601(x = as.POSIXlt(time_longEp_num, tz = tz, origin = "1970-01-01"),
                                           tz = tz)
      if (params_general[["dataFormat"]] %in% c("actigraph_csv", "sensewear_xls", 
                                                "actiwatch_awd", "actiwatch_csv") == FALSE) {
        M$metashort = data.frame(timestamp = time_shortEp_8601,
                                 accmetric = D[1:length(time_shortEp_8601),1],stringsAsFactors = FALSE)
      } else {
        M$metashort = as.data.frame(cbind(time_shortEp_8601,
                                          D[1:length(time_shortEp_8601), ]))
        colnames(M$metashort) = c("timestamp", colnames(D))
        for (ic in 2:ncol(M$metashort)) {
          M$metashort[, ic] <- as.numeric(M$metashort[, ic])
        }
      }
      if (length(which(is.na(M$metashort$ZCY) == TRUE)) > 0) {
        # impute missing data by zero
        # if it is a lot then this will be detected as non-wear
        M$metashort$ZCY[is.na(M$metashort$ZCY)] = 0 
      }
      LML = length(time_longEp_8601)
      if (params_general[["dataFormat"]] == "ukbiobank_csv") {
        names(M$metashort)[2] = "LFENMO"
        #Collapse second column of input data to use as non-wear score
        imp = D[, 2]
        M$metashort$LFENMO = M$metashort$LFENMO/1000
      } else if (length(grep(pattern = "actiwatch", x = params_general[["dataFormat"]], ignore.case = TRUE)) > 0) {
        imp = unlist(D[, 1])
      } else if (params_general[["dataFormat"]] == "actigraph_csv") {
        imp = unlist(D[, 1])
      } else if (params_general[["dataFormat"]] == "sensewear_xls") {
        imp = unlist(D[, 1])
      }
      navalues = which(is.na(imp) == TRUE)
      if (length(navalues) > 0) imp[navalues] = 1

      if (params_general[["dataFormat"]] == "ukbiobank_csv") {
        # Take long epoch mean of UK Biobank based invalid data indicater per 5 seconds
        imp2 = cumsum(imp)
        imp3 = diff(imp2[seq(1, length(imp2),
                             by = ((60/epSizeShort) * (epSizeLong/60)))]) / ((60/epSizeShort) * (epSizeLong/60)) # rolling mean
        nonwearscore = round(imp3 * 3) # create three level nonwear score from it, not really necessary for GGIR, but helps to retain some of the information
      } else if (length(grep(pattern = "actiwatch", x = params_general[["dataFormat"]], ignore.case = TRUE)) > 0 |
                 params_general[["dataFormat"]] == "actigraph_csv" |
                 params_general[["dataFormat"]] == "sensewear_xls") {
        # Using rolling long window sum to indicate whether it is nonwear
        nonwearscore = rep(0, LML)
        ni = 1
        for (g in seq(from = 1, to = length(imp), by = epSizeLong / epSizeShort)) {
          iend = g + (epSizeNonWear / epSizeShort) - 1
          indices = g:iend
          if (iend <= length(imp)) {
            if (sum(imp[indices], na.rm = TRUE) == 0) {
              nonwearscore[ni] = 3
            }
          }
          ni = ni + 1
        }
        if (length(nonwearscore) > length(time_longEp_8601)) nonwearscore = nonwearscore[1:length(time_longEp_8601)]
        nonwearscore = round(nonwearscore)
        if (any(is.na(nonwearscore))) nonwearscore[which(is.na(nonwearscore))] = 3
      }
      if (length(nonwearscore) < LML) {
        nonwearscore = c(nonwearscore, rep(0, LML - length(nonwearscore)))
      }
      if (params_general[["dataFormat"]] == "sensewear_xls") {
        # Create myfun object, this to trigger outcome type specific analysis
        myfun = list(FUN = NA,
                     parameters = NA, 
                     expected_sample_rate = NA, 
                     expected_unit = "g", 
                     colnames = c("ExtAct", "ExtStep", "ExtSleep"),
                     outputres = epSizeShort,
                     minlength = NA,
                     outputtype = c("numeric", "numeric", "numeric"),
                     aggfunction = NA,
                     timestamp = F, 
                     reporttype = c("scalar", "event", "type"))
      }
      # create data.frame for metalong, note that light and temperature are just set at zero
      M$metalong = data.frame(timestamp = time_longEp_8601,nonwearscore = nonwearscore, #rep(0,LML)
                              clippingscore = rep(0,LML), lightmean = rep(0,LML),
                              lightpeak = rep(0,LML), temperaturemean = rep(0,LML), EN = rep(0,LML))
      
      # update weekday name and number based on actual data
      M$wdayname = weekdays(x = starttime, abbreviate = FALSE)
      M$wday = as.POSIXlt(starttime)$wday + 1
      # Save these files as new meta-file
      filefoldername = NA
      save(M, C, I, myfun, filename_dir, filefoldername,
           file = outputFileName)
    }
  }
}
