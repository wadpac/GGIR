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
    for (niv in c("sleep", "ExtSleep", "light", "nonwear", "marker", "light")) {
      if (niv %in% colnames(D)) mat[, niv] = round(mat[, niv] / step)
    }
    # Incremental variables are counts, calories, ExtAct as so far
    # none of the data formats provide movement expressed as average acceleration
    # per epoch
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
  # Identify input data files
  if (dir.exists(datadir) == FALSE) {
    stop("\nWhen working with external data, argument datadir is expected to be a directory")
  }
  
  if (params_general[["dataFormat"]] == "phb_xlsx") {
    # Philips Health Band data comes with two files per recording that need to be matched
    # Identify all xlsx files
    xlsx_files = dir(datadir, recursive = TRUE, full.names = TRUE, pattern = "[.]xlsx")
    xlsx_files = xlsx_files[grep(pattern = "sleep|datalist", x = xlsx_files, ignore.case = TRUE)]
    # Identify the pairs by looking for matching ID
    fileOverview = data.frame(filename = xlsx_files)
    extractID = function(x) {
      x = basename(x)
      # remove _ in sleep_wake to ease finding the ID
      x = gsub(pattern = "sleep_wake", replacement = "sleepwake", x = tolower(x))
      ID = unlist(strsplit(x, "_"))[2]
      return(ID)
    }
    fileOverview$ID = unlist(lapply(fileOverview$filename, FUN = extractID))
    # Put matching file pairs in a vector of lists
    uids = unique(fileOverview$ID)
    fnames = rep(NA, length(uids))
    for (uid in 1:length(uids)) {
      # Only keep recording where we have both files
      matchingFiles = which(fileOverview$ID == uids[uid])
      if (length(matchingFiles) == 2) {
        fnames[uid] = list(fileOverview$filename[matchingFiles])
      } else if (length(matchingFiles) == 1) {
        warning(paste0("No matching file found for ", fileOverview$filename[matchingFiles]), call. = FALSE)
      } else if (length(matchingFiles) > 2) {
        warning(paste0("There are more than 2 files for ID ", uids[uid], "."), call. = FALSE)
      }
    }
  } else if (params_general[["dataFormat"]] == "fitbit_json") {
    # Fitbit comes with one folder per participant that has multiple files
    # Identify folders
    participantFolders = dir(datadir, include.dirs = TRUE, full.names = TRUE)
    participantFolders = participantFolders[dir.exists(participantFolders)]
    IDs = fnames = rep(NA, length(participantFolders))
    for (uid in 1:length(participantFolders)) {
      IDs[uid] = basename(participantFolders[uid]) # Use folder name as ID because ID is not inside the files
      # Identify files in each folder
      jsonFiles = dir(participantFolders[uid], pattern = "json", recursive = TRUE, full.names = TRUE)
      jsonFiles = grep(pattern = "sleep|steps|calories", x = jsonFiles, value = TRUE)
      if (length(jsonFiles) > 0) {
        fnames[uid] = list(jsonFiles = jsonFiles)
      }
    }
    IDs = IDs[!is.na(fnames)]
    fnames = fnames[!is.na(fnames)]
  } else {
    # Data with just one recording per file.
    # Here we do a check that no unexpected file types are found.
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
    epSizeShort = 5
  } else if (params_general[["dataFormat"]] == "actigraph_csv") {
    deviceName = "ActiGraph"
    monn = "actigraph"
    monc = 3
    dformc = 95
    epSizeShort = 0 # extract from file header
  } else if (actiwatchData == TRUE) {
    deviceName = "Actiwatch"
    monn = "actiwatch"
    monc = 99
    dformc = 99
  } else if (params_general[["dataFormat"]] == "sensewear_xls") {
    deviceName = "Sensewear"
    monn = "sensewear"
    monc = 98
    dformc = 98
  } else  if (params_general[["dataFormat"]] == "phb_xlsx") {
    deviceName = "PhilipsHealthBand"
    monn = "philipshealthband"
    monc = 97
    dformc = 97
  } else  if (params_general[["dataFormat"]] == "fitbit_json") {
    deviceName = "Fitbit"
    monn = "Fitbit"
    monc = 96
    dformc = 96
  } else if (params_general[["dataFormat"]] == "actical_csv") {
    deviceName = "Actical"
    monn = "actical"
    monc = 94
    dformc = 94
    epSizeShort = 0 # extract from file header
  }
  sf = NA
  dformn = params_general[["dataFormat"]]
  
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
                                  ExtAct = rep(0, 3)), 
           wday = 0,
           wdayname = "Sunday",
           windowsizes = params_general[["windowsizes"]],
           bsc_qc = data.frame(A = 1:3,
                               B = 1:3))
  dummyheader = data.frame(uniqueSerialCode = 0, 
                           frequency = NA, # NA because we work with processed data
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
    if (!all(is.na(unlist(fnames[i])))) {
      fname = basename(unlist(fnames[i])[1])
      outputFileName = paste0(metadatadir, "/meta/basic/meta_", fname, ".RData")
      skip = TRUE
      if (params_general[["overwrite"]] == TRUE) {
        skip = FALSE
      } else {
        if (!file.exists(outputFileName)) {
          skip = FALSE
        }
      }
    } else {
      skip = TRUE
    }
    if (skip == FALSE) {
      I = I_bu
      D_extraVars = NULL
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
      } else if (params_general[["dataFormat"]] == "actical_csv") {
        D = GGIRread::readActicalCount(filename = fnames[i],
                                       timeformat = params_general[["extEpochData_timeformat"]],
                                       desiredtz = params_general[["desiredtz"]],
                                       configtz = params_general[["configtz"]],
                                       timeformatName = "extEpochData_timeformat")
        # Rename to align with GGIR metric naming
        colnames(D$data)[which(colnames(D$data) == "counts")] = "ZCY" 
        # Note: the counts were externally generated, they may not be 
        # zero crossing counts but to ease generic functionality all counts
        # generated by external devices are referred to as ZCY
        # for analysis it does not matter
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
        if (D$epochSize == 120) {
          # Oddly data can be stored in 120 second epochs even though original publications
          # never proposed this. The rest of GGIR cannot handle epoch size of 120 seconds
          # and most practical solution seems to duplicate data to simulate 60 second epoch
          D$data = D$data[rep(1:nrow(D$data), each = 2),]
          D$data[, "ZCY"] = D$data[, "ZCY"] / 2
          D$epochSize = 60
        }
      } else if (params_general[["dataFormat"]] == "phb_xlsx") {
        # phb = Philips Health Band
        D = GGIRread::mergePHBdata(filenames = fnames[[i]], 
                                   timeformat = params_general[["extEpochData_timeformat"]],
                                   desiredtz = params_general[["desiredtz"]],
                                   configtz = params_general[["configtz"]],
                                   timeformatName = "extEpochData_timeformat")
        if (nrow(D$data) < 2) next
        epochSize = difftime(D$data$timestamp[2], D$data$timestamp[1], 
                             units = "secs")
        epSizeShort = as.numeric(epochSize)
        timestamp_POSIX = D$data$timestamp[1]
        
        D$epochSize = epSizeShort
        D$startTime = timestamp_POSIX
        
        extraVars  = grep(pattern = "nonwear", x = colnames(D$data))
        if (length(extraVars) > 0) {
          # split the extraVars
          D_extraVars = D$data[, extraVars, drop = FALSE]
          D$data = D$data[, -extraVars, drop = FALSE]
        }
        colnames(D$data)[which(colnames(D$data) == "counts")] = "ExtAct"
        D$data = D$data[, grep(pattern = "cardio|heart|sleepevent|battery|duration|missing|activem|vo2|energy|respiration|timestamp",
                               x = colnames(D$data), ignore.case = TRUE, invert = TRUE)]
      } else if (params_general[["dataFormat"]] == "fitbit_json") {
        data = GGIRread::mergeFitbitData(filenames = fnames[[i]],
                            desiredtz = params_general[["desiredtz"]],
                            configtz = params_general[["configtz"]])
        timeRangeSleep = range(data$dateTime[!is.na(data$sleeplevel)])
        timeRangeCalories = range(data$dateTime[!is.na(data$calories)])
        if (timeRangeSleep[1] > timeRangeCalories[2] ||
            timeRangeSleep[2] < timeRangeCalories[1]) {
          warning(paste0("Fitbit sleep and calories data do not overlap ",
                         "in time for files in: ", unique(basename(dirname(fnames[[i]]))),
                         ". Sleep is available from ",
                         timeRangeSleep[1], " to ", timeRangeSleep[2],
                         ". Calories are available from ",
                         timeRangeCalories[1], " to ", timeRangeCalories[2]),
                  call. = FALSE)
        }
        ID = IDs[i]
        I$filename = filename_dir = fname = ID
        epochSizes = names(table(diff(data$dateTime)))
        if (length(epochSizes) > 1) {
          stop(paste0("multiple epoch sizes encountered in Fitbit data ",
                      basename(fnames[[i]])), call. = FALSE)
        } else if (length(epochSizes) == 1) {
          epSizeShort = as.numeric(epochSizes)
        } else {
          stop(paste0("No epoch size recognised in Fitbit data ",
                      basename(fnames[[i]])), call. = FALSE)
        }
        timestamp_POSIX = data$dateTime[1]
        D = list(data = data, epochSize = epSizeShort, startTime = timestamp_POSIX)
        fnames[[i]] = list(unlist(fnames[i]), ID) # add ID to fnames object
        # convert sleeplevel to sleep because GGIR does not deal with sleep stages
        if ("sleeplevel" %in% names(D$data)) {
          D$data$sleep = 0
          D$data$sleep[which(D$data$sleeplevel %in% c("asleep", "deep", "light", "rem", "restless"))] = 1
          D$data = D$data[, -which(names(D$data) == "sleeplevel")]
        }
        if ("calories" %in% names(D$data)) {
          names(D$data)[which(names(D$data) == "calories")] = "ExtAct"
        }
        D$data = D$data[, -which(names(D$data) %in% c("seconds", "dateTime"))]
      }
      if ("sleep" %in% colnames(D$data)) colnames(D$data)[which(colnames(D$data) == "sleep")] = "ExtSleep"
      if ("steps" %in% colnames(D$data)) colnames(D$data)[which(colnames(D$data) == "steps")] = "ExtStep"
      # Convert lists to objects as expected by code below
      timestamp_POSIX = D$startTime
      epSizeShort = D$epochSize
      D = D$data
      
      #-----------------------------------
      # Aggregate long epochs if desired epoch length is larger
      if (!is.null(D_extraVars)) {
        if ("light" %in% colnames(D_extraVars)) {
          step = epSizeLong %/% epSizeShort
          D_extraVars = matAggregate(D_extraVars, step)
        }
      }
      
      #-----------------------------------
      # Aggregate short epochs if desired epoch length is larger
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
      # formats with possibly more than 1 column
      morethan1 = c("actigraph_csv", "sensewear_xls", "actiwatch_awd",
                    "actiwatch_csv", "phb_xlsx", "actical_csv", "fitbit_json") #, "
      if (params_general[["dataFormat"]] %in% morethan1 == FALSE) {
        M$metashort = data.frame(timestamp = time_shortEp_8601,
                                 ExtAct = D[1:length(time_shortEp_8601),1],stringsAsFactors = FALSE)
      } else {
        M$metashort = as.data.frame(cbind(time_shortEp_8601,
                                          D[1:length(time_shortEp_8601), ]))
        colnames(M$metashort) = c("timestamp", colnames(D))
        for (ic in 2:ncol(M$metashort)) {
          M$metashort[, ic] <- as.numeric(M$metashort[, ic])
        }
      }
      LML = length(time_longEp_8601)
      if (params_general[["dataFormat"]] == "ukbiobank_csv") {
        acc_column = 2
        names(M$metashort)[2] = "LFENMO"
        #Collapse second column of input data to use as non-wear score
        imp = D[, 2]
        M$metashort$LFENMO = M$metashort$LFENMO/1000
        nonwear_in_data = TRUE
      } else {
        acc_column = 1
        imp = unlist(D[, 1])
        nonwear_in_data = FALSE
      }
      # Nonwear in the day
      if (nonwear_in_data == TRUE) {
        # Take long epoch mean of UK Biobank based invalid data indicater per 5 seconds
        imp2 = cumsum(imp)
        imp3 = diff(imp2[seq(1, length(imp2),
                             by = ((60/epSizeShort) * (epSizeLong/60)))]) / ((60/epSizeShort) * (epSizeLong/60)) # rolling mean
        nonwearscore = round(imp3 * 3) # create three level nonwear score from it, not really necessary for GGIR, but helps to retain some of the information
      } else {
        # Using rolling long window sum to indicate whether it is nonwear
        nonwearscore = rep(0, LML)
        ni = 1
 
        for (g in seq(from = 1, to = length(imp), by = epSizeLong / epSizeShort)) {
          iend = g + (epSizeNonWear / epSizeShort) - 1
          indices = g:iend
          if (iend <= length(imp)) {
            if (any(is.na(imp[indices])) || sum(imp[indices], na.rm = TRUE) <= 0) {
              nonwearscore[ni] = 3
            }
            if (length(which(!is.na(imp[indices]))) > 0) {
              # For Fitbit and Sensewear we do nonwear detection based on calories
              if (params_general[["dataFormat"]] %in% c("fitbit_json", "sensewear_xls") &&
                  sd(imp[indices], na.rm = TRUE) < 0.0001 &&
                  mean(imp[indices], na.rm = TRUE) < 2) {
                nonwearscore[ni] = 3
              }
            } else {
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
      if (params_general[["dataFormat"]] %in% c("sensewear_xls", "phb_xlsx", "fitbit_json")) {
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
      if (params_general[["dataFormat"]] == "actiwatch_csv" && "ExtSleep" %in% colnames(D)) {
        # Create myfun object, this to trigger outcome type specific analysis
        myfun = list(FUN = NA,
                     parameters = NA, 
                     expected_sample_rate = NA, 
                     expected_unit = "g", 
                     colnames = c("ExtSleep"),
                     outputres = epSizeShort,
                     minlength = NA,
                     outputtype = c("numeric"),
                     aggfunction = NA,
                     timestamp = F, 
                     reporttype = c("type"))
        # flip sleep scoring 1 <-> 0 to be consistent with GGIR,
        # where 1 is sleep and 0 is wake:
        M$metashort$ExtSleep[which(M$metashort$ExtSleep == 0)] = -1
        M$metashort$ExtSleep[which(M$metashort$ExtSleep == 1)] = 0
        M$metashort$ExtSleep[which(M$metashort$ExtSleep == -1)] = 1
      }
      if (params_general[["dataFormat"]] == "phb_xlsx") {
        neg_indices = which(D$data$ExtAct < 0)
        D$data$ExtAct[neg_indices] = 0
      }
      # create data.frame for metalong, note that light and temperature are just set at zero by default
      M$metalong = data.frame(timestamp = time_longEp_8601, nonwearscore = nonwearscore, #rep(0,LML)
                              clippingscore = rep(0,LML), lightmean = rep(0,LML),
                              lightpeak = rep(0,LML), temperaturemean = rep(0,LML), EN = rep(0,LML))
      # If light was loaded (Actiwatch/PHB) then store this in metalong
      if (!is.null(D_extraVars) && "light" %in% colnames(D_extraVars)) {
        if (nrow(D_extraVars) == nrow(M$metalong)) {
          M$metalong$lightpeak = D_extraVars[, "light"]
        }
      }
      # update weekday name and number based on actual data
      M$wdayname = weekdays(x = starttime, abbreviate = FALSE)
      M$wday = as.POSIXlt(starttime)$wday + 1
      # replace missing values by zero
      for (activityColumn in c("ExtAct", "ZCY")) {
        if (activityColumn %in% colnames(M$metashort)) {
          M$metashort[is.na(M$metashort[, activityColumn]),  activityColumn] = 0
        }
      }
      # Save these files as new meta-file
      filefoldername = NA
      save(M, C, I, myfun, filename_dir, filefoldername,
           file = outputFileName)
    }
  }
}
