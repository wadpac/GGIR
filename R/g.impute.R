g.impute = function(M, I, params_cleaning = c(), desiredtz = "",
                    dayborder = 0, TimeSegments2Zero = c(), acc.metric = "ENMO", 
                    ID, qwindowImp = c(), ...) {
  
  #get input variables
  input = list(...)
  if (length(input) > 0 || length(params_cleaning) == 0) {
    # Extract and check parameters if user provides more arguments than just the parameter arguments,
    # or if params_[...] aren't specified (so need to be filled with defaults).
    # So, inside GGIR this will not be used, but it is used when g.impute is used on its own
    # as if it was still the old g.impute function
    params = extract_params(params_cleaning = params_cleaning,
                            input = input,
                            params2check = c("cleaning")) # load default parameters
    params_cleaning = params$params_cleaning
    rm(params)
  }
  
  windowsizes = M$windowsizes #default: c(5,900,3600)
  metashort = M$metashort
  metalong = M$metalong
  shortEpoch = windowsizes[1]
  mediumEpoch = windowsizes[2]
  longEpoch = windowsizes[3]
  n_short_in_mediumEpoch = mediumEpoch/shortEpoch
  # Move some object from params_cleaning to local to reduce repeated retrieval
  # and for readability
  ndayswindow = params_cleaning[["ndayswindow"]] 
  data_masking_strategy = params_cleaning[["data_masking_strategy"]]
  hrs.del.start = params_cleaning[["hrs.del.start"]]
  hrs.del.end = params_cleaning[["hrs.del.end"]]
  maxdur = params_cleaning[["maxdur"]]
  max_calendar_days = params_cleaning[["max_calendar_days"]]
  rm(M)
  # What is the minimum number of accelerometer axis needed to meet the criteria for nonwear in order for the data to be detected as nonwear?
  wearthreshold = 2 #needs to be 0, 1 or 2
  # windows per day
  n_longEpoch_perday = 86400 / longEpoch
  n_mediumEpoch_perday = 86400 / mediumEpoch
  n_shortEpoch_perday = 86400 / shortEpoch
  # windows per minute
  n_shortEpoch_permin = 60 / shortEpoch
  # windows per hour
  n_longEpoch_perhour = 3600 / longEpoch
  n_medium_perhour = 3600 / mediumEpoch
  n_shortEpoch_perhour = 3600 / shortEpoch
  
  #check that matrices match
  if ((nrow(metalong) / n_mediumEpoch_perday) - (nrow(metashort) / n_shortEpoch_perday) > 0.1) {
    warning("Matrices 'metalong' and 'metashort' are not compatible", call. = FALSE)
  }
  tmi = which(colnames(metalong) == "timestamp")
  time = as.character(as.matrix(metalong[,tmi]))
  startt = as.matrix(metalong[1, tmi])
  #====================================
  # Deriving file characteristics from 15 min summary files
  LD = nrow(metalong) * (mediumEpoch / 60) #length data in minutes
  ND = nrow(metalong) / n_mediumEpoch_perday #number of days
  #==============================================
  # Generating time variable
  timeline = seq(0, ceiling(nrow(metalong) / n_mediumEpoch_perday),
                 by = 1/n_mediumEpoch_perday)
  timeline = timeline[1:nrow(metalong)]
  
  #========================================
  # Extracting non-wear and clipping and make decision on which additional time needs to be considered non-wear
  out = g.weardec(metalong, wearthreshold, mediumEpoch,
                  params_cleaning = params_cleaning,
                  desiredtz = desiredtz,
                  qwindowImp = qwindowImp)
  r1 = out$r1 #non-wear
  r2 = out$r2 #clipping
  r3 = out$r3 #additional non-wear
  r4 = matrix(0,length(r3),1) #protocol based decisions on data removal
  LC = out$LC
  LC2 = out$LC2
  nonwearHoursFiltered = out$nonwearHoursFiltered
  nonwearEventsFiltered = out$nonwearEventsFiltered
  #========================================================
  # Check whether TimeSegments2Zero exist, because this means that the
  # user wants to ignore specific time windows. This feature is used
  # for example if the accelerometer was not worn during the night and the user wants
  # to include the nighttime acceleration in the analyses without imputation,
  # but wants to use imputation for the rest of the day.
  # So, those time windows should not be imputed.
  # and acceleration metrics should have value zero during these windows.
  if (length(TimeSegments2Zero) > 0) {
    r1long = matrix(0, length(r1), n_short_in_mediumEpoch)
    r1long = replace(r1long, 1:length(r1long), r1)
    r1long = t(r1long)
    dim(r1long) = c(length(r1) * n_short_in_mediumEpoch, 1)
    timelinePOSIX = iso8601chartime2POSIX(metashort$timestamp,tz = desiredtz)
    # Combine r1Long with TimeSegments2Zero
    for (kli in 1:nrow(TimeSegments2Zero)) {
      startTurnZero = which(timelinePOSIX == TimeSegments2Zero$windowstart[kli])
      endTurnZero = which(timelinePOSIX == TimeSegments2Zero$windowend[kli])
      r1long[startTurnZero:endTurnZero] = 0
      # Force ENMO and other acceleration metrics to be zero for these intervals
      metashort[startTurnZero:endTurnZero, 
                which(colnames(metashort) %in% c("timestamp","anglex","angley","anglez") == FALSE)] = 0
    }
    # collaps r1long (short epochs) back to r1 (long epochs)
    r1longc = cumsum(c(0, r1long))
    select = seq(1, length(r1longc), by = n_short_in_mediumEpoch)
    r1 = diff(r1longc[round(select)]) / abs(diff(round(select)))
    r1 = round(r1)
  }
  
  #======================================
  # detect first and last midnight and all midnights
  tooshort = 0
  dmidn = g.detecmidnight(time, desiredtz, dayborder)
  firstmidnight = dmidn$firstmidnight;  firstmidnighti = dmidn$firstmidnighti
  lastmidnight = dmidn$lastmidnight;    lastmidnighti = dmidn$lastmidnighti
  midnights = dmidn$midnights;          midnightsi = dmidn$midnightsi
  #===================================================================
  # Trim data based on study_dates_file
  study_dates_log_used = FALSE
  study_date_indices = NULL
  if (!is.null(params_cleaning[["study_dates_file"]])) {
    # Read content of study dates file 
    studyDates = data.table::fread(file = params_cleaning[["study_dates_file"]], data.table = FALSE)
    # Check ID and date formats
    studyDates = check_log(log = studyDates, 
                           dateformat = params_cleaning[["study_dates_dateformat"]], 
                           colid = 1, datecols = 2:3, 
                           logPath = params_cleaning[["study_dates_file"]],
                           logtype = "study dates log")
    # identify first and last study dates
    rowID = which(studyDates[, 1] == ID)
    if (length(rowID) > 0) {
      if (length(rowID) > 1) {
        # this is in the unlikely event that an ID appears twice in the log
        rowID = rowID[1]
        warning(paste0("The ID ", ID, " appears twice in the study dates log"), call. = FALSE)
      }
      # expected first and last midnight
      fmt = params_cleaning[["study_dates_dateformat"]]
      firstmidnight = as.Date(studyDates[rowID, 2], format = fmt)
      lastmidnight = as.Date(studyDates[rowID, 3], format = fmt)
      # find the dates in metalong
      firstmidnighti = midnightsi[grep(firstmidnight, midnights)]
      lastmidnighti = midnightsi[grep(lastmidnight, midnights) + 1] # plus 1 to include the reported date in log
      # trim start
      if (length(firstmidnighti) > 0) {
        r4[1:(firstmidnighti - 1)] = 1
        study_dates_log_used = TRUE
      } else {
        # if midnight timestamp for the date is not available, 
        # do not trim the data and recover the firstmidnight value
        list2env(dmidn[c("lastmidnighti", "lastmidnight")], envir = environment())
        # warning(paste0("The start date provided in the study dates file for ID = ", 
        #                ID, "is not within the dates available in the recording. ",
        #                "The data was not trimmed at the beginning of the recording."), call. = FALSE)
      }
      # trim end
      if (length(lastmidnighti) > 0 && !is.na(lastmidnighti)) {
        r4[lastmidnighti:nrow(r4)] = 1
        study_dates_log_used = TRUE
      } else {
        # if midnight timestamp for the date is not available, 
        # do not trim the data and recover the lastmidnight value
        list2env(dmidn[c("lastmidnighti", "lastmidnight")], envir = environment())
        # warning(paste0("The end date provided in the study dates file for ID = ", 
        #                ID, "is not within the dates available in the recording. ",
        #                "The data was not trimmed at the end of the recording."), call. = FALSE)
      }
      # cut out r4 to apply strategies only on the trimmed portion of data
      # after application of strategies, r4 would reset to the original length
      if (study_dates_log_used == TRUE) {
        r4_bu = r4 # backup copy of r4 before cutting it to be imputed later on
        study_date_indices = which(r4[, 1] == 0)
        r4 = as.matrix(r4[study_date_indices, , drop = FALSE])
      }
    } else if (length(rowID) == 0) {
      warning(paste0("The ID ", ID, " does not appear  in the study dates log ",
                     "the full recording has been considered within the study ",
                     "protocol selection"), call. = FALSE)
    }
  }
  #===================================================================
  # Select data based on data_masking_strategy
  if (data_masking_strategy == 1) { 	#protocol based data selection
    if (hrs.del.start > 0) {
      r4[1:(hrs.del.start * n_medium_perhour)] = 1
    }
    if (hrs.del.end > 0) {
      if (length(r4) > hrs.del.end * n_medium_perhour) {
        r4[(length(r4) + 1 - (hrs.del.end * n_medium_perhour)):length(r4)] = 1
      } else {
        r4[1:length(r4)] = 1
      }
    }
    
    if (LD < 1440) {
      r4 = r4[1:floor(LD / (mediumEpoch / 60))]
    }
    starttimei = 1
    endtimei = length(r4)
  } else if (data_masking_strategy == 2) { #midnight to midnight data_masking_strategy
    starttime = firstmidnight
    endtime = lastmidnight  
    # only apply data_masking_strategy 2 if study dates log is not used for trimming the data,
    # otherwise the data already start and finishes at midnight
    if (study_dates_log_used == FALSE) {
      starttimei = firstmidnighti
      endtimei = lastmidnighti
      if (firstmidnighti != 1) { #ignore everything before the first midnight
        r4[1:(firstmidnighti - 1)] = 1 #-1 because first midnight 00:00 itself contributes to the first full day
      }
      r4[(lastmidnighti):length(r4)] = 1  #ignore everything after the last midnight
    } else {
      starttimei = 1
      endtimei = length(r4)
    }
  } else if (data_masking_strategy %in% c(3, 5)) { #select X most active days
    #==========================================
    # Look out for X most active days and use this to define window of interest
    if (acc.metric %in% colnames(metashort)) {
      atest = as.numeric(as.matrix(metashort[, acc.metric]))
    } else {
      acc.metric = grep("timestamp|angle", colnames(metashort),
                        value = TRUE, invert = TRUE)[1]
      atest = as.numeric(as.matrix(metashort[, acc.metric]))
    }
    r2tempe = rep(r2, each = (n_short_in_mediumEpoch))
    r1tempe = rep(r1, each = (n_short_in_mediumEpoch))
    atest[which(r2tempe == 1 | r1tempe == 1)] = 0
    
    if (!is.null(study_date_indices)) {
      # If study_dates_file was used then r4 has possibly been trimmed
      # If this is the case then also trim atest to allow for direct comparisons
      tt1 = ((study_date_indices[1] - 1) * n_short_in_mediumEpoch) + 1
      tt2 = max(study_date_indices) * n_short_in_mediumEpoch
      atest = atest[tt1:tt2]
    }
    if (data_masking_strategy == 3) {
      # Find the most active ndayswindow block via a rolling mean window
      NDAYS = length(atest) / n_shortEpoch_perday
      atestlist = zoo::rollmean(x = atest, k = ndayswindow * n_shortEpoch_perday, align = "left")
      start_ndayswindow_hour = floor(which(atestlist == max(atestlist))[1] / n_shortEpoch_perhour)
      hrs.del.start = start_ndayswindow_hour + hrs.del.start
      maxdur = ((start_ndayswindow_hour / 24) + ndayswindow) - (hrs.del.end/24)
      if (maxdur > NDAYS) maxdur = NDAYS
      # Update r4 (which is medianEpoch size)
      if (hrs.del.start > 0) {
        start_epoch = max((hrs.del.start * n_medium_perhour) - 1, 1)
        r4[1:start_epoch] = 1
      }
      ignore_from = round((maxdur * n_mediumEpoch_perday))
      if (maxdur > 0 && length(r4) > (ignore_from + 1)) {
        r4[ignore_from:length(r4)] = 1
      }
      if (LD < 1440) r4 = r4[1:floor(LD / (mediumEpoch / 60))]
    } else if (data_masking_strategy == 5) {
      # Select the most active calendar days
      atestlist = c()
      # readjust midnightsi if study dates log used for trimming the data
      if (study_dates_log_used == TRUE) {
        midnightsi = midnightsi[midnightsi >= firstmidnighti & midnightsi <= lastmidnighti]
      }
      for (ati in 1:length(midnightsi)) {
        p0 = (midnightsi[ati] * n_short_in_mediumEpoch) - n_short_in_mediumEpoch + 1
        p1 = ((midnightsi[ati + ndayswindow] * n_short_in_mediumEpoch) - n_short_in_mediumEpoch)
        if (is.na(p1) || p1 > length(atest)) break
        atestlist[ati] = mean(atest[p0:p1], na.rm = TRUE)
      }
      # atik is the index where the most active ndayswindow starts
      atik = if (length(atestlist) > 0) which.max(atestlist) else 1
      
      offset = if (isTRUE(study_dates_log_used)) (firstmidnighti - 1) else 0
      # Ignore until
      ignore_until = midnightsi[atik] + (hrs.del.start * n_medium_perhour) - 1 - offset
      if (ignore_until > 0) r4[1:pmin(ignore_until, length(r4))] = 1
      # Ignore from
      target_idx = atik + ndayswindow
      if (target_idx > length(midnightsi)) target_idx = length(midnightsi)
      ignore_from = midnightsi[target_idx] - (hrs.del.end * n_medium_perhour) - offset
      if (!is.na(ignore_from) && ignore_from < length(r4)) {
        r4[pmax(1, ignore_from):length(r4)] = 1
      }
    }
    starttimei = 1
    endtimei = length(r4)
  } else if (data_masking_strategy == 4) { #from first midnight to end of recording
    starttime = firstmidnight
    endtime = lastmidnight  
    # only apply data_masking_strategy 4 if study dates log is not used for trimming the data,
    # otherwise the data already start and finishes at midnight
    if (study_dates_log_used == FALSE) {
      starttimei = firstmidnighti
      endtimei = lastmidnighti
      if (firstmidnighti != 1) { #ignore everything before the first midnight
        r4[1:(firstmidnighti - 1)] = 1 # -1 because first midnight 00:00 itself contributes to the first full day
      }
    } else {
      starttimei = 1
      endtimei = length(r4)
    }
  }
  # Mask data based on maxdur
  if (maxdur > 0 &&
      (length(r4) > (maxdur * n_mediumEpoch_perday) + 1)) {
    r4[((maxdur * n_mediumEpoch_perday) + 1):length(r4)] = 1
  }
  # Mask data based on max_calendar_days
  if (max_calendar_days > 0) {
    # get dates that are part of the study protocol
    if (study_dates_log_used == TRUE) {
      datetime = metalong$timestamp[firstmidnighti:(lastmidnighti - 1)]
    } else {
      datetime = metalong$timestamp
    }
    dates = as.Date(iso8601chartime2POSIX(datetime, tz = desiredtz))
    if (max_calendar_days < length(unique(dates))) {
      lastDateToInclude = sort(unique(dates))[max_calendar_days]
      r4[which(dates > lastDateToInclude)] = 1
    }
  }
  #===================================================================
  # if data selected based on study dates log, recover the original length of r4
  if (study_dates_log_used == TRUE) {
    r4_bu[which(r4_bu == 0),] = r4
    r4 = r4_bu
  }
  #========================================================================================
  # Impute shortEpoch second data based on mediumEpoch minute estimates of non-wear time
  r5 = r1 + r2 + r3 + r4
  r5[which(r5 > 1) ] = 1
  r5[which(metalong$nonwearscore == -1) ] = -1 # expanded data with expand_tail_max_hours
  r5long = matrix(0,length(r5), n_short_in_mediumEpoch) #r5long is the same as r5, but with more values per period of time
  r5long = replace(r5long, 1:length(r5long), r5)
  r5long = t(r5long)
  dim(r5long) = c(length(r5) * n_short_in_mediumEpoch,1)
  
  #------------------------------
  # detect which features have been calculated in part 1 and in what column they have ended up
  ENi = which(colnames(metashort) == "en")
  if (length(ENi) == 0) ENi = -1
  #==============================
  if (nrow(metashort) > length(r5long)) {
    metashort = metashort[1:length(r5long),]
  }
  wpd = 1440 * n_shortEpoch_permin #windows per day
  averageday = matrix(0, wpd, ncol(metashort) - 1)
  
  for (mi in 2:ncol(metashort)) {# generate 'average' day for each variable
    # The average day is used for imputation and defined relative to the starttime of the measurement
    # irrespective of dayborder as used in other parts of GGIR
    metr = as.numeric(as.matrix(metashort[, mi]))
    is.na(metr[which(r5long != 0)]) = T #turn all values of metr to na if r5long is different to 0 (it now leaves the expanded time with expand_tail_max out of the averageday calculation)
    imp = matrix(NA, wpd, ceiling(length(metr) / wpd)) #matrix used for imputation of seconds
    ndays = ncol(imp) #number of days (rounded upwards)
    nvalidsec = matrix(0, wpd, 1)
    dcomplscore = length(which(r5 == 0)) / length(r5)
    if (ndays > 1 ) { # only do imputation if there is more than 1 day of data #& length(which(r5 == 1)) > 1
      # all days except last one
      for (j in 1:(ndays - 1)) {
        imp[, j] = as.numeric(metr[(((j - 1) * wpd) + 1):(j * wpd)])
      }
      # last day
      lastday = metr[(((ndays - 1) * wpd) + 1):length(metr)]
      imp[1:length(lastday),ndays] = as.numeric(lastday)
      if (colnames(metashort)[mi] == "step_count") {
        # Median per row, which equals to median for one time point in the 'average' day
        # (median day would be a better term in this context)
        # chances are high that this will often be zero, because a person
        # would have to walk on a certain time point in the day for more than half of
        # each day in the to get a median above zero
        imp3 = apply(imp, 1, median, na.rm = TRUE)
      } else if (colnames(metashort)[mi] == "marker") {
        imp[is.na(imp)] <- 0 # to prevent imputation of marker data
        imp3 = apply(imp, 1, min, na.rm = TRUE)
      } else {
        # mean per row, which equals to mean or one time point in the 'average' day
        imp3 = rowMeans(imp, na.rm = TRUE)
      }
      dcomplscore = length(which(is.nan(imp3) == F | is.na(imp3) == F)) / length(imp3)
      
      if (length(imp3) < wpd)  {
        dcomplscore = dcomplscore * (length(imp3)/wpd)
      }
      if (ENi == mi) { #replace missing values for EN by 1
        imp3[which(is.nan(imp3) == T | is.na(imp3) == T)] = 1
      } else { #replace missing values for other metrics by 0
        imp3[which(is.nan(imp3) == T | is.na(imp3) == T)] = 0 # for those part of the data where there is no single data point for a certain part of the day (this is CRITICAL)
      }
      averageday[, (mi - 1)] = imp3
      for (j in 1:ndays) {
        missing = which(is.na(imp[,j]) == T)
        if (length(missing) > 0) {
          imp[missing,j] = imp3[missing]
        }
      }
      # imp is now the imputed time series
      dim(imp) = c(length(imp), 1)
      # but do not use imp for expanded time
      toimpute = which(r5long != -1)       # do not impute the expanded time with expand_tail_max_hours
      metashort[toimpute, mi] = as.numeric(imp[toimpute]) #to cut off the latter part of the last day used as a dummy data
    } else {
      dcomplscore = length(which(r5long == 0)) / wpd
    }
  }
  n_decimal_places = 4
  
  metashort[,2:ncol(metashort)] = round(metashort[,2:ncol(metashort)], digits = n_decimal_places)
  rout = data.frame(r1 = r1, r2 = r2, r3 = r3, r4 = r4, r5 = r5, stringsAsFactors = TRUE)
  invisible(list(metashort = metashort, rout = rout, r5long = r5long, dcomplscore = dcomplscore,
                 averageday = averageday, windowsizes = windowsizes, data_masking_strategy = data_masking_strategy,
                 LC = LC, LC2 = LC2, hrs.del.start = hrs.del.start, hrs.del.end = hrs.del.end,
                 maxdur = maxdur, nonwearHoursFiltered = nonwearHoursFiltered,
                 nonwearEventsFiltered = nonwearEventsFiltered))
}
