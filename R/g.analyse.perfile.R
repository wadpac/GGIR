g.analyse.perfile = function(ID, fname, deviceSerialNumber, sensor.location, startt, I, LC2, LD, dcomplscore,
                             LMp, LWp, C, lookat, AveAccAve24hr, colnames_to_lookat, QUAN, ML5AD,
                             ML5AD_names, igfullr, igfullr_names,
                             daysummary, ds_names, includedaycrit, strategy, hrs.del.start,
                             hrs.del.end, maxdur, windowsizes, idloc, snloc, wdayname, doquan,
                             qlevels_names, doiglevels, tooshort, InterdailyStability, IntradailyVariability,
                             IVIS_windowsize_minutes, qwindow, longitudinal_axis_id) {
  filesummary = matrix(" ",1,100) #matrix to be stored with summary per participant
  s_names = rep(" ",ncol(filesummary))
  vi = 1
  # Person identification number
  filesummary[vi] = ID
  # Identify which of the metrics are in g-units to aid deciding whether to multiply by 1000
  g_variables_lookat = lookat[grep(x = colnames_to_lookat, pattern = "BrondCounts|ZCX|ZCY", invert = TRUE)]

  # Serial number
  if (snloc == 1) {
    filesummary[(vi+1)] = deviceSerialNumber
  } else if (snloc == 2) {
    filesummary[(vi+1)] = unlist(strsplit(fname,"_"))[2]
  }
  s_names[vi:(vi+1)] = c("ID","device_sn")
  vi = vi+2
  # starttime of measurement, body location, filename
  filesummary[vi] = sensor.location
  filesummary[(vi+1)] = fname
  filesummary[(vi+2)] = startt # starttime of measurement
  s_names[vi:(vi+2)] = c("bodylocation","filename","start_time")
  vi = vi+3
  # weekday on which measurement started, sample frequency and device
  filesummary[vi] = wdayname 
  filesummary[(vi+1)] = I$sf
  filesummary[(vi+2)] = I$monn
  s_names[vi:(vi+2)] = c("startday","samplefreq","device")
  vi = vi+3
  # clipsing score and measurement duration in days
  filesummary[vi] = LC2  / ((LD/1440)*96)
  filesummary[(vi+1)] = LD/1440 #measurement duration in days
  s_names[vi:(vi+1)] = c("clipping_score",
                         "meas_dur_dys")
  vi = vi+2
  # completeness core and measurement duration with different definitions
  filesummary[vi] = dcomplscore #completeness of the day
  filesummary[(vi+1)] = LMp/1440 #measurement duration according to protocol
  filesummary[(vi+2)] = LWp/1440 #wear duration in days (out of measurement protocol)
  s_names[vi:(vi+2)] = c("complete_24hcycle", # day (fraction of 24 hours for which data is available at all)
                         "meas_dur_def_proto_day","wear_dur_def_proto_day")
  vi = vi+3
  # calibration error after auto-calibration
  if (length(C$cal.error.end) == 0)   C$cal.error.end = c(" ")
  filesummary[vi] = C$cal.error.end
  filesummary[vi+1] = C$QCmessage
  for (la in 1:length(lookat)) {
    AveAccAve24hr[la] = 	AveAccAve24hr[la] * ifelse(test = lookat[la] %in% g_variables_lookat, yes = 1000, no = 1)
  }
  q0 = length(AveAccAve24hr) + 1
  filesummary[(vi+2):(vi+q0)] = AveAccAve24hr
  colnames_to_lookat = paste0(colnames_to_lookat,"_fullRecordingMean")
  s_names[vi:(vi+q0)] = c("calib_err",
                          "calib_status",colnames_to_lookat)
  vi = vi+q0+2
  #quantile, ML5, and intensity gradient variables
  if (doquan == TRUE) {
    q1 = length(QUAN)
    filesummary[vi:((vi-1)+q1)] = QUAN * ifelse(test = lookat[la] %in% g_variables_lookat, yes = 1000, no = 1)
    s_names[vi:((vi-1)+q1)] = paste0(qlevels_names,"_fullRecording")
    vi = vi + q1
    q1 = length(ML5AD)
    filesummary[vi:((vi-1)+q1)] = as.numeric(ML5AD)
    s_names[vi:((vi-1)+q1)] = paste0(ML5AD_names,"_fullRecording")
    vi = vi + q1
  }
  if (doiglevels == TRUE) { 
    # intensity gradient (as described by Alex Rowlands 2018)
    # applied to the averageday per metric (except from angle metrics)
    q1 = length(igfullr)
    filesummary[vi:((vi-1)+q1)] = igfullr
    s_names[vi:((vi-1)+q1)] = paste0(igfullr_names,"_fullRecording")
    vi = vi + q1
  }
  if (tooshort == 0) {
    #====================================================================
    # Summarise per recording (not per day) - additional variables if the recording is long enough
    #====================================================================
    # Recognise weekenddays with enough data
    wkend  = which(daysummary[,which(ds_names == "weekday")] == "Saturday" | daysummary[,which(ds_names == "weekday")] == "Sunday")
    columnWithAlwaysData = which(ds_names == "N hours" | ds_names == "N_hours")
    NVHcolumn = which(ds_names == "N valid hours" | ds_names == "N_valid_hours" ) #only count in the days for which the inclusion criteria is met
    v1 = which(is.na(as.numeric(daysummary[wkend,columnWithAlwaysData])) == F &
                 as.numeric(daysummary[wkend,NVHcolumn]) >= includedaycrit)
    wkend = wkend[v1]
    wkday  = which(daysummary[,which(ds_names == "weekday")] != "Saturday" & daysummary[,which(ds_names == "weekday")] != "Sunday")
    v2 = which(is.na(as.numeric(daysummary[wkday,columnWithAlwaysData])) == F  &
                 as.numeric(daysummary[wkday,NVHcolumn]) >= includedaycrit)
    wkday = wkday[v2]
    # Add number of weekend and weekdays to filesummary
    filesummary[vi:(vi+1)] = c(length(wkend),  length(wkday)) # number of weekend days & weekdays
    iNA = which(is.na(filesummary[vi:(vi+1)]) == TRUE)
    if (length(iNA) > 0) filesummary[(vi:(vi+1))[iNA]] = 0
    s_names[vi:(vi+1)] = c("N valid WEdays","N valid WKdays")
    vi = vi + 2
    # Add ISIV to filesummary
    filesummary[vi:(vi+2)] = c(InterdailyStability, IntradailyVariability,
                               IVIS_windowsize_minutes)
    iNA = which(is.na(filesummary[vi:(vi+3)]) == TRUE)
    if (length(iNA) > 0) filesummary[(vi:(vi+3))[iNA]] = " "
    s_names[vi:(vi+2)] = c("IS_interdailystability", "IV_intradailyvariability", "IVIS_windowsize_minutes")
    vi = vi + 4
    # Variables per metric - summarise with stratification to weekdays and weekend days
    daytoweekvar = c(5:length(ds_names))
    md = which(ds_names[daytoweekvar] %in% c("measurementday", "weekday", "qwindow_timestamps", "qwindow_names"))
    if (length(md) > 0) daytoweekvar = daytoweekvar[-md]

    dtwtel = 0
    if (length(daytoweekvar) >= 1) {
      sp = length(daytoweekvar) + 1
      for (dtwi in daytoweekvar) {
        #check whether columns is empty:
        uncona = unique(daysummary[,dtwi])
        storevalue = !(length(uncona) == 1 & length(qwindow) > 2 & uncona[1] == "")
        if (is.na(storevalue) == TRUE) storevalue = FALSE
        # Only do next 15-isch lines of code if:
        # - there is more than 1 day of data
        # - there are multiple daysegments (qwindow)
        # - first value is not empty
        if (storevalue == TRUE) {
          # Plain average of available days
          v4 = mean(suppressWarnings(as.numeric(daysummary[,dtwi])),na.rm=TRUE) 
          filesummary[(vi+1+(dtwtel*sp))] = v4 # #average all availabel days
          s_names[(vi+1+(dtwtel*sp))] = paste("AD_",ds_names[dtwi],sep="")
          # Average of available days per weekenddays / weekdays:
          dtw_wkend = suppressWarnings(as.numeric(daysummary[wkend,dtwi]))
          dtw_wkday = suppressWarnings(as.numeric(daysummary[wkday,dtwi]))
          filesummary[(vi+2+(dtwtel*sp))] = suppressWarnings(mean(dtw_wkend,na.rm=TRUE))
          filesummary[(vi+3+(dtwtel*sp))] = suppressWarnings(mean(dtw_wkday,na.rm=TRUE))
          s_names[(vi+2+(dtwtel*sp))] = paste0("WE_",ds_names[dtwi]) # Weekend no weighting
          s_names[(vi+3+(dtwtel*sp))] = paste0("WD_",ds_names[dtwi]) # Weekdays no weighting
          # Weighted average of available days per weekenddays / weekdays:
          if (length(dtw_wkend) > 2) {
            dtw_wkend = c((dtw_wkend[1]+dtw_wkend[3])/2,dtw_wkend[2])
          }
          if (length(dtw_wkday) > 5) {
            dtw_wkday = c((dtw_wkday[1]+dtw_wkday[6])/2,dtw_wkday[2:5])
          }
          filesummary[(vi+4+(dtwtel*sp))] = suppressWarnings(mean(dtw_wkend,na.rm=TRUE))
          filesummary[(vi+5+(dtwtel*sp))] = suppressWarnings(mean(dtw_wkday,na.rm=TRUE))
          s_names[(vi+4+(dtwtel*sp))] = paste("WWE_",ds_names[dtwi],sep="") # Weekend with weighting
          s_names[(vi+5+(dtwtel*sp))] = paste("WWD_",ds_names[dtwi],sep="") # Weekdays with weighting
          dtwtel = dtwtel + 1
        }
        # Plain average of available days
        v4 = mean(suppressWarnings(as.numeric(daysummary[,dtwi])),na.rm=TRUE) 
        filesummary[(vi+1+(dtwtel*sp))] = v4
        s_names[(vi+1+(dtwtel*sp))] = paste("AD_",ds_names[dtwi],sep="")
        # Average of available days per weekenddays / weekdays:
        dtw_wkend = suppressWarnings(as.numeric(daysummary[wkend,dtwi]))
        dtw_wkday = suppressWarnings(as.numeric(daysummary[wkday,dtwi]))
        if (storevalue == TRUE) {
          filesummary[(vi+2+(dtwtel*sp))] = mean(dtw_wkend,na.rm=TRUE)
          filesummary[(vi+3+(dtwtel*sp))] = mean(dtw_wkday,na.rm=TRUE)
        }
        s_names[(vi+2+(dtwtel*sp))] = paste("WE_",ds_names[dtwi],sep="") # Weekend no weighting
        s_names[(vi+3+(dtwtel*sp))] = paste("WD_",ds_names[dtwi],sep="") # Weekdays no weighting
        # Weighted average of available days per weekenddays / weekdays:
        if (length(dtw_wkend) > 2) {
          dtw_wkend = c((dtw_wkend[1]+dtw_wkend[3])/2,dtw_wkend[2])
        }
        if (length(dtw_wkday) > 5) {
          dtw_wkday = c((dtw_wkday[1]+dtw_wkday[6])/2,dtw_wkday[2:5])
        }
        if (storevalue == TRUE) {
          filesummary[(vi+4+(dtwtel*sp))] = mean(dtw_wkend,na.rm=TRUE) # Weekend with weighting
          filesummary[(vi+5+(dtwtel*sp))] = mean(dtw_wkday,na.rm=TRUE) # Weekdays with weighting
        }
        s_names[(vi+4+(dtwtel*sp))] = paste("WWE_",ds_names[dtwi],sep="")
        s_names[(vi+5+(dtwtel*sp))] = paste("WWD_",ds_names[dtwi],sep="")
        dtwtel = dtwtel + 1
      }
      vi = vi+6+((dtwtel*sp)-1)
    }
    filesummary[vi] = strategy
    filesummary[(vi+1)] = hrs.del.start
    filesummary[(vi+2)] = hrs.del.end
    filesummary[(vi+3)] = maxdur
    filesummary[(vi+4)] = windowsizes[1]
    filesummary[(vi+5)] = longitudinal_axis_id
    #get GGIR version
    SI = sessionInfo()
    GGIRversion = c()
    try(expr = {GGIRversion = SI$loadedOnly$GGIR$Version},silent=TRUE)
    if (length(GGIRversion) == 0) {
      try(expr = {GGIRversion = SI$otherPkgs$GGIR$Version},silent=TRUE)
    }
    # GGIRversion = SI$otherPkgs$GGIR$Version
    if (length(GGIRversion) == 0) GGIRversion = "GGIR not used"
    filesummary[(vi+6)] = GGIRversion #"2014-03-14 12:14:00 GMT"
    s_names[vi:(vi+6)] = as.character(c(paste0("data exclusion stategy (value=1, ignore specific hours;",
                                               " value=2, ignore all data before the first midnight and",
                                               " after the last midnight)"),
                                        "n hours ignored at start of meas (if strategy=1)",
                                        "n hours ignored at end of meas (if strategy=1)",
                                        "n days of measurement after which all data is ignored (if strategy=1)",
                                        "epoch size to which acceleration was averaged (seconds)",
                                        "if_hip_long_axis_id", "GGIR version"))
    vi = vi + 6
  }
  rm(LD); rm(ID)
  # tidy up daysummary object
  mw = which(is.na(daysummary) == T)
  mw = c(mw, grep(pattern = "NaN", x = daysummary))
  if (length(mw) > 0) {
    daysummary[mw] = " "
  }
  cut = which(ds_names == " " | ds_names == "" | is.na(ds_names)==T)
  if (length(cut > 0)) {
    ds_names = ds_names[-cut]
    daysummary = daysummary[,-cut]
  }
  if(min(dim(as.matrix(daysummary))) == 1) {
    if (nrow(as.matrix(daysummary)) != 1) {
      daysummary = t(daysummary) #if there is only one day of data
    }
  }
  daysummary = data.frame(value=daysummary,stringsAsFactors=FALSE)
  names(daysummary) = ds_names
  # remove double columns with 1-6am variables
  columnswith16am = grep("1-6am",x=colnames(daysummary))
  if (length(columnswith16am) > 1) {
    daysummary = daysummary[,-columnswith16am[2:length(columnswith16am)]]
  }
  # tidy up filesummary object
  mw = which(is.na(filesummary) == T)
  mw = c(mw, grep(pattern = "NaN", x = filesummary))
  if (length(mw) > 0) {
    filesummary[mw] = " "
  }
  cut = which(as.character(s_names) == " " | as.character(s_names) == "" | is.na(s_names)==T | duplicated(s_names) |
                s_names %in% c("AD_", "WE_", "WD_", "WWD_", "WWE_",
                               "AD_N hours", "WE_N hours", "WD_N hours", "WWD_N hours", "WWE_N hours",
                               "AD_N valid hours", "WE_N valid hours", "WD_N valid hours", "WWD_N valid hours", "WWE_N valid hours"))
  if (length(cut) > 0) {
    s_names = s_names[-cut]
    filesummary = filesummary[-cut]
  }
  filesummary = data.frame(value = t(filesummary), stringsAsFactors = FALSE) #needs to be t() because it will be a column otherwise
  names(filesummary) = s_names
  
  columns2order = c()
  if (ncol(filesummary) > 37) {
    columns2order = grep(pattern = "AD_|WE_|WD_|WWD_|WWE_", x = names(filesummary))
  }
  options(encoding = "UTF-8")
  if (length(columns2order) > 0) {
    selectcolumns = c(names(filesummary)[1:(columns2order[1] - 1)],
                      grep(pattern = "^AD_", x = names(filesummary), value = T),
                      grep(pattern = "^WD_", x = names(filesummary), value = T),
                      grep(pattern = "^WE_", x = names(filesummary), value = T),
                      grep(pattern = "^WWD_", x = names(filesummary), value = T),
                      grep(pattern = "^WWE_", x = names(filesummary), value = T),
                      names(filesummary)[(columns2order[length(columns2order)] + 1):ncol(filesummary)])
  } else {
    selectcolumns = names(filesummary)
  }
  selectcolumns = selectcolumns[which(selectcolumns %in% colnames(filesummary) == TRUE)]
  filesummary = filesummary[,selectcolumns]
  filesummary = filesummary[,!duplicated(filesummary)]
  invisible(list(filesummary = filesummary, daysummary = daysummary))
}
