get_starttime_weekday_meantemp_truncdata = function(temp.available, monc, dformat, data, 
                                                    P, header, desiredtz, sf, i, datafile,
                                                    ws2, starttime, wday, wdayname, configtz = NULL) {
  #ensures that first window starts at logical timepoint relative to its size
  # (15,30,45 or 60 minutes of each hour)
  start_meas = ws2/60 
  if (temp.available == TRUE) {
    use.temp = TRUE
  } else {
    use.temp = FALSE
  }
  meantemp = c()
  if (monc == MONITOR$GENEACTIV || (monc == MONITOR$AXIVITY && dformat == FORMAT$CWA) || monc == MONITOR$MOVISENS || (monc == MONITOR$AD_HOC && use.temp == TRUE)) {
    if (monc == MONITOR$GENEACTIV) {
      if ("temperature" %in% colnames(data)) {
        tempcolumn = which(colnames(data) == "temperature") #GGIRread
      } else {
        tempcolumn = 7
      }
    }
    if (monc == MONITOR$AXIVITY || monc == MONITOR$AD_HOC) tempcolumn = 5
    if (monc == MONITOR$MOVISENS) tempcolumn = 4
    meantemp = mean(as.numeric(data[, tempcolumn]), na.rm = TRUE)
    if (is.na(meantemp) == T) { #mean(as.numeric(data[1:10,7]))
      warning("temperature is NA", call. = FALSE)
      meantemp = 0
      use.temp = FALSE
    } else if (mean(as.numeric(data[1:10,tempcolumn])) > 50) {
      warning("temperature value is unreaslistically high (> 50 Celcius)", call. = FALSE)
      meantemp = 0
      use.temp = FALSE
    }
  }
  # extraction and modification of starting point of measurement
  if (i == 1) { #only do this for first block of data
    starttime = g.getstarttime(
      datafile = datafile,
      P = P,
      header = header,
      mon = monc,
      dformat = dformat,
      desiredtz = desiredtz,
      configtz = configtz
    )
    if (exists("P")) rm(P); gc()
    #==================================================
    #inspection timezone
    timezone = attr(unclass(as.POSIXlt(starttime[1])), which = "tzone")
    starttimebefore = as.POSIXlt(starttime)
    # assuming that timestamps is good, but that timezone might be lost in conversion from string to POSIXct
    if (dformat == FORMAT$BIN) { #not sure whether this is required for csv-format (2)
      if (length(which(timezone == "GMT")) > 0) {
        if (length(desiredtz) == 0) {
          warning("desiredtz not specified, local timezoneused as default", call. = FALSE)
          desiredtz = ""
        }
        starttime = as.POSIXlt(starttime[1], tz = desiredtz)
      }
    }
    #================================================
    #assess weekday
    wday = unclass(as.POSIXlt(starttime[1]))$wday #day of the week 0-6 and 0 is Sunday
    wday = wday + 1
    weekdays = c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday")
    wdayname = weekdays[wday]
    #======================================================
    #assess how much data to delete till next 15 minute period
    tmp = unlist(strsplit(format(starttime)," "))
    if (length(tmp) > 1) {
      starttime2 = as.numeric(unlist(strsplit(tmp[2],":")))
    } else if (length(tmp) == 1 && !grepl("T", tmp)) {
      starttime2 = as.numeric(unlist(strsplit(tmp[2],":")))
    } else {
      # first get char to POSIX
      tmp = iso8601chartime2POSIX(starttime, tz = desiredtz)
      tmp = unlist(strsplit(format(tmp)," ")) # to keep it consistent with what we had
      starttime2 = as.numeric(unlist(strsplit(format(tmp[2]),":")))
    }
    if (length(which(is.na(starttime2) ==  TRUE)) > 0 |
        length(starttime2) == 0) {
      starttime2 = c(0,0,0)
    }
    start_hr = as.numeric(starttime2[1])
    start_min = as.numeric(starttime2[2])
    start_sec = as.numeric(starttime2[3])
    
    secshift = 60 - start_sec #shift in seconds needed
    if (secshift != 60) {
      start_min = start_min + 1 #shift in minutes needed (+1 one to account for seconds comp)
    }
    if (secshift == 60) secshift = 0 # if starttime is 00:00 then we do not want to remove data
    minshift = start_meas - (((start_min/start_meas) - floor(start_min/start_meas)) * start_meas)
    if (minshift == start_meas) {
      minshift = 0;
      
    }
    sampleshift = ((minshift)*60*sf) + (secshift*sf) #derive sample shift
    if (floor(sampleshift) > 1) {
      data = data[-c(1:floor(sampleshift)),] #delete data accordingly
    }
    newmin = start_min + minshift #recalculate first timestamp
    newsec = 0
    remem2add24 = FALSE
    if (newmin >= 60) {
      newmin = newmin - 60
      start_hr = start_hr + 1
      if (start_hr == 24) { #if measurement is started in 15 minutes before midnight
        start_hr = 0
        remem2add24 = TRUE #remember to add 24 hours because this is now the wrong day
      }
    }
    starttime3 = paste0(tmp[1], " ", start_hr, ":", newmin, ":", newsec)
    #create timestamp from string (now desiredtz is added)
    if (length(desiredtz) == 0) {
      warning("desiredtz not specified, local timezone used as default", call. = FALSE)
      desiredtz = ""
    }
    starttime_a = as.POSIXct(starttime3, format = "%d/%m/%Y %H:%M:%S", tz = desiredtz)
    starttime_b = as.POSIXct(starttime3, format = "%d-%m-%Y %H:%M:%S", tz = desiredtz)
    starttime_c = as.POSIXct(starttime3, format = "%Y/%m/%d %H:%M:%S", tz = desiredtz)
    starttime_d = as.POSIXct(starttime3, format = "%Y-%m-%d %H:%M:%S", tz = desiredtz)
    if (is.na(starttime_a) == FALSE) {
      starttime = starttime_a
    } else {
      if (is.na(starttime_b) == FALSE) {
        starttime = starttime_b
      } else {
        if (is.na(starttime_c) == FALSE) {
          starttime = starttime_c
        } else {
          if (is.na(starttime_d) == FALSE) {
            starttime = starttime_d
          } else {
            warning("date not recognized")
          }
        }
      }
    }
    if (remem2add24 == TRUE) {
      starttime = as.POSIXlt(as.numeric(starttime) + (24 * 3600), origin = "1970-01-01")
    }
  }
  invisible(
    list(
      starttime = starttime,
      meantemp = meantemp,
      use.temp = use.temp,
      wday = wday,
      wdayname = wdayname,
      desiredtz = desiredtz,
      data = data
    )
  )
}
