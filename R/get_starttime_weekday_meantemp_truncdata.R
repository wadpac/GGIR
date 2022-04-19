get_starttime_weekday_meantemp_truncdata = function(temp.available, monc, dformat, data, selectdaysfile,
                                                    P, header, desiredtz, sf, i, datafile,
                                                    ws2, starttime, wday, weekdays, wdayname) {
  #ensures that first window starts at logical timepoint relative to its size
  # (15,30,45 or 60 minutes of each hour)
  start_meas = ws2/60 
  if (temp.available == TRUE) {
    use.temp = TRUE
  } else {
    use.temp = FALSE
  }
  meantemp =c()
  if (monc == 2 | (monc == 4 & dformat == 4) | monc == 5 | (monc == 0 & use.temp == TRUE)) {
    if (monc == 2) tempcolumn = 7
    if (monc == 4 | monc == 0) tempcolumn = 5
    if (monc == 5) tempcolumn = 4
    meantemp = mean(as.numeric(data[,tempcolumn]),na.rm=TRUE)
    if (is.na(meantemp) == T) { #mean(as.numeric(data[1:10,7]))
      cat("\ntemperature is NA\n")
      meantemp = 0
      use.temp = FALSE
    } else if (mean(as.numeric(data[1:10,tempcolumn])) > 50) {
      cat("\ntemperature value is unreaslistically high (> 50 Celcius)\n")
      meantemp = 0
      use.temp = FALSE
    }
  }
  # extraction and modification of starting point of measurement
  if (i == 1 | (i != 1 & length(selectdaysfile) > 0)) { #only do this for first block of data
    starttime = g.getstarttime(datafile=datafile,P=P,header=header,mon=monc,
                               dformat=dformat,desiredtz=desiredtz,selectdaysfile=selectdaysfile)
    if (exists("P")) rm(P); gc()
    #==================================================
    #inspection timezone
    timezone = attr(unclass(as.POSIXlt(starttime[1])),which="tzone")
    starttimebefore = as.POSIXlt(starttime)
    # assuming that timestamps is good, but that timezone might be lost in conversion from string to POSIXct
    if (dformat == 1) { #not sure whether this is required for csv-format (2)
      if (length(which(timezone == "GMT")) > 0) {
        if (length(desiredtz) == 0) {
          print("desiredtz not specified, local timezoneused as default")
          desiredtz = ""
        }
        starttime = as.POSIXlt(starttime[1],tz=desiredtz)
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
    temp = unlist(strsplit(as.character(starttime)," "))
    if (length(temp) > 1) {
      starttime2 = as.numeric(unlist(strsplit(temp[2],":")))
    } else {
      # first get char to POSIX
      temp = iso8601chartime2POSIX(starttime,tz=desiredtz)
      temp = unlist(strsplit(as.character(temp)," ")) # to keep it consistent with what we had
      starttime2 = as.numeric(unlist(strsplit(as.character(temp[2]),":")))
    }
    if (length(which(is.na(starttime2) ==  TRUE)) > 0 | length(starttime2) ==0) {
      starttime2 = c(0,0,0)
    }
    start_hr = as.numeric(starttime2[1])
    start_min = as.numeric(starttime2[2])
    start_sec = as.numeric(starttime2[3])
    
    secshift = 60 - start_sec #shift in seconds needed
    if (secshift != 60) {
      start_min = start_min +1 #shift in minutes needed (+1 one to account for seconds comp)
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
    newmin = start_min+minshift #recalculate first timestamp
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
    starttime3 = paste(temp[1]," ",start_hr,":",newmin,":",newsec,sep="")
    #create timestamp from string (now desiredtz is added)
    if (length(desiredtz) == 0) {
      print("desiredtz not specified, local timezone used as default")
      desiredtz = ""
    }
    starttime_a = as.POSIXct(starttime3,format="%d/%m/%Y %H:%M:%S",tz=desiredtz) #,origin="1970-01-01"
    starttime_b = as.POSIXct(starttime3,format="%d-%m-%Y %H:%M:%S",tz=desiredtz) #,origin="1970-01-01"
    starttime_c = as.POSIXct(starttime3,format="%Y/%m/%d %H:%M:%S",tz=desiredtz) #,origin="1970-01-01"
    starttime_d = as.POSIXct(starttime3,format="%Y-%m-%d %H:%M:%S",tz=desiredtz) #,origin="1970-01-01"
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
            cat("\ndate not recognized\n")
          }
        }
      }
    }
    if (remem2add24 == TRUE) {
      starttime = as.POSIXlt(as.numeric(starttime) + (24*3600),origin="1970-01-01")
    }
  }
  invisible(list(starttime=starttime, meantemp=meantemp, use.temp=use.temp, wday=wday,
                 weekdays=weekdays, wdayname=wdayname, desiredtz=desiredtz, data=data))
}
