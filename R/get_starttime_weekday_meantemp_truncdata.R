get_starttime_weekday_meantemp_truncdata = function(monc, dformat, data, 
                                                    P, header, desiredtz, sf, datafile,
                                                    ws2, starttime, wday, wdayname, configtz = NULL) {
  #ensures that first window starts at logical timepoint relative to its size
  # (15,30,45 or 60 minutes of each hour)
  start_meas = ws2/60 
  # extraction and modification of starting point of measurement
  starttime = g.getstarttime(
    datafile = datafile,
    P = P,
    mon = monc,
    dformat = dformat,
    desiredtz = desiredtz,
    configtz = configtz
  )
  if (exists("P")) {
    rm(P); gc()
  }

  # assess weekday
  wday = starttime$wday #day of the week 0-6 and 0 is Sunday
  wday = wday + 1
  weekdays = c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday")
  wdayname = weekdays[wday]

  # assess how much data to delete till next 15 minute period

  start_min = starttime$min
  start_sec = starttime$sec

  secshift = 60 - start_sec #shift in seconds needed
  if (secshift != 60) {
    start_min = start_min + 1 #shift in minutes needed (+1 one to account for seconds comp)
  }
  if (secshift == 60) secshift = 0 # if starttime is 00:00 then we do not want to remove data

  minshift = start_meas - (start_min %% start_meas)
  if (minshift == start_meas) {
    minshift = 0
  }

  sampleshift = ((minshift)*60*sf) + (secshift*sf) #derive sample shift
  sampleshift = floor(sampleshift)
  if (sampleshift > 1) {
    data = data[-c(1 : sampleshift),] # delete data accordingly
  }

  # recalculate the timestamp
  starttime$min = start_min + minshift # if the result is >= 60, hours (and possibly date) will adjust automatically
  starttime$sec = 0

  invisible(
    list(
      starttime = starttime,
      wday = wday,
      wdayname = wdayname,
      data = data
    )
  )
}
