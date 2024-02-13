get_starttime_weekday_truncdata = function(monc, dformat, data, 
                                           header, desiredtz, sf, datafile,
                                           ws2, configtz = NULL) {
  # ensures that first window starts at logical timepoint relative to its size
  # (15,30,45 or 60 minutes of each hour)
  start_meas = ws2/60 
  
  starttime = g.getstarttime( # this will return a POSIXlt object
    datafile = datafile,
    data = data,
    mon = monc,
    dformat = dformat,
    desiredtz = desiredtz,
    configtz = configtz
  )

  # assess weekday
  wday = starttime$wday #day of the week 0-6 and 0 is Sunday
  wday = wday + 1
  weekdays = c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday")
  wdayname = weekdays[wday]

  # assess how much data to delete till next 15 minute period
  secshift = 60 - starttime$sec # shift in seconds needed
  if (secshift == 60) {
    secshift = 0 # if starttime is 00:00 then we do not want to remove data
  } else {
    starttime$min = starttime$min + 1 # shift in minutes needed (+1 one to account for seconds comp)
  }

  minshift = start_meas - (((starttime$min/start_meas) - floor(starttime$min/start_meas)) * start_meas)
  if (minshift == start_meas) {
    minshift = 0
  }

  sampleshift = (minshift * 60 * sf) + (secshift * sf) # derive sample shift
  sampleshift = floor(sampleshift)
  if (sampleshift > 1) {
    data = data[-c(1 : sampleshift),] # delete data accordingly
  }

  # recalculate the timestamp
  starttime$min = starttime$min + minshift # if the result is >= 60, hours (and possibly date) will adjust automatically
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
