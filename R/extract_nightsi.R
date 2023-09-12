extract_nightsi = function(ts, dayborder, desiredtz, tail_expansion_log) {
  time_POSIX = iso8601chartime2POSIX(ts$time,tz = desiredtz)
  tempp = as.POSIXlt(time_POSIX) #unclass(time_POSIX)
  if (is.na(tempp$sec[1]) == TRUE) {
    time_POSIX = as.POSIXct(ts$time, tz = params_general[["desiredtz"]])
    tempp = as.POSIXlt(time_POSIX)
  }
  sec = tempp$sec
  min = tempp$min
  hour = tempp$hour
  nightsi = nightsi2 = which(sec == 0 & min == 0 & hour == 0)
  #shift the definition of midnight if required
  if (dayborder != 0) {
    nightsi = which(sec == 0 & min == (dayborder - floor(dayborder)) * 60 & hour == floor(dayborder)) 
  }
  # include last window if has been expanded and not present in ts
  if (length(tail_expansion_log) != 0 & nrow(ts) > max(nightsi)) nightsi[length(nightsi) + 1] = nrow(ts)
  # return
  return(list(nightsi = nightsi, nightsi2 = nightsi2, time_POSIX = time_POSIX,
              sec = sec, min = min, hour = hour))
}