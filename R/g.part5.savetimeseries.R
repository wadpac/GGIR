g.part5.savetimeseries = function(ts, LEVELS, desiredtz, rawlevels_fname) {
  Nts = nrow(ts)
  ms5rawlevels = data.frame(date_time = ts$time, class_id = LEVELS,
                            # class_name = rep("",Nts),
                            stringsAsFactors = FALSE)
  ts$timestamp = as.POSIXlt(ts$time, format="%Y-%m-%dT%H:%M:%S%z",tz=desiredtz)
  ms5rawlevels$date_time = as.POSIXlt(ms5rawlevels$date_time, 
                                      format="%Y-%m-%dT%H:%M:%S%z",tz=desiredtz)
  # Create numeric time to faciltiate merging
  ts$timenum = as.numeric(ts$timestamp)
  ms5rawlevels$timenum = as.numeric(ms5rawlevels$date_time)
  mdat = merge(ts, ms5rawlevels, by = "timenum")
  rm(ts, ms5rawlevels)
  names(mdat)[which(names(mdat) == "nonwear")] = "invalidepoch"
  names(mdat)[which(names(mdat) == "diur")] = "SleepPeriodTime"
  mdat = mdat[,-which(names(mdat) == "date_time")]
  # Add invalid day indicator
  mdat$invalid_wakinghours = mdat$invalid_sleepperiod =  mdat$invalid_fullwindow= 1
  wakeup = which(diff(c(mdat$SleepPeriodTime,0)) == -1) + 1 # first epoch of each day
  if (length(wakeup) > 0) {
    for (di in 1:(length(wakeup)-1)) {
      dayindices = wakeup[di]:(wakeup[di+1]-1)
      wake = which(mdat$SleepPeriodTime[dayindices] == 0)
      sleep = which(mdat$SleepPeriodTime[dayindices] == 1)
      mdat$invalid_wakinghours[dayindices] = mean(mdat$invalidepoch[dayindices[wake]])
      mdat$invalid_sleepperiod[dayindices] = mean(mdat$invalidepoch[dayindices[sleep]])
      mdat$invalid_fullwindow[dayindices] = mean(mdat$invalidepoch[dayindices])
    }
    # round acceleration values to 3 digits to reduce storage space
    mdat$ACC = round(mdat$ACC, digits= 3)
    # remove irrelevant columns and rows
    mdat = mdat[-which(mdat$invalid_fullwindow == 1),] # remove days from which we already know that they are not going to be included (first and last day)
    mdat$guider = ifelse(mdat$guider =='sleeplog', yes = 1, # digitize guider to save storage space
                         no = ifelse(mdat$guider == 'HDCZA', yes = 2,
                                     no =  ifelse(mdat$guider == 'setwindow', yes = 3,
                                                  no = ifelse(mdat$guider == 'L512', yes = 4,
                                                              no = 0))))
    mdat = mdat[,-which(names(mdat) %in% c("timestamp","time","invalidepoch"))]
    # re-oder columns
    naS = colnames(mdat)
    # mdat = mdat[,c(which(naS == "filename"),which(naS != "filename"))]
    # save to csv file
    write.csv(mdat,rawlevels_fname, row.names = F)
    #===============================
    rm(ms5rawlevels, mdat)
  }
  return()
}