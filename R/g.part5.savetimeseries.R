g.part5.savetimeseries = function(ts, LEVELS, desiredtz, rawlevels_fname,
                                  save_ms5raw_format="csv", save_ms5raw_without_invalid=TRUE,
                                  DaCleanFile = c(),
                                  includedaycrit.part5= 2/3, ID = c()) {
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
  mdat$invalid_wakinghours = mdat$invalid_sleepperiod =  mdat$invalid_fullwindow = 100
  wakeup = which(diff(c(mdat$SleepPeriodTime,0)) == -1) + 1 # first epoch of each day
  if (length(wakeup) > 1) {
    for (di in 1:(length(wakeup)-1)) {
      dayindices = wakeup[di]:(wakeup[di+1]-1)
      wake = which(mdat$SleepPeriodTime[dayindices] == 0)
      sleep = which(mdat$SleepPeriodTime[dayindices] == 1)
      mdat$invalid_wakinghours[dayindices] = round(mean(mdat$invalidepoch[dayindices[wake]])*100, digits=2)
      mdat$invalid_sleepperiod[dayindices] = round(mean(mdat$invalidepoch[dayindices[sleep]])*100, digits=2)
      mdat$invalid_fullwindow[dayindices] = round(mean(mdat$invalidepoch[dayindices])*100, digits=2)
    }
    # round acceleration values to 3 digits to reduce storage space
    mdat$ACC = round(mdat$ACC, digits= 3)
    if (save_ms5raw_without_invalid == TRUE) {
      # Remove days based on data_cleaning_file
      if (length(DaCleanFile) > 0) { 
        if (ID %in% DaCleanFile$ID) {
          days2exclude = DaCleanFile$day_part5[which(DaCleanFile$ID == ID)]
          if (length(days2exclude) > 0) {
            cut = which(mdat$window %in% days2exclude == TRUE)
            if (length(cut) > 0) mdat = mdat[-cut,] # remove days from which we already know that they are not going to be included (first and last day)       
          }
        }
      }
      # Reformat includedaycrit.part5 to maximum percentage non-wear during waking hours:
      if (includedaycrit.part5 >= 0 & includedaycrit.part5 <= 1) { # if includedaycrit.part5 is used as a ratio
        includedaycrit.part5 = includedaycrit.part5 * 100
      } else if (includedaycrit.part5 > 1 & includedaycrit.part5 <= 25) { # if includedaycrit.part5 is used like includedaycrit as a number of hours
        includedaycrit.part5 = (includedaycrit.part5 / 24) * 100
      } else if (includedaycrit.part5 < 0 ) {
        warning("\nNegative value of includedaycrit.part5 is not allowed, please change.")
      } else if (includedaycrit.part5 > 25) {
        warning("\nIncorrect value of includedaycrit.part5, this should be a fraction of the day between zero and one or the number of hours in a day.")
      }
      maxpernwday = 100 - includedaycrit.part5
      # Exclude days that have 100% nonwear over the full window or 100% over wakinghours
      cut = which(mdat$invalid_fullwindow == 100 | mdat$invalid_wakinghours > maxpernwday)
      if (length(cut) > 0) mdat = mdat[-cut,] # remove days from which we already know that they are not going to be included (first and last day)
    }
    mdat$guider = ifelse(mdat$guider =='sleeplog', yes = 1, # digitize guider to save storage space
                         no = ifelse(mdat$guider == 'HDCZA', yes = 2,
                                     no =  ifelse(mdat$guider == 'setwindow', yes = 3,
                                                  no = ifelse(mdat$guider == 'L512', yes = 4,
                                                              no = 0))))
    mdat = mdat[,-which(names(mdat) %in% c("timestamp","time"))]
    # re-oder columns
    naS = colnames(mdat)
    if (save_ms5raw_format == "csv") {
      # save to csv file
      write.csv(mdat,rawlevels_fname, row.names = F)
    } else if (save_ms5raw_format == "RData"){
      # only doing this for RData output, because it would affect file size too much in csv,
      # remember that this function can create many files: sample sizes times all combinations of thresholds.
      mdat$timestamp = as.POSIXlt(mdat$timenum, origin="1970-01-01",tz=desiredtz) 
      save(mdat, file = rawlevels_fname)
    }
    #===============================
    rm(mdat)
  }
  return()
}
