g.part5.savetimeseries = function(ts, LEVELS, desiredtz, rawlevels_fname,
                                  save_ms5raw_format = "csv",
                                  save_ms5raw_without_invalid = TRUE,
                                  DaCleanFile = NULL,
                                  includedaycrit.part5 = 2/3, ID = NULL,
                                  sep_reports = ",",
                                  params_247 = NULL) {
  ms5rawlevels = data.frame(date_time = ts$time, class_id = LEVELS,
                            # class_name = rep("",Nts),
                            stringsAsFactors = FALSE)

  if (length(grep(pattern = " ", x = ts$time[1])) == 0) {
    ts$timestamp = as.POSIXct(ts$time, tz = desiredtz, format = "%Y-%m-%dT%H:%M:%S%z")
    ms5rawlevels$date_time = as.POSIXct(ms5rawlevels$date_time, 
                                        tz = desiredtz, format = "%Y-%m-%dT%H:%M:%S%z")
  } else {
    ts$timestamp = ts$time
  }
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
  if (length(wakeup) > 0) {
    if (length(wakeup) > 1) {
      for (di in 1:(length(wakeup) - 1)) {
        dayindices = wakeup[di]:(wakeup[di + 1] - 1)
        wake = which(mdat$SleepPeriodTime[dayindices] == 0)
        sleep = which(mdat$SleepPeriodTime[dayindices] == 1)
        mdat$invalid_wakinghours[dayindices] = round(mean(mdat$invalidepoch[dayindices[wake]]) * 100, digits = 2)
        mdat$invalid_sleepperiod[dayindices] = round(mean(mdat$invalidepoch[dayindices[sleep]]) * 100, digits = 2)
        mdat$invalid_fullwindow[dayindices] = round(mean(mdat$invalidepoch[dayindices]) * 100, digits = 2)
      }
    } else {
      dayindices = 1:nrow(mdat)
      wake = which(mdat$SleepPeriodTime[dayindices] == 0)
      sleep = which(mdat$SleepPeriodTime[dayindices] == 1)
      mdat$invalid_wakinghours[dayindices] = round(mean(mdat$invalidepoch[dayindices[wake]]) * 100, digits = 2)
      mdat$invalid_sleepperiod[dayindices] = round(mean(mdat$invalidepoch[dayindices[sleep]]) * 100, digits = 2)
      mdat$invalid_fullwindow[dayindices] = round(mean(mdat$invalidepoch[dayindices]) * 100, digits = 2)
    }
    # round acceleration values to 3 digits to reduce storage space
    mdat$ACC = round(mdat$ACC, digits = 3)
    # round light data to 0 digits to reduce storage space
    if ("lightpeak" %in% names(mdat)) mdat$lightpeak = round(mdat$lightpeak)
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
      }
      maxpernwday = 100 - includedaycrit.part5
      # Exclude days that have 100% nonwear over the full window or 100% over wakinghours
      cut = which(mdat$invalid_fullwindow == 100 | mdat$invalid_wakinghours > maxpernwday)
      if (length(cut) > 0) mdat = mdat[-cut,] # remove days from which we already know that they are not going to be included (first and last day)
    }
    mdat$guider = ifelse(mdat$guider == 'sleeplog', yes = 1, # digitize guider to save storage space
                         no = ifelse(mdat$guider == 'HDCZA', yes = 2,
                                     no =  ifelse(mdat$guider == 'setwindow', yes = 3,
                                                  no = ifelse(mdat$guider == 'L512', yes = 4,
                                                              no = ifelse(mdat$guider == 'HorAngle', yes = 5,
                                                                          no = ifelse(mdat$guider == 'NotWorn', yes = 6,
                                                                                      no = 0))))))
    mdat = mdat[,-which(names(mdat) %in% c("timestamp","time"))]
    # re-oder columns
    if ("csv" %in% save_ms5raw_format) {
      # save to csv file
      fname = rawlevels_fname[grep("*csv$", rawlevels_fname)]
      data.table::fwrite(mdat, fname, row.names = F, sep = sep_reports)
    }
    if ("RData" %in% save_ms5raw_format || params_247[["part6HCA"]] == TRUE || params_247[["part6CR"]] == TRUE) {
      # only doing this for RData output, because it would affect file size too much in csv,
      # remember that this function can create many files: sample sizes times all combinations of thresholds.
      mdat$timestamp = as.POSIXct(mdat$timenum, origin = "1970-01-01",tz = desiredtz)
      rawlevels_fname = gsub(pattern = ".csv", replacement = ".RData", x = rawlevels_fname)
      fname = unique(rawlevels_fname[grep("*RData$", rawlevels_fname)])
      save(mdat, file = fname)
    }
    #===============================
    rm(mdat)
  }
  return()
}
