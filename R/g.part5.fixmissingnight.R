g.part5.fixmissingnight = function(summarysleep_tmp2, sleeplog=c(), ID) {
  #========================================================
  # Added 24 March 2020, for the rare situation when part 4 does misses a night.
  # This is possible when the accelerometer was not worn the entire day and
  # ignorenonwear was set to TRUE, part4 then gives up and does not try to store anything.
  # rather than fiddling with part 4 again it seems more logical to address this here,
  # because only part 5 needs this.
  clock2numtime = function(x) { # function used for converting sleeplog times to hour times
    x2 = as.numeric(unlist(strsplit(x,":"))) / c(1,60,3600)
    return(sum(x2))
  }
  hr_to_clocktime = function(x) {
    hrsNEW = floor(x)
    minsUnrounded = (x - hrsNEW) *60
    minsNEW =  floor(minsUnrounded)
    secsNEW =  floor( (minsUnrounded - minsNEW)*60)
    if (minsNEW < 10) minsNEW = paste0(0,minsNEW)
    if (secsNEW < 10) secsNEW = paste0(0,secsNEW)
    if (hrsNEW < 10) hrsNEW = paste0(0,hrsNEW)
    time = paste0(hrsNEW,":",minsNEW,":",secsNEW)
    return(time)
  }
  potentialnight = min(summarysleep_tmp2$night):max(summarysleep_tmp2$night)
  missingnight = which(as.numeric(potentialnight) %in% as.numeric(summarysleep_tmp2$night) == FALSE)
  
  if ("guider_wakeup" %in% colnames(summarysleep_tmp2) == TRUE) {
    guider_onset = "guider_onset"
    guider_wakeup = "guider_wakeup"
  } else {
    guider_onset = "guider_inbedStart"
    guider_wakeup = "guider_inbedEnd"
  }
  if (length(missingnight) > 0) {
    for (mi in missingnight) {
      missingNight = potentialnight[mi]
      newnight = summarysleep_tmp2[1,]
      newnight[which(names(newnight) %in% c("ID","night", "sleepparam","filename","filename_dir","foldername") == FALSE)] = NA
      newnight$wakeup = newnight[,guider_wakeup] = newnight$sleeponset = newnight[,guider_onset] = NA
      newnight$night = missingNight
      newnight$calendar_date = format(as.Date(as.POSIXlt(summarysleep_tmp2$calendar_date[mi-1],format="%d/%m/%Y") + (36*3600)), "%d/%m/%Y")
      timesplit = as.numeric(unlist(strsplit(as.character(newnight$calendar_date),"/"))) # remove leading zeros
      newnight$calendar_date = paste0(timesplit[1],"/",timesplit[2],"/",timesplit[3])
      newnight$daysleeper = 0
      newnight$acc_available = 0
      if (length(sleeplog) > 0) { # we impute with sleeplog (TO DO: Also implement catch for if sleeplog is not available)
        sleeplogonset = sleeplog$sleeponset[which(sleeplog$ID == ID & sleeplog$night == missingNight)]
        sleeplogwake = sleeplog$sleepwake[which(sleeplog$ID == ID & sleeplog$night == missingNight)]
        newnight$sleeplog_used = 0
        newnight$guider = "nosleeplog_accnotworn"
        if (length(sleeplogonset) != 0 & length( sleeplogwake) != 0) {
          if (is.na(sleeplogonset) == FALSE & is.na(sleeplogonset) == FALSE) {
            sleeplogonset_hr = clock2numtime(sleeplogonset)
            sleeplogwake_hr= clock2numtime(sleeplogwake)
            newnight$sleeponset = newnight[,guider_onset] = sleeplogonset_hr
            newnight$wakeup = newnight[,guider_wakeup] = sleeplogwake_hr
            if (sleeplogwake_hr > 36) {
              newnight$daysleeper = 1
              newnight$sleeponset_ts = hr_to_clocktime(sleeplogonset_hr)
              newnight$wakeup_ts = hr_to_clocktime(sleeplogwake_hr)
              newnight$sleeplog_used = 1
              newnight$guider = "sleeplog"
            }
          }
        }
      } else {
        newnight$sleeplog_used = 0
        newnight$guider = "nosleeplog_accnotworn"
      }
      newnight$cleaningcode = 5
      summarysleep_tmp2 = rbind(summarysleep_tmp2[1:(mi-1),],
                                newnight,
                                summarysleep_tmp2[mi:nrow(summarysleep_tmp2),])
    }
  }
  return(summarysleep_tmp2)
}