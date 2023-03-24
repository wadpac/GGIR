g.part5.wakesleepwindows = function(ts, summarysleep_tmp2, desiredtz, nightsi, sleeplog, ws3new, Nts, ID,
                                    Nepochsinhour) {
  #========================================================
  # DIURNAL BINARY CLASSIFICATION INTO NIGHT (onset-wake) OR DAY (wake-onset) PERIOD 
  
  findIndex = function(timeChar = NULL, wc = NULL) {
    wc_withzerotime = paste0(wc," 00:00:00")
    if (length(grep(pattern = " ", x = wc)) == 0) {
      wc = wc_withzerotime
    }
    index = which(timeChar == wc)[1]
    if (is.na(index) == TRUE) {
      index = which(timeChar == wc_withzerotime)[1]
    }
    return(index)
  }
  clock2numtime = function(x) { # function used for converting sleeplog times to hour times
    x2 = as.numeric(unlist(strsplit(x,":"))) / c(1,60,3600)
    return(sum(x2))
  }
  w0 = w1 = rep(0,length(summarysleep_tmp2$calendar_date))
  # Round seconds to integer number of epoch lengths (needed if cleaningcode = 5).
  round_seconds_to_ws3new = function(x, ws3new) {
    temp = as.numeric(unlist(strsplit(x,":")))
    if (length(temp) == 3) {
      if (temp[3] / ws3new != round(temp[3] / ws3new)) {
        x = paste0(temp[1],":",temp[2],":",round(temp[3] / ws3new)*ws3new)
      }
    } else {
      x = ""
    }
    return(x)
  }
  # standardise time series to character without iso8601 respresentation
  timeChar = format(ts$time) 
  if (is.ISO8601(timeChar[1]) == TRUE) { # only do this for ISO8601 format
    timeChar = format(iso8601chartime2POSIX(timeChar, tz = desiredtz))
  }
  
  for (k in 1:length(summarysleep_tmp2$calendar_date)) { # loop through nights from part 4
    summarysleep_tmp2$wakeup_ts[k] = round_seconds_to_ws3new(summarysleep_tmp2$wakeup_ts[k], ws3new)
    summarysleep_tmp2$sleeponset_ts[k] = round_seconds_to_ws3new(summarysleep_tmp2$sleeponset_ts[k], ws3new)
    
    summarysleep_tmp2$sleeponset[k] = (round((summarysleep_tmp2$sleeponset[k]*3600) / ws3new) * ws3new) / 3600
    summarysleep_tmp2$wakeup[k] = (round((summarysleep_tmp2$wakeup[k]*3600) / ws3new) * ws3new) / 3600
    
    # Load sleep onset and waking time from part 4 and convert them into timestamps
    tt = unlist(strsplit(as.character(summarysleep_tmp2$calendar_date[k]),"/")) # calendar date
    # if sleep onset is not available in from acc and/or sleep then us the following default
    # in order to still have some beginning and end of the night, these days will be discared
    # anyway, because typically this coincides with a lot of non-wear time:
    if (is.na(summarysleep_tmp2$sleeponset[k]) == T) {
      defSO = 22 
      defSO_ts = "22:00:00"
    } else {
      defSO = summarysleep_tmp2$sleeponset[k]
      defSO_ts = summarysleep_tmp2$sleeponset_ts[k]
    }
    if (is.na(summarysleep_tmp2$wakeup[k]) == T) {
      defWA = 31
      defWA_ts = "07:00:00"
    } else {
      defWA = summarysleep_tmp2$wakeup[k]
      defWA_ts = summarysleep_tmp2$wakeup_ts[k]
    }
    w0[k] = paste(tt[3],"-", tt[2], "-", tt[1], " ", as.character(defSO_ts), sep = "")
    w1[k] = paste(tt[3],"-", tt[2], "-", tt[1], " ", as.character(defWA_ts), sep = "")
    # if time is beyond 24 then change the date
    if (defSO >= 24) { 
      w0[k] = as.character(as.POSIXlt(w0[k],tz = desiredtz) + (24*3600))
    }
    if (defWA >= 24 |
        (summarysleep_tmp2$daysleeper[k] == 1 & defWA < 18)) {
      w1[k] = format(as.POSIXlt(w1[k], tz = desiredtz) + (24*3600))
    }
    s0 = findIndex(timeChar, wc = format(as.POSIXlt(w0[k], tz = desiredtz)))
    s1 = findIndex(timeChar, wc = format(as.POSIXlt(w1[k], tz = desiredtz)))
    
    if (is.na(s0) == TRUE) {
      s0 = 1
    }
    if (is.na(s1) == TRUE) {
      # might still be NA if the timestamps is not in ts (expanded time from expand_tail)
      # if so, we assume the participant is sleeping at the end of the recording, this night will be disregarded later on
      s1 = nrow(ts)
    }
    
    if (length(s1) != 0 & length(s0) != 0 & is.na(s0) == FALSE & is.na(s1) == FALSE) {
      distance2midnight = abs(nightsi - s1) + abs(nightsi - s0)
      closestmidnighti = which.min(distance2midnight)
      closestmidnight = nightsi[closestmidnighti]
      noon0 = closestmidnight - (12 * (60/ws3new) * 60)
      noon1 = closestmidnight + (12 * (60/ws3new) * 60)
      
      if (noon0 < 1) noon0 = 1
      if (noon1 > Nts) noon1 = Nts
      nonwearpercentage = mean(ts$nonwear[noon0:noon1])
      if ((length(sleeplog) > 0 & (nonwearpercentage > 0.33) | summarysleep_tmp2$sleeponset_ts[k] == "")) { # added condition to detect nights that are not detected in part 4
        
        # If non-wear is high for this day and if sleeplog is available
        sleeplogonset = sleeplog$sleeponset[which(sleeplog$ID == ID & sleeplog$night == summarysleep_tmp2$night[k])]
        sleeplogwake = sleeplog$sleepwake[which(sleeplog$ID == ID & sleeplog$night == summarysleep_tmp2$night[k])]
        if (length(sleeplogonset) != 0 & length(sleeplogwake) != 0) {
          if (!is.na(sleeplogonset) &  !is.na(sleeplogwake)) {
            # ... and if there is sleeplog data for the relevant night
            # rely on sleeplog for defining the start and end of the night
            sleeplogonset_hr = clock2numtime(sleeplogonset)
            sleeplogwake_hr = clock2numtime(sleeplogwake)
            # express hour relative to midnight within the noon-noon:
            if (sleeplogonset_hr > 12) {
              sleeplogonset_hr = sleeplogonset_hr - 24
            }
            if (sleeplogwake_hr > 18 & summarysleep_tmp2$daysleeper[k] == 1) {
              sleeplogwake_hr = sleeplogwake_hr - 24 # 18 because daysleepers can wake up after 12
            } else if (sleeplogwake_hr > 12 & summarysleep_tmp2$daysleeper[k] == 0) {
              sleeplogwake_hr = sleeplogwake_hr - 24
            }
            if (sleeplogwake_hr > 36 &  sleeplogonset_hr > 36) {
              sleeplogwake_hr = sleeplogwake_hr - 24
              sleeplogonset_hr = sleeplogonset_hr - 24
            }
            s0 = closestmidnight + round(sleeplogonset_hr * Nepochsinhour)
            if (s0 < 1) {
              warning("Impossible index for first night, consider setting excludefirst.part4=TRUE")
              s0 = 1
            }
            s1 = closestmidnight + round(sleeplogwake_hr * Nepochsinhour)
          }
        }
      }
      ts$diur[s0:(s1 - 1)] = 1
    }
  }
  return(ts)
}
