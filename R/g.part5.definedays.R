g.part5.definedays = function(nightsi, wi, indjump, nightsi_bu, 
                              ws3new, qqq_backup=c(), ts, Nts, timewindowi, Nwindows) {
  
  
  # Check that this is a meaningful day (that is all the qqq variable is used for),
  # before storing it.
  qqq = rep(0,2)
  # Check that it is possible to find both windows (WW and MM)
  # in the data for this day.
  if (timewindowi == "MM") {
    if (nightsi[1] == 1) wi = wi + 1 # if recording starts at midnight
    if (length(nightsi) >= wi) {
      if (wi==1) {
        qqq[1] = 1
        qqq[2] = nightsi[wi]
      } else if (wi<=length(nightsi)) {
        qqq[1] = nightsi[wi-1] + 1
        qqq[2] = nightsi[wi]
        qqq_backup = qqq
      } else if (wi>length(nightsi)) {
        qqq[1] = qqq_backup[2] + 1
        if (wi <= length(nightsi)) {
          qqq[2] = nightsi[wi]
        } else {
          if (wi-indjump <= length(nightsi)) {
            tmp1 = which(nightsi_bu == nightsi[wi-indjump])
            qqq[2] = nightsi_bu[tmp1 + indjump]
            indjump = indjump + 1 # in case there are multiple days beyond nightsi
            if (is.na(qqq[2])) { # if that does not work use last midnight and add 24 hours
              index_lastmidn = which(nightsi_bu == nightsi[wi-(indjump-1)]) + (indjump-1)
              if (length(index_lastmidn) > 0) {
                qqq[2] = nightsi_bu[index_lastmidn] + (24*(60/ws3new) * 60) -1
              } else {
                qqq[2] = NA
              }
            }
          } else {
            qqq[2] = NA
          }
          if (is.na(qqq[2])) { # if that does not work use last midnight and add 24 hours
            qqq[2] = qqq_backup[2] + (24*(60/ws3new) * 60) -1
          }
          if (qqq[1] == qqq[2])  qqq[2] = qqq[2] + (24*(60/ws3new) * 60) - 1
        }
        if(is.na(qqq[2])==TRUE | Nts < qqq[2]) {
          qqq[2] = Nts
        }
      }
    } else {
      qqq = c(NA,NA)
    }
  } else if(timewindowi == "WW") {
    if (wi <=(Nwindows-1)) { # all full wake to wake days
      qqq[1] = which(diff(ts$diur) == -1)[wi] + 1
      qqq[2] = which(diff(ts$diur) == -1)[wi+1]
      
    } else {
      # time after last reliable waking up (this can be more than 24 hours)
      # ignore this day, because if the night was ignored for sleep analysis
      # then the description of the day in part 5 including that night is
      # not informative.
      qqq = c(NA, NA)
    }
  }
  return(invisible(list(qqq=qqq,qqq_backup=qqq_backup)))
}
