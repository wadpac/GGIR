g.sib.sum = function(SLE,M,ignorenonwear=TRUE,desiredtz="") {
  A = as.data.frame(SLE$output, stringsAsFactors = TRUE)
  invalid = A$invalid
  if (ignorenonwear == TRUE) {
    if (length(which(A$invalid==1)) > 0) {
      A[which(A$invalid==1), which(colnames(A) %in% c("time", "night") == FALSE)] = 0
    }
  }
  space = ifelse(length(unlist(strsplit(format(A$time[1])," "))) > 1,TRUE,FALSE)
  time = format(unlist(A$time))
  #time stored as iso8601 what makes it sensitive to daylight saving days.
  if (space == TRUE) {
    time = POSIXtime2iso8601(as.POSIXlt(time, tz = desiredtz), tz = desiredtz)
  }
  night = A$night
  sleep = as.data.frame(as.matrix(A[,(which(colnames(A)=="night")+1):ncol(A)]), stringsAsFactors = TRUE)
  colnames(sleep) = colnames(A)[(which(colnames(A)=="night")+1):ncol(A)]
  ws3 = M$windowsizes[1]
  ws2 = M$windowsizes[2]
  sib.cla.sum = as.data.frame(matrix(-1, 1000,9))
  # label night after last night as -1 to differentiate it from night 0
  lastNightEnd = max(which(night == max(night)))
  if (length(night) > lastNightEnd) {
    night[(lastNightEnd + 1):length(night)] = -1
  }
  # unique nights
  un = unique(night) #unique nights
  missingnights = which(un == -1 | is.na(un) == TRUE)
  if (length(missingnights) > 0) {
    un = un[-missingnights]
  }
  # if recording starts after 4am, then also remove first night
  firstTS_posix = iso8601chartime2POSIX(M$metashort$timestamp[1], tz = desiredtz)
  firstday4am_posix = as.POSIXct(paste0(as.Date(firstTS_posix), " 04:00:00"), tz = desiredtz)
  if (firstday4am_posix < firstTS_posix) { # then first timestamp is later than 4am
    missingnights = which(un == 0)
    if (length(missingnights) > 0) {
      un = un[-missingnights]
    }
  }
  if (length(un) != 0) {
    if (is.numeric(max(un)) == TRUE) {
      cnt = 1
      for (i in un) { #nights #length(un)
        qqq1 = which(night == i)[1]
        qqq2 = which(night == i)[length(which(night == i))]
        if (length(qqq1) == 1 & length(qqq2) == 1) {
          time.t = time[qqq1:qqq2]
          sleep.t = as.data.frame(sleep[qqq1:qqq2,], stringsAsFactors = TRUE)
          colnames(sleep.t) = colnames(sleep)
          invalid.t = invalid[qqq1:qqq2]
          for (j in 1:ncol(sleep.t)) { #sleep definitions
            nsleepperiods = length(which(diff(c(0, sleep.t[,j])) == 1))
            if (nsleepperiods > 0) {
              start_sp = which(diff(c(0, sleep.t[,j])) == 1)
              end_sp = which(diff(c(sleep.t[,j], 0)) == -1)
              if(length(end_sp) == 0) end_sp = nrow(sleep.t) #if sleep period ends the next 'nightday'
              if (start_sp[1] > end_sp[1]) { #if period starts with sleep
                start_sp = c(1,start_sp)
              }
              if (start_sp[length(start_sp)] > end_sp[length(end_sp)]) { #if period ends with sleep
                end_sp = c(end_sp,nrow(sleep.t))
              }
              nsleepperiods = length(start_sp)
            }
            colnames(sib.cla.sum)[1:9] = c("night", "definition", "start.time.day",
                                           "nsib.periods", "tot.sib.dur.hrs",
                                           "fraction.night.invalid",
                                           "sib.period", "sib.onset.time",
                                           "sib.end.time")
            NepochsInDay = (60/ws3)*1440
            Nmissingvalues = max(c(0, (NepochsInDay - length(invalid.t))))
            fraction.night.invalid = (length(which(invalid.t != 0)) + Nmissingvalues) / NepochsInDay
            if (fraction.night.invalid > 1) fraction.night.invalid = 1 # day with 25 hours
            if (nsleepperiods == 0) {
              sib.cla.sum[cnt, 1] = i #night
              sib.cla.sum[cnt, 2] = colnames(sleep.t)[j] #definition
              sib.cla.sum[cnt, 3] = format(time.t[1])
              sib.cla.sum[cnt, 4] = nsleepperiods #number of sleep periods
              sib.cla.sum[cnt, 5] = 0 #total sleep duration
              sib.cla.sum[cnt, 6] = fraction.night.invalid #length(invalid.t)
              sib.cla.sum[cnt, 7] = 0
              sib.cla.sum[cnt, 8:9] = ""
              cnt = cnt + 1
            } else {
              for (spi in 1:nsleepperiods) {
                sib.cla.sum[cnt, 1] = i #night
                sib.cla.sum[cnt, 2] = colnames(sleep.t)[j] #definition
                sib.cla.sum[cnt, 3] = format(time.t[1])
                sib.cla.sum[cnt, 4] = nsleepperiods #number of sleep periods
                sleep_sp = sleep.t[start_sp[spi]:end_sp[spi],j]
                time_sp = time.t[start_sp[spi]:end_sp[spi]]
                sleep_dur = (round((length(which(sleep_sp == 1))/(60/ws3)) * 100)) / 100
                # general characteristics of the time window
                sib.cla.sum[cnt, 5] = sleep_dur/60 #total sleep duration
                sib.cla.sum[cnt, 6] = fraction.night.invalid #length(invalid.t)
                sib.cla.sum[cnt, 7] = spi
                sib.cla.sum[cnt, 8] = format(time_sp[which(sleep_sp == 1)[1]])
                sib.cla.sum[cnt, 9] = format(time_sp[length(sleep_sp)])
                cnt = cnt + 1
              }
            }
            if (cnt  > 800) {
              emptydf = as.data.frame(matrix(0, 1000, ncol(sib.cla.sum)))
              colnames(emptydf) = colnames(sib.cla.sum)
              sib.cla.sum = rbind(sib.cla.sum, emptydf)
            }
          }
        }
      }
    }
  }
  sib.cla.sum = sib.cla.sum[-which(sib.cla.sum$night == -1 | sib.cla.sum$sib.period == 0), ]
  return(sib.cla.sum)
}
