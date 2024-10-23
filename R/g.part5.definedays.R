g.part5.definedays = function(nightsi, wi, indjump, nightsi_bu,
                              epochSize, qqq_backup = c(), ts, timewindowi, 
                              Nwindows, qwindow, ID = NULL,
                              dayborder = 0) {
  Nts = nrow(ts)
  lastDay = FALSE
  # define local functions ----
  qwindow2timestamp = function(qwindow, epochSize) {
    hour = floor(qwindow)
    minute = floor((qwindow - hour) * 60)
    second = floor((qwindow - hour - minute/60) * 60 * 60)
    expected_seconds = seq(0, 60, by = epochSize)
    seconds_tobe_revised = which(!second %in% expected_seconds)
    if (length(seconds_tobe_revised) > 0) {
      for (si in seconds_tobe_revised) {
        second[si] = expected_seconds[which.min(abs(expected_seconds - second[si]))]
        if (second[si] == 60) { # shift minute if second == 60
          minute[si] = minute[si] + 1
          second[si] = 0
        }
      }
    }
    hour = as.character(hour); minute = as.character(minute); second = as.character(second)
    hour = ifelse(nchar(hour) == 1, paste0("0", hour), hour)
    minute = ifelse(nchar(minute) == 1, paste0("0", minute), minute)
    second = ifelse(nchar(second) == 1, paste0("0", second), second)
    HMS = paste(hour, minute, second, sep = ":")
    if (HMS[1] != "00:00:00") HMS = c("00:00:00", HMS)
    return(HMS)
  }
  subtractEpochFromTimeName = function(x, epochSize) {
    hms = strptime(x, format = "%H:%M:%S")
    hms = hms - epochSize
    hms = format(hms, format = "%H:%M:%S")
    return(hms)
  }
  # main script -----
  # Check that this is a meaningful day (that is all the qqq variable is used for),
  # before storing it.
  qqq = rep(0,2); segments = segments_names = c()
  # Check that it is possible to find both windows (WW and MM)
  # in the data for this day.
  if (timewindowi == "MM") {
    # include first and last partial days in MM
    if (nightsi[1] > 1) nightsi = c(1, nightsi)
    if (nightsi[length(nightsi)] < nrow(ts)) nightsi = c(nightsi, nrow(ts))
    # define window
    qqq[1] = nightsi[wi]
    qqq[2] = nightsi[wi + 1] - 1
    # is this the last day?
    if (qqq[2] >= Nts - 1) {
      qqq[2] = Nts
      lastDay = TRUE
    }
    qqq_backup = qqq
    # in MM, also define segments of the day based on qwindow
    if (!is.na(qqq[1]) & !is.na(qqq[2])) {
      if (qqq[2] > Nts) qqq[2] = Nts
      fullQqq = qqq[1]:qqq[2]
      firstepoch = format(ts$time[qqq[1]],  "%H:%M:%S")
      lastepoch = format(ts$time[qqq[2]],  "%H:%M:%S")
      qnames = NULL
      if (is.data.frame(qwindow)) {
        date_of_interest = substr(ts$time[qqq[1]], 1, 10)
        qdate = which(qwindow$ID == ID & qwindow$date == date_of_interest)
        if (length(qdate) == 1) { # if ID/date matched with activity log
          qnames = unlist(qwindow$qwindow_names[qdate])
          qwindow = unlist(qwindow$qwindow_values[qdate])
        } else { # if ID/date not correctly matched with activity log
          qwindow = c(0, 24)
        }
      } else {
        qwindow = sort(qwindow)
        if (qwindow[1] != 0) qwindow = c(0, qwindow)
        if (qwindow[length(qwindow)] != 24) qwindow = c(qwindow, 24)
      }
      breaks = qwindow2timestamp(qwindow, epochSize)
      if (24 %in% qwindow) {
        # 24:00:00: probably does not exist, replace by last timestamp in a day
        # here, we consider N epochs per day plus 1 hour just in case we are deriving this in 
        # a 25-hour daylight saving time day
        NepochPerDayPlusOneHr = ((25*3600) / epochSize)
        latest_time_in_day = max(format(ts$time[1:pmin(Nts, NepochPerDayPlusOneHr)], format = "%H:%M:%S"))
        breaks = gsub(pattern = "24:00:00", replacement = latest_time_in_day, x = breaks)
      }
      breaks_i = c()
      for (bi in 1:length(breaks)) {
        if (any(grepl(breaks[bi], ts$time[fullQqq]))) {
          breaks_i[bi] = fullQqq[grep(breaks[bi], ts$time[fullQqq])]
        } else {
          breaks_i[bi] = qqq[1]
        }
      }
      # build up segments
      segments = list(qqq)
      segments_timing = paste(firstepoch, lastepoch, sep = "-")
      segments_names = "MM"
      si = 2
      do.segments = TRUE
      if (length(qwindow) == 2) {
        if (all((qwindow) == c(0, 24))) {
          do.segments = FALSE
        }
      }
      if (do.segments == TRUE) {
        for (bi in 1:(length(breaks) - 1)) {
          minusOne = ifelse(breaks[bi + 1] == lastepoch, 0, 1)
          if (minusOne == 1) {
            segments[[si]] = c(breaks_i[bi], breaks_i[bi + 1] - 1)
            endOfSegment = subtractEpochFromTimeName(breaks[bi + 1], epochSize)
          } else {
            segments[[si]] = c(breaks_i[bi], breaks_i[bi + 1])
            endOfSegment = breaks[bi + 1]
          }
          if (segments[[si]][2] < segments[[si]][1]) segments[[si]][2] = segments[[si]][1]
          segments_timing[si] = paste(breaks[bi], endOfSegment, sep = "-")
          if (is.null(qnames)) {
            segments_names[si] = paste0("segment", bi)
          } else {
            segments_names[si] = paste(qnames[si - 1], qnames[si], sep = "-")
          }
          si = si + 1 
        }
      }
      names(segments) = segments_timing
    }
  } else if (timewindowi == "WW" || timewindowi == "OO") {
    windowEdge = ifelse(timewindowi == "WW", yes = -1, no = 1)
    if (wi <= (Nwindows - 1)) { # all full windows
      qqq[1] = which(diff(ts$diur) == windowEdge)[wi] + 1
      qqq[2] = which(diff(ts$diur) == windowEdge)[wi + 1]
    } else {
      # time after last reliable waking up or onset (this can be more than 24 hours)
      # ignore this window, because if the night was ignored for sleep analysis
      # then the description of the day in part 5 including that night is
      # not informative.
      qqq = c(NA, NA)
    }
    # build up segments
    if (!is.na(qqq[1]) & !is.na(qqq[2])) {
      segments = list(qqq)
      start = format(ts$time[qqq[1]], "%H:%M:%S")
      end = format(ts$time[qqq[2]], "%H:%M:%S")
      names(segments) = paste(start, end, sep = "-")
      segments_names = timewindowi
    }
    if (wi >= Nwindows) lastDay = TRUE
  }
  return(invisible(list(qqq = qqq, qqq_backup = qqq_backup, lastDay = lastDay,
                        segments = segments, segments_names = segments_names)))
}
