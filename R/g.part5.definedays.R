g.part5.definedays = function(nightsi, wi, indjump, epochSize, qqq_backup = c(), ts, timewindowi, 
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
    if (nightsi[1] > 1 && nightsi[1] < 25 * 3600 / epochSize) {
      nightsi = c(1, nightsi)
    }
    if (nightsi[length(nightsi)] < nrow(ts) && 
        nrow(ts) - nightsi[length(nightsi)] < 25 * 3600 / epochSize) {
      nightsi = c(nightsi, nrow(ts))
    }
    # define window
    qqq[1] = nightsi[wi]
    if (length(nightsi) >= wi + 1) {
      qqq[2] = nightsi[wi + 1] - 1
    } else {
      qqq[2] = Nts
      lastDay = TRUE
    }
    # is this the last day?
    if (wi == length(nightsi) - 1) {
      lastDay = TRUE
    }
    if (qqq[2] >= Nts - 1) {
      qqq[2] = Nts
      lastDay = TRUE
    }
    qqq_backup = qqq
    # in MM, also define segments of the day based on qwindow
    if (!is.na(qqq[1]) & !is.na(qqq[2])) {
      segments_timing = NULL
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
      # define segments timing in H:M:S format
      breaks = qwindow2timestamp(qwindow, epochSize)
      startOfSegments = breaks[-length(breaks)]
      endOfSegments = subtractEpochFromTimeName(breaks[-1], epochSize)
      if (length(startOfSegments) > 1) { # when qwindow segments are defined, add fullwindow at the beginning
        startOfSegments = c(firstepoch, startOfSegments)
        endOfSegments = c(lastepoch, endOfSegments)
      } 
      segments_timing = paste(startOfSegments, endOfSegments, sep = "-")
      # define segment names based on qnames or segmentX
      if (is.null(qnames)) {
        segments_names = paste0("segment", 0:(length(segments_timing) - 1))
        segments_names = gsub("segment0", "MM", segments_names)
      } else {
        segments_names = c("MM", paste(qnames[-length(qnames)], qnames[-1], sep = "-"))
      }
      # Get indices in ts for segments start and end limits
      hms = format(ts$time[fullQqq], format = "%H:%M:%S")
      segments = vector("list", length = length(segments_timing))
      names(segments) = segments_timing
      for (si in 1:length(segments_timing)) {
        s0s1 = unlist(strsplit(segments_timing[si], split = "[-]"))
        s0s1 = format(s0s1, format = "%H:%M:%S")
        # tryCatch is needed in the case that the segment is not available in ts,
        # then a no non-missing values warning would be triggered by the which function
        segments[[si]] = tryCatch(range(fullQqq[which(hms >= s0s1[1] & hms <= s0s1[2])]), #segStart and segEnd
                                  warning = function(w) rep(NA, 2))
      }
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
