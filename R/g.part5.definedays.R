g.part5.definedays = function(nightsi, wi, indjump, nightsi_bu,
                              ws3new, qqq_backup=c(), ts, Nts, timewindowi, 
                              Nwindows, qwindow) {
  # define local functions ----
  qwindow2timestamp = function(qwindow, lastepoch) {
    H = floor(qwindow)
    M = floor((qwindow - H) * 60)
    S = floor((qwindow - H - M/60) * 60 * 60)
    H = as.character(H); M = as.character(M); S = as.character(S)
    H = ifelse(nchar(H) == 1, paste0("0", H), H)
    M = ifelse(nchar(M) == 1, paste0("0", M), M)
    S = ifelse(nchar(S) == 1, paste0("0", S), S)
    HMS = paste(H, M, S, sep = ":")
    if (HMS[length(HMS)] == "24:00:00") HMS[length(HMS)] = lastepoch
    if (HMS[1] != "00:00:00") HMS = c("00:00:00", HMS)
    if (HMS[length(HMS)] != lastepoch) HMS = c(HMS, lastepoch)
    return(HMS)
  }
  fixTime = function(x, ws3) {
    hms = strptime(x, format = "%H:%M:%S")
    hms = hms - ws3
    hms = substr(as.character(hms), 12, 19)
    return(hms)
  }
  # main script -----
  # Check that this is a meaningful day (that is all the qqq variable is used for),
  # before storing it.
  qqq = rep(0,2); segments = segments_names = c()
  # Check that it is possible to find both windows (WW and MM)
  # in the data for this day.
  if (timewindowi == "MM") {
    if (nightsi[1] == 1) wi = wi + 1 # if recording starts at midnight
    if (length(nightsi) >= wi) {
      if (wi == 1) {
        qqq[1] = 1
        qqq[2] = nightsi[wi] - 1
      } else if (wi <= length(nightsi)) {
        qqq[1] = nightsi[wi - 1]
        qqq[2] = nightsi[wi] - 1
        qqq_backup = qqq
      } else if (wi > length(nightsi)) {
        qqq[1] = qqq_backup[2] + 1
        if (wi <= length(nightsi)) {
          qqq[2] = nightsi[wi] - 1
        } else {
          if (wi - indjump <= length(nightsi)) {
            tmp1 = which(nightsi_bu == nightsi[wi - indjump])
            qqq[2] = nightsi_bu[tmp1 + indjump] - 1
            indjump = indjump + 1 # in case there are multiple days beyond nightsi
            if (is.na(qqq[2])) { # if that does not work use last midnight and add 24 hours
              index_lastmidn = which(nightsi_bu == nightsi[wi - (indjump - 1)]) + (indjump - 1)
              if (length(index_lastmidn) > 0) {
                qqq[2] = nightsi_bu[index_lastmidn] + (24*(60/ws3new) * 60) - 1
              } else {
                qqq[2] = NA
              }
            }
          } else {
            qqq[2] = NA
          }
          if (is.na(qqq[2])) { # if that does not work use last midnight and add 24 hours
            qqq[2] = qqq_backup[2] + (24*(60/ws3new) * 60) - 1
          }
          if (qqq[1] == qqq[2])  qqq[2] = qqq[2] + (24*(60/ws3new) * 60) - 1
        }
        if (is.na(qqq[2]) == TRUE | Nts < qqq[2]) {
          qqq[2] = Nts
        }
      }
    } else {
      qqq = c(NA, NA)
    }
    # in MM, also define segments of the day based on qwindow
    if (!is.na(qqq[1]) & !is.na(qqq[2])) {
      fullQqq = qqq[1]:qqq[2]
      lastepoch = substr(ts$time[qqq[2]], 12, 19)
      qnames = NULL
      if (is.data.frame(qwindow)) {
        browser()
        date_of_interest = unique(substr(ts$time, 1, 10))[wi]
        qdate = which(qwindow$date == date_of_interest)
        qnames = unlist(qwindow$qwindow_names[qdate])
        qwindow = unlist(qwindow$qwindow_values[qdate])
      }
      breaks = qwindow2timestamp(qwindow, lastepoch = lastepoch)
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
      segments_timing = paste("00:00:00", lastepoch, sep = "-")
      segments_names = "MM"
      si = 2
      if (all((qwindow) == c(0, 24)) == FALSE) {
        for (bi in 1:(length(breaks) - 1)) {
          minusOne = ifelse(breaks[bi + 1] == lastepoch, 0, 1)
          if (minusOne == 1) {
            segments[[si]] = c(breaks_i[bi], breaks_i[bi + 1] - 1)
            endOfSegment = fixTime(breaks[bi + 1], ws3new)
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
  } else if (timewindowi == "WW") {
    if (wi <= (Nwindows - 1)) { # all full wake to wake days
      qqq[1] = which(diff(ts$diur) == -1)[wi] + 1
      qqq[2] = which(diff(ts$diur) == -1)[wi + 1]
    } else {
      # time after last reliable waking up (this can be more than 24 hours)
      # ignore this day, because if the night was ignored for sleep analysis
      # then the description of the day in part 5 including that night is
      # not informative.
      qqq = c(NA, NA)
    }
    # build up segments
    if (!is.na(qqq[1]) & !is.na(qqq[2])) {
      segments = list(qqq)
      start = substr(ts$time[qqq[1]], 12, 19)
      end = substr(ts$time[qqq[2]], 12, 19)
      names(segments) = paste(start, end, sep = "-")
      segments_names = "WW"
    }
  }
  return(invisible(list(qqq = qqq, qqq_backup = qqq_backup, 
                        segments = segments, segments_names = segments_names)))
}
