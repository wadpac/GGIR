g.IVIS = function(Xi, epochsizesecondsXi = 5, IVIS_epochsize_seconds=c(),
                  IVIS_windowsize_minutes = 60, IVIS.activity.metric = 1,
                  IVIS_acc_threshold = 20,
                  IVIS_per_daypair = FALSE) {
  if (length(IVIS_epochsize_seconds) > 0) {
    warning("Argument IVIS_epochsize_seconds has been depricated and has been ignored.")
  }
  IVIS_epochsize_seconds = IVIS_windowsize_minutes*60
  if (IVIS.activity.metric == 2) { # use binary scoring
    Xi = ifelse((Xi * 1000) < IVIS_acc_threshold, 0, 1) # explored on 5 June 2018 as an attempt to better mimic original ISIV construct
    Xi = zoo::rollsum(x = Xi, k = (600/epochsizesecondsXi)) # explored on 5 June 2018 as an first attempt to better mimic original ISIV construct
  }
  if (length(Xi) > (IVIS_epochsize_seconds/epochsizesecondsXi) & length(Xi) >  (IVIS_windowsize_minutes * 60)/epochsizesecondsXi) {
    if (IVIS_epochsize_seconds > epochsizesecondsXi) { # downsample Xi now from minute to hour (IVIS_windowsize)
      # aggregation replaced 14 April 2022 to make it able to handle missing values
      tmp = data.frame(Xi = Xi, time = numeric(length(Xi)))
      time = seq(0, length(Xi) * epochsizesecondsXi, by = epochsizesecondsXi)
      tmp$time = time[1:nrow(tmp)]
      tmp = aggregate(x = tmp, by = list(floor(tmp$time / IVIS_epochsize_seconds) * IVIS_epochsize_seconds), 
                      FUN = mean, na.action = na.pass)
      Xi = tmp$Xi
    }
    nhr = 24 * round(60 / IVIS_windowsize_minutes) # Number of hours in a day (modify this variable if you want to study different resolutions)
    Nsecondsinday = 24 * 3600
    ni = (Nsecondsinday/nhr)/IVIS_epochsize_seconds # number of epochs in an hour
    # derive average day with 1 'hour' resolution (hour => windowsize):
    N = length(Xi) #
    hour = rep(1:ceiling(N / ni), each = ni) - 1
    if (length(hour) > N) hour = hour[1:N]
    dat = data.frame(Xi = Xi, hour = hour, stringsAsFactors = TRUE)
    InterdailyStability = NA
    IntradailyVariability = NA
    if (nrow(dat) > 1) {
      hh = aggregate(. ~ hour, data = dat, mean, na.action = na.pass)
      hh$hour_perday = hh$hour - (floor(hh$hour / nhr) * nhr) # 24 hour in a day
      hh$day = floor(hh$hour/nhr) + 1
      if (nrow(hh) > 1) {
        if (IVIS_per_daypair == FALSE) {
          hh2 = aggregate(. ~ hour_perday, data = hh, mean, na.action = na.pass)
          Xh = hh2$Xi
          Xm = suppressWarnings(mean(Xh,na.rm = TRUE)) # Average acceleration per day
          p = length(which(is.na(Xh) == FALSE))
          N = length(Xi[which(is.na(Xi) == FALSE)])
          dat$day = floor(dat$hour/nhr) + 1
          S = aggregate(dat$Xi, by = list(dat$day), FUN = function(x) sum(diff(c(x))^2))
          InterdailyStability = (sum((Xh - Xm)^2, na.rm = TRUE) * N) / (p * sum((Xi - Xm)^2, na.rm = TRUE)) # IS: lower is less synchronized with the 24 hour zeitgeber
          IntradailyVariability = (sum(S$x, na.rm = TRUE) * N) / ((N - 1) * sum((Xm - Xi)^2, na.rm = TRUE)) #IV: higher is more variability within days (fragmentation)
        } else {
          hh$hour_perday = hh$hour - (floor(hh$hour / nhr) * nhr) # 24 hour in a day
          hh$day = floor(hh$hour/nhr) + 1
          # Calculate IV and IS per day and then aggregate over days
          # weighted by number of valid hours per day, while ignoring missing values
          NR = ceiling(nrow(hh) / 24)
          uniquedays = 1:NR
          IVIS_daypair_summary = data.frame(IS = numeric(NR - 1),
                                            IV = numeric(NR - 1),
                                            fracvalid = numeric(NR - 1),
                                            daypair = character(NR - 1))
          for (i in 1:(NR - 1)) { # Loop over all days
            thisday = which(hh$day == uniquedays[i])
            nextday = which(hh$day == uniquedays[i + 1])
            if (length(thisday) >= 23 & length(nextday) >= 23) {
              if (length(thisday) == 25) {
                thisday = thisday[1:24]
              } else if (length(thisday) == 23) {
                nextday = nextday[1:23]
              }
              if (length(nextday) == 25) {
                nextday = nextday[1:24]
              } else if (length(nextday) == 23) {
                thisday = thisday[1:23]
              }
              hh.t = hh[c(thisday, nextday), ]
              testNA = c(is.na(hh.t$Xi) | is.na(hh.t$Xi))
              Xi = hh.t$Xi
              hh2 = aggregate(. ~ hour_perday, data = hh.t, mean, na.action = na.pass)
              Xh = hh2$Xi
              Xm = suppressWarnings(mean(Xh,na.rm = TRUE))
              p = length(which(is.na(Xh) == FALSE))
              N = length(Xi[which(is.na(Xi) == FALSE)])
              S = aggregate(hh.t$Xi, by = list(hh.t$day), FUN = function(x) sum(diff(c(x))^2, na.rm = TRUE) )
              IVIS_daypair_summary$IS[i] = (sum((Xh - Xm)^2, na.rm = TRUE) * N) / (p * sum((Xi - Xm)^2, na.rm = TRUE)) # IS: lower is less synchronized with the 24 hour zeitgeber
              IVIS_daypair_summary$IV[i] = (sum(S$x, na.rm = TRUE) * N) / ((N - 1) * sum((Xm - Xi)^2, na.rm = TRUE)) #IV: higher is more variability within days (fragmentation)
              IVIS_daypair_summary$fracvalid[i] = length(which(testNA == FALSE)) / length(testNA)
              IVIS_daypair_summary$daypair[i] = paste0(i, "-", i + 1)
            }
          }
          IVIS_daypair_summary$fracvalid = ifelse(IVIS_daypair_summary$fracvalid < 0.66, yes = 0, no = IVIS_daypair_summary$fracvalid)
          if (length(which(IVIS_daypair_summary$fracvalid == 1)) > 0) { # expectation that at least 1 day pair is complete
            IVIS_daypair_summary = IVIS_daypair_summary[which(IVIS_daypair_summary$fracvalid != 0), ]
            InterdailyStability = mean(x = IVIS_daypair_summary$IS, w = IVIS_daypair_summary$fracvalid)
            IntradailyVariability = mean(x = IVIS_daypair_summary$IV, w = IVIS_daypair_summary$fracvalid)
          } else {
            InterdailyStability = NA
            IntradailyVariability = NA
          }
        }
      } else {
        InterdailyStability = NA
        IntradailyVariability = NA
      }
    }
  } else {
    InterdailyStability = NA
    IntradailyVariability = NA
  }
  invisible(list(InterdailyStability = InterdailyStability, IntradailyVariability = IntradailyVariability))
}
