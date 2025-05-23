filterNonwearNight = function(r1, metalong, qwindowImp, desiredtz,
                              params_cleaning, ws2) {
  nonwearFiltermaxHours  = params_cleaning[["nonwearFiltermaxHours"]]
  nonwearFilterWindow = params_cleaning[["nonwearFilterWindow"]]
  nonwearEventsFiltered = nonwearHoursFiltered = 0
  # Identify method to use
  if (!is.null(nonwearFilterWindow)) {
    filter_method = 1 # set window as provided by user
  } else {
    # If nonwearFilterWindow is not provided then
    # look for qwindow is available
    if (is.null(qwindowImp)) {
      stop(paste0("Please specify parameter nonwearFilterWindow or ",
                  "qwindow as diary with columns to define the window for ",
                  "filtering short nonwear. See documentation for ",
                  "parameter nonwearFiltermaxHours"), call. = FALSE)
    }
    if (inherits(qwindowImp, "data.frame")) {
      filter_method = 2 # Use it as a dataframe
    } else {
      filter_method = 1 
      nonwearFilterWindow = qwindowImp
    }
  }
  # Get metalong timestamps
  metalong$time_POSIX = iso8601chartime2POSIX(metalong$timestamp, tz = desiredtz)
  metalong$hour = as.numeric(format(metalong$time_POSIX, "%H")) + as.numeric(format(metalong$time_POSIX, "%M")) / 60
  # Mask short nonwear during the night
  x = rle(as.numeric(r1))
  if (length(x$values) != 1) {
    r1B = as.data.frame(lapply(x, rep, times =  x$lengths))
    r1B$hr = metalong$hour
    r1B$filter_method = filter_method
    r1B$lengths = (r1B$lengths * ws2) / 3600 # convert unit from epoch to hours
    r1B$filterWindow = 0
    # Define night window
    if (filter_method == 1) { 
      # set window same for all recordings
      if (nonwearFilterWindow[1] > nonwearFilterWindow[2]) {
        r1B$filterWindow[which(r1B$hr >= nonwearFilterWindow[1] |
                                 r1B$hr < nonwearFilterWindow[2])] = 1
      } else {
        r1B$filterWindow[which(r1B$hr >= nonwearFilterWindow[1] &
                                 r1B$hr < nonwearFilterWindow[2])] = 1
      }
    } else if (filter_method == 2) { 
      # window defined by diary input from qwindow specific per participant
      r1B$date = as.Date(metalong$time_POSIX)
      for (qi in 1:nrow(qwindowImp)) {
        qwindow_temp = qwindowImp$qwindow_values[[qi]]
        # Only consider window when both start and end are reported
        available = c(length(grep(pattern = "inbed|sleeponset|lightsout", x = qwindowImp$qwindow_names[[qi]])) > 0,
                      length(grep(pattern = "outbed|wakeup|lightsoff", x = qwindowImp$qwindow_names[[qi]])) > 0)
        # filter only SPT and time in bed reports
        qwindow_temp = qwindow_temp[grep(pattern = "bed|wakeup|sleeponset|lights", x = qwindowImp$qwindow_names[[qi]])]
        qwindow_temp = sort(qwindow_temp)
        # Ignore if somehow only teh default window is available
        isDefaultWindow = length(qwindow_temp) == 2 && qwindow_temp[1] == 0 && qwindow_temp[2] == 24
        if (length(qwindow_temp) > 1 && isDefaultWindow == FALSE && all(available)) {
          # convert to continuous scale to ease finding start and end
          below18 = which(qwindow_temp < 18)
          if (length(below18) > 0) {
            qwindow_temp[below18] = qwindow_temp[below18] + 24
          }
          start = min(qwindow_temp)
          end = max(qwindow_temp)
          if (length(below18) > 0) {
            start = ifelse(start >= 24, yes = start - 24, no = start)
            end = ifelse(end >= 24, yes = end - 24, no = end)
          }
          if (start > end) {
            r1B$filterWindow[which(r1B$date == qwindowImp$date[qi] & 
                                     r1B$hr >= start |
                                     r1B$hr < end)] = 1
          } else {
            r1B$filterWindow[which(r1B$date == qwindowImp$date[qi] & 
                                     r1B$hr >= start &
                                     r1B$hr < end)] = 1
          }
        } else {
          # If diary has one date missing use default window
          r1B$filter_method[which(r1B$hr >= 0 &
                                    r1B$hr < 6)] = 4
          # use midnight - 6am as fall back option
          r1B$filterWindow[which(r1B$hr >= 0 &
                                   r1B$hr < 6)] = 1
        }
      }
    }
    # Identify short nonwear during the night
    short_nonwear_night = which(r1B$values == 1 &
                                  r1B$lengths < nonwearFiltermaxHours &
                                  r1B$filterWindow == 1)
    
    if (length(short_nonwear_night) > 0) {
      nonwearHoursFiltered = (length(which(diff(short_nonwear_night) == 1)) * ws2) / 3600
      nonwearEventsFiltered = length(which(diff(short_nonwear_night) != 1)) + 1
      r1[short_nonwear_night] = 0
    }
  }
  invisible(list(r1 = r1, nonwearHoursFiltered = nonwearHoursFiltered,
                 nonwearEventsFiltered = nonwearEventsFiltered))
}