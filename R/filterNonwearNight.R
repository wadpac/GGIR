filterNonwearNight = function(r1, metalong, qwindowImp, desiredtz,
                              params_cleaning, ws2) {
  nonwearFiltermaxHours  = params_cleaning[["nonwearFiltermaxHours"]]
  nonwearFilterWindow = params_cleaning[["nonwearFilterWindow"]]
  nonwearEventsFiltered = nonwearHoursFiltered = 0
  # Identify method to use
  if (!is.null(nonwearFilterWindow)) {
    filter_method = 1 # set window as provide by use
  } else {
    # If nonwearFilterWindow is not provided then
    # assumption is the qwindow is available
    errorMessage = paste0("Please specify parameter nonwearFilterWindow or ",
                          "qwindow with length > 3 to ",
                          "define the window used for filtering short nonwear")
    if (!is.null(qwindowImp)) {
      if (inherits(qwindowImp, "data.frame")) {
        filter_method = 2
      } else {
        if (length(qwindowImp) > 3) {
          filter_method = 3
          nonwearFilterWindow = c(rev(qwindowImp)[2], qwindowImp[2])
        } else {
          # qwindow does not have more than 3 values
          stop(errorMessage, call. = FALSE)    
        }
      }
    } else {
      # qwindow not available
      stop(errorMessage, call. = FALSE)
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
    if (filter_method %in% c(1, 3)) { # set window same for all recordings
      if (nonwearFilterWindow[1] > nonwearFilterWindow[2]) {
        r1B$filterWindow[which(r1B$hr >= nonwearFilterWindow[1] |
                                 r1B$hr < nonwearFilterWindow[2])] = 1
      } else {
        r1B$filterWindow[which(r1B$hr >= nonwearFilterWindow[1] &
                                 r1B$hr < nonwearFilterWindow[2])] = 1
      }
    } else if (filter_method == 2) { # based on qwindow specific per participant
      r1B$date = as.Date(metalong$time_POSIX)
      for (qi in 1:nrow(qwindowImp)) {
        qwindow_temp = qwindowImp$qwindow_values[[qi]]
        # filter only SPT and time in bed reports
        qwindow_temp = qwindow_temp[grep(pattern = "bed|wakeup|sleeponset|lights", x = qwindowImp$qwindow_names[[qi]])]
        qwindow_temp = sort(qwindow_temp)
        if (length(qwindow_temp) > 3) {
          # convert to continuous scale to ease finding start and end
          below18 = which(qwindow_temp < 18)
          if (length(below18) > 0) {
            qwindow_temp = qwindow_temp[below18] + 24
          }
          start = min(qwindow_temp)
          end = max(qwindow_temp)
          if (length(below18) > 0) {
            start = ifelse(start > 24, yes = start - 24, no = start)
            end = ifelse(end > 24, yes = end - 24, no = end)
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