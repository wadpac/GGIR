filterNonwearNight = function(r1, metalong, qwindowImp, desiredtz,
                              params_cleaning, ws2) {
  nonwearFiltermaxHours  = params_cleaning[["nonwearFiltermaxHours"]]
  nonwearFilterWindow = params_cleaning[["nonwearFilterWindow"]]
  
  # Identify method to use
  if (!is.null(nonwearFilterWindow)) {
    filter_method = 1 # set window as provide by use
  } else {
    errorMessage = paste0("Please specify parameter nonwearFilterWindow or ",
                          "qwindow with length > 3 to ",
                          "define the window used for filtering short nonwear")
    if (!is.null(qwindowImp)) {
      if (inherits(qwindowImp, "data.frame")) {
        filter_method = 3
      } else {
        if (length(qwindowImp) > 3) {
          filter_method = 4
          nonwearFilterWindow = c(rev(qwindowImp)[2], qwindowImp[2])
        } else {
          stop(errorMessage, call. = FALSE)    
        }
      }
    } else {
      filter_method = 2
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
    r1B$lengths = (r1B$lengths * ws2) / 3600 # convert unit from epoch to hours
    r1B$filterWindow = 0
    # Define night window
    if (filter_method %in% c(1, 2, 3)) { # set window same for all recordings
      if (nonwearFilterWindow[1] > nonwearFilterWindow[2]) {
        r1B$filterWindow[which(r1B$hr >= nonwearFilterWindow[1] |
                                 r1B$hr < nonwearFilterWindow[2])] = 1
      } else {
        r1B$filterWindow[which(r1B$hr >= nonwearFilterWindow[1] &
                                 r1B$hr < nonwearFilterWindow[2])] = 1
      }
    } else if (filter_method == 4) { # based on qwindow specific per participant
      r1B$date = as.Date(metalong$time_POSIX)
      for (qi in 1:nrow(qwindowImp)) {
        start = rev(qwindowImp$qwindow_values[[qi]])[2]
        end = qwindowImp$qwindow_values[[qi]][2]
        if (start > end) {
          r1B$filterWindow[which(r1B$date == qwindowImp$date[qi] & 
                                   r1B$hr >= start |
                                   r1B$hr < end)] = 1
        } else {
          r1B$filterWindow[which(r1B$date == qwindowImp$date[qi] & 
                                   r1B$hr >= start &
                                   r1B$hr < end)] = 1
        }
      }
    }
    # Identify short nonwear during the night
    # browser()
    short_nonwear_night = which(r1B$values == 1 &
                                  r1B$lengths < nonwearFiltermaxHours &
                                  r1B$filterWindow == 1)
    
    if (length(short_nonwear_night) > 0) {
      r1[short_nonwear_night] = 0
    }
  }
  return(r1)
}