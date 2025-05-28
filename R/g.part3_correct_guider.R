g.part3_correct_guider = function(SLE, desiredtz, epochSize) {
  
  # Add min max guider across all nights to each night
  minmax_window = c(min(SLE$SPTE_start), max(SLE$SPTE_end))
  SLE$output$time_POSIX = iso8601chartime2POSIX(SLE$output$time, tz = desiredtz)
  SLE$output$clocktime = format(SLE$output$time_POSIX, format = "%H:%M:%S")
  for (ki in 1:2) {
    if (minmax_window[ki] >= 24) {
      minmax_window[ki] = minmax_window[ki] - 24
    }
    HR = floor(minmax_window[ki])
    MIN = floor((minmax_window[ki] - HR) * 60)
    SEC = round((minmax_window[ki] - HR - (MIN / 60)) * 3600)
    HR = ifelse(HR < 10, yes = paste0("0", HR), no = as.character(HR))
    MIN = ifelse(MIN < 10, yes = paste0("0", MIN), no = as.character(MIN))
    SEC = ifelse(SEC < 10, yes = paste0("0", SEC), no = as.character(SEC))
    ref_time = paste(HR, MIN, SEC, sep = ":")
    if (ki == 1) {
      start_minmax = which(SLE$output$clocktime == ref_time)
    } else if (ki == 2) {
      end_minmax = which(SLE$output$clocktime == ref_time)
    }
  }
  # # Check each night and correct if needed
  for (ji in 1:length(start_minmax)) {
    # get crude estimate values between min-max pair
    tSegment = start_minmax[ji]:end_minmax[ji]
    crude_est = SLE$output$spt_crude_estimate[tSegment]
    # only consider window if there is rest outside guider
    # as indicated by a 1, because 2 is the current guider and zero is not restign
    if (1 %in% crude_est) { 
      # Identify length of these resting blocks
      rle_rest = rle(crude_est)
      long_rest = which(rle_rest$values == 1 & rle_rest$lengths * epochSize >= 3600)
      if (length(long_rest) > 0) {
        rle_rest$values[long_rest] = 2
        rle_rest$values[which(rle_rest$values == 1)] = 0
        rle_rest$values[which(rle_rest$values == 2)] = 1
        N = length(crude_est)
        crude_est = rep(rle_rest$values, rle_rest$lengths)[1:N]
        # redefine window by range
        new_window = range(which(crude_est == 1))
        new_SPTE = SLE$output$clocktime[tSegment[new_window]]
        convert_time = function(x) {
          spit_time = as.numeric(unlist(strsplit(x, ":")))
          time_hours = round(sum((spit_time * c(3600, 60, 1)) / 3600), digits = 3)
          if (time_hours < 18) time_hours = time_hours + 24
          return(time_hours)
        }
        new_SPTE = unlist(lapply(X = new_SPTE, FUN = convert_time))
        # when person falls asleep before 6pm on first day
        # the 24 hour correction needs to be corrected:
        if (new_SPTE[1] > new_SPTE[2]) new_SPTE[1] = new_SPTE[1] - 24
        SLE$SPTE_start[ji] = new_SPTE[1]
        SLE$SPTE_end[ji] = new_SPTE[2]
      }
    }
  }
  # remove temp columns
  temp_columns = c("clocktime", "time_POSIX", "spt_crude_estimate")
  SLE$output = SLE$output[, which(names(SLE$output) %in% temp_columns == FALSE)]
  return(SLE)
}