g.part3_correct_guider = function(SLE, desiredtz, epochSize) {
  # HASPT includes a guider estimate for every night regardless of how much
  # invalid data there is, but here we should only the valid nights
  invalid_per_night = aggregate(x = SLE$output$invalid, by = list(SLE$output$night), FUN = sum)
  names(invalid_per_night) = c("night", "count")
  valid_nights = invalid_per_night$night[which(invalid_per_night$night > 0 & invalid_per_night$count < 24 * 3600 / epochSize * 0.333)]
  
  if (length(valid_nights) > 0) {
    SPTE_start = SLE$SPTE_start[valid_nights]
    SPTE_end = SLE$SPTE_end[valid_nights]
  }
  if (length(SPTE_start) > 0 && any(!is.na(SPTE_start)) &&
      length(SPTE_end) > 0 && any(!is.na(SPTE_end))) {
    # Add min max guider across all nights to each night
    minmax_window = c(min(SPTE_start, na.rm = TRUE), max(SPTE_end, na.rm = TRUE))
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
    if (end_minmax[1] < start_minmax[1]) {
      # recording started after the start of minmax causing a shift
      # correct for this
      start_minmax = start_minmax[-length(start_minmax)]
      end_minmax = end_minmax[-1]
    }
    # Draft code to implement max gap length:
    # max_gap_length = NULL # hours
    min_rest_length = 1 #hours
    
    # # Check each night and correct if needed
    for (ji in 1:length(start_minmax)) {
      # get crude estimate values between min-max pair
      tSegment = start_minmax[ji]:end_minmax[ji]
      crude_est = SLE$output$spt_crude_estimate[tSegment]
      
      # only consider window if there is rest outside guider
      # as indicated by a 1, because 2 is the current guider and zero is not restign
      if (1 %in% crude_est) { 
        rle_rest = rle(crude_est)
        #--------------------------------------------
        # Draft code to implement max gap length:
        # # Label too long wake period:
        # long_wake = which(rle_rest$values == 0 & rle_rest$lengths * epochSize >= max_gap_length * 3600)
        # if (length(long_wake) > 0) {
        #   rle_rest$values[long_wake] = -1
        # }
        #--------------------------------------------
        # Identify long resting blocks
        long_rest = which(rle_rest$values == 1 & rle_rest$lengths * epochSize >= min_rest_length * 3600)
        #--------------------------------------------
        # Draft code to implement max gap length:
        # if (!is.null(max_gap_length)) {
        #   # Remove any long rest that are separated from main sleep by a too long wake period
        #   ind2remove = NULL
        #   if (length(long_rest) > 0) {
        #     for (lri  in 1:long_rest) {
        #       after = long_rest[lri] + 1:2
        #       before = long_rest[lri] + -2:-1
        #       after = after[which(after > 0 & after <= length(rle_rest$values))]
        #       before = before[which(before > 0 & before <= length(rle_rest$values))]
        #       if (length(before) == 2 && rle_rest$values[before[1]] == 2 && rle_rest$values[before[2]] == -1) {
        #         ind2remove = c(ind2remove, lri)
        #       }
        #       if (length(after) == 2 && rle_rest$values[after[1]] == -1 && rle_rest$values[after[2]] == 2) {
        #         ind2remove = c(ind2remove, lri)
        #       }
        #     }
        #     if (!is.null(ind2remove)) {
        #       long_rest = long_rest[-ind2remove]
        #     }
        #   }
        # }
        #--------------------------------------------
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
            if (time_hours <= 12) time_hours = time_hours + 24
            return(time_hours)
          }
          new_SPTE = unlist(lapply(X = new_SPTE, FUN = convert_time))
          # when person falls asleep before 6pm on first day
          # the 24 hour correction needs to be corrected:
          if (new_SPTE[1] > new_SPTE[2]) {
            new_SPTE[1] = new_SPTE[1] - 24
          }
          # if (new_SPTE[1] > new_SPTE[2]) new_SPTE[2] = new_SPTE[2] + 24
          
          SPTE_start[ji] = new_SPTE[1]
          SPTE_end[ji] = new_SPTE[2]
        }
      }
    }
  }
  SLE$SPTE_start[valid_nights] = SPTE_start
  SLE$SPTE_end[valid_nights] = SPTE_end
  # remove temp columns
  temp_columns = c("clocktime", "time_POSIX", "spt_crude_estimate")
  SLE$output = SLE$output[, which(names(SLE$output) %in% temp_columns == FALSE)]
  return(SLE)
}