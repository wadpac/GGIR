g.part3_correct_guider = function(SLE, desiredtz, epochSize,
                                  guider_correction_maxgap_hrs = NULL) {
  # HASPT includes a guider estimate for every night regardless of how much
  # invalid data there is, but here we should only the valid nights
  invalid_per_night = aggregate(x = SLE$output$invalid, by = list(SLE$output$night), FUN = sum)
  names(invalid_per_night) = c("night", "count")
  valid_nights = invalid_per_night$night[which(invalid_per_night$night > 0 & invalid_per_night$count < 24 * 3600 / epochSize * 0.333)]
  if (length(valid_nights) > 0) {
    valid_nights = valid_nights[which(is.na(SLE$SPTE_start[valid_nights]) == FALSE &
                                        is.na(SLE$SPTE_end[valid_nights]) == FALSE)]
  }
  SLE$SPTE_corrected = rep(FALSE, length(SLE$SPTE_start))
  
  clean_SLE = function(SLE) {
    # remove temp columns on exit
    temp_columns = c("clocktime", "time_POSIX", "spt_crude_estimate")
    SLE$output = SLE$output[, which(names(SLE$output) %in% temp_columns == FALSE)]
    return(SLE)
  }
  
  if (length(valid_nights) == 0) {
    SLE = clean_SLE(SLE)
    return(SLE)
  }
  
  # Add reference guider across all nights to each night
  reference_window = c(min(SLE$SPTE_start[valid_nights], na.rm = TRUE),
                       max(SLE$SPTE_end[valid_nights], na.rm = TRUE))
  SLE$output$time_POSIX = iso8601chartime2POSIX(SLE$output$time, tz = desiredtz)
  SLE$output$clocktime = format(SLE$output$time_POSIX, format = "%H:%M:%S")
  start_reference = end_reference = NULL
  for (ki in 1:2) {
    if (reference_window[ki] >= 24) {
      reference_window[ki] = reference_window[ki] - 24
    }
    HR = floor(reference_window[ki])
    MIN = floor((reference_window[ki] - HR) * 60)
    SEC = floor((reference_window[ki] - HR - (MIN / 60)) * 3600)
    SEC = floor(SEC / epochSize) * epochSize
    HR = ifelse(HR < 10, yes = paste0("0", HR), no = as.character(HR))
    MIN = ifelse(MIN < 10, yes = paste0("0", MIN), no = as.character(MIN))
    SEC = ifelse(SEC < 10, yes = paste0("0", SEC), no = as.character(SEC))
    ref_time = paste(HR, MIN, SEC, sep = ":")
    if (ki == 1) {
      start_reference = which(SLE$output$clocktime == ref_time)
    } else if (ki == 2) {
      end_reference = which(SLE$output$clocktime == ref_time)
    }
  }
  if (end_reference[1] < start_reference[1]) {
    # recording started after the start of reference causing a shift
    # correct for this
    start_reference = c(1, start_reference)
  }
  if (length(start_reference) > length(end_reference)) {
    # recording end before end of reference causing a shift
    # correct for this
    end_reference = c(end_reference, length(SLE$output$clocktime))
  }
  
  # minimum rest duration to be considered as possible extension of guider
  min_rest_length = 1 #hours
  # # Check each night and correct if needed
  for (ji in 1:length(start_reference)) {
    if (ji %in% valid_nights) {
      # get crude estimate values between min-max pair
      tSegment = start_reference[ji]:end_reference[ji]
      crude_est = SLE$output$spt_crude_estimate[tSegment]
      sib = SLE$output[tSegment, grep(pattern = "time|invalid|night|estimate", x = colnames(SLE$output), invert = TRUE)]
      temp_time = SLE$output$time_POSIX[tSegment]
      # At this point rle_rest$values can have the following values:
      # 2: guider based classification of main sleep window
      # 1: other resting windows that were discarded
      # 0: remaining time
      if (1 %in% crude_est) { 
        # omit segments that have less than 80% sib
        class_changes = diff(c(0, crude_est, 0)) 
        segment_start = which(class_changes == 1)
        segment_end = which(class_changes == -1) - 1
        for (gi in seq_along(segment_start)) {
          if (mean(sib[segment_start[gi]:segment_end[gi]]) < 0.8) {
            crude_est[segment_start[gi]:segment_end[gi]] = 0
          }
        }
      }
      
      # hours_in_class_1 = (length(which(crude_est == 1)) * epochSize) / 3600
      # if (hours_in_class_1 < 2) {
      #   # only consider nights with 2 or more hours outside main sleep window
      #   # we are only interested in fixing major night distortions
      #   next 
      # }
      
      # only consider window if there is rest outside guider
      # as indicated by a 1, because 2 is the current guider and zero is not resting
      if (1 %in% crude_est) { 
        rle_rest = rle(crude_est)
        
        #--------------------------------------------
        # Identify long resting blocks
        long_rest = which(rle_rest$values == 1 & rle_rest$lengths * epochSize >= min_rest_length * 3600)
        #--------------------------------------------
        # Remove any long rest that are separated from main sleep
        # by a too long wake period
        if (!is.null(guider_correction_maxgap_hrs) && !is.infinite(guider_correction_maxgap_hrs)) {
          long_wake = which(rle_rest$values == 0 & rle_rest$lengths * epochSize >= guider_correction_maxgap_hrs * 3600)
          if (length(long_wake) > 0) {
            rle_rest$values[long_wake] = -1
            ind2remove = NULL
            if (length(long_rest) > 0) {
              for (lri in 1:length(long_rest)) {
                original = which(rle_rest$values == 2)
                this_long_rest = long_rest[lri]
                too_long_wake = which(rle_rest$values == -1)
                if (any(too_long_wake > original & too_long_wake < this_long_rest) |
                    any(too_long_wake < original & too_long_wake > this_long_rest)) {
                  ind2remove = c(ind2remove, lri)
                }
              }
              if (!is.null(ind2remove)) {
                long_rest = long_rest[-ind2remove]
              }
            }
          }
          
        }
        #--------------------------------------------
        # Any remaining long rests are considered
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
          SLE$SPTE_start[ji] = new_SPTE[1]
          SLE$SPTE_end[ji] = new_SPTE[2]
          SLE$SPTE_corrected[ji] = TRUE
        }
      }
    }
  }
  SLE = clean_SLE(SLE)
  return(SLE)
}