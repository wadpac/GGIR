g.part3_correct_guider = function(SLE, desiredtz, epochSize,
                                  guider_correction_maxgap_hrs = NULL) {
  # local parameters:
  
  # enhancement
  perc_HDCZA_outside_medmed = 0.9 # required fraction of HDCZA outside med-med to search for secondary HDCZA
  perc_HDCZA_inside_medmed = 0.4 #  required fraction of HDCZA inside med-med to accept secondary HDCZA
  min_hours_HDCZA = 2 # minimum duration of secondary HDCZA
  medmed_correction = TRUE # TO DO: Give user control of this
  # original algorithm
  perc_sib_HDCZA = 0.8 # required fraction with sib in HDCZA
  min_rest_length = 1 # minimum rest duration (hours) to be considered as possible extension of guider
  
  # local functions:
  get_matching_indices = function(SLE, reference_window, tz) {
    correct_for_DST = function(x, epochSize) {
      # When clock moves forward a timepoint may be missed
      missingday = which(diff(x) > (25 * 3600/epochSize))
      if (length(missingday) > 0) {
        missingday = missingday[1]
        x = c(x[1:missingday],
              x[missingday] + 24 * 3600/epochSize,
              x[(missingday + 1):length(x)])
      }
      return(x)
    }
    ref_min = ref_max = NULL
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
        ref_min = which(SLE$output$clocktime == ref_time)
      } else if (ki == 2) {
        ref_max = which(SLE$output$clocktime == ref_time)
      }
    }
    if (ref_max[1] < ref_min[1]) {
      # recording started after the start of reference causing a shift
      # correct for this
      ref_min = c(1, ref_min)
    }
    if (length(ref_min) > length(ref_max)) {
      # recording end before end of reference causing a shift
      # correct for this
      ref_max = c(ref_max, length(SLE$output$clocktime))
    }
    ref_min = correct_for_DST(ref_min, epochSize)    
    ref_max = correct_for_DST(ref_max, epochSize)
    
    invisible(list(ref_min = ref_min, ref_max = ref_max))
  }
  
  get_crude_estimate = function(SLE, tSegment) {
    # get crude estimate values between min-max pair
    crude_est = SLE$output$spt_crude_estimate[tSegment]
    sib = SLE$output[tSegment, grep(pattern = "time|invalid|night|estimate|medmed", x = colnames(SLE$output), invert = TRUE)]
    temp_time = SLE$output$time_POSIX[tSegment]
    # At this point rle_rest$values can have the following values:
    # 2: guider based classification of main sleep window
    # 1: other resting windows that were discarded
    # 0: remaining time
    if (1 %in% crude_est) { 
      # omit 1- segments that have less than 80% sib
      class_changes = diff(c(0, crude_est, 0)) 
      segment_start = which(class_changes == 1)
      segment_end = which(class_changes == -1) - 1
      for (gi in seq_along(segment_start)) {
        if (mean(sib[segment_start[gi]:segment_end[gi]]) < 0.8) {
          crude_est[segment_start[gi]:segment_end[gi]] = 0
        }
      }
    }
    return(crude_est)
  }
  
  clean_SLE = function(SLE) {
    # remove temp columns on exit
    temp_columns = c("clocktime", "time_POSIX", "spt_crude_estimate", "medmed")
    SLE$output = SLE$output[, which(names(SLE$output) %in% temp_columns == FALSE)]
    return(SLE)
  }
  
  convert_ts_to_hours = function(SLE, crude_est, tSegment, ref_value = 1) {
    # convert timeseries to zeros to clock hour in the day
    # redefine window by range
    new_window = range(which(crude_est == ref_value ))
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
    return(new_SPTE)
  }
  
  #-----------------------------------------
  # main code
  # HASPT includes a guider estimate for every night regardless of how much
  # invalid data there is, but here we should only the valid nights
  invalid_per_night = aggregate(x = SLE$output$invalid, by = list(SLE$output$night), FUN = sum)
  names(invalid_per_night) = c("night", "count")
  valid_nights = invalid_per_night$night[which(invalid_per_night$night > 0 & invalid_per_night$count < 24 * 3600 / epochSize * 0.333)]
  if (length(valid_nights) > 0) {
    valid_nights = valid_nights[which(is.na(SLE$SPTE_start[valid_nights]) == FALSE &
                                        is.na(SLE$SPTE_end[valid_nights]) == FALSE)]
  }
  SLE$SPTE_corrected = rep(0, length(SLE$SPTE_start))
  
  if (length(valid_nights) == 0) {
    SLE = clean_SLE(SLE)
    return(SLE)
  }
  
  # Step 1) Identify reference guider across all nights
  # from the earliest (min) start till the latest (max) end.
  reference_window = c(min(SLE$SPTE_start[valid_nights], na.rm = TRUE),
                       max(SLE$SPTE_end[valid_nights], na.rm = TRUE))
  SLE$output$time_POSIX = iso8601chartime2POSIX(SLE$output$time, tz = desiredtz)
  SLE$output$clocktime = format(SLE$output$time_POSIX, format = "%H:%M:%S")
  
  # Get indices of full time series where the reference guider matches the timestamps
  ref_indices = get_matching_indices(SLE, reference_window,  tz = desiredtz)
  ref_min = ref_indices$ref_min
  ref_max = ref_indices$ref_max
  
  if (length(ref_min) < 2 || length(ref_max) < 2) {
    SLE = clean_SLE(SLE)
    return(SLE)
  }
  
  if (medmed_correction == TRUE) {
    # Step 2) Identify median-median window and corresponding indices in the time series
    # this window will be used to assess whether HDCZA window can be replaced
    medmed_reference_window = c(median(SLE$SPTE_start[valid_nights], na.rm = TRUE),
                                median(SLE$SPTE_end[valid_nights], na.rm = TRUE))
    ref_indices = get_matching_indices(SLE, medmed_reference_window,  tz = desiredtz)
    ref_med1 = ref_indices$ref_min
    ref_med2 = ref_indices$ref_max
    if (length(ref_med1) < 2 || length(ref_med2) < 2) {
      SLE = clean_SLE(SLE)
      return(SLE)
    }
    # Deal with non-matching index vectors caused by incomplete first or last night
    Ntimepoints = length(SLE$output$clocktime)
    newVectors = g.part3_alignIndexVectors(x = ref_min, y = ref_max,
                                           a = ref_med1, b = ref_med2,
                                           N = Ntimepoints) 
    ref_min = newVectors$x
    ref_max = newVectors$y
    ref_med1 = newVectors$a
    ref_med2 = newVectors$b
    
    if (length(ref_min) != length(ref_med1) ||
        length(ref_max) != length(ref_med2)) {
      stop("index vectors do not match")
    }
    # Step 3): Select an alternative HDCZA window if the default falls outside med-med
    # and there is a secondary HDCZA in med-med.
    
    # - Check each night for main HDCZA outside med-med window and correct if needed
    for (ji in 1:length(ref_min)) {
      if (ji %in% valid_nights) {
        # get crude estimate values between min-max pair
        tSegment = ref_min[ji]:ref_max[ji]
        crude_est = get_crude_estimate(SLE, tSegment)
        sib = SLE$output[tSegment, grep(pattern = "time|invalid|night|estimate|medmed", x = colnames(SLE$output), invert = TRUE)]
        # derive time series across min-max with medmed indicated
        tSegment_med = ref_med1[ji]:ref_med2[ji]
        SLE$output$medmed = 0
        SLE$output$medmed[tSegment_med] = 1
        medmed = SLE$output$medmed[tSegment]
        # Is original HDCZA estimate largely outside median-median window?
        if (length(tSegment_med) > 3600 / epochSize &&
            length(which(crude_est == 2)) != 0  &&
            length(which(crude_est == 2 & medmed == 0)) / 
            length(which(crude_est == 2)) > perc_HDCZA_outside_medmed) {
          # To assess other criteria first create summary per segment
          # and give each non-zero segment a unique segment id
          temp_rle = rle(crude_est)
          nonzero = which(temp_rle$values != 0)
          temp_rle$values[nonzero] = 1:length(nonzero)
          seg_id = rep(temp_rle$values, temp_rle$lengths)
          
          df = data.frame(crude_est = crude_est, medmed = medmed, 
                          index = 1:length(crude_est), sib = sib,
                          seg_id = seg_id)
          # Crude estimate per segment (code by 0, 1 or 2 as explained above)
          segment_level = aggregate(df[, c("crude_est", "seg_id")], by = list(df$seg_id), FUN = mean)[, 1:2]
          names(segment_level) = c("seg_id", "crude_est")
          # sib
          segment_sib = aggregate(df[, c("sib", "seg_id")], by = list(df$seg_id), FUN = mean)[, 1:2]
          names(segment_sib) = c("seg_id", "sib")
          segment_summary = merge(segment_level, segment_sib, by = "seg_id")
          
          # Overlaps with median-median window
          perc_value_one = function(x) {
            return(length(which(x == 1)) / length(x))
          }
          segment_overlap_medmed = aggregate(df[, c("medmed", "seg_id")], by = list(df$seg_id), FUN = perc_value_one)[, 1:2]
          names(segment_overlap_medmed) = c("seg_id", "medmed")
          segment_summary = merge(segment_summary, segment_overlap_medmed, by = "seg_id")
          
          # duration
          segment_size_hours = aggregate(df$index, by = list(df$seg_id), FUN = length)[, 1:2]
          segment_size_hours[, 2] = segment_size_hours[, 2] / (3600 / epochSize)
          names(segment_size_hours) = c("seg_id", "segment_size_hours")
          segment_summary = merge(segment_summary, segment_size_hours, by = "seg_id")
          
          # Start
          segment_start = aggregate(df[, c("index", "seg_id")], by = list(df$seg_id), FUN = min)[, 1:2]
          names(segment_start) = c("seg_id", "start_index")
          segment_summary = merge(segment_summary, segment_start, by = "seg_id")
          
          # End
          segment_end = aggregate(df[, c("index", "seg_id")], by = list(df$seg_id), FUN = max)[, 1:2]
          names(segment_end) = c("seg_id", "end_index")
          segment_summary = merge(segment_summary, segment_end, by = "seg_id")
          
          rm(segment_sib, segment_overlap_medmed, segment_start, segment_size_hours, segment_end)
          
          SLE$output = SLE$output[, -which(colnames(SLE$output) == "medmed")]
          # Identify whether there is a secondary HDCZA window that meets
          # following criteria:
          # - (Mostly) Overlaps with the median-median window
          # - lasts at least 3 hours
          # - has at least 80% sustained inactivity (already removed above)
          new_main_HDCZA = which(segment_summary$crude_est == 1 &
                                   segment_summary$medmed > perc_HDCZA_inside_medmed &
                                   segment_summary$segment_size_hours > min_hours_HDCZA &
                                   segment_summary$sib > perc_sib_HDCZA)

          if (length(new_main_HDCZA) > 0) {
            # If yes, then select this other HDCZA window.
            # Update such that old window has value 1
            old_2 = which(segment_summary$crude_est == 2)
            segment_summary$crude_est[old_2] = 1
            crude_est[segment_summary$start_index[old_2]:segment_summary$end_index[old_2]] = 1
            # Update such that new window has 2
            segment_summary$crude_est[new_main_HDCZA] = 2
            crude_est[segment_summary$start_index[new_main_HDCZA]:segment_summary$end_index[new_main_HDCZA]] = 2
            
            # Update corresponding SLE$SPTE_start and SLE$SPTE_end values.
            new_SPTE = convert_ts_to_hours(SLE, crude_est, tSegment, ref_value = 2)
            SLE$output$spt_crude_estimate[tSegment] = crude_est
            SLE$SPTE_start[ji] = new_SPTE[1]
            SLE$SPTE_end[ji] = new_SPTE[2]
            SLE$SPTE_corrected[ji] = 2
          }
        }
      }
    }
    if (any(SLE$SPTE_corrected != 0)) {
      # Step 4):
      # If any night was corrected, repeat the initial steps of min-max and index extraction:
      # Identify reference guider across all nights
      reference_window = c(min(SLE$SPTE_start[valid_nights], na.rm = TRUE),
                           max(SLE$SPTE_end[valid_nights], na.rm = TRUE))
      
      # Get indices of full time series where the reference guider matches the timestamps
      ref_indices = get_matching_indices(SLE, reference_window,  tz = desiredtz)
      ref_min = ref_indices$ref_min
      ref_max = ref_indices$ref_max
      if (length(ref_min) < 2 || length(ref_max) < 2) {
        SLE = clean_SLE(SLE)
        return(SLE)
      }
    }
  }
  # Step 5): Loop again over nights but this time expand HDCZA with neighboring HDCZA
  # if criteria are met.
  
  
  # # Check each night and correct if needed
  for (ji in 1:length(ref_min)) {
    if (ji %in% valid_nights) {
      # get crude estimate values between min-max pair
      tSegment = ref_min[ji]:ref_max[ji]
      crude_est = get_crude_estimate(SLE, tSegment)
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
        if (length(long_rest) > 0) {
          if (!is.null(guider_correction_maxgap_hrs) && 
              !is.infinite(guider_correction_maxgap_hrs)) {
            long_wake = which(rle_rest$values == 0 & rle_rest$lengths * epochSize >= guider_correction_maxgap_hrs * 3600)
            if (length(long_wake) > 0) {
              rle_rest$values[long_wake] = -1
              ind2remove = NULL
              for (lri in 1:length(long_rest)) {
                # for each long rest (1)
                # check whether any of the long wake (-1) separates
                # it from the original HDCZA (2)
                original = which(rle_rest$values == 2) # only one segment is expected to be 2
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
          
          new_SPTE = convert_ts_to_hours(SLE, crude_est, tSegment, ref_value = 1)
          SLE$SPTE_start[ji] = new_SPTE[1]
          SLE$SPTE_end[ji] = new_SPTE[2]
          SLE$SPTE_corrected[ji] = 1
        }
      }
    }
  }
  SLE = clean_SLE(SLE)
  return(SLE)
}