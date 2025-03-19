markerButtonForRest = function(sibreport, params_sleep, ts) {
  #-----------------------------------------------------------
  # Consider using marker button data to aid nap detection
  
  # nap_markerbutton_method:
  # 0 do not use marker button (default)
  # 1 do not require it and copy timing => use marker button to improve definition of nap
  # 2 require it and do not copy timing => use marker button as a condition for nap detection
  # 3 require it and copy timing => use marker button to improve definition and as a condition for nap detection
  
  # Explanation objects as used below
  # nap_copy_timing_mb: rely on marker for final timing of the nap?
  # if TRUE, only consider marker buttons  before midtime of sib for start
  # and only consider marker buttons after midtime of sib for end
  # if FALSE, then consider any mark button, this uses the marker solely as 
  # a confirmation that nap nearby
  
  # nap_require_mb: require availability of marker to consider 
  # detection of a nap
  # if TRUE, only keep sibs that had marker button nearby
  # if FALSE, also keep sibs that may not have marker button but meet 
  # criteria for another reason
  # 
  sibreport$ignore = FALSE # if no marker button than keep sibs
  if  (params_sleep[["nap_markerbutton_method"]] > 0) {
    if (params_sleep[["nap_markerbutton_method"]] > 1) {
      sibreport$ignore = TRUE # if no marker button then ignore all sibs
    }
    # maximum gap allowed between sib edge and marker
    nap_mb_max_dist = params_sleep[["nap_markerbutton_max_distance"]]
    
    # If marker button data available
    if ("marker" %in% colnames(ts)) {
      if (1 %in% ts$marker) {
        if (params_sleep[["nap_markerbutton_method"]] == 1) {
          nap_require_mb = FALSE
          nap_copy_timing_mb = TRUE
        } else if (params_sleep[["nap_markerbutton_method"]] == 2) {
          nap_require_mb = TRUE
          nap_copy_timing_mb = FALSE
        } else if (params_sleep[["nap_markerbutton_method"]] == 3) {
          nap_require_mb = TRUE
          nap_copy_timing_mb = TRUE
        }
        # Calculate time in minutes to nearest button press
        # that starts before and after midway the sib, for start and end respectively
        sibreport$midtime = sibreport$start + (sibreport$end - sibreport$start) / 2
        marker_times = ts$time[which(ts$marker == 1)]
        sibreport$start_to_marker = NA
        sibreport$end_to_marker = NA
        
        for (hi in 1:nrow(sibreport)) {
          # assess duration marker buttons relative to sib midtime
          delta_times = as.numeric(sibreport$midtime[hi]) - as.numeric(marker_times)
          if (nap_copy_timing_mb == TRUE) {
            # Plan is to use marker button timing, so only consider
            # for start the marker buttons that start before midtime
            end_delta_times = delta_times[which(delta_times < 0)]
            # Plan is to use marker button timing, so only consider
            # for end the marker buttons that start after midtime
            start_delta_times = delta_times[which(delta_times > 0)]
          } else {
            # Plan is NOT to use marker button timing, so any nearby
            # marker button is sufficient
            start_delta_times = end_delta_times = delta_times
          }
          nap_start_found = nap_end_found = FALSE
          half_sib_dur = sibreport$duration[hi] / 2
          tmp_start = tmp_end = NULL
          # find marker for start nap
          if (length(start_delta_times) > 0) {
            sibreport$start_to_marker[hi] = min(abs(start_delta_times)) / 60 - half_sib_dur
            if (any(sibreport$start_to_marker[hi] < nap_mb_max_dist)) {
              # marker button nearby
              if (nap_copy_timing_mb == TRUE) {
                # use nearby marker button to overwrite sib start
                tmp_start = sibreport$midtime[hi] - start_delta_times[which.min(abs(start_delta_times))]
              }
              nap_start_found = TRUE
            }
          }
          # find marker for end nap
          if (length(end_delta_times) > 0) {
            sibreport$end_to_marker[hi] = min(abs(end_delta_times)) / 60 - half_sib_dur
            if (any(sibreport$end_to_marker[hi] < nap_mb_max_dist)) {
              # marker button nearby
              if (nap_copy_timing_mb == TRUE) {
                # use nearby marker button to overwrite sib end
                tmp_end = sibreport$midtime[hi] + abs(end_delta_times[which.min(abs(end_delta_times))])
              }
              nap_end_found = TRUE
            }
          }
          if (nap_require_mb == TRUE && (nap_start_found == FALSE || nap_end_found == FALSE)) { 
            # if nearby marker button is required but none was available
            # ignore this sib
            sibreport$ignore[hi] = TRUE
          } else {
            # enable nap detection further down becaue marker button was found
            sibreport$ignore[hi] = FALSE
            # overwrite all other sibs that overlap such that they form duplicated rows
            if (length(tmp_start) > 0) {
              sibreport$start[hi] = tmp_start
              sibreport$start[which(sibreport$start < sibreport$midtime[hi] & sibreport$start >= tmp_start)] = sibreport$start[hi]
            }
            if (length(tmp_end) > 0) {
              sibreport$end[hi] = tmp_end
              sibreport$end[which(sibreport$end < sibreport$midtime[hi] & sibreport$end >= tmp_end)] = sibreport$end[hi]
            }
            # recalculate midtime
            sibreport$midtime = sibreport$start + (sibreport$end - sibreport$start) / 2
          }
        }
      }
    }
  }
  # remove duplicate rows
  sibreport = sibreport[!duplicated(sibreport[, c("start", "end")]), ]
  return(sibreport)
}