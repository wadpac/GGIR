HASPT = function(angle, params_sleep = NULL, ws3 = 5,
                 HASPT.algo = "HDCZA", invalid,
                 activity = NULL, marker = NULL,
                 sibs = NULL) {
  tib.threshold = SPTE_start = SPTE_end = part3_guider = spt_crude_estimate = NULL
  
  #-------------------------------------
  # Use marker button as guider:
  # if available and required by user (only Actiwatch and Philips Health band at the moment)
  # See documentation for parameter consider_marker_button.
  if (length(marker) > 0 && params_sleep[["consider_marker_button"]] == TRUE) {
    button_pressed = which(marker != 0)
    N_markers = length(button_pressed)
    # Only consider when there is more than 1 marker and the range spans more than 1 hour
    fraction_invalid = length(which(invalid == 1)) / length(invalid)
    if (N_markers > 1 &&
        diff(range(button_pressed)) * (60/ws3) > 60 &&
        fraction_invalid < 0.5) {
      # Consider the 10 marker buttons closest to the day of interest,
      # or all marker buttons for that day if more than 10
      ranking = sort(marker[button_pressed], decreasing = TRUE, index.return = TRUE)$ix
      button_pressed = button_pressed[ranking[1:pmax(length(which(marker == 1)), 10)]]
      # find marker pair with lowest amount of activity in between and longest duration
      pairs = expand.grid(button_pressed, button_pressed)
      pairs = pairs[which(pairs$Var1 < pairs$Var2),]
      # define duration
      pairs$duration_hours = (pairs$Var2 - pairs$Var1) / (3600/ws3)
      
      # ignore pairs that reflect both short distances and involve imputed marker buttons
      pairs$mark1 = marker[pairs$Var1]
      pairs$mark2 = marker[pairs$Var2]
      pairs_to_ignore = which((pairs$duration_hours < 3 & (pairs$mark1 < 1 | pairs$mark2 < 1)) |
                                (pairs$duration_hours < 1))
      if (length(pairs_to_ignore) > 0 & length(pairs_to_ignore) < nrow(pairs)) {
        pairs = pairs[-pairs_to_ignore,]
      }
      # define activity
      pairs$activity = 0
      if (is.null(activity) && !is.null(sibs)) {
        activity_tmp = 1 - sibs
      } else {
        activity_tmp = activity
      }
      # ignore invalid timepoints
      invalid_index = which(invalid == 1)
      if (length(invalid_index) > 0) {
        activity_tmp[invalid_index] = NA
      }
      pairs$fraction_sibs_outside = NA
      pairs$fraction_sibs_inside = NA
      # create table of each marker button pair and their statistics
      for (i in 1:nrow(pairs)) {
        i0 = pairs$Var1[i]
        i1 = pairs$Var2[i]
        pairs$activity[i] = mean(activity_tmp[i0:i1], na.rm = TRUE) + 1
        if (i0 > 1 && i1 < length(sibs)) {
          outside = c(1:(i0 - 1), (i1 + 1):length(sibs))
          sum_outside = sum(sibs[outside])
          fraction_outside = sum_outside / length(outside)
          fraction_inside = sum(sibs[i0:i1]) / (i1 - i0 + 1)
          pairs$fraction_sibs_inside[i] = fraction_inside
          pairs$fraction_sibs_outside[i] = fraction_outside
        }
      }
      # Only consider when fraction sibs outside is less than inside and time spent outside is less than 6 hours
      pairs = pairs[which(pairs$fraction_sibs_outside < pairs$fraction_sibs_inside),]
      if (nrow(pairs) > 0) {
        # Define performance score
        pairs$dur_score = pmax(abs(pairs$duration_hours - 8), 1) - 1
        pairs$score = pairs$activity * pairs$dur_score
        # find winning marker button pair
        winner = which.min(pairs$score)[1]
        if (!is.na(winner)) {
          start = pairs$Var1[winner]
          end = pairs$Var2[winner]
          HASPT.algo = "markerbutton"
          return(invisible(list(SPTE_start = start, SPTE_end = end,
                                tib.threshold = 0,
                                part3_guider = HASPT.algo)))
        }
      }
    }
  }

  # internal functions ---------
  adjustlength = function(x, invalid) {
    if (length(invalid) > length(x)) {
      invalid = invalid[1:length(x)]
    } else if (length(invalid) < length(x)) {
      invalid = c(invalid, rep(0, length(x) - length(invalid)))
    }
    return(invalid)
  }
  rebuild_rle = function(rlex, N) {
    # after rle values have been changed
    # this function aids in rebuilding it
    y = rep(rlex$values, rlex$length)
    return(rle(y[1:N]))
  }
  # main code -----------------
  if (HASPT.algo != "notused") {
    if (HASPT.algo == "HDCZA") { # original, default
      # x = 5-min rolling median of abs differences
      # threshold = 10th percentile (constrained to 0.13-0.5 if required)
      medabsdi = function(angle) {
        #50th percentile, do not use mean because that will be outlier sensitive
        angvar = stats::median(abs(diff(angle)))
        return(angvar)
      }
      k1 = 5 * (60/ws3)
      
      x = zoo::rollapply(angle, width = k1, FUN = medabsdi, fill = 0) # 5 minute rolling median of the absolute difference
      if (is.null(params_sleep[["HDCZA_threshold"]])) {
        params_sleep[["HDCZA_threshold"]] = c(10, 15)
      }
      if (length(params_sleep[["HDCZA_threshold"]]) == 2) {
        threshold = quantile(x, probs = params_sleep[["HDCZA_threshold"]][1] / 100) * params_sleep[["HDCZA_threshold"]][2]
        if (threshold < 0.13) {
          threshold = 0.13
        } else if (threshold > 0.50) {
          threshold = 0.50
        }
      } else {
        threshold = params_sleep[["HDCZA_threshold"]]
      }
    } else if (HASPT.algo == "HorAngle") {  # if hip, then require horizontal angle
      # x = absolute angle
      # threshold = 45 degrees
      x = abs(angle)
      threshold = params_sleep[["HorAngle_threshold"]]
    } else if (HASPT.algo == "NotWorn") {
      # When protocol is to not wear sensor during the night,
      # and data is collected in count units we do not know angle
      # as needed for HorAngle and HDCZA.
      # Instead look for longest period of zero or very low intensity
      
      # First attempt:
      # However, we need to take into account that there may be some
      # noise in the data, so threshold needs to be above zero
      x = activity
      # smooth x to 5 minute rolling average to reduce sensitivity to sudden peaks
      ma <- function(x, n = 300 / ws3){stats::filter(x, rep(1 / n, n), sides = 2, circular = TRUE)}
      x = ma(x)
      nonzero = which(x != 0)
      if (length(nonzero) > 0) {
        activityThreshold = sd(x[nonzero], na.rm = TRUE) * 0.05
        # For sensewear external data this will not work as it mostly has values of 1 and up.
        if (activityThreshold < min(activity)) {
          activityThreshold = quantile(x, probs = 0.1)
        }
      } else {
        activityThreshold = 0
      }
      # this algorithm looked for x <= threshold, now a minimum quantity is added
      # to the threshold to allow for consistent definition of nomov below
      # i.e., x < threshold
      threshold = activityThreshold + 0.001
      # Always set HASPT.ignore.invalid to NA for HASPT.algo NotWorn
      # because NotWorn is by definition interested in invalid periods
      # and we definitely do not want to rely on imputed time series
      params_sleep[["HASPT.ignore.invalid"]] = NA
    } else if (HASPT.algo == "MotionWare") {
      
      # Auto-Sleep Detection / Marking based on description in
      # Information bulletin no.3 sleep algorithms by Cambridge Neurotechnologies
      # Section 1.6. The letters A, B, C, D, and E below refer to sections in 
      # this description.
      
      # parameters that GGIR user may want to change
      # keep hardcoded for now while this is still experimental
      MarkerButtonLimit = 3
      minimum_sleep_fraction = 0.6
      Prescan_Sleep_Fraction = 0.8
      # (A - classify sleep/wake)
      if (ws3 == 15) {
        score_threshold = 1.5
      } else if (ws3 == 30) {
        score_threshold = 3
      } else if (ws3 == 60) {
        score_threshold = 6
      }
      sleep_wake_score = rep(0, length(activity))
      sleep_wake_score[which(activity <= score_threshold)] = 1
      
      # (B - ignore nonwear)
      # Ignore all detected nonwear
      sleep_wake_score[which(invalid == 1)] = 0 
      
      # Only do analysis if at least 1 hour of sleep and wake in the data
      # and less than 33% of the data points is invalid
      Npoints = length(sleep_wake_score) 
      start_log = end_log = NULL
      # initialise output
      start = 0
      end = 0 #length(sibs)
      tib.threshold = 0
      if (length(which(invalid == 1)) / Npoints <= 0.33 &&
          length(which(sleep_wake_score == 1)) > 60 * (60/ws3) &&
          length(which(sleep_wake_score == 0)) > 60 * (60/ws3)) {
        # (F while loop to repeat steps C, D, E to find all sleep periods) 
        start_point = 1
        while (start_point < Npoints - 180 * (60/ws3)) { 
          # (C - find midpoint)
          find_midpoint = function(cnt, sleep_wake_score, ws3) {
            midpoint = NULL
            avscore = 0
            while (avscore < Prescan_Sleep_Fraction) { #0.8
              j0 = cnt
              j1 = cnt + ((60/ws3) * 180)
              if (j1 > Npoints) break
              avscore = mean(sleep_wake_score[j0:j1])
              cnt = cnt + 1
            }
            if (avscore >= Prescan_Sleep_Fraction) {
              while (avscore > Prescan_Sleep_Fraction - 0.02) {
                j1 = j1 + 1
                if (j1 > Npoints) break
                avscore = mean(sleep_wake_score[j0:j1])
              }
              midpoint = round((j1 - j0) / 2) + j0
            } else {
              j0 = j1 = midpoint = NULL
            }
            invisible(list(j0 = j0, j1 = j1, midpoint = midpoint))
          }
          # use j0 and j1 as estimate for start and end for now
          fmout = find_midpoint(cnt = start_point, sleep_wake_score, ws3)
          
          start = fmout$j0
          end = fmout$j1
          midpoint = fmout$midpoint
          if (is.null(midpoint)) break
          # (D - extend)
          find_edge = function(x, where, midpoint, minimum_sleep_fraction) {
            if (where == "before") {
              xrle = rle(rev(x[1:midpoint]))
            } else if (where == "after") {
              xrle = rle(x[midpoint:length(x)])
            }
            log_len = 0

            if (xrle$values[1] == 0) {
              # If midpoint equals wake, then always include that segment              
              log_len = log_len + xrle$lengths[1]
              xrle$values = xrle$values[-1]
              xrle$lengths = xrle$lengths[-1]
            }
            if (length(xrle$values) > 0) {
              if (xrle$values[1] == 1) {
                # Always include first sleep segment
                log_len = log_len + xrle$lengths[1]
                xrle$values = xrle$values[-1]
                xrle$lengths = xrle$lengths[-1]
              }
              Nsegments = length(xrle$lengths)
              Nout = floor(Nsegments / 2)
              if (Nsegments > 1) {
                # Explore which extension is possible
                wakesegs = cumsum(xrle$lengths[seq(1, by = 2, length.out = Nout)])
                sleepsegs = cumsum(xrle$lengths[seq(2, by = 2, length.out = Nout)])
                extension_duration = sleepsegs + wakesegs
                fractions = sleepsegs / (sleepsegs + wakesegs)
                # ignore (keep) extensions that do not meet sleep fraction
                too_little_sleep = which(fractions < minimum_sleep_fraction)
                if (length(too_little_sleep) > 0) {
                  last_segment = (too_little_sleep[1] - 1) * 2
                } else {
                  last_segment = length(xrle$lengths)
                }
                if (last_segment > 0 && !is.na(last_segment)) {
                  log_len = log_len + sum(xrle$lengths[1:last_segment])
                }
              }
            }
            if (where == "before") {
              edge = midpoint - log_len
            } else if (where == "after") {
              edge = midpoint + log_len
            }
            return(edge)
          }
          start = find_edge(sleep_wake_score, where = "before", midpoint, minimum_sleep_fraction)
          end = find_edge(sleep_wake_score, where = "after", midpoint, minimum_sleep_fraction)
          # (E)
          # ignore marker button if while loop is stuck
          do_no_use_marker = FALSE
          if (length(start_log) > 2 && 
              start_log[length(start_log)] == start_log[length(start_log) - 1]) {
            do_no_use_marker = TRUE
          }
          # is marker button data present?
          if (length(marker) > 0 && do_no_use_marker == FALSE) {
            marker_indices = which(marker == 1)
            if (length(marker_indices) > 1) {
              # check for nearby marker buttons and use those
              delta_start = abs(start - marker_indices)
              ds2 = which(delta_start < (60/ws3) * 60 * MarkerButtonLimit)
              if (length(ds2) > 0) {
                start = rev(marker_indices[ds2])[1] # use last
              }
              delta_end = abs(end - marker_indices)
              de2 = which(delta_end < (60/ws3) * 60 * MarkerButtonLimit)
              if (length(de2) > 0) {
                end = rev(marker_indices[de2])[1] # use last
              }
            }
          }
          start_log = c(start_log, start)
          end_log = c(end_log, end)
          start_point = end + (60/ws3) * 60
        }
        dur_log = end_log - start_log + 1
        # Keep longest sleep periods, because at the moment GGIR cannot handle multiple sleep periods
        start = start_log[which.max(dur_log)[1]]
        end = end_log[which.max(dur_log)[1]]
      }
      # exit function with the result
      return(invisible(list(SPTE_start = start, SPTE_end = end, tib.threshold = 0,
                            part3_guider = HASPT.algo)))
    } else if (HASPT.algo == "HLRB") {
      start = end = NULL
      # smooth classification
      sibs = round(zoo::rollmean(x = sibs, k = (60/ws3) * 60, fill = 0))
      MaxSleepGap = 1
      # ignore all X hour gaps
      srle = rle(c(0, as.numeric(sibs), 0))
      wakegaps = which(srle$values == 0 & srle$lengths < (60/ws3) * 60 * 1)
      wakegaps = wakegaps[which(wakegaps %in% c(1, length(srle$values)) == FALSE)]
      if (length(wakegaps) > 0) {
        srle$values[wakegaps] = 1
      }
      sibs = rep(srle$values, srle$lengths)
      sibs = sibs[-c(1, length(sibs))]
      # keep longest sleep period
      srle = rle(c(0, as.numeric(sibs), 0))
      if (length(srle$lengths) > 2) {
        size_longest_sleep = max(srle$lengths[which(srle$values == 1)])
        sleepgaps = which(srle$values == 1 & srle$lengths < size_longest_sleep)
        if (length(sleepgaps) > 0) {
          srle$values[sleepgaps] = 0
        }
      }
      sibs = c(0, rep(srle$values, srle$lengths), 0)
      start = which(diff(sibs) == 1)
      end = which(diff(sibs) == -1)
      # If there are multiple windows with equal length take last one
      if (length(start) > 1 & length(end) > 1) {
        start = start[length(end)]
        end = end[length(end)]
      }
      sibs = sibs[-c(1, length(sibs))]
      # If start and end are not found try again
      # with shorter filter window
      if (length(start) == 0 & length(end) == 0) {
        start = 1
        end = length(sibs)
      }
      # exit function with the result
      return(invisible(list(SPTE_start = start, SPTE_end = end, tib.threshold = 0,
                            part3_guider = HASPT.algo)))
    }

    # Now define nomov periods with the selected strategy for invalid time
    nomov = rep(0,length(x)) # no movement
    invalid = adjustlength(x, invalid)
    if (is.na(params_sleep[["HASPT.ignore.invalid"]])) { # all invalid = no movement
      nomov[which(x < threshold | invalid == 1)] = 1
    } else if (params_sleep[["HASPT.ignore.invalid"]] == FALSE) { # calculate no movement over the imputed angle
      nomov[which(x < threshold)] = 1
    } else if (params_sleep[["HASPT.ignore.invalid"]] == TRUE) {  # all invalid = movement
      nomov[which(x < threshold & invalid == 0)] = 1
    }
    
    # initialise output
    SPTE_end = c()
    SPTE_start = c()
    tib.threshold = c()
    part3_guider = "none"
    #------------------------------------------------------
    # apply final 3 steps to estimate the main SPT window
    # these steps are the same for HDCZA, HorAngle, and NotWorn
    N = length(x)
    spt_estimate = rep(NA, N)
    nomov = c(0, nomov, 0)
    rle_nomov = rle(nomov)
    # only keep the blocks that are long enough
    fraction_night_invalid = sum(invalid) / length(invalid)
    if (fraction_night_invalid < 1) {
      # Step -3: ignore blocks that are too short
      blocks_to_remove = which(rle_nomov$values == 1 & rle_nomov$lengths <= (60 / ws3) * params_sleep[["spt_min_block_dur"]])    
      blocks_to_remove = blocks_to_remove[which(blocks_to_remove %in% c(1, length(rle_nomov$values)) == FALSE)]
      if (length(blocks_to_remove) > 0) {
        rle_nomov$values[blocks_to_remove] = 0
        rle_nomov = rebuild_rle(rle_nomov, N)
      }
      # Step -2: fill gaps that are short
      Nsegments = length(rle_nomov$lengths)
      if (!is.null(params_sleep[["spt_max_gap_ratio"]]) && params_sleep[["spt_max_gap_ratio"]] < 1 && Nsegments > 3) {
        # Note: spt_max_gap_ratio is NULL by default
        gap_ratios = data.frame(values = rle_nomov$values, lengths = rle_nomov$lengths)
        gap_ratios$ratio = gap_ratios$length_after = gap_ratios$length_before = 0
        gap_ratios$length_after[1:(Nsegments - 1)] = gap_ratios$lengths[2:Nsegments]
        gap_ratios$length_before[2:Nsegments] = gap_ratios$lengths[1:(Nsegments - 1)]
        gaps_to_fill = which(gap_ratios$values == 0 &
                               gap_ratios$lengths < (60 / ws3) * params_sleep[["spt_max_gap_dur"]] &
                               gap_ratios$lengths / gap_ratios$length_after < params_sleep[["spt_max_gap_ratio"]] &
                               gap_ratios$lengths / gap_ratios$length_before < params_sleep[["spt_max_gap_ratio"]])
      } else {
        gaps_to_fill = which(rle_nomov$values == 0 & rle_nomov$lengths < (60 / ws3) * params_sleep[["spt_max_gap_dur"]])
      }
      if (length(gaps_to_fill) > 0) {
        gaps_to_fill = gaps_to_fill[which(gaps_to_fill %in% c(1, length(rle_nomov$values)) == FALSE)]
      }
      if (length(gaps_to_fill) > 0) {
        rle_nomov$values[gaps_to_fill] = 1
        rle_nomov = rebuild_rle(rle_nomov, N)
      }
      # Retain estimate before the selecting the longest block
      spt_crude_estimate = rep(rle_nomov$values, rle_nomov$length)
      # Step -1: keep indices for longest spt block
      if (1 %in% rle_nomov$values) {
        max_length =  max(rle_nomov$length[which(rle_nomov$values == 1)])
        rle_nomov$values[which(rle_nomov$values == 1 & rle_nomov$length == max_length)[1]] = 2
        rle_nomov$values[which(rle_nomov$values != 2)] = 0
        rle_nomov$values[which(rle_nomov$values == 2)] = 1
      }
      spt_estimate = rep(rle_nomov$values, rle_nomov$length)
      spt_estimate = spt_estimate[1:length(x)]
      # identify start and end of longest block
      SPTE_start = which(diff(c(0, spt_estimate, 0)) == 1) - 1
      SPTE_end = which(diff(c(0, spt_estimate, 0)) == -1) - 1
      if (length(SPTE_start) == 1 && length(SPTE_end) == 1 && SPTE_start == 0) SPTE_start = 1
      if (length(SPTE_start) > 0 & length(SPTE_end) > 0) {
        spt_crude_estimate[SPTE_start:SPTE_end] = 2 # clarify in crude estimate what final estimate is
      }
      spt_crude_estimate = spt_crude_estimate[1:max(c(length(angle), length(activity), length(sibs)))]
      part3_guider = HASPT.algo
      if (is.na(params_sleep[["HASPT.ignore.invalid"]])) {
        # investigate if invalid time was included in the SPT definition,
        # and if so, keep track of that in the guider. This is needed in the
        # case that sleeplog is used, to inform part 4 that it should
        # trust the sleeplog times for this specific night.
        spt_long = rep(0, length(invalid))
        spt_long[SPTE_start:SPTE_end] = 1
        invalid_in_spt = which(invalid == 1 & spt_long == 1)
        if (length(invalid_in_spt)) {
          part3_guider = paste0(HASPT.algo, "+invalid")
        }
      }
    }
    tib.threshold = threshold
  }
  invisible(list(SPTE_start = SPTE_start, SPTE_end = SPTE_end, tib.threshold = tib.threshold,
                 part3_guider = part3_guider, spt_crude_estimate = spt_crude_estimate))
}
