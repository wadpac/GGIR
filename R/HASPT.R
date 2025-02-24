HASPT = function(angle, sptblocksize = 30, spt_max_gap = 60, ws3 = 5,
                 HASPT.algo = "HDCZA", HDCZA_threshold = c(), invalid,
                 HASPT.ignore.invalid = FALSE, activity = NULL,
                 marker = NULL,
                 sibs = NULL) {
  tib.threshold = SPTE_start = SPTE_end = part3_guider = c()
  # internal functions ---------
  adjustlength = function(x, invalid) {
    if (length(invalid) > length(x)) {
      invalid = invalid[1:length(x)]
    } else if (length(invalid) < length(x)) {
      invalid = c(invalid, rep(0, length(x) - length(invalid)))
    }
    return(invalid)
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
      if (is.null(HDCZA_threshold)) {
        HDCZA_threshold = c(10, 15)
      }
      if (length(HDCZA_threshold) == 2) {
        threshold = quantile(x, probs = HDCZA_threshold[1] / 100) * HDCZA_threshold[2]
        if (threshold < 0.13) {
          threshold = 0.13
        } else if (threshold > 0.50) {
          threshold = 0.50
        }
      } else {
        threshold = HDCZA_threshold
      }
    } else if (HASPT.algo == "HorAngle") {  # if hip, then require horizontal angle
      # x = absolute angle
      # threshold = 45 degrees
      x = abs(angle)
      threshold = 60
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
      HASPT.ignore.invalid = NA
    } else if (HASPT.algo == "MotionWare") {
      
      # Auto-Sleep Detection / Marking based on description in
      # Information bulletin no.3 sleep algorithms by Cambrige Neurotechnologies
      
      # parameters that user may want to change
      
      minimum_sleep_fraction = 0.6
      
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
      if (length(which(invalid == 1)) / Npoints <= 0.33 &&
          length(which(sleep_wake_score == 1)) > 60 * (60/ws3) &&
          length(which(sleep_wake_score == 0)) > 60 * (60/ws3)) {
        # (F while loop to repeat steps C, D, E to find all sleep periods) 
        start_point = 1
        while (start_point < Npoints - 180 * (60/ws3)) { 
          # (C - find midpoint)
          find_midpoint = function(cnt, sleep_wake_score, ws3) {
            midpoint = NULL
            avscore = 1
            while (avscore > 0.2) {
              j0 = cnt
              j1 = cnt + ((60/ws3) * 180)
              if (j1 > Npoints) break
              avscore = 1 - mean(sleep_wake_score[j0:j1])
              cnt = cnt + 1
            }
            while (avscore < 0.22) {
              j1 = j1 + 1
              if (j1 > Npoints) break
              avscore = 1 - mean(sleep_wake_score[j0:j1])
            }
            midpoint = round((j1 - j0) / 2) + j0
            invisible(list(j0 = j0, j1 = j1, midpoint = midpoint))
          }
          # use j0 and j1 as estimate for start and end for now
          fmout = find_midpoint(cnt = start_point, sleep_wake_score, ws3)
          
          start = fmout$j0
          end = fmout$j1
          midpoint = fmout$midpoint
          
          # (D - extend)
          find_edge = function(x, where, midpoint, minimum_sleep_fraction) {
            if (where == "before") {
              xrle = rle(rev(x[1:midpoint]))
            } else if (where == "after") {
              xrle = rle(x[midpoint:length(x)])
            }
            log_len = 0
            if (xrle$values[1] == 0) {
              log_len = log_len + xrle$lengths[1]
              xrle$values = xrle$values[-1]
              xrle$lengths = xrle$lengths[-1]
            }
            if (length(xrle$values) > 0) {
              if (xrle$values[1] == 1) {
                log_len = log_len + xrle$lengths[1]
                xrle$values = xrle$values[-1]
                xrle$lengths = xrle$lengths[-1]
              }
              Nsegments = length(xrle$lengths)
              Nout = floor(Nsegments / 2)
              if (Nsegments > 1) {
                wakesegs = xrle$lengths[seq(1, by = 2, length.out = Nout)]
                sleepsegs = xrle$lengths[seq(2, by = 2, length.out = Nout)]
                fractions = sleepsegs / (sleepsegs + wakesegs)
                too_little_sleep = which(fractions < minimum_sleep_fraction)[1] - 1
                if (length(too_little_sleep) == 1 && !is.na(too_little_sleep) &&
                    too_little_sleep > 0) {
                  log_len = log_len + sum(xrle$lengths[1:too_little_sleep])
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
      } else {
        start = 1
        end = length(sibs)
        tib.threshold = 0
      }
      # exit function with the result
      return(invisible(list(SPTE_start = start, SPTE_end = end, tib.threshold = 0,
                            part3_guider = HASPT.algo)))
    } else if (HASPT.algo == "vanHees2025") {
      MarkerButtonLimit = 3
      MaxSleepGap = 2
      # ignore all wake  gaps < 120 minutes
      srle = rle(c(as.numeric(sibs), 0))
      wakegaps = which(srle$values == 0 & srle$lengths < (60/ws3) * 60 * MaxSleepGap)
      if (length(wakegaps) > 0) {
        srle$values[wakegaps] = 1      }
      sibs = rep(srle$values, srle$lengths)
      
      # keep longest sleep period
      srle = rle(as.numeric(sibs))
      if (length(srle$lengths) > 2) {
        sleepgaps = which(srle$values == 1 & srle$lengths < max(srle$lengths))
        
        if (length(sleepgaps) > 0) {
          srle$values[sleepgaps] = 0
        }
      }
      sibs = rep(srle$values, srle$lengths)
      # find start and end
      start = which(diff(sibs) == 1)
      end = which(diff(sibs) == -1)
      # is marker button data present?
      if (length(marker) > 0) {
        marker_indices = which(marker == 1)
        if (length(marker_indices) > 1) {
          # check for nearby marker buttons and use those 
          delta_start = abs(start - marker_indices)
          ds2 = which(delta_start < (60/ws3) * 60 * MarkerButtonLimit)
          if (length(ds2) > 0) {
            delta_start = delta_start[ds2]
            marker_indices2 = marker_indices[ds2]
            start = marker_indices2[which.min(delta_start)[1]]
          }
          delta_end = abs(end - marker_indices)
          de2 = which(delta_end < (60/ws3) * 60 * MarkerButtonLimit)
          if (length(de2) > 0) {
            delta_end = delta_end[de2]
            marker_indices2 = marker_indices[de2]
            end = marker_indices2[which.min(delta_end)[1]]
          }
        }
      }
      # exit function with the result
      return(invisible(list(SPTE_start = start, SPTE_end = end, tib.threshold = 0,
                     part3_guider = HASPT.algo)))
    }

    # Now define nomov periods with the selected strategy for invalid time
    nomov = rep(0,length(x)) # no movement
    invalid = adjustlength(x, invalid)
    if (is.na(HASPT.ignore.invalid)) { # all invalid = no movement
      nomov[which(x < threshold | invalid == 1)] = 1
    } else if (HASPT.ignore.invalid == FALSE) { # calculate no movement over the imputed angle
      nomov[which(x < threshold)] = 1
    } else if (HASPT.ignore.invalid == TRUE) {  # all invalid = movement
      nomov[which(x < threshold & invalid == 0)] = 1
    }
    # apply steps (assumptions on sleep)
    inspttime = rep(NA,length(x))
    nomov = c(0,nomov,0)
    s1 = which(diff(nomov) == 1) #start of blocks in spt
    e1 = which(diff(nomov) == -1) #end of blocks in spt
    sptblock = which((e1 - s1) > ((60/ws3)*sptblocksize*1)) #which are the blocks longer than sptblocksize in minutes?
    fraction_night_invalid = sum(invalid) / length(invalid)
    if (length(sptblock) > 0 & fraction_night_invalid < 1) { #
      s2 = s1[sptblock] # only keep the sptblocks that are long enough
      e2 = e1[sptblock] # only keep the sptblocks that are long enough
      for (j in 1:length(s2)) {
        inspttime[s2[j]:e2[j]] = 1 #record these blocks in the inspttime vector
      }
      # fill up gaps in time between spt blocks
      outofspt = rep(0,length(inspttime))
      outofspt[which(is.na(inspttime) == TRUE)] = 1
      outofspt = c(0,outofspt,0)
      s3 = which(diff(outofspt) == 1) #start of blocks out of spt?
      e3 = which(diff(outofspt) == -1) #end blocks out of spt?
      # starting block not to be filled
      if (length(s3) > 0) {
        if (s3[1] == 1) {
          s3 = s3[-1]
          e3 = e3[-1]
        }
      }
      if (length(e3) > 0) {
        if (e3[length(e3)] > length(x)) {
          # ending block not to be filled
          s3 = s3[-length(s3)]
          e3 = e3[-length(e3)]
        }
      }
      outofsptblock = which((e3 - s3) < ((60/ws3)*spt_max_gap*1))
      if (length(outofsptblock) > 0) { # only fill up gap if there are gaps
        s4 = s3[outofsptblock]
        e4 = e3[outofsptblock]
        if (length(s4) > 0) {
          for (j in 1:length(s4)) {
            inspttime[ s4[j]:e4[j]] = 1
          }
        }
      }
      if (length(inspttime) == (length(x) + 1)) inspttime = inspttime[1:(length(inspttime) - 1)]
      # keep indices for longest in spt block:
      inspttime2 = rep(1,length(inspttime))
      inspttime2[which(is.na(inspttime) == TRUE)] = 0
      s5 = which(diff(c(0,inspttime2,0)) == 1) #start of blocks out of spt
      e5 = which(diff(c(0,inspttime2,0)) == -1) #end of blocks out of spt
      insptdurations = e5 - s5
      longestinspt = which(insptdurations == max(insptdurations))
      if (length(longestinspt) > 1) longestinspt = longestinspt[ceiling(length(longestinspt)/2)]
      SPTE_start = s5[longestinspt] - 1
      SPTE_end = e5[longestinspt] - 1
      if (SPTE_start == 0) SPTE_start = 1
      part3_guider = HASPT.algo
      if (is.na(HASPT.ignore.invalid)) {
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
      
      # # Code to help investigate classifications:
      # plot(x, col = "black", type = "l")
      # abline(v = SPTE_start, col = "green", lwd = 2)
      # abline(v = SPTE_end, col = "red", lwd = 2)
      # rect(xleft = s1, ybottom = rep(0, length(s1)),
      #      xright = e1, ytop = rep(0.1, length(s1)),
      #      col = rgb(0, 0, 255, max = 255, alpha = 50), border = NA)
      # 
      # rect(xleft = s5, ybottom = rep(0.1, length(s1)),
      #      xright = e5, ytop = rep(1, length(s1)),
      #      col = rgb(255, 0, 0, max = 255, alpha = 20), border = NA)
      # lines(x, col = "black", type = "l")
      # abline(h = threshold, col = "purple", lwd = 2)
      # inva = which(invalid == 1)
      # if (length(inva) > 0) {
      #   lines(inva, rep(0.1, length(inva)),
      #         type = "p", pch = 20, lwd = 4, col = "black")
      # }
      # lines(invalid* 0.05, type = "l", col = "red")
      # # graphics.off()
      # browser()
      
    } else {
      SPTE_end = c()
      SPTE_start = c()
      tib.threshold = c()
      part3_guider = "none"
    }
    tib.threshold = threshold
  }
  invisible(list(SPTE_start = SPTE_start, SPTE_end = SPTE_end, tib.threshold = tib.threshold,
                 part3_guider = part3_guider))
}
