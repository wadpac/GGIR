HASPT = function(angle, sptblocksize = 30, spt_max_gap = 60, ws3 = 5,
                 HASPT.algo="HDCZA", HDCZA_threshold = c(), invalid,
                 HASPT.ignore.invalid=FALSE, activity = NULL) {
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
  # handle_spt_gaps = function(s2, e2, s3, e3, 
  #                            spt_max_gap_epochs) {
  #   outofsptblock = integer()
  #   if (length(s3) > 0) { # there are at least 1 gap between candidate guider windows
  #     gaps = matrix(0, length(s3), 3) #characteristics of all gaps in between guider periods
  #     for (gapi in 1:length(s3)) {
  #       gaps[gapi,1] = abs(e2[gapi] - s2[gapi]) # length of guider preceding gap
  #       gaps[gapi,2] = abs(e3[gapi] - s3[gapi]) # length of gap
  #       gaps[gapi,3] = abs(e2[gapi + 1] - s2[gapi + 1]) # length of guider following gap
  #       if (gaps[gapi, 2] < spt_max_gap_epochs &
  #           (gaps[gapi, 2] / (gaps[gapi, 1] + gaps[gapi, 3])) < 0.3) {
  #         # if gap is < than spt_max_gap (60 min)
  #         # & gap < 30% of surrounding candidate spt windows
  #         # then fill gap between candidate spt blocks
  #         outofsptblock = c(outofsptblock, gapi)
  #       }
  #     }
  #   }
  #   invisible(outofsptblock)
  # }
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
      # spt_max_gap_epochs = (60/ws3)*spt_max_gap*1
      # outofsptblock = handle_spt_gaps(s2, e2, s3, e3, 
      #                                 spt_max_gap_epochs)
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
