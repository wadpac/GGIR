g.getM5L5 = function(varnum, epochSize, t0_LFMF, t1_LFMF, M5L5res, winhr, 
                     qM5L5  = c(), iglevels = c(), MX.ig.min.dur = 10,
                     UnitReScale = 1000) {
  #diurnal pattern features extracted only meaningful if more than 16 hours
  meanVarnum = mean(varnum)
  do.M5L5 = meanVarnum > 0 # & length(varnum) > 1440*(60/ws3)
  nwindow_f = (t1_LFMF-winhr) - t0_LFMF #number of windows for L5M5 analyses
  if (length(do.M5L5) == 0 | is.na(do.M5L5) == TRUE | nwindow_f < 1) do.M5L5 = FALSE
  
  if (do.M5L5 == TRUE) { # only do the analysis if varnum has values other than zero
    reso = M5L5res #resolution
    nwindow_f = nwindow_f * (60/reso)
    DAYrunav5 = matrix(NA,nwindow_f,1)
    first_hri = (t0_LFMF*(60/reso))
    last_hri = min(nrow(DAYrunav5), floor(((t1_LFMF - winhr) * (60/reso)) - 1))
    for (hri in first_hri:last_hri) { #e.g.9am-9pm
      e1 = (hri * reso * (60/epochSize)) + 1 #e.g. 9am
      e2 = (hri + (winhr * (60/reso))) * reso * (60/epochSize) #e.g. 9am + 5 hrs
      if (e2 <= length(varnum)) {
        einclude = e1:e2
      } else { #allow for analyses beyond end of day 
        einclude = c(1:(e2 - length(varnum)), e1:length(varnum))
      }
      DAYrunav5[((hri - (t0_LFMF * (60/reso))) + 1),1] = mean(varnum[einclude])
    }
    valid = which(is.na(DAYrunav5) == F)
    DAYL5HOUR = ((which(DAYrunav5 == min(DAYrunav5[valid], na.rm = T) &
                          is.na(DAYrunav5) == F) - 1)/(60/reso)) + t0_LFMF #- 1
    DAYL5VALUE = min(DAYrunav5[valid]) * UnitReScale
    DAYM5HOUR = ((which(DAYrunav5 == max(DAYrunav5[valid], na.rm = T) & is.na(DAYrunav5) == F) - 1)/(60/reso)) + t0_LFMF #- 1
    DAYM5VALUE = max(DAYrunav5[valid]) * UnitReScale

    #-------------------------------------
    if (length(DAYL5VALUE) > 1) { DAYL5VALUE = sort(DAYL5VALUE)[ceiling(length(DAYL5VALUE)/2)] }
    if (length(DAYL5HOUR) > 1) { DAYL5HOUR = sort(DAYL5HOUR)[ceiling(length(DAYL5HOUR)/2)] }
    if (length(DAYM5VALUE) > 1) { DAYM5VALUE = sort(DAYM5VALUE)[ceiling(length(DAYM5VALUE)/2)] }
    if (length(DAYM5HOUR) > 1) { DAYM5HOUR = sort(DAYM5HOUR)[ceiling(length(DAYM5HOUR)/2)] }
    M5L5vars = data.frame(DAYL5HOUR = DAYL5HOUR[1],
                          DAYL5VALUE = DAYL5VALUE, DAYM5HOUR = DAYM5HOUR[1],
                          DAYM5VALUE = DAYM5VALUE, stringsAsFactors = TRUE)
  } else {
    M5L5vars = data.frame(DAYL5HOUR = NA, DAYL5VALUE = NA, 
                          DAYM5HOUR = NA, DAYM5VALUE = NA, stringsAsFactors = TRUE)
  }
  ML5N = c(paste0("L", winhr,"hr"), paste0("L", winhr),
           paste0("M", winhr, "hr"), paste0("M", winhr))
  names(M5L5vars) = ML5N
  if ((length(iglevels) > 0 || length(qM5L5 ) > 0) && do.M5L5 == TRUE) { #
    # get indices of window
    L5start = (DAYL5HOUR * (3600 / epochSize)) + 1 # (hour * (number of epochSize epochs in an hour)) + 1 because first epoch is one and not zero
    L5end = L5start + (winhr * (3600 / epochSize))
    M5start = (DAYM5HOUR * (3600 / epochSize)) + 1 # (hour * (number of epochSize epochs in an hour)) + 1 because first epoch is one and not zero
    M5end = M5start + (winhr * (3600 / epochSize))
  }
  # added 24 hours to create continuous scale to achieve more meaningful aggregation at person level
  if (do.M5L5 == TRUE) {
    if (!is.null(M5L5vars[1]) &&
        is.na(M5L5vars[1]) == FALSE &&
        M5L5vars[1] < 12) {
      M5L5vars[1] = M5L5vars[1] + 24 
    }
  }
  #-----------------------
  if (length(iglevels) > 0 && length(MX.ig.min.dur) == 1 && do.M5L5 == TRUE) { # intensity gradient (as described by Alex Rowlands 2018)
    if (winhr >= MX.ig.min.dur) {
      for (li in 1:2) { # do twice, once for LX and once for MX
        q49 = c()
        if (li == 1) {
          q50 = cut(varnum[L5start:L5end]*UnitReScale, breaks = iglevels, right = FALSE)
        } else {
          q50 = cut(varnum[M5start:M5end]*UnitReScale, breaks = iglevels, right = FALSE)
        }
        q50 = table(q50)
        q49  = (as.numeric(q50) * epochSize)/60 #converting to minutes
        x_ig = zoo::rollmean(iglevels, k = 2)
        y_ig = q49
        igout = g.intensitygradient(x_ig, y_ig) # produces three values
        varnameig = paste0(paste0(ifelse(li == 1, yes = "L", no = "M"), winhr, "_"),
                           c("ig_gradient", "ig_intercept", "ig_rsquared"))
        if (length(varnum) > 0) {
          M5L5vars = as.data.frame(c(M5L5vars, as.vector(unlist(igout))), stringsAsFactors = TRUE)
        } else {
          M5L5vars = as.data.frame(c(M5L5vars, rep("", 3)), stringsAsFactors = TRUE)
        }
        names(M5L5vars) = c(ML5N, varnameig)
      }
    }
  }
  #-----------------------
  if (length(qM5L5 ) > 0) {
    if (do.M5L5 == TRUE) {
      # calculate statistics on acceleration metrics
      L5q = quantile(varnum[L5start:L5end], probs = qM5L5 , na.rm = TRUE)
      M5q = quantile(varnum[M5start:M5end], probs = qM5L5 , na.rm = TRUE)
      M5L5varsExtra = as.numeric(c(L5q, M5q)) * UnitReScale
    } else {
      M5L5varsExtra = rep(NA, length(qM5L5) * 2)
    }
    M5L5varsExtraNames = c(paste0("L", winhr, "_q", round(qM5L5 * 100)),
                           paste0("M", winhr, "_q", round(qM5L5 * 100)))
    # add to M5L5vars data.frame
    M5L5vars = as.data.frame(c(M5L5vars, M5L5varsExtra), stringsAsFactors = TRUE)
    names(M5L5vars) = c(ML5N, M5L5varsExtraNames)
  }
  return(M5L5vars)
}