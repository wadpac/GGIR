MXLX = function(Y = NULL, X = 5, epochSize = 1, tseg = c(0, 24), resolutionMin = 10) {
  # Y 24 hours of data
  # tseg is vector of length 2 with the start and end time as hour in the day
  # X is the window size in hours to look for
  
  do.MXLX = mean(Y, na.rm = TRUE) > 0
  Nwindows = (tseg[2] - X) - tseg[1]
  if (length(do.MXLX) == 0 ||
      is.na(do.MXLX) == TRUE ||
      Nwindows < 1 ||
      length(Y) < length(X) * (3600 / epochSize)) {
    do.MXLX = FALSE
  }
  
  nStepsPerHour = 60 / resolutionMin # number of steps per hour
  nEpochsPerStep = (resolutionMin * 60) / epochSize # number of epochs per step
  if (do.MXLX == TRUE) { # only do the analysis if Y has values other than zero
    Y = Y[((((tseg[1] - tseg[1]) * 3600)/epochSize) + 1):(((tseg[2] - tseg[1]) * 3600)/epochSize)]
    Nwindows = Nwindows * nStepsPerHour
    rollingMean = matrix(NA, Nwindows, 1)
    for (hri in 1:Nwindows) { #e.g.9am-9pm
      # start and end in terms of steps
      indexStart = hri - 1 #e.g. 9am
      indexEnd = indexStart + (X * nStepsPerHour) #e.g. 9am + 5 hrs
      # start and end in terms of Y index
      indexStart = (indexStart * nEpochsPerStep) + 1
      indexEnd = indexEnd * nEpochsPerStep
      einclude = indexStart:indexEnd
      if (indexEnd <= length(Y)) {
        rollingMean[hri,1] = mean(Y[einclude])
      }
    }
    valid = which(is.na(rollingMean) == F)
    LXvalue = min(rollingMean[valid], na.rm = T)
    MXvalue = max(rollingMean[valid], na.rm = T)
    LXhr = ((which(rollingMean == LXvalue & is.na(rollingMean) == F) - 1) / nStepsPerHour) + tseg[1]
    MXhr = ((which(rollingMean == MXvalue & is.na(rollingMean) == F) - 1) / nStepsPerHour) + tseg[1]
    
    #-------------------------------------
    if (length(LXvalue) > 1) { LXvalue = sort(LXvalue)[ceiling(length(LXvalue)/2)] }
    if (length(LXhr) > 1) { LXhr = sort(LXhr)[ceiling(length(LXhr)/2)] }
    if (length(MXvalue) > 1) { MXvalue = sort(MXvalue)[ceiling(length(MXvalue)/2)] }
    if (length(MXhr) > 1) { MXhr = sort(MXhr)[ceiling(length(MXhr)/2)] }
    
    #Note it is + 1 because first epoch is one and not zero:
    LXstart = ((LXhr - tseg[1]) * (3600 / epochSize)) + 1
    LXend = LXstart + (X * (3600 / epochSize)) - 1
    MXstart = ((MXhr - tseg[1]) * (3600 / epochSize)) + 1 
    MXend = MXstart + (X * (3600 / epochSize)) - 1
    
    MXLX = data.frame(LXhr = LXhr[1],
                      LXvalue = LXvalue,
                      LXindex0 = LXstart,
                      LXindex1 = LXend,
                      MXhr = MXhr[1],
                      MXvalue = MXvalue,
                      MXindex0 = MXstart,
                      MXindex1 = MXend, stringsAsFactors = TRUE)
  } else {
    MXLX = data.frame(LXhr = NA, LXvalue = NA, 
                      LXindex0 = NA,
                      LXindex1 = NA,
                      MXhr = NA, MXvalue = NA,
                      MXindex0 = NA,
                      MXindex1 = NA, stringsAsFactors = TRUE)
  }
  MXLXnames = c(paste0("L", X, "hr"), paste0("L", X), paste0("start_L", X),  paste0("end_L", X),
                paste0("M", X, "hr"), paste0("M", X),  paste0("start_M", X),  paste0("end_M", X))
  names(MXLX) = MXLXnames
  return(MXLX)
}