cosinorAnalyses = function(Xi, epochsize = 60, timeOffsetHours = 0) {
  if (epochsize < 60) {
    # If epochsize < 1 minute then aggregate to 1 minute
    # but keep NA values
    XTtime = rep(1:length(Xi), each = 60 / epochsize)
    XT = data.frame(Xi = Xi, time = XTtime[1:length(Xi)])
    XT = aggregate(x = XT, by = list(XT$time), FUN = mean, na.rm = TRUE)
    if (length(which(is.nan(XT$Xi) == TRUE)) > 0) {
      is.na(XT$Xi[which(is.nan(XT$Xi) == TRUE)]) = TRUE
    }
    Xi = XT$Xi
    epochsizesecondsXi = 60
  } else {
    epochsizesecondsXi = epochsize
  }
  # Apply Extended Cosinor function from ActRC
  N = 1440 * (60 / epochsizesecondsXi) # Number of epochs per day
  Xi = Xi[1:(N * floor(length(Xi) / N))] # ActCR expects integer number of days
  coef = ActCR::ActCosinor(x = Xi, window = 1440 / N)
  coefext = ActCR::ActExtendCosinor(x = Xi, window = 1440 / N) # need to set lower and upper argument?
  # Correct time estimates by offset in start of recording
  coef$acrotime = coef$acrotime - timeOffsetHours
  add24ifneg = function(x) {
    if (x < 0) x = x + 24
    return(x)
  }
  coef$acrotime = add24ifneg(coef$acrotime)
  coef$acr = coef$acr + ((timeOffsetHours / 24) * 2 * pi)
  if (coef$acr < 0) coef$acr = coef$acr + (2 * pi)
  coefext$UpMesor = add24ifneg(coefext$UpMesor - timeOffsetHours)
  coefext$DownMesor = add24ifneg(coefext$DownMesor - timeOffsetHours)
  coefext$acrotime = add24ifneg(coefext$acrotime - timeOffsetHours)
  invisible(list(coef = coef, coefext = coefext))
}