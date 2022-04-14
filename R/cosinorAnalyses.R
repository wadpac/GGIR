cosinorAnalyses = function(Xi, epochsize = 60, timeOffsetHours = 0) {
  # Apply Extended Cosinor function from ActRC
  N = 1440 * (60 / epochsize) # Number of epochs per day
  Xi = Xi[1:(N * floor(length(Xi) / N))] # ActCR expects integer number of days
  coef = ActCR::ActCosinor(x = Xi, window = 1440 / N)
  coefext = ActCR::ActExtendCosinor(x = Xi, window = 1440 / N) # need to set lower and upper argument?
  # Correct time estimates by offset in start of recording
  add24ifneg = function(x) {
    if (x < 0) x = x + 24
    return(x)
  }
  coef$acrotime = add24ifneg(coef$acrotime - timeOffsetHours)
  coefext$UpMesor = add24ifneg(coefext$UpMesor - timeOffsetHours)
  coefext$DownMesor = add24ifneg(coefext$DownMesor - timeOffsetHours)
  coefext$acrotime = add24ifneg(coefext$acrotime - timeOffsetHours)
  # do same for acrophase in radians (24 hours: 2 * pi)
  # take absolute value of acrophase, because it seems ActCR provides negative value in radians,
  # which is inversaly correlated with acrotime
  coef$acr = abs(coef$acr) - ((timeOffsetHours / 24) * 2 * pi)
  k = ceiling(abs(coef$acr) / (pi * 2))
  if (coef$acr < 0) coef$acr = coef$acr + (k * 2 * pi)
  # Perform IVIS on the same input signal to allow for direct comparison
  IVIS = g.IVIS(Xi = Xi / 1000, # divide by 1000 because function g.IVIS internally multiplies by 1000 when IVIS.activity.metric = 2
                epochsizesecondsXi = epochsize, 
                IVIS_windowsize_minutes = 60,
                IVIS.activity.metric = 2,
                IVIS_acc_threshold = log(20 + 1),
                IVIS_per_daypair = TRUE) # take log, because Xi is logtransformed with offset of 1
  
  invisible(list(coef = coef, coefext = coefext, IVIS = IVIS))
}