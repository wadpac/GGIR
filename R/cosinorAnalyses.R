cosinorAnalyses = function(Xi, epochsize = 60, timeOffsetHours = 0) {
  # Apply Cosinor function from ActRC
  N = 1440 * (60 / epochsize) # Number of epochs per day
  Xi = Xi[1:(N * floor(length(Xi) / N))] # ActCR expects integer number of days
  coef = ActCR::ActCosinor(x = Xi, window = 1440 / N)

  # Apply Extended Cosinor function from ActRC (now temporarily turned of to apply my own version)
  # ActCR::
  coefext = ActCR::ActExtendCosinor(x = Xi, window = 1440 / N, export_ts = TRUE) # need to set lower and upper argument?
  # Correct time estimates by offset in start of recording
  add24ifneg = function(x) {
    if (x < 0) x = x + 24
    return(x)
  }
  coef$params$acrotime = add24ifneg(coef$params$acrotime - timeOffsetHours)
  coefext$params$UpMesor = add24ifneg(coefext$params$UpMesor - timeOffsetHours)
  coefext$params$DownMesor = add24ifneg(coefext$params$DownMesor - timeOffsetHours)
  coefext$params$acrotime = add24ifneg(coefext$params$acrotime - timeOffsetHours)
  # do same for acrophase in radians (24 hours: 2 * pi)
  # take absolute value of acrophase, because it seems ActCR provides negative value in radians,
  # which is inverse correlated with acrotime
  coef$params$acr = abs(coef$params$acr) - ((timeOffsetHours / 24) * 2 * pi)
  k = ceiling(abs(coef$params$acr) / (pi * 2))
  if (coef$params$acr < 0) coef$params$acr = coef$params$acr + (k * 2 * pi)
  # Perform IVIS on the same input signal to allow for direct comparison
  IVIS = g.IVIS(Xi = Xi / 1000, # divide by 1000 because function g.IVIS internally multiplies by 1000 when IVIS.activity.metric = 2
                epochsizesecondsXi = epochsize, 
                IVIS_windowsize_minutes = 60,
                IVIS.activity.metric = 2,
                IVIS_acc_threshold = log(20 + 1),
                IVIS_per_daypair = TRUE) # take log, because Xi is logtransformed with offset of 1
  
  coefext$params$R2 = cor(coefext$cosinor_ts$original, coefext$cosinor_ts$fittedYext)^2
  coef$params$R2 = cor(coefext$cosinor_ts$original, coefext$cosinor_ts$fittedY)^2
  
  # # this should equal: https://en.wikipedia.org/wiki/Coefficient_of_determination
  # yi = coefext$cosinor_ts$original
  # fi = coefext$cosinor_ts$fittedY
  # meanY = mean(coefext$cosinor_ts$original)
  # SSres = sum((yi - fi)^2)
  # SStot = sum((y - meanY)^2)
  # R2 = 1 - (SSres / SStot)
  
  invisible(list(coef = coef, coefext = coefext, IVIS = IVIS))
}