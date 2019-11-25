g.intensitygradient = function(x,y) {
  # x: numeric vector of bin mid-points
  # y: numeric vector of time spent in bins
  y = ifelse(test = y <=0,yes = NA,no = y)
  ly = log(y)
  lx = log(x)
  y_intercept = NA
  gradient = NA
  rsquared = NA
  if (length(which(is.na(lx) == FALSE)) > 1 & length(which(is.na(ly) == FALSE)) > 1) {
    if (sd(lx,na.rm = TRUE) != 0 & sd(ly,na.rm = TRUE) != 0) {
      fitsum = summary(stats::lm(ly ~ lx))
      y_intercept = stats::coef(fitsum)[1,1]
      gradient = stats::coef(fitsum)[2,1]
      rsquared = fitsum$r.squared
    }
  }
  invisible(list(gradient=gradient,y_intercept=y_intercept,rsquared=rsquared))
}