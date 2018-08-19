g.intensitygradient = function(x,y) {
  # x: numeric vector of bin mid-points
  # y: numeric vector of time spent in bins
  y = ifelse(test = y <=0,yes = NA,no = y)
  ly = log(y)
  lx = log(x)
  fitsum = summary(stats::lm(ly ~ lx))
  y_intercept = stats::coef(fitsum)[1,1]
  gradient = stats::coef(fitsum)[2,1]
  rsquared = fitsum$r.squared
  invisible(list(gradient=gradient,y_intercept=y_intercept,rsquared=rsquared))
}