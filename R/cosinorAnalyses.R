cosinorAnalyses = function(Xi, epochsize = 60, timeOffsetHours = 0) {
  
  # TEMPORARILY INCORPORATING ActCR CODE BY JUNRUI DI AND COLLEAGUES
  # TO INVESTIGATE HOW WE CAN IMPROVE IT
  # AND USE THAT TO INFORM A POSSIBLE PULL REQUEST
  
  ActExtendCosinor = function(
    x,
    window = 1,
    lower = c(0, 0, -1, 0, -3), ## min, amp, alpha, beta, phi
    upper = c(Inf, Inf, 1, Inf, 27)) {
    if(1440 %% window != 0){
      stop("Only use window size that is an integer factor of 1440")
    }
    if(length(x) %% (1440/window) != 0){
      stop("Window size and length of input vector doesn't match.
         Only use window size that is an integer factor of 1440")
    }
    dim = 1440/window
    n.days = length(x)/dim
    
    # Stage 1 ---- Cosinor Model
    tmp.dat = data.frame(time = rep(1:dim, n.days) / (60/window), Y = x)
    fit = cosinor::cosinor.lm(Y ~ time(time) + 1, data = tmp.dat, period = 24)
    
    mesor = fit$coefficients[1]
    amp = fit$coefficients[2]
    acr = cosinor2::correct.acrophase(fit)
    acrotime = (-1) * acr * 24/(2 * pi)
    
    names(mesor) = names(amp) = names(acr) = names(acrotime) = NULL
    
    # Stage 2 ---- Transformation
    
    ## Set up the initial values
    e_min0 = max(mesor - amp, 0)
    e_amp0 = 2 * amp
    e_phi0 = acrotime
    e_par0 = c(e_min0, e_amp0, 0, 2, e_phi0) ## min, amp, alpha, beta, phi

    
    tmp.dat = tmp.dat[!is.na(tmp.dat$Y),] # new line relative to ActCR original
    
    fit_nls = minpack.lm::nls.lm(e_par0, fn = fn_obj,
                     lower = lower,
                     upper = upper,
                     tmp.dat = tmp.dat,
                     control = minpack.lm::nls.lm.control(maxiter = 1000))

    # Extract time series to ease plotting the models:
    fittedYext = tmp.dat$Y - residuals(fit_nls) # new line relative to ActCR original
    fittedY = fitted(fit$fit) # new line relative to ActCR original
    original = tmp.dat$Y # new line relative to ActCR original
    time = tmp.dat$time # new line relative to ActCR original
    cosinor_ts = as.data.frame(cbind(time, original, fittedY, fittedYext)) # new line relative to ActCR original

    # Estimated exteded cosinor parameters,in the order of
    ## minimum, amplitude, alpha, beta, acrophase
    coef.nls = coef(fit_nls)
    
    e_min = coef.nls[1]
    e_amp = coef.nls[2]
    e_alpha = coef.nls[3]
    e_beta = coef.nls[4]
    e_acrotime = coef.nls[5]
    
    ## Pseudo F statistics
    RSS_cos = sum((fit$fit$residuals)^2)
    RSS_ext = sum(residuals(fit_nls)^2)
    F_pseudo = ((RSS_cos - RSS_ext)/2)/(RSS_ext/(nrow(tmp.dat) - 5))
    
    ## Derived metrics
    UpMesor = -acos(e_alpha)/(2*pi/24) + e_acrotime
    DownMesor = acos(e_alpha)/(2*pi/24) + e_acrotime
    MESOR = e_min + e_amp/2
    
    ret = list("minimum" = e_min,
               "amp" = e_amp,
               "alpha" = e_alpha,
               "beta" = e_beta,
               "acrotime" = e_acrotime,
               "F_pseudo" = F_pseudo,
               "UpMesor" = UpMesor,
               "DownMesor" = DownMesor,
               "MESOR" = MESOR,
               "ndays" = n.days,
               "cosinor_ts" = cosinor_ts)
    return(ret)
  }
  
  ## Objective function to optimize for extended cosinor model
  fn_obj = function(par, tmp.dat) {
    ct = cos((tmp.dat[, 1] - par[5]) * 2 * pi / 24)
    lct = exp(par[4] * (ct - par[3])) / (1 + exp(par[4] * (ct - par[3])))
    rt = par[1] + par[2] * lct
    tmp.dat[, 2] - rt
  }
  #==========================================  
  
  # Continuation of actual GGIR code
  
  # Apply Cosinor function from ActRC
  N = 1440 * (60 / epochsize) # Number of epochs per day
  Xi = Xi[1:(N * floor(length(Xi) / N))] # ActCR expects integer number of days
  coef = ActCR::ActCosinor(x = Xi, window = 1440 / N)

  # Apply Extended Cosinor function from ActRC (now temporarily turned of to apply my own version)
  # ActCR::
  coefext = ActExtendCosinor(x = Xi, window = 1440 / N) # need to set lower and upper argument?
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
  # which is inverse correlated with acrotime
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