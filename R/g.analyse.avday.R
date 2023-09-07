g.analyse.avday = function(doquan, averageday, M, IMP, t_TWDI, quantiletype,
                           ws3, doiglevels, firstmidnighti, ws2, midnightsi, params_247 = c(), 
                           qcheck = c(), acc.metric = c(), ...) {
  #get input variables
  input = list(...)
  if (length(input) > 0 || length(params_247) == 0) {
    # Extract and check parameters if user provides more arguments than just the parameter arguments,
    # or if params_[...] aren't specified (so need to be filled with defaults).
    # So, inside GGIR this will not be used, but it is used when g.analyse is used on its own
    # as if it was still the old g.analyse function
    params = extract_params(params_247 = params_247,
                            input = input) # load default parameters
    params_247 = params$params_247
  }
  if (length(acc.metric) == 0) {
    acc.metric = colnames(IMP$metashort)[which(colnames(IMP$metashort) %in% 
                                                 c("anglex", "angley", "anglez", "timestamp") == FALSE)[1]]
  }
  
  if (doquan == TRUE) {
    QLN = rep(" ",length(params_247[["qlevels"]]))
    for (QLNi in 1:length(params_247[["qlevels"]])) {
      QLN[QLNi] = paste0("p", (round((params_247[["qlevels"]][QLNi]) * 10000))/100)
    }
  }
  QUAN = qlevels_names = c()
  ML5AD = ML5AD_names = c()
  one2sixname = ""
  if (is.data.frame(t_TWDI) == TRUE) { # If an activity log was used then use for average day only 24 hour window.
    t_TWDI = c(0, 24)
  }
  if (doquan == TRUE) {
    for (quani in 1:ncol(averageday)) { # these columns of averageday correspond to the metrics from part 1
      if (colnames(M$metashort)[(quani + 1)] %in% c("anglex", "angley", "anglez") == FALSE) {
        #--------------------------------------
        # quantiles
        QUANtmp =  quantile(averageday[((t_TWDI[1] * (3600 / ws3)) + 1):(t_TWDI[length(t_TWDI)] * (3600 / ws3)), quani],
                            probs = params_247[["qlevels"]], na.rm = TRUE, type = quantiletype)
        QUAN = c(QUAN, QUANtmp)
        qlevels_namestmp = rep(" ", length(params_247[["qlevels"]]))
        for (QLNi in 1:length(params_247[["qlevels"]])) {
          qlevels_namestmp[QLNi] = paste0(QLN[QLNi], "_", colnames(M$metashort)[(quani + 1)], "_mg_",
                                          t_TWDI[1], "-", t_TWDI[length(t_TWDI)], "h")
        }
        qlevels_names = c(qlevels_names, qlevels_namestmp)
        rm(QUANtmp)
        #--------------------------------------
        #M5L5 - (Note: averageday is relative to starttime of measurement, but so is firstmidnighti (index of midnights), so M5L5 are correct)
        #loop through winhr
        for (winhr_value in params_247[["winhr"]]) {
          # Time window for L5 & M5 analysis
          t0_LFMF = params_247[["L5M5window"]][1] #start in 24 hour clock hours
          t1_LFMF = params_247[["L5M5window"]][2] + (winhr_value - (params_247[["M5L5res"]] / 60)) #end in 24 hour clock hours (if a value higher than 24 is chosen, it will take early hours of previous day to complete the 5 hour window
          avday = averageday[, quani]
          avday = c(avday[(firstmidnighti * (ws2 / ws3)):length(avday)], avday[1:((firstmidnighti * (ws2 / ws3)) - 1)])
          # Note that t_TWDI[length(t_TWDI)] in the next line makes that we only calculate ML5 over the full day
          ML5ADtmp = g.getM5L5(avday,ws3,t0_LFMF = t_TWDI[1], t1_LFMF = t_TWDI[length(t_TWDI)],
                               M5L5res = params_247[["M5L5res"]],
                               winhr_value, qM5L5 = params_247[["qM5L5"]], MX.ig.min.dur = params_247[["MX.ig.min.dur"]])
          ML5AD = as.data.frame(c(ML5AD, ML5ADtmp), stringsAsFactors = TRUE)
        }
        ML5N = names(ML5AD)
        for (ML5ADi in 1:length(ML5N)) {
          if (length(grep(pattern = "1to6", ML5N[ML5ADi])) == 0) {
            ML5AD_names[ML5ADi] = paste0(ML5N[ML5ADi], "_", colnames(M$metashort)[(quani + 1)],"_mg_",
                                         params_247[["L5M5window"]][1], "-", params_247[["L5M5window"]][2], "h")
          }
        }
        # Acceleration 1-6am
        one2sixname = paste0("1to6am_", colnames(M$metashort)[(quani + 1)], "_mg")
        ML5AD_names = c(ML5AD_names, one2sixname)
        ML5AD$one2six = mean(avday[((1 * 60 * (60 / ws3)) + 1):(6 * 60 * (60 / ws3))]) * 1000
        colnames(ML5AD)[which(colnames(ML5AD) == "one2six")] = one2sixname
      }	
    }	
  }
  igfullr_names = igfullr = c()
  if (doiglevels == TRUE) { 
    # intensity gradient (as described by Alex Rowlands 2018)
    # applied to the averageday per metric (except from angle metrics)
    igfullr = igfullr_names = c()
    for (igi in 1:ncol(averageday)) {
      if (colnames(M$metashort)[(igi + 1)] %in% c("anglex", "angley", "anglez") == FALSE) {
        y_ig = c()
        avday = averageday[, igi] 
        avday = c(avday[(firstmidnighti * (ws2 / ws3)):length(avday)], avday[1:((firstmidnighti * (ws2 / ws3)) - 1)])
        # we are not taking the segment of the day now (too much output)
        q60 = cut((avday * 1000), breaks = params_247[["iglevels"]], right = FALSE)
        y_ig  = (as.numeric(table(q60)) * ws3) / 60 #converting to minutes
        x_ig = zoo::rollmean(params_247[["iglevels"]], k = 2)
        igout = g.intensitygradient(x_ig, y_ig)
        if (length(avday) > 0) {
          igfullr = c(igfullr, as.vector(unlist(igout)))
        } else {
          igfullr = c(igfullr, rep("", 3))
        }
        igfullr_names = c(igfullr_names, paste0(c("ig_gradient", "ig_intercept", "ig_rsquared"),
                                                paste0("_", colnames(IMP$metashort)[igi + 1], "_0-24hr")))
      }
    }
  }
  # IS and IV variables
  fmn = midnightsi[1] * (ws2/ws3) # select data from first midnight to last midnight because we need full calendar days to compare
  lmn = (midnightsi[length(midnightsi)] * (ws2/ws3)) - 1
  # By using the metashort from the IMP we do not need to ignore segments, because data imputed
  Xi = IMP$metashort[fmn:lmn, acc.metric]
  # IV IS
  IVISout = g.IVIS(Xi, epochsizesecondsXi = ws3, 
                   IVIS_epochsize_seconds = params_247[["IVIS_epochsize_seconds"]], 
                   IVIS_windowsize_minutes = params_247[["IVIS_windowsize_minutes"]],
                   IVIS.activity.metric = params_247[["IVIS.activity.metric"]],
                   IVIS_acc_threshold = params_247[["IVIS_acc_threshold"]])
  InterdailyStability = IVISout$InterdailyStability
  IntradailyVariability = IVISout$IntradailyVariability
  rm(Xi)
  
  #----------------------------------
  # (Extended) Cosinor analysis
  if (params_247[["cosinor"]] == TRUE) {
    # Re-derive Xi but this time include entire time series
    # Here, we ignore duplicated values (when clock moves backward due to DST)
    handleDST = !duplicated(IMP$metashort)
    qcheck = qcheck[handleDST]
    Xi = IMP$metashort[handleDST, acc.metric]
    Nlong_epochs_day =  (1440 * 60) / ws2 # this is 96 by default
    dstgap = which(diff(midnightsi) != Nlong_epochs_day)
    if (length(dstgap) > 0) {
      # Time moved forward due to DST
      gaplocation = ((midnightsi[dstgap[1]] * ws2) / ws3) + (2 * (3600/ws3))
      # Insert NA values
      Xi = c(Xi[1:gaplocation], rep(NA, 3600/ws3), Xi[(gaplocation + 1):length(Xi)])
      qcheck = c(qcheck[1:gaplocation], rep(NA, 3600/ws3), qcheck[(gaplocation + 1):length(qcheck)])
    }

    # Xi = log((Xi * 1000) + 1)  # log transformed to be more robust against peaks in the data
    # set non-wear to missing values, because for Cosinor fit
    # it seems more logical to only fit with real data
    # this comes at the price of not being able to extract F_pseudo
    firstvalid = 1
    if (length(which(qcheck == 1)) > 0) {
      is.na(Xi[which(qcheck == 1)]) = TRUE
      # ignore invalid start of recording (if applicable)
      # such that 24 hour blocks start from first valid value
      firstvalid = which(qcheck == 0)[1]
      if (is.na(firstvalid) == FALSE) {
        if (firstvalid != 1) {
          Xi = Xi[firstvalid:length(Xi)]
        }
      }
    }
    if (length(which(is.na(Xi) == FALSE)) > (1440 * (60/ws3))) { # Only attempt cosinor analyses if there is more than 24 hours of data
      midnightsi_ws3 = (midnightsi - 1) * (ws2 / ws3)
      timeOffsetHours = (midnightsi_ws3[which(midnightsi_ws3 >= firstvalid - 1)[1]] - (firstvalid - 1)) / (3600 / ws3)
      if (ws3 < 60) {
        # If epochsize < 1 minute then aggregate to 1 minute by taking maximum value
        # but keep NA values
        XTtime = rep(1:length(Xi), each = 60 / ws3)
        XT = data.frame(Xi = Xi, time = XTtime[1:length(Xi)])
        custommean = function(x) {
          y = NA
          if (length(x) > 0) {
            if (length(which(is.na(x) == FALSE) ) > 0) {
              y = mean(x, na.rm = TRUE)
            }
          }
          return(y)
        }
        XT = aggregate(x = XT, by = list(XT$time), FUN = custommean)
        if (length(which(is.nan(XT$Xi) == TRUE)) > 0) {
          is.na(XT$Xi[which(is.nan(XT$Xi) == TRUE)]) = TRUE
        }
        # experimental: clip all peaks above Xth percentile?
        # Q9 = quantile(x = XT$Xi, probs = 0.75, na.rm = TRUE)
        # XT$Xi[which(XT$Xi >= Q9)] = Q9
        
        # log transform of data in millig
        notna = !is.na(XT$Xi)
        XT$Xi[notna] = log((XT$Xi[notna]*1000) + 1) 
        Xi = XT$Xi
        epochsize = 60
      } else {
        epochsize = ws3
      }
      cosinor_coef = cosinorAnalyses(Xi = Xi, epochsize = epochsize, timeOffsetHours = timeOffsetHours) 
      cosinor_coef$timeOffsetHours = timeOffsetHours
    } else {
      cosinor_coef = c()
    }
  } else {
    cosinor_coef = c()
  }
  invisible(list(InterdailyStability = InterdailyStability,
                 IntradailyVariability = IntradailyVariability, 
                 igfullr_names = igfullr_names,
                 igfullr = igfullr, QUAN = QUAN,
                 qlevels_names = qlevels_names,
                 ML5AD = ML5AD,
                 ML5AD_names = ML5AD_names, cosinor_coef = cosinor_coef))
}