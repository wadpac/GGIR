g.analyse.avday = function(doquan, averageday, M, IMP, t_TWDI, quantiletype,
                           ws3, doiglevels, firstmidnighti, ws2, midnightsi, params_247 = c(), 
                           qcheck = c(), acc.metric = c(), params_phyact = NULL, ...) {
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
                                                 c("anglex", "angley", "anglez", 
                                                   "ExtSleep", "timestamp", "marker") == FALSE)[1]]
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
      if (colnames(M$metashort)[(quani + 1)] %in% c("anglex", "angley", "anglez",
                                                    "ExtSleep", "marker") == FALSE) {
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
      if (colnames(M$metashort)[(igi + 1)] %in% c("anglex", "angley", "anglez", "marker") == FALSE) {
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
  #----------------------------------
  # Cosinor analysis, (Extended) Cosinor analysis, including IV, IS, and phi
  # based on time series where invalid data points are set to NA
  if (params_247[["cosinor"]] == TRUE) {
    cosinor_coef = apply_cosinor_IS_IV_Analyses(ts = IMP$metashort[, c("timestamp", acc.metric)],
                                                qcheck = qcheck,
                                                midnightsi, epochsizes = c(ws3, ws2),
                                                threshold = params_phyact[["threshold.lig"]][1]) # only use one threshold
  } else {
    cosinor_coef = c()
  }
  invisible(list(igfullr_names = igfullr_names,
                 igfullr = igfullr, QUAN = QUAN,
                 qlevels_names = qlevels_names,
                 ML5AD = ML5AD,
                 ML5AD_names = ML5AD_names,
                 cosinor_coef = cosinor_coef))
}