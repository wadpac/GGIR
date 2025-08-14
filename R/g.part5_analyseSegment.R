g.part5_analyseSegment = function(indexlog, timeList, levelList,
                                  segments,
                                  segments_names,
                                  dsummary, ds_names,
                                  params_general, params_output,
                                  params_sleep, params_247,
                                  params_phyact,
                                  sumSleep, sibDef,
                                  fullFilename,
                                  add_one_day_to_next_date,
                                  lightpeak_available,
                                  tail_expansion_log,
                                  foldernamei, sibreport = NULL) {
  # unpack list objects:
  # indexlog
  fileIndex = indexlog$fileIndex
  timewindowi = indexlog$winType
  wi = indexlog$winIndex
  qqq = indexlog$winStartEnd
  si = indexlog$segIndex1
  current_segment_i = indexlog$segIndex2
  segStart = indexlog$segStartEnd[1]
  segEnd = indexlog$segStartEnd[2]
  fi = indexlog$columnIndex
  
  # timeList
  ts = timeList$ts
  sec = timeList$sec
  min = timeList$min
  hour = timeList$hour
  time_POSIX = timeList$time_POSIX
  ws3new = timeList$epochSize
  
  # levelList
  TRLi = levelList$threshold[1]
  TRMi = levelList$threshold[2]
  TRVi = levelList$threshold[3]
  LEVELS = levelList$LEVELS
  Lnames = levelList$Lnames
  OLEVELS = levelList$OLEVELS
  bc.mvpa = levelList$bc.mvpa
  bc.in = levelList$bc.in
  bc.lig = levelList$bc.lig
  
  skiponset = skipwake = TRUE
  nap_overwrite_bc = params_sleep[["nap_overwrite_behaviourclass"]]
  nap_class_names = NULL
  #==========================
  # The following is to avoid issue with merging sleep variables from part 4
  # Note that this means that for MM windows there can be multiple or no wake or onsets in window
  date = as.Date(ts$time[qqq[1]], tz = params_general[["desiredtz"]]) # changed segStart for qqq[1] in case segment is not available in this day
  if (add_one_day_to_next_date == TRUE & timewindowi %in% c("WW", "OO")) { # see below for explanation
    date = date + 1
    add_one_day_to_next_date = FALSE
  }
  weekday = weekdays(date, abbreviate = FALSE)
  dsummary[si,fi:(fi + 1)] = c(weekday, as.character(date))
  ds_names[fi:(fi + 1)] = c("weekday", "calendar_date"); fi = fi + 2
  # window and segment of the day
  segmentName = segments_names[current_segment_i]
  segmentTiming = names(segments)[current_segment_i]
  dsummary[si, fi:(fi + 2)] = c(wi, segmentName, segmentTiming)
  ds_names[fi:(fi + 2)] = c("window_number", "window", "start_end_window"); fi = fi + 3
  # Get onset and waking timing, both as timestamp and as index
  onsetwaketiming = g.part5.onsetwaketiming(qqq, ts, min, sec, hour, timewindowi)
  onset = onsetwaketiming$onset; wake = onsetwaketiming$wake
  onseti = onsetwaketiming$onseti; wakei = onsetwaketiming$wakei
  skiponset = onsetwaketiming$skiponset; skipwake = onsetwaketiming$skipwake
  if (wake < 24 & timewindowi == "WW") {
    # waking up before midnight means that next WW window
    # will start a day before the date we refer to when discussing it's SPT
    # So, for next window we have to do date = date + 1
    add_one_day_to_next_date = TRUE
  }
  if (onset > 24 & timewindowi == "OO") {
    # onset after midnight means that next OO window
    # will start a date after the date we refer to when discussing it;s SPT
    # So, for next window we have to do date = date + 1
    add_one_day_to_next_date = TRUE
  }
  # Add to dsummary output matrix
  if (skiponset == FALSE) {
    dsummary[si,fi] = onset
    dsummary[si,fi + 1] = as.character(strftime(format(time_POSIX[onseti]), tz = params_general[["desiredtz"]], format = "%H:%M:%S"))
  } else {
    dsummary[si,fi:(fi + 1)] = rep(NA, 2)
  }
  ds_names[fi:(fi + 1)] = c("sleeponset", "sleeponset_ts");      fi = fi + 2
  if (skipwake == FALSE) {
    dsummary[si,fi] = wake
    dsummary[si,fi + 1] = as.character(strftime(format(time_POSIX[wakei]), tz = params_general[["desiredtz"]], format = "%H:%M:%S"))
  } else {
    dsummary[si,fi:(fi + 1)] = rep(NA, 2)
  }
  ds_names[fi:(fi + 1)] = c("wakeup", "wakeup_ts");      fi = fi + 2
  # if skiponset and skipwake and "MM", then set full window as awake
  if (skiponset == TRUE & skipwake == TRUE & timewindowi == "MM") {
    ts$diur_bu = ts$diur
    ts$diur[qqq[1]:qqq[2]] = 0
  }
  # extract date and use this to retrieve corresponding part 4 information about the nights:
  options(encoding = "UTF-8")
  # look up matching part4 entry:
  recDates = as.Date(sumSleep$calendar_date, format = "%d/%m/%Y", origin = "1970-01-01")
  dsummary[si, fi] = sibDef
  ds_names[fi] = "sleepparam";      fi = fi + 1
  dayofinterest = which(recDates == date)
  if (length(dayofinterest) > 0) {
    dayofinterest = dayofinterest[1]
    dsummary[si,fi:(fi + 5)] = c(sumSleep$night[dayofinterest],
                                 sumSleep$daysleeper[dayofinterest],
                                 sumSleep$cleaningcode[dayofinterest],
                                 sumSleep$guider[dayofinterest],
                                 sumSleep$sleeplog_used[dayofinterest],
                                 sumSleep$acc_available[dayofinterest])
    ds_names[fi:(fi + 5)] = c("night_number", "daysleeper", "cleaningcode",
                              "guider", "sleeplog_used", "acc_available");      fi = fi + 6
    if (!is.na(segStart) & !is.na(segEnd)) {
      # segment available in time series
      ts$guider[segStart:segEnd] = sumSleep$guider[dayofinterest] # add guider also to timeseries
    }
  } else {
    dsummary[si,fi:(fi + 5)] = rep(NA, 6)
    ds_names[fi:(fi + 5)] = c("night_number",
                              "daysleeper","cleaningcode","guider",
                              "sleeplog_used","acc_available"); fi = fi + 6
  }
  #==========================
  # define time windows:
  # We will get onset and wakeup
  # regarding to the same day of measurement in the same row.
  # which differs between MM and WW
  # Also, it allows for the analysis of the first day for those studies 
  # in which the accelerometer is started during the morning and the first day is of interest.
  # qqq1 is the start of the day/segment
  # qqq2 is the end of the day/segment
  qqq1 = segStart
  qqq2 = segEnd
  # keep track of threshold value
  dsummary[si, fi:(fi + 2)] = c(TRLi, TRMi, TRVi)
  ds_names[fi:(fi + 2)] = c("TRLi", "TRMi", "TRVi")
  fi = fi + 3
  wlih = ((qqq2 - qqq1) + 1)/((60/ws3new) * 60)
  if (!is.na(qqq1)) {
    if (qqq1 > length(LEVELS)) qqq1 = length(LEVELS)
    sse = qqq1:qqq2
  } else {
    sse = NULL
  }
  doNext = FALSE
  if (length(sse) >= 1) { #next
    #============================================================
    # percentage of available data
    zt_hrs_nonwear = (length(which(ts$diur[sse] == 0 & ts$nonwear[sse] == 1)) * ws3new) / 3600 #day
    zt_hrs_total = (length(which(ts$diur[sse] == 0)) * ws3new) / 3600 #day
    dsummary[si,fi] = (zt_hrs_nonwear/zt_hrs_total)  * 10000 / 100
    ds_names[fi] = "nonwear_perc_day";      fi = fi + 1
    zt_hrs_nonwear = (length(which(ts$diur[sse] == 1 & ts$nonwear[sse] == 1)) * ws3new) / 3600 #night
    zt_hrs_total = (length(which(ts$diur[sse] == 1)) * ws3new) / 3600 #night
    dsummary[si,fi] =  (zt_hrs_nonwear/zt_hrs_total)  * 10000 / 100
    ds_names[fi] = "nonwear_perc_spt";      fi = fi + 1
    zt_hrs_nonwear = (length(which(ts$nonwear[sse] == 1)) * ws3new) / 3600
    zt_hrs_total = (length(ts$diur[sse]) * ws3new) / 3600 #night and day
    dsummary[si,fi] =  (zt_hrs_nonwear/zt_hrs_total)  * 10000 / 100
    ds_names[fi] = "nonwear_perc_day_spt";      fi = fi + 1
    #=======================================================
    # nap/sib/nonwear overlap analysis
    #=======================================================
    if (params_output[["do.sibreport"]]  == TRUE &&
        !is.null(params_sleep[["possible_nap_window"]]) &&
        !is.null(params_sleep[["possible_nap_dur"]])) {
      restAnalyses = g.part5.analyseRest(sibreport = sibreport, dsummary = dsummary,
                                         ds_names = ds_names, fi = fi, di = si,
                                         ts = ts[sse[ts$diur[sse] == 0], ],
                                         tz = params_general[["desiredtz"]],
                                         params_sleep = params_sleep)
      fi = restAnalyses$fi
      si = restAnalyses$di
      ts[sse[ts$diur[sse] == 0], ] = restAnalyses$ts
      dsummary = restAnalyses$dsummary
      ds_names = restAnalyses$ds_names
      
      # If TRUE: Add detect naps to LEVELS, which probably overwrites the inactivity classification
      # If FALSE: Create separate vector to indicate when naps happened
      # so this overwrites the behavioural classes for those time segments
      # False may become default as this allows for more flexibility for the future
      # as we retain both classifications
      if (nap_overwrite_bc == FALSE) {
        nap_LEVELS = rep(NA, length(LEVELS))
      }
      
      N_nap_dur_classes = length(params_sleep[["possible_nap_dur"]])
      for (ndi in 1:(N_nap_dur_classes - 1)) {
        detectedNaps = which(ts$sibdetection[sse] == (ndi + 1))
        nap_class_name = paste0("day_nap_", params_sleep[["possible_nap_dur"]][ndi],
                                "_", params_sleep[["possible_nap_dur"]][ndi + 1])
        if (nap_overwrite_bc == TRUE) {
          if (nap_class_name %in% Lnames == FALSE) {
            Lnames = c(Lnames, nap_class_name)
          }
        } else {
          nap_class_names = c(nap_class_names, nap_class_name)
        }
        if (length(detectedNaps) > 0) {
          if (nap_overwrite_bc == TRUE) {
            LEVELS[sse[detectedNaps]] = which(Lnames == nap_class_name) - 1
          } else {
            nap_LEVELS[sse[detectedNaps]] = which(nap_class_names == nap_class_name) - 1
          }
        }
      }
    }
    #===============================================
    # TIME SPENT IN WINDOWS (window is either midnight-midnight or waking up-waking up)
    for (levelsc in 0:(length(Lnames) - 1)) {
      dsummary[si,fi] = (length(which(LEVELS[sse] == levelsc)) * ws3new) / 60
      ds_names[fi] = paste0("dur_", Lnames[levelsc + 1],"_min");      fi = fi + 1
    }
    for (g in 1:4) {
      dsummary[si, (fi + (g - 1))] = (length(which(OLEVELS[sse] == g)) * ws3new) / 60
    }
    ds_names[fi:(fi + 3)] = c("dur_day_total_IN_min",
                              "dur_day_total_LIG_min",
                              "dur_day_total_MOD_min",
                              "dur_day_total_VIG_min")
    fi = fi + 4
    dsummary[si, fi] = (length(which(ts$diur[sse] == 0)) * ws3new) / 60
    ds_names[fi] = "dur_day_min";      fi = fi + 1
    dsummary[si, fi] = (length(which(ts$diur[sse] == 1)) * ws3new) / 60
    ds_names[fi] = "dur_spt_min";      fi = fi + 1
    dsummary[si, fi] = (length(c(sse)) * ws3new) / 60
    ds_names[fi] = "dur_day_spt_min";      fi = fi + 1
    # TIME SPENT IN NAP WINDOWS
    if (nap_overwrite_bc == FALSE && length(nap_class_names) > 0) {
      for (levelsc in 0:(length(nap_class_names) - 1)) {
        dsummary[si,fi] = (length(which(nap_LEVELS[sse] == levelsc)) * ws3new) / 60
        ds_names[fi] = paste0("dur_", nap_class_names[levelsc + 1],"_min");      fi = fi + 1
      }
    }
    #============================================
    # Number of long wake periods (defined as > 5 minutes) during the night
    Nawake = length(which(abs(diff(which(LEVELS[sse] == 0))) > (300 / ws3new))) - 2
    if (Nawake < 0) Nawake = 0
    dsummary[si, fi] = Nawake
    ds_names[fi] = "N_atleast5minwakenight";      fi = fi + 1
    #=============================
    # sleep efficiency
    dsummary[si,fi] = length(which(ts$sibdetection[sse] == 1 &
                                     ts$diur[sse] == 1)) / length(which(ts$diur[sse] == 1))
    ds_names[fi] = "sleep_efficiency_after_onset";      fi = fi + 1
    #===============================================
    # NAPS (estimation)
    if (params_output[["do.sibreport"]] == TRUE &&
        "nap1_nonwear2" %in% colnames(ts) &&
        length(params_sleep[["nap_model"]]) > 0) {
      dsummary[si,fi] = length(which(diff(c(-1, which(ts$nap1_nonwear2[sse] == 1 & ts$diur[sse] == 0))) > 1))
      ds_names[fi] = "nap_count";      fi = fi + 1
      dsummary[si,fi] = round((sum(ts$nap1_nonwear2[sse[which(ts$nap1_nonwear2[sse] == 1 & ts$diur[sse] == 0)]]) * ws3new) / 60, digits = 2)
      ds_names[fi] = "nap_totalduration";      fi = fi + 1
    }
    if (length(tail_expansion_log) != 0) {
      # do not store sleep variables if data was expanded in GGIR part 1
      dsummary[si, fi] = (tail_expansion_log[["short"]] * ws3new) / 60
    } else {
      dsummary[si, fi] = 0
    }
    ds_names[fi] = "tail_expansion_minutes";      fi = fi + 1
    #===============================================
    # AVERAGE ACC PER WINDOW
    for (levelsc in 0:(length(Lnames) - 1)) {
      dsummary[si,fi] = mean(ts$ACC[sse[LEVELS[sse] == levelsc]], na.rm = TRUE)
      ds_names[fi] = paste("ACC_", Lnames[levelsc + 1], "_mg", sep = "");      fi = fi + 1
    }
    for (g in 1:4) {
      dsummary[si,(fi + (g - 1))] = mean(ts$ACC[sse[OLEVELS[sse] == g]], na.rm = TRUE)
    }
    ds_names[fi:(fi + 3)] = c("ACC_day_total_IN_mg", "ACC_day_total_LIG_mg",
                              "ACC_day_total_MOD_mg", "ACC_day_total_VIG_mg")
    fi = fi + 4
    dsummary[si, fi] = mean(ts$ACC[sse[ts$diur[sse] == 0]], na.rm = TRUE)
    ds_names[fi] = "ACC_day_mg";      fi = fi + 1
    dsummary[si, fi] = mean(ts$ACC[sse[ts$diur[sse] == 1]], na.rm = TRUE)
    ds_names[fi] = "ACC_spt_mg";      fi = fi + 1
    dsummary[si, fi] = median(ts$ACC[sse[ts$diur[sse] == 1]], na.rm = TRUE)
    ds_names[fi] = "ACC_spt_mg_median";      fi = fi + 1
    dsummary[si, fi] = sd(ts$ACC[sse[ts$diur[sse] == 1]], na.rm = TRUE)
    ds_names[fi] = "ACC_spt_mg_stdev";      fi = fi + 1
    dsummary[si, fi] = mean(ts$ACC[sse], na.rm = TRUE)
    ds_names[fi] = "ACC_day_spt_mg";      fi = fi + 1
    
    # #======================================================
    # # STEPS... (IF STEP_COUNT IS AVAILABLE)
    # if ("step_count" %in% names(ts)) {
    #   if (length(params_247[["clevels"]]) > 1) {
    #     for (windowhalf in c("day", "spt")) {
    #       step_subsegment = sse[ts$diur[sse] == ifelse(windowhalf == "day", yes = 0, no = 1)]
    #       cadence = ts$step_count[step_subsegment] / (60 / ws3new)
    #       step_count = ts$step_count[step_subsegment]
    #       for (cleveli in 1:(length(params_247[["clevels"]]) - 1)) {
    #         cadence_condition_met = which(cadence >= params_247[["clevels"]][cleveli] &
    #                                         cadence < params_247[["clevels"]][cleveli + 1])
    #         dsummary[si, fi] = floor(sum(step_count[cadence_condition_met]))
    #         tmp_ending_of_name = paste0("in_candence_", params_247[["clevels"]][cleveli],
    #                                     "_", params_247[["clevels"]][cleveli + 1], "_spm")
    #         ds_names[fi] = paste0("STEPS_", windowhalf, "_count_", tmp_ending_of_name)
    #         fi = fi + 1
    #         dsummary[si, fi] = length(step_count[cadence_condition_met]) * (ws3new / 60)
    #         ds_names[fi] = paste0("STEPS_", windowhalf, "_min_", tmp_ending_of_name)
    #         fi = fi + 1
    #       }
    #     }
    #   }
    # }
    #===============================================
    # QUANTILES...
    WLH = ((qqq2 - qqq1) + 1)/((60/ws3new) * 60)
    if (WLH <= 1) WLH = 1.001
    dsummary[si, fi] = quantile(ts$ACC[sse],probs = ((WLH - 1)/WLH), na.rm = TRUE)
    ds_names[fi] = paste("quantile_mostactive60min_mg", sep = "");      fi = fi + 1
    dsummary[si, fi] = quantile(ts$ACC[sse],probs = ((WLH - 0.5)/WLH), na.rm = TRUE)
    ds_names[fi] = paste("quantile_mostactive30min_mg", sep = "");      fi = fi + 1
    #===============================================
    NANS = which(is.nan(dsummary[si,]) == TRUE) #average of no values will results in NaN
    if (length(NANS) > 0) dsummary[si,NANS] = ""
    #===============================================
    # NUMBER OF BOUTS
    checkshape = function(boutcount) {
      if (is.matrix(boutcount) == FALSE) {# if there is only one bout setting
        boutcount = as.matrix(boutcount)
        if (nrow(boutcount) > ncol(boutcount)) boutcount = t(boutcount)
      }
      return(boutcount)
    }
    bc.mvpa = checkshape(bc.mvpa)
    for (bci in 1:nrow(bc.mvpa)) {
      RLE = rle(bc.mvpa[bci, sse])
      dsummary[si, fi + (bci - 1)] = length(which(RLE$values == 1))
      if (bci == 1) {
        ds_names[fi + (bci - 1)] = paste0("Nbouts_day_MVPA_bts_", params_phyact[["boutdur.mvpa"]][bci])
      } else {
        ds_names[fi + (bci - 1)] = paste0("Nbouts_day_MVPA_bts_", params_phyact[["boutdur.mvpa"]][bci], "_", params_phyact[["boutdur.mvpa"]][bci - 1])
      }
    }
    fi = fi + bci
    bc.in = checkshape(bc.in)
    for (bci in 1:nrow(bc.in)) {
      RLE = rle(bc.in[bci,sse])
      dsummary[si,fi + (bci - 1)] = length(which(RLE$values == 1))
      if (bci == 1) {
        ds_names[fi + (bci - 1)] = paste0("Nbouts_day_IN_bts_",params_phyact[["boutdur.in"]][bci])
      } else {
        ds_names[fi + (bci - 1)] = paste0("Nbouts_day_IN_bts_",
                                          params_phyact[["boutdur.in"]][bci], "_",
                                          params_phyact[["boutdur.in"]][bci - 1])
      }
    }
    fi = fi + bci
    bc.lig = checkshape(bc.lig)
    for (bci in 1:nrow(bc.lig)) {
      RLE = rle(bc.lig[bci,sse])
      dsummary[si,fi + (bci - 1)] = length(which(RLE$values == 1))
      if (bci == 1) {
        ds_names[fi + (bci - 1)] = paste0("Nbouts_day_LIG_bts_",
                                          params_phyact[["boutdur.lig"]][bci])
      } else {
        ds_names[fi + (bci - 1)] = paste0("Nbouts_day_LIG_bts_",
                                          params_phyact[["boutdur.lig"]][bci], "_",
                                          params_phyact[["boutdur.lig"]][bci - 1])
      }
    }
    fi = fi + bci
    #===============================================
    # NUMBER OF WINDOWS / BLOCKS
    RLE_LEVELS = rle(LEVELS[sse])
    RLE_OLEVELS = rle(OLEVELS[sse])
    # RunLengthEncoding
    for (levelsc in 0:(length(Lnames) - 1)) {
      dsummary[si, fi] = length(which(RLE_LEVELS$values == levelsc))
      ds_names[fi] = paste("Nblocks_", Lnames[levelsc + 1], sep = "");      fi = fi + 1
    }
    for (g in 1:4) {
      dsummary[si, (fi + (g - 1))] = length(which(RLE_OLEVELS$values == g))
    }
    ds_names[fi:(fi + 3)] = c("Nblocks_day_total_IN", "Nblocks_day_total_LIG",
                              "Nblocks_day_total_MOD", "Nblocks_day_total_VIG")
    fi = fi + 4
    dsummary[si, fi:(fi + 5)] = c(params_phyact[["boutcriter.in"]],
                                  params_phyact[["boutcriter.lig"]],
                                  params_phyact[["boutcriter.mvpa"]],
                                  paste(params_phyact[["boutdur.in"]], collapse = "_"),
                                  paste(params_phyact[["boutdur.lig"]], collapse = "_"),
                                  paste(params_phyact[["boutdur.mvpa"]], collapse = "_"))
    ds_names[fi:(fi + 5)] = c("boutcriter.in", "boutcriter.lig", "boutcriter.mvpa",
                              "boutdur.in",  "boutdur.lig", "boutdur.mvpa"); fi = fi + 6
    
    #-------------
    # If naps are in separate vector than count them separately
    if (nap_overwrite_bc == FALSE && length(nap_class_names) > 0) {
      RLE_nap_LEVELS = rle(nap_LEVELS[sse])
      for (levelsc in 0:(length(nap_class_names) - 1)) {
        dsummary[si,fi] = length(which(RLE_nap_LEVELS$values == levelsc))
        ds_names[fi] = paste0("Nblocks_", nap_class_names[levelsc + 1]);      fi = fi + 1
      }
    }
    #===========================
    # Intensity gradient over waking hours
    if (length(params_247[["iglevels"]]) > 0) {
      q55 = cut(ts$ACC[sse[ts$diur[sse] == 0]], breaks = params_247[["iglevels"]], right = FALSE)
      x_ig = zoo::rollmean(params_247[["iglevels"]], k = 2)
      y_ig = (as.numeric(table(q55)) * ws3new)/60 #converting to minutes
      dsummary[si,fi:(fi + 2)] = as.numeric(g.intensitygradient(x_ig, y_ig))
      ds_names[fi:(fi + 2)] = c("ig_day_gradient", "ig_day_intercept", "ig_day_rsquared")
      fi = fi + 3
    }
    #===========================
    # Intensity gradient over the full window (waking + spt)
    if (length(params_247[["iglevels"]]) > 0) {
      q55 = cut(ts$ACC[sse], breaks = params_247[["iglevels"]], right = FALSE)
      x_ig = zoo::rollmean(params_247[["iglevels"]], k = 2)
      y_ig = (as.numeric(table(q55)) * ws3new)/60 #converting to minutes
      dsummary[si,fi:(fi + 2)] = as.numeric(g.intensitygradient(x_ig, y_ig))
      ds_names[fi:(fi + 2)] = c("ig_day_spt_gradient", "ig_day_spt_intercept", "ig_day_spt_rsquared")
      fi = fi + 3
    }
    #===============================================
    # FRAGMENTATION
    if (length(params_phyact[["frag.metrics"]]) > 0) {
      # Only do daytime fragmentation and not spt window because in part 5 the spt window does
      # not necessarily include valid data. spt window fragmentation will be performed in part 6
      fragmode = "day"
      frag.out = g.fragmentation(frag.metrics = params_phyact[["frag.metrics"]],
                                 LEVELS = LEVELS[sse[ts$diur[sse] == ifelse(fragmode == "day", 0, 1)]],
                                 Lnames = Lnames, xmin = 60/ws3new, mode = fragmode)
      # fragmentation values can come with a lot of decimal places
      dsummary[si, fi:(fi + (length(frag.out) - 1))] = round(as.numeric(frag.out), digits = 6)
      ds_names[fi:(fi + (length(frag.out) - 1))] = paste0("FRAG_", names(frag.out), "_", fragmode)
      fi = fi + length(frag.out)
    }
    #===============================================
    # LIGHT, IF AVAILABLE
    if ("lightpeak" %in% colnames(ts) & length(params_247[["LUX_day_segments"]]) > 0) {
      # mean LUX
      if (length(which(ts$diur[sse] == 0)) > 0 & length(which(ts$diur[sse] == 1)) > 0) {
        dsummary[si,fi] =  round(max(ts$lightpeak[sse[ts$diur[sse] == 0]], na.rm = TRUE), digits = 1)
        dsummary[si,fi + 1] = round(mean(ts$lightpeak[sse[ts$diur[sse] == 0]], na.rm = TRUE), digits = 1)
        dsummary[si,fi + 2] = round(mean(ts$lightpeak[sse[ts$diur[sse] == 1]], na.rm = TRUE), digits = 1)
        dsummary[si,fi + 3] = round(mean(ts$lightpeak[sse[ts$diur[sse] == 0 & ts$ACC[sse] > TRMi]], na.rm = TRUE),
                                    digits = 1)
      } else {
        dsummary[si,fi:(fi + 3)] = NA
      }
      ds_names[fi:(fi + 3)] = c("LUX_max_day", "LUX_mean_day", "LUX_mean_spt", "LUX_mean_day_mvpa"); fi = fi + 4
      # time in LUX ranges
      Nluxt = length(params_247[["LUXthresholds"]])
      for (lti in 1:Nluxt) {
        if (lti < Nluxt) {
          dsummary[si, fi + lti - 1] =  length(which(ts$lightpeak[sse[ts$diur[sse] == 0]] >= params_247[["LUXthresholds"]][lti] &
                                                       ts$lightpeak[sse[ts$diur[sse] == 0]] < params_247[["LUXthresholds"]][lti + 1])) / (60/ws3new)
          ds_names[fi + lti - 1] = paste0("LUX_min_", params_247[["LUXthresholds"]][lti], "_", params_247[["LUXthresholds"]][lti + 1], "_day")
        } else {
          dsummary[si, fi + lti - 1] =  length(which(ts$lightpeak[sse[ts$diur[sse] == 0]] >= params_247[["LUXthresholds"]][lti])) / (60/ws3new)
          ds_names[fi + lti - 1] = paste0("LUX_min_", params_247[["LUXthresholds"]][lti], "_inf_day")
        }
      }
      fi = fi + Nluxt
      # LUX per segment of the day
      luxperseg = g.part5.lux_persegment(ts, sse,
                                         LUX_day_segments = params_247[["LUX_day_segments"]],
                                         epochSize = ws3new,
                                         desiredtz = params_general[["desiredtz"]])
      dsummary[si, fi:(fi + (length(luxperseg$values) - 1))] = luxperseg$values
      ds_names[fi:(fi + (length(luxperseg$values) - 1))] = luxperseg$names
      fi = fi + length(luxperseg$values)
    }

    #===============================================
    # FOLDER STRUCTURE
    if (params_output[["storefolderstructure"]] == TRUE) {
      if ("filename_dir" %in% ds_names) fi = which( ds_names == "filename_dir")
      dsummary[si,fi] = fullFilename #full filename structure
      ds_names[fi] = "filename_dir"; fi = fi + 1
      dsummary[si,fi] = foldernamei #store the lowest foldername
      if ("foldername" %in% ds_names) fi = which(ds_names == "foldername")
      ds_names[fi] = "foldername"; fi = fi + 1
    }
  } else {
    doNext = TRUE
  }
  # if skiponset and skipwake and "MM", then reset ts$diur
  if (skiponset == TRUE & skipwake == TRUE & timewindowi == "MM") {
    ts$diur = ts$diur_bu
    ts = ts[, -which(colnames(ts) == "diur_bu")]
  }
  # group categories of object back into lists
  indexlog = list(fileIndex = fileIndex,
                  winStartEnd = qqq,
                  segIndex1 = si,
                  segIndex2 = current_segment_i,
                  segStartEnd = c(segStart, segEnd),
                  columnIndex = fi)
  timeList = list(ts = ts,
                  epochSize = ws3new,
                  LEVELS = LEVELS,
                  Lnames = Lnames)
  invisible(list(
    indexlog = indexlog,
    ds_names = ds_names,
    dsummary = dsummary,
    timeList = timeList,
    doNext = doNext,
    add_one_day_to_next_date = add_one_day_to_next_date
  ))
}
