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
  
  #==========================
  # The following is to avoid issue with merging sleep variables from part 4
  # This code extract them the time series (ts) object create in g.part5
  # Note that this means that for MM windows there can be multiple or no wake or onsets
  date = as.Date(ts$time[segStart + 1], tz = params_general[["desiredtz"]])
  if (add_one_day_to_next_date == TRUE & timewindowi %in% c("WW", "OO")) { # see below for explanation
    date = date + 1
    add_one_day_to_next_date = FALSE
  }
  Sys.setlocale("LC_TIME", "C")
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
  # extract date and use this to retrieve corresponding part 4 information about the nights:
  options(encoding = "UTF-8")
  Sys.setlocale("LC_TIME", "C") # set language to English
  # look up matching part4 entry:
  recDates = as.Date(sumSleep$calendar_date, format = "%d/%m/%Y", origin = "1970-01-01")
  dsummary[si, fi] = sibDef
  ds_names[fi] = "sleepparam";      fi = fi + 1
  dayofinterst = which(recDates == date)
  if (length(dayofinterst) > 0) {
    dayofinterst = dayofinterst[1]
    dsummary[si,fi:(fi + 5)] = c(sumSleep$night[dayofinterst],
                                 sumSleep$daysleeper[dayofinterst],
                                 sumSleep$cleaningcode[dayofinterst],
                                 sumSleep$guider[dayofinterst],
                                 sumSleep$sleeplog_used[dayofinterst],
                                 sumSleep$acc_available[dayofinterst])
    ds_names[fi:(fi + 5)] = c("night_number", "daysleeper", "cleaningcode",
                              "guider", "sleeplog_used", "acc_available");      fi = fi + 6
    ts$guider[segStart:segEnd] = sumSleep$guider[dayofinterst] # add guider also to timeseries
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
  # qqq1 is the start of the day
  # qqq2 is the end of the day
  qqq1 = segStart
  qqq2 = segEnd
  # keep track of threshold value
  dsummary[si, fi:(fi + 2)] = c(TRLi, TRMi, TRVi)
  ds_names[fi:(fi + 2)] = c("TRLi", "TRMi", "TRVi")
  fi = fi + 3
  wlih = ((qqq2 - qqq1) + 1)/((60/ws3new) * 60)
  if (qqq1 > length(LEVELS)) qqq1 = length(LEVELS)
  sse = qqq1:qqq2
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
    #===============================================
    # TIME SPENT IN WINDOWS (window is either midnight-midnight or waking up-waking up)
    test_remember = c(si,fi)
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
    ds_names[fi] = "sleep_efficiency";      fi = fi + 1
    #===============================================
    # NAPS (estimation)
    if (params_output[["do.sibreport"]] == TRUE & "nap1_nonwear2" %in% colnames(ts) &
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
    #===============================================
    # QUANTILES...
    WLH = ((qqq2 - qqq1) + 1)/((60/ws3new) * 60)
    if (WLH <= 1) WLH = 1.001
    dsummary[si, fi] = quantile(ts$ACC[sse],probs = ((WLH - 1)/WLH), na.rm = TRUE)
    ds_names[fi] = paste("quantile_mostactive60min_mg", sep = "");      fi = fi + 1
    dsummary[si, fi] = quantile(ts$ACC[sse],probs = ((WLH - 0.5)/WLH), na.rm = TRUE)
    ds_names[fi] = paste("quantile_mostactive30min_mg", sep = "");      fi = fi + 1
    #===============================================
    # L5 M5, L10 M10...
    for (wini in params_247[["winhr"]]) {
      reso = params_247[["M5L5res"]] #resolution at 5 minutes
      endd = floor(WLH * 10) / 10 # rounding needed for non-integer window lengths
      nwindow_f = (endd - wini) #number of windows for L5M5 analyses
      ignore = FALSE
      if (endd <= wini | nwindow_f < 1) ignore = TRUE # day is shorter then time window, so ignore this
      nwindow_f = nwindow_f * (60/reso)
      if (ignore == FALSE) {
        # Calculate running window variables
        ACCrunwin = matrix(0, nwindow_f, 1)
        TIMErunwin = matrix("", nwindow_f, 1)
        for (hri in 0:floor((((endd - wini) * (60/reso)) - 1))) {
          e1 = (hri * reso * (60/ws3new)) + 1
          e2 = (hri + (wini * (60/reso))) * reso * (60/ws3new)
          if (e2 > length(sse)) e2 = length(sse)
          ACCrunwin[(hri + 1), 1] = mean(ts$ACC[sse[e1:e2]])
          TIMErunwin[(hri + 1), 1] = format(ts$time[sse[e1]])
        }
        ACCrunwin = ACCrunwin[is.na(ACCrunwin) == F]
        TIMErunwin = TIMErunwin[is.na(ACCrunwin) == F]
        if (length(ACCrunwin) > 0 & length(TIMErunwin) > 0) {
          # Derive day level variables
          L5HOUR = TIMErunwin[which(ACCrunwin == min(ACCrunwin))[1]]
          L5VALUE = min(ACCrunwin)
          M5HOUR = TIMErunwin[which(ACCrunwin == max(ACCrunwin))[1]]
          M5VALUE = max(ACCrunwin)
          if (lightpeak_available == TRUE) {
            if (length(unlist(strsplit(M5HOUR, " |T"))) == 1) M5HOUR = paste0(M5HOUR, " 00:00:00")
            startM5 = which(format(ts$time) == M5HOUR)
            M5_mean_peakLUX = round(mean(ts$lightpeak[startM5[1]:(startM5[1] + (wini*60*(60/ws3new)))], na.rm = TRUE), digits = 1)
            M5_max_peakLUX = round(max(ts$lightpeak[startM5[1]:(startM5[1] + (wini*60*(60/ws3new)))], na.rm = TRUE), digits = 1)
          }
        } else {
          L5HOUR = M5HOUR = "not detected"
          L5VALUE = M5VALUE = ""
          if (lightpeak_available == TRUE) {
            M5_mean_peakLUX = M5_max_peakLUX = ""
          }
        }
      }
      # Add variables calculated above to the output matrix
      if (ignore == FALSE) {
        dsummary[si, fi:(fi + 3)] = c(L5HOUR, L5VALUE, M5HOUR, M5VALUE)
      }
      ds_names[fi:(fi + 3)] = c(paste0("L", wini, "TIME"),
                                paste0("L", wini, "VALUE"),
                                paste0("M", wini, "TIME"),
                                paste0("M", wini, "VALUE"))
      fi = fi + 4
      if ("lightpeak" %in% colnames(ts)) {
        if (ignore == FALSE) {
          dsummary[si,fi] = M5_mean_peakLUX
          dsummary[si,fi + 1] = M5_max_peakLUX
        }
        ds_names[fi] = paste("M", wini, "_mean_peakLUX", sep = "")
        ds_names[fi + 1] = paste("M", wini,"_max_peakLUX", sep = "")
        fi = fi + 2
      }
      if (ignore == FALSE) {
        # Add also numeric time
        if (is.ISO8601(L5HOUR)) { # only do this for ISO8601 format
          L5HOUR = format(iso8601chartime2POSIX(L5HOUR, tz = params_general[["desiredtz"]]))
          M5HOUR = format(iso8601chartime2POSIX(M5HOUR, tz = params_general[["desiredtz"]]))
        }
        if (length(unlist(strsplit(L5HOUR," "))) == 1) L5HOUR = paste0(L5HOUR," 00:00:00") #added because on some OS timestamps are deleted for midnight
        if (length(unlist(strsplit(M5HOUR," "))) == 1) M5HOUR = paste0(M5HOUR," 00:00:00")
        if (L5HOUR != "not detected") {
          time_num = sum(as.numeric(unlist(strsplit(unlist(strsplit(L5HOUR," "))[2], ":"))) * c(3600, 60, 1)) / 3600
          dsummary[si,fi] = time_num
        } else {
          dsummary[si,fi] = NA
        }
      }
      ds_names[fi] = paste("L", wini, "TIME_num", sep = "");      fi = fi + 1
      if (ignore == FALSE) {
        if (M5HOUR != "not detected") {
          time_num = sum(as.numeric(unlist(strsplit(unlist(strsplit(M5HOUR," "))[2], ":"))) * c(3600, 60, 1)) / 3600
          dsummary[si, fi] = time_num
        } else {
          dsummary[si, fi] = NA
        }
      }
      ds_names[fi] = paste("M", wini, "TIME_num", sep = "");      fi = fi + 1
    }
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
      for (fragmode in c("day", "spt")) {
        frag.out = g.fragmentation(frag.metrics = params_phyact[["frag.metrics"]],
                                   LEVELS = LEVELS[sse[ts$diur[sse] == ifelse(fragmode == "day", 0, 1)]],
                                   Lnames = Lnames, xmin = 60/ws3new, mode = fragmode)
        # fragmentation values can come with a lot of decimal places
        dsummary[si, fi:(fi + (length(frag.out) - 1))] = round(as.numeric(frag.out), digits = 6)
        ds_names[fi:(fi + (length(frag.out) - 1))] = paste0("FRAG_", names(frag.out), "_", fragmode)
        fi = fi + length(frag.out)
      }
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
      if (timewindowi %in% c("WW", "OO")) {
        # LUX per segment of the day
        luxperseg = g.part5.lux_persegment(ts, sse,
                                           LUX_day_segments = params_247[["LUX_day_segments"]],
                                           epochSize = ws3new,
                                           desiredtz = params_general[["desiredtz"]])
        dsummary[si, fi:(fi + (length(luxperseg$values) - 1))] = luxperseg$values
        ds_names[fi:(fi + (length(luxperseg$values) - 1))] = luxperseg$names
        fi = fi + length(luxperseg$values)
      }
    }
    #=======================================================
    # nap/sib/nonwear overlap analysis
    #=======================================================
    if (params_output[["do.sibreport"]]  == TRUE & !is.null(sibreport))  {
      restAnalyses = g.part5.analyseRest(sibreport = sibreport, dsummary = dsummary,
                                         ds_names = ds_names, fi = fi, di = si,
                                         time = ts$time[sse[ts$diur[sse] == 0]],
                                         tz = params_general[["desiredtz"]],
                                         possible_nap_dur = params_sleep[["possible_nap_dur"]])
      fi = restAnalyses$fi
      si = restAnalyses$di
      dsummary = restAnalyses$dsummary
      ds_names = restAnalyses$ds_names
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
  # group categories of object back into lists
  indexlog = list(fileIndex = fileIndex,
                  winStartEnd = qqq,
                  segIndex1 = si,
                  segIndex2 = current_segment_i,
                  segStartEnd = c(segStart, segEnd),
                  columnIndex = fi)
  timeList = list(ts = ts,
                  epochSize = ws3new)
  invisible(list(
    indexlog = indexlog,
    ds_names = ds_names,
    dsummary = dsummary,
    timeList = timeList,
    doNext = doNext
  ))
}