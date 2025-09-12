g.part6 = function(datadir = c(), metadatadir = c(), f0 = c(), f1 = c(),
                   params_general = c(), params_phyact = c(), params_247 = c(),
                   params_cleaning = c(),
                   verbose = TRUE, ...) {
  
  # This function called by function GGIR
  # and aims to facilitate time-pattern analysis building on the labelled time
  # series derived in GGIR part 5
  #----------------------------------------------------------
  # Extract and check parameters
  input = list(...)
  params = extract_params(params_general = params_general,
                          params_phyact = params_phyact,
                          params_247 = params_247,
                          input = input, params2check = c("general", "phyact", "247"))
  params_general = params$params_general
  params_phyact = params$params_phyact
  params_247 = params$params_247
  
  #======================================================================
  # create new folder (if not existent) for storing milestone data
  ms6.out = "/meta/ms6.out"
  if (!file.exists(paste(metadatadir, ms6.out, sep = ""))) {
    dir.create(file.path(metadatadir, ms6.out))
  }
  
  #======================================================================
  # compile lists of milestone data filenames
  
  # Identify correct subfolder
  expected_ts_path = paste0(metadatadir, "/meta/ms5.outraw/", params_phyact[["part6_threshold_combi"]])
  expected_ms5raw_path = paste0(metadatadir, "/meta/ms5.outraw")
  if (!dir.exists(expected_ts_path)) {
    if (!dir.exists(expected_ms5raw_path)) {
      stop("\nPath ", expected_ms5raw_path, " does not exist", call. = FALSE)
    } else {
      subDirs = list.dirs(expected_ms5raw_path)
      if (length(subDirs) > 0) {
        expected_ts_path = paste0(expected_ms5raw_path, "/", subDirs[1])
        warning(paste0("\nThreshold combi ", params_phyact[["part6_threshold_combi"]],
                       " in time series ouput. Instead now using", subDirs[1]), call. = FALSE)
      } else {
        stop(paste0("\nNo subfolders found inside ", expected_ms5raw_path), call. = FALSE)
      }
    }
  }
  fnames.ms5raw = dir(expected_ts_path)
  # It can be that the use stored rdata and csv files
  # only use rdata in that case
  getExt = function(x) {
    tmp = unlist(strsplit(basename(x), "\\."))
    return(tmp[length(tmp)])
  }
  EXT = unlist(lapply(fnames.ms5raw, FUN = getExt))
  uniqueEXT = unique(EXT)
  if (length(uniqueEXT) == 2) {
    fnames.ms5raw = fnames.ms5raw[which(EXT == "RData")]
    EXT = EXT[which(EXT == "RData")]
  }
  EXT = EXT[1]
  # Identify existing part 6 milestone data
  fnames.ms6 = dir(paste(metadatadir, "/meta/ms6.out", sep = ""))
  ffdone = fnames.ms6
  #------------------------------------------------
  # specify parameters
  N5 = length(fnames.ms5raw)
  if (length(f0) == 0) f0 = 1
  if (length(f1) == 0) f1 = N5
  
  if (f0 > N5) {
    stop("Argument f0 is larger than the number of files in meta/ms5.outraw.", call. = FALSE)
  }
  if (f1 > N5 | f1 == 0) f1 = N5
  if (f0 == 0) f0 = 1
  
  
  resultsdir = paste0(metadatadir , "/results")
  if (!dir.exists(resultsdir)) dir.create(resultsdir)
  
  #=========================================================
  # Recording-group (e.g. household members) level co-analysis,
  # which at the end of this g.part6 is either
  if (params_247[["part6HCA"]] == TRUE) {
    part6AlignIndividuals(GGIR_ts_dir = expected_ts_path,
                          outputdir = resultsdir,
                          path_ggirms = paste0(metadatadir , "/meta"),
                          desiredtz = params_general[["desiredtz"]],
                          verbose = verbose)
    
    part6PairwiseAggregation(outputdir = paste0(metadatadir, "/results"),
                             desiredtz = params_general[["desiredtz"]],
                             verbose = verbose)
  }
  
  #=========================================================
  # Declare recording level functionality, which at the end of this g.part6 is either
  # applied to the file in parallel with foreach or serially with a loop
  main_part6_recordlevel = function(i, metadatadir = c(), f0 = c(), f1 = c(),
                                    fnames.ms5raw, ffdone, EXT, verbose) {
    if (length(ffdone) > 0) {
      if (length(which(ffdone == fnames.ms5raw[i])) > 0) {
        skip = 1 #skip this file because it was analysed before")
      } else {
        skip = 0 #do not skip this file
      }
    } else {
      skip = 0
    }
    if (params_general[["overwrite"]] == TRUE) skip = 0
    Lnames = cosinor_coef = mdat = desiredtz_part1 = NULL
    if (skip == 0) {
      # Load time series:
      if (EXT == "RData") {
        load(file = paste0(metadatadir, "/meta/ms5.outraw/",
                           params_phyact[["part6_threshold_combi"]], "/", fnames.ms5raw[i]))
        if (!is.null(desiredtz_part1)) {
          params_general[["desiredtz"]] = desiredtz_part1
        }
        if (is.null(Lnames)) stop("Part 5 was processed with an older version of GGIR, reprocess part 5")
        mdat$time = mdat$timestamp # duplicate column because cosinor function expect columntime
      } else {
        # use behavioural codes files to derive Lnames object (names of behavioural classes)
        legendfile = dir(path = paste0(metadatadir, "/meta/ms5.outraw"), pattern = "behavioralcodes", full.names = TRUE)
        if (length(legendfile) > 1) {
          df <- file.info(legendfile)
          legendfile = rownames(df)[which.max(df$mtime)]
        } else if (length(legendfile) == 0) {
          stop(paste0("behaviouralcodes file could not be found in ",
                      paste0(metadatadir, "/meta/ms5.outraw")))
        }
        Lnames = data.table::fread(file = legendfile)$class_name
        mdat = data.table::fread(file = paste0(metadatadir, "/meta/ms5.outraw/", 
                                               params_phyact[["part6_threshold_combi"]],  "/", fnames.ms5raw[i]), data.table = FALSE)
        filename = fnames.ms5raw[i]
      }
      nfeatures = 200
      summary = matrix(NA, nfeatures, 1)
      s_names = rep("", nfeatures)
      fi = 1
      deltatime = diff(mdat$timenum)
      epochSize = min(deltatime[which(deltatime != 0)])
      if (any(deltatime > epochSize)) {
        imputeTimeGaps = function(mdat, epochSize) {
          # impute timegaps
          mdat$gap = 1
          mdat$gap[1:(nrow(mdat) - 1)] = diff(mdat$timenum) / epochSize
          gapp = which(mdat$gap != 1)
          if (length(gapp) > 0) {
            if (gapp[1] > 1) {
              newTime = mdat$timenum[1:(gapp[1] - 1)]
            } else {
              newTime = NULL
            }
            for (g in 1:length(gapp)) {
              newTime = c(newTime, mdat$timenum[gapp[g]] + seq(0, by = epochSize, length.out = mdat$gap[gapp[g]]))
              if (g < length(gapp)) {
                newTime = c(newTime, mdat$timenum[(gapp[g] + 1):(gapp[g + 1] - 1)])
              }
            }
            newTime =  c(newTime, mdat$timenum[(gapp[g] + 1):length(mdat$timenum)])
          }
          mdat <- as.data.frame(lapply(mdat, rep, mdat$gap))
          invalid = duplicated(mdat)
          if (length(gapp) > 0) {
            mdat$timenum = newTime[1:nrow(mdat)]
          }
          mdat = mdat[, which(colnames(mdat) != "gap")]
          mdat$ACC[invalid] = NA
          mdat$class_id[invalid] = NA
          mdat$guider[invalid] = NA
          mdat$invalidepoch[invalid] = 1
          mdat$window[invalid] = 9999
          mdat$invalid_sleepperiod[invalid] = 100
          mdat$invalid_wakinghours[invalid] = 100
          mdat$time = mdat$timestamp = as.POSIXct(mdat$timenum,
                                                  tz =  params_general[["desiredtz"]],
                                                  origin = "1970-01-01")
          return(mdat)
        }
        mdat = imputeTimeGaps(mdat, epochSize)
      }
      # Select relevant section of the time series
      wakeuptimes = which(diff(c(1, mdat$SleepPeriodTime, 0)) == -1)
      onsettimes = which(diff(c(0, mdat$SleepPeriodTime, 1)) == 1) 
      Nmdat = nrow(mdat)
      if (wakeuptimes[length(wakeuptimes)] > Nmdat) wakeuptimes[length(wakeuptimes)] = Nmdat
      if (onsettimes[length(onsettimes)] > Nmdat) onsettimes[length(onsettimes)] = Nmdat
      getIndex = function(x, ts, wakeuptimes, onsettimes, epochSize) {
        if (x == "start") {
          y = 1 # simply the start of the time series
        } else if (x == "end") {
          y = nrow(ts) # simply the end of the time series
        } else if (substr(x, start = 1, stop = 1) == "W") {
          ni = as.numeric(substr(x, start = 2, stop = 5))
          if (ni > 0) { # nth Wakeup after the start
            y = wakeuptimes[ni]
          } else if (ni < 0) { # nth Wakeup before the end
            y = wakeuptimes[length(wakeuptimes) + ni + 1] # + because it already is -
          }
        } else if (substr(x, start = 1, stop = 1) == "O") {
          ni = as.numeric(substr(x, start = 2, stop = 5))
          if (ni > 0) { # nth Onset after the start
            y = onsettimes[ni]
          } else if (ni < 0) { # nth Onset before the end
            y = onsettimes[length(onsettimes) + ni + 1] # + because it already is -
          }
        } else if (substr(x, start = 1, stop = 1) == "H") {
          ni = as.numeric(substr(x, start = 2, stop = 5))
          if (ni > 0) { # nth Hour after the start
            y = round((ni * 3600) / epochSize)
          } else if (ni < 0) { # nth Hour before the end
            y = nrow(ts) - round((ni * 3600) / epochSize)
          }
        }
        return(y)
      }
      t0 = getIndex(params_247[["part6Window"]][1], ts = mdat, wakeuptimes, onsettimes, epochSize)
      t1 = getIndex(params_247[["part6Window"]][2], ts = mdat, wakeuptimes, onsettimes, epochSize)
      if (length(t1) == 1 && length(t0) == 1 && !is.na(t1) && !is.na(t0) && t1 > t0) {
        do.cr = TRUE
      } else {
        do.cr = FALSE
      }
      
      if (do.cr == TRUE) {
        ts = mdat[t0:t1, ]
        rm(mdat)
        ts = ts[which(ts$window != 0), ]
      } else {
        ts = mdat[1,]
      }
      
      # Set windows to missing that do not have enough valid data in either spt or day.
      # Note that columns invalid_wakinghours and invalid_sleepperiod here
      # are constants per window, we use this to identify the entire window as valid/invalid
      invalidWindows = which(ts$invalid_wakinghours > (1 - params_cleaning[["includecrit.part6"]][1]) * 100 | 
                               ts$invalid_sleepperiod > (1 - params_cleaning[["includecrit.part6"]][2]) * 100)
      if (length(invalidWindows) > 0) {
        ts[invalidWindows,c("class_id", "ACC")] = NA
        ts$invalidepoch[invalidWindows] = 1
      }
      # Include basic information in the summary
      summary[fi] = unlist(strsplit(fnames.ms5raw[i], "_"))[1]
      s_names[fi] = "ID"
      fi = fi + 1
      starttime = as.POSIXlt(ts$time[1], tz = params_general[["desiredtz"]],
                             origin = "1970-01-01")
      summary[fi] = format(starttime)
      s_names[fi] = "starttime"
      fi = fi + 1
      summary[fi] = gsub(pattern = "[.]RData$|[.]csv$", replacement = "", x = filename)
      s_names[fi] = "filename"
      fi = fi + 1
      summary[fi] = ifelse(test = nrow(ts) == 1,
                           yes = 0,
                           no = nrow(ts) / ((3600 * 24) / epochSize))
      s_names[fi] = "N_days"
      fi = fi + 1
      N_valid_days = ifelse(test = nrow(ts) == 1,
                            yes = 0,
                            no = length(which(ts$invalidepoch == 0)) / ((3600 * 24) / epochSize))
      summary[fi] = N_valid_days
      s_names[fi] = "N_valid_days"
      fi = fi + 1
      CountWeekDays = table(weekdays(ts$time[which(ts$invalidepoch == 0)]))
      CountWeekDays = CountWeekDays / (1440 * (60 / epochSize))
      CountWeekDays = CountWeekDays[which(CountWeekDays > 0.66)]
      if (length(CountWeekDays) > 0) {
        Nweekendays = length(which(names(CountWeekDays) %in% c("Saturday", "Sunday")))
      } else {
        Nweekendays = 0
      }
      summary[fi] = Nweekendays
      s_names[fi] = "N_valid_weekend_days"
      fi = fi + 1
      #=================================================================
      # Circadian rhythm analysis
      if (do.cr == TRUE & N_valid_days > 2) {
        #===============================================
        # Cosinor analysis which comes with IV IS estimtes
        # Note: applyCosinorAnalyses below uses column invalidepoch to turn
        # imputed values to NA.
        colnames(ts)[which(colnames(ts) == "timenum")] = "time"
        acc4cos = ts[, c("time", "ACC")]
        qcheck = ts$invalidepoch

        # Add missing value to complete an interger number of full days
        Na4c = nrow(acc4cos)
        NepochsPerDay = 24 * (3600 / epochSize)
        NepochsNeeded = ((ceiling(Na4c / NepochsPerDay) + 4) * NepochsPerDay - Na4c) + 1
        if (NepochsNeeded > 0)  {
          acc4cos[(Na4c + 1):(Na4c + NepochsNeeded), ] = NA
          acc4cos$time[Na4c:(Na4c + NepochsNeeded)] = seq(from = acc4cos$time[Na4c], by = epochSize, length.out = NepochsNeeded + 1)
          qcheck = c(qcheck, rep(1, NepochsNeeded))
        }

        threshold = as.numeric(unlist(strsplit( params_phyact[["part6_threshold_combi"]], "_"))[1])
        
        # extract nightsi again
        tempp = unclass(as.POSIXlt(acc4cos$time, tz = params_general[["desiredtz"]], origin = "1970-01-01"))
        sec = tempp$sec
        min = tempp$min
        hour = tempp$hour
        if (params_general[["dayborder"]] == 0) {
          nightsi = which(sec == 0 & min == 0 & hour == 0)
        } else {
          nightsi = which(sec == 0 & min == (params_general[["dayborder"]] - floor(params_general[["dayborder"]])) * 60 & hour == floor(params_general[["dayborder"]])) #shift the definition of midnight if required
        }
        cosinor_coef = apply_cosinor_IS_IV_Analyses(ts = acc4cos,
                                                    qcheck = qcheck,
                                                    midnightsi = nightsi,
                                                    epochsizes = rep(epochSize, 2),
                                                    threshold = threshold)
        rm(acc4cos)
        try(expr = {summary[fi:(fi + 6)] = c(cosinor_coef$timeOffsetHours,
                                             cosinor_coef$coef$params$mes,
                                             cosinor_coef$coef$params$amp,
                                             cosinor_coef$coef$params$acr,
                                             cosinor_coef$coef$params$acrotime,
                                             cosinor_coef$coef$params$ndays,
                                             cosinor_coef$coef$params$R2)},
            silent = TRUE)
        
        s_names[fi:(fi + 6)] = c("cosinor_timeOffsetHours", "cosinor_mes", "cosinor_amp", "cosinor_acrophase",
                                 "cosinor_acrotime", "cosinor_ndays", "cosinor_R2")
        fi = fi + 7
        try(expr = {summary[fi:(fi + 10)] = c(cosinor_coef$coefext$params$minimum,
                                              cosinor_coef$coefext$params$amp,
                                              cosinor_coef$coefext$params$alpha,
                                              cosinor_coef$coefext$params$beta,
                                              cosinor_coef$coefext$params$acrotime,
                                              cosinor_coef$coefext$params$UpMesor,
                                              cosinor_coef$coefext$params$DownMesor,
                                              cosinor_coef$coefext$params$MESOR,
                                              cosinor_coef$coefext$params$ndays,
                                              cosinor_coef$coefext$params$F_pseudo,
                                              cosinor_coef$coefext$params$R2)},
            silent = TRUE)
        s_names[fi:(fi + 10)] = c("cosinorExt_minimum", "cosinorExt_amp", "cosinorExt_alpha",
                                  "cosinorExt_beta", "cosinorExt_acrotime", "cosinorExt_UpMesor",
                                  "cosinorExt_DownMesor", "cosinorExt_MESOR",
                                  "cosinorExt_ndays", "cosinorExt_F_pseudo", "cosinorExt_R2")
        fi = fi + 11
        try(expr = {summary[fi:(fi + 2)] = c(cosinor_coef$IVIS$InterdailyStability,
                                             cosinor_coef$IVIS$IntradailyVariability,
                                             cosinor_coef$IVIS$phi)},
            silent = TRUE)
        s_names[fi:(fi + 2)] = c("IS", "IV", "phi")
        fi = fi + 3
        #===============================================
        # Transition probabilities
        for (fragmode in c("day", "spt")) {
          ts_temp = ts
          # turn other half of data to NA
          ts_temp$class_id[which(ts$SleepPeriodTime == ifelse(fragmode == "spt", yes = 0, no = 1))] = NA
          frag.out = g.fragmentation(frag.metrics = params_phyact[["frag.metrics"]],
                                     LEVELS = ts_temp$class_id,
                                     Lnames = Lnames, xmin = 60/epochSize, mode = fragmode)
          # fragmentation values can come with a lot of decimal places
          summary[fi:(fi + (length(frag.out) - 1))] = round(as.numeric(frag.out), digits = 6)
          s_names[fi:(fi + (length(frag.out) - 1))] = paste0("FRAG_", names(frag.out), "_", fragmode)
          fi = fi + length(frag.out)
        }
        
        #===============================================
        # LXMX: code copied from g.part 5
        # To be refactored as a central generic function once
        # new MXLX function is merged
        window_number = unique(ts$window)
        dsummary = matrix(NA, 100, length(params_247[["winhr"]]) * 80)
        ds_names = rep("", ncol(dsummary))
        lightpeak_available = "lightpeak" %in% names(ts)
        si = 1
        for (wi in window_number) {
          gi = 1
          sse = which(ts$window == wi)
          if (is.na(ts$ACC[sse[2]])) {
            next
          }
          ts$time_num = ts$time
          ts$time = ts$timestamp
          WLH = length(sse)/((60/epochSize) * 60)
          if (WLH <= 1) WLH = 1.001
          for (wini in params_247[["winhr"]]) {
            reso = params_247[["M5L5res"]] #resolution at 5 minutes
            endd = floor((WLH * 10) / 10) # rounding needed for non-integer window lengths
            nwindow_f = (endd - wini) #number of windows for L5M5 analyses
            ignore = FALSE
            if (endd <= wini | nwindow_f < 1) ignore = TRUE # day is shorter then time window, so ignore this
            nwindow_f = nwindow_f * (60/reso)
            if (ignore == FALSE) {
              # Calculate running window variables
              ACCrunwin = matrix(0, nwindow_f, 1)
              TIMErunwin = matrix("", nwindow_f, 1)
              for (hri in 0:floor((((endd - wini) * (60/reso)) - 1))) {
                e1 = (hri * reso * (60/epochSize)) + 1
                e2 = (hri + (wini * (60/reso))) * reso * (60/epochSize)
                if (e2 > length(sse)) e2 = length(sse)
                ACCrunwin[(hri + 1), 1] = mean(ts$ACC[sse[e1:e2]])
                TIMErunwin[(hri + 1), 1] = format(ts$time[sse[e1]])
              }
              ACCrunwin = ACCrunwin[is.na(ACCrunwin) == F]
              TIMErunwin = TIMErunwin[is.na(ACCrunwin) == F]
              if (length(ACCrunwin) > 0 & length(TIMErunwin) > 0) {
                # Derive day level variables
                L5VALUE = min(ACCrunwin)
                L5HOUR = TIMErunwin[which(ACCrunwin == L5VALUE)[1]]
                M5VALUE = max(ACCrunwin)
                M5HOUR = TIMErunwin[which(ACCrunwin == M5VALUE)[1]]
                if (lightpeak_available == TRUE) {
                  if (length(unlist(strsplit(M5HOUR, " |T"))) == 1) M5HOUR = paste0(M5HOUR, " 00:00:00")
                  startM5 = which(format(ts$time) == M5HOUR)
                  M5_mean_peakLUX = round(mean(ts$lightpeak[startM5[1]:(startM5[1] + (wini*60*(60/epochSize)))], na.rm = TRUE), digits = 1)
                  M5_max_peakLUX = round(max(ts$lightpeak[startM5[1]:(startM5[1] + (wini*60*(60/epochSize)))], na.rm = TRUE), digits = 1)
                }
              } else {
                L5HOUR = M5HOUR = "not detected"
                L5VALUE = M5VALUE = ""
                if (lightpeak_available == TRUE) {
                  M5_mean_peakLUX = M5_max_peakLUX = ""
                }
              }
              # Add variables calculated above to the output matrix
              dsummary[si, gi:(gi + 1)] = c(L5VALUE, M5VALUE)
            }
            ds_names[gi:(gi + 1)] = c(paste0("L", wini, "VALUE"),
                                      paste0("M", wini, "VALUE"))
            gi = gi + 2
            if ("lightpeak" %in% colnames(ts)) {
              if (ignore == FALSE) {
                dsummary[si,gi] = M5_mean_peakLUX
                dsummary[si,gi + 1] = M5_max_peakLUX
              }
              ds_names[gi] = paste("M", wini, "_mean_peakLUX", sep = "")
              ds_names[gi + 1] = paste("M", wini,"_max_peakLUX", sep = "")
              gi = gi + 2
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
                if (time_num <= 12) {
                  time_num = time_num + 24
                }
                dsummary[si,gi] = time_num
              } else {
                dsummary[si,gi] = NA
              }
            }
            ds_names[gi] = paste("L", wini, "TIME_num", sep = "");      gi = gi + 1
            if (ignore == FALSE) {
              if (M5HOUR != "not detected") {
                time_num = sum(as.numeric(unlist(strsplit(unlist(strsplit(M5HOUR," "))[2], ":"))) * c(3600, 60, 1)) / 3600
                dsummary[si, gi] = time_num
              } else {
                dsummary[si, gi] = NA
              }
            }
            ds_names[gi] = paste("M", wini, "TIME_num", sep = "");      gi = gi + 1
          }
          si = si + 1
          if (si > nrow(dsummary)) {
            dsummary[nrow(dsummary) + 1, ] = NA
            ds_names = c(ds_names, "")
          }
        }
        gi = which(ds_names == "")[1]
        dsummary = dsummary[1:(si - 1), 1:(gi - 1), drop = FALSE]
        ds_names = ds_names[1:(gi - 1)]
        dsummary = as.data.frame(x = dsummary)
        colnames(dsummary) = ds_names
        # aggregate across recording
        MXLXsummary = colMeans(x = dsummary, na.rm = TRUE)
        # Add to summary
        summary[fi:(fi + length(MXLXsummary) - 1)] = as.numeric(MXLXsummary)
        s_names[fi:(fi + length(MXLXsummary) - 1)] = names(MXLXsummary)
        fi = fi + length(MXLXsummary)
        # Translate times to clock time and add to summary
        cols2convert = grep(pattern = "TIME_num", x = names(MXLXsummary), value = FALSE)
        if (length(cols2convert) > 0) {
          for (cvi in cols2convert) {
            s_names[fi] = gsub(pattern = "_num", replacement = "_clock", x = names(MXLXsummary)[cvi])
            hr = as.numeric(floor(MXLXsummary[cvi]))
            min = as.numeric(floor((MXLXsummary[cvi] - hr) * 60))
            sec = as.numeric(floor((MXLXsummary[cvi] - hr - (min / 60)) * 3600))
            if (!is.na(hr) && !is.na(min) && !is.na(sec)) {
              if (hr >= 24) hr = hr - 24
              clock_time = paste0(hr, ":", min, ":", sec)
            } else {
              clock_time = NA
            }
            summary[fi] = clock_time 
            fi = fi + 1
          }
        }
        #=======================================================================
        # DFA analyses is used independent of do.cr because it is time consuming
        if (params_247[["part6DFA"]] == TRUE) {
          ssp = SSP(ts$ACC[!is.na(ts$ACC)])
          abi = ABI(ssp)
          summary[fi:(fi + 1)] = c(ssp, abi)
          s_names[fi:(fi + 1)] = c("SSP", "ABI")
          fi = fi + 2
        }
        #------------------------------------------------------------
        # Sleep Regularity Index
        if (!is.null(params_247[["SRI2_WASOmin"]])) {
          sleepnap_classid = grep(pattern = "spt_sleep|_nap", x = Lnames) - 1
          ts$sleepnap = 0
          ts$sleepnap[which(ts$class_id %in% sleepnap_classid)] = 1
          # remove short lasting 'WASO' based on WASOmin parameters
          waso_classid = grep(pattern = "spt_wake", x = Lnames) - 1
          waso = rep(0, nrow(ts))
          waso[which(ts$class_id %in% waso_classid)] = 1
          rlew = rle(waso)
          shortwaso = which(rlew$values == 1 & (rlew$lengths / (60/epochSize)) <= params_247[["SRI2_WASOmin"]])
          if (length(shortwaso) > 0) {
            rlew$values[shortwaso] = 2
            waso = rep(rlew$values, rlew$lengths)
            if (2 %in% rlew$values) {
              ts$sleepnap[which(waso == 2)] = 1
            }
          }
          SRI = CalcSleepRegularityIndex(data = ts,
                                         epochsize = epochSize,
                                         desiredtz = params_general[["desiredtz"]])
          
          SRI = SRI[which(SRI$frac_valid > params_cleaning[["includecrit.part6"]][1]), ]
          if (nrow(SRI) > 0) {
            summary[fi] = weighted.mean(x = SRI$SleepRegularityIndex, w = SRI$frac_valid)
            summary[fi + 1] = nrow(SRI)
          } else {
            summary[fi:(fi + 1)] = NA
          }
          s_names[fi:(fi + 1)] = c("SleepRegularityIndex2", "SleepRegularityIndex2_Ndaypairs")
          fi = fi + 2
          rm(rlew, shortwaso, waso)
        }
      }
      #=============================================
      # Store results in milestone data
      summary = summary[1:(fi - 1),]
      s_names = s_names[1:(fi - 1)]
      summary = summary[which(s_names != "")]
      s_names = s_names[which(s_names != "")]
      output_part6 = data.frame(t(summary), stringsAsFactors = FALSE)
      names(output_part6) = s_names
      nonnumeric = grep(pattern = "TIME_clock", x = s_names, invert = TRUE)
      nonnumeric = nonnumeric[nonnumeric %in% 1:3 == FALSE]
      output_part6[, nonnumeric] = as.numeric(output_part6[, nonnumeric])
      if (length(output_part6) > 0) {
        if (length(cosinor_coef) > 0) {
          cosinor_ts = cosinor_coef$coefext$cosinor_ts
        } else {
          cosinor_ts = c()
        }
        GGIRversion = "GGIR not used"
        if (is.element('GGIR', installed.packages()[,1])) {
          GGIRversion = as.character(utils::packageVersion("GGIR"))
          if (length(GGIRversion) != 1) GGIRversion = sessionInfo()$otherPkgs$GGIR$Version
        }
        output_part6$GGIRversion = GGIRversion
        save(output_part6, cosinor_ts, GGIRversion, file = paste0(metadatadir,
                                                                  ms6.out, "/", gsub(pattern = "[.]csv|[.]RData",
                                                                                     replacement = "",
                                                                                     x = fnames.ms5raw[i]), ".RData"))
      }
      rm(output_part6, summary)
    }
    # Function has no output_part6 because ideally all relevant output_part6
    # is stored in milestone data by now
  }
  
  if (params_247[["part6CR"]] == TRUE) {
    #======================================================================
    # loop through milestone data-files or filenames stored in output of g.part5
    # setup parallel backend to use many processors
    if (params_general[["do.parallel"]] == TRUE) {
      cores = parallel::detectCores()
      Ncores = cores[1]
      if (Ncores > 3) {
        if (length(params_general[["maxNcores"]]) == 0) params_general[["maxNcores"]] = Ncores
        Ncores2use = min(c(Ncores - 1, params_general[["maxNcores"]], (f1 - f0) + 1))
        if (Ncores2use > 1) {
          cl <- parallel::makeCluster(Ncores2use) # not to overload your computer
          parallel::clusterExport(cl = cl, 
                                  varlist = c(unclass(lsf.str(envir = asNamespace("GGIR"), all = T)),
                                              "MONITOR", "FORMAT"),
                                  envir = as.environment(asNamespace("GGIR"))
          )
          parallel::clusterEvalQ(cl, Sys.setlocale("LC_TIME", "C"))
          doParallel::registerDoParallel(cl)
        } else {
          # Don't process in parallel if only one core
          params_general[["do.parallel"]] = FALSE
        }
      } else {
        if (verbose == TRUE) cat(paste0("\nparallel processing not possible because number of available cores (",Ncores,") < 4"))
        params_general[["do.parallel"]] = FALSE
      }
    }
    if (params_general[["do.parallel"]] == TRUE) {
      if (verbose == TRUE) cat(paste0('\n Busy processing ... see ', metadatadir, ms6.out, ' for progress\n'))
      # check whether we are in development mode:
      GGIRinstalled = is.element('GGIR', installed.packages()[,1])
      packages2passon = functions2passon = NULL
      GGIRloaded = "GGIR" %in% .packages()
      if (GGIRloaded) { #pass on package
        packages2passon = 'GGIR'
        errhand = 'pass'
      } else {
        # pass on functions
        functions2passon = c("cosinor_IS_IV_Analyses", "apply_cosinor_IS_IV_Analyses", "g.IVIS")
        errhand = 'stop'
      }
      i = 0 # declare i because foreach uses it, without declaring it
      `%myinfix%` = foreach::`%dopar%`
      output_list = foreach::foreach(i = f0:f1,  .packages = packages2passon,
                                     .export = functions2passon, .errorhandling = errhand) %myinfix% {
                                       tryCatchResult = tryCatch({
                                         main_part6_recordlevel(i, metadatadir, f0, f1,
                                                                fnames.ms5raw, ffdone, EXT, verbose)
                                       })
                                       return(tryCatchResult)
                                     }
      on.exit(parallel::stopCluster(cl))
      for (oli in 1:length(output_list)) { # logged error and warning messages
        if (is.null(unlist(output_list[oli])) == FALSE) {
          if (verbose == TRUE) cat(paste0("\nErrors and warnings for ", fnames.ms5raw[oli]))
          print(unlist(output_list[oli])) # print any error and warnings observed
        }
      }
    } else {
      errors = list()
      for (i in f0:f1) {
        if (verbose == TRUE) cat(paste0(i, " "))
        function_to_evaluate = expression(
          main_part6_recordlevel(i, metadatadir, f0, f1,
                                 fnames.ms5raw, ffdone, EXT, verbose)
        )
        if (params_general[["use_trycatch_serial"]] == TRUE) {
          tryCatch(
            eval(function_to_evaluate),
            error = function(e) {
              err_msg = conditionMessage(e)
              errors[[as.character(fnames.ms5raw[i])]] <<- err_msg
            }
          )
        } else {
          eval(function_to_evaluate)
        }
      }
      # show logged errors after the loop:
      if (params_general[["use_trycatch_serial"]] == TRUE)  {
        if (length(errors) > 0) {
          cat(paste0("\n\nErrors in part 6... for:"))
          for (e in 1:length(errors)) {
            cat(paste0("\n- ", names(errors)[e], ": ", errors[[e]]))
          }
        }
      }
    }
  }
}
