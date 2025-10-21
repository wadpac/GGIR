g.part5 = function(datadir = c(), metadatadir = c(), f0=c(), f1=c(),
                   params_sleep = c(), params_metrics = c(),
                   params_247 = c(), params_phyact = c(),
                   params_cleaning = c(), params_output = c(),
                   params_general = c(), verbose = TRUE, ...) {
  options(encoding = "UTF-8")
  filename_dir = NULL
  # This function called by function GGIR
  # and aims to combine all the milestone output from the previous parts
  # in order to facilitate a varierty of analysis on time-use, interactions
  # between day and night activity, and circadian rhythms
  #----------------------------------------------------------
  # Extract and check parameters
  input = list(...)
  params = extract_params(params_sleep = params_sleep,
                          params_metrics = params_metrics,
                          params_247 = params_247,
                          params_phyact = params_phyact,
                          params_cleaning = params_cleaning,
                          params_output = params_output,
                          params_general = params_general,
                          input = input, params2check = c("sleep", "metrics", "247",
                                                          "phyact", "cleaning",
                                                          "general", "output"))
  params_sleep = params$params_sleep
  params_metrics = params$params_metrics
  params_247 = params$params_247
  params_phyact = params$params_phyact
  params_cleaning = params$params_cleaning
  params_output = params$params_output
  params_general = params$params_general
  #======================================================================
  # create new folder (if not existent) for storing milestone data
  checkMilestoneFolders(metadatadir, partNumber = 5)
  ms5.out = "/meta/ms5.out"
  if (params_output[["save_ms5rawlevels"]] == TRUE | params_output[["do.sibreport"]] == TRUE) {
    ms5.outraw = "/meta/ms5.outraw"
    if (file.exists(paste(metadatadir, ms5.outraw, sep = ""))) {
    } else {
      dir.create(file.path(metadatadir, ms5.outraw))
    }
    if (params_output[["save_ms5rawlevels"]] == TRUE) {
      # Create on subfolder per configuration
      configurations = c()
      for (TRLi in params_phyact[["threshold.lig"]]) {
        for (TRMi in params_phyact[["threshold.mod"]]) {
          for (TRVi in params_phyact[["threshold.vig"]]) {
            configurations = c(configurations, paste0(TRLi, "_", TRMi, "_", TRVi))
          }
        }
      }
      for (hi in configurations) {
        folder2create = paste0(metadatadir, ms5.outraw, "/", hi)
        if (dir.exists(folder2create) == FALSE) {
          dir.create(path = folder2create)
        }
      }
    }
  }
  nightsummary = M = IMP = sib.cla.sum = c() # declaring variable as otherwise R is confused where they come from, while in fact they are loaded as part of the load operations
  #======================================================================
  # compile lists of milestone data filenames
  fnames.ms3 = dir(paste(metadatadir, "/meta/ms3.out", sep = ""))
  
  fnames.ms5 = dir(paste(metadatadir, "/meta/ms5.out", sep = ""))
  # path to sleeplog milestonedata, if it exists:
  
  if (length(params_sleep[["loglocation"]]) > 0) {
    sleeplogRDA = paste0(metadatadir,"/meta/sleeplog_", basename(params_sleep[["loglocation"]]), ".RData")
  } else {
    sleeplogRDA = paste(metadatadir, "/meta/sleeplog.RData", sep = "")
  }
  if (file.exists(sleeplogRDA) == TRUE) {
    sleeplog = logs_diaries = c()
    load(sleeplogRDA)
    if (length(logs_diaries) > 0) {# new format
      if (is.list(logs_diaries)) { # advanced format
        if (params_sleep[["sleepwindowType"]] == "TimeInBed" && length(logs_diaries$bedlog) > 0) {
          sleeplog = logs_diaries$bedlog
        } else {
          sleeplog = logs_diaries$sleeplog
        }
      } else {
        sleeplog = logs_diaries
      }
    }
  } else {
    sleeplog = logs_diaries = c()
  }
  # Extract activity diary if applicable
  if (is.character(params_247[["qwindow"]])) {
    if (length(grep(pattern = "onlyfilter|filteronly", x = params_247[["qwindow"]])) == 0) {
      epochSize_tmp = ifelse(params_general[["part5_agg2_60seconds"]],
                             yes = 60, no = params_general[["windowsizes"]][1])
      params_247[["qwindow"]] = g.conv.actlog(params_247[["qwindow"]],
                                              params_247[["qwindow_dateformat"]],
                                              epochSize = epochSize_tmp)
      # This will be an object with numeric qwindow values for all individuals and days
    } else {
      # ignore the diary specified by qwindow because user only want to use
      # it for filtering night time nonwear in part 2, but not as a way to
      # do day segment analysis.
      params_247[["qwindow"]] = c(0, 24)
    }
  }
  #------------------------------------------------
  # specify parameters
  ffdone = fnames.ms5 #ffdone is now a list of files that have already been processed by g.part5
  if (f1 > length(fnames.ms3)) f1 = length(fnames.ms3) # this is intentionally ms3 and not ms4, do not change!
  params_phyact[["boutdur.mvpa"]] = sort(params_phyact[["boutdur.mvpa"]],decreasing = TRUE)
  params_phyact[["boutdur.lig"]] = sort(params_phyact[["boutdur.lig"]],decreasing = TRUE)
  params_phyact[["boutdur.in"]] = sort(params_phyact[["boutdur.in"]],decreasing = TRUE)
  #--------------------------------
  # get full file path and folder name if requested by end-user and keep this for storage in output
  if (params_output[["storefolderstructure"]] == TRUE && dir.exists(datadir)) {
    extractfilenames = function(x) as.character(unlist(strsplit(x,".RDa"))[1])
    referencefnames = sapply(fnames.ms3,extractfilenames)
    folderstructure = getfolderstructure(datadir,referencefnames)
    fullfilenames = folderstructure$fullfilenames
    foldername = folderstructure$foldername
  } else {
    referencefnames = fullfilenames = gsub(pattern = "[.]RData", replacement = "", x = fnames.ms3)
    foldername = rep("", length(fnames.ms3))
  }
  if (f0 > length(fnames.ms3)) f0 = 1
  if (f1 == 0 | length(f1) == 0 | f1 > length(fnames.ms3))  f1 = length(fnames.ms3)
  #=========================================================
  # Declare core functionality, which at the end of this g.part5 is either
  # applied to the file in parallel with foreach or serially with a loop
  main_part5 = function(i, metadatadir = c(), f0=c(), f1=c(),
                        params_sleep = c(), params_metrics = c(),
                        params_247 = c(), params_phyact = c(),
                        params_cleaning = c(), params_output = c(),
                        params_general = c(), ms5.out, ms5.outraw,
                        fnames.ms3, sleeplog, logs_diaries,
                        referencefnames, folderstructure,
                        fullfilenames, foldername, ffdone, verbose) {
    tail_expansion_log =  desiredtz_part1 = NULL
    filename_dir = NULL # to be loaded
    fnames.ms1 = dir(paste(metadatadir, "/meta/basic", sep = ""))
    fnames.ms2 = dir(paste(metadatadir, "/meta/ms2.out", sep = ""))
    fnames.ms4 = dir(paste(metadatadir, "/meta/ms4.out", sep = ""))
    nfeatures = 500
    ws3 = params_general[["windowsizes"]][1]
    ds_names = rep("",nfeatures)
    di = 1
    DaCleanFile = c()
    if (length(params_cleaning[["data_cleaning_file"]]) > 0) {
      if (file.exists(params_cleaning[["data_cleaning_file"]])) DaCleanFile = data.table::fread(params_cleaning[["data_cleaning_file"]], data.table = FALSE)
    }
    if (length(ffdone) > 0) {
      if (length(which(ffdone == fnames.ms3[i])) > 0) {
        skip = 1 #skip this file because it was analysed before")
      } else {
        skip = 0 #do not skip this file
      }
    } else {
      skip = 0
    }
    if (params_general[["overwrite"]] == TRUE) skip = 0
    # skip files from ms3 if there is no equivalent in ms4
    selp = which(fnames.ms4 == fnames.ms3[i])
    if (length(selp) > 0) {
      if (file.exists(paste0(metadatadir, "/meta/ms4.out/", fnames.ms4[selp])) == FALSE) {
        skip = 1
      }
    } else {
      skip = 1
    }
    if (skip == 0) {
      # load output g.part2
      selp = which(fnames.ms2 == fnames.ms3[i]) # so, fnames.ms3[i] is the reference point for filenames
      load(file = paste0(metadatadir, "/meta/ms2.out/", fnames.ms2[selp]))
      # load output g.part4
      selp = which(fnames.ms4 == fnames.ms3[i])
      load(file = paste0(metadatadir,"/meta/ms4.out/", fnames.ms4[selp]))
      summarysleep = nightsummary
      rm(nightsummary)
      idindex = which(summarysleep$filename == fnames.ms3[i])
      ID = summarysleep$ID[idindex[1]]
      ndays = nrow(summarysleep) #/ length(unique(summarysleep$sleepparam))
      dsummary = matrix("", ((nrow(summarysleep) + 12) * length(unique(summarysleep$sleepparam))
                             * length(unique(params_phyact[["threshold.lig"]]))
                             * length(unique(params_phyact[["threshold.mod"]]))
                             * length(unique(params_phyact[["threshold.vig"]]))
                             * length(unique(params_output[["timewindow"]]))), nfeatures)
      di = 1
      fi = 1
      SPTE_end = c() # if it is not loaded from part3 milestone data then this will be the default
      # Only attempt to load file if it has at least 1 night of data
      if (length(idindex) > 0 & nrow(summarysleep) > 0) { 
        summarysleep_tmp = summarysleep
        #======================================================================
        # load output g.part1
        selp = which(fnames.ms1 == paste0("meta_", fnames.ms3[i]))
        if (length(selp) != 1) {
          if (verbose == TRUE) cat("Warning: Milestone data part 1 could not be retrieved")
        }
        load(paste0(metadatadir, "/meta/basic/", fnames.ms1[selp]))
        if (!is.null(desiredtz_part1)) {
          params_general[["desiredtz"]] = desiredtz_part1
        }
        # convert to character/numeric if stored as factor in metashort and metalong
        M$metashort = correctOlderMilestoneData(M$metashort)
        M$metalong = correctOlderMilestoneData(M$metalong)
        filename = filename_dir # object comes from load() call above
        # load output g.part3
        longitudinal_axis = NULL # initialise var that is part of ms3.out
        load(paste0(metadatadir, "/meta/ms3.out/", fnames.ms3[i]))
        # remove expanded time so that it is not used for behavioral classification
        if (length(tail_expansion_log) != 0) {
          expanded_short = which(IMP$r5long == -1)
          expanded_long = which(IMP$rout$r5 == -1)
          IMP$metashort = IMP$metashort[-expanded_short,]
          IMP$rout = IMP$rout[-expanded_long,]
          M$metashort = M$metashort[-expanded_short,]
          M$metalong = M$metalong[-expanded_long,]
        }
        
        #====================================
        # Initialise time series data.frame (ts) which will hold the time series
        # which forms the center of all part 5 activity
        # note longitudinal_axis comes from loaded part 3 milestone data and not from params_sleep
        
        ts = g.part5_initialise_ts(IMP, M, params_247, params_general,
                                   longitudinal_axis = longitudinal_axis)
        Nts = nrow(ts)
        lightpeak_available = "lightpeak" %in% names(ts)
        
        rm(IMP, M ,I)
        clock2numtime = function(x) { # function used for converting sleeplog times to hour times
          x2 = as.numeric(unlist(strsplit(x, ":"))) / c(1, 60, 3600)
          return(sum(x2))
        }
        Nepochsinhour = (60/ws3) * 60
        #=======================
        # extract epoch by epoch classification of the entire measurement (day specific summaries will follow further down)
        S = sib.cla.sum
        rm(sib.cla.sum)
        def = unique(S$definition)
        cut = which(S$fraction.night.invalid > 0.9 | S$nsib.periods == 0)
        if (length(cut) > 0) S = S[-cut,]
        if (params_general[["part5_agg2_60seconds"]] == TRUE) {
          ts_backup = ts
        }
        # Remove impossible entries:
        pko = which(summarysleep_tmp$sleeponset == 0 & summarysleep_tmp$wakeup == 0 & summarysleep_tmp$SptDuration == 0)
        if (length(pko) > 0) {
          summarysleep_tmp = summarysleep_tmp[-pko,]
        }
        # if expanded time with expand_tail_max_hours, then latest wakeup might not be in data
        # reset latest wakeup to latest observed timestamp
        if (length(tail_expansion_log) != 0) {
          last_night = S[which(S$night == max(S$night)),]
          last_wakeup = last_night$sib.end.time[which(last_night$sib.period == max(last_night$sib.period))]
          if (!(last_wakeup %in% ts$time)) {
            replaceLastWakeup = which(S$sib.end.time == last_wakeup)
            S$sib.end.time[replaceLastWakeup] = ts$time[nrow(ts)]
          }
        }
        for (sibDef in def) { # loop through sleep definitions (defined by angle and time threshold in g.part3)
          ws3new = ws3 # reset wse3new, because if part5_agg2_60seconds is TRUE then this will have been change in the previous iteration of the loop
          if (params_general[["part5_agg2_60seconds"]] == TRUE) {
            ts = ts_backup
          }
          # extract time and from that the indices for midnights
          time_POSIX = iso8601chartime2POSIX(ts$time,tz = params_general[["desiredtz"]])
          tempp = as.POSIXlt(time_POSIX) #unclass(time_POSIX)
          if (is.na(tempp$sec[1]) == TRUE) {
            time_POSIX = as.POSIXct(ts$time, tz = params_general[["desiredtz"]])
            tempp = as.POSIXlt(time_POSIX)
          }
          sec = tempp$sec
          min = tempp$min
          hour = tempp$hour
          if (params_general[["dayborder"]] == 0) {
            nightsi = which(sec == 0 & min == 0 & hour == 0)
            nightsi2 = nightsi # nightsi2 will be used in g.part5.wakesleepwindows
          } else {
            nightsi = which(sec == 0 & min == (params_general[["dayborder"]] - floor(params_general[["dayborder"]])) * 60 & hour == floor(params_general[["dayborder"]])) #shift the definition of midnight if required
            nightsi2 = which(sec == 0 & min == 0 & hour == 0)
          }
          # include last window if has been expanded and not present in ts
          if (length(tail_expansion_log) != 0 && nrow(ts) > max(nightsi)) nightsi[length(nightsi) + 1] = nrow(ts)
          # create copy of only relevant part of sleep summary dataframe
          summarysleep_tmp2 = summarysleep_tmp[which(summarysleep_tmp$sleepparam == sibDef),]
          # Add sustained inactivity bouts (sib) to the time series
          ts = g.part5.addsib(ts,
                              epochSize = ws3new,
                              part3_output = S[S$definition == sibDef,],
                              desiredtz = params_general[["desiredtz"]],
                              sibDefinition = sibDef,
                              nightsi)
          if (nrow(summarysleep_tmp2) > 0) {
            if (!all(is.na(summarysleep_tmp$sleepparam))) {
              # Fix missing nights in part 4 data:
              summarysleep_tmp2 = g.part5.fixmissingnight(summarysleep_tmp2, sleeplog = sleeplog, ID)
            } else {
              summarysleep_tmp2 = summarysleep_tmp2[-which(is.na(summarysleep_tmp$sleepparam)), ]
            }
          }
          #Initialise diur variable, which will  indicate the diurnal rhythm: 0 if wake/daytime, 1 if sleep/nighttime
          ts$diur = 0
          if (nrow(summarysleep_tmp2) > 0) {
            # Add defenition of wake and sleep windows in diur column of data.frame ts
            ts = g.part5.wakesleepwindows(ts,
                                          part4_output = summarysleep_tmp2,
                                          desiredtz = params_general[["desiredtz"]],
                                          nightsi = nightsi2,
                                          sleeplog,
                                          epochSize = ws3, ID, Nepochsinhour)
            # Add first waking up time, if it is missing:
            ts = g.part5.addfirstwake(ts, summarysleep = summarysleep_tmp2, nightsi, sleeplog, ID,
                                      Nepochsinhour, SPTE_end)
            # Convert time column from iso8601 to POSIX regardless of whether it is aggregated
            # to ensure the format is consistent
            ts$time = iso8601chartime2POSIX(ts$time,tz = params_general[["desiredtz"]])
            if (params_general[["part5_agg2_60seconds"]] == TRUE) { # Optionally aggregate to 1 minute epoch:
              ts$time_num = floor(as.numeric(ts$time) / 60) * 60
              
              # only include angle if angle is present
              angleColName = grep(pattern = "angle", x = names(ts), value = TRUE)
              if (lightpeak_available == TRUE) {
                light_columns = c("lightpeak", "lightpeak_imputationcode")
              } else {
                light_columns = NULL
              }
              temperature_col = grep(pattern = "temperature", x = names(ts), value = TRUE)
              
              # aggregate steps by taking the sum
              stepcount_available = ifelse("step_count" %in% names(ts), yes = TRUE, no = FALSE)
              if (stepcount_available) {
                step_count_tmp = aggregate(ts$step_count, by = list(ts$time_num), FUN = function(x) sum(x))
                colnames(step_count_tmp)[2] = "step_count"
              }
              # aggregate guider names as the first value per time segment
              agg_guider = aggregate(ts$guider,
                                     by = list(ts$time_num), FUN = function(x) x[1])
              colnames(agg_guider)[2] = "guider"
              # aggregate all other variables by taking the mean
              ts = aggregate(ts[,c("ACC","sibdetection", "diur", "nonwear", angleColName, light_columns, temperature_col)],
                             by = list(ts$time_num), FUN = function(x) mean(x))
              ts = merge(x = ts, y = agg_guider, by = "Group.1")
              if (stepcount_available) {
                ts = merge(x = ts, y = step_count_tmp, by = "Group.1")
              }
              ts$sibdetection = round(ts$sibdetection)
              ts$diur = round(ts$diur)
              ts$nonwear = round(ts$nonwear)
              names(ts)[1] = "time"
              # # convert back to iso8601 format
              ts$time = as.POSIXct(ts$time, origin = "1970-1-1", tz = params_general[["desiredtz"]])
              ws3new = 60 # change because below it is used to decide how many epochs are there in
              # extract nightsi again
              time_POSIX = ts$time
              tempp = as.POSIXlt(time_POSIX) #unclass(time_POSIX)
              if (is.na(tempp$sec[1]) == TRUE) {
                time_POSIX = as.POSIXct(ts$time, tz = params_general[["desiredtz"]])
                tempp = as.POSIXlt(time_POSIX)
              }
              sec = tempp$sec
              min = tempp$min
              hour = tempp$hour
              if (params_general[["dayborder"]] == 0) {
                nightsi = which(sec == 0 & min == 0 & hour == 0)
              } else {
                nightsi = which(sec == 0 & min == (params_general[["dayborder"]] - floor(params_general[["dayborder"]])) * 60 & hour == floor(params_general[["dayborder"]])) #shift the definition of midnight if required
              }
              if (length(tail_expansion_log) != 0 & nrow(ts) > max(nightsi)) nightsi[length(nightsi) + 1] = nrow(ts) # include last window
              Nts = nrow(ts)
            }
            #===============================================
            # Use sib.report to classify naps, non-wear and integrate these in time series
            # does not depend on bout detection criteria or window definitions.
            if (params_output[["do.sibreport"]]  == TRUE) {
              IDtmp = as.character(ID)
              sibreport = g.sibreport(ts, ID = IDtmp, epochlength = ws3new, logs_diaries,
                                      desiredtz = params_general[["desiredtz"]])
              
              # Add self-reported classes to ts object
              ts$selfreported = NA
              
              if ("imputecode" %in% colnames(sibreport)) {
                if ("logImputationCode" %in% colnames(ts) == FALSE) {
                  ts$diaryImputationCode = NA # initialise value
                }
                addImputationCode = TRUE
              } else {
                addImputationCode = FALSE
              }
              for (srType in c("sleeplog", "nap", "nonwear", "bedlog")) {
                sr_index = which(sibreport$type == srType)
                if (length(sr_index) > 0) {
                  for (sii in sr_index) {
                    ts_index = which(ts$time >= sibreport$start[sii] & ts$time < sibreport$end[sii])
                    if (addImputationCode == TRUE && srType %in% c("sleeplog", "bedlog")) {
                      ts$diaryImputationCode[ts_index] = as.numeric(sibreport$imputecode[sii])
                    }
                    ts_index1 = ts_index[which(is.na(ts$selfreported[ts_index]))]
                    ts_index2 = ts_index[which(!is.na(ts$selfreported[ts_index]))]
                    if (length(ts_index1) > 0) {
                      ts$selfreported[ts_index1] = srType
                    }
                    if (length(ts_index2) > 0) {
                      ts$selfreported[ts_index2] = paste0(ts$selfreported[ts_index2], "+", srType)
                    }
                  }
                }
              }
              ts$selfreported = as.factor(ts$selfreported)
              # store in csv file:
              ms5.sibreport = "/meta/ms5.outraw/sib.reports"
              if (!file.exists(paste(metadatadir, ms5.sibreport, sep = ""))) {
                dir.create(file.path(metadatadir, ms5.sibreport))
              }
              shortendFname = gsub(pattern = "[.]|RData|csv|cwa|bin", replacement = "", x = fnames.ms3[i], ignore.case = TRUE)
              sibreport_fname =  paste0(metadatadir,ms5.sibreport,"/sib_report_", shortendFname, "_",sibDef,".csv")
              if (length(sibreport) > 0) {
                data.table::fwrite(x = sibreport, file = sibreport_fname, row.names = FALSE,
                                   sep = params_output[["sep_reports"]],
                                   dec = params_output[["dec_reports"]],
                                   dateTimeAs = "write.csv")
              }
              addNapOut = g.part5.addNaps(sibreport = sibreport, ts = ts,
                                          params_general = params_general,
                                          params_sleep = params_sleep,
                                          params_phyact = params_phyact)
              ts = addNapOut$ts
              sibreport = addNapOut$sibreport
              long_nap_boutsi = addNapOut$long_nap_boutsi
            } else {
              sibreport = NULL
              long_nap_boutsi = NULL
            }
            # backup of nightsi outside threshold defintions to avoid
            # overwriting the backup after the first iteration
            nightsi_bu = nightsi
            for (TRLi in params_phyact[["threshold.lig"]]) {
              for (TRMi in params_phyact[["threshold.mod"]]) {
                for (TRVi in params_phyact[["threshold.vig"]]) {
                  # derive behavioral levels (class), e.g. MVPA, inactivity bouts, etc.
                  levelList = identify_levels(ts = ts, TRLi = TRLi, TRMi = TRMi, TRVi = TRVi,
                                              ws3 = ws3new, params_phyact = params_phyact)
                  LEVELS = levelList$LEVELS
                  OLEVELS = levelList$OLEVELS
                  Lnames = levelList$Lnames
                  ts = levelList$ts
                  
                  #=============================================
                  # NOW LOOP TROUGH DAYS AND GENERATE DAY SPECIFIC SUMMARY VARIABLES
                  # we want there to be one more nights in the accelerometer data than there are nights with sleep results
                  NNIGHTSSLEEP = length(unique(summarysleep_tmp2$calendar_date)) # nights with sleep results
                  NNIGHTSACC = length(nightsi) #acc
                  #-------------------------------
                  # ignore all nights in 'inights' before the first waking up and after the last waking up
                  FM = which(diff(ts$diur) == -1)
                  SO = which(diff(ts$diur) == 1)
                  # now 0.5+6+0.5 midnights and 7 days
                  for (timewindowi in params_output[["timewindow"]]) {
                    ts$window = 0
                    nightsi = nightsi_bu
                    # part3 estimate used for first night then
                    part3_estimates_firstnight = which(ts$guider == "part3_estimate")
                    if (length(part3_estimates_firstnight) > 0) {
                      ts$guider[part3_estimates_firstnight] = rev(params_sleep[["HASPT.algo"]])[1]
                    }
                    if (timewindowi == "WW") {
                      if (length(FM) > 0) {
                        # ignore first and last midnight because we did not do sleep detection on it
                        nightsi = nightsi[nightsi > FM[1] & nightsi < FM[length(FM)]]
                      }
                    } else if (timewindowi == "OO") {
                      if (length(SO) > 0) {
                        # ignore data before the first sleep onset and after the last sleep onset
                        nightsi = nightsi[nightsi > SO[1] & nightsi < SO[length(SO)]]
                      }
                    } else {
                      # if first night is missing then nights needs to align with diur
                      startend_sleep = which(abs(diff(ts$diur)) == 1)
                      Nepochsin12Hours =  (60/ws3new) * 60 * 12
                      nightsi = nightsi[nightsi >= (startend_sleep[1] - Nepochsin12Hours) &
                                          nightsi <= (startend_sleep[length(startend_sleep)] + Nepochsin12Hours)]  # newly added on 25-11-2019
                    }
                    if (timewindowi == "MM") {
                      Nwindows = length(nightsi) + 1  # +1 to include the data after last awakening
                    } else if (timewindowi == "WW") {
                      Nwindows = length(which(diff(ts$diur) == -1))
                    } else if (timewindowi == "OO") {
                      Nwindows = length(which(diff(ts$diur) == 1))
                    }
                    indjump = 1
                    qqq_backup = c()
                    add_one_day_to_next_date = FALSE
                    lastDay = ifelse(Nwindows > 0 && length(nightsi) > 0, yes = FALSE, no = TRUE) # skip while loop if there are no days to analyses
                    wi = 1
                    while (lastDay == FALSE) { #loop through windows
                      # Define indices of start and end of the day window (e.g. midnight-midnight, or waking-up or wakingup
                      defdays = g.part5.definedays(nightsi, wi, indjump,
                                                   epochSize = ws3new, qqq_backup, ts, 
                                                   timewindowi, Nwindows, qwindow = params_247[["qwindow"]],
                                                   ID = ID, dayborder = params_general[["dayborder"]])
                      qqq = defdays$qqq
                      qqq_backup = defdays$qqq_backup
                      segments = defdays$segments
                      segments_names = defdays$segments_names
                      lastDay = defdays$lastDay
                      if (length(which(is.na(qqq) == TRUE)) == 0) { #if it is a meaningful day then none of the values in qqq should be NA
                        if ((qqq[2] - qqq[1]) * ws3new > 900) {
                          ts$window[qqq[1]:qqq[2]] = wi
                          if (di == 1) {
                            next_si = 1
                          } else {
                            next_si = sum(dsummary[,1] != "") + 1
                          }
                          for (si in next_si:(next_si + length(segments) - 1)) {
                            fi = 1
                            current_segment_i = si - next_si + 1
                            Nsegments = length(segments[[current_segment_i]])
                            segStart = segments[[current_segment_i]][seq(1, Nsegments, by = 2)]
                            segEnd = segments[[current_segment_i]][seq(2, Nsegments, by = 2)]
                            extraRowsNeeded = max(c(si, di)) - nrow(dsummary)
                            if (extraRowsNeeded > 0) {
                              dsummary = rbind(dsummary, matrix(data = "", nrow = extraRowsNeeded, ncol = ncol(dsummary)))
                            }
                            if (timewindowi %in% c("MM", "WW") & si > 1) { # because first segment is always full window
                              if (("segment" %in% colnames(ts)) == FALSE) ts$segment = NA
                              for (gi in 1:Nsegments) {
                                if (!is.na(segStart[gi]) && !is.na(segEnd[gi])) {
                                  ts$segment[segStart[gi]:segEnd[gi]] = si
                                }
                              }
                            }
                            # Already store basic information about the file
                            # in the output matrix: 
                            dsummary[si,fi:(fi + 1)] = c(ID, fnames.ms3[i])
                            ds_names[fi:(fi + 1)] = c("ID", "filename"); fi = fi + 2
                            
                            ##################################################
                            # Analysis per segment:
                            # Group categories of objects together
                            # to reduce number of individual objects that need to be
                            # passed on to analyseSegment
                            indexlog = list(fileIndex = i,
                                            winType = timewindowi,
                                            winIndex = wi,
                                            winStartEnd = qqq,
                                            segIndex1 = si,
                                            segIndex2 = current_segment_i,
                                            segStartEnd = c(segStart, segEnd),
                                            columnIndex = fi)
                            timeList = list(ts = ts,
                                            sec = sec,
                                            min = min,
                                            hour = hour,
                                            time_POSIX = time_POSIX,
                                            epochSize = ws3new)
                            gas = g.part5_analyseSegment(indexlog, timeList, levelList,
                                                         segments,
                                                         segments_names,
                                                         dsummary, ds_names,
                                                         # parameter objects
                                                         params_general, params_output,
                                                         params_sleep, params_247,
                                                         params_phyact,
                                                         # sleep related
                                                         sumSleep = summarysleep_tmp2, sibDef,
                                                         # other arguments
                                                         fullFilename = fullfilenames[i],
                                                         add_one_day_to_next_date,
                                                         lightpeak_available, tail_expansion_log,
                                                         foldernamei = foldername[i],
                                                         sibreport = sibreport,
                                                         long_nap_boutsi = long_nap_boutsi)
                            # Extract essential object to be used as input for the next 
                            # segment
                            indexlog = gas$indexlog
                            ds_names = gas$ds_names
                            dsummary = gas$dsummary
                            timeList = gas$timeList
                            doNext = gas$doNext
                            add_one_day_to_next_date = gas$add_one_day_to_next_date
                            # indexlog
                            qqq = indexlog$winStartEnd
                            si = indexlog$segIndex1
                            current_segment_i = indexlog$segIndex2
                            segStart = indexlog$segStartEnd[1]
                            segEnd = indexlog$segStartEnd[2]
                            fi = indexlog$columnIndex
                            # timeList
                            ts = timeList$ts
                            ws3new = timeList$epochSize
                            Lnames = levelList$Lnames = timeList$Lnames
                            LEVELS = levelList$LEVELS = timeList$LEVELS
                            if (doNext == TRUE) next
                          }
                          #===============================================
                          # FOLDER STRUCTURE
                          if (params_output[["storefolderstructure"]] == TRUE) {
                            if ("filename_dir" %in% ds_names) fi = which( ds_names == "filename_dir")
                            dsummary[di,fi] = fullfilenames[i] #full filename structure
                            ds_names[fi] = "filename_dir"; fi = fi + 1
                            dsummary[di,fi] = foldername[i] #store the lowest foldername
                            if ("foldername" %in% ds_names) fi = which( ds_names == "foldername")
                            ds_names[fi] = "foldername"; fi = fi + 1
                          }
                          di = di + 1
                        }
                      }
                      di = di + 1
                      wi = wi + 1
                    }
                  }
                  if (params_output[["save_ms5rawlevels"]] == TRUE || params_247[["part6HCA"]] == TRUE || params_247[["part6CR"]] == TRUE) {
                    legendfile = paste0(metadatadir,ms5.outraw,"/behavioralcodes",as.Date(Sys.time()),".csv")
                    legendtable = data.frame(class_name = Lnames, class_id = 0:(length(Lnames) - 1), stringsAsFactors = FALSE)
                    if (!file.exists(legendfile)) {
                      data.table::fwrite(legendtable, file = legendfile, row.names = FALSE,
                                         sep = params_output[["sep_reports"]],
                                         dec = params_output[["dec_reports"]])
                    }
                    # I moved this bit of code to the end, because we want guider to be included (VvH April 2020)
                    rawlevels_fname =  paste0(metadatadir, ms5.outraw, "/", TRLi, "_", TRMi, "_", TRVi, "/",
                                              gsub(pattern = "[.]|rdata|csv|cwa|gt3x|bin",
                                                   replacement = "", x = fnames.ms3[i], ignore.case = TRUE),
                                              "_", sibDef, ".", params_output[["save_ms5raw_format"]])
                    # save time series to csv files
                    if (params_output[["do.sibreport"]] == TRUE & length(params_sleep[["nap_model"]]) > 0) {
                      napNonwear_col = "nap1_nonwear2"
                    } else {
                      napNonwear_col = NULL
                    }
                    if (params_output[["do.sibreport"]]  == TRUE) {
                      selfreported_col = "selfreported"
                    } else {
                      selfreported_col = NULL
                    }
                    
                    if (lightpeak_available == TRUE) {
                      lightpeak_col = "lightpeak"
                    } else {
                      lightpeak_col = NULL
                    }
                    angle_col = grep(pattern = "angle", x = names(ts), value = TRUE)
                    if (length(angle_col) == 0) {
                      angle_col = NULL
                    }
                    temperature_col = grep(pattern = "temperature", x = names(ts), value = TRUE)
                    if (length(temperature_col) == 0) {
                      temperature_col = NULL
                    }
                    step_count_col = grep(pattern = "step_count", x = names(ts), value = TRUE)
                    if (length(step_count_col) == 0) {
                      step_count_col = NULL
                    }
                    
                    diaryImputationCode_col = grep(pattern = "diaryImputationCode", 
                                                   x = names(ts), value = TRUE)
                    if (length(diaryImputationCode_col) == 0) {
                      diaryImputationCode_col = NULL
                    }
                    
                    marker_col = grep(pattern = "marker", x = names(ts), value = TRUE)
                    if (length(marker_col) == 0) {
                      marker_col = NULL
                    }
                    g.part5.savetimeseries(ts = ts[, c("time", "ACC", "diur", "nonwear",
                                                       "guider", "window", "sibdetection", napNonwear_col,
                                                       lightpeak_col, selfreported_col,
                                                       angle_col, temperature_col, step_count_col,
                                                       diaryImputationCode_col, marker_col)],
                                           LEVELS = LEVELS,
                                           desiredtz = params_general[["desiredtz"]],
                                           rawlevels_fname = rawlevels_fname,
                                           DaCleanFile = DaCleanFile,
                                           includedaycrit.part5 = params_cleaning[["includedaycrit.part5"]],
                                           includenightcrit.part5 = params_cleaning[["includenightcrit.part5"]],
                                           ID = ID,
                                           params_output = params_output,
                                           params_247 = params_247,
                                           Lnames = Lnames, timewindow = timewindowi,
                                           filename = filename)
                  }
                }
              }
            }
          }
        }
        if ("angle" %in% colnames(ts)) {
          ts = ts[, -which(colnames(ts) == "angle")]
        }
        #remove NA values
        for (kik in 1:ncol(dsummary)) {
          naval = which(is.na(dsummary[, kik]) == TRUE)
          if (length(naval) > 0) dsummary[naval, kik] = ""
        }
        output = data.frame(dsummary,stringsAsFactors = FALSE)
        names(output) = ds_names
        
        # correct definition of sleep log availability for window = WW, because now it
        # also relies on sleep log from previous night
        whoareWW = which(output$window == "WW") # look up WW
        if (length(params_sleep[["loglocation"]]) > 0) { #only do this if there is a sleep log
          if (length(whoareWW) > 0) {
            whoareNOSL = which(output$sleeplog_used[whoareWW] == "0") #look up nights with no Sleeplog
            if (length(whoareNOSL) > 0) {
              for (k23 in 1:length(whoareNOSL)) {
                k24 = whoareWW[(whoareNOSL[k23] - 1)]
                if (length(k24) > 0) {
                  if (k24 > 0) {
                    output$sleeplog_used[k24] = "0"
                  }
                }
              }
            }
          }
        }
        # tidy up output data frame, because it may have a lot of empty rows and columns
        emptyrows = which(output[,1] == "" & output[,2] == "")
        if (length(emptyrows) > 0) output = output[-emptyrows,]
        lastcolumn = which(colnames(output) == "boutdur.mvpa")
        if (length(lastcolumn) > 0) {
          if (ncol(output) > lastcolumn) {
            emptycols = sapply(output, function(x)all(x == ""))# Find columns filled with missing values which(output[1,] == "" & output[2,] == "")
            # ignore columns with the LUX hour
            emptycols = which(emptycols == TRUE &
                                colnames(output) %in%
                                grep(pattern = "LUX_|FRAG_|dur_|ACC_|Nbouts_|Nblocks_",
                                     x = colnames(output), value = TRUE) == FALSE)
            if (length(emptycols) > 0) emptycols = emptycols[which(emptycols > lastcolumn)]
            # While we explore the fragmentation variables, we want to make sure that all variables are kept in the output
            FRAG_variables_indices = grep(pattern = "FRAG_",x = names(output))
            emptycols = emptycols[which(emptycols %in% FRAG_variables_indices == FALSE)]
            if (length(emptycols) > 0) output = output[,-emptycols]
          }
          if (length(output) > 0) {
            if (nrow(output) > 0) {
              GGIRversion = "GGIR not used"
              if (is.element('GGIR', installed.packages()[,1])) {
                GGIRversion = as.character(utils::packageVersion("GGIR"))
                if (length(GGIRversion) != 1) GGIRversion = sessionInfo()$otherPkgs$GGIR$Version
              }
              output$GGIRversion = GGIRversion
              # Capture final timestamp to ease filtering last window in g.report.part5
              last_timestamp = time_POSIX[length(time_POSIX)] 
              # move experimental nap columns to the end of output
              if ("nap" %in% params_output[["method_research_vars"]]) {
                output = output[, c(grep(pattern = "denap|srnap|srnonw|sibreport_n_items", x = names(output), invert = TRUE, value = FALSE),
                                    grep(pattern = "denap|srnap|srnonw|sibreport_n_items", x = names(output), invert = FALSE, value = FALSE))]
              } else {
                output = output[, grep(pattern = "denap|srnap|srnonw|sibreport_n_items", x = names(output), invert = TRUE, value = FALSE)]
              }
              save(output, tail_expansion_log, GGIRversion, last_timestamp,
                   file = paste(metadatadir, ms5.out, "/", fnames.ms3[i], sep = ""))
            }
          }
        }
        rm(output, dsummary)
      }
    }
  }
  #======================================================================
  # loop through milestone data-files or filenames stored in output of g.part2 and g.part4
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
                                envir = as.environment(asNamespace("GGIR")))
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
    if (verbose == TRUE) cat(paste0('\n Busy processing ... see ', metadatadir, ms5.out, ' for progress\n'))
    # check whether we are in development mode:
    GGIRinstalled = is.element('GGIR', installed.packages()[,1])
    packages2passon = functions2passon = NULL
    GGIRloaded = "GGIR" %in% .packages()
    if (GGIRloaded) { #pass on package
      packages2passon = 'GGIR'
      errhand = 'pass'
    } else {
      # pass on functions
      functions2passon = c("is.ISO8601", "iso8601chartime2POSIX", "identify_levels", "g.getbout",
                           "g.sibreport", "extract_params", "load_params", "check_params",
                           "correctOlderMilestoneData",
                           "g.part5.addfirstwake", "g.part5.addsib",
                           "g.part5.definedays", "g.part5.fixmissingnight",
                           "g.part5.handle_lux_extremes", "g.part5.lux_persegment",
                           "g.part5.savetimeseries", "g.part5.wakesleepwindows",
                           "g.part5.onsetwaketiming", "g.part5_analyseSegment",
                           "g.part5_initialise_ts", "g.part5.analyseRest",
                           "g.fragmentation", "g.intensitygradient",
                           "g.part5.addNaps", "g.part4_extractid",
                           "markerButtonForRest")
      errhand = 'stop'
    }
    i = 0 # declare i because foreach uses it, without declaring it
    `%myinfix%` = foreach::`%dopar%`
    output_list = foreach::foreach(i = f0:f1,  .packages = packages2passon,
                                   .export = functions2passon, .errorhandling = errhand) %myinfix% {
                                     tryCatchResult = tryCatch({
                                       main_part5(i, metadatadir, f0, f1,
                                                  params_sleep, params_metrics,
                                                  params_247, params_phyact,
                                                  params_cleaning, params_output,
                                                  params_general, ms5.out, ms5.outraw,
                                                  fnames.ms3, sleeplog, logs_diaries,
                                                  referencefnames, folderstructure,
                                                  fullfilenames, foldername, ffdone, verbose)
                                     })
                                     return(tryCatchResult)
                                   }
    on.exit(parallel::stopCluster(cl))
    for (oli in 1:length(output_list)) { # logged error and warning messages
      if (is.null(unlist(output_list[oli])) == FALSE) {
        if (verbose == TRUE) cat(paste0("\nErrors and warnings for ", fnames.ms3[oli]))
        print(unlist(output_list[oli])) # print any error and warnings observed
      }
    }
  } else {
    errors = list()
    for (i in f0:f1) {
      if (verbose == TRUE) cat(paste0(i, " "))
      function_to_evaluate = expression(
        main_part5(i, metadatadir, f0, f1,
                   params_sleep, params_metrics,
                   params_247, params_phyact,
                   params_cleaning, params_output,
                   params_general, ms5.out, ms5.outraw,
                   fnames.ms3, sleeplog, logs_diaries,
                   referencefnames, folderstructure,
                   fullfilenames, foldername, ffdone, verbose)
      )
      if (params_general[["use_trycatch_serial"]] == TRUE) {
        tryCatch(
          eval(function_to_evaluate),
          error = function(e) {
            err_msg = conditionMessage(e)
            errors[[as.character(fnames.ms3[i])]] <<- err_msg
          }
        )
      } else {
        eval(function_to_evaluate)
      }
    }
    # show logged errors after the loop:
    if (params_general[["use_trycatch_serial"]] == TRUE && verbose == TRUE) {
      if (length(errors) > 0) {
        cat(paste0("\n\nErrors in part 5... for:"))
        cat(paste0("\n-", names(errors), ": ", unlist(errors), collapse = ""))
      }
    }
  }
}
