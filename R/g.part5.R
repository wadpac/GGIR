g.part5 = function(datadir = c(), metadatadir = c(), f0=c(), f1=c(),
                   params_sleep = c(), params_metrics = c(),
                   params_247 = c(), params_phyact = c(),
                   params_cleaning = c(), params_output = c(),
                   params_general = c(), ...) {
  options(encoding = "UTF-8")
  Sys.setlocale("LC_TIME", "C") # set language to English
  # description: function called by g.shell.GGIR
  # aimed to merge the milestone output from g.part2, g.part3, and g.part4
  # in order to create a merged report of both physical activity and sleep

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
  ms5.out = "/meta/ms5.out"
  if (!file.exists(paste(metadatadir, ms5.out, sep = ""))) {
    dir.create(file.path(metadatadir, ms5.out))
  }
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
  fnames.ms3 = sort(dir(paste(metadatadir, "/meta/ms3.out", sep = "")))

  fnames.ms5 = sort(dir(paste(metadatadir, "/meta/ms5.out", sep = "")))
  # path to sleeplog milestonedata, if it exists:
  sleeplogRDA = paste(metadatadir, "/meta/sleeplog.RData", sep = "")
  if (file.exists(sleeplogRDA) == TRUE) {
    sleeplog = logs_diaries = c()
    load(sleeplogRDA)
    if (length(logs_diaries) > 0) {# new format
      if (is.list(logs_diaries)) { # advanced format
        sleeplog = logs_diaries$sleeplog
      } else {
        sleeplog = logs_diaries
      }
    }
  } else {
    sleeplog = logs_diaries = c()
  }
  #------------------------------------------------
  # specify parameters
  ffdone = fnames.ms5 #ffdone is now a list of files that have already been processed by g.part5
  fnames.ms3 = sort(fnames.ms3)
  if (f1 > length(fnames.ms3)) f1 = length(fnames.ms3) # this is intentionally ms3 and not ms4, do not change!
  params_phyact[["boutdur.mvpa"]] = sort(params_phyact[["boutdur.mvpa"]],decreasing = TRUE)
  params_phyact[["boutdur.lig"]] = sort(params_phyact[["boutdur.lig"]],decreasing = TRUE)
  params_phyact[["boutdur.in"]] = sort(params_phyact[["boutdur.in"]],decreasing = TRUE)
  if (params_output[["save_ms5raw_format"]] != "RData" & params_output[["save_ms5raw_format"]] != "csv") {
    params_output[["save_ms5raw_format"]] = "csv"# specify as csv if user does not clearly specify format
  }

  #--------------------------------
  # get full file path and folder name if requested by end-user and keep this for storage in output
  if (params_output[["storefolderstructure"]] == TRUE) {
    extractfilenames = function(x) as.character(unlist(strsplit(x,".RDa"))[1])
    referencefnames = sapply(fnames.ms3,extractfilenames)
    folderstructure = getfolderstructure(datadir,referencefnames)
    fullfilenames = folderstructure$fullfilenames
    foldername = folderstructure$foldername
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
                        extractfilenames, referencefnames, folderstructure, fullfilenames, foldernam) {

    fnames.ms1 = sort(dir(paste(metadatadir, "/meta/basic", sep = "")))
    fnames.ms2 = sort(dir(paste(metadatadir, "/meta/ms2.out", sep = "")))
    fnames.ms4 = sort(dir(paste(metadatadir, "/meta/ms4.out", sep = "")))

    nfeatures = 500
    ws3 = params_general[["windowsizes"]][1]
    ds_names = rep("",nfeatures)
    di = 1
    DaCleanFile = c()
    if (length(params_cleaning[["data_cleaning_file"]]) > 0) {
      if (file.exists(params_cleaning[["data_cleaning_file"]])) DaCleanFile = read.csv(params_cleaning[["data_cleaning_file"]])
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
      if (length(idindex) > 0 & nrow(summarysleep) > 1) { #only attempt to load file if it was processed for sleep
        summarysleep_tmp = summarysleep
        #======================================================================
        # load output g.part1
        selp = which(fnames.ms1 == paste0("meta_", fnames.ms3[i]))
        if (length(selp) != 1) {
          cat("Warning: Milestone data part 1 could not be retrieved")
        }
        load(paste0(metadatadir, "/meta/basic/", fnames.ms1[selp]))
        # load output g.part3
        load(paste0(metadatadir, "/meta/ms3.out/", fnames.ms3[i]))
        # extract key variables from the mile-stone data: time, acceleration and elevation angle
        # note that this is imputed ACCELERATION because we use this for describing behaviour:
        ts = data.frame(time = IMP$metashort[,1], ACC = IMP$metashort[,params_general[["acc.metric"]]] * 1000,
                        guider = rep("unknown", nrow(IMP$metashort)),
                        angle = as.numeric(as.matrix(IMP$metashort[,which(names(IMP$metashort) == "anglez")])) )
        Nts = nrow(ts)
        if (length(which(names(IMP$metashort) == "anglez")) == 0) {
          cat("Warning: anglez not extracted. Please check that do.anglez == TRUE")
        }
        # add non-wear column
        nonwear = IMP$rout[,5]
        nonwear = rep(nonwear, each = (IMP$windowsizes[2]/IMP$windowsizes[1]))
        if (length(nonwear) > Nts) {
          nonwear = nonwear[1:Nts]
        } else if (length(nonwear) < Nts) {
          nonwear = c(nonwear, rep(0, (Nts - length(nonwear))))
        }
        ts$nonwear = 0 # initialise column
        ts$nonwear = nonwear
        lightpeak_available = "lightpeak" %in% colnames(M$metalong)
        # Check if temperature and light are availble
        if (lightpeak_available == TRUE) {
          luz = M$metalong$lightpeak
          if (length(params_247[["LUX_cal_constant"]]) > 0 &
              length(params_247[["LUX_cal_exponent"]]) > 0) { # re-calibrate light
            luz = params_247[["LUX_cal_constant"]] * exp(params_247[["LUX_cal_exponent"]] * luz)
          }
          handle_luz_extremes = g.part5.handle_lux_extremes(luz)
          luz = handle_luz_extremes$lux
          correction_log = handle_luz_extremes$correction_log
          # repeate values to match resolution of other data
          repeatvalues = function(x, windowsizes, Nts) {
            x = rep(x, each = (windowsizes[2]/windowsizes[1]))
            if (length(x) > Nts) {
              x = x[1:Nts]
            } else if (length(x) < Nts) {
              x = c(x, rep(0, (Nts - length(x))))
            }
            return(x)
          }
          luz = repeatvalues(x = luz, windowsizes = IMP$windowsizes, Nts)
          correction_log = repeatvalues(x = correction_log, windowsizes = IMP$windowsizes, Nts)
          ts$lightpeak_imputationcode = ts$lightpeak = 0 # initialise column
          ts$lightpeak = luz
          ts$lightpeak_imputationcode = correction_log
        }
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
        cut = which(S$fraction.night.invalid > 0.7 | S$nsib.periods == 0)
        if (length(cut) > 0) S = S[-cut,]
        if (params_general[["part5_agg2_60seconds"]] == TRUE) {
          ts_backup = ts
        }
        # Remove impossible entries:
        pko = which(summarysleep_tmp$sleeponset == 0 & summarysleep_tmp$wakeup == 0 & summarysleep_tmp$SptDuration == 0)
        if (length(pko) > 0) {
          summarysleep_tmp = summarysleep_tmp[-pko,]
        }
        for (j in def) { # loop through sleep definitions (defined by angle and time threshold in g.part3)
          ws3new = ws3 # reset wse3new, because if part5_agg2_60seconds is TRUE then this will have been change in the previous iteration of the loop
          if (params_general[["part5_agg2_60seconds"]] == TRUE) {
            ts = ts_backup
          }
          # extract time and from that the indices for midnights
          time_POSIX = iso8601chartime2POSIX(ts$time,tz = params_general[["desiredtz"]])
          tempp = unclass(time_POSIX)
          if (is.na(tempp$sec[1]) == TRUE) {
            tempp = unclass(as.POSIXlt(ts$time, tz = params_general[["desiredtz"]]))
          }
          sec = tempp$sec
          min = tempp$min
          hour = tempp$hour
          if (params_general[["dayborder"]] == 0) {
            nightsi = which(sec == 0 & min == 0 & hour == 0)
          } else {
            nightsi = which(sec == 0 & min == (params_general[["dayborder"]] - floor(params_general[["dayborder"]])) * 60 & hour == floor(params_general[["dayborder"]])) #shift the definition of midnight if required
          }
          # create copy of only relevant part of sleep summary dataframe
          summarysleep_tmp2 = summarysleep_tmp[which(summarysleep_tmp$sleepparam == j),]
          S2 = S[S$definition == j,] # simplify to one definition
          # Add sustained inactivity bouts (sib) to the time series
          ts = g.part5.addsib(ts, ws3new, Nts, S2, params_general[["desiredtz"]], j,  nightsi)
          # Fix missing nights in part 4 data:
          summarysleep_tmp2 = g.part5.fixmissingnight(summarysleep_tmp2, sleeplog = sleeplog, ID)
          #Initialise diur variable, which will  indicate the diurnal rhythm: 0 if wake/daytime, 1 if sleep/nighttime
          ts$diur = 0
          if (nrow(summarysleep_tmp2) > 0) {
            # Add defenition of wake and sleep windows in diur column of data.frame ts
            ts = g.part5.wakesleepwindows(ts, summarysleep_tmp2, params_general[["desiredtz"]], nightsi,
                                          sleeplog, ws3, Nts, ID, Nepochsinhour)
            # Add first waking up time, if it is missing:
            ts = g.part5.addfirstwake(ts, summarysleep_tmp2, nightsi, sleeplog, ID,
                                      Nepochsinhour, Nts, SPTE_end, ws3)
            if (params_general[["part5_agg2_60seconds"]] == TRUE) { # Optionally aggregate to 1 minute epoch:
              ts$time_num = floor(as.numeric(iso8601chartime2POSIX(ts$time,tz = params_general[["desiredtz"]])) / 60) * 60
              if (lightpeak_available == TRUE) {
                ts = aggregate(ts[, c("ACC","sibdetection", "diur", "nonwear", "angle", "lightpeak", "lightpeak_imputationcode")],
                               by = list(ts$time_num), FUN = function(x) mean(x))
              } else {
                ts = aggregate(ts[,c("ACC","sibdetection", "diur", "nonwear", "angle")],
                               by = list(ts$time_num), FUN = function(x) mean(x))
              }
              ts$sibdetection = round(ts$sibdetection)
              ts$diur = round(ts$diur)
              ts$nonwear = round(ts$nonwear)
              names(ts)[1] = "time"
              # # convert back to iso8601 format
              ts$time = as.POSIXlt(ts$time, origin = "1970-1-1", tz = params_general[["desiredtz"]])
              ws3new = 60 # change because below it is used to decide how many epochs are there in
              # extract nightsi again
              time_POSIX = ts$time
              tempp = unclass(time_POSIX)
              if (is.na(tempp$sec[1]) == TRUE) {
                tempp = unclass(as.POSIXlt(ts$time, tz = params_general[["desiredtz"]]))
              }
              sec = tempp$sec
              min = tempp$min
              hour = tempp$hour
              if (params_general[["dayborder"]] == 0) {
                nightsi = which(sec == 0 & min == 0 & hour == 0)
              } else {
                nightsi = which(sec == 0 & min == (params_general[["dayborder"]] - floor(params_general[["dayborder"]])) * 60 & hour == floor(params_general[["dayborder"]])) #shift the definition of midnight if required
              }
              Nts = nrow(ts)
            }
            if ("angle" %in% colnames(ts)) {
              ts = ts[, -which(colnames(ts) == "angle")]
            }
            #===============================================
            # Use sib.report to classify naps, non-wear and integrate these in time series
            # Done at this point in the code, because it
            # does not depend on bout detection criteria or window definitions.
            if (params_output[["do.sibreport"]]  == TRUE & length(params_sleep[["nap_model"]]) > 0) {
              if (params_sleep[["sleeplogidnum"]] == TRUE) {
                IDtmp = as.numeric(ID)
              } else {
                IDtmp = as.character(ID)
              }
              sibreport = g.sibreport(ts, ID = IDtmp, epochlength = ws3new, logs_diaries,
                                      desiredtz = params_general[["desiredtz"]])
              # store in csv file:
              ms5.sibreport = "/meta/ms5.outraw/sib.reports"
              if (!file.exists(paste(metadatadir, ms5.sibreport, sep = ""))) {
                dir.create(file.path(metadatadir, ms5.sibreport))
              }
              sibreport_fname =  paste0(metadatadir,ms5.sibreport,"/sib_report_",fnames.ms3[i],"_",j,".csv")
              write.csv(x = sibreport, file = sibreport_fname, row.names = FALSE)
              # nap detection
              if (params_general[["acc.metric"]] != "ENMO" |
                  params_sleep[["HASIB.algo"]] != "vanHees2015") {
                warning("\nNap classification currently assumes acc.metric = ENMO and HASIB.algo = vanHees2015, so output may not be meaningful")
              }
              naps_nonwear = g.part5.classifyNaps(sibreport = sibreport,
                                                  desiredtz = params_general[["desiredtz"]],
                                                  possible_nap_window = params_sleep[["possible_nap_window"]],
                                                  possible_nap_dur = params_sleep[["possible_nap_dur"]],
                                                  nap_model = params_sleep[["nap_model"]],
                                                  HASIB.algo = params_sleep[["HASIB.algo"]])

              # store in ts object, such that it is exported in as time series
              ts$nap1_nonwear2 = 0
              # napsindices = which(naps_nonwear$probability_nap == 1)
              # if (length(napsindices) > 0) {
              if (length(naps_nonwear) > 0) {
                for (nni in 1:nrow(naps_nonwear)) {
                  nnc_window = which(time_POSIX >= naps_nonwear$start[nni] & time_POSIX <= naps_nonwear$end[nni] & ts$diur == 0)
                  if (length(nnc_window) > 0) {
                    if (naps_nonwear$probability_nap[nni] == 1) {
                      ts$nap1_nonwear2[nnc_window] = 1 # nap
                    } else if (naps_nonwear$probability_nap[nni] == 0) {
                      ts$nap1_nonwear2[nnc_window] = 2 # nonwear
                    }
                  }
                }
              }
              # impute non-naps episodes (non-wear)
              nonwearindices = which(naps_nonwear$probability_nap == 0)
              if (length(nonwearindices) > 0) {
                for (nni in nonwearindices) {
                  nwwindow_start = which(time_POSIX >= naps_nonwear$start[nni] & time_POSIX <= naps_nonwear$end[nni] & ts$diur == 0)
                  if (length(nwwindow_start) > 0) {
                    Nepochsin24Hours =  (60/ws3new) * 60 * 24
                    if (nwwindow_start[1] > Nepochsin24Hours) {
                      nwwindow = nwwindow_start - Nepochsin24Hours # impute time series with preceding day
                      if (length(which(ts$nap1_nonwear2[nwwindow] == 2)) / length(nwwindow) > 0.5) {
                        # if there is also a lot of overlap with non-wear there then do next day
                        nwwindow = nwwindow_start + Nepochsin24Hours
                      }
                    } else {
                      nwwindow = nwwindow_start + Nepochsin24Hours # if there is not preceding day use next day
                    }
                    if (max(nwwindow) <= nrow(ts)) { # only attempt imputation if possible
                      # check again that there is not a lot of overlap with non-wear
                      if (length(which(ts$nap1_nonwear2[nwwindow] == 2)) / length(nwwindow) > 0.5) {
                        ts$ACC[nwwindow_start] = ts$ACC[nwwindow] # impute
                      }
                    }
                  }
                }
              }
            }
            ts$window = 0
            for (TRLi in params_phyact[["threshold.lig"]]) {
              for (TRMi in params_phyact[["threshold.mod"]]) {
                for (TRVi in params_phyact[["threshold.vig"]]) {
                  # derive behavioral levels (class), e.g. MVPA, inactivity bouts, etc.
                  levels = identify_levels(ts = ts, TRLi = TRLi, TRMi = TRMi, TRVi = TRVi,
                                           ws3 = ws3new, params_phyact = params_phyact)
                  LEVELS = levels$LEVELS
                  OLEVELS = levels$OLEVELS
                  Lnames = levels$Lnames
                  bc.mvpa = levels$bc.mvpa
                  bc.lig = levels$bc.lig
                  bc.in = levels$bc.in
                  ts = levels$ts
                  #=============================================
                  # NOW LOOP TROUGH DAYS AND GENERATE DAY SPECIFIC SUMMARY VARIABLES
                  # we want there to be one more nights in the accelerometer data than there are nights with sleep results
                  NNIGHTSSLEEP = length(unique(summarysleep_tmp2$calendar_date)) # nights with sleep results
                  NNIGHTSACC = length(nightsi) #acc
                  #-------------------------------
                  # ignore all nights in 'inights' before the first waking up and after the last waking up
                  FM = which(diff(ts$diur) == -1)
                  nightsi_bu = nightsi
                  # now 0.5+6+0.5 midnights and 7 days
                  for (timewindowi in params_output[["timewindow"]]) {
                    nightsi = nightsi_bu
                    ts$guider = "unknown"
                    if (timewindowi == "WW") {
                      if (length(FM) > 0) {
                        # ignore first and last midnight because we did not do sleep detection on it
                        nightsi = nightsi[nightsi > FM[1] & nightsi < FM[length(FM)]]
                      }
                    } else {
                      # newly added on 31-3-2019, because if first night is missing then nights needs to allign with diur
                      startend_sleep = which(abs(diff(ts$diur)) == 1)
                      Nepochsin12Hours =  (60/ws3new) * 60 * 12
                      nightsi = nightsi[nightsi >= (startend_sleep[1] - Nepochsin12Hours) &
                                          nightsi <= (startend_sleep[length(startend_sleep)] + Nepochsin12Hours)]  # newly added on 25-11-2019
                      #nightsi = nightsi[which(nightsi >= startend_sleep[1] & nightsi <= startend_sleep[length(startend_sleep)])]
                    }
                    if (timewindowi == "MM") {
                      #  Nwindows = nrow(summarysleep_tmp2)
                      #  Nwindows = length(which(diff(ts$diur) == -1)) + 1
                      Nwindows = length(nightsi) + 1
                    } else {
                      Nwindows = length(which(diff(ts$diur) == -1))
                    }
                    indjump = 1
                    qqq_backup = c()
                    add_one_day_to_next_date = FALSE
                    for (wi in 1:Nwindows) { #loop through 7 windows (+1 to include the data after last awakening)
                      # Define indices of start and end of the day window (e.g. midnight-midnight, or waking-up or wakingup
                      defdays = g.part5.definedays(nightsi, wi, indjump,
                                                   nightsi_bu, ws3new, qqq_backup, ts, Nts,
                                                   timewindowi, Nwindows)
                      qqq = defdays$qqq
                      qqq_backup = defdays$qqq_backup
                      if (length(which(is.na(qqq) == TRUE)) == 0) { #if it is a meaningful day then none of the values in qqq should be NA
                        if ((qqq[2] - qqq[1]) * ws3new > 900) {
                          fi = 1
                          # START STORING BASIC INFORMATION
                          dsummary[di,fi:(fi + 2)] = c(ID, fnames.ms3[i], wi)
                          ds_names[fi:(fi + 2)] = c("ID", "filename", "window_number"); fi = fi + 3
                          if (timewindowi == "WW") {
                            plusb = 0
                          } else {
                            plusb = 1
                          }
                          skiponset = skipwake = TRUE
                          ts$window[qqq[1]:qqq[2]] = wi
                          #==========================
                          # Newly added to avoid issue with merging sleep
                          # variables from part 4, we simply extract them
                          # from the new time series
                          # Note that this means that for MM windows there can be multiple or no wake or onsets
                          date = as.Date(ts$time[qqq[1] + 1])
                          if (add_one_day_to_next_date == TRUE & timewindowi == "WW") { # see below for explanation
                            date = date + 1
                            add_one_day_to_next_date = FALSE
                          }
                          Sys.setlocale("LC_TIME", "C")
                          weekday = weekdays(date, abbreviate = FALSE)
                          dsummary[di,fi:(fi + 1)] = c(weekday, as.character(date))
                          ds_names[fi:(fi + 1)] = c("weekday", "calendar_date"); fi = fi + 2
                          # Get onset and waking timing, both as timestamp and as index
                          onsetwaketiming = g.part5.onsetwaketiming(qqq,ts, min, sec, hour, timewindowi, skiponset, skipwake)
                          onset = onsetwaketiming$onset; wake = onsetwaketiming$wake
                          onseti = onsetwaketiming$onseti; wakei = onsetwaketiming$wakei
                          skiponset = onsetwaketiming$skiponset; skipwake = onsetwaketiming$skipwake
                          if (wake < 24 & timewindowi == "WW") {
                            # waking up before midnight means that next WW window
                            # will start a day before the day we refer to when discussing it's SPT
                            # So, for next window we have to do date = date + 1
                            add_one_day_to_next_date = TRUE
                          }
                          # Add to dsummary output matrix
                          if (skiponset == FALSE) {
                            dsummary[di,fi] = onset
                            dsummary[di,fi + 1] = as.character(strftime(as.character(time_POSIX[onseti]), tz = params_general[["desiredtz"]], format = "%H:%M:%S"))
                          } else {
                            dsummary[di,fi:(fi + 1)] = rep(NA, 2)
                          }
                          ds_names[fi:(fi + 1)] = c("sleeponset", "sleeponset_ts");      fi = fi + 2
                          if (skipwake == FALSE) {
                            dsummary[di,fi] = wake
                            dsummary[di,fi + 1] = as.character(strftime(as.character(time_POSIX[wakei]), tz = params_general[["desiredtz"]], format = "%H:%M:%S"))
                          } else {
                            dsummary[di,fi:(fi + 1)] = rep(NA, 2)
                          }
                          ds_names[fi:(fi + 1)] = c("wakeup", "wakeup_ts");      fi = fi + 2
                          # extract date and use this to retrieve corresponding part 4 information about the nights:
                          options(encoding = "UTF-8")
                          Sys.setlocale("LC_TIME", "C") # set language to English
                          # look up matching part4 entry:
                          recDates = as.Date(summarysleep_tmp2$calendar_date, format = "%d/%m/%Y", origin = "1970-01-01")
                          dsummary[di, fi] = j
                          ds_names[fi] = "sleepparam";      fi = fi + 1
                          dayofinterst = which(recDates == date)
                          if (length(dayofinterst) > 0) {
                            dayofinterst = dayofinterst[1]
                            dsummary[di,fi:(fi + 5)] = c(summarysleep_tmp2$night[dayofinterst],
                                                         summarysleep_tmp2$daysleeper[dayofinterst],
                                                         summarysleep_tmp2$cleaningcode[dayofinterst],
                                                         summarysleep_tmp2$guider[dayofinterst],
                                                         summarysleep_tmp2$sleeplog_used[dayofinterst],
                                                         summarysleep_tmp2$acc_available[dayofinterst])
                            ds_names[fi:(fi + 5)] = c("night_number", "daysleeper", "cleaningcode",
                                                      "guider", "sleeplog_used", "acc_available");      fi = fi + 6
                            ts$guider[qqq[1]:qqq[2]] = summarysleep_tmp2$guider[dayofinterst] # add guider also to timeseries
                          } else {
                            dsummary[di,fi:(fi + 5)] = rep(NA, 6)
                            ds_names[fi:(fi + 5)] = c("night_number",
                                                      "daysleeper","cleaningcode","guider",
                                                      "sleeplog_used","acc_available"); fi = fi + 6
                          }
                          # Untill here.
                          #==========================
                          # define time windows:
                          # We will get acc_onset and wakeup
                          # regarding to the same day of measurement in the same row.
                          # which differs between MM and WW
                          # Also, it allows for the analysis of the first day for those studies in which the accelerometer is started during the morning and the first day is of interest.
                          # qqq1 is the start of the day
                          # qqq2 is the end of the day
                          qqq1 = qqq[1] # added 26 Feb 2020
                          qqq2 = qqq[2] # added 26 Feb 2020
                          if (timewindowi == "MM") {
                            dsummary[di, fi] = "MM"
                          } else if (timewindowi == "WW") {
                            dsummary[di, fi] = "WW"
                          }
                          ds_names[fi] = "window";      fi = fi + 1
                          # keep track of threshold value
                          dsummary[di,fi] = TRLi
                          ds_names[fi] = "TRLi";      fi = fi + 1
                          dsummary[di,fi] = TRMi
                          ds_names[fi] = "TRMi";      fi = fi + 1
                          dsummary[di,fi] = TRVi
                          ds_names[fi] = "TRVi";      fi = fi + 1
                          wlih = ((qqq2 - qqq1) + 1)/((60/ws3new) * 60)
                          if (qqq1 > length(LEVELS)) qqq1 = length(LEVELS)
                          sse = qqq1:qqq2
                          #============================================================
                          # percentage of available data
                          zt_hrs_nonwear = (length(which(ts$diur[sse] == 0 & ts$nonwear[sse] == 1)) * ws3new) / 3600 #day
                          zt_hrs_total = (length(which(ts$diur[sse] == 0)) * ws3new) / 3600 #day
                          dsummary[di,fi] = (zt_hrs_nonwear/zt_hrs_total)  * 10000 / 100
                          ds_names[fi] = "nonwear_perc_day";      fi = fi + 1
                          zt_hrs_nonwear = (length(which(ts$diur[sse] == 1 & ts$nonwear[sse] == 1)) * ws3new) / 3600 #night
                          zt_hrs_total = (length(which(ts$diur[sse] == 1)) * ws3new) / 3600 #night
                          dsummary[di,fi] =  (zt_hrs_nonwear/zt_hrs_total)  * 10000 / 100
                          ds_names[fi] = "nonwear_perc_spt";      fi = fi + 1
                          zt_hrs_nonwear = (length(which(ts$nonwear[sse] == 1)) * ws3new) / 3600
                          zt_hrs_total = (length(ts$diur[sse]) * ws3new) / 3600 #night and day
                          dsummary[di,fi] =  (zt_hrs_nonwear/zt_hrs_total)  * 10000 / 100
                          ds_names[fi] = "nonwear_perc_day_spt";      fi = fi + 1
                          #===============================================
                          # TIME SPENT IN WINDOWS (window is either midnight-midnight or waking up-waking up)
                          test_remember = c(di,fi)
                          for (levelsc in 0:(length(Lnames) - 1)) {
                            dsummary[di,fi] = (length(which(LEVELS[sse] == levelsc)) * ws3new) / 60
                            ds_names[fi] = paste0("dur_", Lnames[levelsc + 1],"_min");      fi = fi + 1
                          }
                          for (g in 1:4) {
                            dsummary[di, (fi + (g - 1))] = (length(which(OLEVELS[sse] == g)) * ws3new) / 60
                          }
                          ds_names[fi:(fi + 3)] = c("dur_day_total_IN_min",
                                                    "dur_day_total_LIG_min",
                                                    "dur_day_total_MOD_min",
                                                    "dur_day_total_VIG_min")
                          fi = fi + 4
                          dsummary[di, fi] = (length(which(ts$diur[sse] == 0)) * ws3new) / 60
                          ds_names[fi] = "dur_day_min";      fi = fi + 1
                          dsummary[di, fi] = (length(which(ts$diur[sse] == 1)) * ws3new) / 60
                          ds_names[fi] = "dur_spt_min";      fi = fi + 1
                          dsummary[di, fi] = (length(c(sse)) * ws3new) / 60
                          ds_names[fi] = "dur_day_spt_min";      fi = fi + 1
                          #============================================
                          # Number of long wake periods (defined as > 5 minutes) during the night
                          Nawake = length(which(abs(diff(which(LEVELS[sse] == 0))) > (300 / ws3new))) - 2
                          if (Nawake < 0) Nawake = 0
                          dsummary[di, fi] = Nawake
                          ds_names[fi] = "N_atleast5minwakenight";      fi = fi + 1
                          #=============================
                          # sleep efficiency
                          dsummary[di,fi] = length(which(ts$sibdetection[sse] == 1 &
                                                           ts$diur[sse] == 1)) / length(which(ts$diur[sse] == 1))
                          ds_names[fi] = "sleep_efficiency";      fi = fi + 1
                          #===============================================
                          # NAPS (estimation)
                          if (params_output[["do.sibreport"]] == TRUE & "nap1_nonwear2" %in% colnames(ts) & length(params_sleep[["nap_model"]]) > 0) {
                            dsummary[di,fi] = length(which(diff(c(-1, which(ts$nap1_nonwear2[sse] == 1 & ts$diur[sse] == 0))) > 1))
                            ds_names[fi] = "nap_count";      fi = fi + 1
                            dsummary[di,fi] = round((sum(ts$nap1_nonwear2[sse[which(ts$nap1_nonwear2[sse] == 1 & ts$diur[sse] == 0)]]) * ws3new) / 60, digits = 2)
                            ds_names[fi] = "nap_totalduration";      fi = fi + 1
                          }
                          #===============================================
                          # AVERAGE ACC PER WINDOW
                          for (levelsc in 0:(length(Lnames) - 1)) {
                            dsummary[di,fi] = mean(ts$ACC[sse[LEVELS[sse] == levelsc]], na.rm = TRUE)
                            ds_names[fi] = paste("ACC_", Lnames[levelsc + 1], "_mg", sep = "");      fi = fi + 1
                          }
                          for (g in 1:4) {
                            dsummary[di,(fi + (g - 1))] = mean(ts$ACC[sse[OLEVELS[sse] == g]], na.rm = TRUE)
                          }
                          ds_names[fi:(fi + 3)] = c("ACC_day_total_IN_mg", "ACC_day_total_LIG_mg",
                                                    "ACC_day_total_MOD_mg", "ACC_day_total_VIG_mg")
                          fi = fi + 4
                          dsummary[di, fi] = mean(ts$ACC[sse[ts$diur[sse] == 0]], na.rm = TRUE)
                          ds_names[fi] = "ACC_day_mg";      fi = fi + 1
                          dsummary[di, fi] = mean(ts$ACC[sse[ts$diur[sse] == 1]], na.rm = TRUE)
                          ds_names[fi] = "ACC_spt_mg";      fi = fi + 1
                          dsummary[di, fi] = mean(ts$ACC[sse], na.rm = TRUE)
                          ds_names[fi] = "ACC_day_spt_mg";      fi = fi + 1
                          #===============================================
                          # QUANTILES...
                          WLH = ((qqq2 - qqq1) + 1)/((60/ws3new) * 60)
                          if (WLH <= 1) WLH = 1.001
                          dsummary[di, fi] = quantile(ts$ACC[sse],probs = ((WLH - 1)/WLH), na.rm = TRUE)
                          ds_names[fi] = paste("quantile_mostactive60min_mg", sep = "");      fi = fi + 1
                          dsummary[di, fi] = quantile(ts$ACC[sse],probs = ((WLH - 0.5)/WLH), na.rm = TRUE)
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
                              TIMErunwin= matrix("", nwindow_f, 1)
                              for (hri in 0:floor((((endd - wini) * (60/reso)) - 1))) {
                                e1 = (hri * reso * (60/ws3new)) + 1
                                e2 = (hri + (wini * (60/reso))) * reso * (60/ws3new)
                                if (e2 > length(sse)) e2 = length(sse)
                                ACCrunwin[(hri + 1), 1] = mean(ts$ACC[sse[e1:e2]])
                                TIMErunwin[(hri + 1), 1] = as.character(ts$time[sse[e1]])
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
                                  startM5 = which(ts$time == M5HOUR)
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
                              dsummary[di, fi] = L5HOUR
                              dsummary[di, fi + 1] = L5VALUE
                              dsummary[di, fi + 2] = M5HOUR
                              dsummary[di, fi + 3] = M5VALUE
                            }
                            ds_names[fi] = paste("L", wini, "TIME", sep = "")
                            ds_names[fi + 1] = paste("L", wini, "VALUE", sep = "")
                            ds_names[fi + 2] = paste("M", wini, "TIME", sep = "")
                            ds_names[fi + 3] = paste("M", wini, "VALUE", sep = "")
                            fi = fi + 4
                            if ("lightpeak" %in% colnames(ts)) {
                              if (ignore == FALSE) {
                                dsummary[di,fi] = M5_mean_peakLUX
                                dsummary[di,fi + 1] = M5_max_peakLUX
                              }
                              ds_names[fi] = paste("M", wini, "_mean_peakLUX", sep = "")
                              ds_names[fi + 1] = paste("M", wini,"_max_peakLUX", sep = "")
                              fi = fi + 2
                            }
                            if (ignore == FALSE) {
                              # Add also numeric time
                              if (is.ISO8601(L5HOUR)) { # only do this for ISO8601 format
                                L5HOUR = as.character(iso8601chartime2POSIX(L5HOUR, tz = params_general[["desiredtz"]]))
                                M5HOUR = as.character(iso8601chartime2POSIX(M5HOUR, tz = params_general[["desiredtz"]]))
                              }
                              if (length(unlist(strsplit(L5HOUR," "))) == 1) L5HOUR = paste0(L5HOUR," 00:00:00") #added because on some OS timestamps are deleted for midnight
                              if (length(unlist(strsplit(M5HOUR," "))) == 1) M5HOUR = paste0(M5HOUR," 00:00:00")
                              if (L5HOUR != "not detected") {
                                time_num = sum(as.numeric(unlist(strsplit(unlist(strsplit(L5HOUR," "))[2], ":"))) * c(3600, 60, 1)) / 3600
                                dsummary[di,fi] = time_num
                              } else {
                                dsummary[di,fi] = NA
                              }
                            }
                            ds_names[fi] = paste("L", wini, "TIME_num", sep = "");      fi = fi + 1
                            if (ignore == FALSE) {
                              if (M5HOUR != "not detected") {
                                time_num = sum(as.numeric(unlist(strsplit(unlist(strsplit(M5HOUR," "))[2], ":"))) * c(3600, 60, 1)) / 3600
                                dsummary[di, fi] = time_num
                              } else {
                                dsummary[di, fi] = NA
                              }
                            }
                            ds_names[fi] = paste("M", wini, "TIME_num", sep = "");      fi = fi + 1
                          }
                          #===============================================
                          NANS = which(is.nan(dsummary[di,]) == TRUE) #average of no values will results in NaN
                          if (length(NANS) > 0) dsummary[di,NANS] = ""
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
                            dsummary[di, fi + (bci-1)] = length(which(RLE$values == 1))
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
                            dsummary[di,fi + (bci - 1)] = length(which(RLE$values == 1))
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
                            dsummary[di,fi + (bci - 1)] = length(which(RLE$values == 1))
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
                            dsummary[di, fi] = length(which(RLE_LEVELS$values == levelsc))
                            ds_names[fi] = paste("Nblocks_", Lnames[levelsc + 1], sep = "");      fi = fi + 1
                          }
                          for (g in 1:4) {
                            dsummary[di, (fi + (g - 1))] = length(which(RLE_OLEVELS$values == g))
                          }
                          ds_names[fi:(fi + 3)] = c("Nblocks_day_total_IN", "Nblocks_day_total_LIG",
                                                    "Nblocks_day_total_MOD", "Nblocks_day_total_VIG")
                          fi = fi + 4
                          dsummary[di, fi:(fi + 6)] = c(params_phyact[["boutcriter.in"]],
                                                        params_phyact[["boutcriter.lig"]],
                                                        params_phyact[["boutcriter.mvpa"]],
                                                        paste(params_phyact[["boutdur.in"]], collapse = "_"),
                                                        paste(params_phyact[["boutdur.lig"]], collapse = "_"),
                                                        paste(params_phyact[["boutdur.mvpa"]], collapse = "_"),
                                                        params_phyact[["bout.metric"]])
                          ds_names[fi:(fi + 6)] = c("boutcriter.in", "boutcriter.lig", "boutcriter.mvpa",
                                                    "boutdur.in",  "boutdur.lig", "boutdur.mvpa", "bout.metric"); fi = fi + 7
                          #===========================
                          # Intensity gradient over waking hours
                          if (length(params_247[["iglevels"]]) > 0) {
                            q55 = cut(ts$ACC[sse[ts$diur[sse] == 0]], breaks = params_247[["iglevels"]], right = FALSE)
                            x_ig = zoo::rollmean(params_247[["iglevels"]], k = 2)
                            y_ig = (as.numeric(table(q55)) * ws3new)/60 #converting to minutes
                            dsummary[di,fi:(fi + 2)] = as.numeric(g.intensitygradient(x_ig, y_ig))
                            ds_names[fi:(fi + 2)] = c("ig_gradient", "ig_intercept", "ig_rsquared")
                            fi = fi + 3
                          }
                          #===============================================
                          # FRAGMENTATION for daytime hours only
                          if (length(params_phyact[["frag.metrics"]]) > 0) {
                            frag.out = g.fragmentation(frag.metrics = params_phyact[["frag.metrics"]],
                                                       LEVELS = LEVELS[sse[ts$diur[sse] == 0]],
                                                       Lnames = Lnames, xmin = 60/ws3new)
                            # fragmentation values come with a lot of decimal places
                            dsummary[di, fi:(fi + (length(frag.out) - 1))] = round(as.numeric(frag.out), digits = 5)
                            ds_names[fi:(fi + (length(frag.out) - 1))] = paste0("FRAG_", names(frag.out), "_day")
                            fi = fi + length(frag.out)
                          }
                          #===============================================
                          # LIGHT, IF AVAILABLE
                          if ("lightpeak" %in% colnames(ts) & length(params_247[["LUX_day_segments"]]) > 0) {
                            # mean LUX
                            dsummary[di,fi] =  round(max(ts$lightpeak[sse[ts$diur[sse] == 0]], na.rm = TRUE), digits = 1)
                            dsummary[di,fi + 1] = round(mean(ts$lightpeak[sse[ts$diur[sse] == 0]], na.rm = TRUE), digits = 1)
                            dsummary[di,fi + 2] = round(mean(ts$lightpeak[sse[ts$diur[sse] == 1]], na.rm = TRUE), digits = 1)
                            dsummary[di,fi + 3] = round(mean(ts$lightpeak[sse[ts$diur[sse] == 0 & ts$ACC[sse] > TRMi]], na.rm = TRUE),
                                                        digits = 1)
                            ds_names[fi:(fi + 3)] = c("LUX_max_day", "LUX_mean_day", "LUX_mean_spt", "LUX_mean_day_mvpa"); fi = fi + 4
                            # time in LUX ranges
                            Nluxt = length(params_247[["LUXthresholds"]])
                            for (lti in 1:Nluxt) {
                              if (lti < Nluxt) {
                                dsummary[di, fi + lti - 1] =  length(which(ts$lightpeak[sse[ts$diur[sse] == 0]] >= params_247[["LUXthresholds"]][lti] &
                                                                             ts$lightpeak[sse[ts$diur[sse] == 0]] < params_247[["LUXthresholds"]][lti + 1])) / (60/ws3new)
                                ds_names[fi + lti - 1] = paste0("LUX_min_", params_247[["LUXthresholds"]][lti], "_", params_247[["LUXthresholds"]][lti + 1], "_day")
                              } else {
                                dsummary[di, fi + lti - 1] =  length(which(ts$lightpeak[sse[ts$diur[sse] == 0]] >= params_247[["LUXthresholds"]][lti])) / (60/ws3new)
                                ds_names[fi + lti - 1] = paste0("LUX_min_", params_247[["LUXthresholds"]][lti], "_inf_day")
                              }
                            }
                            fi = fi + Nluxt
                            if (timewindowi == "WW") {
                              # LUX per segment of the day
                              luxperseg = g.part5.lux_persegment(ts, sse, params_247[["LUX_day_segments"]], ws3new)
                              dsummary[di, fi:(fi + (length(luxperseg$values) - 1))] = luxperseg$values
                              ds_names[fi:(fi + (length(luxperseg$values) - 1))] = luxperseg$names
                              fi = fi + length(luxperseg$values)
                            }
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
                    }
                  }
                  if (params_output[["save_ms5rawlevels"]] == TRUE) {
                    legendfile = paste0(metadatadir,ms5.outraw,"/behavioralcodes",as.Date(Sys.time()),".csv")
                    if (file.exists(legendfile) == FALSE) {
                      legendtable = data.frame(class_name = Lnames, class_id = 0:(length(Lnames) - 1), stringsAsFactors = FALSE)
                      write.csv(legendtable, file = legendfile, row.names = FALSE)
                    }
                    # I moved this bit of code to the end, because we want guider to be included (VvH April 2020)
                    rawlevels_fname =  paste0(metadatadir, ms5.outraw, "/", TRLi, "_", TRMi, "_", TRVi, "/",
                                              gsub(pattern = "[.]|rdata|csv|cwa|gt3x|bin", 
                                                   replacement = "", x = tolower(fnames.ms3[i])),
                                              "_", j, ".", params_output[["save_ms5raw_format"]])
                    
                    
                    
                    
                    # save time series to csv files
                    if (params_output[["do.sibreport"]] == TRUE & length(params_sleep[["nap_model"]]) > 0) {
                      napNonwear_col = "nap1_nonwear2"
                    } else {
                      napNonwear_col = c()
                    }
                    g.part5.savetimeseries(ts = ts[, c("time", "ACC", "diur", "nonwear", "guider", "window", napNonwear_col)],
                                           LEVELS = LEVELS,
                                           desiredtz = params_general[["desiredtz"]],
                                           rawlevels_fname = rawlevels_fname,
                                           save_ms5raw_format = params_output[["save_ms5raw_format"]],
                                           save_ms5raw_without_invalid = params_output[["save_ms5raw_without_invalid"]],
                                           DaCleanFile = DaCleanFile,
                                           includedaycrit.part5 = params_cleaning[["includedaycrit.part5"]], ID = ID)
                  }
                }
              }
            }
          }
        }
        #remove NA values
        for (kik in 1:ncol(dsummary)) {
          naval = which(is.na(dsummary[, kik]) == TRUE)
          if (length(naval) > 0) dsummary[naval, kik] = ""
        }
        output = data.frame(dsummary,stringsAsFactors = FALSE)
        names(output) = ds_names
        if (params_cleaning[["excludefirstlast.part5"]] == TRUE) {
          output$window_number = as.numeric(output$window_number)
          cells2exclude = c(which(output$window_number == min(output$window_number,na.rm = TRUE)),
                            which(output$window_number == max(output$window_number,na.rm = TRUE)))
          if (length(cells2exclude) > 0) {
            output = output[-cells2exclude,]
          }
        }
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
        lastcolumn = which(colnames(output) == "bout.metric")
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
            if (length(emptycols) > 0) output = output[-emptycols]
          }

          if (length(output) > 0) {
            if (nrow(output) > 0) {
              save(output, file = paste(metadatadir,
                                        ms5.out, "/", fnames.ms3[i], sep = ""))
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
      cl <- parallel::makeCluster(Ncores2use) #not to overload your computer
      doParallel::registerDoParallel(cl)
    } else {
      cat(paste0("\nparallel processing not possible because number of available cores (",Ncores,") < 4"))
      params_general[["do.parallel"]] = FALSE
    }
    cat(paste0('\n Busy processing ... see ', metadatadir, ms5.out, ' for progress\n'))
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
                           "g.part5.addfirstwake", "g.part5.addsib",
                           "g.part5.definedays", "g.part5.fixmissingnight",
                           "g.part5.onsetwaketiming", "g.part5.wakesleepwindows",
                           "g.part5.savetimeseries", "g.fragmentation", "g.intensitygradient",
                           "g.part5.handle_lux_extremes", "g.part5.lux_persegment", "g.sibreport",
                           "extract_params", "load_params", "check_params")
      errhand = 'stop'
    }
    fe_dopar = foreach::`%dopar%`
    fe_do = foreach::`%do%`
    i = 0 # declare i because foreach uses it, without declaring it
    `%myinfix%` = ifelse(params_general[["do.parallel"]], fe_dopar, fe_do)
    output_list = foreach::foreach(i = f0:f1,  .packages = packages2passon,
                                   .export = functions2passon, .errorhandling = errhand) %myinfix% {
                                     tryCatchResult = tryCatch({
                                       main_part5(i, metadatadir, f0, f1,
                                                  params_sleep, params_metrics,
                                                  params_247, params_phyact,
                                                  params_cleaning, params_output,
                                                  params_general, ms5.out, ms5.outraw,
                                                  fnames.ms3, sleeplog, logs_diaries,
                                                  extractfilenames, referencefnames, folderstructure,
                                                  fullfilenames, foldername)
                                     })
                                     return(tryCatchResult)
                                   }

    on.exit(parallel::stopCluster(cl))
    for (oli in 1:length(output_list)) { # logged error and warning messages
      if (is.null(unlist(output_list[oli])) == FALSE) {
        cat(paste0("\nErrors and warnings for ", fnames.ms3[oli]))
        print(unlist(output_list[oli])) # print any error and warnings observed
      }
    }
  } else {
    for (i in f0:f1) {
      cat(paste0(i, " "))
      main_part5(i, metadatadir, f0, f1,
                 params_sleep, params_metrics,
                 params_247, params_phyact,
                 params_cleaning, params_output,
                 params_general, ms5.out, ms5.outraw,
                 fnames.ms3, sleeplog, logs_diaries,
                 extractfilenames, referencefnames, folderstructure,
                 fullfilenames, foldername)
    }
  }
}
