g.part4 = function(datadir = c(), metadatadir = c(), f0 = f0, f1 = f1,
                   params_sleep = c(), params_metrics = c(),
                   params_cleaning = c(), params_output = c(),
                   params_general = c(), verbose = TRUE, ...) {
  #----------------------------------------------------------
  # Extract and check parameters
  input = list(...)
  params = extract_params(params_sleep = params_sleep, params_metrics = params_metrics,
                          params_general = params_general, params_output = params_output,
                          params_cleaning = params_cleaning, input = input,
                          params2check = c("sleep", "metrics",
                                           "general", "output", "cleaning")) # load default parameters
  params_sleep = params$params_sleep
  params_metrics = params$params_metrics
  params_cleaning = params$params_cleaning
  params_output = params$params_output
  params_general = params$params_general
  if (exists("relyonsleeplog") == TRUE & exists("relyonguider") == FALSE) relyonguider = params_sleep[["relyonsleeplog"]]
  # description: function to load sleep detection from g.part3 and to convert it into night-specific summary measures of sleep,
  # possibly aided by sleep log/diary information (if available and provided by end-user)
  nnpp = 40 # number of nights to be displayed in the report (hard-coded not a critical parameter for most scenarios)
  #------------------------------------------------
  ms3.out = "/meta/ms3.out"
  meta.sleep.folder = paste0(metadatadir, ms3.out)
  ms4.out = "/meta/ms4.out"
  checkMilestoneFolders(metadatadir, partNumber = 4)
  #------------------------------------------------
  # Get sleeplog data
  if (length(params_sleep[["loglocation"]]) > 0) {
    dolog = TRUE
  } else {
    dolog = FALSE
  }
  if (dolog == TRUE) {
    sleeplogRDataFile = paste0(metadatadir,"/meta/sleeplog_", basename(params_sleep[["loglocation"]]), ".RData")
    # only re-process sleeplog if sleeplog.RData does not exist 
    # or if sleeplog is from a date equal to or after sleeplog.RData
    # or if sleeplog.Rdata file is older than the news files in milestone 3.
    if (!file.exists(sleeplogRDataFile) || 
        file.info(params_sleep[["loglocation"]])$mtime >= file.info(sleeplogRDataFile)$mtime ||
        max(file.info(dir(meta.sleep.folder, full.names = TRUE))$mtime) >= file.info(sleeplogRDataFile)$mtime) {
      logs_diaries = g.loadlog(params_sleep[["loglocation"]], 
                               coln1 = params_sleep[["coln1"]],
                               colid = params_sleep[["colid"]],
                               meta.sleep.folder = meta.sleep.folder,
                               desiredtz = params_general[["desiredtz"]],
                               sleepwindowType = params_sleep[["sleepwindowType"]])
      
      if (params_sleep[["sleepwindowType"]] == "SPT" && length(logs_diaries$bedlog) > 0 &&
          length(logs_diaries$sleeplog) == 0) {
        stop(paste0("The sleep diary as provided only appears to have time indicators",
                    " for time in bed and not for the Sleep Period Time window, while",
                    " parameter sleepwindowType is set to SPT (default). Either change sleepwindowType to",
                    " \"TimeInBed\" or change your sleep diary column names for sleep timing",
                    " to \"wakeup\" and \"sleeponset\"."), call. = FALSE)
      } else if (params_sleep[["sleepwindowType"]] == "TimeInBed" && length(logs_diaries$bedlog) == 0 &&
                 length(logs_diaries$sleeplog) > 0) {
        stop(paste0("The sleep diary as provided only appears to have time indicators",
                    " for SPT and not for the Time in Bed, while",
                    " parameter sleepwindowType is set to TimeInBed. Either change sleepwindowType to",
                    " \"SPT\" or change your sleep diary column names for sleep timing",
                    " to \"outbed\" and \"inbed\"."), call. = FALSE)
      }
      save(logs_diaries, file = sleeplogRDataFile)
    } else {
      load(file = sleeplogRDataFile)
    }
    if (params_sleep[["sleepwindowType"]] == "TimeInBed" && length(logs_diaries$bedlog) > 0) {
      sleeplog = logs_diaries$bedlog
    } else {
      sleeplog = logs_diaries$sleeplog
    }
    sleeplog$night = as.numeric(sleeplog$night)
    sleeplog$duration = as.numeric(sleeplog$duration)
    if (is.null(logs_diaries$sleeplog) && is.null(logs_diaries$bedlog)) {
      dolog = FALSE
      rm(sleeplog)
    }
  }
  #------------------------------------------------
  # get list of accelerometer milestone data files from sleep (produced by g.part3)
  fnames = dir(meta.sleep.folder)

  if (f1 > length(fnames)) f1 = length(fnames)
  if (f0 > length(fnames)) f0 = 1
  if (f1 == 0 | length(f1) == 0 | f1 > length(fnames))  f1 = length(fnames)
  #-----------------------------------------------------
  # related to plotting
  cnt = 1 #counter to keep track of how many nights have been plotted in the visual report
  idlabels = rep(0,nnpp) # initialize vaiable for plot labels
  pagei = 1
  cnt67 = 1  # counter used to decide whether to create a new pdf file for the plots
  #-----------------------------------------------------
  # initialize output variable names
  colnamesnightsummary = c("ID", "night", "sleeponset", "wakeup", "SptDuration", "sleepparam", "guider_onset",
                           "guider_wakeup", "guider_SptDuration", "error_onset", "error_wake", "error_dur", "fraction_night_invalid",
                           "SleepDurationInSpt", "WASO", "duration_sib_wakinghours",
                           "number_sib_sleepperiod", "number_of_awakenings",
                           "number_sib_wakinghours", "duration_sib_wakinghours_atleast15min",
                           "sleeponset_ts", "wakeup_ts", "guider_onset_ts",  "guider_wakeup_ts",
                           "sleeplatency", "sleepefficiency", "page", "daysleeper", "weekday", "calendar_date",
                           "filename", "cleaningcode", "sleeplog_used", "sleeplog_ID", "acc_available", "guider", "SleepRegularityIndex1", "SriFractionValid",
                           "longitudinal_axis") #
  
  
  if (params_output[["storefolderstructure"]] == TRUE) {
    colnamesnightsummary = c(colnamesnightsummary, "filename_dir", "foldername")
  }
  # initialize variable to hold sleeplog derived sleep duration if not sleep log was used then the
  # estimates of the sleep period time window will be used instead
  logdur = rep(0, length(fnames))
  # ======================================================================== check which files have
  # already been processed, such that no double work is done
  ffdone = c()
  ms4.out = "/meta/ms4.out"
  fnames.ms4 = dir(paste0(metadatadir, ms4.out))
  # fnames.ms4 = sort(fnames.ms4)
  ffdone = fnames.ms4
  # ffdone a matrix with all the binary filenames that have been processed
  # fnames = sort(fnames)
  #--------------------------------
  # get original file path of the accelerometer (some studies may like to keep track of original folder
  # structure if their study structure is embodied in folder structure)
  if (params_output[["storefolderstructure"]] == TRUE) {
    filelist = FALSE
    if (length(datadir) == 1) {
      # could be a directory or one file
      if (length(unlist(strsplit(datadir, split = "[.](cs|bi|cw)"))) > 1)
        filelist = TRUE
    } else {
      # multiple files
      filelist = TRUE
    }
    if (filelist == FALSE) {
      fnamesfull = dir(datadir, recursive = TRUE, pattern = "[.](csv|bin|cwa)")
    } else {
      fnamesfull = datadir
    }
    f17 = function(X) {
      out = unlist(strsplit(X, "/"))
      f17 = out[(length(out) - 1)]
    }
    ffd = ffp = rep("", length(fnamesfull))
    if (length(fnamesfull) > 0) {
      fnamesshort = basename(fnamesfull)
      foldername = apply(X = as.matrix(fnamesfull), MARGIN = 1, FUN = f17)
      for (i in 1:length(fnames)) {
        #
        ff = as.character(unlist(strsplit(fnames[i], ".RDa"))[1])
        if (length(which(fnamesshort == ff)) > 0) {
          ffd[i] = fnamesfull[which(fnamesshort == ff)]
          ffp[i] = foldername[which(fnamesshort == ff)]
        }
      }
    }
  }
  convertHRsinceprevMN2Clocktime = function(x) {
    # x = hours Since Previous Midnight
    if (x > 24) x = x - 24
    HR = floor(x)
    MI = floor((x - HR) * 60)
    SE = round(((x - HR) - (MI/60)) * 3600)
    if (SE == 60) {
      MI = MI + 1
      SE = 0
    }
    if (MI == 60) {
      HR = HR + 1
      MI = 0
    }
    if (HR == 24) HR = 0
    if (HR < 10) HR = paste0("0", HR)
    if (MI < 10) MI = paste0("0", MI)
    if (SE < 10) SE = paste0("0", SE)
    return(paste0(HR, ":", MI, ":", SE))
  }
  if (length(params_cleaning[["data_cleaning_file"]]) > 0) {
    # allow for forced relying on guider based on external data_cleaning_file
    DaCleanFile = data.table::fread(params_cleaning[["data_cleaning_file"]], data.table = FALSE)
  }
  # =================================================================
  # start of loop through the
  # participants
  for (i in f0:f1) {
    tail_expansion_log = NULL
    # decide whether file was processed before
    if (params_general[["overwrite"]] == TRUE) {
      skip = 0  # this will make that analyses is done regardless of whether it was done before
    } else {
      skip = 0  #do not skip this file
      if (length(ffdone) > 0) {
        if (length(which(ffdone == fnames[i])) > 0)
          skip = 1  #skip this file because it was analysed before')
      }
    }
    if (skip == 0) {
      if (verbose == TRUE) cat(paste0(" ", i))
      addlegend = FALSE
      if (cnt67 == 1) {
        # only create new pdf if there is actually new plots to be generated keep pdf for QC
        # purposes
        if (params_output[["do.visual"]] == TRUE) {
          pdf(file = paste0(metadatadir, "/results/visualisation_sleep.pdf"), width = 8.27, height = 11.69)
          par(mar = c(4, 5, 1, 2) + 0.1)
          plot(c(0, 0), c(1, 1), xlim = c(12, 36), ylim = c(0, nnpp), col = "white", axes = FALSE, xlab = "time",
               ylab = "", main = paste0("Page ", pagei))
          axis(side = 1, at = 12:36, labels = c(12:24, 1:12), cex.axis = 0.7)
          abline(v = c(18, 24, 30), lwd = 0.2, lty = 2)
          abline(v = c(15, 21, 27, 33), lwd = 0.2, lty = 3, col = "grey")
          addlegend = TRUE
        }
        cnt67 = 2
      }
      nightsummary = as.data.frame(matrix(0, 0, length(colnamesnightsummary)))
      colnames(nightsummary) = colnamesnightsummary
      if (params_sleep[["sleepwindowType"]] == "TimeInBed") {
        colnames(nightsummary) = gsub(replacement = "guider_inbedStart", pattern = "guider_onset", x = colnames(nightsummary))
        colnames(nightsummary) = gsub(replacement = "guider_inbedEnd", pattern = "guider_wakeup", x = colnames(nightsummary))
        colnames(nightsummary) = gsub(replacement = "guider_inbedDuration", pattern = "guider_SptDuration",
                                      x = colnames(nightsummary))
      }
      sumi = 1  # counter to keep track of where we are in filling the output matrix 'nightsummary'
      ID  = SPTE_end = SPTE_start = L5list = sib.cla.sum = longitudinal_axis = part3_guider = c()
      # load milestone 3 data (RData files), check whether there is data, identify id numbers...
      load(paste0(meta.sleep.folder, "/", fnames[i]))
      accid = c()
      logid = NA  # keep track of what log id matched to accid
      if (length(ID) > 0) {
        if (!is.na(ID)) {
          # continue with same ID as extracted in GGIR parts 1-3:
          accid = ID
        }
        # if ID not available, function part4_extractid will attempt to extract it from the file name
      }
      if (exists("SRI") == FALSE) SRI = NA
      if (nrow(sib.cla.sum) != 0) {
        # there needs to be some information
        sib.cla.sum$sib.onset.time = iso8601chartime2POSIX(sib.cla.sum$sib.onset.time, tz = params_general[["desiredtz"]])
        sib.cla.sum$sib.end.time = iso8601chartime2POSIX(sib.cla.sum$sib.end.time, tz = params_general[["desiredtz"]])
        # extract the identifier from accelerometer data and matching indices of sleeplog:
        idwi = g.part4_extractid(params_general[["idloc"]], fname = fnames[i],
                                 dolog,
                                 sleeplog, accid = accid)
        accid = idwi$accid
        wi = idwi$matching_indices_sleeplog
        #-----------------------------------------------------------
        # create overview of night numbers in the data file: nnightlist
        if (dolog == TRUE) {
          logid = sleeplog$ID[wi][1]
          first_night = min(min(sib.cla.sum$night), 
                            min(as.numeric(sleeplog$night)))
          last_night = max(max(sib.cla.sum$night), 
                           max(as.numeric(sleeplog$night[which(sleeplog$ID == accid)])))
        } else {
          first_night = min(sib.cla.sum$night)
          last_night = max(sib.cla.sum$night)
        }
        nnightlist = first_night:last_night
        
        if (length(nnightlist) < length(wi)) {
          nnightlist = nnightlist[1:length(wi)]
        }
        # create overview of which night numbers in the file that have a value and are not equal
        # to zero
        nnights.list = nnightlist
        nnights.list = nnights.list[which(is.na(nnights.list) == FALSE)]
        if (params_cleaning[["excludefirstlast"]] == TRUE & params_cleaning[["excludelast.part4"]] == FALSE & params_cleaning[["excludefirst.part4"]] == FALSE) {
          # exclude first and last night
          if (length(nnights.list) >= 3) {
            nnights.list = nnights.list[2:(length(nnights.list) - 1)]
          } else {
            nnights.list = c()
          }
        } else if (params_cleaning[["excludelast.part4"]] == FALSE & params_cleaning[["excludefirst.part4"]] == TRUE) {
          if (length(nnights.list) >= 2) {
            nnights.list = nnights.list[2:length(nnights.list)]
          } else {
            nnights.list = c()
          }
        } else if (params_cleaning[["excludelast.part4"]] == TRUE & params_cleaning[["excludefirst.part4"]] == FALSE) {
          if (length(nnights.list) >= 2) {
            nnights.list = nnights.list[1:(length(nnights.list) - 1)]
          } else {
            nnights.list = c()
          }
        }
        # initialize variables calendardate and daysleeper from the nnights.list variable
        calendar_date = wdayname = rep("", length(nnights.list))
        # daysleeper variable to keep track of whether the person woke up after noon or a certain
        # day
        daysleeper = rep(FALSE, length(nnights.list))
        ###########################################################
        nightj = 1
        # max(c(max(nnights.list),
        guider.df = data.frame(matrix(NA, length(nnights.list), 5), stringsAsFactors = FALSE)
        names(guider.df) = c("ID", "night", "duration", "sleeponset", "sleepwake")
        guider.df$night = nnights.list
        if (dolog == TRUE) {
          if (length(wi) > 0) {
            wi2 = wi[which(sleeplog$night[wi] %in% guider.df$night)]
            guider.df[which(guider.df$night %in% sleeplog$night[wi2]),] = sleeplog[wi2,]
          }
        }
        for (j in nnights.list) {
          # go through the nights get default onset and wake (based on sleeplog or on heuristic
          # algorithms) def.noc.sleep is an input argument the GGIR user can use to specify what
          # detection strategy is used in the absense of a sleep diary
          if ((length(params_sleep[["def.noc.sleep"]]) == 0 ||
               length(SPTE_start) == 0 ||
               length(SPTE_start[which(is.na(SPTE_start) == FALSE)]) == 0) &&
              length(params_sleep[["def.noc.sleep"]]) != 2) {
            # use L5+/-6hr algorithm if SPTE fails OR if the user explicitely asks for it (length
            # zero argument)
            guider = "notavailable"
            if (length(L5list) > 0 & length(L5list) >= j) {
              defaultGuiderOnset = L5list[j] - 6
              defaultGuiderWake = L5list[j] + 6
              guider = "L512"
            } else {
              # This should never happen, but just as a final backup
              defaultGuiderOnset = 21
              defaultGuiderWake = 31
            }
            defaultGuider = guider
          } else if ((length(params_sleep[["def.noc.sleep"]]) == 1 ||
                      length(params_sleep[["loglocation"]]) != 0) &&
                     length(SPTE_start) != 0) {
            # use SPTE algorithm (inside the g.sib.det function) as backup for sleeplog OR if user
            # explicitely asks for it
            defaultGuiderOnset = SPTE_start[j]
            defaultGuiderWake = SPTE_end[j]
            defaultGuider = part3_guider[j] # HDCZA, NotWorn, HorAngle (or plus invalid)
            if (is.null(defaultGuider)) {
              # this ensures compatibility with previous versions in which part3_guider was not stored
              # for newer version we expect defaultGuider to always not be NULL
              guider = params_sleep[["HASPT.algo"]][1]
              defaultGuider = guider
            } else {
              if (is.na(defaultGuider)) { 
                # No default guider available, for example when sleeplog is 
                # available but not accelerometer
                # In that case guider will be set to "sleeplog" later on
              } else {
                guider = defaultGuider
              }
            }
            if (is.na(defaultGuiderOnset) == TRUE) {
              # If SPTE was not derived for this night, use average estimate for other nights
              availableestimate = which(is.na(SPTE_start) == FALSE)
              cleaningcode = 6
              if (length(availableestimate) > 0) {
                defaultGuiderOnset = mean(SPTE_start[availableestimate])
                guider = defaultGuider = names(sort(table(part3_guider[availableestimate]), decreasing = TRUE)[1])
              } else {
                defaultGuiderOnset = L5list[j] - 6
                guider = "L512"
              }
            }
            if (is.na(defaultGuiderWake) == TRUE) {
              # If SPTE was not derived for this night, use average estimate for other nights
              availableestimate = which(is.na(SPTE_end) == FALSE)
              cleaningcode = 6
              if (length(availableestimate) > 0) {
                defaultGuiderWake = mean(SPTE_end[availableestimate])
                guider = defaultGuider = names(sort(table(part3_guider[availableestimate]), decreasing = TRUE)[1])
              } else {
                defaultGuiderWake = L5list[j] + 6
                guider = "L512"
              }
            }
          } else if (length(params_sleep[["def.noc.sleep"]]) == 2) {
            # use constant onset and waking time as specified with def.noc.sleep argument
            defaultGuiderOnset = params_sleep[["def.noc.sleep"]][1]  #onset
            defaultGuiderWake = params_sleep[["def.noc.sleep"]][2]  #wake
            defaultGuider = guider = "setwindow"
          }
          if (defaultGuiderOnset >= 24) {
            defaultGuiderOnset = defaultGuiderOnset - 24
          }
          if (defaultGuiderWake >= 24) {
            defaultGuiderWake = defaultGuiderWake - 24
          }
          defaultdur = defaultGuiderWake - defaultGuiderOnset  #default sleep duration based on sleeplog, L5+/-6hr, or HDCZA algorithm
          sleeplog_used = FALSE
          if (dolog == TRUE) {
            #-----------------------------------------------------------
            # If sleep log is available and values are not missing
            if (all(!is.na(guider.df[which(guider.df$night == j), 4:5]))) {
              sleeplog_used = TRUE
            }
          }
          if (sleeplog_used == FALSE) {
            #-----------------------------------------------------------
            # If sleep log is not available available, use default values calculated above (with
            # the heuristic algorithm HDCZA or if that fails L5+/-6hr.
            guider.df[which(guider.df$night == j), 1:5] = c(accid, j, defaultdur, convertHRsinceprevMN2Clocktime(defaultGuiderOnset),
                                                            convertHRsinceprevMN2Clocktime(defaultGuiderWake))
            cleaningcode = 1
          }
          nightj = nightj + 1
          # keep track of whether enough accelerometer data is available for each night
          acc_available = TRUE  #default assumption
          # initialize dataframe to hold sleep period overview:
          spocum = data.frame(nb = numeric(0), start = numeric(0),  end = numeric(0),
                              overlapGuider = numeric(0), def = character(0))
          
          spocumi = 1  # counter for sleep periods
          # continue now with the specific data of the night
          
          guider.df2 = guider.df[which(guider.df$night == j), ]
          # ================================================================================ get
          # sleeplog (or HDCZA or L5+/-6hr algorithm) onset and waking time and assess whether it
          # is a nightworker onset
          tmp1 = format(guider.df2$sleeponset[1])
          tmp2 = unlist(strsplit(tmp1, ":"))
          GuiderOnset = as.numeric(tmp2[1]) + (as.numeric(tmp2[2])/60) + (as.numeric(tmp2[3])/3600)
          # wake
          tmp4 = format(guider.df2$sleepwake[1])
          tmp5 = unlist(strsplit(tmp4, ":"))
          GuiderWake = as.numeric(tmp5[1]) + (as.numeric(tmp5[2])/60) + (as.numeric(tmp5[3])/3600)
          # Assess whether it is a daysleeper or a nightsleeper
          daysleeper[j] = FALSE  # default
          if (is.na(GuiderOnset) == FALSE & is.na(GuiderWake) == FALSE & tmp1 != "" & tmp4 != "") {
            # is the sleep log valid?  transform possible Single Digit clock times to Double
            # Digits: hours, minutes and seconds
            doubleDigitClocktime = function(x) {
              x = unlist(strsplit(x, ":"))
              xHR = as.numeric(x[1])
              xMI = as.numeric(x[2])
              xSE = as.numeric(x[3])
              if (xHR < 10) xHR = paste0("0", xHR)
              if (xMI < 10) xMI = paste0("0", xMI)
              if (xSE < 10) xSE = paste0("0", xSE)
              x = paste0(xHR, ":", xMI, ":", xSE)
              return(x)
            }
            tmp1 = doubleDigitClocktime(tmp1)
            tmp4 = doubleDigitClocktime(tmp4)
            #------------------------------------------------------------------
            # does sleep period overlap with noon? If yes, then classify as daysleeper
            if (GuiderWake > 12 & GuiderOnset < 12) daysleeper[j] = TRUE
            if (GuiderWake > 12 & GuiderOnset > GuiderWake) daysleeper[j] = TRUE
            # change time stamps to be a continues time
            if (GuiderOnset < 12) GuiderOnset = GuiderOnset + 24  #shift 24 hours to create continues time
            if (GuiderWake <= 12) GuiderWake = GuiderWake + 24  #shift 24 hours to create continues time
            if (GuiderWake > 12 & GuiderWake < 18 & daysleeper[j] == TRUE) GuiderWake = GuiderWake + 24  # NEW 10/5/2018 by Vincent
            if (daysleeper[j] == TRUE) {
              logdur[i] = GuiderOnset - GuiderWake
            } else {
              logdur[i] = GuiderWake - GuiderOnset
            }
            if (sleeplog_used == TRUE) {
              cleaningcode = 0
              guider = "sleeplog"
            }
          } else {
            GuiderOnset = defaultGuiderOnset  #use default assumption about onset
            GuiderWake = defaultGuiderWake + 24  #use default assumption about wake
            logdur[i] = GuiderWake - GuiderOnset
            cleaningcode = 1  # no diary available for this night, so fall back on detaults
            sleeplog_used = FALSE
          }
          #-----------------------------------------
          # plan analysis according to knowledge about whether it is a daysleeper or not if you
          # are not excluding the last day and is a daysleeper and not the last night
          if (params_cleaning[["excludefirstlast"]] == FALSE) {
            if (daysleeper[j] == TRUE & j != max(nnights.list)) {
              loaddays = 2
            } else {
              loaddays = 1
            }
            if (daysleeper[j] == TRUE & j == max(nnights.list)) {
              # and is a daysleeper and the last night
              daysleeper[j] = FALSE  #treat it like a normal day
              loaddays = 1
              if (GuiderWake > 36)  GuiderWake = 36  #round diary to noon
              logdur[i] = GuiderWake - GuiderOnset
            }
          } else {
            # if you are excluding the last day dont worrry about whether it is the last day,
            # because it is not included
            if (daysleeper[j] == TRUE) {
              loaddays = 2
            } else {
              loaddays = 1
            }
          }
          # now generate empty overview for this night / person
          dummyspo = data.frame(nb = numeric(1), start = numeric(1),  end = numeric(1),
                                overlapGuider = numeric(1), def = character(1), duration = numeric(1))
          dummyspo$nb[1] = 1
          spo_day = c()
          spo_day_exists = FALSE
          # ============================================================================================
          # Loop through sleep definitions
          defs = unique(sib.cla.sum$definition)  # definition of sleep episode metric (see van Hees 2015 PLoSONE paper)
          for (k in defs) {
            # load twice if daysleeper because we also need data from the afternoon on the next day
            for (loaddaysi in 1:loaddays) {
              qq = sib.cla.sum
              sleepdet = qq[which(qq$night == (j + (loaddaysi - 1))), ]
              if (nrow(sleepdet) == 0) {
                if (spocumi == 1) {
                  spocum = dummyspo
                } else {
                  spocum = rbind(spocum, dummyspo)
                }
                spocumi = spocumi + 1
                cleaningcode = 3
                acc_available = FALSE
              } else if (length(tail_expansion_log) != 0 & sumi == max(nnightlist)) {
                # so that we skip the visualization of last night when data has been expanded
                # also, cleaningcode is more realistic as acc data is not available for this night
                cleaningcode = 3
                acc_available = FALSE
              } else {
                acc_available = TRUE
              }
              
              if (nrow(sleepdet) == 0) next
              ki = which(sleepdet$definition == k)
              if (length(ki) == 0) next
              sleepdet.t = sleepdet[ki, ]
              if (loaddaysi == 1) remember_fraction_invalid_day1 = sleepdet.t$fraction.night.invalid[1]
              # now get sleep periods
              nsp = length(unique(sleepdet.t$sib.period))  #number of sleep periods
              spo = data.frame(nb = numeric(nsp), start = numeric(nsp),  end = numeric(nsp),
                               overlapGuider = numeric(nsp), def = character(nsp))
              if (nsp <= 1 & unique(sleepdet.t$sib.period)[1] == 0) {
                # no sleep periods
                spo$nb[1] = 1
                spo[1, c("start", "end", "overlapGuider")] = 0
                spo$def[1] = k
                if (daysleeper[j] == TRUE) {
                  tmpCmd = paste0("spo_day", k, "= c()")
                  eval(parse(text = tmpCmd))  ##
                  spo_day_exists = TRUE
                }
              } else {
                DD = g.create.sp.mat(nsp, spo, sleepdet.t, daysleep = daysleeper[j])
                if (loaddaysi == 1) {
                  wdayname[j] = DD$wdayname
                  calendar_date[j] = DD$calendar_date
                }
                spo = DD$spo
                if (daysleeper[j] == TRUE) {
                  if (loaddaysi == 1) {
                    w1 = which(spo$end >= 18)  #only use periods ending after 6pm
                    if (length(w1) > 0) {
                      spo = spo[w1, ]
                      if (nrow(spo) == 1) {
                        if (spo$start[1] <= 18) spo$start[1] = 18  #turn start time on 1st day before 6pm to 6pm
                      } else {
                        spo$start[which(spo$start <= 18)] = 18  #turn start times on 1st day before 6pm to 6pm
                      }
                      tmpCmd = paste0("spo_day", k, "= spo")  #spo needs to be rememered specific to definition
                      eval(parse(text = tmpCmd))
                      spo_day_exists = TRUE
                    } else {
                      tmpCmd = paste0("spo_day", k, "= c()")
                      eval(parse(text = tmpCmd))
                      spo_day_exists = TRUE
                    }
                  } else if (loaddaysi == 2 & spo_day_exists == TRUE) {
                    # length check added because day may have been skipped
                    w2 = which(spo$start < 18)  #only use periods starting before 6pm
                    if (length(w2) > 0) {
                      spo = spo[w2, ]
                      if (ncol(spo) == 1) spo = t(spo)
                      if (nrow(spo) == 1) {
                        if (spo$end[1] > 18) spo$end[1] = 18  #turn end time on 2nd day after 6pm to 6pm
                      } else {
                        spo$end[which(spo$end > 18)] = 18  #turn end times on 2nd day after 6pm to 6pm
                      }
                      spo[, c("start", "end")] = spo[, c("start", "end")] + 24  # + 24 to create continues timelines for day 2 relative to day 1
                      tmpCmd = paste0("spo_day2", k, "= spo")  #spo needs to be rememered specific to definition
                      eval(parse(text = tmpCmd))
                    } else {
                      tmpCmd = paste0("spo_day2", k, "= c()")
                      eval(parse(text = tmpCmd))
                    }
                    # attach days together as being one day
                    name1 = paste0("spo_day", k)
                    name2 = paste0("spo_day2", k)
                    tmpCmd = paste0("spo = rbind(", name1, ",", name2, ")")
                    eval(parse(text = tmpCmd))
                  }
                }
                if (daysleeper[j] == TRUE) {
                  if (GuiderWake < 21 & GuiderWake > 12 & GuiderOnset > GuiderWake) {
                    # waking up in the afternoon should have value above 36
                    GuiderWake = GuiderWake + 24
                  }
                }
                # Detect whether none of the SIBs overlaps with the SPT, if this is the case then
                # probably the night is not going to be very useful for sleep analysis however,
                # for part 5 we may still want to have some estimate to define the waking-up to
                # waking windows.  So, we will now add two tiny artificial sibs (1-minute) at the
                # beginning and end of the guider-based SPT to make that the SPT is still detected
                # guided entirely by the guider.  and add cleaningcode = 5, this means that the
                # night will be omitted from the part 4 cleaned results, but is still available
                # for inspection in the full part 4 results and available for part 5, because in
                # part 5 we are only interested in the edges of the SPT and not what happens in
                # it.
                relyonguider_thisnight = FALSE
                # user specified to rely on guider in data cleaning file:
                # relyonguider_thisnight = TRUE and cleaningcode = 5
                if (length(params_cleaning[["data_cleaning_file"]]) > 0) {
                  if (length(which(DaCleanFile$relyonguider_part4 == j &
                                   DaCleanFile$ID == accid)) > 0) {
                    relyonguider_thisnight = TRUE
                    cleaningcode = 5 # user specified to rely on guider
                  }
                }
                # No SIBs overlap with SPT window
                # relyonguider_thisnight = TRUE and cleaningcode = 5
                if (length(spo) == 0) {
                  # add empty spo object, in case it was removed above
                  # we do this because code below assumes that spo is a matrix
                  spo = data.frame(nb = numeric(1), start = numeric(1),  end = numeric(1),
                                   overlapGuider = numeric(1), def = character(1))
                  spo$nb[1] = 1
                  spo[1, 2:4] = 0
                  spo$def[1] = k
                }
                if (length(which(spo$start < GuiderWake &
                                 spo$end > GuiderOnset)) == 0) {
                  relyonguider_thisnight = TRUE
                  cleaningcode = 5 
                }
                # if invalid time was used in part3 with HASPT.ignore.invalid,
                # then rely on guider, but cleaningcode should not be 5
                if (grepl("+invalid", guider) | grepl("+invalid", defaultGuider)) {
                  relyonguider_thisnight = TRUE # rely on guider because some nonwear was used to help the slep identification
                }
                # If no SIBs overlap with the guider window
                if (relyonguider_thisnight == TRUE) {
                  if (guider != "NotWorn") {
                    newlines = rbind(spo[1, ], spo[1, ])
                    newlines[1, 1:4] = c(nrow(spo) + 1, GuiderOnset, GuiderOnset + 1/60, 1)
                    newlines[2, 1:4] = c(nrow(spo) + 1, GuiderWake - 1/60, GuiderWake, 1)
                    spo = rbind(spo, newlines) # When NotWorn was used then fully trust on guider and ignore sibs detected
                  } else {
                    newlines = spo[1, ] # initialise object
                    newlines[1, 1:4] = c(nrow(spo) + 1, GuiderOnset, GuiderWake, 1)
                    spo = newlines
                  }
                  spo = spo[order(spo$start), ]
                  spo$nb = 1:nrow(spo)
                  relyonguider_thisnight = TRUE
                }
                # spo is now a data.frame of start and end for each sib (sustained inactivity bout)
                # Classify as being part of the guider window or not
                for (evi in 1:nrow(spo)) {
                  if (spo$start[evi] < GuiderWake && spo$end[evi] > GuiderOnset) {
                    if ((params_sleep[["sleepwindowType"]] == "TimeInBed" || guider == "markerbutton") &&
                        sum(params_sleep[["sib_must_fully_overlap_with_TimeInBed"]]) != 0) {
                      if (all(params_sleep[["sib_must_fully_overlap_with_TimeInBed"]]) &&
                          spo$start[evi] > GuiderOnset && spo$end[evi] < GuiderWake) {
                        spo$overlapGuider[evi] = 1  # Consider only full overlap
                      }
                      if (all(params_sleep[["sib_must_fully_overlap_with_TimeInBed"]] == c(FALSE, TRUE)) &&
                          spo$end[evi] < GuiderWake) {
                        spo$overlapGuider[evi] = 1 # Consider only full overlap
                      }
                      if (all(params_sleep[["sib_must_fully_overlap_with_TimeInBed"]] == c(TRUE, FALSE)) &&
                          spo$start[evi] > GuiderOnset) {
                        spo$overlapGuider[evi] = 1  # Consider only full overlap
                      }
                    } else {
                      spo$overlapGuider[evi] = 1  # Consider partial overlap
                    }
                    if (params_sleep[["relyonguider"]] == TRUE | relyonguider_thisnight == TRUE) {
                      # Redefine sib start and end if it overlaps with guider
                      # to match guider 
                      if ((spo$start[evi] < GuiderWake && spo$end[evi] > GuiderWake) |
                          (spo$start[evi] < GuiderWake && spo$end[evi] < spo$start[evi])) {
                        spo$end[evi] = GuiderWake
                      }
                      if ((spo$start[evi] < GuiderOnset && spo$end[evi] > GuiderOnset) |
                          (spo$end[evi] > GuiderOnset && spo$end[evi] < spo$start[evi])) {
                        spo$start[evi] = GuiderOnset
                      }
                    }
                  }
                }
                spo$duration = spo$end - spo$start
                if (daysleeper[j] == TRUE) {
                  # for the labelling above it was needed to have times > 36, but for the plotting
                  # time in the second day needs to be returned to a normal 24 hour scale.
                  reversetime2 = which(spo$start >= 36)
                  reversetime3 = which(spo$end >= 36)
                  if (length(reversetime2) > 0) spo$start[reversetime2] = spo$start[reversetime2] - 24
                  if (length(reversetime3) > 0) spo$end[reversetime3] = spo$end[reversetime3] - 24
                }
              }
            }
            #------------------------------------------------------------------------
            # Object spo contains all the sib for one sib defintion
            # Object spocum contains all the sib for multiple sib definitions
            if (exists("spo")) {
              spo$def = k
              if (spocumi == 1) {
                spocum = spo
              } else {
                spocum = rbind(spocum, spo)
              }
              spocumi = spocumi + 1
            }
          }
          #------------------------------------------------------------------------
          # Take variables 'spocum', GuiderOnset and GuiderWake to derive nightsummary measures and to
          # create a plot for the current night in the current participant
          #------------------------------------------------------------------------
          # PLOTTING related
          if (params_output[["do.visual"]] == TRUE) {
            if (cnt == (nnpp + 1)) {
              if (verbose == TRUE) cat(" NEW ")
              pagei = pagei + 1
              # add y-axis before starting new page
              if (length(idlabels) < nnpp) {
                idlabels = c(idlabels, rep(" ", length(idlabels) - nnpp))
              } else if (length(idlabels) > nnpp) {
                idlabels = idlabels[1:nnpp]
              }
              axis(side = 2, at = 1:nnpp, labels = idlabels, las = 1, cex.axis = 0.6)
              idlabels = rep(0, nnpp)
              plot(c(0, 0), c(1, 1), xlim = c(12, 36), ylim = c(0, nnpp), col = "white", axes = FALSE,
                   xlab = "time", ylab = "", main = paste0("Page ", pagei))
              axis(side = 1, at = 12:36, labels = c(12:24, 1:12), cex.axis = 0.7)
              abline(v = c(18, 24, 30), lwd = 0.2, lty = 2)
              abline(v = c(15, 21, 27, 33), lwd = 0.2, lty = 3, col = "grey")
              cnt = 1
              addlegend = TRUE
            }
          }
          if (length(spocum) > 0) {
            NAvalues = which(is.na(spocum$def) == TRUE)
            if (length(NAvalues) > 0) {
              spocum = spocum[-NAvalues, ]
            }
          }
          # Fill matrix 'nightsummary' with key sleep parameters
          if (length(spocum) > 0 & class(spocum)[1] == "data.frame" & length(calendar_date) >= j) {
            if (nrow(spocum) > 0 & ncol(spocum) >= 5 & calendar_date[j] != "") {
              undef = unique(spocum$def)
              undef = undef[undef != ""]
              for (defi in undef) {
                #------------------------------------------------------------------------
                # nightsummary
                rowswithdefi = which(spocum$def == defi)
                if (length(rowswithdefi) > 0) {
                  # only process day if there are at least 2 sustained inactivity bouts
                  spocum.t = spocum[rowswithdefi, ]
                  evin = 2
                  if (guider == "NotWorn") {
                    while (evin <= nrow(spocum.t)) {
                      if (spocum.t$start[evin] - spocum.t$start[evin - 1] < -2) {
                        spocum.t$start[evin] = spocum.t$start[evin] + 24
                      }
                      if (spocum.t$end[evin] - spocum.t$end[evin - 1] < -2) {
                        spocum.t$end[evin] = spocum.t$end[evin] + 24
                      }
                      evin = evin + 1
                    }
                  }
                  # in DST it can be that a double hour is not recognized as part of the SPT
                  correct01010pattern = function(x) {
                    x = as.numeric(x)
                    if (length(which(diff(x) == 1)) > 1) {
                      minone = which(diff(x) == -1) + 1
                      plusone = which(diff(x) == 1)
                      matchingvalue = which(minone %in% plusone == TRUE)
                      if (length(matchingvalue) > 0) x[minone[matchingvalue]] = 1
                    }
                    return(x)
                  }
                  delta_t1 = diff(as.numeric(spocum.t$end))
                  spocum.t$overlapGuider = correct01010pattern(spocum.t$overlapGuider)
                  #----------------------------
                  nightsummary[sumi, 1] = accid
                  nightsummary[sumi, 2] = j  #night
                  # remove double rows
                  spocum.t = spocum.t[!duplicated(spocum.t), ]
                  
                  #------------------------------------
                  # ACCELEROMETER
                  if (length(which(as.numeric(spocum.t$overlapGuider) == 1)) > 0) {
                    rtl = which(spocum.t$overlapGuider == 1)
                    nightsummary[sumi, 3] = spocum.t$start[rtl[1]]
                    nightsummary[sumi, 4] = spocum.t$end[rtl[length(rtl)]]
                  } else {
                    cleaningcode = 5  # only for first day, other cleaningcode is assigned to wrong day
                    nightsummary[sumi, 3] = GuiderOnset  #use default assumption about onset
                    nightsummary[sumi, 4] = GuiderWake  #use default assumption about wake
                  }
                  nightsummary[, 3] = as.numeric(nightsummary[, 3])  # onset
                  nightsummary[, 4] = as.numeric(nightsummary[, 4])  # wake
                  # onset after wake is impossible and even more impossible if wake occurs
                  # before none, while we previously labelled it as daysleep
                  if (nightsummary[sumi, 3] > nightsummary[sumi, 4] &
                      nightsummary[sumi, 4] < 36 & daysleeper[j] == TRUE) {
                    nightsummary[sumi, 4] = nightsummary[sumi, 4] + 24  # correction for overcorrection in waking time
                  }
                  if (nightsummary[sumi, 3] == nightsummary[sumi, 4] & nightsummary[sumi, 4] == 18) {
                    # sleeping from 6pm to 6pm (probably non-wear)
                    nightsummary[sumi, 4] = nightsummary[sumi, 4] + 24
                  }
                  nightsummary[sumi, 5] = nightsummary[sumi, 4] - nightsummary[sumi, 3]  #duration Spt
                  nightsummary[, 5] = as.numeric(nightsummary[, 5])
                  nightsummary[sumi, 6] = defi  #sleep definition
                  #------------------------------------
                  # Correct GuiderOnset and GuiderWake to fall within [12-36] window and
                  # store as guider_onset and guider_wake, except when it is a daysleeper
                  # because for daysleeper we expect the window to run beyond time 36 (noon next day)
                  if (GuiderOnset > 36) {
                    nightsummary[sumi, 7] = GuiderOnset - 24
                  } else {
                    nightsummary[sumi, 7] = GuiderOnset
                  }
                  if (GuiderWake > 36 & daysleeper[j] == FALSE) {
                    nightsummary[sumi, 8] = GuiderWake - 24
                  } else {
                    nightsummary[sumi, 8] = GuiderWake
                  }
                  if (nightsummary[sumi, 7] > nightsummary[sumi, 8]) {
                    nightsummary[sumi, 9] = abs((36 - nightsummary[sumi, 7]) + (nightsummary[sumi,
                                                                                             8] - 12))
                  } else {
                    nightsummary[sumi, 9] = abs(nightsummary[sumi, 8] - nightsummary[sumi, 7])
                  }
                  #------------------------------------
                  # Calculate differences between final estimate and guider
                  # In the output we call this error
                  nightsummary[sumi, 10] = nightsummary[sumi, 3] - nightsummary[sumi, 7]  #error onset
                  nightsummary[sumi, 11] = nightsummary[sumi, 4] - nightsummary[sumi, 8]  #error wake
                  # sometimes difference calculations (error) can be in the wrong direction,
                  # e.g. log = 11, acc is 35
                  if (nightsummary[sumi, 10] > 12) nightsummary[sumi, 10] = -(24 - nightsummary[sumi, 10])
                  if (nightsummary[sumi, 10] < -12) nightsummary[sumi, 10] = -(nightsummary[sumi, 10] + 24)
                  if (nightsummary[sumi, 11] > 12) nightsummary[sumi, 11] = -(24 - nightsummary[sumi, 11])
                  if (nightsummary[sumi, 11] < -12) nightsummary[sumi, 11] = -(nightsummary[sumi, 11] + 24)
                  nightsummary[sumi, 12] = nightsummary[sumi, 5] - nightsummary[sumi, 9]  #error duration
                  #------------------------------------
                  # Other variables
                  if (acc_available == TRUE) {
                    nightsummary[sumi, 13] = remember_fraction_invalid_day1  #sleepdet.t$fraction.night.invalid[1]
                    if (remember_fraction_invalid_day1 > ((24 - params_cleaning[["includenightcrit"]])/24)) {
                      cleaningcode = 2  # only for first day, other cleaningcode is assigned to wrong day
                    }
                  } else {
                    nightsummary[sumi, 13] = 1
                  }
                  # Accumulated sustained inactivity bouts during SPT (nocturnal) and dyatime
                  overlap = which(spocum.t$overlapGuider == 1)
                  nocs = spocum.t$duration[overlap]
                  no_overlap = which(spocum.t$overlapGuider == 0)
                  sibds = spocum.t$duration[no_overlap]
                  # it is possible that nocs is negative if when sleep episode starts before dst
                  # in the autumn and ends inside the dst hour
                  negval = which(nocs < 0)
                  if (length(negval) > 0) {
                    kk0 = as.numeric(spocum.t$start[which(spocum.t$overlapGuider == 1)])  # episode onsets
                    kk1 = as.numeric(spocum.t$end[which(spocum.t$overlapGuider == 1)])  # episode endings
                    kk1[negval] = kk1[negval] + 1
                    nocs = kk1 - kk0
                  }
                  if (length(nocs) > 0) {
                    spocum.t.dur.noc = sum(nocs)
                  } else {
                    spocum.t.dur.noc = 0
                  }
                  # ======================================================================================================
                  # check whether it is day saving time (DST) the next day (= the night
                  # connected to the present day)
                  is_this_a_dst_night_output = is_this_a_dst_night(calendar_date = calendar_date[j],
                                                                   tz = params_general[["desiredtz"]])
                  dst_night_or_not = is_this_a_dst_night_output$dst_night_or_not
                  dsthour = is_this_a_dst_night_output$dsthour
                  # if yes, then check whether any of the sleep episodes overlaps dst in spring,
                  # one hour skipped
                  if (dst_night_or_not == 1) {
                    checkoverlap = spocum.t[which(spocum.t$overlapGuider == 1), c("start", "end")]
                    if (nrow(checkoverlap) > 0) {
                      overlaps = which(checkoverlap[, 1] <= (dsthour + 24) & checkoverlap[, 2] >=
                                         (dsthour + 25))
                    } else {
                      overlaps = c()
                    }
                    if (length(overlaps) > 0) {
                      # if yes, then reduce the length of those sleep episodes
                      spocum.t.dur.noc = spocum.t.dur.noc - 1
                      nightsummary[sumi, 5] = nightsummary[sumi, 5] - 1
                      nightsummary[sumi, 9] = nightsummary[sumi, 9] - 1
                    }
                  } else if (dst_night_or_not == -1) {
                    # dst in autumn, one double hour spocum.t.dur.noc has been calculated
                    # correctly including the double hour However, time elapse between onset and
                    # wake needs to be expanded by 1 if onset was before dst and waking up was
                    # after dst.
                    if (nightsummary[sumi, 3] <= (dsthour + 24) &
                        nightsummary[sumi, 4] >= (dsthour + 25)) {
                      nightsummary[sumi, 5] = nightsummary[sumi, 5] + 1  # accelerometer derived sleep duration
                    }
                    if (nightsummary[sumi, 7] <= (dsthour + 24) &
                        nightsummary[sumi, 8] >= (dsthour + 25)) {
                      nightsummary[sumi, 9] = nightsummary[sumi, 9] + 1  # sleep log sleepduration
                    }
                    # does SPT end within double hour?
                    correctSptEdgingInDoubleHour = function(nightsummary, onsetcol, wakecol, durcol,
                                                            dsthour, delta_t1) {
                      wakeInDoubleHour = nightsummary[, wakecol] >= (dsthour + 24) & nightsummary[,
                                                                                                  wakecol] <= (dsthour + 25)
                      onsetInDoubleHour = nightsummary[, onsetcol] >= (dsthour + 24) & nightsummary[,
                                                                                                    onsetcol] <= (dsthour + 25)
                      onsetBeforeDoubleHour = nightsummary[, onsetcol] <= (dsthour + 24)
                      wakeAfterDoubleHour = nightsummary[, wakecol] >= (dsthour + 25)
                      timeWentBackward = length(which(delta_t1 < 0)) > 0
                      if (onsetBeforeDoubleHour == TRUE & wakeInDoubleHour == TRUE) {
                        if (timeWentBackward == TRUE) {
                          # if time went back then it ended in the second hour of the double
                          # hour and the + 1 is definitely justified
                          nightsummary[, durcol] = nightsummary[, durcol] + 1
                        } else if (timeWentBackward == FALSE) {
                          # if time did not go back then SPT may have ended in either the first
                          # or second of the double hour TO DO: distinguish these rare cases,
                          # for now assume it ends in the second hour to avoid sleep efficiency
                          # above 100%.
                          nightsummary[, durcol] = nightsummary[, durcol] + 1
                        }
                      }
                      if (wakeAfterDoubleHour == TRUE & onsetInDoubleHour == TRUE) {
                        if (timeWentBackward == TRUE) {
                          # if time went back then it ended in the second hour of the double
                          # hour and the + 1 is definitely justified
                          nightsummary[, durcol] = nightsummary[, durcol] + 1  # accelerometer derived sleep duration
                        } else if (timeWentBackward == FALSE) {
                          # if time did not go back then SPT may have ended in either the first
                          # or second of the double hour TO DO: distinguish these rare cases,
                          # for now assume it ends in the second hour to avoid sleep efficiency
                          # above 100%.
                          nightsummary[, durcol] = nightsummary[, durcol] + 1  # accelerometer derived sleep duration
                        }
                      }
                      return(nightsummary)
                    }
                    nightsummary[sumi, ] = correctSptEdgingInDoubleHour(nightsummary[sumi, ], onsetcol = 3,
                                                                        wakecol = 4, durcol = 5, dsthour = dsthour, delta_t1 = delta_t1)
                    nightsummary[sumi, ] = correctSptEdgingInDoubleHour(nightsummary[sumi, ], onsetcol = 7,
                                                                        wakecol = 8, durcol = 9, dsthour = dsthour, delta_t1 = delta_t1)
                  }
                  # ======================================================================================================
                  sibds_atleast15min = 0
                  if (length(sibds) > 0) {
                    spocum.t.dur_sibd = sum(sibds)
                    atleast15min = which(sibds >= 1/4)
                    if (length(atleast15min) > 0) {
                      sibds_atleast15min = sibds[atleast15min]
                      spocum.t.dur_sibd_atleast15min = sum(sibds_atleast15min)
                    } else {
                      spocum.t.dur_sibd_atleast15min = 0
                    }
                  } else {
                    spocum.t.dur_sibd = 0
                    spocum.t.dur_sibd_atleast15min = 0
                  }
                  
                  nightsummary[sumi, 14] = spocum.t.dur.noc  #SleepDurationInSpt
                  nightsummary[sumi, 15] = nightsummary[sumi, 5] - spocum.t.dur.noc  #WASO
                  nightsummary[sumi, 16] = spocum.t.dur_sibd  #total sib (sustained inactivty bout) duration during wakinghours
                  nightsummary[sumi, 17] = length(which(spocum.t$overlapGuider == 1))  #number of nocturnalsleep periods
                  nightsummary[sumi, 18] = nightsummary[sumi, 17] - 1  #number of awakenings
                  nightsummary[sumi, 19] = length(which(spocum.t$overlapGuider == 0))  #number of sib (sustained inactivty bout) during wakinghours
                  nightsummary[sumi, 20] = as.numeric(spocum.t.dur_sibd_atleast15min)  #total sib (sustained inactivty bout) duration during wakinghours of at least 5 minutes
                  #-------------------------------------------------------
                  # Also report timestamps in non-numeric format:
                  acc_onset = nightsummary[sumi, 3]
                  acc_wake = nightsummary[sumi, 4]
                  if (acc_onset > 24) acc_onset = acc_onset - 24
                  if (acc_wake > 24) acc_wake = acc_wake - 24
                  #--------------------------------------------
                  # convert into clocktime
                  acc_onsetTS = convertHRsinceprevMN2Clocktime(acc_onset)
                  acc_wakeTS = convertHRsinceprevMN2Clocktime(acc_wake)
                  nightsummary[sumi, 21] = acc_onsetTS
                  nightsummary[sumi, 22] = acc_wakeTS
                  #----------------------------------------------
                  nightsummary[sumi, 23] = tmp1  #guider_onset_ts
                  nightsummary[sumi, 24] = tmp4  #guider_wake_ts
                  if (params_sleep[["sleepwindowType"]] == "TimeInBed" || guider == "markerbutton") {
                    # If guider is a sleeplog and if the sleeplog recorded time in bed or a marker button
                    # then calculate:
                    # sleep latency:
                    nightsummary[sumi, 25] = round(nightsummary[sumi, 3] - nightsummary[sumi, 7],
                                                   digits = 7)  #sleeponset - guider_onset
                    # sleep efficiency:
                    if (params_sleep[["sleepefficiency.metric"]] == 1) {
                      nightsummary[sumi, 26] = round(nightsummary[sumi, 14]/nightsummary[sumi, 9], digits = 5)  #accumulated nocturnal sleep / guider
                    } else if (params_sleep[["sleepefficiency.metric"]] == 2) {
                      nightsummary[sumi, 26] = round(nightsummary[sumi, 14]/(nightsummary[sumi, 5] + nightsummary[sumi, 25]), digits = 5)  #accumulated nocturnal sleep / detected spt + latency
                    }
                  }
                  nightsummary[sumi, 27] = pagei
                  nightsummary[sumi, 28] = daysleeper[j]
                  nightsummary[sumi, 29] = wdayname[j]
                  nightsummary[sumi, 30] = calendar_date[j]
                  nightsummary[sumi, 31] = fnames[i]
                  # nightsummary
                  #------------------------------------------------------------------------
                  # PLOT
                  if (params_output[["do.visual"]] == TRUE) {
                    if (defi == undef[1]) {
                      # only decide whether to plot the first time
                      if (params_output[["outliers.only"]] == TRUE) {
                        if (abs(nightsummary$error_onset[sumi]) > params_output[["criterror"]] | abs(nightsummary$error_wake[sumi]) >
                            params_output[["criterror"]] | abs(nightsummary$error_dur[sumi]) > (params_output[["criterror"]] * 2)) {
                          doplot = TRUE
                          if (verbose == TRUE) cat(" PLOT ")
                        } else {
                          doplot = FALSE
                        }
                      } else {
                        doplot = TRUE
                      }
                    }
                    # upcoming 5 lines added to avoid ending up with meaningless visualisations
                    # of nights for which no sleep log entry was available, and for which L5
                    # method provided estimates
                    if (length(params_sleep[["loglocation"]]) > 0) {
                      cleaningcriterion = 1
                    } else {
                      cleaningcriterion = 2
                    }
                    if (doplot == TRUE & cleaningcode < cleaningcriterion) {
                      idlabels[cnt] = paste0("ID", accid, " night", j)
                      den = 20
                      defii = which(undef == defi)
                      qtop = ((defii/length(undef)) * 0.6) - 0.3
                      qbot = (((defii - 1)/length(undef)) * 0.6) - 0.3
                      # add bar for each sleep defintion of accelerometer
                      for (pli in 1:nrow(spocum.t)) {
                        if (spocum.t$start[pli] > spocum.t$end[pli]) {
                          if (pli > 1 & pli < nrow(spocum.t) &
                              abs(as.numeric(spocum.t$start[pli]) - as.numeric(spocum.t$end[pli])) < 2) {
                            spocum.t[pli, c("start", "end")] = spocum.t[pli, c("end", "start")]
                          }
                        }
                        if (spocum.t$overlapGuider[pli] == 1) {
                          colb = rainbow(length(undef), start = 0.7, end = 1)
                        } else {
                          colb = rainbow(length(undef), start = 0.2, end = 0.4)
                        }
                        if (spocum.t$start[pli] > spocum.t$end[pli]) {
                          # plot sib that starts on the right (morning) and ends on the left (afternoon)
                          rect(xleft = spocum.t$start[pli], ybottom = (cnt + qbot), xright = 36,
                               ytop = (cnt + qtop), col = colb[defii], border = NA)
                          rect(xleft = 12, ybottom = (cnt + qbot), xright = spocum.t$end[pli], ytop = (cnt + qtop),
                               col = colb[defii], border = NA)
                        } else {
                          rect(xleft = spocum.t$start[pli], ybottom = (cnt + qbot), xright = spocum.t$end[pli],
                               ytop = (cnt + qtop), col = colb[defii], border = NA)
                        }
                      }
                      GuiderWaken = GuiderWake
                      GuiderOnsetn = GuiderOnset
                      
                      if (GuiderWake > 36) GuiderWaken = GuiderWake - 24
                      if (GuiderOnset > 36) GuiderOnsetn = GuiderOnset - 24
                      if (defi == undef[length(undef)]) {
                        # only plot log for last definition night sleeper
                        
                        if (GuiderOnsetn > GuiderWaken) {
                          # day sleeper
                          rect(xleft = GuiderOnsetn, ybottom = (cnt - 0.3), xright = 36, ytop = (cnt + 0.3),
                               col = "black", border = TRUE, density = den)
                          rect(xleft = 12, ybottom = (cnt - 0.3), xright = GuiderWaken, ytop = (cnt + 0.3),
                               col = "black", border = TRUE, density = den)
                        } else {
                          rect(xleft = GuiderOnsetn, ybottom = (cnt - 0.3), xright = GuiderWaken, ytop = (cnt + 0.3),
                               col = "black", border = TRUE, density = den)
                        }
                      }
                    }
                  }
                  # PLOT
                  #------------------------------------------------------------------------
                  nightsummary[sumi, 32] = cleaningcode
                  nightsummary[sumi, 33] = sleeplog_used
                  nightsummary[sumi, 34] = logid
                  nightsummary[sumi, 35] = acc_available
                  nightsummary[sumi, 36] = guider
                  # Extract SRI for this night
                  nightsummary[sumi, 37:38] = NA
                  if (!exists("SleepRegularityIndex")) {
                    SleepRegularityIndex = NA
                  }
                  SRI = SleepRegularityIndex
                  if (is.data.frame(SRI) == TRUE) {
                    calendar_date_asDate = format(as.Date(calendar_date[j], format = "%d/%m/%Y"), format = ("%d/%m/%Y"))
                    calendar_date_reformat = format(x = calendar_date_asDate, format = "%d/%m/%Y")
                    SRIindex = which(SRI$date == calendar_date_reformat & SRI$frac_valid > (params_cleaning[["includenightcrit"]]/24))
                    if (length(SRIindex) > 0) {
                      nightsummary[sumi, 37] = SRI$SleepRegularityIndex[SRIindex[1]]
                      nightsummary[sumi, 38] = SRI$frac_valid[SRIindex[1]]
                    }
                  }
                  if (length(longitudinal_axis) == 0) {
                    nightsummary[sumi, 39] = NA
                  } else {
                    nightsummary[sumi, 39] = longitudinal_axis
                  }
                  if (params_output[["storefolderstructure"]] == TRUE) {
                    nightsummary[sumi, 40] = ffd[i]  #full filename structure
                    nightsummary[sumi, 41] = ffp[i]  #use the lowest foldername as foldername name
                  }
                  sumi = sumi + 1
                }  #run through definitions
                if (params_output[["do.visual"]] == TRUE) {
                  if (cleaningcode < cleaningcriterion & doplot == TRUE) {
                    lines(x = c(12, 36), y = c(cnt, cnt), lwd = 0.2, lty = 2)  #abline(h=cnt,lwd=0.2,lty=2)
                    if (daysleeper[j] == TRUE) {
                      lines(x = c(18, 18), y = c((cnt - 0.3), (cnt + 0.3)), lwd = 2, lty = 2, col = "black")
                    }
                    # only increase count if there was bar plotted and it is the last definition
                    if (defi == undef[length(undef)]) {
                      cnt = cnt + 1
                    }
                  }
                }
              }
              if (addlegend == TRUE) {
                colb_spt = rainbow(length(undef), start = 0.7, end = 1)
                colb_day = rainbow(length(undef), start = 0.2, end = 0.4)
                colb = c(colb_spt, colb_day)
                legnames = c(paste0("sib", undef, "_spt"), paste0("sib", undef, "_day"), "guider, e.g. diary")
                legend("top", legend = legnames, density = c(rep(NA, 2 * length(undef)), 40), fill = c(colb, "black"),
                       border = c(colb, "black"), ncol = min(c(3, length(legnames))), cex = 0.7)
                addlegend = FALSE
              }
            }
          }
          # END OF PLOT AND nightsummary MEASURES
        }  #nights
        if (length(nnights.list) == 0) {
          # if there were no nights to analyse
          nightsummary[sumi, 1:2] = c(accid, 0)
          nightsummary[sumi, c(3:30, 34, 36:39)] = NA
          nightsummary[sumi, 31] = fnames[i]
          nightsummary[sumi, 32] = 4  #cleaningcode = 4 (no nights of accelerometer available)
          nightsummary[sumi, c(33, 35)] = c(FALSE, TRUE)  #sleeplog_used acc_available
          if (params_output[["storefolderstructure"]] == TRUE) {
            nightsummary[sumi, 40:41] = c(ffd[i], ffp[i])  #full filename structure and use the lowest foldername as foldername name
          }
          sumi = sumi + 1
        }
        if (params_sleep[["sleepwindowType"]] != "TimeInBed" &&
            params_sleep[["consider_marker_button"]] == FALSE) {
          nightsummary = nightsummary[, which(colnames(nightsummary) %in% c("sleeplatency", "sleepefficiency") ==
                                                FALSE)]
        }
        GGIRversion = "GGIR not used"
        if (is.element('GGIR', installed.packages()[,1])) {
          GGIRversion = as.character(utils::packageVersion("GGIR"))
          if (length(GGIRversion) != 1) GGIRversion = sessionInfo()$otherPkgs$GGIR$Version
        }
        if (nrow(nightsummary) > 0) {
          nightsummary$GGIRversion = GGIRversion
        }
        save(nightsummary, tail_expansion_log, GGIRversion, file = paste0(metadatadir, ms4.out, "/", fnames[i]))
      }
    }
  }  #end of loop through acc files
  if (cnt67 == 2 & params_output[["do.visual"]] == TRUE) {
    if (cnt - 1 != (nnpp + 1)) {
      zerolabel = which(idlabels == 0)
      if (length(zerolabel) > 0) idlabels[zerolabel] = " "
      axis(side = 2, at = 1:nnpp, labels = idlabels, las = 1, cex.axis = 0.5)
    }
    dev.off()
    cnt67 = 1
  }
}