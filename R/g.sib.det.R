g.sib.det = function(M, IMP, I, twd = c(-12, 12),
                     acc.metric = "ENMO", desiredtz = "",
                     myfun=c(), sensor.location = "wrist",
                     params_sleep = c(), zc.scale = 1, ...) {
  #get input variables
  input = list(...)
  if (length(input) > 0 || length(params_sleep) == 0) {
    # Extract and check parameters if user provides more arguments than just the parameter arguments,
    # or if params_[...] aren't specified (so need to be filled with defaults).
    # So, inside GGIR this will not be used, but it is used when g.sleep is used on its own
    # as if it was still the old g.sleep function
    params = extract_params(params_sleep = params_sleep,
                            input = input,
                            params2check = "sleep") # load default parameters
    params_sleep = params$params_sleep
  }
  #==============================================================
  perc = 10; spt_threshold = 15; sptblocksize = 30; spt_max_gap = 60 # default configurations (keep hardcoded for now
  # Abbreviaton SPTE = Sleep Period Time Estimate, although in case of HorAngle it is the Time in Bed estimate
  # but then we would have to come up with yet another term to represent the main sleep and/or time in bed window of the day
  # So, out of convenience I keep the object name SPTE. 
  dstime_handling_check = function(tmpTIME = tmpTIME, spt_estimate = spt_estimate, tz = c(),
                                   calc_SPTE_end = c(), calc_SPTE_start = c()) {
    # Function to readjust estimated SPTE_start and SPTE_end in DST days
    t = tmpTIME[c(1, spt_estimate$SPTE_start, spt_estimate$SPTE_end)]
    timezone = format(GGIR::iso8601chartime2POSIX(t, tz = tz), "%z")
    if (length(unique(timezone)) == 1) {
      return(c(calc_SPTE_start, calc_SPTE_end))
    } else {
      sign = ifelse(substr(timezone, 1, 1) == "-", -1, 1)
      hours = as.numeric(substr(timezone, 2, 3))
      minutes = as.numeric(substr(timezone, 4, 5))
      offset = sign * (hours + minutes / 60)
      offset = offset[2:3] - offset[1]
      return(c(calc_SPTE_start + offset[1], calc_SPTE_end + offset[2]))
    }
  }
  decide_guider = function(HASPT.algo, nonwear_percentage) {
    # Function to decide the guider to use in the case that HASPT.algo is of length 2
    if (HASPT.algo[1] == "NotWorn" && nonwear_percentage < 25 && length(HASPT.algo) == 2) {
      return(2)
    } else {
      return(1)
    }
  }
  #==================================================
  # get variables
  nD = nrow(IMP$metashort)
  mon = I$monn
  ws3 = M$windowsizes[1] #short epoch size
  ws2 = M$windowsizes[2] # medium length epoch size used for non-wear detection
  n_ws2_perday = (1440*60) / ws2
  n_ws3_perday = (1440*60) / ws3
  #--------------------
  # get indicator of non-wear periods
  rout = IMP$rout
  r5 = as.numeric(as.matrix(rout[,5]))
  r5long = matrix(0, length(r5), (ws2/ws3))
  r5long = replace(r5long, 1:length(r5long), r5)
  r5long = t(r5long)
  dim(r5long) = c((length(r5) * (ws2/ws3)),1)
  if (nD < length(r5long)) {
    invalid = r5long[1:nD]
  } else {
    invalid = c(r5long, rep(0, (nD - length(r5long))))
  }
  rm(r5,rout)
  # Deriving file characteristics from 15 min summary files
  ND = nD/n_ws3_perday #number of days
  if (ND > 0.2) {
    #========================================================================
    # timestamps
    time = format(IMP$metashort[,1])
    fix_NA_invector = function(x){
      if (length(which(is.na(x) == TRUE)) > 0) {
        x[which(is.na(x) == T)] = 0
      }
      return(x)
    }
    anglez = as.numeric(as.matrix(IMP$metashort[,which(colnames(IMP$metashort) == "anglez")]))
    anglez = fix_NA_invector(anglez)
    anglex = angley = c()
    do.HASPT.hip = FALSE
    if (sensor.location == "hip" &
        "anglex" %in% colnames(IMP$metashort) &
        "angley" %in% colnames(IMP$metashort) &
        "anglez" %in% colnames(IMP$metashort)) {
      do.HASPT.hip = TRUE
      if (length(colnames(IMP$metashort) == "anglex") > 0) {
        anglex =  IMP$metashort[, which(colnames(IMP$metashort) == "anglex")]
        anglex = fix_NA_invector(anglex)
      }
      if (length(colnames(IMP$metashort) == "angley") > 0) {
        angley = IMP$metashort[,which(colnames(IMP$metashort) == "angley")]
        angley = fix_NA_invector(angley)
      }
    }
    if (acc.metric %in% colnames(IMP$metashort) == FALSE) {
      if ("ExtAct" %in% colnames(IMP$metashort) == TRUE) {
        acc.metric = "ExtAct"
      } else {
        warning("Argument acc.metric is set to ",acc.metric," but not found in GGIR part 1 output data")
      }
    }
    ACC = as.numeric(as.matrix(IMP$metashort[,which(colnames(IMP$metashort) == acc.metric)]))
    night = rep(0, length(ACC))
    
    if ("marker" %in% colnames(IMP$metashort)) {
      MARKER = as.numeric(as.matrix(IMP$metashort[,which(colnames(IMP$metashort) == "marker")]))
    } else {
      MARKER = NULL
    }
    if (params_sleep[["HASIB.algo"]] == "Sadeh1994" | 
        params_sleep[["HASIB.algo"]] == "Galland2012" |
        params_sleep[["HASIB.algo"]] == "ColeKripke1992") { # extract zeroCrossingCount
      zeroCrossingCount =  IMP$metashort[,which(colnames(IMP$metashort) == paste0("ZC", params_sleep[["Sadeh_axis"]]))]
      zeroCrossingCount = fix_NA_invector(zeroCrossingCount)
      zeroCrossingCount = zeroCrossingCount * zc.scale
      # always do zeroCrossingCount but optionally also add BrondCounts to output for comparison
      BrondCount_colname = paste0("BrondCount_", tolower(params_sleep[["Sadeh_axis"]]))
      if (BrondCount_colname %in% colnames(IMP$metashort)) {
        BrondCount =  IMP$metashort[, BrondCount_colname]
        BrondCount = fix_NA_invector(BrondCount)
      } else {
        BrondCount = c()
      }
      # optionally add NeishabouriCounts for comparison
      NeishabouriCount_colname = paste0("NeishabouriCount_", tolower(params_sleep[["Sadeh_axis"]]))
      if (NeishabouriCount_colname %in% colnames(IMP$metashort)) {
        NeishabouriCount =  IMP$metashort[, NeishabouriCount_colname]
        NeishabouriCount = fix_NA_invector(NeishabouriCount)
      } else {
        NeishabouriCount = c()
      }
    } else {
      zeroCrossingCount = c()
      BrondCount = c()
      NeishabouriCount = c()
    }
    #==================================================================
    # 'sleep' detection if sleep is not provided by external function.
    # Note that inside the code we call it sustained inactivity bouts
    # to emphasize that we know that this is not actually neurological sleep
    getSleepFromExternalFunction = FALSE
    if (length(myfun) != 0) {
      if ("wake_sleep" %in% myfun$colnames) {
        if (myfun$outputtype[which(myfun$colnames == "wake_sleep")] == "character") {
          getSleepFromExternalFunction = TRUE
          sleepColName = "wake_sleep"
          sleepColType = "character"
        }
      }
      if ("ExtSleep" %in% myfun$colnames) {
        getSleepFromExternalFunction = TRUE
        sleepColName = "ExtSleep"
        sleepColType = "numeric"
      }
    }
    if (getSleepFromExternalFunction == FALSE) {
      sleep = HASIB(HASIB.algo = params_sleep[["HASIB.algo"]], 
                    timethreshold = params_sleep[["timethreshold"]],
                    anglethreshold = params_sleep[["anglethreshold"]], 
                    time = time, anglez = anglez, ws3 = ws3,
                    zeroCrossingCount = zeroCrossingCount,
                    BrondCount = BrondCount,
                    NeishabouriCount = NeishabouriCount, activity = ACC,
                    marker = MARKER)
    } else { # getSleepFromExternalFunction == TRUE
      # Code now uses the sleep estimates from the external function
      # So, the assumption is that the external function provides a 
      # character "Sleep" when it detects sleep
      sleep = matrix(0, nD, 1)
      if (sleepColType == "character") {
        sleep[which(M$metashort[, sleepColName] == "Sleep")] = 1 
      } else {
        sleep[which(M$metashort[, sleepColName] == 1)] = 1 
      }
    }
    #-------------------------------------------------------------------
    # detect midnights
    detemout = g.detecmidnight(time, desiredtz, dayborder = 0) # ND, # for sleep dayborder is always 0, it is the summary of sleep that will be dayborder specific
    midnights = detemout$midnights
    midnightsi = detemout$midnightsi
    countmidn = length(midnightsi)
    tib.threshold = SPTE_end = SPTE_start = L5list = part3_guider = rep(NA,countmidn)
    if (countmidn != 0) {
      if (countmidn == 1) {
        tooshort = 1
        lastmidnight = midnights[length(midnights)]
        lastmidnighti = midnightsi[length(midnights)]
        firstmidnight = time[1]
        firstmidnighti = midnightsi[1]
      } else {
        cut = which(as.numeric(midnightsi) == 0)
        if (length(cut) > 0) {
          midnights = midnights[-cut]
          midnightsi = midnightsi[-cut]
        }
        lastmidnight = midnights[length(midnights)]
        lastmidnighti = midnightsi[length(midnights)]
        firstmidnight = midnights[1]
        firstmidnighti = midnightsi[1]
      }
      # if recording started before 4am, then also derive first awakening
      first4am = grep("04:00:00", time[1:pmin(nD, (n_ws3_perday + 1))])[1]
      # only do this if there is a 4am in the recording
      if (!is.na(first4am)) {
        if (first4am < firstmidnighti) { # this means recording started after midnight and before 4am
          midn_start = 0
        } else {
          midn_start = 1
        }
      } else {
        # since there's no 4am in the recording, then even if there is a midnight, skip this midnight. 
        # (basically treat this midnight like any other midnight without a preceding 4am)
        midn_start = countmidn
      }
      sptei = 0
      for (j in midn_start:(countmidn)) { #Looping over the midnight
        if (j == 0) {
          qqq1 = 1 # preceding noon (not available in recording)
          qqq2 = midnightsi[1] + (twd[1] * (3600 / ws3))# first noon in recording
        } else {
          qqq1 = midnightsi[j] + (twd[1] * (3600 / ws3)) + 1 #preceding noon
          qqq2 = midnightsi[j] + (twd[2] * (3600 / ws3)) #next noon
        }
        # twd assumed 24 hour window, which is not the case for DST
        if (qqq2 < length(time)) {
          qqq2_hour = as.numeric(format(iso8601chartime2POSIX(time[qqq2], tz = desiredtz), "%H"))
          if (qqq2_hour == 11) {
            qqq2 = qqq2 + (3600 / ws3)
          } else if (qqq2_hour == 13) {
            qqq2 = qqq2 - (3600 / ws3)
          }
        }
        sptei = sptei + 1
        if (qqq2 - qqq1 < 60) next # skip night if it has less than 60 epochs
        if (qqq2 > length(time))  qqq2 = length(time)
        if (qqq1 < 1)             qqq1 = 1
        if (qqq1 == 1 && qqq2 != 24 * 3600 / ws3) {
          partialFirstDay = TRUE
        } else {
          partialFirstDay = FALSE
        }
        night[qqq1:qqq2] = sptei
        detection.failed = FALSE
        # Calculate nonwear percentage for this window
        nonwear_percentage = (length(which(invalid[qqq1:qqq2] == 1)) /  (qqq2 - qqq1 + 1)) * 100
        guider_to_use = decide_guider(params_sleep[["HASPT.algo"]], nonwear_percentage)
        #------------------------------------------------------------------
        # calculate L5 because this is used as back-up approach
        tmpACC = ACC[qqq1:qqq2]
        windowRL = round((3600/ws3)*5)
        if ((windowRL / 2) == round(windowRL / 2)) windowRL = windowRL + 1
        if (length(tmpACC) < windowRL) {  0 # added 4/4/2-17
          L5 = 0
        } else {
          ZRM = zoo::rollmean(x = c(tmpACC), k = windowRL, fill = "extend", align = "center") #
          L5 = which(ZRM == min(ZRM))[1]
          if (sd(ZRM) == 0) {
            L5 = 0
          } else {
            L5 = (L5  / (3600 / ws3)) + 12
          }
          if (length(L5) == 0) L5 = 0 #if there is no L5, because full day is zero
        }
        L5list[sptei] = L5
        # Estimate Sleep Period Time window, because this will be used by g.part4 if sleeplog is not available
        tmpANGLE = anglez[qqq1:qqq2]
        tmpTIME = time[qqq1:qqq2]
        daysleep_offset = 0
        if (do.HASPT.hip == TRUE & params_sleep[["HASPT.algo"]][guider_to_use] != "NotWorn") {
          if (params_sleep[["longitudinal_axis"]] == 1) {
            tmpANGLE = anglex[qqq1:qqq2]
          } else if (params_sleep[["longitudinal_axis"]] == 2) {
            tmpANGLE = angley[qqq1:qqq2]
          }
        }
        if (length(params_sleep[["def.noc.sleep"]]) == 1) {
          spt_estimate = HASPT(angle = tmpANGLE, ws3 = ws3,
                               sptblocksize = sptblocksize, spt_max_gap = spt_max_gap,
                               HASPT.algo = params_sleep[["HASPT.algo"]][guider_to_use],
                               invalid = invalid[qqq1:qqq2], # load only invalid time in the night of interest (i.e., qqq1:qqq2)
                               HDCZA_threshold = params_sleep[["HDCZA_threshold"]],
                               HASPT.ignore.invalid = params_sleep[["HASPT.ignore.invalid"]],
                               activity = tmpACC)
        } else {
          spt_estimate = list(SPTE_end = NULL, SPTE_start = NULL, tib.threshold = NULL, part3_guider = NULL)
        }
        if (length(spt_estimate$SPTE_end) != 0 & length(spt_estimate$SPTE_start) != 0) {
          if (spt_estimate$SPTE_end + qqq1 >= qqq2 - (1 * (3600 / ws3))) {
            # if estimated SPT ends within one hour of noon, re-run with larger window
            # to be able to detect sleep in daysleepers
            daysleep_offset = 6 # hours in which the window of data sent to SPTE is moved fwd from noon
            newqqq1 = qqq1 + (daysleep_offset * (3600 / ws3))
            newqqq2 = qqq2 + (daysleep_offset * (3600 / ws3))
            if (qqq1 == 1 && newqqq2 - newqqq1 < (24*3600) / ws3 && newqqq2 > (24*3600) / ws3) {
              newqqq1 = newqqq2 - (24 * 3600) / ws3
              partialFirstDay = FALSE
            }
            if (newqqq2 > length(anglez)) newqqq2 = length(anglez)
            # only try to extract SPT again if it is possible to extract a window of more than 23 hour
            if (newqqq2 < length(anglez) & (newqqq2 - newqqq1) > (23*(3600/ws3)) ) {
              # Recalculate nonwear percentage for new window (6pm to 6pm)
              nonwear_percentage = (length(which(invalid[newqqq1:newqqq2] == 1)) /  (newqqq2 - newqqq1 + 1)) * 100
              guider_to_use = decide_guider(params_sleep[["HASPT.algo"]], nonwear_percentage)
              # new TIME (6pm tp 6pm)
              tmpTIME = time[newqqq1:newqqq2]
              if (params_sleep[["HASPT.algo"]][guider_to_use] != "NotWorn") {
                tmpANGLE = anglez[newqqq1:newqqq2]
                if (do.HASPT.hip == TRUE) {
                  if (params_sleep[["longitudinal_axis"]] == 1) {
                    tmpANGLE = anglex[newqqq1:newqqq2]
                  } else if (params_sleep[["longitudinal_axis"]] == 2) {
                    tmpANGLE = angley[newqqq1:newqqq2]
                  }
                }
              }
              spt_estimate_tmp = HASPT(angle = tmpANGLE, ws3 = ws3,
                                       sptblocksize = sptblocksize, spt_max_gap = spt_max_gap,
                                       HASPT.algo = params_sleep[["HASPT.algo"]][guider_to_use],
                                       invalid = invalid[newqqq1:newqqq2],
                                       HDCZA_threshold = params_sleep[["HDCZA_threshold"]],
                                       HASPT.ignore.invalid = params_sleep[["HASPT.ignore.invalid"]],
                                       activity = ACC[newqqq1:newqqq2])
              if (length(spt_estimate_tmp$SPTE_start) > 0) {
                # If new SPTE_end is beyond noon (qqq2) then use the new SPTE_end
                if (spt_estimate_tmp$SPTE_end + newqqq1 >= qqq2) {
                  spt_estimate = spt_estimate_tmp
                } else {
                  daysleep_offset  = 0
                }
              } else {
                daysleep_offset  = 0
              }
            } else {
              daysleep_offset  = 0
            }
          }
          if (qqq1 == 1 && partialFirstDay == TRUE) {  
            # only use startTimeRecord if the start of the block send into SPTE was after noon
            startTimeRecord = unlist(iso8601chartime2POSIX(IMP$metashort$timestamp[1], tz = desiredtz))
            startTimeRecord = sum(as.numeric(startTimeRecord[c("hour", "min", "sec")]) / c(1, 60, 3600))
            daysleep_offset = daysleep_offset + startTimeRecord
          } else {
            daysleep_offset = daysleep_offset + 12
          }
          SPTE_end[sptei] = (spt_estimate$SPTE_end / (3600 / ws3)) + daysleep_offset
          SPTE_start[sptei] = (spt_estimate$SPTE_start / (3600 / ws3)) + daysleep_offset
          SPTE_dst = dstime_handling_check(tmpTIME = tmpTIME, spt_estimate = spt_estimate,
                                           tz = desiredtz, 
                                           calc_SPTE_end = SPTE_end[sptei],
                                           calc_SPTE_start = SPTE_start[sptei])
          SPTE_start[sptei] = SPTE_dst[1]
          SPTE_end[sptei] = SPTE_dst[2]
          tib.threshold[sptei] = spt_estimate$tib.threshold
          part3_guider[sptei] = spt_estimate$part3_guider
        }
      }
      detection.failed = FALSE
    } else {
      message("No midnights found in file")
      detection.failed = TRUE
    }
    metatmp = data.frame(time, invalid, night = night, sleep = sleep, stringsAsFactors = T)
  } else {
    metatmp = L5list = SPTE_end = SPTE_start = tib.threshold = part3_guider = c()
    detection.failed = TRUE
  }
  invisible(list(output = metatmp, detection.failed = detection.failed, L5list = L5list,
                 SPTE_end = SPTE_end, SPTE_start = SPTE_start,
                 tib.threshold = tib.threshold, longitudinal_axis = params_sleep[["longitudinal_axis"]],
                 part3_guider = part3_guider))
}
