g.analyse.perday = function(ndays, firstmidnighti, time, nfeatures,
                            midnightsi, metashort, averageday,
                            doiglevels, nfulldays,lastmidnight, ws3, ws2, qcheck,
                            fname, idloc, sensor.location, wdayname, tooshort, includedaycrit,
                            doquan, quantiletype, doilevels, domvpa,
                            mvpanames, wdaycode, ID,
                            deviceSerialNumber, ExtFunColsi, myfun, desiredtz = "",
                            params_247 = c(), params_phyact = c(),
                            ...) {
  #get input variables
  input = list(...)
  
  if (length(input) > 0 ||
      length(params_247) == 0 || length(params_phyact) == 0) {
    # Extract and check parameters if user provides more arguments than just the parameter arguments,
    # or if params_[...] aren't specified (so need to be filled with defaults).

    # So, inside GGIR this will not be used, but it is used when g.analyse is used on its own
    # as if it was still the old g.analyse function
    params = extract_params(params_247 = params_247,
                            params_phyact = params_phyact,
                            input = input,
                            params2check = c("247", "phyact")) # load default parameters
    params_247 = params$params_247
    params_phyact = params$params_phyact
    rm(params)
  }
  
  startatmidnight = endatmidnight = 0
  if (nfulldays >= 1) {
    if (firstmidnighti == 1) {  #if measurement starts at midnight
      ndays = ndays - 1
      startatmidnight =  1
    }
    if (lastmidnight == time[length(time)] & nrow(metashort) < ((60/ws3) * 1440)) {	#if measurement ends at midnight
      ndays = ndays - 1
      endatmidnight = 1
    }
  }
  
  daysummary = matrix("",ceiling(ndays),nfeatures)
  ds_names = rep("",nfeatures)
  windowsummary = ws_names = c()
  #===============================================
  # Features per day (based on on single variables)
  qwindow_names = qwindowbackup = params_247[["qwindow"]]
  qwindow_actlog = FALSE
  if (is.data.frame(params_247[["qwindow"]]) == TRUE) {
    qwindow_actlog = TRUE
  }
  unique_dates_recording = unique(as.Date(iso8601chartime2POSIX(time[c(seq(1, length(time),
                                                                           by = (3600/ws2) * 12),
                                                                       length(time))], tz = desiredtz)))
  ExtFunColsi = ExtFunColsi - 1 # subtract 1 because code ignores timestamp
  
  for (di in 1:ndays) { #run through days
    params_247[["qwindow"]] = qwindowbackup
    if (is.data.frame(params_247[["qwindow"]]) == TRUE) {
      currentdate = which(params_247[["qwindow"]]$date == unique_dates_recording[di])
      if (length(currentdate) > 0) {
        qwindow_times = as.character(unlist(params_247[["qwindow"]]$qwindow_times[currentdate]))
        qwindow_names = as.character(unlist(params_247[["qwindow"]]$qwindow_names[currentdate]))
        params_247[["qwindow"]] = qwindow_values_backup = unlist(params_247[["qwindow"]]$qwindow_values[currentdate])
        qwindow_names = qwindow_names[!is.na(params_247[["qwindow"]])]
        qwindow_times = qwindow_times[!is.na(params_247[["qwindow"]])]
      }
      params_247[["qwindow"]] = params_247[["qwindow"]][!is.na(params_247[["qwindow"]])]
      if (length(params_247[["qwindow"]]) == 1) params_247[["qwindow"]] = c()
      if (length(params_247[["qwindow"]]) == 0 | length(currentdate) == 0)  {
        qwindow_times = c("00:00","24:00")
        qwindow_names = c("daystart","dayend")
        params_247[["qwindow"]] = qwindow_values_backup = c(0, 24)
      }
    } else {
      if (length(params_247[["qwindow"]]) == 0) {
        params_247[["qwindow"]] = c(0, 24)
      }
      qwindow_values_backup = params_247[["qwindow"]]
      qwindow_times = as.character(params_247[["qwindow"]])
      qwindow_names = as.character(params_247[["qwindow"]])
    }
    # extract day from matrix D and qcheck
    if (startatmidnight == 1 & endatmidnight == 1) {
      qqq1 = midnightsi[di] * (ws2/ws3)	#a day starts at 00:00
      qqq2 = (midnightsi[(di + 1)]*(ws2/ws3)) - 1
    } else if (startatmidnight == 1 & endatmidnight == 0) {
      if (di < floor(ndays)) { #applying floor because when day there is day saving time then ndays is not an integer
        qqq1 = (midnightsi[di] - 1) * (ws2 / ws3) + 1
        qqq2 = ((midnightsi[(di + 1)] - 1) * (ws2 / ws3))  #remove +1 because I got some extra seconds per day
      } else if (di == floor(ndays)) {
        qqq1 = (midnightsi[di] - 1) * (ws2 / ws3) + 1
        qqq2 = nrow(metashort)
      }
    } else if (startatmidnight == 0 & endatmidnight == 0) {
      if (di == 1) {
        qqq1 = 1
        qqq2 = ((midnightsi[di] - 1) * (ws2 / ws3))
      } else if (di > 1 & di < floor(ndays)) {
        qqq1 = (midnightsi[(di - 1)] - 1) * (ws2 / ws3) + 1 # a day starts at 00:00
        qqq2 = ((midnightsi[di] - 1) * (ws2 / ws3))
      } else if (di == floor(ndays)) {
        qqq1 = (midnightsi[(di - 1)] - 1)*(ws2 / ws3) + 1 # a day starts at 00:00
        qqq2 = nrow(metashort)
      }
    } else if (startatmidnight == 0 & endatmidnight == 1) {
      if (di == 1) {
        qqq1 = 1
        qqq2 = (midnightsi[di] * (ws2 / ws3)) - 1
      } else if (di > 1 & di <= floor(ndays)) {
        qqq1 = midnightsi[(di - 1)] * (ws2 / ws3) # a day starts at 00:00
        qqq2 = (midnightsi[di] * (ws2 / ws3)) - 1
      }
    }
    if (qqq2 > nrow(metashort)) qqq2 = nrow(metashort)
    vari = metashort[qqq1:qqq2,]
    val = qcheck[qqq1:qqq2]
    nvalidhours_qwindow = rep(0, length(params_247[["qwindow"]]) - 1)
    nhours_qwindow = rep(0, length(params_247[["qwindow"]]) - 1)
    # Ignore qwindow values that are not possible for this day
    LENVAL_hours = length(val) / (60 * (60 / ws3)) #11.2
    if (length(which(round(LENVAL_hours) %in% 23:25 == TRUE)) == 0) {
      if (di == 1) {
        # Following 8 lines were turned off on June 5 2018 because first and last day are imputed,
        # but turned on again on 14 March 2019 because when the first day is half the relative start
        # of the windows must be different. Yes, the half days are imputed, but we are still only
        # interested in the non-imputed part. This is probably were the confusion came from.
        hours2delta = 24 - LENVAL_hours
        qw_select = which(params_247[["qwindow"]] > hours2delta)
        # if (qw_select[1] > 1) qw_select = c(qw_select[1] - 1, qw_select)
        # params_247[["qwindow"]] = params_247[["qwindow"]][qw_select]
        # # qwindow_names = qwindow_names[qw_select]
        qwindowindices = params_247[["qwindow"]] - hours2delta # - LENVAL_hours # because 1 is now different
        if (length(which(qwindowindices < 0)) > 0) qwindowindices[which(qwindowindices < 0)] = 0
      } else if (di == ndays) {
        qwindowindices = params_247[["qwindow"]]
      }
    } else {
      hours2delta = 0
      qwindowindices = params_247[["qwindow"]]
    }
    deltaLengthQwindow = 0
    if (length(params_247[["qwindow"]]) < 2) params_247[["qwindow"]] = c()
    if (length(params_247[["qwindow"]]) > 0) {
      if (length(qwindow_names) == 1) {
        warning("Argument to qwindow is invalid, requires a vector of at least length 2")
      }
      if (length(qwindow_names) == 2) {
        if (params_247[["qwindow"]][1] != 0 | params_247[["qwindow"]][2] != 24) {
          if((qwindowindices[2] * 60 * (60 / ws3)) <= length(val)) {
            valq = val[((qwindowindices[1] * 60 * (60 / ws3)) + 1):(qwindowindices[2] * 60 * (60 / ws3))]
          } else {
            valq = val[((qwindowindices[1] * 60 * (60 / ws3)) + 1):length(val)]
          }
          nvalidhours_qwindow = length(which(valq == 0)) / (3600 / ws3)
          nhours_qwindow = length(valq) / (3600 / ws3) #valid hours per day (or half a day)
        }
      } else if (length(qwindow_names) > 2) {
        deltaLengthQwindow = length(qwindow_names) - length(qwindowindices)
        
        for (qwi in 1:(length(qwindowindices) - 1)) {
          startindex = qwindowindices[qwi] * 60 * (60/ws3)
          endindex = qwindowindices[qwi + 1] * 60 * (60/ws3)
          if (startindex == endindex) {
            # inexistent qwindow (may occure in the first day if recording started later than first qwindow)
            valq = c()
          } else if (startindex <= length(val) & endindex <= length(val)) {
            valq = val[(startindex + 1):endindex]
          } else if (startindex <= length(val) & endindex >= length(val)) {
            valq = val[(startindex + 1):length(val)]
          } else if (startindex >= length(val) & endindex >= length(val)) {
            valq = c()
          }
          if (length(valq) > 0) {
            nvalidhours_qwindow[qwi + deltaLengthQwindow] = length(which(valq == 0)) / (3600 / ws3)
            nhours_qwindow[qwi + deltaLengthQwindow] = length(valq) / (3600 / ws3) #valid hours per day (or half a day)
          } else {
            nvalidhours_qwindow[qwi + deltaLengthQwindow] = ""
            nhours_qwindow[qwi + deltaLengthQwindow] = "" #valid hours per day (or half a day)
          }
        }
      }
    }
    val = as.numeric(val)
    nvalidhours = length(which(val == 0)) / (3600 / ws3) #valid hours per day (or half a day)
    nhours = length(val) / (3600 / ws3) #valid hours per day (or half a day)
    #start collecting information about the day
    fi = 1
    
    
    check_daysummary_size = function(daysummary, ds_names, fi) {
      if (fi > ncol(daysummary) - 1000) {
        expand = fi - (ncol(daysummary) - 1000)
        daysummary = cbind(daysummary, matrix("", nrow(daysummary), expand +  1000))
        ds_names = c(ds_names, rep("", expand + 1000))
      }
      invisible(list(daysummary = daysummary, ds_names = ds_names))
    }
    daysummary[di,fi] = ID
    idremember = daysummary[di,fi]
    ds_names[fi] = "ID";      fi = fi + 1
    daysummary[di,fi] = fname
    ds_names[fi] = "filename";  fi = fi + 1
    calendardate = unlist(strsplit(as.character(vari[1,1])," "))[1]
    daysummary[di,fi] = calendardate
    daysummary[di,(fi + 1)] = sensor.location
    daysummary[di,(fi + 2)] = nvalidhours
    daysummary[di,(fi + 3)] = nhours
    ds_names[fi:(fi + 3)] = c("calendar_date","bodylocation","N valid hours","N hours")
    fi = fi + 4
    vari = vari[,2:ncol(vari)] # remove timestamp now it is no longer needed
    colnames(averageday) = colnames(vari)
    
    if (length(params_247[["qwindow"]]) > 0) {
      if (length(params_247[["qwindow"]]) > 2 | params_247[["qwindow"]][1] != 0 | params_247[["qwindow"]][2] != 24) {
        for (qwi in 1:(length(qwindow_names) - 1)) {
          tmp_name = c(paste0("N_valid_hours_", qwindow_names[qwi], "-", qwindow_names[qwi + 1], "hr"),
                       paste0("N_hours_", qwindow_names[qwi], "-", qwindow_names[qwi + 1], "hr"))
          matchi1 = which(ds_names %in% tmp_name[1] == TRUE)
          matchi2 = which(ds_names %in% tmp_name[2] == TRUE)
          if (length(matchi1) == 1 & length(matchi2) == 1) {
            ds_names[matchi1] == tmp_name[1]
            ds_names[matchi2] == tmp_name[2]
            if (length(which(qwindow_values_backup[qwi] %in% params_247[["qwindow"]] == TRUE)) > 0) {
              daysummary[di,matchi1] = nvalidhours_qwindow[qwi]
              daysummary[di,matchi2] = nhours_qwindow[qwi]
            } else {
              daysummary[di,matchi1] = "" # note that we only consider qwindows when there is data for the entire window, otherwise duration is 0
              daysummary[di,matchi2] = ""
            }
            fi = fi + 2
          } else {  # variable does not exists, so add it,
            ds_names = c(ds_names,tmp_name)
            daysummary = cbind(daysummary, matrix("", nrow(daysummary), 2))
            newncol = ncol(daysummary)
            if (length(which(qwindow_values_backup[qwi] %in% params_247[["qwindow"]] == TRUE)) > 0) {
              daysummary[di, (newncol - 1):newncol] = c(nvalidhours_qwindow[qwi], nhours_qwindow[qwi])
            } else {
              daysummary[di, (newncol - 1):newncol] = c("", "")
            }
          }
        }
      }
    }
    if (di > 1 & length(which(ds_names == "weekday")) > 0) { #qwindow_actlog == TRUE &
      fi = which(ds_names == "weekday")
    }
    #--------------------------------------
    weekdays = c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday")
    weekdays = rep(weekdays, 104) # hardcoded maximum number of weeks of 104, not ideal
    if (di == 1) {
      daysummary[di,fi] = wdayname
    } else {
      daysummary[di,fi] = weekdays[wdaycode + (di - 1)]
    }
    daysummary[di, (fi + 1)] = di #day number relative to start of measurement
    ds_names[fi:(fi + 1)] = c("weekday","measurementday")
    fi = fi + 2
    if (qwindow_actlog == TRUE | length(params_247[["qwindow"]]) > 2) { # add column with all timestamps in single character
      ds_names[fi] =  "qwindow_timestamps"
      daysummary[di, fi] = paste0(qwindow_times, collapse = "_")
      fi = fi + 1
      ds_names[fi] =  "qwindow_names"
      daysummary[di,fi] = paste0(qwindow_names, collapse = "_")
      fi = fi + 1
    }
    correct_fi = function(di, ds_names, fi, varname) {
      # function to reset variable index to allign with previous day
      if (di > 1) {
        varexists = which(ds_names == varname)
        if (length(varexists) > 0) {
          fi = varexists
        } else {
          fi = which(ds_names == "")[1]
        }
      }
      return(fi)
    }
    if (tooshort == 0) {
      if (nvalidhours >= includedaycrit[1]) {
        #--------------------------------------
        # Features per day and per segment of the day
        # guided by qwindow, which is a vector of indices to slice up the day
        keepindex_46 = keepindex_48 = matrix(NA, length(2:ncol(metashort)), 2)
        # generate objects to help with selecting the slices and to give names to the respective variables
        anwi_t0 = 1 # analysis window time 0
        anwi_t1 = nrow(vari) # analysis window time 1
        anwi_nameindices = "_0-24hr"
        anwi_index = 1
        if (length(params_247[["qwindow"]]) > 0) {
          if (length(params_247[["qwindow"]]) == 2 & params_247[["qwindow"]][1] == 0 & params_247[["qwindow"]][2] == 24) {
          } else {
            oddqwindow = 1:(length(qwindowindices) - 1)
            evenqwindow = 2:length(qwindowindices)
            anwi_t0 = c(anwi_t0, ((qwindowindices[oddqwindow] * 60 * (60 / ws3)) + 1))
            anwi_t1 = c(anwi_t1, (qwindowindices[evenqwindow] * 60 * (60 / ws3)))
            for (iin in 1:length(oddqwindow)) {
              anwi_nameindices = c(anwi_nameindices,paste0("_", qwindow_names[oddqwindow[iin]], "-",
                                                           qwindow_names[evenqwindow[iin]], "hr"))
            }
          }
        } else {
          anwi_t0 = c(anwi_t0, ((0 * 60 * (60 / ws3)) + 1))
          anwi_t1 = c(anwi_t1,(24 * 60 * (60 / ws3)))
          anwi_nameindices = c(anwi_nameindices, "")
        }
        fi_remember = fi
        vari_bu = vari
        for (anwi_index in 1:length(anwi_t0)) { # Loop over starts of the windows
          if (anwi_index != 1 & di == 1) {
            # increase value of fi to leave enough space for the variables to be calculated in second day of measurement
            shift = (deltaLengthQwindow * (fi - fi_remember))
            fi = fi + shift
            fi_remember = fi
          }
          vari = vari_bu
          new = check_daysummary_size(daysummary, ds_names, fi)
          daysummary = new$daysummary
          ds_names = new$ds_names
          
          L5M5window_name = anwi_nameindices[anwi_index]
          anwindices = anwi_t0[anwi_index]:anwi_t1[anwi_index] # indices of varnum corresponding to a segment
          if (length(anwindices) > 0 & all(diff(anwindices) > 0)) { # negative diff(anwindices) may occur in the first day if a qwindow is not within the recorded hours
            minames = colnames(vari)
            
            #=====================================
            # Note: vari equals the imputed time series (metashort) data from one day
            # Also impute the unrecorded period before and after the recording
            NRV = nrow(vari)
            deltaLength = NRV - nrow(averageday)
            if (deltaLength < 0) {
              # Less than 24 hours: Append data from averageday
              if (di == 1) {
                # On first day of recording append the average day to the start
                vari = rbind(averageday[1:abs(deltaLength), ], vari)
                # readjust anwi indices in case that varnum has been imputed
                if (max(anwi_t1) < nrow(vari)) { # since GGIR always calculates full window, max(anwi_t1) should always equals length(varnum)
                  anwi_t0 = anwi_t0 + abs(deltaLength)
                  anwi_t1 = anwi_t1 + abs(deltaLength)
                  # then, we reset the minimum anwi_t0 to 1 to consider the imputed varnum
                  # anwi_t0[which(anwi_t0 == min(anwi_t0))] = 1
                  anwi_t0[1] = 1
                }
              } else {
                # When it is not the first day of recording
                if (NRV == 23) { # day has 23 hours (assuming DST)
                  # Append data after 2nd hour
                  startMissingHour = 2 * 60 * (60/ws3) + 1
                  enMissingHour = 3 * 60 * (60/ws3)
                  vari = rbind(vari[1:(startMissingHour - 1)], averageday[startMissingHour:enMissingHour, ],
                           vari[startMissingHour:nrow(vari),])
                } else { # day has less than 24 hours for another reason
                  # Append the average day to the end
                  a56 = nrow(averageday) - abs(deltaLength) + 1
                  a57 = nrow(averageday)
                  vari = rbind(vari,averageday[a56:a57,])
                }
              }
            } else if (deltaLength > 0) { # 25 hour days, assuming DST
              # If there is more than 24 hours in a day then clock must
              # have moved backward, remove the double hour.
              startDoubleHour = 2 * 60 * (60/ws3) + 1
              endDoubleHour = 3 * 60 * (60/ws3)
              if (nrow(vari) > endDoubleHour) {
                vari = vari[-c(startDoubleHour:endDoubleHour)]
              }
            }
            if (anwi_index != 1) {
              if (length(anwindices) > 0) {
                if (max(anwindices) > length(vari)) {
                  anwindices = anwindices[which(anwindices <= nrow(vari))]
                }
                vari = vari[anwindices, ] #cut short varnum to match day segment of interest
              } else {
                vari = c()
              }
            }
            #===
            for (mi in 1:ncol(vari)) { #run through metrics (for features based on single metrics)
              #=======================================
              # Motivation on the code below:
              # Standardise the number of hours in a day as an attempt to create a
              # fair comparison between days in terms of day length. For example, to
              # compare time spent in intensity levels or MVPA if days are not of
              # equal length, such as when a recording starts in
              # the middle of the day.
              # To do this we impute the missing part based on the average day
              # (literally called averageday in the code).
              # Note: We only do this here in part 2 and not in part 5, and it has
              # been this way since the early days of GGIR.
              # In part 2, GGIR aims to use as much data as possible to provide
              # estimates of behaviour on each recording day, even for the
              # incomplete first and last recording day. As a result, it is
              # important to account for imbalance in day length, which we do below.
              # In part 5, however, GGIR forces the user to only work with complete
              # days and by that the day length is less of a problem and not accounted for.
              
              NRV = length(which(is.na(as.numeric(as.matrix(vari[,mi]))) == FALSE))
              # Note: vari equals the imputed time series (metashort) data from one day
              varnum = as.numeric(as.matrix(vari[,mi])) # Note: varnum is one column of vari
              #==============================
              # varnum_step
              isAccMetric = minames[mi] %in% c("ENMO","LFENMO", "BFEN", "EN", "HFEN", "HFENplus", "MAD", "ENMOa",
                                 "ZCX", "ZCY", "ZCZ", "BrondCount_x", "BrondCount_y",
                                 "BrondCount_z", "NeishabouriCount_x", "NeishabouriCount_y",
                                 "NeishabouriCount_z", "NeishabouriCount_vm", "ExtAct")
              
              if (isAccMetric == TRUE & length(ExtFunColsi) > 0) {
                # Then also extract count metric
                
                varnum_event = as.numeric(as.matrix(vari[,ExtFunColsi]))
                if (NRV < length(averageday[, ExtFunColsi])) {
                  if (di == 1) {
                    varnum_event = c(averageday[1:abs(deltaLength), ExtFunColsi], varnum_event)
                  } else {
                    a56 = length(averageday[, ExtFunColsi]) - abs(deltaLength)
                    a57 = length(averageday[, ExtFunColsi])
                    varnum_event = c(varnum_event, averageday[a56:a57, ExtFunColsi])
                  }
                }
                if (anwi_index != 1) {
                  if (length(anwindices) > 0) {
                    varnum_event = as.numeric(varnum_event[anwindices]) #cut short varnum_event to match day segment of interest
                  } else {
                    varnum_event = c()
                  }
                }
              }
              #=====
              gUnitMetric = length(grep(x = colnames(metashort)[mi], pattern = "BrondCount|ZCX|ZCY|ZCZ|NeishabouriCount|ExtAct", invert = TRUE)) > 0
              UnitReScale = ifelse(test = gUnitMetric, yes = 1000, no = 1)
              # Starting filling output matrix daysummary with variables per day segment and full day.
              if (isAccMetric == TRUE) {
                collectfi = c()
                for (winhr_value in params_247[["winhr"]]) { # Variable (column) names
                  # We are first defining location of variable names, before calculating
                  # variables
                  ML5colna = c(paste0("L",winhr_value,"hr"), paste0("L",winhr_value),
                               paste0("M",winhr_value,"hr"), paste0("M",winhr_value))
                  if (length(params_247[["iglevels"]]) > 0 & length(params_247[["MX.ig.min.dur"]]) == 1) { # intensity gradient (as described by Alex Rowlands 2018)
                    if (winhr_value >= params_247[["MX.ig.min.dur"]]) {
                      for (li in 1:2) { # do twice, once for LX and once for MX
                        varnameig = paste0(paste0(ifelse(li == 1, yes = "L", no = "M"),winhr_value,"_"), c("ig_gradient","ig_intercept","ig_rsquared"))
                        ML5colna = c(ML5colna, varnameig)
                      }
                    }
                  }
                  if (length(params_247[["qM5L5"]]) > 0) {
                    ML5colna = c(ML5colna,
                                 paste0("L", winhr_value, "_q", round(params_247[["qM5L5"]] * 100)),
                                 paste0("M", winhr_value, "_q", round(params_247[["qM5L5"]] * 100)))
                  }
                  # add metric name and timewindow name
                  fi = correct_fi(di, ds_names, fi, varname = paste0(ML5colna[1],"_", colnames(vari)[mi], "_mg",
                                                                     L5M5window_name))
                  ds_names[fi:(fi - 1 + length(ML5colna))] = paste0(ML5colna, "_", colnames(vari)[mi], "_mg",
                                                                    L5M5window_name)
                  collectfi = c(collectfi, fi)
                  fi = fi + length(ML5colna)
                }
                if (length(varnum) > ((60 / ws3) * 60 * min(params_247[["winhr"]]) * 1.2)) { # Calculate values
                  for (winhr_value in params_247[["winhr"]]) {
                    exfi = collectfi[which(params_247[["winhr"]] == winhr_value)]
                    if (length(varnum) > (60 / ws3) * 60 * winhr_value * 1.2) { # Calculate values
                      # Time window for L5 & M5 analysis
                      t0_LFMF =  0 #start
                      # L5M5window[2] #end in 24 hour clock hours (if a value higher than 24 is chosen, it will take early hours of previous day to complete the 5 hour window
                      t1_LFMF = length(varnum) / (60 * (60 / ws3)) + (winhr_value - (params_247[["M5L5res"]] / 60))
                      ML5 = g.getM5L5(varnum, epochSize = ws3, t0_LFMF, t1_LFMF,
                                      params_247[["M5L5res"]], winhr_value,
                                      qM5L5 = params_247[["qM5L5"]],
                                      iglevels = params_247[["iglevels"]],
                                      MX.ig.min.dur = params_247[["MX.ig.min.dur"]])
                      ML5colna = colnames(ML5)
                      ML5 = as.numeric(ML5)
                      if (anwi_index > 1) {
                        L5M5shift = qwindow_values_backup[anwi_index - 1] #qwindow
                      } else {
                        L5M5shift = 0
                      }
                      if (length(ML5) > 3) {
                        ML5 = as.numeric(ML5)
                        ML5[c(1, 3)] = ML5[c(1, 3)] + L5M5shift
                        daysummary[di, (exfi):(exfi + length(ML5) - 1)] = ML5
                      } else {
                        daysummary[di, (exfi):(exfi + length(ML5) - 1)] = ""
                      }
                    } else {
                      daysummary[di,(exfi):(exfi + (4 * length(params_247[["winhr"]])) - 1 + (length(params_247[["qM5L5"]]) * 2))] = ""
                    }
                    # (Below) 4 is the length of ML5 and then 2 extra variables for every qM5L5 value
                    # winhr is not considered because we are in the winhr loop:
                    exfi = exfi + 4 + (length(params_247[["qM5L5"]]) * 2)
                  }
                } else {
                  daysummary[di,collectfi] = ""
                }
                if (anwindices[1] == 1 & length(anwindices) > 6*60*(60/ws3)) { # only derive if 1-6am falls within window
                  fi = correct_fi(di, ds_names, fi, varname = paste0("mean_", colnames(vari)[mi], "_mg_1-6am"))
                  daysummary[di,fi] = mean(varnum[((1 * 60 * (60 / ws3)) + 1):(6 * 60 * (60 / ws3))]) * UnitReScale #from 1am to 6am
                  ds_names[fi] = paste0("mean_",colnames(vari)[mi],"_mg_1-6am"); fi = fi + 1
                }
                if (anwi_nameindices[anwi_index] == "") { # for consistency with previous GGIR version
                  anwi_nameindices[anwi_index] = "_24hr"
                }
                cn_metashort = colnames(vari)
                
                fi = correct_fi(di, ds_names, fi, varname = paste0("mean_", cn_metashort[mi], "_mg",
                                                                   anwi_nameindices[anwi_index]))
                if (length(varnum) > 0) {
                  daysummary[di,fi] = mean(varnum) * UnitReScale
                } else {
                  daysummary[di,fi] = ""
                }
                ds_names[fi] = paste0("mean_", cn_metashort[mi], "_mg", anwi_nameindices[anwi_index]); fi = fi + 1
                if (anwi_nameindices[anwi_index] == "_24hr") {
                  anwi_nameindices[anwi_index] = ""
                }
                if (doquan == TRUE) {
                  q46 = c()
                  q46 = quantile(varnum, probs = params_247[["qlevels"]], na.rm = T, type = quantiletype) * UnitReScale
                  keepindex_46[mi - 1,] = c(fi,(fi + (length(params_247[["qlevels"]]) - 1)))
                  namesq46 = rep(0,length(rownames(as.matrix(q46))))
                  for (rq46i in 1:length(rownames(as.matrix(q46)))) {
                    tmp1 = rownames(as.matrix(q46))[rq46i]
                    tmp2 = as.character(unlist(strsplit(tmp1, "%")))
                    namesq46[rq46i] = paste0("p", tmp2, "_", cn_metashort[mi], "_mg",
                                             anwi_nameindices[anwi_index])
                  }
                  fi = correct_fi(di, ds_names, fi, varname = namesq46[1])
                  if (length(varnum) > 0) {
                    daysummary[di, fi:(fi + (length(params_247[["qlevels"]]) - 1))] = q46
                  } else {
                    daysummary[di, fi:(fi + (length(params_247[["qlevels"]]) - 1))] = ""
                  }
                  ds_names[fi:(fi + (length(params_247[["qlevels"]]) - 1))] = namesq46
                  fi = fi + length(params_247[["qlevels"]])
                }
                if (doilevels == TRUE) {
                  q48 = c()
                  #times 1000 to convert to mg only if it g-unit metric
                  q47 = cut((varnum * UnitReScale), breaks = params_247[["ilevels"]], right = FALSE)
                  q47 = table(q47)
                  q48  = (as.numeric(q47) * ws3) / 60 #converting to minutes
                  keepindex_48[mi - 1,] = c(fi, (fi + (length(q48) - 1)))
                  namesq47 = rep(0, length(rownames(q47)))
                  for (rq47i in 1:length(rownames(q47))) {
                    namesq47[rq47i] = paste0(rownames(q47)[rq47i], "_", cn_metashort[mi], "_mg",
                                             anwi_nameindices[anwi_index])
                  }
                  fi = correct_fi(di, ds_names, fi, varname = namesq47[1])
                  if (length(varnum) > 0) {
                    daysummary[di,fi:(fi + (length(q48) - 1))] = q48
                  } else {
                    daysummary[di, fi:(fi + (length(q48) - 1))] = ""
                  }
                  ds_names[fi:(fi + (length(q48) - 1))] = namesq47
                  fi = fi + length(q48)
                }
                if (doiglevels == TRUE) { # intensity gradient (as described by Alex Rowlands 2018)
                  q49 = c()
                  q50 = cut((varnum * UnitReScale), breaks = params_247[["iglevels"]], right = FALSE)
                  q50 = table(q50)
                  q49  = (as.numeric(q50) * ws3) / 60 #converting to minutes
                  x_ig = zoo::rollmean(params_247[["iglevels"]], k = 2)
                  y_ig = q49
                  igout = g.intensitygradient(x_ig, y_ig)
                  varnameig = paste0(c("ig_gradient", "ig_intercept", "ig_rsquared"),
                                     paste0("_", cn_metashort[mi], anwi_nameindices[anwi_index]))
                  fi = correct_fi(di, ds_names, fi, varname = varnameig[1])
                  if (length(varnum) > 0) {
                    daysummary[di, fi:(fi + 2)] = as.vector(unlist(igout))
                  } else {
                    daysummary[di, fi:(fi + 2)] = rep("", 3)
                  }
                  ds_names[fi:(fi + 2)] = varnameig
                  fi = fi + 3
                }
                #=========================================
                if (domvpa == TRUE) {
                  for (mvpai in 1:length(params_phyact[["mvpathreshold"]])) {
                    mvpa = rep(0,6)
                    if (length(varnum) < 100) {
                      mvpa[1:6] = 0
                    } else {
                      # METHOD 1: time spent above threhold based on 5 sec epoch
                      mvpa[1] = length(which(varnum * UnitReScale >= params_phyact[["mvpathreshold"]][mvpai])) / (60/ws3) #time spent MVPA in minutes
                      # METHOD 2: time spent above threshold based on 1minute epoch
                      varnum2 = cumsum(c(0, varnum))
                      select = seq(1, length(varnum2), by = 60/ws3)
                      varnum3 = diff(varnum2[round(select)]) / abs(diff(round(select)))
                      mvpa[2] = length(which(varnum3 * UnitReScale >= params_phyact[["mvpathreshold"]][mvpai])) #time spent MVPA in minutes
                      # METHOD 3: time spent above threshold based on 5minute epoch
                      select = seq(1, length(varnum2), by = 300/ws3)
                      varnum3 = diff(varnum2[round(select)]) / abs(diff(round(select)))
                      mvpa[3] = length(which(varnum3 * UnitReScale >= params_phyact[["mvpathreshold"]][mvpai])) * 5 #time spent MVPA in minutes
                      # METHOD 4: time spent above threshold
                      boutduration = params_phyact[["mvpadur"]][1] * (60/ws3) # per minute
                      rr1 = matrix(0, length(varnum), 1)
                      p = which(varnum * UnitReScale >= params_phyact[["mvpathreshold"]][mvpai]); rr1[p] = 1
                      getboutout = g.getbout(x = rr1, boutduration = boutduration,
                                             boutcriter = params_phyact[["boutcriter"]], ws3 = ws3)
                      mvpa[4] = length(which(getboutout == 1)) / (60/ws3) #time spent MVPA in minutes
                      
                      # METHOD 5: time spent above threshold 5 minutes
                      boutduration = params_phyact[["mvpadur"]][2] * (60/ws3) #per five minutes
                      rr1 = matrix(0, length(varnum), 1)
                      p = which(varnum * UnitReScale >= params_phyact[["mvpathreshold"]][mvpai]); rr1[p] = 1
                      getboutout = g.getbout(x = rr1, boutduration = boutduration,
                                             boutcriter = params_phyact[["boutcriter"]], ws3 = ws3)
                      mvpa[5] = length(which(getboutout == 1))   / (60/ws3) #time spent MVPA in minutes
                      # METHOD 6: time spent above threshold 10 minutes
                      boutduration = params_phyact[["mvpadur"]][3] * (60/ws3) # per ten minutes
                      rr1 = matrix(0,length(varnum),1)
                      p = which(varnum * UnitReScale >= params_phyact[["mvpathreshold"]][mvpai]); rr1[p] = 1
                      getboutout = g.getbout(x = rr1, boutduration = boutduration,
                                             boutcriter = params_phyact[["boutcriter"]], ws3 = ws3)
                      mvpa[6] = length(which(getboutout == 1)) / (60/ws3) #time spent MVPA in minutes
                    }
                    if (length(which(is.nan(mvpa) == TRUE)) > 0) mvpa[which(is.nan(mvpa) == TRUE)] = 0
                    mvpanames[,mvpai] = c( paste0("MVPA_E", ws3, "S_T", params_phyact[["mvpathreshold"]][mvpai]),
                                           paste0("MVPA_E1M_T", params_phyact[["mvpathreshold"]][mvpai]),
                                           paste0("MVPA_E5M_T", params_phyact[["mvpathreshold"]][mvpai]),
                                           paste0("MVPA_E", ws3, "S_B", params_phyact[["mvpadur"]][1], "M", (params_phyact[["boutcriter"]] * 100), "%_T", params_phyact[["mvpathreshold"]][mvpai]),
                                           paste0("MVPA_E", ws3, "S_B", params_phyact[["mvpadur"]][2], "M", (params_phyact[["boutcriter"]] * 100), "%_T", params_phyact[["mvpathreshold"]][mvpai]),
                                           paste0("MVPA_E", ws3, "S_B", params_phyact[["mvpadur"]][3], "M", (params_phyact[["boutcriter"]] * 100), "%_T", params_phyact[["mvpathreshold"]][mvpai]))
                    for (fillds in 1:6) {
                      mvpavarname = paste0(mvpanames[fillds,mvpai], "_", cn_metashort[mi], anwi_nameindices[anwi_index])
                      fi = correct_fi(di, ds_names, fi, varname = mvpavarname)
                      daysummary[di,fi] = mvpa[fillds]
                      ds_names[fi] = mvpavarname; fi = fi + 1
                    }
                  }
                }
                
                
                if (length(ExtFunColsi) > 0) { # If events are detected with external function
                  if (myfun$reporttype == "event") {
                    # Step bout detection
                    eventBouts = detectEventBouts(myfun, varnum_event = varnum_event,
                                                  varnum = varnum,
                                                  UnitReScale = UnitReScale,
                                                  daysummary = daysummary,
                                                  ds_names = ds_names,
                                                  di = di, fi = fi,
                                                  ws3 = ws3,
                                                  boutnameEnding = paste0(cn_metashort[mi],
                                                                          anwi_nameindices[anwi_index]))
                    
                    daysummary = eventBouts$daysummary
                    ds_names = eventBouts$ds_names
                    di = eventBouts$di
                    fi = eventBouts$fi
                  }
                }
              }
              if (mi %in% ExtFunColsi == TRUE) { # INSERT HERE VARIABLES DERIVED WITH EXTERNAL FUNCTION
                rti = which(ExtFunColsi == mi)
                if (myfun$reporttype[rti] == "event") { # For the event report type we take the sum
                  
                  segmentInfo = list(anwi_nameindices = anwi_nameindices,
                                     anwi_index = anwi_index,
                                     anwi_t0 = anwi_t0,
                                     anwi_t1 = anwi_t1)
                  eventAgg = aggregateEvent(metric_name = cn_metashort[mi],
                                            epochsize = ws3, 
                                            daysummary = daysummary,
                                            ds_names = ds_names,
                                            fi = fi, di = di,
                                            vari = vari,
                                            segmentInfo, myfun, params_247)
                  daysummary = eventAgg$daysummary
                  ds_names = eventAgg$ds_names
                  fi = eventAgg$fi
                } else if (myfun$reporttype[rti] == "scalar") { # For the scalar report type we take the mean
                  varnamescalar = paste0(colnames(vari)[mi], "_mean", anwi_nameindices[anwi_index])
                  fi = correct_fi(di, ds_names, fi, varname = varnamescalar)
                  daysummary[di,fi] = mean(varnum)
                  ds_names[fi] = varnamescalar; fi = fi + 1
                } else if (myfun$reporttype[rti] == "type") { # For type we calculate time spent in each class
                  # Not implemented yet
                }
              }
            }
          }
        }
      }
      rm(val); rm(vari)
    }
  }
  invisible(list(daysummary = daysummary, ds_names = ds_names,
                 windowsummary = windowsummary, ws_names = ws_names))
}
