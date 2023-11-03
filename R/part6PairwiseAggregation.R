part6PairwiseAggregation = function(outputdir = NULL, desiredtz = "", verbose = TRUE) {
  #----------------------------------------------------------
  # declare internal functions only used in this function:
  remove_invalid = function(wx, N) {
    # wx are indices for wakeup times
    # N is length of time serieses
    valid = which(wx <= N - 15)
    if (length(valid) > 0) {
      wx = wx[valid]
    } else {
      wx = NULL
    }
    return(wx)
  }
  
  find_nearest_b_to_a = function(a, b) {
    # Example of how this works
    # a = c(1, 10, 20, 30)
    # b = c(2, 9, 17, 21, 28)
    # nearest_b_to_a = unlist(lapply(X = a,
    #                                FUN = find_nearest_b_to_a, b))
    # which should be: 2  9 21 28
    return(b[which.min(abs(a - b))])
  }
  
  
  calcSimilarity = function(x, y) {
    EqualState = c(x == y)
    testNA = c(is.na(x) | is.na(y))
    SummedValue = sum(ifelse(test = EqualState[which(testNA == FALSE)] == TRUE, yes = 1, no = 0))
    NValuesSkipped = length(which(testNA == TRUE))
    M = length(x)
    frac_valid = round((M - NValuesSkipped) / M, digits = 4)
    if (M != NValuesSkipped) {
      similarity = round(-100 + (200/(M - NValuesSkipped)) * SummedValue, digits = 3)
    } else {
      frac_valid = NA
      similarity = NA
    }
    invisible(list(similarity = similarity, frac_valid = frac_valid))
  }

  #----------------------------------------------------------
  
  if (verbose == TRUE) {
    cat("\n  Aggregate per pair:")
  }
  path_hh_ts = paste0(outputdir, "/part6HouseholdCoAnalysis/alignedTimeseries")
  if (!dir.exists(path_hh_ts)) stop("first process data with function align_individuals")
  
  compfolder =  paste0(outputdir, "/part6HouseholdCoAnalysis")

  fns = dir(path = path_hh_ts, full.names = TRUE)
  
  # loop over households
  pairsum_all_housholds = NULL
  if (length(fns) > 0) {
    for (i in 1:length(fns)) {
      # load entire data for household
      load(file = fns[i])
      uHID = unique(alignedTimeseries$HID)
      if (verbose == TRUE) cat(paste0("\n    Household: ", uHID))
      #unique member IDs
      uMID = gsub(pattern = "ACC.", replacement = "", x = grep(pattern = "ACC", x = colnames(alignedTimeseries), value = TRUE))
      #unique pair IDs
      uPID = gsub(pattern = "validpair_", replacement = "", x = grep(pattern = "validpair", x = colnames(alignedTimeseries), value = TRUE))
      alignedTimeseries$time_POSIX = as.POSIXct(alignedTimeseries$timenum, tz = desiredtz, origin = "1970-01-01")
      alignedTimeseries$date = as.character(as.Date(alignedTimeseries$time_POSIX))
      Npairs = length(uPID)
      pairsum_final = NULL
      if (length(uPID) > 0) {
        # Loop over pairs
        for (j in 1:length(uPID)) {
          # extrac MID for members in the pair
          pp = unlist(strsplit(uPID[j], "_"))
          m1 = pp[1]    
          m2 = pp[2]
          # simplify D to only columns of interest
          dataForPair = alignedTimeseries[, grep(pattern = paste0("time|date|HID|",m1,"|",m2), x = colnames(alignedTimeseries)), ] #guider|window|valid
          validpaircol = grep(pattern = uPID[j], x = colnames(dataForPair)) # column index where the indicator of valid/invalid can be found
          invalidepochs = which(dataForPair[,validpaircol] == FALSE)
          # Turn all data invalid
          if (length(invalidepochs) > 0) {
            is.na(dataForPair[invalidepochs, grep(pattern = "time|date|window|HID|guider|valid", x = colnames(dataForPair), invert = TRUE)]) = TRUE
          }
          # extract wake-up times in the entire recording
          index_wake1 = which(dataForPair[, paste0("wakeup.",m1)] == 1)
          index_wake2 = which(dataForPair[, paste0("wakeup.",m2)] == 1)
          index_onsetpair = rowSums(dataForPair[, paste0("onset.", c(m1, m2))])
          # ignore wake-up times that are  within 15 minutes before the end of the recording
          index_wake1 = remove_invalid(wx = index_wake1, N = length(dataForPair[,validpaircol]))
          index_wake2 = remove_invalid(wx = index_wake2, N = length(dataForPair[,validpaircol]))
          # pair nearest times
          N = length(index_wake1)
          do.pair.sum = FALSE
          if (N > 0 & length(index_wake2) > 0) {
            pw = data.frame(index_wake1 = index_wake1,
                            index_wake2 = unlist(lapply(X = index_wake1,
                                                        FUN = find_nearest_b_to_a, index_wake2)),
                            wakeup_time1 = character(N),
                            wakeup_time2 = character(N),
                            wakeup_firstMID = character(N))
            
            pairsum = pw[which(abs(pw[,2] - pw[, 1]) < 12 * 60), ]
            if (nrow(pairsum) > 0) do.pair.sum = TRUE
          } else {
            # No valid data, but still create empty row to 
            # communicate that there was no data
            pairsum = data.frame(index_wake1 = NA, index_wake2 = NA,
                                 wakeup_time1 = NA, wakeup_time2 = NA,
                                 wakeup_firstMID = NA)
          }
          if (do.pair.sum == TRUE) {
            pairsum$PID = uPID[j]
            pairsum$HID = uHID
            pairsum$Npairs = Npairs
            pairsum$event = 1:nrow(pairsum)
            pairsum$MID1 = m1
            pairsum$MID2 = m2
          } else {
            pairsum = data.frame(index_wake1 = NA, index_wake2 = NA,
                                 wakeup_time1 = NA, wakeup_time2 = NA,
                                 wakeup_firstMID = NA)
            pairsum[, c("PID", "HID",
                        "Npairs", "event",
                        "MID1", "MID2")] = NA
          }
          # Initialise wakeup variables
          pairsum[, c("wakeup_date1", "wakeup_date2",
                      "wakeup_acc_1_before_2_mg", "wakeup_acc_2_before_2_mg",
                      "wakeup_lux_1_before_2", "wakeup_lux_2_before_2",
                      "wakeup_secondMID", "wakeup_deltatime_min")] = NA
          if (do.pair.sum == TRUE) {
            pairsum$wakeup_date1 = as.Date(pairsum$wakeup_date1)
            pairsum$wakeup_date2 = as.Date(pairsum$wakeup_date2)
          }
          # initialise day variables
          pairsum[, c("day_r_acceleration", "day_ICC_active", "day_ICC_active_Fvalue",
                       "day_Kappa_active","day_Kappa_active_CI_lower", "day_Kappa_active_CI_upper",
                       "day_ActivitySimilarityIndex", "day_frac_valid", "day_duration_awake")] = NA
          
          # initialise night variables
          pairsum[, c("night_ICC_sleep", "night_ICC_sleep_Fvalue",
                      "night_Kappa_sleep", "night_Kappa_sleep_CI_lower",
                      "night_Kappa_sleep_CI_upper",
                      "night_SleepSimilarityIndex",
                      "night_frac_valid", "nigth_duration_night")] = NA
          
          # initialise sptPattern variables
          pairsum[, c("sptPattern_sameTime",
                      "sptPattern_M2thenM1", "sptPattern_M1thenM2",
                      "sptPattern_soloWakeM1", "sptPattern_soloWakeM2",
                      "sptPattern_sptduration", "sptPattern_fractionValid")] = 0
          if (do.pair.sum == TRUE) {
            # loop of wakeup pairs:
            for (h in 1:nrow(pairsum)) {
              # Assess who woke up first
              pairsum$wakeup_firstMID[h] = ifelse(pairsum$index_wake1[h] < pairsum$index_wake2[h], yes = m1,
                                                  no = ifelse(pairsum$index_wake1[h] > pairsum$index_wake2[h], yes = m2, no = "equal"))
              # Deduce who woke up second
              secondawake = ifelse(test = pairsum$wakeup_firstMID[h] == pairsum$MID1[h], yes = pairsum$MID2[h], no = pairsum$MID1[h])
              pairsum$wakeup_secondMID[h] = secondawake
              # Assess delta time of waking up
              pairsum$wakeup_deltatime_min[h] = difftime(time1 = dataForPair$time_POSIX[pairsum$index_wake2[h]], time2 = dataForPair$time_POSIX[pairsum$index_wake1[h]], units = "mins")
              # Extract dates of waking up
              pairsum$wakeup_date1[h] = as.Date(dataForPair$time_POSIX[pairsum$index_wake1[h]])
              pairsum$wakeup_date2[h] = as.Date(dataForPair$time_POSIX[pairsum$index_wake2[h]])
              
              pairsum$wakeup_time1[h] = format(dataForPair$time_POSIX[pairsum$index_wake1[h]], format = "%H:%M:%S")
              pairsum$wakeup_time2[h] = format(dataForPair$time_POSIX[pairsum$index_wake2[h]], format = "%H:%M:%S")
              
              index_last_wakeup = pairsum$index_wake1[h]
              if (pairsum$wakeup_firstMID[h] != "equal") {
                index_last_wakeup = max(pairsum[h,c("index_wake1", "index_wake2")])
                # Activity of person who first woke up during minute before second person wake up
                pairsum$wakeup_acc_1_before_2_mg[h] = mean(dataForPair[(index_last_wakeup - 3):index_last_wakeup,
                                                                    paste0("ACC.", pairsum$wakeup_firstMID[h])])
                # Activity of second person to wake up before they woke up
                pairsum$wakeup_acc_2_before_2_mg[h] = mean(dataForPair[(index_last_wakeup - 3):index_last_wakeup, 
                                                                    paste0("ACC.", secondawake)])
                # LUX of person who first woke up during minute before second person wake up
                pairsum$wakeup_lux_1_before_2[h] = max(dataForPair[(index_last_wakeup - 3):index_last_wakeup, 
                                                                   paste0("lightpeak.", pairsum$wakeup_firstMID[h])])
                # LUX of second person to wake up before they woke up
                pairsum$wakeup_lux_2_before_2[h] = max(dataForPair[(index_last_wakeup - 3):index_last_wakeup, 
                                                                   paste0("lightpeak.", secondawake)])
              } else if (pairsum$wakeup_firstMID[h] == "equal") {
                pairsum$wakeup_acc_1_before_2_mg[h] = NA
                pairsum$wakeup_acc_2_before_2_mg[h] = NA
                pairsum$wakeup_lux_1_before_2[h] = NA
                pairsum$wakeup_lux_2_before_2[h] = NA
              }
              #------------------------------------------------
              # Describe matching waking hours
              ind.wake = which(dataForPair$date == as.character(pairsum$wakeup_date1[h]) &
                                 dataForPair[, paste0("SleepPeriodTime.", m1)] == 0 &
                                 dataForPair[, paste0("SleepPeriodTime.", m2)] == 0)
              
              if (length(ind.wake) > 0) {
                ts_comp = dataForPair[ind.wake, 
                                      c(paste0("class_id.", c(m1, m2)),
                                        paste0("ACC.", c(m1, m2)))]
                if (length(which(rowSums(is.na(ts_comp[, 3:4])) == 0)) > 120) {
                  # Correlation between continuous acceleration values
                  pairsum$day_r_acceleration[h] = round(cor(x = ts_comp[, 3], y = ts_comp[,4]), digits = 3)
                  
                  # derive binary class of inactive / active
                  for (ja in 3:4) ts_comp[,ja] = ifelse(test = ts_comp[,ja] < 50, yes = 1, no = 0)
                  # ICC based on binary scores
                  IRR = irr::icc(ts_comp[, 3:4], model = "twoway", 
                                 type = "agreement", unit = "single")
                  pairsum$day_ICC_active[h] = round(IRR$value, digits = 3)
                  pairsum$day_ICC_active_Fvalue[h] = round(IRR$Fvalue, digits = 3)
                  # Cohen's Kappa based on binary scores
                  KP = psych::cohen.kappa(x = ts_comp[, 3:4])
                  KP$confid = round(KP$confid, digits = 3)
                  pairsum$day_Kappa_active[h] = round(KP$weighted.kappa, digits = 3)
                  pairsum$day_Kappa_active_CI_lower[h] = KP$confid[2,1]
                  pairsum$day_Kappa_active_CI_upper[h] = KP$confid[2,3]
                  # Similarity in binary scores
                  SIM = calcSimilarity(x = ts_comp[, 3], y = ts_comp[, 4])
                  pairsum$day_ActivitySimilarityIndex[h] = SIM$similarity
                  pairsum$day_frac_valid[h] = SIM$frac_valid
                  
                  pairsum$day_duration_awake = length(ind.wake)
                }
              }
              #------------------------------------------------
              # Describe noon-noon window of waking up in more detail
              ind.night = which(dataForPair$date == as.character(pairsum$wakeup_date1[h])) 
              # ignore first hour
              ind.night = ind.night - (12 * 60)
              ind.night = ind.night[which(ind.night > 0)]
              if (length(ind.night) > 0) {
                ts_comp = dataForPair[ind.night, c(paste0("class_id.", c(m1, m2)), paste0("ACC.", c(m1, m2)))]
                if (length(which(rowSums(is.na(ts_comp[,1:2])) == 0)) > 120) {
                  # derive binary class of sleep / wakefulness
                  for (ja in 1:2) ts_comp[,ja] = ifelse(test = ts_comp[,ja] < 1, yes = 1, no = 0)
                  # ICC based on binary scores
                  IRR = irr::icc(ts_comp[, 1:2], model = "twoway", 
                                 type = "agreement", unit = "single")
                  pairsum$night_ICC_sleep[h] = round(IRR$value, digits = 3)
                  pairsum$night_ICC_sleep_Fvalue[h] = round(IRR$Fvalue, digits = 3)
                  
                  # Cohen's Kappa based on binary scores
                  KP = psych::cohen.kappa(x = ts_comp[, 1:2])
                  KP$confid = round(KP$confid, digits = 3)
                  pairsum$night_Kappa_sleep[h] = round(KP$weighted.kappa, digits = 3)
                  pairsum$night_Kappa_sleep_CI_lower[h] = KP$confid[2,1]
                  pairsum$night_Kappa_sleep_CI_upper[h] = KP$confid[2,3]
                  # Similarity in binary scores
                  SIM = calcSimilarity(x = ts_comp[, 1], y = ts_comp[, 2])
                  pairsum$night_SleepSimilarityIndex[h] = SIM$similarity
                  pairsum$night_frac_valid[h] = SIM$frac_valid
                  pairsum$nigth_duration_night[h] = length(ind.night)
                }
              }
              # Describe wakefulness dynamics during the SPT prior to wakeup.
              onsetsi = which(index_onsetpair[1:pairsum$index_wake1[h]] > 0)
              if (length(onsetsi) > 0) {
                # indicies of the spt prior to the wake up
                ind.priorspt = (as.numeric(tail(onsetsi, n = 1)) + 1):pairsum$index_wake1[h]
                Npriorspt = length(ind.priorspt)
                # Fraction valid data
                fractionValid = sum(dataForPair[ind.priorspt,validpaircol], na.rm = TRUE) / Npriorspt
                pairsum$sptPattern_sptduration[h] = Npriorspt
                pairsum$sptPattern_fractionValid[h] = fractionValid
                Dtemp = dataForPair[ind.priorspt, ]
                # Identify wakeup times during the night
                sleep = Dtemp[, c(paste0("class_id.", m1), paste0("class_id.", m2))]
                sleep = ifelse(test = sleep >= 1, yes = 1, no = 0)
                for (nn in 1:(nrow(sleep) - 5)) {
                  # if at least one person wakes up
                  if (sum(sleep[nn, 1:2]) >= 1 & sum(sleep[nn - 1, 1:2]) == 0) {
                    if (sum(sleep[nn, 1:2]) == 2) {
                      # both wake up in the same minute
                      pairsum$sptPattern_sameTime[h] = pairsum$sptPattern_sameTime[h] + 1
                    } else {
                      next5min = rowSums(sleep[(nn + 1):(nn + 5), ])
                      if (any(next5min == 2)) {
                        # other person also wakes up
                        if (sleep[nn, 1] == 1) {
                          pairsum$sptPattern_M1thenM2[h] = pairsum$sptPattern_M1thenM2[h] + 1 # M1 first M2 second
                        } else {
                          pairsum$sptPattern_M2thenM1[h] = pairsum$sptPattern_M2thenM1[h] + 1 # M2 first M1 second
                        }
                      } else {
                        # other person does not wake up
                        if (sleep[nn, 1] == 1) {
                          pairsum$sptPattern_soloWakeM1[h] = pairsum$sptPattern_soloWakeM1[h] + 1 # M1 but not followed by M2
                        } else {
                          pairsum$sptPattern_soloWakeM2[h] = pairsum$sptPattern_soloWakeM2[h] + 1 # M2 but not followed by M1
                        }
                      }
                    }
                  }
                }
              }
            }
            # reorder columns
            colorder = c("HID", "Npairs", "PID", "MID1", "MID2", "event", "wakeup_time1", 
                         "wakeup_time2", "wakeup_date1", "wakeup_date2", "wakeup_firstMID", "wakeup_secondMID", 
                         "wakeup_deltatime_min", "wakeup_acc_1_before_2_mg", "wakeup_acc_2_before_2_mg",
                         "wakeup_lux_1_before_2", "wakeup_lux_2_before_2", grep(pattern = "sptPattern", x = names(pairsum), value = TRUE))
            pairsum = pairsum[, c(colorder, sort(colnames(pairsum)[which(colnames(pairsum) %in% colorder == FALSE)]))]
          } else {
            # no data for this pair, set the default zeros to missing
            is.na(pairsum[which(pairsum == 0)]) = TRUE
          }
          # remove columns we do not want in the ouput
          pairsum = pairsum[, which(colnames(pairsum) %in% c("index_wake1", "index_wake2") == FALSE)]
          # append to previous result
          pairsum_final = rbind(pairsum_final, pairsum)
          rm(pairsum)
        }
      }
      pairsum_all_housholds = rbind(pairsum_all_housholds, pairsum_final)
    }
    if (length(pairsum_all_housholds) > 0) {
      for (i in 1:ncol(pairsum_all_housholds)) {
        NA_NaN = which(is.na( pairsum_all_housholds[, i]))
        pairsum_all_housholds[NA_NaN, i] = ""
      }
      write.csv(x = pairsum_all_housholds, file = paste0(compfolder, "/pairwise_summary_all_housholds.csv"), row.names = FALSE)
    }
   }
}