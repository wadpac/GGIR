g.part5.addNaps = function(sibreport = NULL, ts = NULL, params_general = NULL,
                           params_sleep = NULL, params_phyact = NULL) {
  tz = params_general[["desiredtz"]]
  epochSize_min = as.numeric(difftime(ts$time[2], ts$time[1], units = "mins"))
  if (!is.null(sibreport) &&
      length(sibreport[[1]]) > 1)  {
    #---------------------------------------
    # transform time to POSIX
    if (is.ISO8601(as.character(ts$time[1]))) {
      ts$time = iso8601chartime2POSIX(ts$time, tz = tz)
    }
    sibreport$end = as.POSIXct(sibreport$end, tz = tz)
    sibreport$start = as.POSIXct(sibreport$start, tz = tz)
    
    #---------------------------------------
    # merge sibs when gap is shorter than possible_nap_gap
    if (params_sleep[["possible_nap_gap"]] > 0) {
      sibreport$gap2next = NA
      Nrow = nrow(sibreport)
      sibreport$gap2next[1:(Nrow - 1)] = (as.numeric(sibreport$start[2:Nrow]) - as.numeric(sibreport$end[1:(Nrow - 1)])) / 60
      sibreport$gap2next[which(sibreport$type != "sib" | sibreport$gap2next < 0)] = NA
      iter = 1
      while (iter < nrow(sibreport)) {
        if (!is.na(sibreport$gap2next[iter]) &&
            sibreport$gap2next[iter] < params_sleep[["possible_nap_gap"]]) {
          sibreport$end[iter] = sibreport$end[iter + 1]
          sibreport$mean_acc_1min_after[iter] = sibreport$mean_acc_1min_after[iter + 1]
          sibreport = sibreport[-(iter + 1),]
          sibreport$gap2next[iter] = as.numeric(sibreport$start[iter + 1]) - as.numeric(sibreport$end[iter])
          # no need to increment iter, because by merging the sib blocks
          # the current iter now refers to the next gap
        } else {
          iter = iter + 1
        }
        if (iter > nrow(sibreport) - 1) {
          break
        }
      }
      sibreport$duration = as.numeric(difftime(sibreport$end, sibreport$start, units = "mins")) + epochSize_min
    }
    
    # Only consider sib episodes with minimum duration
    if (length(grep(pattern = "mean_acc_1min", x = colnames(sibreport))) > 0) {
      sibreport$acc_edge = pmax(sibreport$mean_acc_1min_before, sibreport$mean_acc_1min_after)
    } else {
      sibreport$acc_edge = 0
    }
    #-----------------------------------------------------------
    # Consider using marker button data to aid nap detection
    sibreport = markerButtonForRest(sibreport, params_sleep, ts)
    
    sibreport$startHour = as.numeric(format(sibreport$start, "%H"))
    sibreport$endHour = as.numeric(format(sibreport$end, "%H"))
    
    # overlapMidnight = which(sibreport$endHour < sibreport$startHour)
    # if (length(overlapMidnight) > 0) {
    #   sibreport$endHour[overlapMidnight] = sibreport$endHour[overlapMidnight] + 24
    # }
    
    long_nap_boutsi = which((sibreport$type == "sib" &
                          sibreport$duration >= params_sleep[["possible_nap_dur"]][1] &
                          sibreport$duration < tail(params_sleep[["possible_nap_dur"]], n = 1) &
                          sibreport$acc_edge <= params_sleep[["possible_nap_edge_acc"]] &
                          sibreport$startHour >= params_sleep[["possible_nap_window"]][1] &
                          sibreport$endHour < params_sleep[["possible_nap_window"]][2] &
                          sibreport$ignore == FALSE) |
                         (sibreport$type != "sib" & sibreport$duration >= 1))
    Nlongbouts = length(long_nap_boutsi)
    # add segment of sleeplog to sibreport. Done here and not near
    # function g.sibreport called in function g.part5 because the segment needs to
    # be part of the time range otherwise it is omitted further down
    sibreport = sibreport[which(sibreport$type != "sleeplog" & sibreport$type != "sleeplog+bedlog"),]
    sleeplogi = which(ts$selfreported == "sleeplog" | ts$selfreported == "sleeplog+bedlog")
    if (length(sleeplogi) > 0) {
      dsi = diff(sleeplogi)
      sl_starts = c(1, which(dsi != 1) + 1)
      sl_ends = c(which(dsi != 1), length(sleeplogi))
      if (length(sl_starts) > 0) {
        for (Nsi in 1:length(sl_starts)) {
          newline = nrow(sibreport) + 1
          sibreport[newline,] = NA
          sibreport$ID[newline] = sibreport$ID[newline - 1]
          sibreport$type[newline] = "sleeplog"
          sibreport$start[newline] = format(ts$time[sleeplogi[sl_starts[Nsi]]])
          sibreport$end[newline] = format(ts$time[sleeplogi[sl_ends[Nsi]]])
        }
      }
    }
  } else {
    Nlongbouts = 0
    long_nap_boutsi = NULL
  }
  if (length(long_nap_boutsi) > 0 & nrow(ts) > 0) {
    sibreport = sibreport[long_nap_boutsi,]
    srep_tmp = sibreport[which(sibreport$start >= min(ts$time) &
                                 sibreport$end <= max(ts$time)),]
    
    # update ts time series with the classified naps
    if ("sib" %in% srep_tmp$type) {
      sibnaps = which(srep_tmp$type == "sib")
      if (length(sibnaps) > 0) {
        accThreshold = params_sleep[["threshold.nap"]]
        for (sni in 1:length(sibnaps)) {
          sibnap = which(ts$time >= srep_tmp$start[sibnaps[sni]] & ts$time <= srep_tmp$end[sibnaps[sni]])
          if (length(sibnap) > 0) {
            # Only consider nap if:
            # - The 95th percentile acceleration is less than the threshold of light physical activity
            ACCp95 = as.numeric(quantile(ts$ACC[sibnap], probs = 0.95))
            # - Surrounding hour has more than 15% sib
            sib_mid_point = sibnap[ceiling(length(sibnap)/2)]
            sib_window = c(sib_mid_point - (30 / epochSize_min), (sib_mid_point + (30 / epochSize_min)))
            if (sib_window[1] < 1) sib_window[1] = 1
            if (sib_window[2] > nrow(ts)) sib_window[2] = nrow(ts)
            fractionRest = length(which(ts$sibdetection[sib_window[1]:sib_window[2]] > 0)) / (61 / epochSize_min)
            if (ACCp95 < accThreshold && fractionRest > 0.25) {
              nap_dur_min = length(sibnap) / (1 / epochSize_min)
              nap_dur_class = which(c(params_sleep[["possible_nap_dur"]], Inf) > nap_dur_min)[1]
              # expected class number is 2 or higher, e.g.
              # possible_nap_dur c(0, 10, 30) with nap_dur_min = 5, will result in 2
              # possible_nap_dur c(10, 30) with nap_dur_min = 12, will result in 2
              # possible_nap_dur c(10, 30) with nap_dur_min = 10, will result in 2
              ts$sibdetection[sibnap] = nap_dur_class
            }
          }
        }
      }
    }
  }
  invisible(list(ts = ts, sibreport = sibreport, long_nap_boutsi = long_nap_boutsi))
}