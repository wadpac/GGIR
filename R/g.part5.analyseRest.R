g.part5.analyseRest = function(sibreport = NULL, dsummary = NULL,
                               ds_names = NULL, fi = NULL, di = NULL,
                               ts = NULL, tz = NULL,
                               params_sleep = NULL, long_nap_boutsi = NULL) {
  # define function to summarise overlap between selfreported behaviour and sibs
  summarise_overlap = function(srep_tmp, X, Y, xi, yi, name = "", sumobject = NULL) {
    # X: column name in srep_temp to reflect overlap SIB with Selfreport
    # Y: column name in srep_temp to reflect overlap Selfreport with SIB
    # xi: indices of overlapping sibs corresponding to X
    # yi: indices of overlapping sibs corresponding to Y
    # name: name of self-reported behaviour to be used inside the variable name
    
    dsummary = sumobject$dsummary
    ds_names = sumobject$ds_names
    fi = sumobject$fi
    di = sumobject$di
    
    calcOverlapPercentage = function(overlap, duration) {
      return(sum(overlap * duration) / sum(duration))
    }
    # Overlap sib with selfreport
    if (length(xi) > 0) {
      dsummary[di,fi:(fi + 2)] = c(mean(srep_tmp$duration[xi]),
                                   sum(srep_tmp$duration[xi]),
                                   calcOverlapPercentage(overlap = srep_tmp[xi, X],
                                                         duration = srep_tmp$duration[xi]))
    } else {
      dsummary[di,fi:(fi + 2)] = c(0, 0, 0)
    }
    ds_names[fi:(fi + 2)] = c(paste0("mdur_denap_overl_", name),
                              paste0("tdur_denap_overl_", name),
                              paste0("perc_denap_overl_", name))
    fi = fi + 3
    # Overlap selfreport with sib
    if (length(xi) > 0) {
      dsummary[di,fi:(fi + 2)] = c(mean(srep_tmp$duration[yi]),
                                   sum(srep_tmp$duration[yi]),
                                   calcOverlapPercentage(overlap = srep_tmp[yi, Y],
                                                         duration = srep_tmp$duration[yi]))
    } else {
      dsummary[di,fi:(fi + 2)] = c(0, 0, 0)
    }
    ds_names[fi:(fi + 2)] = c(paste0("mdur_", name, "_overl_denap"),
                              paste0("tdur_", name, "_overl_denap"),
                              paste0("perc_", name ,"_overl_denap"))
    fi = fi + 3
    invisible(list(fi = fi, ds_names = ds_names, dsummary = dsummary, di = di))
  }
  
  epochSize_min = as.numeric(difftime(ts$time[2], ts$time[1], units = "mins"))
  Nlongbouts = length(long_nap_boutsi)
  ds_names[fi:(fi + 26)] = c("sibreport_n_items",
                             "sibreport_n_items_day", "nbouts_day_denap",
                             "nbouts_day_srnap", "nbouts_day_srnonw",
                             "noverl_denap_srnap", "noverl_denap_srnonw",
                             "noverl_srnap_denap", "noverl_srnonw_denap",
                             "frag_mean_dur_denap_day", "dur_day_denap_min",
                             "frag_mean_dur_srnap_day", "dur_day_srnap_min",
                             "frag_mean_dur_srnonw_day", "dur_day_srnonw_min",
                             "mdur_denap_overl_srnap", "tdur_denap_overl_srnap",
                             "perc_denap_overl_srnap", "mdur_srnap_overl_denap",
                             "tdur_srnap_overl_denap", "perc_srnap_overl_denap",
                             "mdur_denap_overl_srnonw", "tdur_denap_overl_srnonw",
                             "perc_denap_overl_srnonw", "mdur_srnonw_overl_denap",
                             "tdur_srnonw_overl_denap", "perc_srnonw_overl_denap")
  dsummary[di,fi] = Nlongbouts
  fi = fi + 1
  if (length(long_nap_boutsi) > 0 & nrow(ts) > 0) {
    sibreport = sibreport[long_nap_boutsi,]
    srep_tmp = sibreport[which(sibreport$start >= min(ts$time) &
                                 sibreport$end <= max(ts$time)),]
    
    # update ts time series with the classified naps
    if ("sib" %in% srep_tmp$type) {
      sibnaps = which(srep_tmp$type == "sib")
      srep_tmp_rowsdelete = NULL
      if (length(sibnaps) > 0) {
        for (sni in 1:length(sibnaps)) {
          sibnap = which(ts$time >= srep_tmp$start[sibnaps[sni]] & ts$time <= srep_tmp$end[sibnaps[sni]])
          if (length(sibnap) > 0) {
            # Only consider nap it does not overlap for more than 10% with known nonwear.
            fractionInvalid = length(which(ts$nonwear[sibnap] == 1)) / length(sibnap)
            if (fractionInvalid >= 0.5) {
              srep_tmp_rowsdelete = c(srep_tmp_rowsdelete , sibnaps[sni])
            }
          }
        }
        if (!is.null(srep_tmp_rowsdelete )) {
          srep_tmp = srep_tmp[-srep_tmp_rowsdelete,]
        }
      }
    }
    #	identify overlapping and non-overlapping, (nap-sib, non-wear-sib, sib, nap, nonwear)
    #	calculate for all five categories number, total duration, mean duration
    # but also account for possibility that some of these categories do not exist
    
    # for qc purposes:
    dsummary[di,fi] = nrow(srep_tmp)
    # if (ds_names[fi] != "sibreport_n_items_day") stop("mismatch in columnnames 1")
    fi = fi + 1
    if (nrow(srep_tmp) > 0) {
      sibs = which(srep_tmp$type == "sib")
      srep_tmp$SIBoverlapNonwear = 0
      srep_tmp$SIBoverlapNap = 0
      srep_tmp$SIBoverlapSleeplog = 0
      srep_tmp$NonwearOverlapSIB = 0
      srep_tmp$NapOverlapSIB = 0
      srep_tmp$SleeplogOverlapSIB = 0
      srep_tmp$start = as.POSIXct(srep_tmp$start, tz = tz)
      srep_tmp$end = as.POSIXct(srep_tmp$end, tz = tz)
      if (length(sibs) > 0) {
        classes = unique(srep_tmp$type)
        selfreport = which(srep_tmp$type == "nonwear" | srep_tmp$type == "nap" | srep_tmp$type == "sleeplog") 
        # summarise overlap between selfreported and accelerometer-based SIB
        if (length(selfreport) > 0) {
          for (si in sibs) {
            for (sr in selfreport) {
              # SIB overlap with selfreported behaviour
              if (srep_tmp$start[si] <= srep_tmp$end[sr] &
                  srep_tmp$end[si] >= srep_tmp$start[sr]) {
                end_overlap = as.numeric(pmin(srep_tmp$end[si], srep_tmp$end[sr]))
                start_overlap = as.numeric(pmax(srep_tmp$start[si], srep_tmp$start[sr]))
                duration_overlap = end_overlap - start_overlap
                duration_sib = as.numeric(srep_tmp$end[si]) - as.numeric(srep_tmp$start[si])
                # percentage of overlap
                perc_overlap = round(100 * (duration_overlap / duration_sib), digits = 1)
                if (srep_tmp$type[sr] == "nonwear") {
                  srep_tmp$SIBoverlapNonwear[si] = perc_overlap
                } else if (srep_tmp$type[sr] == "nap") {
                  srep_tmp$SIBoverlapNap[si] = perc_overlap
                } else if (srep_tmp$type[sr] == "sleeplog") {
                  srep_tmp$SIBoverlapSleeplog[si] = perc_overlap
                }
              }
              # Selfreport behaviour overlap with SIB
              if (srep_tmp$start[sr] <= srep_tmp$end[si] &
                  srep_tmp$end[sr] >= srep_tmp$start[si]) {
                end_overlap = as.numeric(pmin(srep_tmp$end[si], srep_tmp$end[sr]))
                start_overlap = as.numeric(pmax(srep_tmp$start[si], srep_tmp$start[sr]))
                duration_overlap = end_overlap - start_overlap
                duration_sr = as.numeric(srep_tmp$end[sr]) - as.numeric(srep_tmp$start[sr])
                # percentage of overlap
                perc_overlap = round(100 * (duration_overlap / duration_sr), digits = 1)
                if (srep_tmp$type[sr] == "nonwear") {
                  srep_tmp$NonwearOverlapSIB[sr] = perc_overlap
                } else if (srep_tmp$type[sr] == "nap") {
                  srep_tmp$NapOverlapSIB[sr] = perc_overlap
                } else if (srep_tmp$type[sr] == "sleeplog") {
                  srep_tmp$SleeplogOverlapSIB[sr] = perc_overlap
                }
              }
            }
          }
        }
      }
      # Identify where segments overlap
      sibs_indices = which(srep_tmp$type == "sib")
      nap_indices = which(srep_tmp$type == "nap")
      nonwear_indices = which(srep_tmp$type == "nonwear")
      # sleeplog_indices = which(srep_tmp$type == "sleeplog")
      SIBoverlapNap_indices = which(srep_tmp$SIBoverlapNap != 0)
      SIBoverlapNonwear_indices = which(srep_tmp$SIBoverlapNonwear != 0)
      SIBoverlapSleeplog_indices = which(srep_tmp$SIBoverlapSleeplog != 0)
      NapOverlapSIB_indices = which(srep_tmp$NapOverlapSIB != 0)
      NonwearOverlapSIB_indices = which(srep_tmp$NonwearOverlapSIB != 0)
      SleeplogOverlapSIB_indices = which(srep_tmp$NonwearOverlapSIB != 0)
      
      # Count number of occurrences (do not count sleeplog because not informative)
      dsummary[di,fi:(fi + 6)] = c(length(sibs_indices),
                                   length(nap_indices),
                                   length(nonwear_indices),
                                   length(SIBoverlapNap_indices),
                                   length(SIBoverlapNonwear_indices),
                                   length(NapOverlapSIB_indices),
                                   length(NonwearOverlapSIB_indices))
      fi = fi + 7      
      
      # mean and total duration in sib per day
      if (length(sibs_indices) > 0) {
        dsummary[di,fi:(fi + 1)] = c(mean(srep_tmp$duration[sibs_indices]),
                                     sum(srep_tmp$duration[sibs_indices]))
      } else {
        dsummary[di,fi:(fi + 1)] = c(0, 0)
      }
      fi = fi + 2
      # mean and total duration in self-reported naps per day
      if (length(nap_indices) > 0) {
        srep_tmp$duration[nap_indices] = (as.numeric(srep_tmp$end[nap_indices]) -
                                            as.numeric(srep_tmp$start[nap_indices])) / 60
        dsummary[di,fi:(fi + 1)] = c(mean(srep_tmp$duration[nap_indices]),
                                     sum(srep_tmp$duration[nap_indices]))
      } else {
        dsummary[di,fi:(fi + 1)] = c(0, 0)
      }
      fi = fi + 2
      # mean and total duration in self-reported nonwear per day
      if (length(nonwear_indices) > 0) {
        dsummary[di,fi:(fi + 1)] = c(mean(srep_tmp$duration[nonwear_indices]),
                                     sum(srep_tmp$duration[nonwear_indices]))
      } else {
        dsummary[di,fi:(fi + 1)] = c(0, 0)
      }
      fi = fi + 2
      so = list(fi = fi, ds_names = ds_names, dsummary = dsummary, di = di)
      # Self-reported naps
      so = summarise_overlap(
        srep_tmp,
        X = "SIBoverlapNap",
        Y = "NapOverlapSIB",
        xi = SIBoverlapNap_indices,
        yi = NapOverlapSIB_indices,
        name = "srnap",
        sumobject = so
      )
      # Self-reported nonwear
      so = summarise_overlap(
        srep_tmp,
        X = "SIBoverlapNonwear",
        Y = "NonwearOverlapSIB",
        xi = SIBoverlapNonwear_indices,
        yi = NonwearOverlapSIB_indices,
        name = "srnonw",
        sumobject = so
      )
      dsummary = so$dsummary; ds_names = so$ds_names; fi = so$fi
      rm(srep_tmp)
    } else {
      fi  =  fi + 25
    }
  } else {
    fi  =  fi + 26
  }
  invisible(list(fi = fi, di = di, ds_names = ds_names, dsummary = dsummary, ts = ts))
}
