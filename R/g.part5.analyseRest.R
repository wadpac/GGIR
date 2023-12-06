g.part5.analyseRest = function(sibreport = NULL, dsummary = NULL,
                                ds_names = NULL, fi = NULL, di = NULL,
                                time = NULL, tz = NULL, possible_nap_dur = 0,
                               possible_nap_edge_acc = Inf) {
  # transform time to POSIX
  if (is.ISO8601(as.character(time[1]))) {
    time = iso8601chartime2POSIX(time, tz = tz)
  }
  sibreport$end = as.POSIXct(sibreport$end, tz = tz)
  sibreport$start = as.POSIXct(sibreport$start, tz = tz)
  
  # Only consider sib episodes with minimum duration
  if (length(grep(pattern = "mean_acc_1min", x = colnames(sibreport))) > 0) {
    sibreport$acc_edge = pmax(sibreport$mean_acc_1min_before, sibreport$mean_acc_1min_after)
  } else {
    sibreport$acc_edge = 0
  }
  longboutsi = which((sibreport$type == "sib" &
                        sibreport$duration >= possible_nap_dur[1] &
                        sibreport$acc_edge <= possible_nap_edge_acc) |
                       (sibreport$type != "sib" & sibreport$duration >= 1))
  # for qc purposes:
  dsummary[di,fi] = length(longboutsi)
  ds_names[fi] = "sibreport_n_items"
  fi = fi + 1
  if (length(longboutsi) > 0) {
    sibreport = sibreport[longboutsi,]
    srep_tmp = sibreport[which(sibreport$start > min(time) &
                                 sibreport$end < max(time)),]
    
    # account for possibility that some of these categories do not exist
    #	identify overlapping and non-overlapping, (nap-sib, non-wear-sib, sib, nap, nonwear)
    #	calculate for all five categories number, total duration, mean duration
    # for qc purposes:
    dsummary[di,fi] = nrow(srep_tmp)
    ds_names[fi] = "sibreport_n_items_day"
    fi = fi + 1
    
    if (nrow(srep_tmp) > 0) {
      sibs = which(srep_tmp$type == "sib")
      srep_tmp$SIBoverlapNonwear = 0
      srep_tmp$SIBoverlapNap = 0
      srep_tmp$NonwearOverlapSIB = 0
      srep_tmp$NapOverlapSIB = 0
      srep_tmp$start = as.POSIXct(srep_tmp$start, tz = tz)
      srep_tmp$end = as.POSIXct(srep_tmp$end, tz = tz)
      # # for qc purposes:
      # dsummary[di,fi] = nrow(srep_tmp)
      # ds_names[fi] = "n_sibs_sibreport"
      # fi = fi + 1
      if (length(sibs) > 0) {
        classes = unique(srep_tmp$type)
        selfreport = which(srep_tmp$type == "nonwear" | srep_tmp$type == "nap")
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
                }
              }
              
            }
          }
        }
      }
      sibs_indices = which(srep_tmp$type == "sib")
      nap_indices = which(srep_tmp$type == "nap")
      nonwear_indices = which(srep_tmp$type == "nonwear")
      SIBoverlapNap_indices = which(srep_tmp$SIBoverlapNap != 0)
      SIBoverlapNonwear_indices = which(srep_tmp$SIBoverlapNonwear != 0)
      NapOverlapSIB_indices = which(srep_tmp$NapOverlapSIB != 0)
      NonwearOverlapSIB_indices = which(srep_tmp$NonwearOverlapSIB != 0)
      dsummary[di,fi:(fi + 6)] = c(length(sibs_indices),
                                   length(nap_indices),
                                   length(nonwear_indices),
                                   length(SIBoverlapNap_indices),
                                   length(SIBoverlapNonwear_indices),
                                   length(NapOverlapSIB_indices),
                                   length(NonwearOverlapSIB_indices))
      ds_names[fi:(fi + 6)] = c("nbouts_day_sib", "nbouts_day_srnap", "nbouts_day_srnonw",
                                "noverl_sib_srnap", "noverl_sib_srnonw",
                                "noverl_srnap_sib", "noverl_srnonw_sib")
      fi = fi + 7
      # mean and total duration in sib per day
      if (length(sibs_indices) > 0) {
        dsummary[di,fi:(fi + 1)] = c(mean(srep_tmp$duration[sibs_indices]),
                                     sum(srep_tmp$duration[sibs_indices]))
      } else {
        dsummary[di,fi:(fi + 1)] = c(0, 0)
      }
      ds_names[fi:(fi + 1)] = c("frag_mean_dur_sib_day", "dur_day_sib_min")
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
      
      ds_names[fi:(fi + 1)] = c("frag_mean_dur_srnap_day", "dur_day_srnap_min")
      fi = fi + 2
      # mean and total duration in self-reported nonwear per day
      if (length(nonwear_indices) > 0) {
        dsummary[di,fi:(fi + 1)] = c(mean(srep_tmp$duration[nonwear_indices]),
                                     sum(srep_tmp$duration[nonwear_indices]))
      } else {
        dsummary[di,fi:(fi + 1)] = c(0, 0)
      }
      ds_names[fi:(fi + 1)] = c("frag_mean_dur_srnonw_day", "dur_day_srnonw_min")
      fi = fi + 2
      
      calcOverlapPercentage = function(overlap, duration) {
        return(sum(overlap * duration) / sum(duration))
      }
      
      # Overlap sib with srnap
      if (length(SIBoverlapNap_indices) > 0) {
        overlap_perc = calcOverlapPercentage(overlap = srep_tmp$SIBoverlapNap[SIBoverlapNap_indices],
                                             duration = srep_tmp$duration[SIBoverlapNap_indices])
        dsummary[di,fi:(fi + 2)] = c(mean(srep_tmp$duration[SIBoverlapNap_indices]),
                                     sum(srep_tmp$duration[SIBoverlapNap_indices]),
                                     overlap_perc)
      } else {
        dsummary[di,fi:(fi + 2)] = c(0, 0, 0)
      }
      ds_names[fi:(fi + 2)] = c("mdur_sib_overl_srnap", "tdur_sib_overl_srnap", "perc_sib_overl_srnap")
      fi = fi + 3
      # Overlap srnap with sib
      if (length(SIBoverlapNap_indices) > 0) {
        overlap_perc = calcOverlapPercentage(overlap = srep_tmp$NapOverlapSIB[NapOverlapSIB_indices],
                                             duration = srep_tmp$duration[NapOverlapSIB_indices])
        dsummary[di,fi:(fi + 2)] = c(mean(srep_tmp$duration[NapOverlapSIB_indices]),
                                     sum(srep_tmp$duration[NapOverlapSIB_indices]),
                                     overlap_perc)
      } else {
        dsummary[di,fi:(fi + 2)] = c(0, 0, 0)
      }
      ds_names[fi:(fi + 2)] = c("mdur_srnap_overl_sib", "tdur_srnap_overl_sib", "perc_srnap_overl_sib")
      fi = fi + 3
      # Overlap sib with srnonw
      if (length(SIBoverlapNonwear_indices) > 0) {
        overlap_perc = calcOverlapPercentage(overlap = srep_tmp$SIBoverlapNonwear[SIBoverlapNonwear_indices],
                              duration = srep_tmp$duration[SIBoverlapNonwear_indices])
        dsummary[di,fi:(fi + 2)] = c(mean(srep_tmp$duration[SIBoverlapNonwear_indices]),
                                     sum(srep_tmp$duration[SIBoverlapNonwear_indices]),
                                     overlap_perc)
      } else {
        dsummary[di,fi:(fi + 2)] = c(0, 0, 0)
      }
      ds_names[fi:(fi + 2)] = c("mdur_sib_overl_srnonw", "tdur_sib_overl_srnonw", "perc_sib_overl_srnonw")
      fi = fi + 3
      # Overlap srnonw with sib
      if (length(SIBoverlapNonwear_indices) > 0) {
        overlap_perc = calcOverlapPercentage(overlap = srep_tmp$NonwearOverlapSIB[NonwearOverlapSIB_indices],
                              duration = srep_tmp$duration[NonwearOverlapSIB_indices])
        dsummary[di,fi:(fi + 2)] = c(mean(srep_tmp$duration[NonwearOverlapSIB_indices]),
                                     sum(srep_tmp$duration[NonwearOverlapSIB_indices]),
                                     overlap_perc)
      } else {
        dsummary[di,fi:(fi + 2)] = c(0, 0, 0)
      }
      ds_names[fi:(fi + 2)] = c("mdur_srnonw_overl_sib", "tdur_srnonw_overl_sib", "perc_srnonw_overl_sib")
      fi = fi + 3
      rm(srep_tmp)
    } else {
      fi  =  fi + 26
    }
  }
  invisible(list(fi = fi, di = di, ds_names = ds_names, dsummary = dsummary))
}
