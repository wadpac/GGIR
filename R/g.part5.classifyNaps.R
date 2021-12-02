g.part5.classifyNaps = function(sibreport=c(), desiredtz="", 
                              possible_nap_window = c(9, 18), possible_nap_dur = c(15, 240),
                              nap_model = "hip3yr",
                              HASIB.algo = "vanHees2015") {
  
  # possible_nap_window: window of clock hours during which naps are assumed to take place
  # possible_nap_dur: mininum and maximum nap duration in minutes
  if (length(sibreport) > 0 & nap_model %in% "hip3yr" & HASIB.algo %in% "vanHees2015") {
    sibs = which(sibreport$type == "sib")
    if (length(sibs) > 1) {
      sibreport = sibreport[sibs,]
      sibreport$start = as.POSIXlt(sibreport$start, tz = desiredtz)
      sibreport$end = as.POSIXlt(sibreport$end, tz = desiredtz)
      # remove all short gaps between SIBs
      sibreport$gap2next = NA
      sibreport$gap2next[sibs[1:(length(sibs) - 1)]] = as.numeric(sibreport$start[sibs[2:length(sibs)]]) - as.numeric(sibreport$end[sibs[1:(length(sibs) - 1)]])
      iter = 1
      while (iter < nrow(sibreport)) {
        if (sibreport$gap2next[iter] < 300 & sibreport$type[iter] == "sib" & sibreport$type[iter + 1] == "sib") {
          sibreport$end[iter] = sibreport$end[iter + 1]
          sibreport$mean_acc_1min_after[iter] = sibreport$mean_acc_1min_after[iter + 1]
          sibreport = sibreport[-(iter + 1),]
        }
        if (iter > nrow(sibreport) - 1) {
          break()
        }
        iter = iter + 1
      }
      sibreport$duration = difftime(sibreport$end, sibreport$start, units = "mins")
      # Only include sibs that ocure within a specific time window and have a certain minimum duration
      startH = as.numeric(format(sibreport$start, "%H"))
      endH = as.numeric(format(sibreport$end, "%H"))
      sibreport = sibreport[which(startH > possible_nap_window[1] & startH < possible_nap_window[2] &
                                    endH > possible_nap_window[1] & endH < possible_nap_window[2] &
                        sibreport$duration >= possible_nap_dur[1] & sibreport$duration < possible_nap_dur[2]),] 
      sibreport$date = as.Date(sibreport$start)
      sibreport$acc_edge = pmax(sibreport$mean_acc_1min_before, sibreport$mean_acc_1min_after)
      # Calculate probability
      sibreport$probability_nap = rep(0, nrow(sibreport))
      if (nap_model == "hip3yr" & HASIB.algo == "vanHees2015") {
        sibreport$probability_nonwear = 1/(1 +  exp(-(-3.56347023 + (0.04303106 * sibreport$acc_edge))))
        
        sibreport$probability_nap[which(sibreport$probability_nonwear <  0.0688203077670859)] = 1
      } else {
        sibreport = c()
      }
    }
  } else {
    sibreport = c()
  }
  return(sibreport)
}