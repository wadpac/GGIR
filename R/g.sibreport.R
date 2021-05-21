g.sibreport = function(ts, ID, epochlength) {
  dayind = which(ts$diur == 0)
  sib_starts = which(diff(c(0,ts$sibdetection[dayind],0)) == 1)
  sib_ends = which(diff(c(ts$sibdetection[dayind],0)) == -1)
  Nsibs = length(sib_starts)
  if (Nsibs > 0) {
    sibreport = data.frame(ID= rep(ID, Nsibs),
                           start = character(Nsibs),
                           end = character(Nsibs),
                           duration = numeric(Nsibs),
                           mean_acc = numeric(Nsibs),
                           sd_acc = numeric(Nsibs),
                           sd_ang = numeric(Nsibs),
                           mean_acc_1min_before = numeric(Nsibs),
                           mean_acc_1min_after = numeric(Nsibs))
    for (sibi in 1:Nsibs) {
      sibreport$start[sibi]  = as.character(ts$time[dayind][sib_starts[sibi]])
      sibreport$end[sibi] = as.character(ts$time[dayind][sib_ends[sibi]])
      sibreport$duration[sibi] = ((sib_ends[sibi] - sib_starts[sibi]) + 1) / (60/epochlength)
      boutind = sib_starts[sibi]:sib_ends[sibi]
      minute_before = (sibi-(60/epochlength)):(sibi-1)
      minute_after = (sibi+1):(sibi+(60/epochlength))
      sibreport$mean_acc[sibi]  = round(mean(ts$ACC[dayind][boutind]), digits=3)
      sibreport$sd_acc[sibi]  = round(sd(ts$ACC[dayind][boutind]), digits=3)
      sibreport$sd_ang[sibi]  = round(sd(ts$angle[dayind][boutind]), digits=3)
      if (min(minute_before) > 1) {
        sibreport$mean_acc_1min_before[sibi]  = round(mean(ts$ACC[minute_before]), digits=3)
      }
      if (max(minute_after) < nrow(ts)) {
        sibreport$mean_acc_1min_after[sibi]  = round(mean(ts$ACC[minute_after]), digits=3)
      }
    }
  }
  return(sibreport)
}