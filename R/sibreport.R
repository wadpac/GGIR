sibreport = function(ts, ID, ws3new) {
  dayind = which(ts$diur == 0)
  sib_starts = which(diff(c(0,ts$sibdetection[dayind],0)) == 1)
  sib_ends = which(diff(c(ts$sibdetection[dayind],0)) == -1)
  Nsibs = length(sib_starts)
  if (Nsibs > 0) {
    sibreport = data.frame(ID= rep(ID, Nsibs),
                           start = character(Nsibs),
                           end = character(Nsibs),
                           duration = numeric(Nsibs),
                           delta_acc = numeric(Nsibs),
                           delta_ang = numeric(Nsibs))
    for (sibi in 1:Nsibs) {
      sibreport$start[sibi]  = as.character(ts$time[dayind][sib_starts[sibi]])
      sibreport$end[sibi] = as.character(ts$time[dayind][sib_ends[sibi]])
      sibreport$duration[sibi] = ((sib_ends[sibi] - sib_starts[sibi]) + 1) / (60/ws3new)
      boutind = sib_starts[sibi]:sib_ends[sibi]
      sibreport$delta_acc[sibi]  = round(mean(abs(diff(ts$ACC[dayind][boutind]))), digits=3)
      sibreport$delta_ang[sibi]  = round(mean(abs(diff(ts$angle[dayind][boutind]))), digits=3)
    }
  }
  return(sibreport)
}