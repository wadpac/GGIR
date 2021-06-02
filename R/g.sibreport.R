g.sibreport = function(ts, ID, epochlength, logs_diaries=c()) {
  dayind = which(ts$diur == 0)
  sib_starts = which(diff(c(0,ts$sibdetection[dayind],0)) == 1)
  sib_ends = which(diff(c(ts$sibdetection[dayind],0)) == -1)
  Nsibs = length(sib_starts)
  sibreport = c()
  if (Nsibs > 0) {
    sibreport = data.frame(ID= rep(ID, Nsibs),
                           type = rep("sib", Nsibs),
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
  if (length(logs_diaries) > 0) {
    # extract self-reported nonwear and naps
    nonwearlog = logs_diaries$nonwearlog
    naplog = logs_diaries$naplog
    extract_logs = function(log, ID, logname) {
      logreport = c()
      if (length(log) > 0) {
        relevant_rows = which(log$ID == ID)
        if (length(relevant_rows) > 0) {
          log = log[relevant_rows,] # extract ID
          for (i in 1:nrow(log)) { # loop over lines (days)
            tmp = log[i,] # convert into timestamps
            date = tmp[,2]
            times = tmp[,3:ncol(tmp)]
            timestamps = sort(as.POSIXlt(paste0(date, " ", times)))
            Nevents = floor(length(timestamps) / 2)
            
            logreport_tmp = data.frame(ID= rep(ID, Nevents),
                                       type = rep(logname, Nevents),
                                       start = character(Nevents),
                                       end = character(Nevents))
            if (length(Nevents) > 0) {
              for (bi in 1:Nevents) {
                logreport_tmp$start[bi]  = as.character(timestamps[(bi*2)-1])
                logreport_tmp$end[bi] = as.character(timestamps[(bi*2)])
              }
              if (length(logreport) == 0) {
                logreport = logreport_tmp
              } else {
                logreport = rbind(logreport, logreport_tmp)
              }
            }
          }
        }
      }
      return(logreport)
    }
    naplogreport = extract_logs(naplog, ID, logname="nap")
    nonwearlogreport = extract_logs(nonwearlog, ID, logname="nonwear")  
    logreport = sibreport
    # append all together in one output data.frame
    if (length(logreport) > 0) {
      logreport = merge(logreport, naplogreport, by=c("ID", "type", "start", "end"), all=TRUE)
    } else {
      logreport = naplogreport
    }
    if (length(logreport) > 0) {
      logreport = merge(logreport, nonwearlogreport, by=c("ID", "type", "start", "end"), all=TRUE)
    } else {
      logreport = nonwearlogreport
    }
  } else {
    logreport = sibreport
  }
  return(logreport)
}