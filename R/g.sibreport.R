g.sibreport = function(ts, ID, epochlength, logs_diaries=c(), desiredtz="") {
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
                           mean_acc_1min_before = numeric(Nsibs),
                           mean_acc_1min_after = numeric(Nsibs), stringsAsFactors = FALSE)
    for (sibi in 1:Nsibs) {
      sibreport$start[sibi]  = as.character(ts$time[dayind[sib_starts[sibi]]])
      sibreport$end[sibi] = as.character(ts$time[dayind[sib_ends[sibi]]])
      if (is.ISO8601(sibreport$start[sibi])) {
        sibreport$start[sibi]  = as.character(iso8601chartime2POSIX(sibreport$start[sibi],tz=desiredtz))
        sibreport$end[sibi]  = as.character(iso8601chartime2POSIX(sibreport$end[sibi],tz=desiredtz))
      }
      sibreport$duration[sibi] = ((sib_ends[sibi] - sib_starts[sibi]) + 1) / (60/epochlength)
      boutind = sib_starts[sibi]:sib_ends[sibi]
      minute_before = (sib_starts[sibi]-(60/epochlength)):(sib_starts[sibi]-1)
      minute_after = (sib_ends[sibi]+1):(sib_ends[sibi]+(60/epochlength))
      if (min(minute_before) > 1) {
        sibreport$mean_acc_1min_before[sibi]  = round(mean(ts$ACC[dayind[minute_before]]), digits=3)
      }
      if (max(minute_after) < nrow(ts)) {
        sibreport$mean_acc_1min_after[sibi]  = round(mean(ts$ACC[dayind[minute_after]]), digits=3)
      }
    }
  }
  if (length(logs_diaries) > 0) {
    # extract self-reported nonwear and naps
    nonwearlog = logs_diaries$nonwearlog
    naplog = logs_diaries$naplog
    dateformat = logs_diaries$dateformat
    extract_logs = function(log, ID, logname) {
      logreport = c()
      if (length(log) > 0) {
        relevant_rows = which(log$ID == ID)
        if (length(relevant_rows) > 0) {
          log = log[relevant_rows,] # extract ID
          for (i in 1:nrow(log)) { # loop over lines (days)
            tmp = log[i,] # convert into timestamps
            # only attempt if there are at least 2 timestamps to process
            nonempty = which(tmp[3:ncol(tmp)] != "")
            if (length(nonempty) > 1) {
              date = as.Date(as.character(tmp[1,2]), format=dateformat)
              times = as.character(unlist(tmp[1,3:ncol(tmp)]))
              # ignore entries without start and/or end time
              t_to_remove = c()
              for (ji in 1:floor(length(times)/2)) {
                check = ((ji*2)-1):(ji*2)
                if (length(which(times[check] == "")) > 0) {
                  t_to_remove = c(t_to_remove, check)
                }
              }
              if (length(t_to_remove) > 0) {
                times = times[-t_to_remove]
              }
              # put remaining timestamps in logreport
              if (length(times) > 1) {
                Nevents = floor(length(times) / 2)
                timestamps = sort(as.POSIXlt(paste0(date, " ", times), tz = desiredtz))
                logreport_tmp = data.frame(ID= rep(ID, Nevents),
                                           type = rep(logname, Nevents),
                                           start = rep("", Nevents),
                                           end = rep("", Nevents), stringsAsFactors = FALSE)
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
      }
      return(logreport)
    }
    naplogreport = extract_logs(naplog, ID, logname="nap")
    nonwearlogreport = extract_logs(nonwearlog, ID, logname="nonwear")  
    logreport = sibreport
    # append all together in one output data.frame
    if (length(logreport) > 0 & length(naplogreport) > 0) {
      logreport = merge(logreport, naplogreport, by=c("ID", "type", "start", "end"), all=TRUE)
    } else if (length(logreport) == 0 & length(naplogreport) > 0) {
      logreport = naplogreport
    }
    if (length(logreport) > 0 & length(nonwearlogreport) > 0) {
      logreport = merge(logreport, nonwearlogreport, by=c("ID", "type", "start", "end"), all=TRUE)
    } else if (length(logreport) == 0 & length(nonwearlogreport) > 0) {
      logreport = nonwearlogreport
    }
  } else {
    logreport = sibreport
  }
  return(logreport)
}