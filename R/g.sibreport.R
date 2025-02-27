g.sibreport = function(ts, ID, epochlength, logs_diaries=c(), desiredtz="") {
  dayind = which(ts$diur == 0)
  sib_starts = which(diff(c(0,ts$sibdetection[dayind],0)) == 1)
  sib_ends = which(diff(c(ts$sibdetection[dayind],0)) == -1)
  Nsibs = length(sib_starts)
  sibreport = c()
  if (Nsibs > 0) {
    sibreport = data.frame(ID = rep(ID, Nsibs),
                           type = rep("sib", Nsibs),
                           start = character(Nsibs),
                           end = character(Nsibs),
                           duration = numeric(Nsibs),
                           mean_acc_1min_before = numeric(Nsibs),
                           mean_acc_1min_after = numeric(Nsibs), stringsAsFactors = FALSE)
    for (sibi in 1:Nsibs) {
      sibreport$start[sibi]  = format(ts$time[dayind[sib_starts[sibi]]])
      sibreport$end[sibi] = format(ts$time[dayind[sib_ends[sibi]]])
      if (is.ISO8601(sibreport$start[sibi])) {
        sibreport$start[sibi] = format(iso8601chartime2POSIX(sibreport$start[sibi], tz = desiredtz))
        sibreport$end[sibi] = format(iso8601chartime2POSIX(sibreport$end[sibi], tz = desiredtz))
      }
      sibreport$duration[sibi] = ((sib_ends[sibi] - sib_starts[sibi]) + 1) / (60/epochlength)
      boutind = sib_starts[sibi]:sib_ends[sibi]
      minute_before = (sib_starts[sibi] - (60/epochlength)):(sib_starts[sibi] - 1)
      minute_after = (sib_ends[sibi] + 1):(sib_ends[sibi] + (60/epochlength))
      if (min(minute_before) > 1) {
        sibreport$mean_acc_1min_before[sibi]  = round(mean(ts$ACC[dayind[minute_before]]), digits = 3)
      }
      if (max(minute_after) < nrow(ts)) {
        sibreport$mean_acc_1min_after[sibi]  = round(mean(ts$ACC[dayind[minute_after]]), digits = 3)
      }
    }
  }
  if (length(logs_diaries) > 0) {
    # extract self-reported nonwear and naps
    nonwearlog = logs_diaries$nonwearlog
    naplog = logs_diaries$naplog
    sleeplog = logs_diaries$sleeplog
    bedlog = logs_diaries$bedlog
    imputecodelog = logs_diaries$imputecodelog
    dateformat = logs_diaries$dateformat

    firstDate = as.Date(ts$time[1], tz = desiredtz)
    extract_logs = function(log, ID, logname, firstDate = NULL) {
      logreport = c()
      if (length(log) > 0) {
        relevant_rows = which(log$ID == ID)
        
        if (length(relevant_rows) > 0) {
          log = log[relevant_rows,] # extract ID
          if (!is.null(firstDate)) {
            # Add date if missing and remove unneeded columns
            log$date = firstDate + as.numeric(log$night) - 1
            log = log[,c("ID", "date", grep(pattern = "onset|wake|bed", x = names(log), value = TRUE))]
          }
          for (i in 1:nrow(log)) { # loop over lines (days)
            
            tmp = log[i,] # convert into timestamps
            # only attempt if there are at least 2 timestamps to process
            if (ncol(tmp) <= 2) next
            nonempty = which(tmp[3:ncol(tmp)] != "" & tmp[3:ncol(tmp)] != "NA")
            if (length(nonempty) > 1) {
              date = as.Date(tmp[1,2], format = dateformat, tz = desiredtz)
              times = as.character(unlist(tmp[1,3:ncol(tmp)]))
              times = grep(pattern = "NA", value = TRUE, invert = TRUE, x = times)
              times = gsub(pattern = " ", replacement = "", x = times)
              times = times[which(times %in% c("", "NA") == FALSE)]
              # ignore entries without start and/or end time
              t_to_remove = c()
              for (ji in 1:floor(length(times)/2)) {
                check = ((ji * 2) - 1):(ji * 2)
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
                timestamps = as.POSIXlt(paste0(date, " ", times), tz = desiredtz)
                hour = as.numeric(format(timestamps, "%H"))
                if (!is.null(firstDate)) {
                  # sleeplog start and/or ending after midnight
                  AM = which(hour <= 12)
                  if (hour[2] > 12 & hour[2] <= 18) { # daysleeper
                    AM = c(AM, 2)
                  }
                  if (length(AM) > 0) {
                    timestamps[AM] = as.POSIXlt(paste0(date + 1, " ", times[AM]), tz = desiredtz)
                  }
                } else {
                  # self-reported nap or nonwear startign before midnight and ending after midnight
                  if (hour[1] >= 12 & hour[2] < 12) {
                    timestamps[2] = as.POSIXlt(paste0(date, " ", times[2]), tz = desiredtz)
                  }
                }
                timestamps = sort(timestamps)
                logreport_tmp = data.frame(ID = rep(ID, Nevents),
                                           type = rep(logname, Nevents),
                                           start = rep("", Nevents),
                                           end = rep("", Nevents),
                                           duration = rep(0, Nevents), stringsAsFactors = FALSE)
                for (bi in 1:Nevents) {
                  tt1 = as.POSIXct(timestamps[(bi * 2) - 1], tz = desiredtz)
                  tt2 = as.POSIXct(timestamps[(bi * 2)], tz = desiredtz)
                  logreport_tmp$start[bi]  = format(tt1)
                  logreport_tmp$end[bi] = format(tt2)
                  if (length(unlist(strsplit(logreport_tmp$start[bi], " "))) == 1) {
                    logreport_tmp$start[bi] = paste0(logreport_tmp$start[bi], " 00:00:00")
                  }
                  if (length(unlist(strsplit(logreport_tmp$end[bi], " "))) == 1) {
                    logreport_tmp$end[bi] = paste0(logreport_tmp$end[bi], " 00:00:00")
                  }
                  logreport_tmp$duration[bi] = abs(as.numeric(difftime(time1 = tt1, time2 = tt2, units = "mins")))
                }
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
    naplogreport = extract_logs(naplog, ID, logname = "nap")
    nonwearlogreport = extract_logs(nonwearlog, ID, logname = "nonwear")
    sleeplogreport = extract_logs(sleeplog, ID, logname = "sleeplog", firstDate = firstDate)
    bedlogreport = extract_logs(bedlog, ID, logname = "bedlog", firstDate = firstDate)
    # add imputecodelog
    if (length(imputecodelog) > 0) {
      addDate = function(x, tz = desiredtz) {
        start_time = as.POSIXct(x$start, tz = tz)
        x$date = as.Date(start_time, tz = tz)
        start_hour = as.numeric(format(start_time, "%H"))
        start_am = which(start_hour < 12)
        if (length(start_am) > 0) {
          x$date[start_am] = x$date[start_am] - 1
        }
        return(x)
      }
      if (length(sleeplogreport) > 0) sleeplogreport = addDate(sleeplogreport)
      if (length(bedlogreport) > 0) bedlogreport = addDate(bedlogreport)
      
      imputecodelog_tmp = imputecodelog[which(imputecodelog$ID == ID), ]
      if (length(sleeplogreport) > 0) {
        sleeplogreport = merge(sleeplogreport, imputecodelog_tmp, by = c("ID", "date"))
        sleeplogreport = sleeplogreport[, which(colnames(sleeplogreport) != "date")]
      }
      if (length(bedlogreport) > 0) {
        bedlogreport = merge(bedlogreport, imputecodelog_tmp, by = c("ID", "date"))
        bedlogreport = bedlogreport[, which(colnames(bedlogreport) != "date")]
      }
    }
    logreport = sibreport
    # append all together in one output data.frame
    if (length(logreport) > 0 & length(naplogreport) > 0) {
      logreport = merge(logreport, naplogreport, by = c("ID", "type", "start", "end", "duration"), all = TRUE)
    } else if (length(logreport) == 0 & length(naplogreport) > 0) {
      logreport = naplogreport
    }
    if (length(logreport) > 0 & length(nonwearlogreport) > 0) {
      logreport = merge(logreport, nonwearlogreport, by = c("ID", "type", "start", "end", "duration"), all = TRUE)
    } else if (length(logreport) == 0 & length(nonwearlogreport) > 0) {
      logreport = nonwearlogreport
    }
    if (length(logreport) > 0 & length(sleeplogreport) > 0) {
      logreport = merge(logreport, sleeplogreport, by = c("ID", "type", "start", "end", "duration"), all = TRUE)
    } else if (length(logreport) == 0 & length(sleeplogreport) > 0) {
      logreport = sleeplogreport
    }
    if (length(logreport) > 0 & length(bedlogreport) > 0) {
      if ("imputecode" %in% colnames(logreport) && "imputecode" %in% colnames(bedlogreport)) {
        include_imputecode = "imputecode"
      } else {
        include_imputecode = NULL
      }
      logreport = merge(logreport, bedlogreport,
                        by = c("ID", "type", "start", "end", "duration", include_imputecode), all = TRUE)
    } else if (length(logreport) == 0 & length(bedlogreport) > 0) {
      logreport = bedlogreport
    }
  } else {
    logreport = sibreport
  }
  # add midnight timetimes which got lost
  convert2POSIX = function(x) {
    if (length(unlist(strsplit(x, " "))) == 1) {
      x = paste0(x, " 00:00:00")
    }
    return(x)
  }
  logreport$start = as.POSIXct(unlist(lapply(X = logreport$start, FUN = convert2POSIX)), tz = desiredtz)
  logreport$end = as.POSIXct(unlist(lapply(X = logreport$end, FUN = convert2POSIX)), tz = desiredtz)
  return(logreport)
}
