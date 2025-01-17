g.conv.actlog = function(qwindow, qwindow_dateformat="%d-%m-%Y", epochSize = 5) {
  # Function to read activity log and convert it into data.frame
  # that has for each ID and date a different qwindow vector
  # local functions:
  time2numeric = function(x) {
    x = unlist(x)
    x = x[grep(pattern = "impute|imputa|uncertain", x = names(x), invert = TRUE)]
    c2t = function(x2) {
      tmp = as.numeric(unlist(strsplit(as.character(x2),":")))
      if (length(tmp) == 2) hourinday = tmp[1] + (tmp[2]/60)
      if (length(tmp) == 3) hourinday = tmp[1] + (tmp[2]/60) + (tmp[3]/60/60)
      return(hourinday)
    }
    out = as.numeric(sapply(x,c2t))
    return(out)
  }
  extract_names = function(x) {
    tmp = names(unlist(x))
    tmp2 = gsub(pattern = "[.].*|_|-",replacement = "",x = tmp)
    return(tmp2)
  }
  #-------------------------
  # main function code
  
  # Read content of activity diary file
  actlog = data.table::fread(file = qwindow, data.table = FALSE, colClasses = "character")
  
  # check ID and date cols in actlog
  actlog = check_log(actlog, dateformat = qwindow_dateformat,
                     colid = 1, datecols = NULL, 
                     logPath = qwindow, logtype = "activity log")
  
  # find dates
  actlog_vec = unlist(actlog) # turn into vector
  actlog_vec = sapply(actlog_vec, function(x) !all(is.na(as.Date(as.character(x),format=qwindow_dateformat))))
  exampledates = unlist(actlog)[which(actlog_vec == TRUE)]
  Ndates = length(which(actlog_vec == TRUE))
  dim(actlog_vec) = c(nrow(actlog),ncol(actlog))
  # create new qwindow object to archive all extracted information
  qwindow = data.frame(ID = rep(0,Ndates), date =  rep("",Ndates))
  qwindow$qwindow_times = qwindow$qwindow_values = qwindow$qwindow_names = c()
  cnt = 1
  for (i in 1:nrow(actlog)) { # loop over rows in activity log = recordings
    datei = which(actlog_vec[i,] == TRUE)
    Ndays = length(datei)
    if (Ndays > 0) {
      qwindow$ID[cnt:(cnt+Ndays-1)]  = rep(actlog[i,1],Ndays)
      qwindow$date[cnt:(cnt+Ndays-1)]  = as.character(as.Date(as.character(actlog[i,datei]),format=qwindow_dateformat))
      # datei are the indices of the date columns
      # add number to be able to identify last class after last date
      datei = c(datei,max(which(actlog[i,] != "")) + 1)
      for (j in 1:(length(datei)-1)) { # loop over days within the recording
        # Note: qindow is a data.frame and the timestamps for each day will be stored
        # as a list object in a single cell of qwindow. The same applies ot the corresponding
        # labels.
        k = cnt + j - 1
        if ((datei[j + 1] - datei[j]) >= 2) {
          actlog_tmp = actlog[i,(datei[j] + 1):(datei[j + 1] - 1)]
          actlog_tmp = actlog_tmp[which(is.na(actlog_tmp) == FALSE & actlog_tmp != "")]
          qwindow$qwindow_times[k] = list(actlog_tmp) # list of times for that day
          # make sure that seconds are multiple of epochSize
          seconds = data.table::second(strptime(qwindow$qwindow_times[[k]], format = "%H:%M:%S"))
          to_next_epoch = which(seconds %% epochSize != 0)
          if (length(to_next_epoch) > 0) {
            timechar = qwindow$qwindow_times[[k]][to_next_epoch]
            time = strptime(timechar, format = "%H:%M:%S")
            seconds2sum = epochSize - seconds[to_next_epoch] %% epochSize
            time = time + seconds2sum
            timechar_new = as.character(time)
            time_new = unlist(lapply(timechar_new, FUN = function(x) strsplit(x, " ", fixed = T)[[1]][2]))
            qwindow$qwindow_times[[k]][to_next_epoch] = time_new
          }
          qwindow$qwindow_values[k] = list(time2numeric(qwindow$qwindow_times[k]))
          qwindow$qwindow_names[k] = list(extract_names(qwindow$qwindow_times[k]))
          unlisted_qv = unlist(qwindow$qwindow_values[k])
          unlisted_qt = unlist(qwindow$qwindow_times[k])
          unlisted_qn = unlist(qwindow$qwindow_names[k])
          
          if (length(which(is.na(unlisted_qv) == FALSE)) > 0) {
            if (min(unlisted_qv, na.rm = TRUE) > 0) {
              qwindow$qwindow_values[k] = list(c(0, unlisted_qv))
              qwindow$qwindow_times[k] = list(c("00:00", unlisted_qt))
              qwindow$qwindow_names[k] = list(c("daystart", unlisted_qn))
            }
            if (max(unlisted_qv, na.rm = TRUE) < 24) {
              qwindow$qwindow_values[k] = list(c(unlist(qwindow$qwindow_values[k]), 24))
              qwindow$qwindow_times[k] = list(c(unlist(qwindow$qwindow_times[k]), "24:00:00"))
              qwindow$qwindow_names[k] = list(c(unlist(qwindow$qwindow_names[k]),"dayend"))
            }
          }
        } else {
          qwindow$qwindow_values[k] = list("")
          qwindow$qwindow_names[k] = list("")
          qwindow$qwindow_times[k] = list("")
        }
      }
      cnt = cnt + Ndays
    }
  }
  
  # When testing the code it seemed sometimes not to recognise the date.
  # The following lines should hopefully help to catch any errors people may encounter.
  if (is.na(as.Date(qwindow$date[1], format = "%y-%m-%d")) == FALSE) {
    qwindow$date =  as.Date(qwindow$date, format = "%y-%m-%d")
  } else {
    qwindow$date =  as.Date(qwindow$date)
  }
  if (is.na(qwindow$date[1]) == TRUE | !is(qwindow$date[1], "Date")) {
    if (length(exampledates) > 0) {
      warning(paste0("\n Date not recognised in activity diary. We expect format ", 
                     qwindow_dateformat, " because that is what you specified in ",
                     "argument qwindow_dateformat, but we see ",
                     paste0(head(exampledates), collapse = " "), 
                     ". You need to update the qwindow_dateformat argument, and check",
                     " that dates are in a consistent format."))
    } else {
      warning("\n Date not recognised in activity diary")
    }
  }
  return(qwindow)
}
