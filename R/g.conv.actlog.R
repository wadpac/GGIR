g.conv.actlog = function(qwindow) {
  # Function to read activity log and convert it into data.frame
  # that has for each ID and date a different qwindow vector
  
  # TO DO: Enable this to also read in different Activity log formats.
  # TO DO: Add documentation in code below
  
  actlog = read.csv(file = qwindow)
  actlog = actlog[which(actlog[,1] != ""),]
  actlog_vec = unlist(actlog)
  actlog_vec = sapply(actlog_vec, function(x) !all(is.na(as.Date(as.character(x),format="%d-%m-%Y"))))
  Ndates = length(which(actlog_vec == TRUE))
  dim(actlog_vec) = c(nrow(actlog),ncol(actlog))
  qwindow = data.frame(ID = rep(0,Ndates), date =  rep("",Ndates))
  qwindow$qwindow_times = qwindow$qwindow_values = qwindow$qwindow_numes = c()
  cnt = 1
  time2numeric = function(x) {
    x = unlist(x)
    c2t = function(x2) {
      tmp = as.numeric(unlist(strsplit(as.character(x2),":")))
      hourinday = tmp[1] + (tmp[2]/60)
      return(hourinday)
    }
    out = as.numeric(sapply(x,c2t))
    return(out)
  }
  extract_names = function(x) {
    tmp = names(unlist(x))
    tmp2 = gsub(pattern = "[.].*",replacement = "",x = tmp)
    return(tmp2)
  }
  for (i in 1:nrow(actlog)) {
    datei = which(actlog_vec[i,] == TRUE)
    Ndays = length(datei)
    if (Ndays > 0) {
      qwindow$ID[cnt:(cnt+Ndays-1)]  = rep(actlog[i,1],Ndays)
      qwindow$date[cnt:(cnt+Ndays-1)]  = as.character(as.Date(as.character(actlog[i,datei]),format="%d-%m-%Y"))
      datei = c(datei,max(which(actlog[i,] != "")) + 1) # add number to be able to identify last class after last date
      for (j in 1:(length(datei)-1)) {
        k = cnt+j-1
        if ((datei[j+1] - datei[j]) >= 2) {
          qwindow$qwindow_times[k] = list(actlog[i,(datei[j]+1):(datei[j+1]-1)])
          qwindow$qwindow_values[k] = list(time2numeric(qwindow$qwindow_times[k]))
          qwindow$qwindow_names[k] = list(extract_names(qwindow$qwindow_times[k]))
          unlisted_qv = unlist(qwindow$qwindow_values[k])
          unlisted_qt = unlist(qwindow$qwindow_times[k])
          unlisted_qn = unlist(qwindow$qwindow_names[k])
          if (unlisted_qv[1] != 0) {
            qwindow$qwindow_values[k] = list(c(0, unlisted_qv))
            qwindow$qwindow_times[k] = list(c("00:00", unlisted_qt))
            qwindow$qwindow_names[k] = list(c("daystart", unlisted_qn))
          }
          if (unlisted_qv[length(unlisted_qv)] != 24) {
            qwindow$qwindow_values[k] = list(c(unlist(qwindow$qwindow_values[k]), 24))
            qwindow$qwindow_times[k] = list(c(unlist(qwindow$qwindow_times[k]), "24:00"))
            qwindow$qwindow_names[k] = list(c(unlist(qwindow$qwindow_names[k]),"dayend"))
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
  if (is.na(as.Date(qwindow$date[1],format="%y-%m-%d")) == FALSE) {
    qwindow$date =  as.Date(qwindow$date,format="%y-%m-%d")  
  } else {
    qwindow$date =  as.Date(qwindow$date)  
  }
  if (is.na(qwindow$date[1]) == TRUE | class(qwindow$date[1]) != "Date") {
    warning("\n Date not recognised in activity diary")
  }
  return(qwindow)
}