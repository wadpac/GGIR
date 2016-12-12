g.loadlog = function(loglocation=c(),coln1=c(),colid=c(),nnights=c(),sleeplogidnum=TRUE) {
  cnt_time_notrecognise = 0
  #===============================
  # Load sleep log data...
  S = read.csv(loglocation)
  sleeplog = matrix(0,(nrow(S)*nnights),3)
  sleeplog_times = matrix(" ",(nrow(S)*nnights),2)
  
  cnt = 1
  for (i in 1:nnights) { #loop through nights
    SL = as.character(S[,coln1+((i-1)*2)])
    WK = as.character(S[,coln1+((i-1)*2)+1])
    # Check whether any correction need to be made to the sleep log:
    for (j in 1:length(SL)) { #loop through participant
      idtmp = S[j,colid]
      if (is.na(WK[j]) == FALSE & is.na(SL[j]) == FALSE & WK[j] != "" & SL[j] != "") {
        SLN = as.numeric(unlist(strsplit(SL[j],":")))
        WKN = as.numeric(unlist(strsplit(WK[j],":")))
        if (length(SLN) == 2) SLN = c(SLN,0) #add seconds when they are not stored
        if (length(WKN) == 2) WKN = c(WKN,0) #add seconds when they are not stored
        SL[j] = paste(SLN[1],":",SLN[2],":",SLN[3],sep="")
        WK[j] = paste(WKN[1],":",WKN[2],":",WKN[3],sep="")
        SLN2 = SLN[1] * 3600 + SLN[2] * 60 + SLN[3]
        WKN2 = WKN[1] * 3600 + WKN[2] * 60  + WKN[3]
        if (WKN2 > SLN2) { #e.g. 01:00 - 07:00
          dur = WKN2 - SLN2
        } else if (WKN2 < SLN2) { #e.g. 22:00 - 07:00
          dur = ((24*3600) - SLN2) + WKN2
        }
        dur = dur / 3600
      } else {
        cnt_time_notrecognise = cnt_time_notrecognise + 1
        dur = 0
        is.na(dur) =  TRUE
      }
      if (sleeplogidnum == TRUE) {
      sleeplog[cnt,1] = round(S[j,colid])
      } else {
        sleeplog[cnt,1] = as.character(S[j,colid])
      }
      sleeplog[cnt,2] = i
      sleeplog[cnt,3] = dur
      sleeplog_times[cnt,1] = SL[j]
      sleeplog_times[cnt,2] = WK[j]
      cnt = cnt + 1
    }
  }
  # delete id-numbers that are unrecognisable
  if (length(which(sleeplog[,1] == 0))) {
    print(paste("N deleted because unrecognisable ID number: ",length(which(sleeplog[,1] == 0))),sep="")
    sleeplog = sleeplog[-c(which(sleeplog[,1] == 0)),]
    sleeplog_times = sleeplog_times[-c(which(sleeplog[,1] == 0)),]
  }
  sleeplog= as.data.frame(sleeplog)
  names(sleeplog) = c("id","night","duration")
  sleeplog$sleeponset = sleeplog_times[,1]
  sleeplog$sleepwake = sleeplog_times[,2]
  invisible(list(sleeplog=sleeplog))
}
