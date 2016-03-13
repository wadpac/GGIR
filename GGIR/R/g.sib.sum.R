g.sib.sum = function(SLE,M,ignorenonwear=FALSE) {
  A = as.data.frame(SLE$output)
  invalid = A$invalid
  
  if (ignorenonwear == TRUE) {
    if (length(which(A$invalid==1)) > 0) {
      A[which(A$invalid==1),] = 0
      # A = A[-c(which(A$invalid==1)),]
    }
  }
  # invalid = A$invalid
  
  time = as.POSIXlt(A$time,tz="UTC") # (added on 7/9/2015) needed when analysing data in different timezone?
  # this timezone correction is my current fix to dealing with the problem of as.POSIXlt getting confused when trying t
  # interpret a time that happened in a timezone, which does not exist in another timezone
  
  night = A$night
  sleep = as.data.frame(as.matrix(A[,(which(colnames(A)=="night")+1):ncol(A)]))
  colnames(sleep) = colnames(A)[(which(colnames(A)=="night")+1):ncol(A)]
  ws3 = M$windowsizes[1]
  ws2 = M$windowsizes[2]
  sib.cla.sum = as.data.frame(matrix(0,0,9))
  cnt = 1
  un = unique(night) #unique nights
  if (length(which(un == 0 | is.na(un) == TRUE)) > 0) {
    un = un[-c(which(un == 0 | is.na(un) == TRUE))]
  }
  for (i in 1:length(un)) { #nights
    qqq1 = which(night == i)[1]
    qqq2 = which(night == i)[length(which(night == i))]
    if (length(qqq1) == 1 & length(qqq2) == 1) {
      time.t = time[qqq1:qqq2]
      sleep.t = as.data.frame(sleep[qqq1:qqq2,])
      colnames(sleep.t) = colnames(sleep)
      invalid.t = invalid[qqq1:qqq2]
      for (j in 1:ncol(sleep.t)) { #sleep definitions
        nsleepperiods = length(which(diff(sleep.t[,j]) == 1))     
        if (nsleepperiods > 0) {
          start_sp = which(diff(sleep.t[,j]) == 1)
          end_sp = which(diff(sleep.t[,j]) == -1)
          if(length(end_sp) == 0) end_sp = nrow(sleep.t) #if sleep period ends the next 'nightday'
          if (start_sp[1] > end_sp[1]) { #if period starts with sleep
            start_sp = c(1,start_sp)
          }
          if (start_sp[length(start_sp)] > end_sp[length(end_sp)]) { #if period ends with sleep
            end_sp = c(end_sp,nrow(sleep.t))
          }
          nsleepperiods = length(start_sp)
        }
        
        if (nsleepperiods == 0) {
          sleep_dur = 0
          nint = 0
          lex = 1
          colnames(sib.cla.sum)[lex:(lex+3)] = c("night","definition","start.time.day","nsib.periods")
          sib.cla.sum[cnt,lex] = i #night
          sib.cla.sum[cnt,(lex+1)] = colnames(sleep.t)[j] #definition
          sib.cla.sum[cnt,(lex+2)] = as.character(time.t[1])
          sib.cla.sum[cnt,(lex+3)] = 0 #number of sleep periods
          lex = lex + 4
          colnames(sib.cla.sum)[lex:(lex+1)] = c("tot.sib.dur.hrs","fraction.night.invalid")
          sib.cla.sum[cnt,lex] = 0 #total sleep duration
          sib.cla.sum[cnt,(lex+1)] = length(which(invalid.t != 0)) / length(invalid.t) #fraction of data non-wear
          lex = lex + 2
          # sleep period specific characteristics  
          colnames(sib.cla.sum)[lex:(lex+2)] = c("sib.period","sib.onset.time","sib.end.time")
          sib.cla.sum[cnt,lex] = 0
          sib.cla.sum[cnt,(lex+1)] = ""
          sib.cla.sum[cnt,(lex+2)] = ""
          cnt = cnt + 1
        } else {
          for (spi in 1:nsleepperiods) {
            sleep_sp = sleep.t[start_sp[spi]:end_sp[spi],j]
            time_sp = time.t[start_sp[spi]:end_sp[spi]]
            sleep_dur = (round((length(which(sleep_sp==1))/(60/ws3)) * 100)) / 100
            # general characteristics of the time window
            lex = 1
            colnames(sib.cla.sum)[lex:(lex+3)] = c("night","definition","start.time.day","nsib.periods")
            sib.cla.sum[cnt,lex] = i #night
            sib.cla.sum[cnt,(lex+1)] = colnames(sleep.t)[j] #definition
            sib.cla.sum[cnt,(lex+2)] = as.character(time.t[1])
            sib.cla.sum[cnt,(lex+3)] = nsleepperiods #number of sleep periods
            lex = lex + 4
            colnames(sib.cla.sum)[lex:(lex+1)] = c("tot.sib.dur.hrs","fraction.night.invalid")
            sib.cla.sum[cnt,lex] = sleep_dur/60 #total sleep duration
            sib.cla.sum[cnt,(lex+1)] = length(which(invalid.t != 0)) / length(invalid.t) #fraction of data non-wear
            lex = lex + 2
            # sleep period specific characteristics  
            colnames(sib.cla.sum)[lex:(lex+2)] = c("sib.period","sib.onset.time","sib.end.time")
            sib.cla.sum[cnt,lex] = spi
            sib.cla.sum[cnt,(lex+1)] = as.character(time_sp[which(sleep_sp == 1)[1]])
            sib.cla.sum[cnt,(lex+2)] = as.character(time_sp[length(sleep_sp)])
            cnt = cnt + 1
          }
        }
      }
    }
  }
  #   invisible(list(sib.cla.sum=sib.cla.sum))
  sib.cla.sum=sib.cla.sum
}