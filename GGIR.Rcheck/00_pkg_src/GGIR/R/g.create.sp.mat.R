g.create.sp.mat = function(nsp,spo,sleepdet.t,daysleep=FALSE) {
  
  if (daysleep == FALSE) {
    th2 = 12
  } else {
    th2 = 18
  }  
  #genspm is a function to convert data into sleep period matrix part of g.part4.R
  weekdays = c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday")
  for (sp in 1:nsp) {
    spo[sp,1] = sp
    tmp7 = as.character(sleepdet.t$sib.onset.time[which(sleepdet.t$sib.period == sp)])
    tmp7w = as.character(sleepdet.t$sib.onset.time[which(sleepdet.t$sib.period == sp)])
    if (length(tmp7) > 0) {
      if (tmp7 != "") {
        tmp8 = unlist(strsplit(tmp7," "))
        tmp8w = unlist(strsplit(tmp7w," "))
        tmp9 = unlist(strsplit(tmp8[2],":"))
        tmp9w = unlist(strsplit(tmp8w[2],":"))
        if (length(tmp8) == 1) {
          tmp10 = 0
        } else {
          tmp10 = as.numeric(tmp9[1]) + (as.numeric(tmp9[2])/60) + (as.numeric(tmp9[3])/3600)
        }
        if (length(tmp8w) == 1) {
          tmp10w = 0
        } else {
          tmp10w = as.numeric(tmp9w[1]) + (as.numeric(tmp9w[2])/60) + (as.numeric(tmp9w[3])/3600)
        }
        # get weekday
        if (sp == 1) { #first sleep period
          soso = unclass(as.POSIXlt(tmp7))
          wday = soso$wday #day of the week 0-6 and 0 is Sunday
          #remember day of the week and date
          wday_safe = wday + 1 #change to 1-7 number
          calendardate_safe = paste(soso$mday,"/",(soso$mon+1),"/",(soso$year+1900),sep="")
        }
        if (sp == nsp)   { #waking up of last sleep period
          soso = unclass(as.POSIXlt(tmp7w))
          wday = soso$wday #day of the week 0-6 and 0 is Sunday
          wday = wday + 1 #change to 1-7 number
          # check whether both beginning and end of the night are on the same day
          if (wday == (wday_safe+1)) { #normal night starting on one day and ending at the ned
            wdayname = weekdays[wday_safe]
            calendardate = calendardate_safe
          } else { #start and end at the same day
            if (tmp10w > th2) { #end of last sleep period starts at first day (end of night is before midnight)
              wdayname = weekdays[wday_safe]
              calendardate = paste(soso$mday,"/",(soso$mon+1),"/",(soso$year+1900),sep="")
            } else { #second day
              tmp7 = as.POSIXlt(tmp7) - (24*3600) #change date
              soso = unclass(as.POSIXlt(tmp7))
              wday = soso$wday #day of the week 0-6 and 0 is Sunday
              wday = wday + 1 #change to 1-7 number
              wdayname = weekdays[wday]
              calendardate = paste(soso$mday,"/",(soso$mon+1),"/",(soso$year+1900),sep="")
            }
          }
        }

        # wake
        tmp11 = as.character(sleepdet.t$sib.end.time[which(sleepdet.t$sib.period == sp)])
        tmp12 = unlist(strsplit(tmp11," "))
        tmp13 = unlist(strsplit(tmp12[2],":"))
        if (length(tmp12) == 1) {
          tmp14 = 0
        } else {
          tmp14 = as.numeric(tmp13[1]) + (as.numeric(tmp13[2])/60) + (as.numeric(tmp13[3])/3600)
        }
        if (tmp10 != tmp14) {#added 15/12/2014
        if (tmp10 < 12) tmp10 = tmp10 + 24
        if (tmp14 <= 12) tmp14 = tmp14 + 24 #changed < into <= on 18-11-2014
        }
        spo[sp,2] = tmp10
        spo[sp,3] = tmp14
      }
    }
  }
  invisible(list(spo=spo,calendardate=calendardate,wdayname=wdayname))
}