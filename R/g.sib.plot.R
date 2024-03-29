g.sib.plot = function(SLE, M, I, plottitle, nightsperpage = 7, desiredtz = "") {
  A = SLE$output
  # invalid = A$invalid
  night = A$night
  sleep = A[,(which(colnames(A) == "night") + 1):ncol(A)]
  D = as.matrix(M$metashort) #IMP$
  nD = nrow(D)
  S = as.matrix(M$metalong)
  nS = nrow(S)
  space = ifelse(length(unlist(strsplit(format(A$time[1])," "))) > 1,TRUE,FALSE)
  temptime = format(unlist(A$time))
  if (space == FALSE) {
    time = as.POSIXlt(temptime,tz = desiredtz, format = "%Y-%m-%dT%H:%M:%S%z")
    timeL = iso8601chartime2POSIX(S[1:nS, 1], tz = desiredtz)
  } else {
    time = as.POSIXlt(temptime, tz = desiredtz)
    timeL = as.POSIXlt(as.character(S[1:nS,1]), tz = desiredtz)
  }
  ws3 = M$windowsizes[1]
  ws2 = M$windowsizes[2]
  n_ws2_perday = (1440*60) / ws2
  n_ws3_perday = (1440*60) / ws3
  #ENMO
  ENMO = as.numeric(as.matrix(D[1:nD,2])) * 1000 #indicator of acceleration
  # angle
  angle = as.numeric(as.matrix(D[1:nD,which(colnames(M$metashort) == "anglez")]))
  if (length(which(is.na(angle) == TRUE)) > 0) {
    if (which(is.na(angle) == TRUE)[1] == length(angle)) {
      angle[length(angle)] = angle[length(angle) - 1]
    }
  }
  # get temperature and lightpeak
  if (I$monn == "geneactive") {
    temperature = as.numeric(as.matrix(S[1:nS,6]))
    lightpeak = as.numeric(as.matrix(S[1:nS,5]))
  } else {
    lightpeak = temperature = rep(0,nS)
  }
  # transform temperature signal into something that is easy to plot on top of other data
  if (length(which(is.na(lightpeak) == TRUE)) > 0) {
    lightpeak[which(is.na(lightpeak) == TRUE)] = 0
  }
  if (length(which(is.na(temperature) == TRUE)) > 0) {
    temperature[which(is.na(temperature) == TRUE)] = mean(temperature, na.rm = TRUE)
  }
  if (min(temperature, na.rm = TRUE) < 0) {
    temperature = temperature + abs(min(temperature, na.rm = TRUE))
  }
  temperature = temperature - min(temperature)
  temperature = (scale(temperature) * 70) - 90
  # transform light signal into something that is easy to plot on top of other data
  light = lightpeak
  light = light + min(light)
  light = (scale(light) * 210) - 120
  un = unique(night)
  if (length(which(un == 0 | is.na(un) == TRUE)) > 0) {
    un = un[-c(which(un == 0 | is.na(un) == TRUE))]
  }
  ncolsleep = ncol(sleep)
  if (length(ncolsleep) == 0) {
    zeros = which(sleep == 0)
    if (length(zeros) > 0) sleep[zeros] = NA
  } else {
    for (j in 1:ncolsleep) {
      zeros = which(sleep[,j] == 0)
      if (length(zeros) > 0) sleep[zeros,j] = NA
    }
  }
  for (i in 1:length(un)) {
    if (i == 1 | (i-1)/9 == round((i-1)/9)) {
      par(mfrow=c(9,1),mar=c(1,5,3,1),oma=c(2,0,0,0)) 
    }
    qqq1 = which(night == i)[1]
    qqq2 = which(night == i)[length(which(night == i))]
    qqq3 = which(format(timeL) == format(time[qqq1]))
    qqq4 = which(format(timeL) == format(time[qqq2]))
    if (length(qqq3) == 0) qqq3 = round(qqq1 / (ws2/ws3))
    if (length(qqq4) == 0) qqq4 = round(qqq2 / (ws2/ws3))
    if (qqq3 == 0) qqq3 = 1
    
    
    #activity in the background, so go first
    ENMOscaled = scale(ENMO[qqq1:qqq2],scale = abs(diff(range(ENMO[qqq1:qqq2])))/180) - 204
    plot(time[qqq1:qqq2],ENMOscaled,type="l",xlab="",main=paste("Night: ",i," ",plottitle),col="grey",
          ylab="angle (black)",   ylim=c(-250,90),xaxt="n",yaxt="n",cex.lab=0.8,cex.main=0.6,bty="n",lwd=0.4)
    #temperature
    lines(timeL[qqq3:qqq4],light[qqq3:qqq4],type="l",col="yellow",lty=1,lwd=1)
    #light
    lines(timeL[qqq3:qqq4],temperature[qqq3:qqq4],type="l",col="lightblue",lty=1,lwd=1)
    
    lines(time[qqq1:qqq2],angle[qqq1:qqq2],type="l",lwd=0.4,col="black")
    axis(2, at = c(-90,0,90))
    r <- as.POSIXct(round(range(time[qqq1:qqq2]), "hours"))
    abline(h=0,lty=3,lwd=1.4,col="grey")
    abline(v=seq(r[1], r[2], by = "hour"),lty=3,lwd=1.4,col="grey")
    #time axis
    axis.POSIXct(1, at = seq(r[1], r[2], by = "hour"), format = "%H")
    
    if (length(ncol(sleep)) > 0) {
      ncolsleep = ncol(sleep)
      CL = rainbow(ncolsleep)
      for (j in 1:ncolsleep) {
        lines(time[qqq1:qqq2],(-(240)+(sleep[qqq1:qqq2,j]*((80/ncolsleep)*j))),type="l",col=CL[j],lwd=1)
      }
    } else {
      ncolsleep = 1
      CL = rainbow(ncolsleep)
      for (j in 1:ncolsleep) {
        lines(time[qqq1:qqq2],(-(240)+(sleep[qqq1:qqq2]*((80/ncolsleep)*j))),type="l",col=CL[j],lwd=1)
      }
    }
  }
}