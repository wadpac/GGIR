g.sib.det = function(M,IMP,I,twd=c(-12,12),anglethreshold = 5,
                       timethreshold = c(5,10)) {
  #==============================================================
  # get variables  
  D = IMP$metashort
  nD = nrow(D)
  mon = I$monn
  ws3 = M$windowsizes[1]
  ws2 = M$windowsizes[2]
  n_ws2_perday = (1440*60) / ws2
  n_ws3_perday = (1440*60) / ws3
  Nsleep = length(timethreshold) * length(anglethreshold)
  sleep = matrix(0,nD,Nsleep)
  #--------------------
  # get indicator of non-wear periods
  rout = IMP$rout
  r5 = as.numeric(as.matrix(rout[,5]))
  r5long = matrix(0,length(r5),(ws2/ws3))
  r5long = replace(r5long,1:length(r5long),r5)
  r5long = t(r5long)
  dim(r5long) = c((length(r5)*(ws2/ws3)),1)
  if (nD < length(r5long)) {
    invalid = r5long[1:nD]
  } else {
    invalid = c(r5long,rep(0,(nD-length(r5long))))
  }
  rm(r5,rout)
  # Deriving file characteristics from 15 min summary files
  ND = nD/n_ws3_perday #number of days
  if (ND > 0.2) {
    #========================================================================
    # timestamps
    time = as.character(D[1:nD,1])
    # angle
    if (length(which(colnames(D)=="anglez")) == 0) {
      cat("metric anglez was not extracted, please make sure that anglez  is extracted")
    }
    angle = as.numeric(as.matrix(D[1:nD,which(colnames(D)=="anglez")]))
    ENMO = as.numeric(as.matrix(D[1:nD,which(colnames(D)=="ENMO")]))
    night = rep(0,length(angle))
    if (length(which(is.na(angle) ==TRUE)) > 0) {
      if (which(is.na(angle) ==TRUE)[1] == length(angle)) {
        angle[length(angle)] = angle[length(angle)-1]
      }
    }
    rm(D)
    #==================================================================
    # sleep detection
    
    angle[which(is.na(angle) == T)] = 0
    cnt = 1
    for (i in timethreshold) {
      for (j in anglethreshold) {
        sdl1 = rep(0,length(time))
        postch = which(abs(diff(angle)) > j) #posture change of at least j degrees
        # count posture changes that happen less than once per ten minutes
        q1 = c()
        if (length(postch) > 1) {
          q1 = which(diff(postch) > (i*12)) #less than once per i minutes
        }
        if (length(q1) > 0) {
          for (gi in 1:length(q1)) {
            sdl1[postch[q1[gi]]:postch[q1[gi]+1]] = 1 #periods with no posture change
          }
        } else { #possibly a day without wearing
          if (length(postch) < 10) {  #possibly a day without wearing
            sdl1[1:length(sdl1)] = 1 #periods with no posture change
          } else {  #possibly a day with constantly posture changes
            sdl1[1:length(sdl1)] = 0 #periodsposture change
          }
        }
        sleep[,cnt] = sdl1
        cnt = cnt+ 1
      }
    }
    cnt = 1
    sleep = as.data.frame(sleep)
    for (i in timethreshold) {
      for (j in anglethreshold) {
        colnames(sleep)[cnt] = paste("T",i,"A",j,sep="")
        cnt = cnt + 1
      }
    }
    #-------------------------------------------------------------------
    # detect midnights
    midnights = midnightsi = matrix(0,(round(ND)+3),1)
    countmidn = 0 #change to 1 on 27/65/2014
    for (cnt in 1:length(time)) { #need to detect all midnights because some days may be 23 or 25 hours
      temp = unlist(strsplit(time[cnt]," "))[2]
      temp2 = as.numeric(unlist(strsplit(temp,":"))[1])
      temp3 = as.numeric(unlist(strsplit(temp,":"))[2])
      temp4 = as.numeric(unlist(strsplit(temp,":"))[3])
      if (temp2+temp3+temp4 == 0) {
        countmidn = countmidn + 1
        midnights[countmidn] = as.character(time[cnt])
        midnightsi[countmidn] = cnt
      }
    }
    L5list = rep(0,countmidn)
    if (countmidn != 0) {
      if (countmidn == 1) {
        tooshort = 1
        lastmidnight = time[length(time)]
        lastmidnighti = length(time)
        firstmidnight = time[1]
        firstmidnighti = 1
        qqq1 = midnightsi[1] + (twd[1]*(3600/ws3)) #noon
        qqq2 = midnightsi[1] + (twd[2]*(3600/ws3)) #noon
        if (qqq2 > length(time))  qqq2 = length(time)
        if (qqq1 < 1)             qqq1 = 1
        night[qqq1:qqq2] = 1
        detection.failed = FALSE  
        #------------------------------------------------------------------
        # calculate L5 because this is used in case the sleep diary is not available (added 17-11-2014)
        tmpENMO = ENMO[qqq1:qqq2]
        windowRL = round((3600/ws3)*5)
        if ((windowRL/2) == round(windowRL/2)) windowRL = windowRL+1
        ZRM = zoo::rollmean(x=c(tmpENMO),k=windowRL,fill="extend",align="center") #
        L5 = which(ZRM == min(ZRM))[1]
        if (sd(ZRM) == 0) {
          L5 = c()
        } else {
          L5 = (L5  / (3600/ ws3)) + 12
        }
        if (length(L5) == 0) L5 = 0 #if there is no L5, because full they is zero
        L5list[1] = L5
      } else { #more than one midnight
        midnights = midnights[-c(which(as.numeric(midnightsi) == 0))]
        midnightsi = midnightsi[-c(which(as.numeric(midnightsi) == 0))]  
        lastmidnight = midnights[length(midnights)]
        lastmidnighti = midnightsi[length(midnights)]
        firstmidnight = midnights[1]
        firstmidnighti = midnightsi[1]
        for (j in 1:(countmidn)) { #-1
          qqq1 = midnightsi[j] + (twd[1]*(3600/ws3)) #noon
          qqq2 = midnightsi[j] + (twd[2]*(3600/ws3)) #noon
          if (qqq2 > length(time))  qqq2 = length(time)
          if (qqq1 < 1)             qqq1 = 1
          night[qqq1:qqq2] = j
          #------------------------------------------------------------------
          # calculate L5 because this is used in case the sleep diary is not available (added 17-11-2014)
          tmpENMO = ENMO[qqq1:qqq2]
          windowRL = round((3600/ws3)*5)
          if ((windowRL/2) == round(windowRL/2)) windowRL = windowRL+1
          ZRM = zoo::rollmean(x=c(tmpENMO),k=windowRL,fill="extend",align="center") #
          L5 = which(ZRM == min(ZRM))[1]
          if (sd(ZRM) == 0) {
            L5 = c()
          } else {
            L5 = (L5  / (3600/ ws3)) + 12
          }
          if (length(L5) == 0) L5 = 0 #if there is no L5, because full they is zero
          L5list[j] = L5
        }
        detection.failed = FALSE      
      }
    } else {
      cat("No midnights found")
      detection.failed = TRUE
    }
    metatmp = data.frame(time,invalid,night=night,sleep = sleep)
  } else {
    metatmp =c()
    L5list = c()
    detection.failed = TRUE
  }
  invisible(list(output = metatmp,detection.failed=detection.failed,L5list=L5list))
}