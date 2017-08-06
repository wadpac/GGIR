g.sib.det = function(M,IMP,I,twd=c(-12,12),anglethreshold = 5,
                     timethreshold = c(5,10), acc.metric = "ENMO", desiredtz="Europe/London") {
  #==============================================================
  inbed = function(angle, k =60, perc = 0.1, inbedthreshold = 15, bedblocksize = 30, outofbedsize = 60, ws3 = 5) {
    # exploratory function 27/7/2017
    medabsdi = function(angle) {
      angvar = stats::median(abs(diff(angle))) #50th percentile, do not use mean because that will be outlier dependent
      return(angvar)
    }
    x = zoo::rollapply(angle, k, medabsdi) # 5 minute rolling median of the absolute difference
    nomov = rep(0,length(x)) # no movement
    inbedtime = rep(NA,length(x))
    pp = quantile(x,probs=c(perc)) * inbedthreshold 
    if (pp == 0) pp = 7
    nomov[which(x < pp)] = 1
    nomov = c(0,nomov,0)
    s1 = which(diff(nomov) == 1) #start of blocks in bed
    e1 = which(diff(nomov) == -1) #end of blocks in bed
    bedblock = which((e1 - s1) > ((60/ws3)*bedblocksize*1)) #which are the blocks longer than bedblocksize in minutes?
    if (length(bedblock) > 0) { #
      s2 = s1[bedblock] # only keep the bedblocks that are long enough
      e2 = e1[bedblock] # only keep the bedblocks that are long enough
      for (j in 1:length(s2)){
        inbedtime[ s2[j]:e2[j]] = 1 #record these blocks in the inbedtime vector
      }
      # fill up gaps in time between bed blocks
      outofbed = rep(0,length(inbedtime))
      outofbed[which(is.na(inbedtime) == TRUE)] = 1
      outofbed = c(0,outofbed,0)
      s3 = which(diff(outofbed) == 1) #start of blocks out of bed?
      e3 = which(diff(outofbed) == -1) #end blocks out of bed?
      outofbedblock = which((e3 - s3) < ((60/ws3)*outofbedsize*1))
      if (length(outofbedblock) > 0) { # only fill up gap if there are gaps
        s4 = s3[outofbedblock]
        e4 = e3[outofbedblock]
        if (length(s4) > 0) {
          for (j in 1:length(s4)){
            inbedtime[ s4[j]:e4[j]] = 1
          }
        }
      }
      if (length(inbedtime) == (length(x)+1)) inbedtime = inbedtime[1:(length(inbedtime)-1)]
      # keep indices for longest in bed block:
      inbedtime2 = rep(1,length(inbedtime))
      inbedtime2[which(is.na(inbedtime) == TRUE)] = 0
      s5 = which(diff(c(0,inbedtime2,0)) == 1) #start of blocks out of bed?
      e5 = which(diff(c(0,inbedtime2,0)) == -1) #end blocks out of bed?
      inbeddurations = e5 - s5
      longestinbed = which(inbeddurations == max(inbeddurations))
      lightsout = s5[longestinbed] - 1
      lightson = e5[longestinbed] - 1
    } else {
      lightson = c()
      lightsout = c()
    }
    invisible(list(lightsout=lightsout,lightson=lightson))
  }
  
  #==================================================
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
    ACC = as.numeric(as.matrix(D[1:nD,which(colnames(D)==acc.metric)]))
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
          
          q1 = which(diff(postch) > (i*(60/ws3))) #less than once per i minutes
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
    
    detemout = g.detecmidnight(time,desiredtz) # ND,
    midnights=detemout$midnights
    midnightsi=detemout$midnightsi
    countmidn = length(midnightsi)
    
    lightson = lightsout = L5list = rep(0,countmidn)
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
        # calculate time in bed, because this will be used by g.part4 if sleeplog is not available
        tmpANGLE = angle[qqq1:qqq2]
        inbedout = inbed(tmpANGLE,ws3=ws3)
        if (length(inbedout$lightson) > 0 & length(inbedout$lightsout) > 0) {
          lightson[1] = inbedout$lightson
          lightsout[1] = inbedout$lightsout
        }
        
        
        #------------------------------------------------------------------
        # calculate L5 because this is used in case the sleep diary is not available (added 17-11-2014)
        tmpACC = ACC[qqq1:qqq2]
        windowRL = round((3600/ws3)*5)
        if ((windowRL/2) == round(windowRL/2)) windowRL = windowRL+1
        if (length(tmpACC) < windowRL) {  0 # added 4/4/2-17
          cat("Warning: time window shorter than 5 hours which makes it impossible to identify L5")
          L5 = 0
        } else {
          ZRM = zoo::rollmean(x=c(tmpACC),k=windowRL,fill="extend",align="center") #
          
          L5 = which(ZRM == min(ZRM))[1]
          if (sd(ZRM) == 0) {
            L5 = c()
          } else {
            L5 = (L5  / (3600/ ws3)) + 12
          }
          if (length(L5) == 0) L5 = 0 #if there is no L5, because full they is zero
        }
        L5list[1] = L5
      } else { #more than one midnight
        cut = which(as.numeric(midnightsi) == 0)
        if (length(cut) > 0) {
          midnights = midnights[-cut]
          midnightsi = midnightsi[-cut]  
        }
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
          tmpACC = ACC[qqq1:qqq2]
          windowRL = round((3600/ws3)*5)
          if ((windowRL/2) == round(windowRL/2)) windowRL = windowRL+1
          ZRM = zoo::rollmean(x=c(tmpACC),k=windowRL,fill="extend",align="center") #
          L5 = which(ZRM == min(ZRM))[1]
          if (sd(ZRM) == 0) {
            L5 = c()
          } else {
            L5 = (L5  / (3600/ ws3)) + 12
          }
          if (length(L5) == 0) L5 = 0 #if there is no L5, because full they is zero
          L5list[j] = L5
          # calculate time in bed, because this will be used by g.part4 if sleeplog is not available
          tmpANGLE = angle[qqq1:qqq2]
          inbedout = inbed(tmpANGLE,ws3=ws3)
          if (length(inbedout$lightson) != 0 & length(inbedout$lightsout) != 0) {
            lightson[j] = (inbedout$lightson / (3600/ ws3)) + 12
            lightsout[j] = (inbedout$lightsout / (3600/ ws3)) + 12
          }
          
          
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
    lightson = c()
    lightsout = c()
    detection.failed = TRUE
  }
  invisible(list(output = metatmp,detection.failed=detection.failed,L5list=L5list,lightson =lightson,lightsout=lightsout))
}
