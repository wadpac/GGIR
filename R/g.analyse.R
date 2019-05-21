g.analyse =  function(I,C,M,IMP,qlevels=c(),qwindow=c(0,24),quantiletype = 7,L5M5window = c(0,24),M5L5res=10,
                      includedaycrit = 16,ilevels=c(),winhr=5,idloc=1,snloc=1,
                      mvpathreshold = c(),boutcriter=c(),mvpadur=c(1,5,10),selectdaysfile=c(),
                      window.summary.size=10,
                      dayborder=0,bout.metric = 1,closedbout=FALSE,desiredtz=c(),
                      IVIS_windowsize_minutes = 60, IVIS_epochsize_seconds = 3600, iglevels = c()) {
  L5M5window = c(0,24) # as of version 1.6-0 this is hardcoded because argument qwindow now
  # specifies the window over which L5M5 analysis is done
  winhr = winhr[1]
  fname=I$filename
  averageday = IMP$averageday
  strategy = IMP$strategy
  hrs.del.start = IMP$hrs.del.start
  hrs.del.end = IMP$hrs.del.end
  maxdur = IMP$maxdur
  windowsizes = M$windowsizes
  metalong = M$metalong
  metashort = IMP$metashort
  rout = IMP$rout
  wdaycode = M$wday
  wdayname = M$wdayname
  if (length(mvpadur) > 0) mvpadur = sort(mvpadur)
  LC2 = IMP$LC2
  LC = IMP$LC
  dcomplscore = IMP$dcomplscore
  r1 = as.numeric(as.matrix(rout[,1]))
  r2 = as.numeric(as.matrix(rout[,2]))
  r4 = as.numeric(as.matrix(rout[,4]))
  r5 = as.numeric(as.matrix(rout[,5]))
  ws3 = windowsizes[1]
  ws2 = windowsizes[2]
  vi = 1
  keepindex_46 = keepindex_48 = c()
  # Time window for L5 & M5 analysis
  t0_LFMF = L5M5window[1] #start in 24 hour clock hours
  t1_LFMF = L5M5window[2]+(winhr-(M5L5res/60)) #end in 24 hour clock hours (if a value higher than 24 is chosen, it will take early hours of previous day to complete the 5 hour window
  # Time window for distribution analysis
  t_TWDI = qwindow #start and of 24 hour clock hours
  if (length(qwindow) == 0) {
    t_TWDI = c(0,24)
    if ((length(qlevels) > 0 | length(ilevels) > 0)) qwindow = c(0,24)
  }
  if (length(qwindow) > 0) {
    if (qwindow[1] != 0) qwindow = c(0,qwindow)
    if (qwindow[length(qwindow)] != 24) qwindow = c(qwindow,24)
  }
  #==========================================================================================
  # Setting paramters (NO USER INPUT NEEDED FROM HERE ONWARDS)
  domvpa = doilevels = doiglevels = doquan = FALSE
  if (length(qlevels) > 0) doquan = TRUE
  if (length(ilevels) > 0) doilevels = TRUE
  if (length(iglevels) > 0) {
    if (length(iglevels) == 1) iglevels = c(seq(0,4000,by=25),8000) # to introduce option to just say TRUE
    doiglevels = TRUE
  }
  if (length(mvpathreshold) > 0) domvpa = TRUE
  doperday = TRUE
  #------------------------------------------------------
  filesummary = matrix(" ",1,100) #matrix to be stored with summary per participant
  s_names = rep(" ",ncol(filesummary))
  NVARS = (length(colnames(IMP$metashort))-1)
  if (NVARS < 1) NVARS = 1
  if (length(qwindow) > 0) NVARS = NVARS + 2 # for qwindow non-wear time
  nfeatures = 50+NVARS*(20+length(qlevels)+length(ilevels))    #levels changed into qlevels
  if (length(qwindow) > 0) {
    if (qwindow[1] != 0 | qwindow[2] != 24) {
      nfeatures = 50+NVARS*(2*(20+(length(qlevels)+length(ilevels))))
    }
  }
  i = 1
  #---------------
  if (domvpa) { #create dummy data
    mvpanames = matrix(0,6,length(mvpathreshold))
    mvpanames[,1:length(mvpathreshold)] = c("MVPA1","MVPA2","MVPA3","MVPA4","MVPA5","MVPA6")
  }
  # What is the minimum number of accelerometer axis needed to meet the criteria for nonwear in order for the data to be detected as nonwear?
  wearthreshold = 2 #needs to be 0, 1 or 2 (hard coded to avoid inconsistency in literature)
  # Extracting basic information about the file
  hvars = g.extractheadervars(I)
  id = hvars$id;              iid =hvars$iid; idd =hvars$idd
  HN = hvars$HN;              BL = hvars$BL
  SX=hvars$SX;                SN = hvars$SN
  n_ws2_perday = (1440*60) / ws2
  n_ws3_perday = (1440*60) / ws3
  if (((nrow(metalong)/((1440*60)/ws2)*10) - (nrow(metashort)/((60/ws3)*1440)) * 10) > 1) {
    cat("Matrices 'metalong' and 'metashort' are not compatible")
  }
  #----------------------
  # Pelotas specific
  id2 = id
  iid2 = iid
  if (idloc == 3) { #remove hyphen in id-name for Pelotas id-numbers
    for (j in 1:length(id)) {
      temp = unlist(strsplit(id,"-"))
      if (length(temp) == 2) {
        id2[j] = as.character(temp[1])
      } else {
        id2[j] = as.character(id[j])
      }
    }
    for (j in 1:length(iid)) {
      temp = unlist(strsplit(iid,"-"))
      if (length(temp) == 2) {
        iid2[j] = as.character(temp[1])
      } else {
        iid2[j] = as.character(iid[j])
      }
    }
  }
  #---------------------
  # detect first and last midnight and all midnights
  tsi = which(colnames(metalong) == "timestamp")
  time = as.character(metalong[,tsi])
  startt = as.character(metalong[1,tsi])
  # basic file characteristics
  LD = nrow(metalong) * (ws2/60) #length data in minutes
  ND = nrow(metalong)/n_ws2_perday #number of days
  #  time variable
  timeline = seq(0,ceiling(nrow(metalong)/n_ws2_perday),by=1/n_ws2_perday)
  timeline = timeline[1:nrow(metalong)]
  tooshort = 0
  dmidn = g.detecmidnight(time,desiredtz) #ND,
  firstmidnight=dmidn$firstmidnight;  firstmidnighti=dmidn$firstmidnighti
  lastmidnight=dmidn$lastmidnight;    lastmidnighti=dmidn$lastmidnighti
  midnights=dmidn$midnights;          midnightsi=dmidn$midnightsi
  if (dayborder != 0) {
    midnightsi = ((midnightsi + (dayborder * (3600/ws2))) -1) + (1/(ws2/ws3)) #shift the definition of midnight if required
  }
  starttimei = 1
  endtimei = nrow(M$metalong)
  if (strategy == 2) {
    starttimei = firstmidnighti
    endtimei = lastmidnighti - 1
  }
  # calibration error
  if (length(which(r1==1)) > 0) {
    CALIBRATE = mean(as.numeric(as.matrix(metalong[which(r1==1 & r2 != 1),which(colnames(M$metalong) == "EN")]))) #mean EN during non-wear time and non-clipping time
  } else {
    CALIBRATE = c()
    is.na(CALIBRATE) = T
  }
  # get r5long
  r5long = matrix(0,length(r5),(ws2/ws3))
  r5long = replace(r5long,1:length(r5long),r5)
  r5long = t(r5long)
  dim(r5long) = c((length(r5)*(ws2/ws3)),1)
  # get indices
  ENMOi = which(colnames(metashort) == "ENMO")
  LFENMOi = which(colnames(metashort) == "LFENMO")
  BFENi = which(colnames(metashort) == "BFEN")
  HFENi = which(colnames(metashort) == "HFEN")
  HFENplusi = which(colnames(metashort) == "HFENplus")
  MADi = which(colnames(metashort) == "MAD")
  ENi = which(colnames(metashort) == "EN")
  ANYANGLEi = which(colnames(M$metashort) %in% c("anglex","angley","anglez") ==  TRUE)
  if (length(ANYANGLEi) == 0) ANYANGLEi = -1
  if (length(ENMOi) == 0) ENMOi = -1
  if (length(LFENMOi) == 0) LFENMOi = -1
  if (length(BFENi) == 0) BFENi = -1
  if (length(HFENi) == 0) HFENi = -1
  if (length(HFENplusi) == 0) HFENplusi = -1
  if (length(MADi) == 0) MADi = -1
  if (length(ENi) == 0) ENi = -1
  #===============================================
  # Extract features from the imputed data
  qcheck = r5long
  LW = length(which(as.numeric(qcheck) != 1)) / (60/ws3) #number of minutes wear time between first and last midnights
  nfulldays = (lastmidnighti - firstmidnighti) / ((3600/ws2)*24)
  ndays = length(midnights) + 1 #ceiling(nfulldays + 2) # ceiling to cope with days with 23 hours
  if (ndays != round(ndays)) { #day saving time causing trouble?
    cat("One day in this measurement is longer or shorter than 24 hours (probably related to day saving time)")
  }
  #--------------------------------------
  # derivation of distribution characteristics of the average day: quantiles (percentiles) and L5M5 method
  # Note that this is done here before all the other analyses because it only relies on the average day
  # The values and variablenames are, however, stored in the filesummary matrix towards the end (not here)
  # create names for quantile variables
  if (doquan == TRUE) {
    QLN = rep(" ",length(qlevels))
    for (QLNi in 1:length(qlevels)) {
      QLN[QLNi] = paste("p",(round((qlevels[QLNi]) * 10000))/100,sep="")
    }
  }
  
  if (doquan == TRUE) {
    QUAN = qlevels_names = c()
    ML5AD = ML5AD_names = c()
    for (quani in 1:ncol(averageday)) {
      if (colnames(M$metashort)[(quani+1)] != "anglex" &
          colnames(M$metashort)[(quani+1)] != "angley" &
          colnames(M$metashort)[(quani+1)] != "anglez") {
        #--------------------------------------
        # quantiles
        QUANtmp =  quantile(averageday[((t_TWDI[1]*(3600/ws3))+1):(t_TWDI[length(t_TWDI)]*(3600/ws3)),quani],probs=qlevels,na.rm=T,type=quantiletype)
        QUAN = c(QUAN,QUANtmp)
        qlevels_namestmp = rep(" ",length(qlevels))
        for (QLNi in 1:length(qlevels)) {
          qlevels_namestmp[QLNi] = paste(QLN[QLNi],"_",colnames(M$metashort)[(quani+1)],"_mg_",
                                         t_TWDI[1],"-",t_TWDI[length(t_TWDI)],"h",sep="")
        }
        qlevels_names = c(qlevels_names,qlevels_namestmp)
        #--------------------------------------
        #M5L5 - (Note: averageday is relative to starttime of measurement, but so is firstmidnighti (index of midnights), so M5L5 are correct)
        avday = averageday[,quani]
        avday = c(avday[(firstmidnighti*(ws2/ws3)):length(avday)],avday[1:((firstmidnighti*(ws2/ws3))-1)])
        
        if (mean(avday) > 0 & nrow(as.matrix(M$metashort)) > 1440*(60/ws3)) {
          # Note that t_TWDI[length(t_TWDI)] in the next line makes that we only calculate ML5 over the full day
          ML5ADtmp = g.getM5L5(avday,ws3,t0_LFMF=t_TWDI[1],t1_LFMF=t_TWDI[length(t_TWDI)],M5L5res,winhr)
          ML5AD = c(ML5AD,c(ML5ADtmp$DAYL5HOUR, ML5ADtmp$DAYL5VALUE, ML5ADtmp$DAYM5HOUR, ML5ADtmp$DAYM5VALUE, ML5ADtmp$V5NIGHT))
        } else {
          ML5AD = c(ML5AD," "," "," "," "," ")
        }
        ML5AD_namestmp = rep(" ",5)
        ML5N = c(paste0("L",winhr,"hr"), paste0("L",winhr), paste0("M",winhr,"hr"), paste0("M",winhr),"1to6am")
        for (ML5ADi in 1:5) {
          ML5AD_namestmp[ML5ADi] = paste(ML5N[ML5ADi],"_",colnames(M$metashort)[(quani+1)],"_mg_",
                                         L5M5window[1],"-",L5M5window[2],"h",sep="")
          if (ML5ADi == 5) {
            ML5AD_namestmp[ML5ADi] = paste(ML5N[ML5ADi],"_",colnames(M$metashort)[(quani+1)],"_mg",sep="")
          }
        }
        ML5AD_names = c(ML5AD_names,ML5AD_namestmp)
        rm(QUANtmp); rm(ML5AD_namestmp)
      }
    }
  }
  
  if (doiglevels == TRUE) { 
    # intensity gradient (as described by Alex Rowlands 2018)
    # applied to the averageday per metric (except from angle metrics)
    igfullr = igfullr_names = c()
    for (igi in 1:ncol(averageday)) {
      if (colnames(M$metashort)[(igi+1)] != "anglex" &
          colnames(M$metashort)[(igi+1)] != "angley" &
          colnames(M$metashort)[(igi+1)] != "anglez") {
        breaks = iglevels
        q59 = c()
        avday = averageday[,igi] 
        avday = c(avday[(firstmidnighti*(ws2/ws3)):length(avday)],avday[1:((firstmidnighti*(ws2/ws3))-1)])
        # we are not taking the segment of the day now (too much output)
        q60 = cut((avday*1000),breaks,right=FALSE)
        q60 = table(q60)
        q59  = (as.numeric(q60) * ws3)/60 #converting to minutes
        x_ig = zoo::rollmean(iglevels,k=2)
        y_ig = q59
        igout = g.intensitygradient(x_ig, y_ig)
        if (length(avday) > 0) {
          igfullr = c(igfullr,as.vector(unlist(igout)))
        } else {
          igfullr = c(igfullr,rep("",3))
        }
        igfullr_names = c(igfullr_names,paste0(c("ig_gradient","ig_intercept","ig_rsquared"),
                                               paste0("_",colnames(metashort)[igi+1], "_0-24hr")))
      }
    }
  }
  
  #============================
  # IS and IV variables
  # select data from first midnight to last midnight
  fmn = midnightsi[1] * (ws2/ws3)
  lmn = midnightsi[length(midnightsi)] * (ws2/ws3)
  Xi = IMP$metashort$ENMO[fmn:lmn] # this is already imputed, so no need to ignore segments
  # Xi = ifelse((Xi*1000) < 20, 0, 1) # explored on 5 June 2018 as an attempt to better mimic original ISIV construct
  # Xi = zoo::rollsum(x=Xi,k=(600/ws3)) # explored on 5 June 2018 as an first attempt to better mimic original ISIV construct
  if (length(Xi) > (IVIS_epochsize_seconds/ws3) & length(Xi) >  (IVIS_windowsize_minutes*60)/ws3) {
    if (IVIS_epochsize_seconds > ws3) { # downsample Xi now
      Xicum =cumsum(Xi)
      step = IVIS_epochsize_seconds/ws3 # should be 6 when ws3=5 and IVIS_epochsize_seconds = 30
      select= seq(1,length(Xicum),by=step) # adjusted 17/7/2017
      Xi = diff(c(0,Xicum[select]))/step
    }
    nhr = 24*round(60/IVIS_windowsize_minutes) # Number of hours in a day (modify this variable if you want to study different resolutions)
    Nsecondsinday = 24*3600
    ni = (Nsecondsinday/nhr)/IVIS_epochsize_seconds # number of epochs in an hour
    # derive average day with 1 'hour' resolution (hour => windowsize):
    N = length(Xi)
    hour = rep(1:ceiling(N/ni),each=ni)
    if (length(hour) > N) hour = hour[1:N]
    dat = data.frame(Xi=Xi,hour=hour)
    InterdailyStability = NA
    IntradailyVariability = NA
    if (nrow(dat) > 1) {
      hh = aggregate(. ~ hour,data=dat,mean)
      hh$hour_perday = hh$hour - (floor(hh$hour/nhr)*nhr) # 24 hour in a day
      hh$day = ceiling(hh$hour/nhr)
      if (nrow(hh) > 1) {
        hh2 = aggregate(. ~ hour_perday,data=hh,mean)
        Xh = hh2$Xi
        # average acceleration per day
        Xm = suppressWarnings(mean(Xh,na.rm = TRUE))
        p = length(Xh)
        InterdailyStability = (sum((Xh - Xm)^2) * N) / (p * sum((Xi-Xm)^2)) # IS: lower is less synchronized with the 24 hour zeitgeber
        IntradailyVariability = (sum(diff(Xi)^2) * N) / ((N-1) * sum((Xm-Xi)^2)) #IV: higher is more variability within days (fragmentation)
      }
    }
  } else {
    InterdailyStability = NA
    IntradailyVariability = NA
  }
  #--------------------------------------------------------------
  # Features per day
  if (doperday == TRUE) {
    if (length(selectdaysfile) > 0 & ndays == 2) {
      ndays = 1
      startatmidnight = endatmidnight  = 1
    } else {
      startatmidnight = endatmidnight = 0
      if (nfulldays >= 1) {
        if (firstmidnighti == 1) {  #if measurement starts at midnight
          ndays = ndays - 1
          startatmidnight =  1
          cat("measurement starts at midnight or there is no midnight")
        }
        if (lastmidnight == time[length(time)] & nrow(M$metashort) < ((60/ws3) * 1440)) {	#if measurement ends at midnight
          ndays = ndays - 1
          endatmidnight = 1
          cat("measurement ends at midnight or there is no midnight")
        }
      }
    }
    daysummary = matrix("",ceiling(ndays),nfeatures)
    ds_names = rep("",nfeatures)
    #=============================
    if (length(selectdaysfile) > 0) {   # Millenium cohort related:
      ndays = ceiling(ndays)
      if (ndays > 2) ndays = 2 # this is now hardcoded to be a maximum of two days
      nwindows = 1440 / window.summary.size # now defining it as X windows per 24
      windowL = 24/nwindows # now defined up hear, because window length should be a constant
      windowsummary = matrix("",(ndays*nwindows),(ncol(metashort)*7)+5)
      ws_names = rep("",ncol(windowsummary))
      # Features per day (based on on single variables)
      if (ndays > 2) ndays = 2
      gikb = 0
    }
    #===============================================
    # Features per day (based on on single variables)
    qwindowbackup = qwindow
    for (di in 1:ndays) { #run through days
      qwindow = qwindowbackup
      #extract day from matrix D and qcheck
      if (length(selectdaysfile) > 0 & startatmidnight == 1 & endatmidnight == 1) { #added on 14/9/2016
        qqq1 = 1#midnightsi[di]*(ws2/ws3) 	#a day starts at 00:00
        qqq2 = midnightsi[di]*(ws2/ws3) #(midnightsi[(di+1)]*(ws2/ws3))-1
      } else if (length(selectdaysfile) == 0 & startatmidnight == 1 & endatmidnight == 1) {
        qqq1 = midnightsi[di]*(ws2/ws3) 	#a day starts at 00:00
        qqq2 = (midnightsi[(di+1)]*(ws2/ws3))-1
      } else if (startatmidnight == 1 & endatmidnight == 0) {
        if (di < floor(ndays)) { #applying floor because when day there is day saving time then ndays is not an integer
          qqq1 = midnightsi[di]*(ws2/ws3)
          qqq2 = (midnightsi[(di+1)]*(ws2/ws3))-1
        } else if (di == floor(ndays)) {
          qqq1 = midnightsi[di]*(ws2/ws3)
          qqq2 = nrow(metashort)
        }
      } else if (startatmidnight == 0 & endatmidnight == 0) {
        if (di == 1) {
          qqq1 = 1
          qqq2 = (midnightsi[di]*(ws2/ws3))-1
        } else if (di > 1 & di < floor(ndays)) {
          qqq1 = midnightsi[(di-1)]*(ws2/ws3) # a day starts at 00:00
          qqq2 = (midnightsi[di]*(ws2/ws3))-1
        } else if (di == floor(ndays)) {
          qqq1 = midnightsi[(di-1)]*(ws2/ws3) # a day starts at 00:00
          qqq2 = nrow(metashort)
        }
      } else if (startatmidnight == 0 & endatmidnight == 1) {
        if (di == 1) {
          qqq1 = 1
          qqq2 = (midnightsi[di]*(ws2/ws3))-1
        } else if (di > 1 & di <= floor(ndays)) {
          qqq1 = midnightsi[(di-1)]*(ws2/ws3) # a day starts at 00:00
          qqq2 = (midnightsi[di]*(ws2/ws3))-1
        }
      }
      if (qqq2 > nrow(metashort)) qqq2 = nrow(metashort)
      vari = as.matrix(metashort[qqq1:qqq2,])
      val = qcheck[qqq1:qqq2]
      nvalidhours_qwindow = rep(0,length(qwindow) - 1)
      nhours_qwindow = rep(0,length(qwindow) - 1)
      # Ignore qwindow values that are not possible for this day
      LENVAL_hours = length(val)/ (60*(60/ws3)) #11.2
      if (length(which(round(LENVAL_hours) %in% 23:25 == TRUE)) == 0) {
        if (di == 1) { 
          # Following 8 lines were turned off on June 5 2018 because first and last day are imputed,
          # but turned on again on 14 March 2019 because when the first day is half the relative start
          # of the windows must be different. Yes, the half days are inputed, but we are still only
          # interested in the non-imputed part. This is probably were the confusion came from.
          hours2delta = 24 - LENVAL_hours
          qw_select = which(qwindow > hours2delta)
          if(qw_select[1] > 1) qw_select = c(qw_select[1] - 1,qw_select)
          qwindow = qwindow[qw_select]
          qwindowindices = qwindow - hours2delta # - LENVAL_hours # because 1 is now different
          if (length(which(qwindowindices < 0)) > 0) qwindowindices[which(qwindowindices < 0)] = 0
        } else if (di == ndays) {
          qwindowindices = qwindow
        }
      } else {
        hours2delta = 0
        qwindowindices = qwindow
      }
      deltaLengthQwindow = 0
      if (length(qwindow) < 2) qwindow = c()
      if (length(qwindow) > 0) {
        if (length(qwindowbackup) == 1) {
          cat("Argument to qwindow is invalid, requires a vector of at least length 2")
        }
        if (length(qwindowbackup) == 2) {
          if (qwindow[1] != 0 | qwindow[2] != 24) {
            if((qwindowindices[2]*60*(60/ws3)) <= length(val)) {
              valq = val[((qwindowindices[1]*60*(60/ws3))+1):(qwindowindices[2]*60*(60/ws3))]
            } else {
              valq = val[((qwindowindices[1]*60*(60/ws3))+1):length(val)]
            }
            nvalidhours_qwindow =length(which(valq == 0))/ (3600/ws3)
            nhours_qwindow = length(valq)/ (3600/ws3) #valid hours per day (or half a day)
          }
        } else if (length(qwindowbackup) > 2) {
          deltaLengthQwindow = length(qwindowbackup) - length(qwindowindices)
          for (qwi in 1:(length(qwindowindices)-1)) { #
            startindex = qwindowindices[qwi]*60*(60/ws3)
            endindex = qwindowindices[qwi+1]*60*(60/ws3)
            if(startindex <= length(val) & endindex <= length(val)) {
              valq = val[(startindex+1):endindex]
            } else if (startindex <= length(val) & endindex >= length(val)) {
              valq = val[(startindex+1):length(val)]
            } else if (startindex >= length(val) & endindex >= length(val)) {
              valq = c()
            }
            if (length(valq) > 0) {
              nvalidhours_qwindow[qwi + deltaLengthQwindow] =length(which(valq == 0))/ (3600/ws3)
              nhours_qwindow[qwi + deltaLengthQwindow] = length(valq)/ (3600/ws3) #valid hours per day (or half a day)
            } else {
              nvalidhours_qwindow[qwi + deltaLengthQwindow] = 0
              nhours_qwindow[qwi + deltaLengthQwindow] = 0 #valid hours per day (or half a day)
            }
          }
        }
      }
      val = as.numeric(val)
      nvalidhours = length(which(val == 0))/ (3600/ws3) #valid hours per day (or half a day)
      nhours = length(val) / (3600/ws3) #valid hours per day (or half a day)
      #start collecting information about the day
      fi = 1
      daysummary[di,fi] = unlist(strsplit(fname,"_"))[1] #participant ID
      if (idloc == 2) {
        daysummary[di,fi] = unlist(strsplit(fname,"_"))[1] #id
      } else if (idloc == 4) {
        daysummary[di,fi] = idd
      } else if (idloc == 1) {
        daysummary[di,fi] = id
      } else if (idloc == 3) {
        daysummary[di,fi] = id2
      }
      idremember = daysummary[di,fi]
      ds_names[fi] = "id";      fi = fi + 1
      daysummary[di,fi] = fname
      ds_names[fi] = "filename";  fi = fi + 1
      calenderdate = unlist(strsplit(as.character(vari[1,1])," "))[1]
      daysummary[di,fi] = calenderdate
      daysummary[di,(fi+1)] =BL
      daysummary[di,(fi+2)] = nvalidhours
      daysummary[di,(fi+3)] = nhours
      ds_names[fi:(fi+3)] = c("calender_date","bodylocation","N valid hours","N hours")
      fi = fi + 4
      if (length(qwindow > 0)) {
        if (length(qwindow) > 2 | qwindow[1] != 0 | qwindow[2] != 24) {
          for (qwi in 1:(length(qwindowbackup)-1)) { # create variables regardless of
            if (length(which(qwindowbackup[qwi] %in% qwindow == TRUE)) > 0) {
              daysummary[di,(fi)] = nvalidhours_qwindow[qwi]
              daysummary[di,(fi+1)] = nhours_qwindow[qwi]
            } else {
              daysummary[di,(fi)] = 0 # note that we only consider qwindows when there is data for the entire window, otherwise duration is 0
              daysummary[di,(fi+1)] = 0
            }
            ds_names[fi:(fi+1)] = c(paste0("N_valid_hours_",qwindowbackup[qwi],"-",qwindowbackup[qwi+1],"hr"),
                                    paste0("N_hours_",qwindowbackup[qwi],"-",qwindowbackup[qwi+1],"hr"))
            fi = fi + 2
          }
        }
      }
      
      #--------------------------------------
      weekdays = c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday")
      weekdays = rep(weekdays,26) # hardcoded maximum number of weeks of 26, not ideal
      if (di == 1) {
        daysummary[di,fi] = wdayname
      } else {
        daysummary[di,fi] = weekdays[wdaycode + (di-1)]
      }
      daysummary[di,(fi+1)] = di #day number relative to start of measurement
      ds_names[fi:(fi+1)] = c("weekday","measurementday")
      fi = fi + 2
      if (length(selectdaysfile) > 0) {
        #---------------------------
        # Description per timewindow for millenium cohort:
        for (metrici in  2:ncol(vari)) {
          Nfeatures = 7
          nhrs = (nrow(vari)/(3600/ws3))
          if (ceiling(nhrs/windowL) < nwindows) nwindows = ceiling(nhrs/windowL)
          for (gik in 1: nwindows) {
            bbb1 = (windowL * (3600/ws3) * (gik-1)) +1
            bbb2 = (windowL * (3600/ws3) * (gik))
            if (bbb2 > length(val)) bbb2 = length(val)
            windowsummary[gikb+gik,1] = idremember #id number
            windowsummary[gikb+gik,2] = SN #sn number
            windowsummary[gikb+gik,3] = fname #filename
            windowsummary[gikb+gik,4] = as.character(vari[bbb1,1])
            windowsummary[gikb+gik,5] = length(which(val[bbb1:bbb2] == 0))/ (3600/ws3) #hours of valid data
            windowsummary[gikb+gik,((metrici-2)*Nfeatures)+6] = mean(as.numeric(vari[bbb1:bbb2,metrici]))
            windowsummary[gikb+gik,((metrici-2)*Nfeatures)+7] = sd(as.numeric(vari[bbb1:bbb2,metrici]))
            if (nrow(vari) > 10 & (bbb2 - bbb1) > 10) {
              windowsummary[gikb+gik,((metrici-2)*Nfeatures)+8:12] =
                quantile(as.numeric(vari[bbb1:bbb2,metrici]),probs=c(0.05,0.25,0.5,0.75,0.95),na.rm =TRUE)
            } else {
              windowsummary[gikb+gik,(((metrici-2)*Nfeatures)+8):(((metrici-2)*Nfeatures)+12)] = NA
            }
          }
        }
        ws_names = c("id","serial number","filename","time","Nhoursvalid")
        if (ncol(vari) >= 3) {
          for (columnvari in 2:ncol(vari)) {
            metricname_end = colnames(vari)[columnvari]
            metricname_begin = c("mean metric ","sd metric ","p5 metric ","p25 metric ","p50 metric ",
                                 "p75 metric ","p95 metric ")
            ws_names = c(ws_names,paste(metricname_begin,metricname_end,sep=""))
          }
        }
        gikb = gikb + gik
      }
      if (tooshort == 0) {
        if (nvalidhours >= includedaycrit) {
          #--------------------------------------
          # Features per day and per segment of the day
          # guided by qwindow, which is a vector of indices to slice up the day 
          keepindex_46 = keepindex_48 = matrix(NA, length(2:ncol(metashort)), 2)
          # generate objects to help with selecting the slices and to give names to the respective variables
          anwi_t0 = 1 # analysis window time 0
          anwi_t1 = nrow(as.matrix(vari)) # analysis window time 1
          anwi_nameindices = "_0-24hr"
          anwi_index = 1
          if (length(qwindow) > 0) {
            if (length(qwindow) == 2 & qwindow[1] == 0 & qwindow[2] == 24) {
            } else {
              oddqwindow = 1:(length(qwindowindices)-1)
              evenqwindow = 2:length(qwindowindices)
              anwi_t0 = c(anwi_t0,((qwindowindices[oddqwindow]*60*(60/ws3))+1))
              anwi_t1 = c(anwi_t1,(qwindowindices[evenqwindow]*60*(60/ws3)))
              for (iin in 1:length(oddqwindow)) {
                anwi_nameindices = c(anwi_nameindices,paste0("_",qwindow[oddqwindow[iin]],"-",qwindow[evenqwindow[iin]],"hr"))
              }
            }
          } else {
            anwi_t0 = c(anwi_t0,((0*60*(60/ws3))+1))
            anwi_t1 = c(anwi_t1,(24*60*(60/ws3)))
            anwi_nameindices = c(anwi_nameindices,"")
          }
          fi_remember = fi
          for (anwi_index in 1:length(anwi_t0)) {
            if(anwi_index == 2 & di == 1) {
              # increase value of fi to leave enough space for the variables to be calculated in second day of measurement
              shift = (deltaLengthQwindow * (fi-fi_remember))
              fi = fi + shift
            }
            L5M5window = anwi_nameindices[anwi_index]
            anwindices = anwi_t0[anwi_index]:anwi_t1[anwi_index] # indices of varnum corresponding to a segment
            if (length(anwindices) > 0) {
              for (mi in 2:ncol(metashort)) { #run through metrics (for features based on single metrics)
                NRV = length(which(is.na(as.numeric(as.matrix(vari[,mi]))) == FALSE))
                varnum = as.numeric(as.matrix(vari[,mi])) #varnum is one column of vari
                # # if this is the first or last day and it has more than includedaycrit number of days then expand it
                # Comment out the following 10 lines if you want to include only the actual data
                if (NRV != length(IMP$averageday[,(mi-1)])) { #
                  difference = NRV - length(IMP$averageday[,(mi-1)])
                  if (di == 1) {
                    varnum = c(IMP$averageday[1:abs(difference),(mi-1)],varnum)
                  } else {
                    a56 = length(IMP$averageday[,(mi-1)]) - abs(difference)
                    a57 = length(IMP$averageday[,(mi-1)])
                    varnum = c(varnum,IMP$averageday[a56:a57,(mi-1)])
                  }
                }
                if (anwi_index != 1) { 
                  if (length(anwindices) > 0) {
                    if (max(anwindices) > length(varnum)) {
                      anwindices = anwindices[which(anwindices <= length(varnum))]
                    }
                    varnum = as.numeric(varnum[anwindices]) #cut short varnum to match day segment of interest
                  } else {
                    varnum = c()
                  }
                }
                # Starting filling output matrix daysummary with variables per day segment and full day.
                if (mi == ENMOi | mi == LFENMOi | mi == BFENi | 
                    mi == ENi | mi == HFENi | mi == HFENplusi | mi == MADi) {
                  if (length(varnum) > ((60/ws3)*60*5.5)) {
                    ML5 = g.getM5L5(varnum,ws3,t0_LFMF,t1_LFMF,M5L5res,winhr)
                    if (anwi_index > 1) {
                      L5M5shift = qwindow[anwi_index - 1]
                    } else {
                      L5M5shift = 0
                    }
                    if (length(ML5$DAYL5HOUR) > 0) {
                      daysummary[di,fi] = ML5$DAYL5HOUR + L5M5shift
                      daysummary[di,fi+1] = ML5$DAYL5VALUE
                      daysummary[di,fi+2] = ML5$DAYM5HOUR + L5M5shift
                      daysummary[di,fi+3] = ML5$DAYM5VALUE
                      if (anwi_index == 1) {
                        daysummary[di,fi+4] = ML5$V5NIGHT
                      } else {
                        daysummary[di,fi+4] = ""
                      }
                    } else {
                      daysummary[di,fi:(fi+4)] = ""
                    }
                  } else {
                    daysummary[di,fi:(fi+4)] = ""
                  }
                  ds_names[fi] = paste0("L",winhr,"hr_",colnames(metashort)[mi],"_mg",L5M5window); fi=fi+1
                  ds_names[fi] = paste0("L",winhr,"_",colnames(metashort)[mi],"_mg",L5M5window); fi=fi+1
                  ds_names[fi] = paste0("M",winhr,"hr_",colnames(metashort)[mi],"_mg",L5M5window); fi=fi+1
                  ds_names[fi] = paste0("M",winhr,"_",colnames(metashort)[mi],"_mg",L5M5window); fi=fi+1
                  ds_names[fi] = paste0("mean_",colnames(metashort)[mi],"_mg_1-6am"); fi=fi+1
                  if (anwi_nameindices[anwi_index] == "") { # for consistency with previous GGIR version
                    anwi_nameindices[anwi_index] = "_24hr"
                  }
                  if (length(varnum) > 0) {
                    daysummary[di,fi] = mean(varnum) * 1000;
                  } else {
                    daysummary[di,fi] = ""
                  }
                  if (mi == ENMOi) {
                    ds_names[fi] = paste0("mean_ENMO_mg",anwi_nameindices[anwi_index]); fi=fi+1 #ENMO
                  } else if (mi == LFENMOi) {
                    ds_names[fi] = paste0("mean_LFENMO_mg",anwi_nameindices[anwi_index]); fi=fi+1 #LFENMO
                  } else if (mi == BFENi) {
                    ds_names[fi] = paste0("mean_BFEN_mg",anwi_nameindices[anwi_index]); fi=fi+1 #BFEN
                  } else if (mi == ENi) {
                    ds_names[fi] = paste0("mean_EN_mg",anwi_nameindices[anwi_index]); fi=fi+1 #EN
                  } else if (mi == HFENi) {
                    ds_names[fi] = paste0("mean_HFEN_mg",anwi_nameindices[anwi_index]); fi=fi+1 #HFEN
                  } else if (mi == HFENplusi) {
                    ds_names[fi] = paste0("mean_HFENplus_mg",anwi_nameindices[anwi_index]); fi=fi+1 #HFEN+
                  } else if (mi == MADi) {
                    ds_names[fi] = paste0("mean_MAD_mg",anwi_nameindices[anwi_index]); fi=fi+1 #MAD
                  }
                  
                  if (anwi_nameindices[anwi_index] == "_24hr") {
                    anwi_nameindices[anwi_index] = ""
                  }
                  if (doquan == TRUE) {
                    q46 = c()
                    q46 = quantile(varnum,probs=qlevels,na.rm=T,type=quantiletype) * 1000 #times 1000 to convert to mg
                    keepindex_46[mi-1,] = c(fi,(fi+(length(qlevels)-1)))
                    namesq46 = rep(0,length(rownames(as.matrix(q46))))
                    for (rq46i in 1:length(rownames(as.matrix(q46)))) {
                      tmp1 = rownames(as.matrix(q46))[rq46i]
                      tmp2 = as.character(unlist(strsplit(tmp1,"%")))
                      namesq46[rq46i] = paste0("p",tmp2,"_",colnames(metashort)[mi],"_mg",anwi_nameindices[anwi_index]) # t_TWDI[1],"-",t_TWDI[2],"h",sep="")
                    }
                    if (length(varnum) > 0) {
                      daysummary[di,fi:(fi+(length(qlevels)-1))] = q46
                    } else {
                      daysummary[di,fi:(fi+(length(qlevels)-1))] = ""
                    }
                    ds_names[fi:(fi+(length(qlevels)-1))] = namesq46
                    fi = fi+length(qlevels)
                  }
                  if (doilevels == TRUE) {
                    breaks = ilevels
                    q48 = c()
                    q47 = cut((varnum*1000),breaks,right=FALSE)
                    q47 = table(q47)
                    q48  = (as.numeric(q47) * ws3)/60 #converting to minutes
                    keepindex_48[mi-1,] = c(fi,(fi+(length(q48)-1)))
                    namesq47 = rep(0,length(rownames(q47)))
                    
                    for (rq47i in 1:length(rownames(q47))) {
                      namesq47[rq47i] = paste0(rownames(q47)[rq47i],"_",colnames(metashort)[mi],"_mg",anwi_nameindices[anwi_index]) #t_TWDI[1],"-",t_TWDI[2],"h",sep="")
                    }
                    if (length(varnum) > 0) {
                      daysummary[di,fi:(fi+(length(q48)-1))] = q48
                    } else {
                      daysummary[di,fi:(fi+(length(q48)-1))] = ""
                    }
                    ds_names[fi:(fi+(length(q48)-1))] = namesq47
                    fi = fi+length(q48)
                  }
                  
                  if (doiglevels == TRUE) { # intensity gradient (as described by Alex Rowlands 2018)
                    breaks = iglevels
                    q49 = c()
                    q50 = cut((varnum*1000),breaks,right=FALSE)
                    q50 = table(q50)
                    q49  = (as.numeric(q50) * ws3)/60 #converting to minutes
                    x_ig = zoo::rollmean(iglevels,k=2)
                    y_ig = q49
                    igout = g.intensitygradient(x_ig, y_ig)
                    if (length(varnum) > 0) {
                      daysummary[di,fi:(fi+2)] = as.vector(unlist(igout))
                    } else {
                      daysummary[di,fi:(fi+2)] = rep("",3)
                    }
                    ds_names[fi:(fi+2)] = paste0(c("ig_gradient","ig_intercept","ig_rsquared"),paste0("_",colnames(metashort)[mi], anwi_nameindices[anwi_index]))
                    fi = fi+3
                  }
                  #=========================================
                  if (domvpa == TRUE) {
                    for (mvpai in 1:length(mvpathreshold)) {
                      mvpa = rep(0,6)
                      if (length(varnum) < 100) {
                        mvpa[1:6] = 0
                      } else {
                        # METHOD 1: time spent above threhold based on 5 sec epoch
                        mvpa[1] = length(which(varnum*1000 >= mvpathreshold[mvpai])) / (60/ws3) #time spent MVPA in minutes
                        # METHOD 2: time spent above threshold based on 1minute epoch
                        varnum2 = cumsum(c(0,varnum))
                        select = seq(1,length(varnum2),by=60/ws3)
                        varnum3 = diff(varnum2[round(select)]) / abs(diff(round(select)))
                        mvpa[2] = length(which(varnum3*1000 >= mvpathreshold[mvpai])) #time spent MVPA in minutes
                        # METHOD 3: time spent above threshold based on 5minute epoch
                        select = seq(1,length(varnum2),by=300/ws3)
                        varnum3 = diff(varnum2[round(select)]) / abs(diff(round(select)))
                        mvpa[3] = length(which(varnum3*1000 >= mvpathreshold[mvpai])) * 5 #time spent MVPA in minutes
                        # METHOD 4: time spent above threshold
                        boutduration = mvpadur[1] * (60/ws3) # per minute
                        rr1 = matrix(0,length(varnum),1)
                        p = which(varnum*1000 >= mvpathreshold[mvpai]); rr1[p] = 1
                        getboutout = g.getbout(x=rr1,boutduration=boutduration,boutcriter=boutcriter,closedbout=closedbout,
                                               bout.metric=bout.metric,ws3=ws3)
                        mvpa[4] = length(which(getboutout$x == 1))   / (60/ws3) #time spent MVPA in minutes
                        
                        # METHOD 5: time spent above threshold 5 minutes
                        boutduration = mvpadur[2] * (60/ws3) #per five minutes
                        rr1 = matrix(0,length(varnum),1)
                        p = which(varnum*1000 >= mvpathreshold[mvpai]); rr1[p] = 1
                        getboutout = g.getbout(x=rr1,boutduration=boutduration,boutcriter=boutcriter,closedbout=closedbout,
                                               bout.metric=bout.metric,ws3=ws3)
                        mvpa[5] = length(which(getboutout$x == 1))   / (60/ws3) #time spent MVPA in minutes
                        # METHOD 6: time spent above threshold 10 minutes
                        boutduration = mvpadur[3] * (60/ws3) # per ten minutes
                        rr1 = matrix(0,length(varnum),1)
                        p = which(varnum*1000 >= mvpathreshold[mvpai]); rr1[p] = 1
                        getboutout = g.getbout(x=rr1,boutduration=boutduration,boutcriter=boutcriter,closedbout=closedbout,
                                               bout.metric=bout.metric,ws3=ws3)
                        mvpa[6] = length(which(getboutout$x == 1))   / (60/ws3) #time spent MVPA in minutes
                      }
                      if (length(which(is.nan(mvpa) == TRUE)) > 0) mvpa[which(is.nan(mvpa) == TRUE)] = 0
                      mvpanames[,mvpai] = c( paste("MVPA_E",ws3,"S_T",mvpathreshold[mvpai],sep=""),
                                             paste("MVPA_E1M_T",mvpathreshold[mvpai],sep=""),
                                             paste("MVPA_E5M_T",mvpathreshold[mvpai],sep=""),
                                             paste("MVPA_E",ws3,"S_B",mvpadur[1],"M",(boutcriter * 100),"%_T",mvpathreshold[mvpai],sep=""),
                                             paste("MVPA_E",ws3,"S_B",mvpadur[2],"M",(boutcriter * 100),"%_T",mvpathreshold[mvpai],sep=""),
                                             paste("MVPA_E",ws3,"S_B",mvpadur[3],"M",(boutcriter * 100),"%_T",mvpathreshold[mvpai],sep=""))
                      for (fillds in 1:6) {
                        daysummary[di,fi] = mvpa[fillds]
                        ds_names[fi] = paste(mvpanames[fillds,mvpai],"_",colnames(metashort)[mi] ,anwi_nameindices[anwi_index],sep=""); fi=fi+1
                      }
                    }
                  }
                }
              }
            }
          }
        }
        rm(val); rm(vari)
      }
    }
  }
  #metashort is shortened from midgnight to midnight if requested (strategy 2)
  if (strategy == 2) {
    if (starttimei == 1) {
      metashort = as.matrix(metashort[starttimei:(endtimei*(ws2/ws3)),])
    } else {
      metashort = as.matrix(metashort[(starttimei*(ws2/ws3)):(endtimei*(ws2/ws3)),])
    }
  }
  ND = nrow(metashort) / n_ws3_perday #update this because of reduction in datapoints (added on 20-11-2012)
  LW = length(which(r5 < 1)) * (ws2/60) #length wear in minutes (for entire signal)
  LWp = length(which(r5[which(r4 == 0)] < 1)) * (ws2/60) #length wear in minutes (for protocol)
  LMp = length(which(r4 == 0)) * (ws2/60) #length protocol
  #================================================
  # Summary per individual, not per day:
  #------------------------------
  # Extract the average 24 hr
  lookattmp = which(colnames(metashort) != "angle" &
                      colnames(metashort) != "anglex" &
                      colnames(metashort) != "angley" &
                      colnames(metashort) != "anglez")
  lookat = lookattmp[which(lookattmp > 1)] #]c(2:ncol(metashort[,lookattmp]))
  colnames_to_lookat = colnames(metashort)[lookat]
  MA = matrix(NA,length(lookat),1)
  if (length(which(r5 == 0)) > 0) { #to catch strategy 2 with only 1 midnight and other cases where there is no valid data
    for (h in 1:length(lookat)) {
      average24h = matrix(0,n_ws3_perday,1)
      average24hc = matrix(0,n_ws3_perday,1)
      if (floor(ND) != 0) {
        for (j in 1:floor(ND)) {
          string = as.numeric(as.matrix(metashort[(((j-1)*n_ws3_perday)+1):(j*n_ws3_perday),lookat[h]]))
          val = which(is.na(string) == F)
          average24h[val,1] = average24h[val,1] + string[val] #mean acceleration
          average24hc[val,1] = average24hc[val,1] +1
        }
      }
      if (floor(ND) < ND) {
        if (floor(ND) == 0) {
          string = as.numeric(as.matrix(metashort[,lookat[h]]))
        } else {
          string = as.numeric(as.matrix(metashort[((floor(ND)*n_ws3_perday)+1):nrow(metashort),lookat[h]]))
        }
        val = which(is.na(string) == F)
        average24h[val,1] = average24h[val,1] + string[val]  #mean acceleration
        average24hc[val,1] = average24hc[val,1] +1
      }
      average24h = average24h / average24hc
      MA[h] = mean(average24h) #average acceleration in an average 24 hour cycle
    }
  } else {
    cat("file skipped for general average caculation because not enough data")
  }
  id[which(id == "NA")] =iid[which(id == "NA")]
  id2[which(id2 == "NA")] =iid2[which(id2 == "NA")]
  #check whether there was enough data in a day by looking at r5long
  weekdays = c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday")
  weekdayi = which(weekdays == wdayname)
  #-----------------------------------
  if (idloc == 2) {
    filesummary[vi] = unlist(strsplit(fname,"_"))[1] #id
  } else if (idloc == 4) {
    filesummary[vi] = idd
  } else if (idloc == 1) {
    filesummary[vi] = id
  } else if (idloc == 3) {
    filesummary[vi] = id2
  }
  #-----------------------------------
  if (snloc == 1) {
    filesummary[(vi+1)] = SN
  } else if (snloc == 2) {
    filesummary[(vi+1)] = unlist(strsplit(fname,"_"))[2]
  }
  s_names[vi:(vi+1)] = c("ID","device_sn")
  vi = vi+2
  #-----------------------------------
  filesummary[vi] = BL
  filesummary[(vi+1)] = fname
  filesummary[(vi+2)] = startt # starttime of measurement
  s_names[vi:(vi+2)] = c("bodylocation","filename","start_time")
  vi = vi+3
  #-----------------------------------
  filesummary[vi] = wdayname # weekday on which measurement started
  filesummary[(vi+1)] = I$sf
  filesummary[(vi+2)] = I$monn
  s_names[vi:(vi+2)] = c("startday","samplefreq","device")
  vi = vi+3
  #-----------------------------------
  filesummary[vi] = LC2  / ((LD/1440)*96)
  filesummary[(vi+1)] = LD/1440 #measurement duration in days
  s_names[vi:(vi+1)] = c("clipping_score", #paste("number of ",ws2/60," minute time windows with potential signal clipping (> 80% of time > 7.5 g)",sep="") divided by number of 15 minute periods
                         "meas_dur_dys")
  vi = vi+2
  #-----------------------------------
  filesummary[vi] = dcomplscore #completeness of the day
  filesummary[(vi+1)] = LMp/1440 #measurement duration according to protocol
  filesummary[(vi+2)] = LWp/1440 #wear duration in days (out of measurement protocol)
  s_names[vi:(vi+2)] = c("complete_24hcycle", # day (fraction of 24 hours for which data is available at all)
                         "meas_dur_def_proto_day","wear_dur_def_proto_day")
  vi = vi+3
  #-----------------------------------
  if (length(C$cal.error.end) == 0)   C$cal.error.end = c(" ")
  filesummary[vi] = C$cal.error.end #CALIBRATE
  filesummary[vi+1] = C$QCmessage #CALIBRATE
  for (la in 1:length(lookat)) {
    MA[la] = 	MA[la] * 1000
  }
  q0 = length(MA) + 1
  filesummary[(vi+2):(vi+q0)] = MA
  colnames_to_lookat = paste0(colnames_to_lookat,"_fullRecordingMean")
  s_names[vi:(vi+q0)] = c("calib_err",
                          "calib_status",colnames_to_lookat) #colnames(metashort)[2:ncol(metashort)]
  vi = vi+q0+2
  #---------------------------------------
  if (doquan == TRUE) {
    q1 = length(QUAN)
    filesummary[vi:((vi-1)+q1)] = QUAN*1000
    s_names[vi:((vi-1)+q1)] = paste0(qlevels_names,"_fullRecording")
    vi = vi + q1
    q1 = length(ML5AD)
    filesummary[vi:((vi-1)+q1)] = ML5AD
    s_names[vi:((vi-1)+q1)] = paste0(ML5AD_names,"_fullRecording")
    vi = vi + q1
  }
  if (doiglevels == TRUE) { 
    # intensity gradient (as described by Alex Rowlands 2018)
    # applied to the averageday per metric (except from angle metrics)
    q1 = length(igfullr)
    filesummary[vi:((vi-1)+q1)] = igfullr
    s_names[vi:((vi-1)+q1)] = paste0(igfullr_names,"_fullRecording")
    vi = vi + q1
  }
  
  #---------------------------------------
  if (tooshort == 0) {
    #--------------------------------------------------------------
    # expand with extracted values from features per day: do this for ENMO and activity levels
    wkend  = which(daysummary[,which(ds_names == "weekday")] == "Saturday" | daysummary[,which(ds_names == "weekday")] == "Sunday")
    columnWithAlwaysData = which(ds_names == "N hours")
    NVHcolumn = which(ds_names == "N valid hours") #only count in the days for which the inclusion criteria is met
    v1 = which(is.na(as.numeric(daysummary[wkend,columnWithAlwaysData])) == F &
                 as.numeric(daysummary[wkend,NVHcolumn]) >= includedaycrit)
    wkend = wkend[v1]
    wkday  = which(daysummary[,which(ds_names == "weekday")] != "Saturday" & daysummary[,which(ds_names == "weekday")] != "Sunday")
    v2 = which(is.na(as.numeric(daysummary[wkday,columnWithAlwaysData])) == F  &
                 as.numeric(daysummary[wkday,NVHcolumn]) >= includedaycrit)
    wkday = wkday[v2]
    filesummary[vi] = length(wkend) # number of weekend days
    if (is.na(filesummary[vi]) == TRUE) filesummary[vi] = 0
    filesummary[(vi+1)] = length(wkday) # number of week day
    if (is.na(filesummary[(vi+1)]) == TRUE) filesummary[(vi+1)] = 0
    s_names[vi:(vi+1)] = c("N valid WEdays","N valid WKdays")
    vi = vi + 2
    filesummary[vi] = InterdailyStability
    if (is.na(filesummary[vi]) == TRUE) filesummary[vi] = " "
    filesummary[(vi+1)] = IntradailyVariability
    if (is.na(filesummary[(vi+1)]) == TRUE) filesummary[(vi+1)] = " "
    s_names[vi:(vi+1)] = c("IS_interdailystability","IV_intradailyvariability")
    vi = vi + 2
    filesummary[vi] = IVIS_windowsize_minutes
    if (is.na(filesummary[vi]) == TRUE) filesummary[vi] = " "
    filesummary[(vi+1)] = IVIS_epochsize_seconds
    if (is.na(filesummary[(vi+1)]) == TRUE) filesummary[(vi+1)] = " "
    s_names[vi:(vi+1)] = c("IVIS_windowsize_minutes","IVIS_epochsize_seconds")
    vi = vi + 2
    #############################################################
    #metrics - summarise with stratification to weekdays and weekend days
    daytoweekvar = c(5:length(ds_names))
    
    md = which(ds_names[daytoweekvar] %in% c("measurementday", "weekday") == TRUE)
    if (length(md) > 0) daytoweekvar = daytoweekvar[-md]
    dtwtel = 0
    if (length(daytoweekvar) >= 1) {
      sp = length(daytoweekvar) + 1
      for (dtwi in daytoweekvar) {
        #checkwhether columns is empty:
        uncona = unique(daysummary[,dtwi])
        storevalue = !(length(uncona) == 1 & length(qwindow) > 2 & uncona[1] == "")
        if (storevalue == TRUE) {
          v4 = mean(suppressWarnings(as.numeric(daysummary[,dtwi])),na.rm=TRUE) #plain average of available days
          filesummary[(vi+1+(dtwtel*sp))] = v4 # #average all availabel days
          s_names[(vi+1+(dtwtel*sp))] = paste("AD_",ds_names[dtwi],sep="")  #daytoweekvar_names[dtwtel+1]
          #========================================================================
          # attempt to stratify to week and weekend days
          # Average of available days
          dtw_wkend = suppressWarnings(as.numeric(daysummary[wkend,dtwi]))
          dtw_wkday = suppressWarnings(as.numeric(daysummary[wkday,dtwi]))
          filesummary[(vi+2+(dtwtel*sp))] = suppressWarnings(mean(dtw_wkend,na.rm=TRUE)) # #weekend average
          filesummary[(vi+3+(dtwtel*sp))] = suppressWarnings(mean(dtw_wkday,na.rm=TRUE)) # #weekday average
          s_names[(vi+2+(dtwtel*sp))] = paste0("WE_",ds_names[dtwi]) #Weekend no weighting
          s_names[(vi+3+(dtwtel*sp))] = paste0("WD_",ds_names[dtwi]) #weekdays no weighting
          #==================================================
          # Weighted average of available days
          if (length(dtw_wkend) > 2) {
            dtw_wkend = c((dtw_wkend[1]+dtw_wkend[3])/2,dtw_wkend[2])
          }
          if (length(dtw_wkday) > 5) {
            dtw_wkday = c((dtw_wkday[1]+dtw_wkday[6])/2,dtw_wkday[2:5])
          }
          filesummary[(vi+4+(dtwtel*sp))] = suppressWarnings(mean(dtw_wkend,na.rm=TRUE)) # #weekend average
          filesummary[(vi+5+(dtwtel*sp))] = suppressWarnings(mean(dtw_wkday,na.rm=TRUE)) # #weekday average
          s_names[(vi+4+(dtwtel*sp))] = paste("WWE_",ds_names[dtwi],sep="")
          s_names[(vi+5+(dtwtel*sp))] = paste("WWD_",ds_names[dtwi],sep="")
          dtwtel = dtwtel + 1
        }
        s_names[(vi+1+(dtwtel*sp))] = paste("AD_",ds_names[dtwi],sep="")  #daytoweekvar_names[dtwtel+1]
        #========================================================================
        # attempt to stratify to week and weekend days
        # Average of available days
        dtw_wkend = suppressWarnings(as.numeric(daysummary[wkend,dtwi]))
        dtw_wkday = suppressWarnings(as.numeric(daysummary[wkday,dtwi]))
        if (storevalue == TRUE) {
          filesummary[(vi+2+(dtwtel*sp))] = mean(dtw_wkend,na.rm=TRUE) # #weekend average
          filesummary[(vi+3+(dtwtel*sp))] = mean(dtw_wkday,na.rm=TRUE) # #weekday average
        }
        s_names[(vi+2+(dtwtel*sp))] = paste("WE_",ds_names[dtwi],sep="") #Weekend no weighting
        s_names[(vi+3+(dtwtel*sp))] = paste("WD_",ds_names[dtwi],sep="") #weekdays no weighting
        #==================================================
        # Weighted average of available days
        if (length(dtw_wkend) > 2) {
          dtw_wkend = c((dtw_wkend[1]+dtw_wkend[3])/2,dtw_wkend[2])
        }
        if (length(dtw_wkday) > 5) {
          dtw_wkday = c((dtw_wkday[1]+dtw_wkday[6])/2,dtw_wkday[2:5])
        }
        if (storevalue == TRUE) {
          filesummary[(vi+4+(dtwtel*sp))] = mean(dtw_wkend,na.rm=TRUE) # #weekend average
          filesummary[(vi+5+(dtwtel*sp))] = mean(dtw_wkday,na.rm=TRUE) # #weekday average
        }
        s_names[(vi+4+(dtwtel*sp))] = paste("WWE_",ds_names[dtwi],sep="")
        s_names[(vi+5+(dtwtel*sp))] = paste("WWD_",ds_names[dtwi],sep="")
        dtwtel = dtwtel + 1
      }
      vi = vi+6+((dtwtel*sp)-1)
    }
    filesummary[vi] = strategy
    filesummary[(vi+1)] = hrs.del.start
    filesummary[(vi+2)] = hrs.del.end
    filesummary[(vi+3)] = maxdur
    filesummary[(vi+4)] = windowsizes[1]
    #get GGIR version
    SI = sessionInfo()
    GGIRversion = c()
    GGIRversion = SI$otherPkgs$GGIR$Version
    if (length(GGIRversion) == 0) GGIRversion = "GGIR not used"
    filesummary[(vi+5)] = GGIRversion #"2014-03-14 12:14:00 GMT"
    s_names[vi:(vi+5)] = as.character(c("data exclusion stategy (value=1, ignore specific hours; value=2, ignore all data before the first midnight and after the last midnight)",
                                        "n hours ignored at start of meas (if strategy=1)",
                                        "n hours ignored at end of meas (if strategy=1)",
                                        "n days of measurement after which all data is ignored (if strategy=1)",
                                        "epoch size to which acceleration was averaged (seconds)",
                                        "GGIR version"))
    vi = vi + 5
  }
  #---------------------------------------------------------------
  rm(metashort); rm(LD); rm(LW); rm(HN); rm(id); rm(metalong)
  #------------------------
  mw = which(is.na(daysummary)==T)
  if (length(mw) > 0) {
    daysummary[which(is.na(daysummary)==T)] = " "
  }
  cut = which(ds_names == " " | ds_names == "" | is.na(ds_names)==T)
  if (length(cut > 0)) {
    ds_names = ds_names[-cut]
    daysummary = daysummary[,-cut]
  }
  if(min(dim(as.matrix(daysummary))) == 1) {
    if (nrow(as.matrix(daysummary)) != 1) {
      daysummary = t(daysummary) #if there is only one day of data
    }
  }
  daysummary = data.frame(value=daysummary,stringsAsFactors=FALSE)
  names(daysummary) = ds_names
  
  # remove double columns with 1-6am variables
  columnswith16am = grep("1-6am",x=colnames(daysummary))
  if (length(columnswith16am) > 1) {
    daysummary = daysummary[,-columnswith16am[2:length(columnswith16am)]]
  }
  mw = which(is.na(filesummary)==T)
  if (length(mw) > 0) {
    filesummary[which(is.na(filesummary)==T)] = " "
  }
  cut = which(as.character(s_names) == " " | as.character(s_names) == "" | is.na(s_names)==T |
                s_names %in% c("AD_", "WE_", "WD_", "WWD_", "WWE_",
                               "AD_N hours", "WE_N hours", "WD_N hours", "WWD_N hours", "WWE_N hours",
                               "AD_N valid hours", "WE_N valid hours", "WD_N valid hours", "WWD_N valid hours", "WWE_N valid hours"))
  if (length(cut) > 0) {
    s_names = s_names[-cut]
    filesummary = filesummary[-cut]
  }
  
  filesummary = data.frame(value=t(filesummary),stringsAsFactors=FALSE) #needs to be t() because it will be a column otherwise
  names(filesummary) = s_names
  
  columns2order = c()
  if (ncol(filesummary) > 37) {
    columns2order = 30:(ncol(filesummary)-6)
  }
  options(encoding = "UTF-8")
  if (length(columns2order) > 0) {
    selectcolumns = c(names(filesummary)[1:29],
                      sort(names(filesummary[,columns2order])),
                      names(filesummary)[(ncol(filesummary)-5):ncol(filesummary)])
  } else {
    selectcolumns = names(filesummary)
  }
  selectcolumns = selectcolumns[which(selectcolumns %in% colnames(filesummary) == TRUE)]
  filesummary = filesummary[,selectcolumns]
  filesummary = filesummary[,!duplicated(filesummary)]
  
  if (length(selectdaysfile) > 0) {
    windowsummary = data.frame(windowsummary,stringsAsFactors = FALSE) # addition for Millenium cohort
    names(windowsummary) = ws_names
    invisible(list(summary=filesummary,daysummary=daysummary,windowsummary=windowsummary))
  } else {
    invisible(list(summary=filesummary,daysummary=daysummary))
  }
}
