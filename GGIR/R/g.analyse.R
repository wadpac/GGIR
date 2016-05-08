g.analyse =  function(I,C,M,IMP,qlevels=c(),qwindow=c(0,24),quantiletype = 7,L5M5window = c(0,24),M5L5res=10,
                      includedaycrit = 16,ilevels=c(),winhr=5,idloc=1,snloc=1,
                      mvpathreshold = c(),boutcriter=c(),mvpadur=c(1,5,10),selectdaysfile=c(),
                      window.summary.size=10,
                      dayborder=0,mvpa.2014 = FALSE,closedbout=FALSE) {
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
  r3 = as.numeric(as.matrix(rout[,3]))
  r4 = as.numeric(as.matrix(rout[,4]))
  r5 = as.numeric(as.matrix(rout[,5]))
  ws3 = windowsizes[1]
  ws2 = windowsizes[2]
  vi = 1
  
  # Time window for L5 & M5 analysis
  t0_LFMF = L5M5window[1] #start in 24 hour clock hours
  t1_LFMF = L5M5window[2]+(winhr-(M5L5res/60)) #end in 24 hour clock hours (if a value higher than 24 is chosen, it will take early hours of previous day to complete the 5 hour window
  # Time window for distribution analysis
  t_TWDI = qwindow #start and of 24 hour clock hours
  #==========================================================================================
  # Setting paramters (NO USER INPUT NEEDED FROM HERE ONWARDS)
  domvpa = doilevels = doquan = FALSE
  if (length(qlevels) > 0) doquan = TRUE    
  if (length(ilevels) > 0) doilevels = TRUE    
  if (length(mvpathreshold) > 0) domvpa = TRUE    
  doperday = TRUE
  #------------------------------------------------------
  summary = matrix(" ",1,100) #matrix to be stored with summary per participant
  s_names = rep(" ",ncol(summary))
  nfeatures = 50+length(qlevels)+length(ilevels)    #levels changed into qlevels 
  i = 1  
  #---------------
  if (domvpa) { #create dummy data
    mvpanames = matrix(0,6,length(mvpathreshold))
    mvpanames[,1:length(mvpathreshold)] = c("MVPA1","MVPA2","MVPA3","MVPA4","MVPA5","MVPA6")
  }
  # What is the minimum number of accelerometer axis needed to meet the criteria for nonwear in order for the data to be detected as nonwear?
  wearthreshold = 2 #needs to be 0, 1 or 2
  # Extracting basic meta_short variables
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
  dmidn = g.detecmidnight(ND,time)
  firstmidnight=dmidn$firstmidnight;  firstmidnighti=dmidn$firstmidnighti
  lastmidnight=dmidn$lastmidnight;    lastmidnighti=dmidn$lastmidnighti
  midnights=dmidn$midnights;          midnightsi=dmidn$midnightsi
  
  if (dayborder != 0) {
    midnightsi = ((midnightsi + (dayborder * (3600/ws2))) -1) + (1/(ws2/ws3)) #shift the definition of midnight if required
    # midnightsi = ((midnightsi + (dayborder * (3600/ws2))) -1) + 1 #shift the definition of midnight if required
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
  # get indeces
  ENMOi = which(colnames(metashort) == "ENMO")
  LFENMOi = which(colnames(metashort) == "LFENMO")
  BFENi = which(colnames(metashort) == "BFEN")
  HFENi = which(colnames(metashort) == "HFEN")
  HFENplusi = which(colnames(metashort) == "HFENplus")
  ENi = which(colnames(metashort) == "EN")
  #   anglei = which(colnames(metashort) == "angle")
  if (length(ENMOi) == 0) ENMOi = -1
  if (length(LFENMOi) == 0) LFENMOi = -1
  if (length(BFENi) == 0) BFENi = -1
  if (length(HFENi) == 0) HFENi = -1
  if (length(HFENplusi) == 0) HFENplusi = -1
  if (length(ENi) == 0) ENi = -1
  #   if (length(anglei) == 0) anglei = -1
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
  # The values and variablenames are, however, stored in the summary matrix towards the end (not here)
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
        QUANtmp =  quantile(averageday[((t_TWDI[1]*(3600/ws3))+1):(t_TWDI[2]*(3600/ws3)),quani],probs=qlevels,na.rm=T,type=quantiletype)
        QUAN = c(QUAN,QUANtmp)
        qlevels_namestmp = rep(" ",length(qlevels))
        for (QLNi in 1:length(qlevels)) {
          qlevels_namestmp[QLNi] = paste(QLN[QLNi],"_",colnames(M$metashort)[(quani+1)],"_mg_",
                                         t_TWDI[1],"-",t_TWDI[2],"h",sep="")
        }
        qlevels_names = c(qlevels_names,qlevels_namestmp)
        #--------------------------------------
        #M5L5
        avday = averageday[,quani]
        avday = c(avday[(firstmidnighti*(ws2/ws3)):length(avday)],avday[1:((firstmidnighti*(ws2/ws3))-1)])
        if (mean(avday) > 0 & nrow(as.matrix(M$metashort)) > 1440*(60/ws3)) {
          ML5ADtmp = g.getM5L5(avday,ws3,t0_LFMF,t1_LFMF,M5L5res,winhr)
          ML5AD = c(ML5AD,c(ML5ADtmp$DAYL5HOUR, ML5ADtmp$DAYL5VALUE, ML5ADtmp$DAYM5HOUR, ML5ADtmp$DAYM5VALUE, ML5ADtmp$V5NIGHT))
        } else {
          ML5AD = c(ML5AD," "," "," "," "," ")
        }
        ML5AD_namestmp = rep(" ",5)      
        ML5N = c("L5hr","L5","M5hr","M5","1to6am")
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
  #--------------------------------------------------------------
  # Features per day
  if (doperday == TRUE) { # extract fe
    startatmidnight = 0  #new 28-11-2012
    if (firstmidnighti == 1) {  #new 28-11-2012 #if measurement starts at midnight
      ndays = ndays - 1  #new 28-11-2012
      startatmidnight =  1   #new 28-11-2012
      cat("measurement starts at midnight or there is no midnight")  #new 28-11-2012
    }
    endatmidnight = 0
    if (lastmidnight == time[length(time)] ) {	#if measurement ends at midnight
      ndays = ndays - 1
      endatmidnight = 1
      cat("measurement ends at midnight or there is no midnight")
    }
#     if (lastmidnighti == firstmidnighti) { #turned off 11 may 2015, because for some projects one night may be still useful
#       tooshort = 1
#     }
    daysummary = matrix("",ceiling(ndays),nfeatures)
    ds_names = rep("",nfeatures)
    #=============================
    if (length(selectdaysfile) > 0) {   # Millenium cohort related:
        ndays = ceiling(ndays)
      if (ndays > 2) ndays = 2 # this is now hardcoded to be a maximum of two days
      nwindows = 1440 / window.summary.size # now defining it as X windows per 24
      windowsummary = matrix("",(ndays*nwindows),(ncol(metashort)*7)+5)
      ws_names = rep("",ncol(windowsummary))
      # Features per day (based on on single variables)
      if (ndays > 2) ndays = 2
      gikb = 0
    }
    #===============================================
    # Features per day (based on on single variables)
    for (di in 1:ndays) { #run through days
      #extract day from matrix D and qcheck
      if (startatmidnight == 1 & endatmidnight == 1) {
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
      vari = as.matrix(metashort[qqq1:qqq2,])
      val = qcheck[qqq1:qqq2]
      #--------------------------------
      val = as.numeric(val)
      nvalidhours = length(which(val == 0))/ (3600/ws3) #valid hours per day (or half a day)
      nhours = length(val)/ (3600/ws3) #valid hours per day (or half a day)
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
      if (length(selectdaysfile) > 0) {
        calenderdate = unlist(strsplit(as.character(vari[min(c(10,nrow(vari))),1])," "))[1] #adjusted for millenium cohort
      } else {
        calenderdate = unlist(strsplit(as.character(vari[1,1])," "))[1]
      }
      daysummary[di,fi] = calenderdate               
      daysummary[di,(fi+1)] =BL
      daysummary[di,(fi+2)] = nvalidhours
      daysummary[di,(fi+3)] = nhours
      ds_names[fi:(fi+3)] = c("calender_date","bodylocation","N valid hours","N hours")
      fi = fi + 4
      #--------------------------------------      
      weekdays = c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"
                   ,"Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"
                   ,"Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"
                   ,"Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday")
      if (di == 1) {
        daysummary[di,fi] = wdayname
      } else {
        daysummary[di,fi] = weekdays[wdaycode + (di-1)]
      }
      daysummary[di,(fi+1)] = di #day number relative to start of measurement
      ds_names[fi:(fi+1)] = c("weekday","measurmentday")
      fi = fi + 2
      if (length(selectdaysfile) > 0) {
        #---------------------------
        # Description per timewindow for millenium cohort:
        for (metrici in  2:ncol(vari)) {
          Nfeatures = 7
          #         print("--------------------------------------")
          #         print(paste0("metric",metrici))
          nhrs = (nrow(vari)/(3600/ws3))
          windowL = 24/nwindows
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
              windowsummary[gikb+gik,((metrici-2)*Nfeatures)+8] = quantile(as.numeric(vari[bbb1:bbb2,metrici]),probs=0.05)
              windowsummary[gikb+gik,((metrici-2)*Nfeatures)+9] = quantile(as.numeric(vari[bbb1:bbb2,metrici]),probs=0.25)
              windowsummary[gikb+gik,((metrici-2)*Nfeatures)+10] = quantile(as.numeric(vari[bbb1:bbb2,metrici]),probs=0.5)
              windowsummary[gikb+gik,((metrici-2)*Nfeatures)+11] = quantile(as.numeric(vari[bbb1:bbb2,metrici]),probs=0.75)
              windowsummary[gikb+gik,((metrici-2)*Nfeatures)+12] = quantile(as.numeric(vari[bbb1:bbb2,metrici]),probs=0.95)
            } else {
              windowsummary[gikb+gik,(((metrici-2)*Nfeatures)+8):(((metrici-2)*Nfeatures)+12)] = NA
            }
          }
        }
        ws_names = c("id","serial number","filename",
                     "time","Nhoursvalid")
        if (ncol(vari) == 3) {
          ws_names = c(ws_names,paste0("mean metric ",colnames(vari)[2]),
                       paste0("sd metric ",colnames(vari)[2]),
                       paste0("p5 metric ",colnames(vari)[2]),
                       paste0("p25 metric ",colnames(vari)[2]),
                       paste0("p50 metric ",colnames(vari)[2]),
                       paste0("p75 metric ",colnames(vari)[2]),
                       paste0("p95 metric ",colnames(vari)[2]),
                       paste0("mean metric ",colnames(vari)[3]),
                       paste0("sd metric ",colnames(vari)[3]),
                       paste0("p5 metric ",colnames(vari)[3]),
                       paste0("p25 metric ",colnames(vari)[3]),
                       paste0("p50 metric ",colnames(vari)[3]),
                       paste0("p75 metric ",colnames(vari)[3]),
                       paste0("p95 metric ",colnames(vari)[3]))
        }
        gikb = gikb + gik
      }
      
      if (tooshort == 0) {
        #--------------------------------------      
        # extract time spent in activity levels (there are possibly many more features that can be extracted from this)
        if (nvalidhours >= includedaycrit) {
          #============================================================
          for (mi in 2:ncol(metashort)) { #run through metrics (for features based on single metrics)
            varnum = as.numeric(as.matrix(vari[,mi]))
            # if this is the first or last day and it has more than includedaycrit number of days then expand it
            if (length(which(is.na(varnum) == FALSE)) != length(IMP$averageday[,(mi-1)])) { #
              difference = length(which(is.na(varnum) == FALSE)) - length(IMP$averageday[,(mi-1)])
              if (di == 1) {
                varnum = c(IMP$averageday[1:abs(difference),(mi-1)],varnum)
              } else {
                a56 = length(IMP$averageday[,(mi-1)]) - abs(difference)
                a57 = length(IMP$averageday[,(mi-1)])
                varnum = c(varnum,IMP$averageday[a56:a57,(mi-1)])
              }
            }
            if (mi == ENMOi | mi == LFENMOi | mi == BFENi | mi == ENi | mi == HFENi | mi == HFENplusi) { #currently intensity/activity level features are based on metric ENMO, but by copy-pasting this to another metric this should work the same.
              ML5 = g.getM5L5(varnum,ws3,t0_LFMF,t1_LFMF,M5L5res,winhr)
              if (length(ML5$DAYL5HOUR) > 0) {
                daysummary[di,fi] = ML5$DAYL5HOUR; ds_names[fi] = paste("L5hr_",colnames(metashort)[mi],"_mg_",L5M5window[1],"-",L5M5window[2],"h",sep=""); fi=fi+1
                daysummary[di,fi] = ML5$DAYL5VALUE; ds_names[fi] = paste("L5_",colnames(metashort)[mi],"_mg_",L5M5window[1],"-",L5M5window[2],"h",sep=""); fi=fi+1
                daysummary[di,fi] = ML5$DAYM5HOUR; ds_names[fi] = paste("M5hr_",colnames(metashort)[mi],"_mg_",L5M5window[1],"-",L5M5window[2],"h",sep=""); fi=fi+1
                daysummary[di,fi] = ML5$DAYM5VALUE;  ds_names[fi] = paste("M5_",colnames(metashort)[mi],"_mg_",L5M5window[1],"-",L5M5window[2],"h",sep=""); fi=fi+1
                daysummary[di,fi] = ML5$V5NIGHT;  ds_names[fi] = paste("mean_",colnames(metashort)[mi],"_mg_1-6am",sep=""); fi=fi+1
              }
              if (mi == ENMOi) {
                daysummary[di,fi] = mean(varnum)* 1000; ds_names[fi] = "mean_ENMO_mg_24hr"; fi=fi+1 #ENMO 
              } else if (mi == LFENMOi) {
                daysummary[di,fi] = mean(varnum) * 1000; ds_names[fi] = "mean_LFENMO_mg_24hr"; fi=fi+1 #LFENMO 
              } else if (mi == BFENi) {
                daysummary[di,fi] = mean(varnum) * 1000; ds_names[fi] = "mean_BFEN_mg_24hr"; fi=fi+1 #BFEN
              } else if (mi == ENi) {
                daysummary[di,fi] = mean(varnum) * 1000; ds_names[fi] = "mean_EN_mg_24hr"; fi=fi+1 #EN
              } else if (mi == HFENi) {
                daysummary[di,fi] = mean(varnum) * 1000;  ds_names[fi] = "mean_HFEN_mg_24hr"; fi=fi+1 #HFEN
              } else if (mi == HFENplusi) {
                daysummary[di,fi] = mean(varnum) * 1000;  ds_names[fi] = "mean_HFENplus_mg_24hr"; fi=fi+1 #HFEN+
              }
              if (doquan == TRUE) {
                #newly added on 9-7-2013, percentiles of acceleration in the specified window:
                q46 = quantile(varnum[((qwindow[1]*60*(60/ws3))+1):(qwindow[2]*60*(60/ws3))],probs=qlevels,na.rm=T,type=quantiletype) * 1000 #times 1000 to convert to mg
                keepindex_46 = c(fi,(fi+(length(qlevels)-1)))
                namesq46 = rep(0,length(rownames(as.matrix(q46))))
                for (rq46i in 1:length(rownames(as.matrix(q46)))) {
                  tmp1 = rownames(as.matrix(q46))[rq46i]
                  tmp2 = as.character(unlist(strsplit(tmp1,"%")))
                  namesq46[rq46i] = paste("p",tmp2,"_mg_",t_TWDI[1],"-",t_TWDI[2],"h",sep="")
                }
                daysummary[di,fi:(fi+(length(qlevels)-1))] = q46
                ds_names[fi:(fi+(length(qlevels)-1))] = paste(namesq46,"_",colnames(metashort)[mi],sep="")
                fi = fi+length(qlevels)
              }
              if (doilevels == TRUE) {
                breaks = ilevels
                q47 = cut((varnum[((qwindow[1]*60*(60/ws3))+1):(qwindow[2]*60*(60/ws3))]*1000),breaks,right=FALSE)
                q47 = table(q47)
                q48  = (as.numeric(q47) * ws3)/60 #converting to minutes
                keepindex_48 = c(fi,(fi+(length(q48)-1)))
                namesq47 = rep(0,length(rownames(q47)))
                for (rq47i in 1:length(rownames(q47))) {
                  namesq47[rq47i] = paste(rownames(q47)[rq47i],"_mg_",t_TWDI[1],"-",t_TWDI[2],"h",sep="")
                }
                daysummary[di,fi:(fi+(length(q48)-1))] = q48
                ds_names[fi:(fi+(length(q48)-1))] = paste(namesq47,"_",colnames(metashort)[mi],sep="")
                fi = fi+length(q48)
              }
              #=========================================
              # newly added on 28/02/2014
              if (domvpa == TRUE) {
                for (mvpai in 1:length(mvpathreshold)) {
                  mvpa = rep(0,6)
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
                  if (mvpa.2014 == TRUE) { # MVPA bout calculation like in the 2014 papers
                    # METHOD 4: time spent above threshold
                    boutdur2 = mvpadur[1] * (60/ws3) # per minute
                    rr1 = matrix(0,length(varnum),1)
                    p = which(varnum*1000 >= mvpathreshold[mvpai]); rr1[p] = 1
                    getboutout = g.getbout(rr=rr1,boutdur2=boutdur2,boutcriter=boutcriter,p=p,closedbout=closedbout)
                    mvpa[4] = length(which(getboutout$rr == 1))   / (60/ws3) #time spent MVPA in minutes
                    # METHOD 5: time spent above threshold 5 minutes
                    boutdur2 = mvpadur[2] * (60/ws3) #per five minutes
                    rr1 = matrix(0,length(varnum),1)
                    p = which(varnum*1000 >= mvpathreshold[mvpai]); rr1[p] = 1
                    getboutout = g.getbout(rr=rr1,boutdur2=boutdur2,boutcriter=boutcriter,p=p,closedbout=closedbout)
                    mvpa[5] = length(which(getboutout$rr == 1))   / (60/ws3) #time spent MVPA in minutes
                    # METHOD 6: time spent above threshold 10 minutes
                    boutdur2 = mvpadur[3] * (60/ws3) # per ten minutes
                    rr1 = matrix(0,length(varnum),1)
                    p = which(varnum*1000 >= mvpathreshold[mvpai]); rr1[p] = 1
                    getboutout = g.getbout(rr=rr1,boutdur2=boutdur2,boutcriter=boutcriter,p=p,closedbout=closedbout)
                    mvpa[6] = length(which(getboutout$rr == 1))   / (60/ws3) #time spent MVPA in minutes
                    if (length(which(varnum*1000 >= mvpathreshold[mvpai])) < 0 & length(varnum) < 100) {
                      mvpa[1:6] = 0
                    }
                    
                  } else { # updated version
                    cat("\nWARNING: MVPA Bout defintion has been updated, please see document for more information")
                    cat("\nincluding instructions on how to continue using the old defintion\n")
                    # METHOD 4: time spent above threshold
                    boutdur2 = 60/ws3 # per minute
                    rr1 = matrix(0,length(varnum),1)
                    p = which(varnum*1000 >= mvpathreshold[mvpai])
                    rr1[p] = 1
                    rr1t = rr1
                    jmvpa = 1
                    while(jmvpa <= length(p)) {
                      endi = p[jmvpa]+boutdur2
                      if (endi <= length(rr1)) { #does bout fall without measurement?
                        lengthbout = sum(rr1[p[jmvpa]:endi])
                        if (lengthbout > (boutdur2*boutcriter)) {
                          rr1t[p[jmvpa]:endi] = 2 #remember that this was a bout in r1t
                        } else {
                          rr1[p[jmvpa]] = 0
                        }    	
                      } else { #bout does not fall within measurement
                        if (length(p) > 1 & jmvpa > 2) {
                          rr1[p[jmvpa]] = rr1[p[jmvpa-1]]
                        }				
                      }
                      jmvpa = jmvpa + 1
                    }
                    rr1[which(rr1t == 2)] = 1
                    mvpa4 = length(which(rr1 == 1))   / (60/ws3) #time spent MVPA in minutes
                    # METHOD 5: time spent above threshold 5 minutes
                    boutdur2 = 5 * (60/ws3)
                    rr1 = matrix(0,length(varnum),1)
                    p = which(varnum*1000 >= mvpathreshold[mvpai])
                    rr1[p] = 1
                    rr1t = rr1
                    jmvpa = 1
                    while(jmvpa <= length(p)) {
                      endi = p[jmvpa]+boutdur2
                      if (endi <= length(rr1)) { #does bout fall without measurement?
                        lengthbout = sum(rr1[p[jmvpa]:endi])
                        if (lengthbout > (boutdur2*boutcriter)) { #0.9 => 90% of the bout needs to meet the criteria
                          rr1t[p[jmvpa]:endi] = 2 #remember that this was a bout in r1t
                        } else {
                          rr1[p[jmvpa]] = 0
                        }    	
                      } else { #bout does not fall within measurement
                        if (length(p) > 1 & jmvpa > 2) {
                          rr1[p[jmvpa]] = rr1[p[jmvpa-1]]
                        }				
                      }
                      jmvpa = jmvpa + 1
                    }
                    rr1[which(rr1t == 2)] = 1
                    mvpa5 = length(which(rr1 == 1))   / (60/ws3) #time spent MVPA in minutes
                    # METHOD 6: time spent above threshold 10 minutes
                    boutdur2 = 10 * (60/ws3) 
                    rr1 = matrix(0,length(varnum),1)
                    p = which(varnum*1000 >= mvpathreshold[mvpai])
                    rr1[p] = 1
                    rr1t = rr1
                    jmvpa = 1
                    while(jmvpa <= length(p)) {
                      endi = p[jmvpa]+boutdur2
                      if (endi <= length(rr1)) { #does bout fall without measurement?
                        lengthbout = sum(rr1[p[jmvpa]:endi])
                        if (lengthbout > (boutdur2*boutcriter)) { #0.9 => 90% of the bout needs to meet the criteria
                          rr1t[p[jmvpa]:endi] = 2 #remember that this was a bout in r1t
                        } else {
                          rr1[p[jmvpa]] = 0
                        }      
                      } else { #bout does not fall within measurement
                        if (length(p) > 1 & jmvpa > 2) {
                          rr1[p[jmvpa]] = rr1[p[jmvpa-1]]
                        }				
                      }
                      jmvpa = jmvpa + 1
                    }
                    rr1[which(rr1t == 2)] = 1
                    mvpa6 = length(which(rr1 == 1))   / (60/ws3) #time spent MVPA in minutes
                  }
                  
                  
                  
                  
                  if (length(which(is.nan(mvpa) == TRUE)) > 0) mvpa[which(is.nan(mvpa) == TRUE)] = 0
                  mvpanames[,mvpai] = c( paste("MVPA_E",ws3,"S_T",mvpathreshold[mvpai],sep=""),
                                         paste("MVPA_E1M_T",mvpathreshold[mvpai],sep=""),
                                         paste("MVPA_E5M_T",mvpathreshold[mvpai],sep=""),
                                         paste("MVPA_E",ws3,"S_B",mvpadur[1],"M",(boutcriter * 100),"%_T",mvpathreshold[mvpai],sep=""),
                                         paste("MVPA_E",ws3,"S_B",mvpadur[2],"M",(boutcriter * 100),"%_T",mvpathreshold[mvpai],sep=""),
                                         paste("MVPA_E",ws3,"S_B",mvpadur[3],"M",(boutcriter * 100),"%_T",mvpathreshold[mvpai],sep=""))
                  daysummary[di,fi] = mvpa[1];  ds_names[fi] = paste(mvpanames[1,mvpai],"_",colnames(metashort)[mi],sep=""); fi=fi+1
                  daysummary[di,fi] = mvpa[2];  ds_names[fi] = paste(mvpanames[2,mvpai],"_",colnames(metashort)[mi],sep=""); fi=fi+1
                  daysummary[di,fi] = mvpa[3];  ds_names[fi] = paste(mvpanames[3,mvpai],"_",colnames(metashort)[mi],sep=""); fi=fi+1
                  daysummary[di,fi] = mvpa[4];  ds_names[fi] = paste(mvpanames[4,mvpai],"_",colnames(metashort)[mi],sep=""); fi=fi+1
                  daysummary[di,fi] = mvpa[5];  ds_names[fi] = paste(mvpanames[5,mvpai],"_",colnames(metashort)[mi],sep=""); fi=fi+1
                  daysummary[di,fi] = mvpa[6];  ds_names[fi] = paste(mvpanames[6,mvpai],"_",colnames(metashort)[mi],sep=""); fi=fi+1
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
    summary[vi] = unlist(strsplit(fname,"_"))[1] #id
  } else if (idloc == 4) {
    summary[vi] = idd
  } else if (idloc == 1) {
    summary[vi] = id
  } else if (idloc == 3) {
    summary[vi] = id2
  }
  #-----------------------------------
  if (snloc == 1) {
    summary[(vi+1)] = SN
  } else if (snloc == 2) {
    summary[(vi+1)] = unlist(strsplit(fname,"_"))[2]
  }
  s_names[vi:(vi+1)] = c("ID","device_sn")
  vi = vi+2
  #-----------------------------------
  summary[vi] = BL
  summary[(vi+1)] = fname
  summary[(vi+2)] = startt # starttime of measurement
  s_names[vi:(vi+2)] = c("bodylocation","filename","start_time")
  vi = vi+3
  #-----------------------------------
  summary[vi] = wdayname # weekday on which measurement started
  summary[(vi+1)] = I$sf
  summary[(vi+2)] = I$monn
  s_names[vi:(vi+2)] = c("startday","samplefreq","device")
  vi = vi+3
  #-----------------------------------
  summary[vi] = LC2  / ((LD/1440)*96)
  summary[(vi+1)] = LD/1440 #measurement duration in days
  s_names[vi:(vi+1)] = c("clipping_score", #paste("number of ",ws2/60," minute time windows with potential signal clipping (> 80% of time > 7.5 g)",sep="") divided by number of 15 minute periods
                         "meas_dur_dys")
  vi = vi+2
  #-----------------------------------
  summary[vi] = dcomplscore #completeness of the day
  summary[(vi+1)] = LMp/1440 #measurement duration according to protocol
  summary[(vi+2)] = LWp/1440 #wear duration in days (out of measurement protocol)
  s_names[vi:(vi+2)] = c("complete_24hcycle", # day (fraction of 24 hours for which data is available at all)
                         "meas_dur_def_proto_day","wear_dur_def_proto_day")
  vi = vi+3
  #-----------------------------------
  if (length(C$cal.error.end) == 0)   C$cal.error.end = c(" ")
  summary[vi] = C$cal.error.end #CALIBRATE
  summary[vi+1] = C$QCmessage #CALIBRATE
  for (la in 1:length(lookat)) {
#     if (colnames(metashort)[lookat[la]] != "angle" & colnames(metashort)[lookat[la]] != "anglez" &
#           colnames(metashort)[lookat[la]] != "angley" & 
#           colnames(metashort)[lookat[la]] != "anglex") {
      MA[la] = 	MA[la] * 1000
#     }
  }
  q0 = length(MA) + 1
  summary[(vi+2):(vi+q0)] = MA
  s_names[vi:(vi+q0)] = c("calib_err",
                          "calib_status",colnames_to_lookat) #colnames(metashort)[2:ncol(metashort)]
  vi = vi+q0+2
  #---------------------------------------
  if (doquan == TRUE) {
    q1 = length(QUAN)
    summary[vi:((vi-1)+q1)] = QUAN*1000
    s_names[vi:((vi-1)+q1)] = qlevels_names
    vi = vi + q1
    q1 = length(ML5AD)
    summary[vi:((vi-1)+q1)] = ML5AD
    s_names[vi:((vi-1)+q1)] = ML5AD_names
    vi = vi + q1
  }
  #---------------------------------------
  if (tooshort == 0) {
    #--------------------------------------------------------------
    # expand with extracted values from features per day: do this for ENMO and activity levels
    wkend  = which(daysummary[,7] == "Saturday" | daysummary[,7] == "Sunday")
    v1 = which(is.na(as.numeric(daysummary[wkend,10])) == F)
    wkend = wkend[v1]
    wkday  = which(daysummary[,7] != "Saturday" & daysummary[,7] != "Sunday")
    v2 = which(is.na(as.numeric(daysummary[wkday,10])) == F)	
    wkday = wkday[v2]
    summary[vi] = length(wkend) # number of weekend days
    if (is.na(summary[vi]) == TRUE) summary[vi] = 0
    summary[(vi+1)] = length(wkday) # number of week day
    if (is.na(summary[(vi+1)]) == TRUE) summary[(vi+1)] = 0
    s_names[vi:(vi+1)] = c("N valid WEdays","N valid WKdays")
    vi = vi + 2
    #############################################################
    #metrics - summarise with stratification to weekdays and weekend days
    indeces = c()
    if (length(mvpathreshold) > 0) {
      for (mvpai in 1:length(mvpathreshold)) {
        #if mvpa is not done then this will look in the dummy variable and replace by c()
        for (mi in 2:ncol(metashort)) {
          if (mi == ENMOi | mi == LFENMOi | mi == BFENi | mi == ENi | mi == HFENi | mi == HFENplusi) {
            indeces = c(indeces,which(ds_names == paste(mvpanames[1,mvpai],"_",colnames(metashort)[mi],sep="")),
                        which(ds_names == paste(mvpanames[2,mvpai],"_",colnames(metashort)[mi],sep="")),
                        which(ds_names == paste(mvpanames[3,mvpai],"_",colnames(metashort)[mi],sep="")),
                        which(ds_names == paste(mvpanames[4,mvpai],"_",colnames(metashort)[mi],sep="")),
                        which(ds_names == paste(mvpanames[5,mvpai],"_",colnames(metashort)[mi],sep="")),
                        which(ds_names == paste(mvpanames[6,mvpai],"_",colnames(metashort)[mi],sep="")),
                        which(ds_names == paste("L5 ",colnames(metashort)[mi]," (mg)",sep="")),
                        which(ds_names == paste("M5 ",colnames(metashort)[mi]," (mg)",sep="")),
                        which(ds_names == paste("mean_",colnames(metashort)[mi],"_mg_1to6am",sep="")))
          }
        }
      }
    }
    daytoweekvar = c(indeces,
#                      which(ds_names == "mean_ENMO_mg_1to6am"),
                     which(ds_names == "mean_ENMO_mg_24hr"),
                     which(ds_names == "mean_LFENMO_mg_24hr"),
                     which(ds_names == "mean_BFEN_mg_24hr"),
                     which(ds_names == "mean_EN_mg_24hr"),
                     which(ds_names == "mean_HFEN_mg_24hr"),
                     which(ds_names == "mean_HFENplus_mg_24hr")
    )
    dtwtel = 0
    if (length(daytoweekvar) >= 1) {
      sp = length(daytoweekvar) + 1
      for (dtwi in daytoweekvar) {
        v4 = mean(as.numeric(daysummary[,dtwi]),na.rm=TRUE) #plain average of available days
        summary[(vi+1+(dtwtel*sp))] = v4 # #average all availabel days
        s_names[(vi+1+(dtwtel*sp))] = paste("AD_",ds_names[daytoweekvar[dtwtel+1]],sep="") 
        #========================================================================
        # attempt to stratify to week and weekend days
        # Average of available days
        dtw_wkday = as.numeric(daysummary[wkday,dtwi])
        dtw_wkend = as.numeric(daysummary[wkend,dtwi])
        v1 = mean(dtw_wkend,na.rm=TRUE)
        v2 = mean(dtw_wkday,na.rm=TRUE)
        summary[(vi+2+(dtwtel*sp))] = v1 # #weekend average
        summary[(vi+3+(dtwtel*sp))] = v2 # #weekday average
        s_names[(vi+2+(dtwtel*sp))] = paste("WE_",ds_names[daytoweekvar[dtwtel+1]],sep="") #Weekend no weighting
        s_names[(vi+3+(dtwtel*sp))] = paste("WD_",ds_names[daytoweekvar[dtwtel+1]],sep="") #weekdays no weighting
        #==================================================
        # Weighted average of available days
        if (length(dtw_wkend) > 2) {
          dtw_wkend = c((dtw_wkend[1]+dtw_wkend[3])/2,dtw_wkend[2])
        }
        if (length(dtw_wkday) > 5) {
          dtw_wkday = c((dtw_wkday[1]+dtw_wkday[6])/2,dtw_wkday[2:5])
        }
        v1 = mean(dtw_wkend,na.rm=TRUE)
        v2 = mean(dtw_wkday,na.rm=TRUE)
        summary[(vi+4+(dtwtel*sp))] = v1 # #weekend average
        summary[(vi+5+(dtwtel*sp))] = v2 # #weekday average
        s_names[(vi+4+(dtwtel*sp))] = paste("WWE_",ds_names[daytoweekvar[dtwtel+1]],sep="")
        s_names[(vi+5+(dtwtel*sp))] = paste("WWD_",ds_names[daytoweekvar[dtwtel+1]],sep="")
        dtwtel = dtwtel + 1
      }
      vi = vi+6+((dtwtel*sp)-1)
      #===========================================================================
      # SUMMARISE Percentiles (q46)
      if (doquan == TRUE) {
        if (length(q46) > 0) {
          for (ki46 in keepindex_46[1]:keepindex_46[2]) {
            v4 = mean(as.numeric(daysummary[,ki46]),na.rm=TRUE) #plain average of available days
            summary[vi] = v4 # #average all availabel days
            s_names[vi] = paste("AD_",ds_names[ki46],sep="") 
            vi = vi + 1
          }
          for (ki46 in keepindex_46[1]:keepindex_46[2]) {
            dtw_wkend = as.numeric(daysummary[wkend,ki46])
            v1 = mean(dtw_wkend,na.rm=TRUE)
            summary[vi] = v1 # #weekend average
            s_names[vi] = paste("WE_",ds_names[ki46],sep="")
            vi = vi + 1
          }
          for (ki46 in keepindex_46[1]:keepindex_46[2]) {
            dtw_wkday = as.numeric(daysummary[wkday,ki46])
            v2 = mean(dtw_wkday,na.rm=TRUE)
            summary[vi] = v2 # #weekday average
            s_names[vi] = paste("WD_",ds_names[ki46],sep="") 
            vi = vi + 1
          }
          # Weighted average of available days
          for (ki46 in keepindex_46[1]:keepindex_46[2]) {
            dtw_wkend = as.numeric(daysummary[wkend,ki46])
            if (length(dtw_wkend) > 2) {
              dtw_wkend = c((dtw_wkend[1]+dtw_wkend[3])/2,dtw_wkend[2])
            }
            v1 = mean(dtw_wkend,na.rm=TRUE)
            summary[vi] = v1 # #weekend average
            s_names[vi] = paste("WWE_",ds_names[ki46],sep="") 
            vi = vi + 1
          }
          for (ki46 in keepindex_46[1]:keepindex_46[2]) {
            dtw_wkday = as.numeric(daysummary[wkday,ki46])
            if (length(dtw_wkday) > 5) {
              dtw_wkday = c((dtw_wkday[1]+dtw_wkday[6])/2,dtw_wkday[2:5])
            }
            v2 = mean(dtw_wkday,na.rm=TRUE)
            summary[vi] = v2 # #weekday average
            s_names[vi] = paste("WWD_",ds_names[ki46],sep="")
            vi = vi+1
          }
        }
      }
      #======================================================
      # SUMMARISE acceleration distribution(q48)
      if (doilevels == TRUE) {
        if (length(q48) > 0) {
          for (ki48 in keepindex_48[1]:keepindex_48[2]) {
            v4 = mean(as.numeric(daysummary[,ki48]),na.rm=TRUE) #plain average of available days
            summary[vi] = v4 # #average all availabel days
            s_names[vi] = paste("AD_",ds_names[ki48],sep="") 
            vi = vi + 1
          }
          for (ki48 in keepindex_48[1]:keepindex_48[2]) {
            dtw_wkend = as.numeric(daysummary[wkend,ki48])
            v1 = mean(dtw_wkend,na.rm=TRUE)
            summary[vi] = v1 # #weekend average
            s_names[vi] = paste("WE_",ds_names[ki48],sep="")
            vi = vi + 1
          }
          for (ki48 in keepindex_48[1]:keepindex_48[2]) {
            dtw_wkday = as.numeric(daysummary[wkday,ki48])
            v2 = mean(dtw_wkday,na.rm=TRUE)
            summary[vi] = v2 # #weekday average
            s_names[vi] = paste("WD_",ds_names[ki48],sep="")
            vi = vi + 1
          }
          # Weighted average of available days
          for (ki48 in keepindex_48[1]:keepindex_48[2]) {
            dtw_wkend = as.numeric(daysummary[wkend,ki48])
            if (length(dtw_wkend) > 2) {
              dtw_wkend = c((dtw_wkend[1]+dtw_wkend[3])/2,dtw_wkend[2])
            }
            v1 = mean(dtw_wkend,na.rm=TRUE)
            summary[vi] = v1 # #weekend average
            s_names[vi] = paste("WWE_",ds_names[ki48],sep="")
            vi = vi + 1
          }
          for (ki48 in keepindex_48[1]:keepindex_48[2]) {
            dtw_wkday = as.numeric(daysummary[wkday,ki48])
            if (length(dtw_wkday) > 5) {
              dtw_wkday = c((dtw_wkday[1]+dtw_wkday[6])/2,dtw_wkday[2:5])
            }
            v2 = mean(dtw_wkday,na.rm=TRUE)
            summary[vi] = v2 # #weekday average
            s_names[vi] = paste("WWD_",ds_names[ki48],sep="") 
            vi = vi+1
          }
        }
      }
    }
    summary[vi] = strategy 
    summary[(vi+1)] = hrs.del.start
    summary[(vi+2)] = hrs.del.end
    summary[(vi+3)] = maxdur
    summary[(vi+4)] = windowsizes[1]
    #get GGIR version
    SI = sessionInfo()
    GGIRversion = c()
    GGIRversion = SI$otherPkgs$GGIR$Version
    if (length(GGIRversion) == 0) GGIRversion = "GGIR not used"
    summary[(vi+5)] = GGIRversion #"2014-03-14 12:14:00 GMT"
    s_names[vi:(vi+5)] = c("data exclusion stategy (value=1, ignore specific hours; value=2, ignore all data before the first midnight and after the last midnight)",
                           "n hours ignored at start of meas (if strategy=1)",
                           "n hours ignored at end of meas (if strategy=1)",
                           "n days of measurement after which all data is ignored (if strategy=1)",
                           "epoch size to which acceleration was averaged (seconds)","GGIR version")
    vi = vi + 5
  }
  #---------------------------------------------------------------
  rm(metashort); rm(LD); rm(LW); rm(HN); rm(id); rm(metalong)
  #------------------------
  mw = which(is.na(daysummary)==T)
  if (length(mw) > 0) {
    daysummary[which(is.na(daysummary)==T)] = " "
  }
  cut = which(ds_names == " ")
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
  #--------------
  mw = which(is.na(summary)==T)
  if (length(mw) > 0) {
    summary[which(is.na(summary)==T)] = " "
  }
  cut = which(as.character(s_names) == " " | is.na(s_names)==T)
  if (length(cut) > 0) {
    s_names = s_names[-cut]
    summary = summary[-cut]
  }
  summary = data.frame(value=t(summary),stringsAsFactors=FALSE) #needs to be t() because it will be a column otherwise
  names(summary) = s_names
  if (length(selectdaysfile) > 0) {
    windowsummary = data.frame(windowsummary,stringsAsFactors = FALSE) # addition for Millenium cohort
    names(windowsummary) = ws_names
    invisible(list(summary=summary,daysummary=daysummary,windowsummary=windowsummary))
  } else {
    invisible(list(summary=summary,daysummary=daysummary))
  }
}