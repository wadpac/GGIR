g.analyse =  function(I,C,M,IMP,qlevels=c(),qwindow=c(0,24),quantiletype = 7,L5M5window = c(0,24),M5L5res=10,
                      includedaycrit = 16,ilevels=c(),winhr=5,idloc=1,snloc=1,
                      mvpathreshold = c(),boutcriter=c(),mvpadur=c(1,5,10),selectdaysfile=c(),
                      window.summary.size=10,
                      dayborder=0,bout.metric = 1,closedbout=FALSE,desiredtz="",
                      IVIS_windowsize_minutes = 60, IVIS_epochsize_seconds = 3600, iglevels = c(),
                      IVIS.activity.metric=1, qM5L5 = c(), myfun=c(), MX.ig.min.dur = 10) {
  L5M5window = c(0,24) # as of version 1.6-0 this is hardcoded because argument qwindow now
  # specifies the window over which L5M5 analysis is done. So, L5M5window is a depricated
  # argument and this is also clarified in the documentation
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
  keepindex_46 = keepindex_48 = c()
  # Extracting basic information about the file
  hvars = g.extractheadervars(I)
  ID = hvars$ID;              iID =hvars$iID; IDd =hvars$IDd
  HN = hvars$HN;              BodyLocation = hvars$BodyLocation
  SX=hvars$SX;                deviceSerialNumber = hvars$deviceSerialNumber
  n_ws2_perday = (1440*60) / ws2
  n_ws3_perday = (1440*60) / ws3
  if (((nrow(metalong)/((1440*60)/ws2)*10) - (nrow(metashort)/((60/ws3)*1440)) * 10) > 1) {
    cat("Matrices 'metalong' and 'metashort' are not compatible")
  }
  #----------------------
  # Pelotas specific
  ID2 = ID
  iID2 = iID
  if (idloc == 3) { #remove hyphen in id-name for Pelotas id-numbers
    get_char_before_hyphen = function(x) {
      for (j in 1:length(x)) {
        temp = unlist(strsplit(x,"-"))
        if (length(temp) == 2) {
          x2[j] = as.character(temp[1])
        } else {
          x2[j] = as.character(x[j])
        }
      }
      return(x2)
    }
    ID2 = get_char_before_hyphen(ID)
    iID2 = get_char_before_hyphen(iID)
  }
  # Extract qwindow if an activity log is provided:
  qwindow_actlog =FALSE
  if (is.data.frame(qwindow) == TRUE) {
    qwindow_actlog = TRUE
    if (idloc == 2) {
      IDt = unlist(strsplit(fname,"_"))[1] #id
    } else if (idloc == 5) {
      IDt = unlist(strsplit(fname," "))[1] #id
    } else if (idloc == 4) {
      IDt = IDd
    } else if (idloc == 1) {
      IDt = ID
    } else if (idloc == 3) {
      IDt = ID2
    }
    qwindow = qwindow[which(qwindow$ID == IDt),]
  }
  # # Time window for L5 & M5 analysis (commented out because this is now defined further down)
  # t0_LFMF = L5M5window[1] #start in 24 hour clock hours
  # t1_LFMF = L5M5window[2]+(winhr-(M5L5res/60)) #end in 24 hour clock hours (if a value higher than 24 is chosen, it will take early hours of previous day to complete the 5 hour window
  # Time window for distribution analysis
  t_TWDI = qwindow #start and of 24 hour clock hours
  if (length(qwindow) == 0) {
    t_TWDI = c(0,24)
    if ((length(qlevels) > 0 | length(ilevels) > 0)) qwindow = c(0,24)
    qwindow_actlog = FALSE # ignore qwdinow_actlog if it does not produce actual qwindow values
  }
  if (length(qwindow) > 0 & qwindow_actlog == FALSE) {
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
  NVARS = (length(colnames(metashort))-1)
  if (NVARS < 1) NVARS = 1
  if (length(qwindow) > 0 | qwindow_actlog == TRUE) NVARS = NVARS + 2 # for qwindow non-wear time
  nfeatures = 50+NVARS*(21+length(qlevels)+length(ilevels))    #levels changed into qlevels
  if (length(qwindow) > 0 | qwindow_actlog == TRUE) {
    nfeatures = 50+NVARS*(length(qwindow)*(21+(length(qlevels)+length(ilevels))))
  }
  i = 1
  #---------------
  if (domvpa) { #create dummy data
    mvpanames = matrix(0,6,length(mvpathreshold))
    mvpanames[,1:length(mvpathreshold)] = c("MVPA1","MVPA2","MVPA3","MVPA4","MVPA5","MVPA6")
  }
  # What is the minimum number of accelerometer axis needed to meet the criteria for nonwear in order for the data to be detected as nonwear?
  wearthreshold = 2 #needs to be 0, 1 or 2 (hard coded to avoid inconsistency in literature)
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
  dmidn = g.detecmidnight(time,desiredtz,dayborder) #ND,
  firstmidnight=dmidn$firstmidnight;  firstmidnighti=dmidn$firstmidnighti
  lastmidnight=dmidn$lastmidnight;    lastmidnighti=dmidn$lastmidnighti
  midnights=dmidn$midnights;          midnightsi=dmidn$midnightsi
  starttimei = 1
  endtimei = nrow(M$metalong)
  if (strategy == 2) {
    starttimei = firstmidnighti
    endtimei = lastmidnighti - 1
  }
  # get r5long - this is vector with the 5 second scores of what data needs to be imputed
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
  ENMOai = which(colnames(metashort) == "ENMOa")
  ANYANGLEi = which(colnames(M$metashort) %in% c("anglex","angley","anglez") ==  TRUE)
  if (length(myfun) > 0) {
    ExtFunColsi = which(colnames(M$metashort) %in% myfun$colnames ==  TRUE)
  } else {
    ExtFunColsi = c()
  }
  if (length(ANYANGLEi) == 0) ANYANGLEi = -1
  if (length(ENMOi) == 0) ENMOi = -1
  if (length(LFENMOi) == 0) LFENMOi = -1
  if (length(BFENi) == 0) BFENi = -1
  if (length(HFENi) == 0) HFENi = -1
  if (length(HFENplusi) == 0) HFENplusi = -1
  if (length(MADi) == 0) MADi = -1
  if (length(ENi) == 0) ENi = -1
  if (length(ENMOai) == 0) ENMOai = -1
  #===============================================
  # Extract features from the imputed data
  qcheck = r5long
  # LW = length(which(as.numeric(qcheck) != 1)) / (60/ws3) #number of minutes wear time between first and last midnights
  nfulldays = (lastmidnighti - firstmidnighti) / ((3600/ws2)*24)
  ndays = length(midnights) + 1 #ceiling(nfulldays + 2) # ceiling to cope with days with 23 hours
  if (ndays != round(ndays)) { #day saving time causing trouble?
    cat("One day in this measurement is longer or shorter than 24 hours (probably related to day saving time)")
  }
  #-------------------------------------
  # Detect orientation (at the moment only desighned for hip with accelerometer):
  # assess which angle per axis is most strongly 24 hour correlated:
  # for hip worn devices this will be the vertical axis
  longitudinal_axis_id = ""
  epochday = 24*6*(60/ws3)
  Ndays = floor(nrow(IMP$metashort)/epochday)
  if (length(which(c("anglex","angley","anglez") %in% colnames(IMP$metashort) == FALSE)) == 0 &
      Ndays >= 2) {
    CorrA = rep(0,3)
    cnt = 1
    for (anglename in  c("anglex","angley","anglez")) {
      CorrA[cnt] = stats::cor(IMP$metashort[1:((Ndays-1)*epochday), anglename],
                       IMP$metashort[(epochday+1):(Ndays*epochday), anglename])
      # # if 5th and 95th percentile do not differ by more than 45 degree than ignore this axis
      # (Commented out, because this was experimental, never used in a GGIR release)
      # if (abs(diff(quantile(IMP$metashort[, anglename], probs=c(0.05,0.95)))) < 45) {
      #   CorrA[cnt] = 0
      # }
      cnt = cnt + 1
    }
    longitudinal_axis_id = which.max(CorrA)
    # TO DO:
    # - Estimate main time in bed period per day
    # - Adjust qwindow accordingly, such that it indicates: daytime, nighttime
    
  }
  
  #--------------------------------------
  # Analysis of the average day
  # Derivation of distribution characteristics of the average day: quantiles (percentiles) and L5M5 method
  # Note that this is done here before all the other analyses because it only relies on the average day
  # The values and variablenames are, however, stored in the filesummary matrix towards the end (not here
  # in function g.analyse.avday).
  output_avday = g.analyse.avday(qlevels,doquan, averageday, M, IMP, t_TWDI, quantiletype, winhr, L5M5window, M5L5res,
                                 ws3, IVIS_epochsize_seconds,
                                 IVIS_windowsize_minutes, IVIS.activity.metric, doiglevels, firstmidnighti, ws2,
                                 midnightsi, iglevels, qM5L5, MX.ig.min.dur=MX.ig.min.dur)
  InterdailyStability = output_avday$InterdailyStability
  IntradailyVariability = output_avday$IntradailyVariability
  igfullr_names = output_avday$igfullr_names
  igfullr = output_avday$igfullr
  QUAN = output_avday$QUAN
  qlevels_names = output_avday$qlevels_names
  ML5AD=output_avday$ML5AD
  ML5AD_names = output_avday$ML5AD_names
  #--------------------------------------------------------------
  # Analysis per day
  if (doperday == TRUE) {
    output_perday = g.analyse.perday(selectdaysfile, ndays, firstmidnighti, time, nfeatures,
                                     window.summary.size, qwindow, midnightsi, metashort, averageday,
                                     ENMOi, LFENMOi, BFENi, ENi,
                                     HFENi, HFENplusi, MADi,  ENMOai, doiglevels, nfulldays, lastmidnight,
                                     ws3, ws2, qcheck, fname, idloc, BodyLocation, wdayname,
                                     tooshort, includedaycrit, winhr,L5M5window, M5L5res,
                                     doquan, qlevels, quantiletype, doilevels, ilevels, iglevels, domvpa,
                                     mvpathreshold, boutcriter, closedbout,
                                     bout.metric, mvpadur, mvpanames, wdaycode, IDd, ID, ID2,
                                     deviceSerialNumber, qM5L5, ExtFunColsi, myfun, desiredtz, MX.ig.min.dur)
    daysummary= output_perday$daysummary
    ds_names=output_perday$ds_names
    windowsummary=output_perday$windowsummary
    ws_names=output_perday$ws_names
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
  # LW = length(which(r5 < 1)) * (ws2/60) #length wear in minutes (for entire signal)
  LWp = length(which(r5[which(r4 == 0)] < 1)) * (ws2/60) #length wear in minutes (for protocol)
  LMp = length(which(r4 == 0)) * (ws2/60) #length protocol
  #====================================================================
  # Analysis per recording (entire file), and merge in average day analysis results
  #====================================================================
  # Extract the average 24 hr but ignore angle metrics
  lookattmp = which(colnames(metashort) %in% c("angle","anglex", "angley", "anglez") ==  FALSE)
  lookat = lookattmp[which(lookattmp > 1)] #]c(2:ncol(metashort[,lookattmp]))
  colnames_to_lookat = colnames(metashort)[lookat]
  AveAccAve24hr = matrix(NA,length(lookat),1)
  if (length(which(r5 == 0)) > 0) { #to catch strategy 2 with only 1 midnight and other cases where there is no valid data
    for (h in 1:length(lookat)) {
      average24h = matrix(0,n_ws3_perday,1)
      average24hc = matrix(0,n_ws3_perday,1)
      if (floor(ND) != 0) {
        for (j in 1:floor(ND)) {
          dataOneDay = as.numeric(as.matrix(metashort[(((j-1)*n_ws3_perday)+1):(j*n_ws3_perday),lookat[h]]))
          val = which(is.na(dataOneDay) == F)
          average24h[val,1] = average24h[val,1] + dataOneDay[val] #mean acceleration
          average24hc[val,1] = average24hc[val,1] +1
        }
      }
      if (floor(ND) < ND) {
        if (floor(ND) == 0) {
          dataOneDay = as.numeric(as.matrix(metashort[,lookat[h]]))
        } else {
          dataOneDay = as.numeric(as.matrix(metashort[((floor(ND)*n_ws3_perday)+1):nrow(metashort),lookat[h]]))
        }
        val = which(is.na(dataOneDay) == F)
        average24h[val,1] = average24h[val,1] + dataOneDay[val]  #mean acceleration
        average24hc[val,1] = average24hc[val,1] +1
      }
      average24h = average24h / average24hc
      AveAccAve24hr[h] = mean(average24h) #average acceleration in an average 24 hour cycle
    }
  } else {
    cat("file skipped for general average caculation because not enough data")
  }
  rm(metalong); rm(metashort)
  ID[which(ID == "NA")] =iID[which(ID == "NA")]
  ID2[which(ID2 == "NA")] =iID2[which(ID2 == "NA")]
  output_perfile = g.analyse.perfile(ID, ID2, IDd, fname, deviceSerialNumber, BodyLocation, startt, I, LC2, LD, dcomplscore,
                                     LMp, LWp, C, lookat, AveAccAve24hr, colnames_to_lookat, QUAN, ML5AD,
                                     ML5AD_names, igfullr, igfullr_names,
                                     daysummary, ds_names, includedaycrit, strategy, hrs.del.start,
                                     hrs.del.end, maxdur, windowsizes, idloc, snloc, wdayname, doquan,
                                     qlevels_names, doiglevels, tooshort, InterdailyStability, IntradailyVariability,
                                     IVIS_windowsize_minutes, IVIS_epochsize_seconds,qwindow, longitudinal_axis_id)
  filesummary = output_perfile$filesummary
  daysummary = output_perfile$daysummary
  if (length(selectdaysfile) > 0) {
    windowsummary = data.frame(windowsummary,stringsAsFactors = FALSE) # addition for Millenium cohort
    names(windowsummary) = ws_names
    invisible(list(summary=filesummary,daysummary=daysummary,windowsummary=windowsummary))
  } else {
    invisible(list(summary=filesummary,daysummary=daysummary))
  }
}
