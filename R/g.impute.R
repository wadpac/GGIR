g.impute = function(M,I,strategy=1,hrs.del.start=0,hrs.del.end=0,maxdur=0,
                    ndayswindow = 7,desiredtz="", dayborder = 0, TimeSegments2Zero=c()) {
  windowsizes = M$windowsizes #c(5,900,3600)
  metashort = M$metashort
  metalong = M$metalong
  ws3 = windowsizes[1]
  ws2 = windowsizes[2]
  # What is the minimum number of accelerometer axis needed to meet the criteria for nonwear in order for the data to be detected as nonwear?
  wearthreshold = 2 #needs to be 0, 1 or 2
  n_ws2_perday = (1440*60) / ws2
  n_ws3_perday = (1440*60) / ws3
  #check that matrices match
  if (((nrow(metalong)/((1440*60)/ws2)*10) - (nrow(metashort)/((60/ws3)*1440)) * 10) > 1) {
    print("Matrices 'metalong' and 'metashort' are not compatible")
  }  
  tmi = which(colnames(metalong) == "timestamp")
  time = as.character(as.matrix(metalong[,tmi]))
  startt = as.matrix(metalong[1,tmi])
  #====================================
  # Deriving file characteristics from 15 min summary files
  LD = nrow(metalong) * (ws2/60) #length data in minutes
  ND = nrow(metalong)/n_ws2_perday #number of days
  #==============================================
  # Generating time variable
  timeline = seq(0,ceiling(nrow(metalong)/n_ws2_perday),by=1/n_ws2_perday)	
  timeline = timeline[1:nrow(metalong)]
  
  #=================================
  # TO DO: issue 199,
  # Check whether object M has a column external-non-wear
  # If yes, then use that instead of GGIR-based non-wear, unless external-nonwear = FALSE (default).
  
  #========================================
  # Extracting non-wear and clipping and make decision on which additional time needs to be considered non-wear
  out = g.weardec(M,wearthreshold,ws2)
  r1 = out$r1 #non-wear
  r2 = out$r2 #clipping
  r3 = out$r3 #additional non-wear
  r4 = matrix(0,length(r3),1) #protocol based decisions on data removal
  LC = out$LC
  LC2 = out$LC2
  
  #========================================================
  # Check whether TimeSegments2Zero exist, because this means that the
  # user wants to ignore specific time windows. This feature is used
  # for example if the accelerometer was not worn during the night and the user wants
  # to include the nighttime acceleration in the analeses without imputation,
  # but wants to use imputation for the rest of the day.
  # So, those time windows should not be imputed.
  # and acceleration metrics should have value zero during these windows.
  if (length(TimeSegments2Zero) > 0) {
    r1long = matrix(0,length(r1),(ws2/ws3)) #r5long is the same as r5, but with more values per period of time
    r1long = replace(r1long,1:length(r1long),r1)
    r1long = t(r1long)
    dim(r1long) = c((length(r1)*(ws2/ws3)),1)
    timelinePOSIX = iso8601chartime2POSIX(M$metashort$timestamp,tz= desiredtz)
    # Combine r1Long with TimeSegments2Zero
    for (kli in 1:nrow(TimeSegments2Zero)) {
      startTurnZero = which(timelinePOSIX == TimeSegments2Zero$windowstart[kli])
      endTurnZero = which(timelinePOSIX == TimeSegments2Zero$windowend[kli])
      r1long[startTurnZero:endTurnZero] = 0
      # Force ENMO and other acceleration metrics to be zero for these intervals
      M$metashort[startTurnZero:endTurnZero,which(colnames(M$metahosrt) %in% c("timestamp","anglex","angley","anglez") == FALSE)] = 0
    }
    # collaps r1long (short epochs) back to r1 (long epochs)
    r1longc = cumsum(c(0,r1long))
    select = seq(1,length(r1longc),by=(ws2/ws3))
    r1 = diff(r1longc[round(select)]) / abs(diff(round(select)))
    r1 = round(r1)
  }

  #======================================
  # detect first and last midnight and all midnights
  tooshort = 0
  dmidn = g.detecmidnight(time,desiredtz,dayborder) #,ND
  firstmidnight=dmidn$firstmidnight;  firstmidnighti=dmidn$firstmidnighti
  lastmidnight=dmidn$lastmidnight;    lastmidnighti=dmidn$lastmidnighti
  midnights=dmidn$midnights;          midnightsi=dmidn$midnightsi
  #===================================================================
  # Select data based on strategy
  if (strategy == 1) { 	#protocol based data selection
    if (hrs.del.start > 0) {
      r4[1:(hrs.del.start*(3600/ws2))] = 1
    }
    if (hrs.del.end > 0) {
      if (length(r4) > hrs.del.end*(3600/ws2)) {
        r4[((length(r4)+1)-(hrs.del.end*(3600/ws2))):length(r4)] = 1
      } else {
        r4[1:length(r4)] = 1
      }
    }
    if (maxdur > 0 & (length(r4) > ((maxdur*n_ws2_perday)+1))) {
      r4[((maxdur*n_ws2_perday)+1):length(r4)] = 1
    }
    if (LD < 1440){
      r4 = r4[1:floor(LD/(ws2/60))]
    }
    starttimei = 1
    endtimei = length(r4)
  } else if (strategy == 2) { #midnight to midnight strategy
    starttime = firstmidnight
    endtime = lastmidnight
    starttimei = firstmidnighti
    endtimei = lastmidnighti
    if (firstmidnighti != 1) { #ignore everything before the first midnight
      r4[1:(firstmidnighti-1)] = 1 #-1 because first midnight 00:00 itself contributes to the first full day
    }
    r4[(lastmidnighti):length(r4)] = 1  #ignore everything after the last midnight
  } else if (strategy == 3) { #select X most active days
    #==========================================
    # Look out for X most active days and use this to define window of interest
    atest = as.numeric(as.matrix(M$metashort[,2]))
    ws3 = M$windowsizes[1]
    ws2 = M$windowsizes[2]
    r2tempe = rep(r2,each=(ws2/ws3))
    atest[which(r2tempe == 1)] = 0
    NDAYS = length(atest) / (12*60*24)
    pend = round((NDAYS - ndayswindow) * 4)
    if (pend < 1) pend = 1
    atestlist = rep(0,pend)
    for (ati in 1:pend) { #40 x quarter a day
      p0 = (((ati-1)*12*60*6)+1)
      p1 = (ati+(ndayswindow*4))*12*60*6  #ndayswindow x quarter of a day = 1 week
      if (p0 > length(atest)) p0 = length(atest)
      if (p1 > length(atest)) p1 = length(atest)
      if ((p1 - p0) > 1000) {
        atestlist[ati] = mean(atest[p0:p1],na.rm=TRUE)
      } else {
        # print("zero")
        atestlist[ati] = 0
      }
    }
    atik = which(atestlist == max(atestlist))
    hrs.del.start = atik * 6
    maxdur = (atik/4) +ndayswindow
    if (maxdur > NDAYS) maxdur = NDAYS
    # now calculate r4    
    if (hrs.del.start > 0) {
      r4[1:(hrs.del.start*(3600/ws2))] = 1
    }
    if (hrs.del.end > 0) {
      if (length(r4) > hrs.del.end*(3600/ws2)) {
        r4[((length(r4)+1)-(hrs.del.end*(3600/ws2))):length(r4)] = 1
      } else {
        r4[1:length(r4)] = 1
      }
    }
    if (maxdur > 0 & (length(r4) > ((maxdur*n_ws2_perday)+1))) {
      r4[((maxdur*n_ws2_perday)+1):length(r4)] = 1
    }
    if (LD < 1440){
      r4 = r4[1:floor(LD/(ws2/60))]
    }
    starttimei = 1
    endtimei = length(r4)
    
  } else if (strategy == 4) { #from first midnight to end of recording
    starttime = firstmidnight
    starttimei = firstmidnighti
    if (firstmidnighti != 1) { #ignore everything before the first midnight
      r4[1:(firstmidnighti-1)] = 1 #-1 because first midnight 00:00 itself contributes to the first full day
    }
  }
  
  #extracting calibration value during periods of non-wear
  if (length(which(r1==1)) > 0) {
    CALIBRATE = mean(as.numeric(metalong[which(r1==1 & r2 != 1),9])) #mean EN during non-wear time and non-clipping time
  } else {
    CALIBRATE = c()
    is.na(CALIBRATE) = T
  }
  #========================================================================================
  # Impute ws3 second data based on ws2 minute estimates of non-wear time
  r5 = r1 + r2 + r3 + r4
  r5[which(r5 > 1) ] = 1
  r5long = matrix(0,length(r5),(ws2/ws3)) #r5long is the same as r5, but with more values per period of time
  r5long = replace(r5long,1:length(r5long),r5)
  r5long = t(r5long)
  dim(r5long) = c((length(r5)*(ws2/ws3)),1)

  #------------------------------
  # detect which features have been calculated in part 1 and in what column they have ended up
  ENi = which(colnames(metashort) == "en")
  if (length(ENi) == 0) ENi = -1
  #==============================
  if (nrow(metashort) > length(r5long)) {
    metashort = as.matrix(metashort[1:length(r5long),])
  }
  wpd = 1440*(60/ws3) #windows per day
  averageday = matrix(0,wpd,(ncol(metashort)-1))
  
  for (mi in 2:ncol(metashort)) {# generate 'average' day for each variable
    # The average day is used for imputation and defined relative to the starttime of the measurement
    # irrespective of dayborder as used in other parts of GGIR
    metrimp = metr = as.numeric(as.matrix(metashort[,mi]))
    is.na(metr[which(r5long == 1)]) = T #turn all values of metr to na if r5long is 1
    imp = matrix(NA,wpd,ceiling(length(metr)/wpd)) #matrix used for imputation of seconds
    ndays = ncol(imp) #number of days (rounded upwards)
    nvalidsec = matrix(0,wpd,1)
    dcomplscore = length(which(r5 == 0)) / length(r5)
    if (ndays > 1 ) { # only do imputation if there is more than 1 day of data #& length(which(r5 == 1)) > 1
      for (j in 1:(ndays-1)) {
        imp[,j] = as.numeric(metr[(((j-1)*wpd)+1):(j*wpd)])
      }
      lastday = metr[(((ndays-1)*wpd)+1):length(metr)]
      imp[1:length(lastday),ndays] = as.numeric(lastday)
      imp3 = rowMeans(imp,na.rm=TRUE) 
      dcomplscore = length(which(is.nan(imp3) == F | is.na(imp3) == F)) / length(imp3)
      
      if (length(imp3) < wpd)  {
        dcomplscore = dcomplscore * (length(imp3)/wpd)
      }
      if (ENi == mi) { #replace missing values for EN by 1
        imp3[which(is.nan(imp3) == T | is.na(imp3) == T)] = 1
      } else { #replace missing values for other metrics by 0
        imp3[which(is.nan(imp3) == T | is.na(imp3) == T)] = 0 # for those part of the data where there is no single data point for a certain part of the day (this is CRITICAL)
      }
      averageday[,(mi-1)] = imp3
      for (j in 1:ndays) { 
        missing = which(is.na(imp[,j]) == T)
        if (length(missing) > 0) {
          imp[missing,j] = imp3[missing]
        }
      }
      dim(imp) = c(length(imp),1)
      #      imp = imp[-c(which(is.na(as.numeric(as.character(imp))) == T))] 
      metashort[,mi] = as.numeric(imp[1:nrow(metashort)]) #to cut off the latter part of the last day used as a dummy data
    } else {
      dcomplscore = length(which(r5long == 0))/wpd
    }
  }
  metashortimp = metashort
  rout = data.frame(r1=r1,r2=r2,r3=r3,r4=r4,r5=r5, stringsAsFactors = TRUE)
  invisible(list(metashort=metashortimp,rout=rout,dcomplscore=dcomplscore,averageday=averageday,windowsizes=windowsizes,strategy=strategy,
                 LC=LC,LC2=LC2,hrs.del.start=hrs.del.start,hrs.del.end=hrs.del.end,maxdur=maxdur))
}
