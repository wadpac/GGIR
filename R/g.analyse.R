g.analyse =  function(I, C, M, IMP, params_247 = c(), params_phyact = c(),
                      params_general = c(), params_cleaning = c(),
                      quantiletype = 7, myfun = c(), ID, ...) {
  
  #get input variables
  input = list(...)
  if (length(input) > 0 ||
      length(params_247) == 0 || length(params_phyact) == 0 ||
      length(params_general) == 0 || length(params_cleaning) == 0) {
    # Extract and check parameters if user provides more arguments than just the parameter arguments,
    # or if params_[...] aren't specified (so need to be filled with defaults).
    # So, inside GGIR this will not be used, but it is used when g.analyse is used on its own
    # as if it was still the old g.analyse function
    params = extract_params(params_247 = params_247,
                            params_phyact = params_phyact,
                            params_general = params_general,
                            params_cleaning = params_cleaning,
                            input = input,
                            params2check = c("247", "phyact", "general", "cleaning")) # load default parameters
    params_247 = params$params_247
    params_phyact = params$params_phyact
    params_general = params$params_general
    params_cleaning = params$params_cleaning
    rm(params)
  }
  params_247[["L5M5window"]] = c(0,24) # as of version 1.6-0 this is hardcoded because argument qwindow now
  # specifies the window over which L5M5 analysis is done. So, L5M5window is a depricated
  # argument and this is also clarified in the documentation
  
  desiredtz = params_general[["desiredtz"]]
  idloc = params_general[["idloc"]]
  includedaycrit = params_cleaning[["includedaycrit"]][1]
  acc.metric = params_general[["acc.metric"]]
  
  fname = I$filename
  averageday = IMP$averageday
  if (is.null(IMP$data_masking_strategy)) {
    data_masking_strategy = IMP$strategy
  } else {
    data_masking_strategy = IMP$data_masking_strategy
  }
  hrs.del.start = IMP$hrs.del.start
  hrs.del.end = IMP$hrs.del.end
  maxdur = IMP$maxdur
  windowsizes = M$windowsizes
  metalong = M$metalong
  metashort = IMP$metashort
  rout = IMP$rout
  nonwearHoursFiltered = IMP$nonwearHoursFiltered
  nonwearEventsFiltered = IMP$nonwearEventsFiltered
  wdaycode = M$wday
  wdayname = M$wdayname
  if (length(params_phyact[["mvpadur"]]) > 0) params_phyact[["mvpadur"]] = sort(params_phyact[["mvpadur"]])
  LC2 = IMP$LC2
  LC = IMP$LC
  dcomplscore = IMP$dcomplscore
  r4 = as.numeric(as.matrix(rout[,4]))
  r5 = as.numeric(as.matrix(rout[,5]))
  ws3 = windowsizes[1]
  ws2 = windowsizes[2]
  keepindex_46 = keepindex_48 = c()
  # Extracting basic information about the file
  hvars = g.extractheadervars(I)
  # ID = hvars$ID;              iID = hvars$iID; IDd = hvars$IDd
  HN = hvars$HN;              sensor.location = hvars$sensor.location
  SX = hvars$SX;                deviceSerialNumber = hvars$deviceSerialNumber
  n_ws2_perday = (1440*60) / ws2
  n_ws3_perday = (1440*60) / ws3
  if (((nrow(metalong)/((1440*60)/ws2)*10) - (nrow(metashort)/((60/ws3)*1440)) * 10) > 1) {
    stop("Matrices 'metalong' and 'metashort' are not compatible")
  }
  #----------------------
  # Extract ID centrally
  # ID = extractID(hvars = hvars, idloc = idloc, fname = I$filename)
  
  #--------------------------------------------------------------
  # Extract qwindow if an activity log is provided:
  qwindow_actlog = FALSE
  if (is.data.frame(params_247[["qwindow"]]) == TRUE) {
    qwindow_actlog = TRUE
    IDacc = gsub(pattern = " ", replacement = "", x = as.character(ID))
    IDlog = gsub(pattern = " ", replacement = "", x = as.character(params_247[["qwindow"]]$ID))
    params_247[["qwindow"]] = params_247[["qwindow"]][which(IDlog == IDacc),]
  }
  # # Time window for L5 & M5 analysis (commented out because this is now defined further down)
  # t0_LFMF = L5M5window[1] #start in 24 hour clock hours
  # t1_LFMF = L5M5window[2]+(winhr-(M5L5res/60)) #end in 24 hour clock hours (if a value higher than 24 is chosen, it will take early hours of previous day to complete the 5 hour window
  # Time window for distribution analysis
  t_TWDI = params_247[["qwindow"]] #start and of 24 hour clock hours
  if (length(params_247[["qwindow"]]) == 0) {
    t_TWDI = c(0,24)
    if ((length(params_247[["qlevels"]]) > 0 | length(params_247[["ilevels"]]) > 0)) params_247[["qwindow"]] = c(0,24)
    qwindow_actlog = FALSE # ignore qwdinow_actlog if it does not produce actual qwindow values
  }
  if (length(params_247[["qwindow"]]) > 0 & qwindow_actlog == FALSE) {
    if (params_247[["qwindow"]][1] != 0) params_247[["qwindow"]] = c(0,params_247[["qwindow"]])
    if (params_247[["qwindow"]][length(params_247[["qwindow"]])] != 24) params_247[["qwindow"]] = c(params_247[["qwindow"]],24)
  }
  #==========================================================================================
  # Setting paramters (NO USER INPUT NEEDED FROM HERE ONWARDS)
  domvpa = doilevels = doiglevels = doquan = FALSE
  if (length(params_247[["qlevels"]]) > 0) doquan = TRUE
  if (length(params_247[["ilevels"]]) > 0) doilevels = TRUE
  if (length(params_247[["iglevels"]]) > 0) {
    if (length(params_247[["iglevels"]]) == 1) {
      params_247[["iglevels"]] = c(seq(0, 4000, by = 25), 8000) # to introduce option to just say TRUE
    }
    doiglevels = TRUE
  }
  if (length(params_phyact[["mvpathreshold"]]) > 0) domvpa = TRUE
  doperday = TRUE
  #------------------------------------------------------
  NVARS = (length(colnames(metashort)) - 1)
  if (NVARS < 1) NVARS = 1
  if (length(params_247[["qwindow"]]) > 0 | qwindow_actlog == TRUE) {
    NVARS = NVARS + 2 # for qwindow non-wear time
  }
  nfeatures = 50 + NVARS * (21 + length(params_247[["qlevels"]]) + length(params_247[["ilevels"]]))    #levels changed into qlevels
  if (length(params_247[["qwindow"]]) > 0 | qwindow_actlog == TRUE) {
    nfeatures = 50 + NVARS*(length(params_247[["qwindow"]]) * 
                              (21 + (length(params_247[["qlevels"]]) + length(params_247[["ilevels"]]))))
  }
  i = 1
  #---------------
  if (domvpa) { #create dummy data
    mvpanames = matrix(0, 6, length(params_phyact[["mvpathreshold"]]))
    mvpanames[, 1:length(params_phyact[["mvpathreshold"]])] = c("MVPA1","MVPA2","MVPA3","MVPA4","MVPA5","MVPA6")
  }
  # What is the minimum number of accelerometer axis needed to meet the criteria for nonwear in order for the data to be detected as nonwear?
  wearthreshold = 2 #needs to be 0, 1 or 2 (hard coded to avoid inconsistency in literature)
  #---------------------
  # detect first and last midnight and all midnights
  tsi = which(colnames(metalong) == "timestamp")
  time = as.character(metalong[,tsi])
  startt = as.character(metalong[1, tsi])
  # basic file characteristics
  LD = nrow(metalong) * (ws2/60) #length data in minutes
  ND = nrow(metalong)/n_ws2_perday #number of days
  #  time variable
  timeline = seq(0, ceiling(nrow(metalong)/n_ws2_perday), by = 1/n_ws2_perday)
  timeline = timeline[1:nrow(metalong)]
  tooshort = 0
  dmidn = g.detecmidnight(time,
                          desiredtz =  params_general[["desiredtz"]],
                          dayborder = params_general[["dayborder"]]) #ND,
  firstmidnight = dmidn$firstmidnight;  firstmidnighti = dmidn$firstmidnighti
  lastmidnight = dmidn$lastmidnight;    lastmidnighti = dmidn$lastmidnighti
  midnights = dmidn$midnights;          midnightsi = dmidn$midnightsi
  starttimei = 1
  endtimei = nrow(M$metalong)
  if (data_masking_strategy == 2) {
    starttimei = firstmidnighti
    endtimei = lastmidnighti - 1
  }
  # get r5long - this is vector with the 5 second scores of what data needs to be imputed
  r5long = matrix(0,length(r5),(ws2/ws3))
  r5long = replace(r5long,1:length(r5long),r5)
  r5long = t(r5long)
  dim(r5long) = c((length(r5)*(ws2/ws3)),1)
  if (length(myfun) > 0) {
    ExtFunColsi = which(colnames(M$metashort) %in% myfun$colnames ==  TRUE)
  } else {
    ExtFunColsi = c()
  }
  #===============================================
  # Extract features from the imputed data
  qcheck = r5long
  # LW = length(which(as.numeric(qcheck) != 1)) / (60/ws3) #number of minutes wear time between first and last midnights
  nfulldays = (lastmidnighti - firstmidnighti) / ((3600/ws2)*24)
  ndays = length(midnights) + 1 #ceiling(nfulldays + 2) # ceiling to cope with days with 23 hours
  #-------------------------------------
  # Detect orientation (at the moment only designed for hip with accelerometer):
  # assess which angle per axis is most strongly 24 hour correlated:
  # for hip worn devices this will be the vertical axis
  longitudinal_axis_id = ""
  epochday = 24 * 60 * (60/ws3)
  Ndays = floor(nrow(IMP$metashort)/epochday)
  if (length(which(c("anglex","angley","anglez") %in% colnames(IMP$metashort) == FALSE)) == 0 &
      Ndays >= 2) {
    Nhalfdays = Ndays - 1
    CorrA = rep(0,3)
    cnt = 1
    for (anglename in  c("anglex","angley","anglez") ) {
      if (sd(IMP$metashort[,anglename]) > 0) {
        CorrA[cnt] = stats::cor(IMP$metashort[1:(Nhalfdays*epochday), anglename],
                                IMP$metashort[(((Ndays - Nhalfdays) *
                                                  epochday) + 1):(Ndays * epochday), anglename])
      } else {
        CorrA[cnt] = NA
      }
      cnt = cnt + 1
    }
    if (length(which(is.na(CorrA) == FALSE)) > 0) {
      longitudinal_axis_id = which.max(CorrA)
    } else {
      longitudinal_axis_id = ""
    }
  }
  #--------------------------------------
  # Analysis of the average day
  # Derivation of distribution characteristics of the average day: quantiles (percentiles) and L5M5 method
  # Note that this is done here before all the other analyses because it only relies on the average day
  # The values and variablenames are, however, stored in the filesummary matrix towards the end (not here
  # in function g.analyse.avday).
  output_avday = g.analyse.avday(doquan = doquan, averageday = averageday,
                                 M = M, IMP = IMP, t_TWDI = t_TWDI,
                                 quantiletype = quantiletype, ws3 = ws3,
                                 doiglevels = doiglevels, firstmidnighti = firstmidnighti, ws2 = ws2,
                                 midnightsi = midnightsi, params_247 = params_247, qcheck = qcheck,
                                 acc.metric = acc.metric, params_phyact = params_phyact)
  cosinor_coef = output_avday$cosinor_coef
  
  #--------------------------------------------------------------
  # Analysis per day
  if (doperday == TRUE) {
    output_perday = g.analyse.perday(ndays = ndays,
                                     firstmidnighti = firstmidnighti, time = time,
                                     nfeatures = nfeatures, midnightsi = midnightsi,
                                     metashort = metashort, averageday = averageday,
                                     doiglevels = doiglevels, nfulldays = nfulldays,
                                     lastmidnight = lastmidnight,
                                     ws3 = ws3, ws2 = ws2, qcheck = qcheck, fname = fname,
                                     idloc = idloc, sensor.location = sensor.location,
                                     wdayname = wdayname,
                                     tooshort = tooshort, includedaycrit = includedaycrit,
                                     quantiletype = quantiletype, doilevels = doilevels, 
                                     domvpa = domvpa,
                                     mvpanames = mvpanames, wdaycode = wdaycode, ID = ID, 
                                     deviceSerialNumber = deviceSerialNumber,
                                     doquan = doquan,  ExtFunColsi = ExtFunColsi,
                                     myfun = myfun, desiredtz = desiredtz,
                                     params_247 = params_247, params_phyact = params_phyact)
  }
  #metashort is shortened from midgnight to midnight if requested (data_masking_strategy 2)
  if (data_masking_strategy == 2) {
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
  # Extract the average 24 hr but ignore angle metrics and externally extracted sleep
  lookattmp = which(colnames(metashort) %in% c("angle","anglex", "angley", "anglez", "ExtSleep") ==  FALSE)
  lookat = lookattmp[which(lookattmp > 1)] #]c(2:ncol(metashort[,lookattmp]))
  colnames_to_lookat = colnames(metashort)[lookat]
  AveAccAve24hr = matrix(NA,length(lookat),1)
  if (length(which(r5 == 0)) > 0) { #to catch data_masking_strategy 2 with only 1 midnight and other cases where there is no valid data
    for (h in 1:length(lookat)) {
      average24h = matrix(0,n_ws3_perday,1)
      average24hc = matrix(0,n_ws3_perday,1)
      if (floor(ND) != 0) {
        for (j in 1:floor(ND)) {
          dataOneDay = as.numeric(as.matrix(metashort[(((j - 1) * n_ws3_perday) + 1):(j * n_ws3_perday),lookat[h]]))
          val = which(is.na(dataOneDay) == F)
          average24h[val,1] = average24h[val,1] + dataOneDay[val] #mean acceleration
          average24hc[val,1] = average24hc[val,1] + 1
        }
      }
      if (floor(ND) < ND) {
        if (floor(ND) == 0) {
          dataOneDay = as.numeric(as.matrix(metashort[,lookat[h]]))
        } else {
          dataOneDay = as.numeric(as.matrix(metashort[((floor(ND) * n_ws3_perday) + 1):nrow(metashort), lookat[h]]))
        }
        val = which(is.na(dataOneDay) == F)
        average24h[val,1] = average24h[val,1] + dataOneDay[val]  #mean acceleration
        average24hc[val,1] = average24hc[val,1] + 1
      }
      average24h = average24h / average24hc
      AveAccAve24hr[h] = mean(average24h) #average acceleration in an average 24 hour cycle
    }
  }
  rm(metalong); rm(metashort)
  dataqual_summary = data.frame(clipping_score = LC2  / ((LD/1440)*96),
                                meas_dur_dys =  LD/1440,
                                dcomplscore = dcomplscore,
                                meas_dur_def_proto_day = LMp / 1440,
                                wear_dur_def_proto_day = LWp / 1440,
                                nonwearHoursFiltered = nonwearHoursFiltered,
                                nonwearEventsFiltered = nonwearEventsFiltered)
  file_summary = data.frame(wdayname = wdayname,
                            deviceSerialNumber = deviceSerialNumber,
                            sensor.location = sensor.location,
                            ID = ID,
                            fname = fname,
                            startt = startt)
  
  if (!is.null(M$QClog)) {
    # Summarise the QC log (currently only expected from cwa Axivity, actigraph, and csv files)
    QCsummarise = function(QClog, wx) {
      x = ifelse(test = length(wx) > 0,
                 yes = sum(QClog$end[wx] - QClog$start[wx]) / 60,
                 no = 0)
      return(x)
    }
    # total imputation
    if ("imputed" %in% colnames(M$QClog)) {
      impdone = which(M$QClog$imputed == TRUE)
      if (any(colnames(M$QClog) == "timegaps_min")) {
        file_summary$Dur_imputed = sum(M$QClog$timegaps_min)
        file_summary$Nblocks_imputed = sum(M$QClog$timegaps_n)
      } else {
        file_summary$Dur_imputed = QCsummarise(M$QClog, impdone)
        file_summary$Nblocks_imputed = length(impdone)
      }
    }
    
    # checksum
    if ("checksum_pass" %in% colnames(M$QClog)) {
      chsum_failed = which(M$QClog$checksum_pass == FALSE)
      file_summary$Dur_chsum_failed = QCsummarise(M$QClog, chsum_failed)
      file_summary$Nblocks_chsum_failed = length(chsum_failed)
      
    }
    
    # nonincremental block ID
    if ("blockID_current" %in% colnames(M$QClog)) {
      nonincremental = which(M$QClog$blockID_current - M$QClog$blockID_next != 1)
      file_summary$Dur_nonincremental = QCsummarise(M$QClog, nonincremental)
      file_summary$Nblocks_nonincremental = length(nonincremental)
    }
    
    # sampling frequency issues
    if ("frequency_blockheader" %in% colnames(M$QClog)) {
      freqBlockHead = M$QClog$frequency_blockheader
      frequency_bias = abs(M$QClog$frequency_observed - freqBlockHead) / freqBlockHead
    }
    if ("frequency_bias" %in% colnames(M$QClog)) {
      freqissue = which(frequency_bias >= 0.05 & frequency_bias < 0.1)
      file_summary$Dur_freqissue_5_10 = QCsummarise(M$QClog, freqissue)
      file_summary$Nblock_freqissue_5_10 = length(freqissue)
      
      freqissue = which(frequency_bias >= 0.1 & frequency_bias < 0.2)
      file_summary$Dur_freqissue_10_20 = QCsummarise(M$QClog, freqissue)
      file_summary$Nblock_freqissue_10_20 = length(freqissue)
      
      freqissue = which(frequency_bias >= 0.2 & frequency_bias < 0.3)
      file_summary$Dur_freqissue_20_30 = QCsummarise(M$QClog, freqissue)
      file_summary$Nblock_freqissue_20_30 = length(freqissue)
      
      freqissue = which(frequency_bias >= 0.3)
      file_summary$Dur_freqissue_30 = QCsummarise(M$QClog, freqissue)
      file_summary$Nblock_freqissue_30 = length(freqissue)
    }
  }
  
  metrics_nav = list(lookat = lookat,
                     colnames_to_lookat = colnames_to_lookat,
                     longitudinal_axis_id = longitudinal_axis_id)
  output_perfile = g.analyse.perfile(I, C, metrics_nav,
                                     AveAccAve24hr, 
                                     doquan, doiglevels, tooshort, 
                                     params_247 = params_247, 
                                     params_cleaning = params_cleaning,
                                     params_general = params_general,
                                     output_avday = output_avday,
                                     output_perday = output_perday,
                                     dataqual_summary = dataqual_summary,
                                     file_summary = file_summary)
  
  filesummary = output_perfile$filesummary
  daysummary = output_perfile$daysummary
  
  if (length(cosinor_coef) > 0) {
    cosinor_ts = cosinor_coef$coefext$cosinor_ts
  } else {
    cosinor_ts = c()
  }
  invisible(list(summary = filesummary, daysummary = daysummary, cosinor_ts = cosinor_ts))
}
