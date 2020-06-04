g.part5 = function(datadir=c(),metadatadir=c(),f0=c(),f1=c(),strategy=1,maxdur=7,hrs.del.start=0,hrs.del.end =0,
                   loglocation= c(),excludefirstlast.part5=FALSE,windowsizes=c(5,900,3600), acc.metric="ENMO",
                   boutcriter.mvpa=0.8,boutcriter.in=0.9,boutcriter.lig=0.8,storefolderstructure=FALSE,
                   threshold.lig = c(40),
                   threshold.mod = c(100),
                   threshold.vig = c(400),timewindow=c("MM","WW"),
                   boutdur.mvpa = c(1,5,10),
                   boutdur.in = c(10,20,30),
                   boutdur.lig = c(1,5,10),
                   winhr = 5,
                   M5L5res = 10,
                   overwrite=FALSE,desiredtz="",bout.metric=4, dayborder = 0, save_ms5rawlevels = FALSE,
                   do.parallel = TRUE, part5_agg2_60seconds = FALSE,
                   save_ms5raw_format = "csv", save_ms5raw_without_invalid=TRUE,
                   frag.classes.day = c(), #c("day_IN_bts", "day_IN_unbt"),
                   frag.classes.spt = c(),# "spt_sleep"
                   frag.metrics = c()) {
  options(encoding = "UTF-8")
  Sys.setlocale("LC_TIME", "C") # set language to Englishs
  # description: function called by g.shell.GGIR
  # aimed to merge the milestone output from g.part2, g.part3, and g.part4
  # in order to create a merged report of both physical activity and sleep
  #======================================================================
  # create new folder (if not existent) for storing milestone data
  ms5.out = "/meta/ms5.out"
  if (file.exists(paste(metadatadir,ms5.out,sep=""))) {
  } else {
    dir.create(file.path(metadatadir,ms5.out))
  }
  if (save_ms5rawlevels == TRUE) {
    ms5.outraw = "/meta/ms5.outraw"
    if (file.exists(paste(metadatadir,ms5.outraw,sep=""))) {
    } else {
      dir.create(file.path(metadatadir,ms5.outraw))
    }
    # Create on subfolder per configuration
    configurations = c()
    for (TRLi in threshold.lig) {
      for (TRMi in threshold.mod) {
        for (TRVi in threshold.vig) {
          configurations = c(configurations, paste0(TRLi,"_",TRMi,"_",TRVi))
        }
      }
    }
    for (hi in configurations) {
      folder2create = paste0(metadatadir,ms5.outraw,"/",hi)
      if (dir.exists(folder2create) == FALSE) {
        dir.create(path = folder2create)
      }
    }
  }
  SUM = nightsummary = M = IMP = sib.cla.sum= c()
  #======================================================================
  # compile lists of milestone data filenames
  fnames.ms1 = sort(dir(paste(metadatadir,"/meta/basic",sep="")))
  fnames.ms2 = sort(dir(paste(metadatadir,"/meta/ms2.out",sep="")))
  fnames.ms3 = sort(dir(paste(metadatadir,"/meta/ms3.out",sep="")))
  fnames.ms4 = sort(dir(paste(metadatadir,"/meta/ms4.out",sep="")))
  fnames.ms5 = sort(dir(paste(metadatadir,"/meta/ms5.out",sep="")))
  # path results folder
  results = paste(metadatadir,"/results",sep="")
  # path to sleeplog milestonedata, if it exists:
  sleeplogRDA = paste(metadatadir,"/meta/sleeplog.RData",sep="")
  if (file.exists(sleeplogRDA) == TRUE){
    load(sleeplogRDA)
  } else {
    sleeplog = c()
  }
  #------------------------------------------------
  # specify parameters
  ffdone = fnames.ms5 #ffdone is now a list of files that have already been processed by g.part5
  wdaynames = c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday")
  nfeatures = 500
  ws3 = windowsizes[1]
  ds_names = rep("",nfeatures)
  di = 1
  cnt = 1
  fnames.ms3 = sort(fnames.ms3)
  if (f1 == 0) length(fnames.ms4)
  if (f1 > length(fnames.ms4)) f1 = length(fnames.ms4)
  boutdur.mvpa = sort(boutdur.mvpa,decreasing = TRUE)
  boutdur.lig = sort(boutdur.lig,decreasing = TRUE)
  boutdur.in = sort(boutdur.in,decreasing = TRUE)
  if (save_ms5raw_format != "RData" & save_ms5raw_format != "csv") {
    save_ms5raw_format = "csv"# specify as csv if user does not clearly specify format
  }
  #--------------------------------
  # get full file path and folder name if requested by end-user and keep this for storage in output
  if (storefolderstructure == TRUE) {
    extractfilenames = function(x) as.character(unlist(strsplit(x,".RDa"))[1])
    referencefnames = sapply(fnames.ms3,extractfilenames)
    folderstructure = getfolderstructure(datadir,referencefnames)
    fullfilenames = folderstructure$fullfilenames
    foldername = folderstructure$foldername
  }
  if (f1 > length(fnames.ms3)) f1 = length(fnames.ms3)
  if (f0 > length(fnames.ms3)) f0 = 1
  if (f1 == 0 | length(f1) == 0 | f1 > length(fnames.ms3))  f1 = length(fnames.ms3)
  #======================================================================
  # loop through milestone data-files or filenames stored in output of g.part2 and g.part4
  # setup parallel backend to use many processors
  if (do.parallel == TRUE) {
    closeAllConnections() # in case there is a still something running from last time, kill it.
    cores=parallel::detectCores()
    Ncores = cores[1]
    if (Ncores > 3) {
      cl <- parallel::makeCluster(Ncores-1) #not to overload your computer
      doParallel::registerDoParallel(cl)
    } else {
      cat(paste0("\nparallel processing not possible because number of available cores (",Ncores,") < 4"))
      do.parallel = FALSE
    }
  }
  t0 = t1 = Sys.time() # copied here
  if (do.parallel == TRUE) {
    cat(paste0('\n Busy processing ... see ',metadatadir,'/ms5', ' for progress\n'))
  }
  # check whether we are in development mode:
  GGIRinstalled = is.element('GGIR', installed.packages()[,1])
  packages2passon = functions2passon = NULL
  GGIRloaded = "GGIR" %in% .packages()
  if (GGIRloaded) { #pass on package
    packages2passon = 'GGIR'
    errhand = 'pass'
  } else { # pass on functions
    functions2passon = c("is.ISO8601", "iso8601chartime2POSIX", "identify_levels", "g.getbout",
                         "g.part5.addfirstwake", "g.part5.addsib",
                         "g.part5.definedays", "g.part5.fixmissingnight",
                         "g.part5.onsetwaketiming", "g.part5.wakesleepwindows",
                         "g.part5.savetimeseries", "g.fragmentation")
    errhand = 'stop'
  }
  fe_dopar = foreach::`%dopar%`
  fe_do = foreach::`%do%`
  i = 0 # declare i because foreach uses it, without declaring it
  `%myinfix%` = ifelse(do.parallel, fe_dopar, fe_do) # thanks to https://stackoverflow.com/questions/43733271/how-to-switch-programmatically-between-do-and-dopar-in-foreach
  output_list =foreach::foreach(i=f0:f1,  .packages = packages2passon,
                                .export=functions2passon, .errorhandling=errhand) %myinfix% { # the process can take easily 1 minute per file, so probably there is a time gain by doing it parallel
  tryCatchResult = tryCatch({
  # for (i in f0:f1) {
    if (length(ffdone) > 0) {
      if (length(which(ffdone == fnames.ms3[i])) > 0) {
        skip = 1 #skip this file because it was analysed before")
      } else {
        skip = 0 #do not skip this file
      }
    } else {
      skip = 0
    }
    if (overwrite == TRUE) skip = 0
    # skip files from ms3 if there is no equivalent in ms4
    selp = which(fnames.ms4 == fnames.ms3[i])
    if (length(selp) > 0) {
      if (file.exists(paste(metadatadir,"/meta/ms4.out/",fnames.ms4[selp],sep="")) == FALSE) {
        skip = 1
      }
    } else {
      skip = 1
    }
    if (skip == 0) {
      t2 = Sys.time()
      deltat = as.numeric(difftime(t2,t1,units="hours"))
      if (deltat > 0.5) { # every half and hour print duration of progress
        deltat0 = round(as.numeric(difftime(t2,t0,units="hours"))*10)/10
        cat(paste(" ",i," (",unlist(strsplit(as.character(t2)," "))[2],")",sep=""))
        t1 = Sys.time()
      } else {
        cat(paste(" ",i,sep=""))
      }
      # load output g.part2
      selp = which(fnames.ms2 == fnames.ms3[i]) # so, fnames.ms3[i] is the reference point for filenames
      load(file=paste(metadatadir,"/meta/ms2.out/",fnames.ms2[selp],sep=""))
      # load output g.part4
      selp = which(fnames.ms4 == fnames.ms3[i])
      load(file=paste(metadatadir,"/meta/ms4.out/",fnames.ms4[selp],sep=""))
      summarysleep = nightsummary
      rm(nightsummary)
      idindex = which(summarysleep$filename == fnames.ms3[i])
      ID = summarysleep$ID[idindex[1]]
      ndays = nrow(summarysleep) #/ length(unique(summarysleep$sleepparam))
      dsummary = matrix("",((nrow(summarysleep)+12)*length(unique(summarysleep$sleepparam))
                            *length(unique(threshold.lig))
                            *length(unique(threshold.mod))
                            *length(unique(threshold.vig))
                            *length(unique(timewindow))),nfeatures)
      di = 1
      fi = 1
      sptwindow_HDCZA_end = c() # if it is not loaded from part3 milestone data then this will be the default
      if (length(idindex) > 0 & nrow(summarysleep) > 1) { #only attempt to load file if it was processed for sleep
        summarysleep_tmp = summarysleep
        #======================================================================
        # load output g.part1
        selp = which(fnames.ms1 == paste("meta_",fnames.ms3[i],sep=""))
        if (length(selp) != 1) {
          cat("Warning: Milestone data part 1 could not be retrieved")
        }
        load(paste0(metadatadir,"/meta/basic/",fnames.ms1[selp]))
        # load output g.part3
        load(paste0(metadatadir,"/meta/ms3.out/",fnames.ms3[i]))
        # extract key variables from the mile-stone data: time, acceleration and elevation angle
        # note that this is imputed ACCELERATION because we use this for describing behaviour:
        ts = data.frame(time=IMP$metashort[,1],ACC = IMP$metashort[,acc.metric] * 1000,
                        guider=rep("unknown",nrow(IMP$metashort)),
                        angle = as.numeric(as.matrix(IMP$metashort[,which(names(IMP$metashort) == "anglez")])) )
        Nts = nrow(ts)
        if (length(which(names(IMP$metashort) == "anglez")) == 0) {
          cat("Warning: anglez not extracted. Please check that do.anglez == TRUE")
        }
        # add non-wear column
        nonwear = IMP$rout[,5]
        nonwear = rep(nonwear,each=(IMP$windowsizes[2]/IMP$windowsizes[1]))
        if (length(nonwear) > Nts) {
          nonwear = nonwear[1:Nts]
        } else if (length(nonwear) < Nts) {
          nonwear = c(nonwear,rep(0,(Nts-length(nonwear))))
        }
        ts$nonwear = 0 # initialise column
        ts$nonwear = nonwear
        rm(IMP,M,I)
        clock2numtime = function(x) { # function used for converting sleeplog times to hour times
          x2 = as.numeric(unlist(strsplit(x,":"))) / c(1,60,3600)
          return(sum(x2))
        }
        Nepochsinhour = (60/ws3) * 60
        #=======================
        # extract epoch by epoch classification of the entire measurement (day specific summaries will follow further down)
        S = sib.cla.sum
        rm(sib.cla.sum)
        def = unique(S$definition)
        cut = which(S$fraction.night.invalid > 0.7 | S$nsib.periods == 0)
        if (length(cut) > 0) S = S[-cut,]
        if (part5_agg2_60seconds == TRUE) {
          ts_backup = ts
        }
        # Remove impossible entries:
        pko = which(summarysleep_tmp$sleeponset == 0 & summarysleep_tmp$wakeup == 0 & summarysleep_tmp$SptDuration == 0)
        if (length(pko) > 0) {
          summarysleep_tmp = summarysleep_tmp2[-pko,]
        }
        for (j in def) { # loop through sleep definitions (defined by angle and time threshold in g.part3)
          ws3new=ws3 # reset wse3new, because if part5_agg2_60seconds is TRUE then this will have been change in the previous iteration of the loop
          if (part5_agg2_60seconds == TRUE) {
            ts = ts_backup
          }
          # extract time and from that the indices for midnights
          time_POSIX = as.POSIXlt(iso8601chartime2POSIX(ts$time,tz=desiredtz),tz=desiredtz)
          tempp = unclass(time_POSIX)
          if (is.na(tempp$sec[1]) == TRUE) {
            tempp = unclass(as.POSIXlt(ts$time,tz=desiredtz))
          }
          sec = tempp$sec
          min = tempp$min
          hour = tempp$hour
          if (dayborder == 0) {
            nightsi = which(sec == 0 & min == 0 & hour == 0)
          } else {
            nightsi = which(sec == 0 & min == (dayborder-floor(dayborder))*60 & hour == floor(dayborder)) #shift the definition of midnight if required
          }
          # create copy of only relevant part of sleep summary dataframe
          summarysleep_tmp2 = summarysleep_tmp[which(summarysleep_tmp$sleepparam == j),]
          S2 = S[S$definition==j,] # simplify to one definition
          # Add sustained inactivity bouts (sib) to the time series
          ts = g.part5.addsib(ts,ws3new, Nts, S2, desiredtz, j,  nightsi)
          # Fix missing nights in part 4 data:
          summarysleep_tmp2 = g.part5.fixmissingnight(summarysleep_tmp2, sleeplog=sleeplog, ID)
          #Initialise diur variable, which will  indicate the diurnal rhythm: 0 if wake/daytime, 1 if sleep/nighttime
          ts$diur = 0
          if (nrow(summarysleep_tmp2) > 0) {
            # Add defenition of wake and sleep windows in diur column of data.frame ts
            ts = g.part5.wakesleepwindows(ts, summarysleep_tmp2, desiredtz, nightsi,
                                          sleeplog, ws3, Nts, ID, Nepochsinhour)
            # Add first waking up time, if it is missing:
            ts = g.part5.addfirstwake(ts, summarysleep_tmp2, nightsi, sleeplog, ID,
                                      Nepochsinhour, Nts, sptwindow_HDCZA_end, ws3)
            if (part5_agg2_60seconds == TRUE) { # Optionally aggregate to 1 minute epoch:
              ts$time_num = round(as.numeric(as.POSIXlt(iso8601chartime2POSIX(ts$time,tz=desiredtz),tz=desiredtz)) / 60) * 60
              ts = aggregate(ts[,c("ACC","sibdetection","diur","nonwear")], by = list(ts$time_num), FUN= function(x) mean(x))
              ts$sibdetection = round(ts$sibdetection)
              ts$diur = round(ts$diur)
              ts$nonwear = round(ts$nonwear)
              names(ts)[1] = "time"
              # convert back to iso8601 format
              ts$time = strftime(as.POSIXlt(ts$time,origin="1970-1-1",tz=desiredtz),format="%Y-%m-%dT%H:%M:%S%z", tz = desiredtz)
              ws3new = 60 # change because below it is used to decide how many epochs are there in
              # extract nightsi again
              time_POSIX = as.POSIXlt(iso8601chartime2POSIX(ts$time,tz=desiredtz),tz=desiredtz)
              tempp = unclass(time_POSIX)
              if (is.na(tempp$sec[1]) == TRUE) {
                tempp = unclass(as.POSIXlt(ts$time,tz=desiredtz))
              }
              sec = tempp$sec
              min = tempp$min
              hour = tempp$hour
              if (dayborder == 0) {
                nightsi = which(sec == 0 & min == 0 & hour == 0)
              } else {
                nightsi = which(sec == 0 & min == (dayborder-floor(dayborder))*60 & hour == floor(dayborder)) #shift the definition of midnight if required
              }
              Nts = nrow(ts)
            }
            for (TRLi in threshold.lig) {
              for (TRMi in threshold.mod) {
                for (TRVi in threshold.vig) {
                  # derive behavioral levels (class), e.g. MVPA, inactivity bouts, etc.
                  levels = identify_levels(ts,
                                           TRLi,TRMi,TRVi,
                                           boutdur.mvpa,boutcriter.mvpa,
                                           boutdur.lig,boutcriter.lig,
                                           boutdur.in,boutcriter.in,
                                           ws3new,bout.metric)
                  LEVELS = levels$LEVELS
                  OLEVELS = levels$OLEVELS
                  Lnames = levels$Lnames
                  bc.mvpa = levels$bc.mvpa
                  bc.lig = levels$bc.lig
                  bc.in = levels$bc.in
                  ts = levels$ts
                  
                  # Prepare fragmentation variables only once:
                  if (length(frag.classes.day) > 0 & length(frag.classes.spt) > 0 & length(frag.metrics) > 0) {
                    frag.classes.day_tmp = frag.classes.day
                    frag.classes.spt_tmp = frag.classes.spt
                    if ("day_MVPA_bts" %in% frag.classes.day_tmp) {
                      frag.classes.day_tmp = c(frag.classes.day_tmp, Lnames[grep(pattern ="day_MVPA_bts", x = Lnames)])
                    }
                    if ("day_LIG_bts" %in% frag.classes.day_tmp) {
                      frag.classes.day_tmp = c(frag.classes.day_tmp, Lnames[grep(pattern ="day_LIG_bts", x = Lnames)])
                    }
                    if ("day_IN_bts" %in% frag.classes.day_tmp) {
                      frag.classes.day_tmp = c(frag.classes.day_tmp, Lnames[grep(pattern ="day_IN_bts", x = Lnames)])
                    }
                  }
                  #=============================================
                  # NOW LOOP TROUGH DAYS AND GENERATE DAY SPECIFIC SUMMARY VARIABLES
                  # we want there to be one more nights in the accelerometer data than there are nights with sleep results
                  NNIGHTSSLEEP = length(unique(summarysleep_tmp2$calendar_date)) # nights with sleep results
                  NNIGHTSACC = length(nightsi) #acc
                  #-------------------------------
                  # ignore all nights in 'inights' before the first waking up and after the last waking up
                  FM = which(diff(ts$diur) == -1)
                  nightsi_bu = nightsi
                  # now 0.5+6+0.5 midnights and 7 days
                  for (timewindowi in timewindow) {
                    nightsi = nightsi_bu
                    ts$guider = "unknown"
                    if (timewindowi == "WW") {
                      if (length(FM) > 0) {
                        # ignore first and last midnight because we did not do sleep detection on it
                        nightsi = nightsi[nightsi > FM[1] & nightsi < FM[length(FM)]]
                      }
                    } else {
                      # newly added on 31-3-2019, because if first night is missing then nights needs to allign with diur
                      startend_sleep = which(abs(diff(ts$diur))==1)
                      Nepochsin12Hours =  (60/ws3new)*60*12
                      nightsi = nightsi[nightsi >= (startend_sleep[1] - Nepochsin12Hours) &
                                                nightsi <= (startend_sleep[length(startend_sleep)] + Nepochsin12Hours)]  # newly added on 25-11-2019
                      #nightsi = nightsi[which(nightsi >= startend_sleep[1] & nightsi <= startend_sleep[length(startend_sleep)])]
                    }
                    if (timewindowi == "MM") {
                      Nwindows = nrow(summarysleep_tmp2)
                    } else {
                      Nwindows = length(which(diff(ts$diur) == -1))
                    }
                    indjump = 1
                    qqq_backup = c()
                    for (wi in 1:Nwindows) { #loop through 7 windows (+1 to include the data after last awakening)
                      # Define indices of start and end of the day window (e.g. midnight-midnight, or waking-up or wakingup
                      defdays = g.part5.definedays(nightsi, wi, summarysleep_tmp2, indjump,
                                               nightsi_bu, ws3new, qqq_backup, ts, Nts,
                                               timewindowi, Nwindows)
                      qqq = defdays$qqq
                      qqq_backup = defdays$qqq_backup
                      if (length(which(is.na(qqq)==TRUE)) == 0) { #if it is a meaningful day then none of the values in qqq should be NA
                        fi = 1
                        # START STORING BASIC INFORMATION
                        dsummary[di,fi:(fi+2)] = c(ID, fnames.ms3[i], wi)
                        ds_names[fi:(fi+2)] = c("ID", "filename", "window_number"); fi = fi + 3
                        if (timewindowi == "WW") {
                          plusb = 0
                        } else {
                          plusb = 1
                        }
                        skiponset = skipwake = TRUE

                        #==========================
                        # Newly added to avoid issue with merging sleep
                        # variables from part 4, we simply extract them
                        # from the new time series
                        # Note that this means that for MM windows there can be multiple or no wake or onsets
                        date = as.Date(as.POSIXlt(ts$time[qqq[1]+1],tz=desiredtz))
                        Sys.setlocale("LC_TIME", "C")
                        weekday = weekdays(date)
                        dsummary[di,fi:(fi+1)] = c(weekday, as.character(date))
                        ds_names[fi:(fi+1)] = c("weekday","calendar_date");fi = fi + 2
                        # Get onset and waking timing, both as timestamp and as index
                        onsetwaketiming = g.part5.onsetwaketiming(qqq,ts, min, sec, hour, timewindowi, skiponset, skipwake)
                        onset = onsetwaketiming$onset; wake = onsetwaketiming$wake
                        onseti = onsetwaketiming$onseti; wakei = onsetwaketiming$wakei
                        skiponset = onsetwaketiming$skiponset; skipwake = onsetwaketiming$skipwake
                        # Add to dsummary output matrix
                        if (skiponset == FALSE) {
                          dsummary[di,fi] = onset
                          dsummary[di,fi+1] = as.character(strftime(as.character(time_POSIX[onseti]), tz=desiredtz, format="%H:%M:%S"))
                        } else {
                          dsummary[di,fi:(fi+1)] = rep(NA,2)
                        }
                        ds_names[fi:(fi+1)] = c("sleeponset", "sleeponset_ts");      fi = fi + 2
                        if (skipwake == FALSE) {
                          dsummary[di,fi] = wake
                          dsummary[di,fi+1] = as.character(strftime(as.character(time_POSIX[wakei]),tz=desiredtz, format="%H:%M:%S"))
                        } else {
                          dsummary[di,fi:(fi+1)] = rep(NA,2)
                        }
                        ds_names[fi:(fi+1)] = c("wakeup", "wakeup_ts");      fi = fi + 2
                        # extract date and use this to retrieve corresponding part 4 information about the nights:
                        options(encoding = "UTF-8")
                        Sys.setlocale("LC_TIME", "C") # set language to English
                        # look up matching part4 entry:
                        recDates = as.Date(summarysleep_tmp2$calendar_date, format="%e/%m/%Y", origin="1970-01-01")
                        dsummary[di,fi] = j
                        ds_names[fi] = "sleepparam";      fi = fi + 1
                        dayofinterst = which(recDates == date)
                        if (length(dayofinterst) > 0) {
                          dsummary[di,fi:(fi+5)] = c(summarysleep_tmp2$night[dayofinterst],
                                              summarysleep_tmp2$daysleeper[dayofinterst],
                                              summarysleep_tmp2$cleaningcode[dayofinterst],
                                              summarysleep_tmp2$guider[dayofinterst],
                                              summarysleep_tmp2$sleeplog_used[dayofinterst],
                                              summarysleep_tmp2$acc_available[dayofinterst])
                          ds_names[fi:(fi+5)] = c("night_number", "daysleeper", "cleaningcode",
                                           "guider", "sleeplog_used", "acc_available");      fi = fi + 6
                          ts$guider[qqq[1]:qqq[2]] = summarysleep_tmp2$guider[dayofinterst] # add guider also to timeseries
                        } else {
                          dsummary[di,fi:(fi+5)] = rep(NA,6)
                          ds_names[fi:(fi+5)] = c("night_number",
                                                  "daysleeper","cleaningcode","guider",
                                                  "sleeplog_used","acc_available"); fi = fi + 6
                        }
                        # Untill here.
                        #==========================

                        # define time windows:
                        # We will get acc_onset and wakeup
                        # regarding to the same day of measurement in the same row.
                        # which differs between MM and WW
                        # Also, it allows for the analysis of the first day for those studies in which the accelerometer is started during the morning and the first day is of interest.
                        # qqq1 is the start of the day
                        # qqq2 is the end of the day
                        qqq1 = qqq[1] # added 26 Feb 2020
                        qqq2 = qqq[2] # added 26 Feb 2020
                        if (timewindowi == "MM") {
                          dsummary[di, fi] = "MM"
                        } else if (timewindowi == "WW") {
                          dsummary[di, fi] = "WW"
                        }
                        ds_names[fi] = "window";      fi = fi + 1
                        # keep track of threshold value
                        dsummary[di,fi] = TRLi
                        ds_names[fi] = "TRLi";      fi = fi + 1
                        dsummary[di,fi] = TRMi
                        ds_names[fi] = "TRMi";      fi = fi + 1
                        dsummary[di,fi] = TRVi
                        ds_names[fi] = "TRVi";      fi = fi + 1
                        wlih = ((qqq2-qqq1)+1)/((60/ws3new)*60)
                        if (qqq1 > length(LEVELS)) qqq1 = length(LEVELS)
                        # # This part should be redundant now, so commented out (26-Feb 2020):
                        # if (wlih > 30 & length(summarysleep_tmp2$night) > 1) {
                        #   # scenario when day is missing and code reaches out to two days before this day
                        #   qqq1 = (qqq2 - (24* ((60/ws3)*60))) + 1 # code now uses only 24hours before waking up
                        #   if (qqq1 < 1) qqq1 = 1
                        #   wlih = ((qqq2-qqq1)+1)/((60/ws3)*60)
                        # }
                        # dsummary[di,fi] = wlih
                        # ds_names[fi] = "window_length_hours";      fi = fi + 1
                        # dsummary[di,fi] = (length(which(nonwear[qqq1:qqq2] == 1)) / ((qqq2-qqq1)+1) ) * 100
                        # ds_names[fi] = "nonwear_perc_window";      fi = fi + 1
                        # dsummary[di,fi] = (length(which(nonwear[qqq1:qqq2] == 1 & ts$diur[qqq1:qqq2] == 0)) / length(which(ts$diur[qqq1:qqq2] == 0)))  *100
                        # ds_names[fi] = "nonwear_perc_waking";      fi = fi + 1
                        # dsummary[di,fi] = (length(which(nonwear[qqq1:qqq2] == 1 & ts$diur[qqq1:qqq2] == 1)) / length(which(ts$diur[qqq1:qqq2] == 1))) * 100
                        # ds_names[fi] = "nonwear_perc_SPT";      fi = fi + 1
                        #============================================================
                        # percentage of available data
                        zt_hrs_nonwear = (length(which(ts$diur[qqq1:qqq2] == 0 & ts$nonwear[qqq1:qqq2] == 1)) * ws3new) / 3600 #day
                        zt_hrs_total = (length(which(ts$diur[qqq1:qqq2] == 0)) * ws3new) / 3600 #day
                        dsummary[di,fi] = (zt_hrs_nonwear/zt_hrs_total)  * 10000 / 100
                        ds_names[fi] = "nonwear_perc_day";      fi = fi + 1
                        zt_hrs_nonwear = (length(which(ts$diur[qqq1:qqq2] == 1 & ts$nonwear[qqq1:qqq2] == 1)) * ws3new) / 3600 #night
                        zt_hrs_total = (length(which(ts$diur[qqq1:qqq2] == 1)) * ws3new) / 3600 #night
                        dsummary[di,fi] =  (zt_hrs_nonwear/zt_hrs_total)  * 10000 / 100
                        ds_names[fi] = "nonwear_perc_spt";      fi = fi + 1
                        zt_hrs_nonwear = (length(which(ts$nonwear[qqq1:qqq2] == 1)) * ws3new) / 3600
                        zt_hrs_total = (length(ts$diur[qqq1:qqq2]) * ws3new) / 3600 #night and day
                        dsummary[di,fi] =  (zt_hrs_nonwear/zt_hrs_total)  * 10000 / 100
                        ds_names[fi] = "nonwear_perc_day_spt";      fi = fi + 1
                        #===============================================
                        # TIME SPENT IN WINDOWS (window is either midnight-midnight or waking up-waking up)
                        test_remember = c(di,fi)
                        for (levelsc in 0:(length(Lnames)-1)) {
                          dsummary[di,fi] = (length(which(LEVELS[qqq1:qqq2] == levelsc)) * ws3new) / 60
                          ds_names[fi] = paste0("dur_",Lnames[levelsc+1],"_min");      fi = fi + 1
                        }
                        for (g in 1:4) {
                          dsummary[di,(fi+(g-1))] = (length(which(OLEVELS[qqq1:qqq2] == g)) * ws3new) / 60
                        }
                        ds_names[fi:(fi+3)] = c("dur_day_total_IN_min",
                                                "dur_day_total_LIG_min",
                                                "dur_day_total_MOD_min",
                                                "dur_day_total_VIG_min")
                        fi = fi + 4
                        dsummary[di,fi] = (length(which(ts$diur[qqq1:qqq2] == 0)) * ws3new) / 60
                        ds_names[fi] = "dur_day_min";      fi = fi + 1
                        dsummary[di,fi] = (length(which(ts$diur[qqq1:qqq2] == 1)) * ws3new) / 60
                        ds_names[fi] = "dur_spt_min";      fi = fi + 1
                        dsummary[di,fi] = (length(c(qqq1:qqq2)) * ws3new) / 60
                        ds_names[fi] = "dur_day_spt_min";      fi = fi + 1

                        
                        
                        #============================================
                        # Number of long wake periods (defined as > 5 minutes) during the night
                        Nawake = length(which(abs(diff(which(LEVELS[qqq1:qqq2] == 0))) > (300 / ws3new))) - 2
                        if (Nawake < 0) Nawake = 0
                        dsummary[di,fi] = Nawake
                        ds_names[fi] = "N_atleast5minwakenight";      fi = fi + 1
                        #=============================
                        # sleep efficiency
                        dsummary[di,fi] = length(which(ts$sibdetection[qqq1:qqq2] == 1 &
                                                         ts$diur[qqq1:qqq2] == 1)) / length(which(ts$diur[qqq1:qqq2] == 1))
                        ds_names[fi] = "sleep_efficiency";      fi = fi + 1
                        #===============================================
                        # AVERAGE ACC PER WINDOW
                        sse = qqq1:qqq2
                        for (levelsc in 0:(length(Lnames)-1)) {
                          dsummary[di,fi] = mean(ts$ACC[sse[LEVELS[sse] == levelsc]])
                          ds_names[fi] = paste("ACC_",Lnames[levelsc+1],"_mg",sep="");      fi = fi + 1
                        }
                        for (g in 1:4) {
                          dsummary[di,(fi+(g-1))] = mean(ts$ACC[sse[OLEVELS[sse] == g]])
                        }
                        ds_names[fi:(fi+3)] = c("ACC_day_total_IN_mg", "ACC_day_total_LIG_mg",
                                                "ACC_day_total_MOD_mg", "ACC_day_total_VIG_mg")
                        fi = fi + 4
                        dsummary[di,fi] = mean(ts$ACC[sse[ts$diur[sse] == 0]])
                        ds_names[fi] = "ACC_day_mg";      fi = fi + 1
                        dsummary[di,fi] = mean(ts$ACC[sse[ts$diur[sse] == 1]])
                        ds_names[fi] = "ACC_spt_mg";      fi = fi + 1
                        dsummary[di,fi] = mean(ts$ACC[sse])
                        ds_names[fi] = "ACC_day_spt_mg";      fi = fi + 1
                        #===============================================
                        # QUANTILES...
                        WLH = ((qqq2-qqq1)+1)/((60/ws3new)*60)
                        if (WLH <= 1) WLH = 1.001
                        dsummary[di,fi] = quantile(ts$ACC[sse],probs=((WLH-1)/WLH),na.rm=TRUE)
                        ds_names[fi] = paste("quantile_mostactive60min_mg",sep="");      fi = fi + 1
                        dsummary[di,fi] = quantile(ts$ACC[sse],probs=((WLH-0.5)/WLH),na.rm=TRUE)
                        ds_names[fi] = paste("quantile_mostactive30min_mg",sep="");      fi = fi + 1
                        #===============================================
                        # L5 M5, L10 M10...
                        for (wini in winhr) {
                          reso = M5L5res #resolution at 5 minutes
                          endd = floor(WLH*10) /10 # rounding needed for non-integer window lengths
                          nwindow_f = (endd-wini) #number of windows for L5M5 analyses
                          ignore = FALSE
                          if (endd <= wini | nwindow_f < 1) ignore = TRUE # day is shorter then time window, so ignore this # modified from < to <= on 21-1-2017
                          nwindow_f = nwindow_f * (60/reso)
                          if (ignore == FALSE) {
                            ACCrunwin = matrix(0,nwindow_f,1)
                            TIMErunwin= matrix("",nwindow_f,1)
                            for (hri in 0:floor((((endd-wini)*(60/reso))-1))) {
                              e1 = (hri*reso*(60/ws3new))+1
                              e2 = (hri+(wini*(60/reso)))*reso*(60/ws3new)
                              if (e2 > length(sse)) e2 = length(sse)
                              ACCrunwin[(hri+1),1] = mean(ts$ACC[sse[e1:e2]])
                              TIMErunwin[(hri+1),1]= as.character(ts$time[sse[e1]])
                            }
                            ACCrunwin = ACCrunwin[is.na(ACCrunwin) == F]
                            TIMErunwin= TIMErunwin[is.na(ACCrunwin) == F]
                            if (length(ACCrunwin) > 0 & length(TIMErunwin) > 0) {
                              L5HOUR = TIMErunwin[which(ACCrunwin == min(ACCrunwin))[1]]
                              L5VALUE = min(ACCrunwin)
                              M5HOUR = TIMErunwin[which(ACCrunwin == max(ACCrunwin))[1]]
                              M5VALUE = max(ACCrunwin)
                            } else {
                              L5HOUR = M5HOUR = "not detected"
                              L5VALUE = M5VALUE = ""
                            }
                          }
                          if (ignore == FALSE) dsummary[di,fi] = L5HOUR
                          ds_names[fi] = paste("L",wini,"TIME",sep="");      fi = fi + 1
                          if (ignore == FALSE) dsummary[di,fi] = L5VALUE
                          ds_names[fi] = paste("L",wini,"VALUE",sep="");      fi = fi + 1
                          if (ignore == FALSE) dsummary[di,fi] = M5HOUR
                          ds_names[fi] = paste("M",wini,"TIME",sep="");      fi = fi + 1
                          if (ignore == FALSE) dsummary[di,fi] = M5VALUE
                          ds_names[fi] = paste("M",wini,"VALUE",sep="");      fi = fi + 1
                          if (ignore == FALSE) {
                            if (is.ISO8601(L5HOUR)) { # only do this for ISO8601 format
                              L5HOUR = as.character(iso8601chartime2POSIX(L5HOUR,tz=desiredtz))
                              M5HOUR = as.character(iso8601chartime2POSIX(M5HOUR,tz=desiredtz))
                              if (length(unlist(strsplit(L5HOUR," "))) == 1) L5HOUR = paste0(L5HOUR," 00:00:00") #added because on some OS timestamps are deleted for midnight
                              if (length(unlist(strsplit(M5HOUR," "))) == 1) M5HOUR = paste0(M5HOUR," 00:00:00")
                            }
                            if (L5HOUR != "not detected") {
                              time_num = sum(as.numeric(unlist(strsplit(unlist(strsplit(L5HOUR," "))[2],":"))) * c(3600,60,1)) / 3600
                              if (time_num < 12) time_num = time_num + 24
                              dsummary[di,fi] = time_num
                            } else {
                              dsummary[di,fi] = NA
                            }
                          }
                          ds_names[fi] = paste("L",wini,"TIME_num",sep="");      fi = fi + 1
                          if (ignore == FALSE) {
                            if (M5HOUR != "not detected") {
                              time_num = sum(as.numeric(unlist(strsplit(unlist(strsplit(M5HOUR," "))[2],":"))) * c(3600,60,1)) / 3600
                              if (time_num < 12) time_num = time_num + 24
                              dsummary[di,fi] = time_num
                            } else {
                              dsummary[di,fi] = NA
                            }
                          }
                          ds_names[fi] = paste("M",wini,"TIME_num",sep="");      fi = fi + 1
                        }
                        #===============================================
                        NANS = which(is.nan(dsummary[di,]) == TRUE) #average of no values will results in NaN
                        if (length(NANS) > 0) dsummary[di,NANS] = ""
                        #===============================================
                        # NUMBER OF BOUTS
                        checkshape = function(boutcount) {
                          if (is.matrix(boutcount) == FALSE) {# if there is only one bout setting
                            boutcount = as.matrix(boutcount)
                            if (nrow(boutcount) > ncol(boutcount)) boutcount = t(boutcount)
                          }
                          return(boutcount)
                        }
                        bc.mvpa = checkshape(bc.mvpa)
                        for (bci in 1:nrow(bc.mvpa)) {
                          dsummary[di,fi+(bci-1)] = length(which(diff(bc.mvpa[bci,])[sse] == 1))
                          if (bci ==1) {
                            ds_names[fi+(bci-1)] = paste0("Nbouts_day_MVPA_bts_",boutdur.mvpa[bci])
                          } else {
                            ds_names[fi+(bci-1)] = paste0("Nbouts_day_MVPA_bts_",boutdur.mvpa[bci],"_",boutdur.mvpa[bci-1])
                          }
                        }
                        
                        fi = fi + bci
                        bc.in = checkshape(bc.in)
                        for (bci in 1:nrow(bc.in)) {
                          dsummary[di,fi+(bci-1)] = length(which(diff(bc.in[bci,])[sse] == 1))
                          if (bci ==1) {
                            ds_names[fi+(bci-1)] = paste0("Nbouts_day_IN_bts_",boutdur.in[bci])
                          } else {
                            ds_names[fi+(bci-1)] = paste0("Nbouts_day_IN_bts_",boutdur.in[bci],"_",boutdur.in[bci-1])
                          }
                        
                        }
                        fi = fi + bci
                        bc.lig = checkshape(bc.lig)
                        for (bci in 1:nrow(bc.lig)) {
                          dsummary[di,fi+(bci-1)] = length(which(diff(bc.lig[bci,])[sse] == 1))
                          if (bci ==1) {
                            ds_names[fi+(bci-1)] = paste0("Nbouts_day_LIG_bts_",boutdur.lig[bci])
                          } else {
                            ds_names[fi+(bci-1)] = paste0("Nbouts_day_LIG_bts_",boutdur.lig[bci],"_",boutdur.lig[bci-1])
                          }
                        }
                        fi = fi + bci
                        #===============================================
                        # NUMBER OF WINDOWS / BLOCKS
                        for (levelsc in 0:(length(Lnames)-1)) {
                          dsummary[di,fi] = length(which(diff(which(LEVELS[sse] != levelsc)) > 1)) #qqq1:qqq2
                          if (dsummary[di,fi] == 0 & LEVELS[qqq1] == levelsc) dsummary[di,fi] = 1
                          ds_names[fi] = paste("Nblocks_",Lnames[levelsc+1],sep="");      fi = fi + 1
                        }
                        for (g in 1:4) {
                          dsummary[di,(fi+(g-1))] = length(which(diff(which(OLEVELS[qqq1:qqq2] != g))> 1))
                        }
                        ds_names[fi:(fi+3)] = c("Nblocks_day_total_IN","Nblocks_day_total_LIG",
                                                "Nblocks_day_total_MOD","Nblocks_day_total_VIG")
                        fi = fi + 4
                        dsummary[di,fi:(fi+6)] = c(boutcriter.in, boutcriter.lig, boutcriter.mvpa,
                                            paste(boutdur.in,collapse="_"), paste(boutdur.lig,collapse="_"),
                                            paste(boutdur.mvpa,collapse="_"), bout.metric)
                        ds_names[fi:(fi+6)] = c("boutcriter.in", "boutcriter.lig", "boutcriter.mvpa",
                                         "boutdur.in",  "boutdur.lig", "boutdur.mvpa", "bout.metric"); fi = fi + 7
                       
                        #===============================================
                        # FRAGMENTATION per window, and split by night and day
                        if (length(frag.classes.day_tmp) > 0 & length(frag.classes.spt_tmp) > 0 & length(frag.metrics) > 0) {
                          binarize = function(x, values) {
                            # converts input into binary signal, with a 1 for x values that match the vector values
                            # and a 0 for all other values
                            tmp = as.integer(ifelse(test = x %in% values, yes = 1, no = 0))
                            return(tmp)
                          }
                          # daytime
                          frag.classes.day2 = which(Lnames %in% frag.classes.day_tmp) - 1 # convert to numberic class id
                          frag.out = g.fragmentation(x=binarize(LEVELS[sse[ts$diur[sse] == 0]], values = frag.classes.day2), 
                                                   frag.metrics = frag.metrics)
                          dsummary[di,fi:(fi+(length(frag.out)-1))] = as.numeric(frag.out)
                          ds_names[fi:(fi+(length(frag.out)-1))] = paste0("FRAG_",names(frag.out),"_day")
                          fi = fi + length(frag.out)
                          # spt
                          frag.classes.spt2 = which(Lnames %in% frag.classes.spt_tmp) - 1 # convert to numberic class id
                          frag.out = g.fragmentation(x=binarize(LEVELS[sse[ts$diur[sse] == 1]], values = frag.classes.spt2),
                                                   frag.metrics = frag.metrics)
                          dsummary[di,fi:(fi+(length(frag.out)-1))] = as.numeric(frag.out)
                          ds_names[fi:(fi+(length(frag.out)-1))] = paste0("FRAG_",names(frag.out),"_spt")  
                          fi = fi + length(frag.out)
                        }
                        #===============================================
                        # FOLDER STRUCTURE
                        if (storefolderstructure == TRUE) {
                          dsummary[di,fi] = fullfilenames[i] #full filename structure
                          ds_names[fi] = "filename_dir"; fi = fi + 1
                          dsummary[di,fi] = foldername[i] #store the lowest foldername
                          ds_names[fi] = "foldername"; fi = fi + 1
                        }
                        di = di + 1
                      }
                    }
                  }
                  if (save_ms5rawlevels == TRUE) {

                    legendfile = paste0(metadatadir,ms5.outraw,"/behavioralcodes",as.Date(Sys.time()),".csv")
                    if (file.exists(legendfile) == FALSE) {
                      legendtable = data.frame(class_name = Lnames, class_id = 0:(length(Lnames)-1), stringsAsFactors = F)
                      write.csv(legendtable, file = legendfile, row.names=F)
                    }

                    # I moved this bit of code to the end, because we want guider to be included (VvH April 2020)

                    rawlevels_fname =  paste0(metadatadir,ms5.outraw,"/",TRLi,"_",TRMi,"_",TRVi,"/",fnames.ms3[i],".",save_ms5raw_format)
                    # save time series to csv files
                    g.part5.savetimeseries(ts[,c("time","ACC","diur","nonwear","guider")], LEVELS,
                                           desiredtz, rawlevels_fname, save_ms5raw_format, save_ms5raw_without_invalid)

                  }
                }
              }
            }
          }
        }
        #remove NA values
        for (kik in 1:ncol(dsummary)) {
          naval = which(is.na(dsummary[,kik])==TRUE)
          if(length(naval) > 0) dsummary[naval,kik] = ""
        }
        output = data.frame(dsummary,stringsAsFactors=FALSE)
        names(output) = ds_names
        if (excludefirstlast.part5 == TRUE) {
          output$window_number = as.numeric(output$window_number)
          cells2exclude = c(which(output$window_number == min(output$window_number,na.rm = TRUE)),
                            which(output$window_number == max(output$window_number,na.rm = TRUE)))
          if (length(cells2exclude) > 0) {
            output = output[-cells2exclude,]
          }
        }
        # correct definition of sleep log availability for window = WW, because now it
        # also relies on sleep log from previous night
        whoareWW = which(output$window == "WW") # look up WW
        if (length(loglocation) > 0) { #only do this if there is a sleep log
          if (length(whoareWW) > 0) {
            whoareNOSL =which(output$sleeplog_used[whoareWW] == "0") #look up nights with no Sleeplog
            if (length(whoareNOSL) > 0) {
              for (k23 in 1:length(whoareNOSL)) {
                k24 = whoareWW[(whoareNOSL[k23]-1)]
                if (length(k24) > 0) {
                  if (k24 > 0) {
                    output$sleeplog_used[k24] = "0"
                  }
                }
              }
            }
          }
        }
        # missing_sleeplog_used = which(is.logical(output$sleeplog_used) == "0")
        # if (length(missing_sleeplog_used) > 0) {
        #   output$sleeplog_used[missing_sleeplog_used] = "0"
        # }
        # tidy up output data frame, because it may have a lot of empty rows and columns
        emptyrows = which(output[,1] == "" & output[,2] == "")
        if (length(emptyrows) > 0) output = output[-emptyrows,]
        lastcolumn = which(colnames(output) == "bout.metric")
        if (length(lastcolumn) > 0) {
          if (ncol(output) > lastcolumn) {
            emptycols = sapply(output, function(x)all(x==""))# Find columns filled with missing values which(output[1,] == "" & output[2,] == "")
            emptycols = which(emptycols == TRUE)
            if (length(emptycols) > 0) emptycols = emptycols[which(emptycols > lastcolumn)]
            if (length(emptycols) > 0) output = output[-emptycols]
          }
          if (length(output) > 0) {
            save(output,file=paste(metadatadir,ms5.out,"/",fnames.ms3[i],sep=""))
          }
        }
        rm(output,dsummary)
      }
    }
  # } # For the for loop
  }) # END tryCatch
  return(tryCatchResult)
  }
  if (do.parallel == TRUE) {
    on.exit(parallel::stopCluster(cl))
    for (oli in 1:length(output_list)) { # logged error and warning messages
      if (is.null(unlist(output_list[oli])) == FALSE) {
        cat(paste0("\nErrors and warnings for ",fnames.ms3[oli]))
        print(unlist(output_list[oli])) # print any error and warnings observed
      }
    }
  }
  SI = sessionInfo()
  sessionInfoFile = paste(metadatadir,"/results/QC/sessioninfo_part5.RData",sep="")
  if (file.exists(sessionInfoFile)) {
    FI = file.info(sessionInfoFile)
    timesincecreation = abs(as.numeric(difftime(FI$ctime,Sys.time(),units="secs")))
    # if file is older than 2 hours plus a random number of seconds (max 1 hours) then overwrite it
    if (timesincecreation > (2*3600 + (sample(seq(1,3600,by=0.1),size = 1)))) {
      save(SI,file=sessionInfoFile)
    }
  } else {
    save(SI,file=sessionInfoFile)
  }
}
