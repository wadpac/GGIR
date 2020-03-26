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
                   do.parallel = TRUE) {
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
    functions2passon = c("is.ISO8601", "iso8601chartime2POSIX", "identify_levels", "g.getbout")
    errhand = 'stop'
  }
  fe_dopar = foreach::`%dopar%`
  fe_do = foreach::`%do%`
  i = 0 # declare i because foreach uses it, without declaring it
  `%myinfix%` = ifelse(do.parallel, fe_dopar, fe_do) # thanks to https://stackoverflow.com/questions/43733271/how-to-switch-programmatically-between-do-and-dopar-in-foreach
  output_list =foreach::foreach(i=f0:f1,  .packages = packages2passon, 
                                .export=functions2passon, .errorhandling=errhand) %myinfix% { # the process can take easily 1 minute per file, so probably there is a time gain by doing it parallel
    tryCatchResult = tryCatch({
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
      id = summarysleep$id[idindex[1]]
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
        time = IMP$metashort[,1]
        #note that this is imputed ACCELERATION because we use this for describing behaviour:
        ACC = IMP$metashort[,acc.metric] * 1000 
        if (length(which(names(IMP$metashort) == "anglez")) == 0) {
          cat("Warning: anglez not extracted. Please check that do.anglez == TRUE")
        }
        #note that this is the non-imputed angle because we use this here only for the visualisation:
        angle = as.numeric(as.matrix(M$metashort[,which(names(IMP$metashort) == "anglez")])) 
        nonwear = IMP$rout[,5]
        nonwear = rep(nonwear,each=(IMP$windowsizes[2]/IMP$windowsizes[1]))
        if (length(nonwear) > length(time)) {
          nonwear = nonwear[1:length(time)]
        } else if (length(nonwear) < length(time)) {
          nonwear = c(nonwear,rep(0,(length(time)-length(nonwear))))
        }
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
        for (j in def) { # loop through sleep definitions (defined by angle and time threshold in g.part3)        
          #========================================================
          # SUSTAINED INACTIVITY BOUTS
          # These are stored in part 3 milestone data as start- and end-times
          # in the following code we convert the them into indices of the recording sequence
          S2 = S[which(S$definition==j),] # simplify to one definition
          sibdetection = rep(0,length(time)) # initialize output vector that will hold the sibs
          s0s1 = c()
          # pr0 and pr1 define the indices relative the start of the recording
          # and specifying a 6 day time window 
          # always relative to the most recently processed sustained inactivity bout
          # s0 and s1 are the indices within that time window 
          # that match the start and end of the next sustained inactivity bout 
          pr0 = 1
          pr1 = pr0 + ((60/ws3)*1440*6)
          pr2 = length(time)
          if (nrow(S2) > 0) {
            gik.ons = as.character(S2$sib.onset.time)
            gik.end = as.character(S2$sib.end.time)
            for (g in 1:nrow(S2)) { # sustained inactivity bouts
              lastpr0 = pr0
              pr1 = pr0 + ((60/ws3)*1440*6)
              if (pr1 > pr2) pr1 = pr2
              if (pr0 > pr1) pr0 = pr1
              #Coerce time into iso8601 format, so it is sensitive to daylight saving times when hours can be potentially repeated
              timebb = as.character(time[pr0:pr1])
              if (is.ISO8601(timebb[1]) == FALSE) { # only do this for POSIX format
                timebb = POSIXtime2iso8601(timebb,tz=desiredtz)
              }
              s0 = which(timebb == gik.ons[g])[1]
              s1 = which(timebb == gik.end[g])[1]
              if ( timebb[1] != as.character(timebb[1])){ #not s0 because s0 does not exist yet if classes differ
                timebb = as.character(timebb)
                s0 = which(timebb == gik.ons[g])[1]
                s1 = which(timebb == gik.end[g])[1]
              }
              if (is.na(s0) == TRUE) s0 = which(timebb == paste(gik.ons[g]," 00:00:00",sep=""))[1]
              if (is.na(s1) == TRUE) s1 = which(timebb == paste(gik.end[g]," 00:00:00",sep=""))[1]
              # add pr0 to make s0 and s1 be relative to the start of the recording
              s0 = s0 + pr0 - 1
              s1 = s1 + pr0 - 1
              pr0 = s1
              if (length(s1) != 0 & length(s0) != 0 & is.na(s0) == FALSE & is.na(s1) == FALSE) {
                s0s1 = c(s0s1,s0:s1)
              } else {
                pr0 = lastpr0 + ((60/ws3)*1440*6)
              }
            }
          }
          sibdetection[s0s1] = 1
          # extract time and from that the indices for midnights
          time_POSIX = as.POSIXlt(iso8601chartime2POSIX(time,tz=desiredtz),tz=desiredtz)
          tempp = unclass(time_POSIX)
          if (is.na(tempp$sec[1]) == TRUE) {
            tempp = unclass(as.POSIXlt(time,tz=desiredtz))
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
          # following code was move to here, because otherwise it would repeated remove the last night in the loop          
          #           ignore last night because the following day is potentially not complete e.g. by reduce protocol compliance
          #=========
          # If excludefirstlast was set to TRUE in part 4 then 
          # SUSTAINED INACTIVITY BOUTS are not assessed for the time between
          # the first midnight and the first noon
          # because it is not used for sleep reports (first night is ignored in this case).
          # Therefore, expand the SIB sibdetection to include this time segment.
          #----------------------------------------------------------------------
          # Step 1: Identify time window that needs to be processed
          redo1 = nightsi[1] - ((60/ws3)*60) # 1 hour before first midnight
          if (redo1 < 1) redo1 = 1
          redo2 = nightsi[1] + (14*(60/ws3)*60) # 14 hours after first midngiht
          # Specify defintion of sustained inactivity bout
          anglethreshold = as.numeric(unlist(strsplit(j,"A"))[2])
          tempi = unlist(strsplit(unlist(strsplit(j,"A"))[1],"T"))
          timethreshold = as.numeric(tempi[length(tempi)])
          Nsleep = length(timethreshold) * length(anglethreshold)
          sleep = matrix(0,length(angle[redo1:redo2]),Nsleep)
          angle[which(is.na(angle[redo1:redo2]) == T)] = 0
          sdl1 = rep(0,length(time[redo1:redo2]))
          postch = which(abs(diff(angle[redo1:redo2])) > anglethreshold) #posture change of at least j degrees
          # count posture changes that happen less than once per ten minutes
          q1 = c()
          if (length(postch) > 1) {
            q1 = which(diff(postch) > (timethreshold*(60/ws3))) #less than once per i minutes
          }
          if (length(q1) > 0) {
            for (gi in 1:length(q1)) {
              sdl1[postch[q1[gi]]:postch[q1[gi]+1]] = 1 #periods with no posture change
            }
          } else { #possibly a day without wearing
            if (length(postch) < 5) {  #possibly a day without wearing
              sdl1[1:length(sdl1)] = 1 #periods with no posture change
            } else {  #possibly a day with constantly posture changes
              sdl1[1:length(sdl1)] = 0 #periodsposture change
            }
          }
          # update variable sibdetection
          if (redo2 > length(sibdetection)) {
            delta = redo2 - length(sibdetection)
            redo2 = length(sibdetection)
            sdl1 = sdl1[1:(length(sdl1)-delta)]
          }
          if (redo1 > length(sibdetection)) {
            redo1 = length(sibdetection)
            sdl1 = sdl1[1]
          }
          sibdetection[redo1:redo2] = sdl1
          #========================================================
          # Added 24 March 2020, for the rare situation when part 4 does misses a night.
          # This is possible when the accelerometer was not worn the entire day and
          # ignorenonwear was set to TRUE, part4 then gives up and does not try to store anything.
          # rather than fiddling with part 4 again it seems more logical to address this here,
          # because only part 5 needs this.
          potentialnight = min(summarysleep_tmp2$night):max(summarysleep_tmp2$night)
          missingnight = which(as.numeric(potentialnight) %in% as.numeric(summarysleep_tmp2$night) == FALSE)
          if (length(missingnight) > 0) {
            for (mi in missingnight) {
              missingNight = potentialnight[mi]
              newnight = summarysleep_tmp2[1,]
              newnight[which(names(newnight) %in% c("id","night", "sleepparam","filename","filename_dir","foldername") == FALSE)] = NA
              newnight$wakeup = newnight$guider_wakeup = newnight$sleeponset = newnight$guider_onset = NA
              newnight$night = missingNight
              newnight$calendardate = format(as.Date(as.POSIXlt(summarysleep_tmp2$calendardate[mi-1],format="%d/%m/%Y") + (36*3600)), "%d/%m/%Y")
              timesplit = as.numeric(unlist(strsplit(as.character(newnight$calendardate),"/"))) # remove leading zeros
              newnight$calendardate = paste0(timesplit[1],"/",timesplit[2],"/",timesplit[3])
              newnight$daysleeper = 0
              newnight$acc_available = 0
              if (length(sleeplog) > 0) { # we impute with sleeplog (TO DO: Also implement catch for if sleeplog is not available)
                sleeplogonset = sleeplog$sleeponset[which(sleeplog$id == id & sleeplog$night == missingNight)]
                sleeplogwake = sleeplog$sleepwake[which(sleeplog$id == id & sleeplog$night == missingNight)]
                if (length(sleeplogonset) != 0 & length( sleeplogwake) != 0) {
                  sleeplogonset_hr = clock2numtime(sleeplogonset)
                  sleeplogwake_hr= clock2numtime(sleeplogwake)
                }
                newnight$sleeponset = newnight$guider_onset = sleeplogonset_hr
                newnight$wakeup = newnight$guider_wakeup = sleeplogwake_hr
                if (sleeplogwake_hr > 36) {
                  newnight$daysleeper = 1
                }
                hr_to_clocktime = function(x) {
                  hrsNEW = floor(x)
                  minsUnrounded = (x - hrsNEW) *60
                  minsNEW =  floor(minsUnrounded)
                  secsNEW =  floor( (minsUnrounded - minsNEW)*60)
                  if (minsNEW < 10) minsNEW = paste0(0,minsNEW)
                  if (secsNEW < 10) secsNEW = paste0(0,secsNEW)
                  if (hrsNEW < 10) hrsNEW = paste0(0,hrsNEW)
                  time = paste0(hrsNEW,":",minsNEW,":",secsNEW)
                  return(time)
                }
                newnight$sleeponset_ts = hr_to_clocktime(sleeplogonset_hr)
                newnight$wakeup_ts = hr_to_clocktime(sleeplogwake_hr)
                newnight$sleeplog_used = 1
                newnight$guider = "sleeplog"
              } else {
                newnight$sleeplog_used = 0
                newnight$guider = "nosleeplog_accnotworn"
              }
              newnight$cleaningcode = 5
              summarysleep_tmp2 = rbind(summarysleep_tmp2[1:(mi-1),],
                                        newnight,
                                        summarysleep_tmp2[mi:nrow(summarysleep_tmp2),])
            }
          }
          #========================================================
          # DIURNAL BINARY CLASSIFICATION INTO NIGHT (onset-wake) OR DAY (wake-onset) PERIOD 
          # Note that the sleep date timestamp corresponds to day before night
          w0 = w1 = rep(0,length(summarysleep_tmp2$calendardate))
          diur = rep(0,length(time)) #diur is a variable to indicate the diurnal rhythm: 0 if wake/daytime, 1 if sleep/nighttime
          pko = which(summarysleep_tmp2$sleeponset == 0 & summarysleep_tmp2$wakeup == 0 & summarysleep_tmp2$SptDuration == 0)
          if (length(pko) > 0) {
            summarysleep_tmp2 = summarysleep_tmp2[-pko,]
          }
          if (nrow(summarysleep_tmp2) > 0) {
            for (k in 1:length(summarysleep_tmp2$calendardate)){ # loop through nights from part 4
              # Load sleep onset and waking time from part 4 and convert them into timestamps
              tt = unlist(strsplit(as.character(summarysleep_tmp2$calendardate[k]),"/")) # calendar date
              w0[k] = paste(tt[3],"-",tt[2],"-",tt[1]," ",as.character(summarysleep_tmp2$sleeponset_ts[k]),sep="")
              w1[k] = paste(tt[3],"-",tt[2],"-",tt[1]," ",as.character(summarysleep_tmp2$wakeup_ts[k]),sep="")
              # if time is beyond 24 then change the date
              if (summarysleep_tmp2$sleeponset[k] >= 24) { 
                w0[k] = as.character(as.POSIXlt(w0[k],tz=desiredtz) + (24*3600))
              }
              if (summarysleep_tmp2$wakeup[k] >= 24 |
                  (summarysleep_tmp2$daysleeper[k] == 1 & summarysleep_tmp2$wakeup[k] < 18)) {
                w1[k] = as.character(as.POSIXlt(w1[k],tz=desiredtz) + (24*3600))
              }
              w0c = as.character(as.POSIXlt(w0[k],tz=desiredtz))
              w1c = as.character(as.POSIXlt(w1[k],tz=desiredtz))
              s0 = which(as.character(time) == w0c)[1]
              s1 = which(as.character(time) == w1c)[1]
              if (length(s0) == 0) {
                w0c = paste0(w0c," 00:00:00")
                s0 = which(as.character(time) == w0c)[1]
              }
              if (length(s1) == 0) {
                w1c = paste0(w1c," 00:00:00")
                s1 = which(as.character(time) == w1c)[1]
              }
              timebb = as.character(time) 
              if (is.ISO8601(timebb[1]) == TRUE) { # only do this for ISO8601 format
                timebb = iso8601chartime2POSIX(timebb,tz=desiredtz)
                s0 = which(as.character(timebb) == w0c)[1]
                s1 = which(as.character(timebb) == w1c)[1]
                if (length(s0) == 0) {
                  w0c = paste0(w0c," 00:00:00")
                  s0 = which(as.character(timebb) == w0c)[1]
                }
                if (length(s1) == 0) {
                  w1c = paste0(w1c," 00:00:00")
                  s1 = which(as.character(timebb) == w1c)[1]
                }
              }
              if (is.na(s0) == TRUE) {
                s0 = which(timebb == paste(w0c," 00:00:00",sep=""))[1]
                if (is.na(s0) == TRUE) {
                  s0 = which(as.character(timebb) == paste(w0c," 00:00:00",sep=""))[1]
                }
              }
              if (is.na(s1) == TRUE) {
                s1 = which(timebb == paste(w1c," 00:00:00",sep=""))[1]
                if (is.na(s1) == TRUE) {
                  s1 = which(as.character(timebb) == paste(w1c," 00:00:00",sep=""))[1]
                }
              }
              if (length(s1) != 0 & length(s0) != 0 & is.na(s0) == FALSE & is.na(s1) == FALSE) {
                distance2midnight = abs(nightsi - s1) + abs(nightsi - s0)
                closestmidnighti = which.min(distance2midnight)
                closestmidnight = nightsi[closestmidnighti]
                noon0 = closestmidnight - (12* (60/ws3) * 60)
                noon1 = closestmidnight + (12* (60/ws3) * 60)
                if (noon0 < 1) noon0 = 1
                if (noon1 > length(nonwear)) noon1 = length(nonwear)
                nonwearpercentage = mean(nonwear[noon0:noon1])
                if (length(sleeplog) > 0 & nonwearpercentage > 0.33) {
                  # If non-wear is high for this day and if sleeplog is available
                  sleeplogonset = sleeplog$sleeponset[which(sleeplog$id == id & sleeplog$night == summarysleep_tmp2$night[k])]
                  sleeplogwake = sleeplog$sleepwake[which(sleeplog$id == id & sleeplog$night == summarysleep_tmp2$night[k])]
                  if (length(sleeplogonset) != 0 & length( sleeplogwake) != 0) {
                    # ... and if there is sleeplog data for the relevant night
                    # rely on sleeplog for defining the start and end of the night
                    sleeplogonset_hr = clock2numtime(sleeplogonset)
                    sleeplogwake_hr= clock2numtime(sleeplogwake)
                    # express hour relative to midnight within the noon-noon:
                    if (sleeplogonset_hr > 12) {
                      sleeplogonset_hr = sleeplogonset_hr - 24
                    }
                    if (sleeplogwake_hr > 18 & summarysleep_tmp2$daysleeper[k] == 1) {
                      sleeplogwake_hr = sleeplogwake_hr - 24 # 18 because daysleepers can wake up after 12
                    } else if (sleeplogwake_hr > 12 & summarysleep_tmp2$daysleeper[k] == 0) {
                      sleeplogwake_hr = sleeplogwake_hr - 24
                    }
                    s0 = closestmidnight + round(sleeplogonset_hr * Nepochsinhour)
                    s1 = closestmidnight + round(sleeplogwake_hr * Nepochsinhour)
                  }
                }
                diur[s0:(s1-1)] = 1
              }
            }
            # Note related to if first and last night were ignored in part 4:
            # - diur lack sthe first and last night at this point in the code.
            # - nightsi has all the midnights, so it is possible to check here
            # whether a wakeup time is missing on the first full day.
            # - if it is missing, then we will impute it in order for part5 to
            # the wake-to-wake analys on the second recording day.
            # Previously we accounted only for this later on in the code, whcih
            # did not benefit the experoted timeseries.
            firstwake = which(diff(diur) == -1)[1]
            firstonset = which(diff(diur) == 1)[1]
            # test whether wake for second day is missing
            # if the full sleep period happens before midnights
            if (firstwake > nightsi[2] | (summarysleep_tmp2$sleeponset[1] < 18 & summarysleep_tmp2$wakeup[1] < 18 & firstwake < nightsi[2])) { 
              if (length(sleeplog) > 0) {
                # use sleeplog for waking up after first night
                wake_night1 = sleeplog$sleepwake[which(sleeplog$id == id & sleeplog$night == 1)]
                wake_night1_index =c()
                if (length(wake_night1) != 0) {
                  wake_night1_hour = clock2numtime(wake_night1)
                  # express hour relative to midnight within the noon-noon:
                  if (wake_night1_hour > 12) wake_night1_hour = wake_night1_hour - 24 # express hour relative to midnight
                  wake_night1_index = nightsi[1] + round(wake_night1_hour * Nepochsinhour)
                  if (wake_night1_index > length(diur)) wake_night1_index = length(diur)
                  if (wake_night1_index < 1) wake_night1_index = 1
                } else { # use HDCZA algorithm as plan B
                  wake_night1_index = round(sptwindow_HDCZA_end[1] * Nepochsinhour)
                }
              }
              if (length(sptwindow_HDCZA_end) > 0 & length(sleeplog) == 0) {
                # use HDCZA algortihm for waking up after first night
                # if there was no sleep log
                wake_night1_index = round(sptwindow_HDCZA_end[1] * Nepochsinhour)
              }
              if (length(wake_night1_index) == 0) {
                # use waking up from next day and subtract 24 hours,
                # the final option if neither of the above routes works
                wake_night1_index = (firstwake - (24* ((60/ws3)*60))) + 1
              }
              if (wake_night1_index < firstwake) {
                diur[nightsi[1]:(wake_night1_index-1)] = 1
              } else {
                # Person slept only during the afternoon on day 2
                # And there is no sleep data available for the first night
                # Add 5 minutes of dummy waking time before it but consider this 
                # non-wear
                # We do this to make sure that the day numbering
                # and merging of the sleep variables is still consistent with
                # the other cases.
                dummywake = max(firstonset - round(Nepochsinhour/12), nightsi[1])
                diur[nightsi[1]:dummywake] = 1 
                nonwear[nightsi[1]:firstonset] = 1
              }
            }
            
            # Optionally aggregate to 1 minute epoch (implemented for ANR project)
            # print(length(time))
            # print(length(diur))
            # print(length(sibdetection))
            # print(length(ACC))
            
            for (TRLi in threshold.lig) {
              for (TRMi in threshold.mod) {
                for (TRVi in threshold.vig) {
                  # derive behavioral levels (class), e.g. MVPA, inactivity bouts, etc.
                  levels = identify_levels(time,diur,sibdetection,ACC,
                                           TRLi,TRMi,TRVi,
                                           boutdur.mvpa,boutcriter.mvpa,
                                           boutdur.lig,boutcriter.lig,
                                           boutdur.in,boutcriter.in,
                                           ws3,bout.metric)
                  LEVELS = levels$LEVELS
                  OLEVELS = levels$OLEVELS
                  Lnames = levels$Lnames
                  bc.mvpa = levels$bc.mvpa
                  bc.lig = levels$bc.lig
                  bc.in = levels$bc.in
                  if (save_ms5rawlevels == TRUE) {
                    rawlevels_fname = paste(metadatadir,ms5.outraw,"/",fnames.ms3[i],"_",TRLi,"_",TRMi,"_",TRVi,"raw.csv",sep="")
                    if (length(time) == length(LEVELS)) {
                      ind = 1:length(time) #c(1,which(diff(LEVELS)!=0) + 1)
                      ms5rawlevels = data.frame(date_time = time[ind],class_id = LEVELS[ind], class_name = rep("",length(time)),stringsAsFactors = FALSE)
                      for (LNi in 1:length(Lnames)) {
                        replacev = which(ms5rawlevels$class_id == (LNi-1))
                        if (length(replacev) > 0) ms5rawlevels$class_name[replacev] = Lnames[LNi]
                      }
                      write.csv(ms5rawlevels,file = rawlevels_fname,row.names = FALSE)
                      rm(ms5rawlevels)
                    }
                  }
                  #=============================================
                  # NOW LOOP TROUGH DAYS AND GENERATE DAY SPECIFIC SUMMARY VARIABLES
                  # we want there to be one more nights in the accelerometer data than there are nights with sleep results
                  NNIGHTSSLEEP = length(unique(summarysleep_tmp2$calendardate)) # nights with sleep results
                  NNIGHTSACC = length(nightsi) #acc
                  #-------------------------------
                  # ignore all nights in 'inights' before the first waking up and after the last waking up
                  FM = which(diff(diur) == -1)                  
                  nightsi_bu = nightsi
                  # now 0.5+6+0.5 midnights and 7 days
                  for (timewindowi in timewindow) {
                    nightsi = nightsi_bu 
                    plusrow = 1
                    if (timewindowi == "WW") {
                      if (length(FM) > 0) {
                        # ignore first and last midnight because we did not do sleep detection on it
                        nightsi = nightsi[which(nightsi > FM[1] & nightsi < FM[length(FM)])]
                      }
                    } else {
                      # newly added on 31-3-2019, because if first night is missing then nights needs to allign with diur
                      startend_sleep = which(abs(diff(diur))==1) 
                      Nepochsin12Hours =  (60/ws3)*60*12
                      nightsi = nightsi[which(nightsi >= (startend_sleep[1] - Nepochsin12Hours) &
                                              nightsi <= (startend_sleep[length(startend_sleep)] + Nepochsin12Hours))]  # newly added on 25-11-2019
                      #nightsi = nightsi[which(nightsi >= startend_sleep[1] & nightsi <= startend_sleep[length(startend_sleep)])]
                    }
                    if (timewindowi == "MM") {
                      Nwindows = nrow(summarysleep_tmp2)+plusrow
                    } else {
                      Nwindows = length(which(diff(diur) == -1))
                    }
                    indjump = 1
                    for (wi in 1:Nwindows) { #loop through 7 windows (+1 to include the data after last awakening)
                      # Check that this is a meaningful day (that is all the qqq variable is used for),
                      # before storing it.
                      qqq = rep(0,2)
                      # Check that it is possible to find both windows (WW and MM)
                      # in the data for this day.
                      if (timewindowi == "MM") {
                        if (wi==1) {
                          qqq[1] = 1
                          qqq[2] = nightsi[wi]
                        } else if (wi<=nrow(summarysleep_tmp2)) {
                          qqq[1] = nightsi[wi-1] + 1
                          qqq[2] = nightsi[wi]
                          qqq_backup = qqq
                        } else if (wi>nrow(summarysleep_tmp2)) {
                          qqq[1] = qqq_backup[2] + 1
                          if (wi <= length(nightsi)) {
                            qqq[2] = nightsi[wi]
                          } else {
                            qqq[2] = nightsi_bu[which(nightsi_bu == nightsi[wi-indjump]) + indjump]
                            indjump = indjump + 1 # in case there are multiple days beyond nightsi
                            if (is.na(qqq[2])) { # if that does not work use last midnight and add 24 hours
                              qqq[2] = nightsi_bu[which(nightsi_bu == nightsi[wi-(indjump-1)]) + (indjump-1)] + (24*(60/ws3) * 60) -1
                            }
                            if (is.na(qqq[2])) { # if that does not work use last midnight and add 24 hours
                              qqq[2] = qqq_backup[2] + (24*(60/ws3) * 60) -1
                            }
                            if (qqq[1] == qqq[2])  qqq[2] = qqq[2] + (24*(60/ws3) * 60) - 1
                          }
                          if(is.na(qqq[2])==TRUE | length(diur) < qqq[2]) {
                            qqq[2] = length(diur)
                          }
                        }
                      } else if(timewindowi == "WW") {
                        if (wi <=(Nwindows-1)) { # all full wake to wake days
                          qqq[1] = which(diff(diur) == -1)[wi] + 1
                          qqq[2] = which(diff(diur) == -1)[wi+1]
                          
                        } else {
                          # time after last reliable waking up (this can be more than 24 hours)
                          # ignore this day, because if the night was ignored for sleep analysis
                          # then the description of the day in part 5 including that night is
                          # not informative.
                          qqq = c(NA, NA)
                        }
                      }
                      if (length(which(is.na(qqq)==TRUE)) == 0) { #if it is a meaningful day then none of the values in qqq should be NA
                        fi = 1
                        # START STORING BASIC INFORMATION
                        dsummary[di,fi] = id #summarysleep_tmp2$id[1]
                        ds_names[fi] = "id";      fi = fi + 1
                        dsummary[di,fi] = fnames.ms3[i]
                        ds_names[fi] = "filename";      fi = fi + 1
                        dsummary[di,fi] = wi
                        ds_names[fi] = "window_number";      fi = fi + 1
                        if (timewindowi == "WW") {
                          plusb = 0
                        } else {
                          plusb = 1
                        }
                        skiponset = skipwake = TRUE
                        onset = wake = 0
                        #==========================
                        # Newly added to avoid issue with merging sleep
                        # variables from part 4, we simply extract them 
                        # from the new time series
                        # Note that this means that for MM windows there can be multiple or no wake or onsets
                        date = as.Date(as.POSIXlt(time[qqq[1]+1],tz=desiredtz))
                        weekday = weekdays(date)
                        dsummary[di,fi:(fi+1)] = c(weekday, as.character(date))
                        ds_names[fi:(fi+1)] = c("weekday","calendardate");fi = fi + 2
                        # Onset index
                        if (timewindowi == "WW") {
                          onseti = c(qqq[1]:qqq[2])[which(diff(diur[qqq[1]:(qqq[2]-1)]) == 1)+1]
                          if (length(onseti) > 1) {
                            onseti = onseti[length(onseti)] # in the case if MM use last onset
                          }
                        } else {
                          onseti = c(qqq[1]:qqq[2])[which(diff(diur[qqq[1]:(qqq[2]-1)]) == 1)+1]
                          if (length(onseti) > 1) {
                            onseti = onseti[length(onseti)] # in the case if MM use last onset
                          }
                        }
                        # Wake index
                        if (timewindowi == "WW") {
                          wakei = qqq[2]+1
                        } else {
                          wakei = c(qqq[1]:qqq[2])[which(diff(diur[qqq[1]:(qqq[2]-1)]) == -1)+1]
                          if (length(wakei) > 1) wakei = wakei[1] # in the case if MM use first wake-up time
                        }
                        # Onset time
                        if (length(onseti) == 1) { # in MM window it is possible to not have an onset
                          if (is.na(onseti) == FALSE) {
                            onset = hour[onseti] + (min[onseti]/60) + (sec[onseti]/3600)
                            skiponset = FALSE
                          }
                        }
                        # Wake time
                        if (length(wakei) == 1) { # in MM window it is possible to not have a wake
                          if (is.na(wakei) == FALSE) {
                            wake = hour[wakei] + (min[wakei]/60) + (sec[wakei]/3600)
                            skipwake = FALSE
                          }
                        }
                        if (wake > 12 & wake < 18) { # daysleeper and onset in the morning or afternoon
                          if (onset < 18 & skiponset == FALSE) onset = onset + 24
                          if (wake < 18 & skipwake == FALSE) wake = wake + 24
                        } else if (wake <= 12) { # no daysleeper, but onset before noon
                          if (onset < 12 & skiponset == FALSE) onset = onset + 24
                          if (wake < 12 & skipwake == FALSE) wake = wake + 24
                        }
                        if (wake > 12 & onset < 18 & skiponset == FALSE) onset = onset + 24
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
                        recDates = as.Date(summarysleep_tmp2$calendardate, format="%e/%m/%Y")
                        dsummary[di,fi] = j
                        ds_names[fi] = "sleepparam";      fi = fi + 1
                        dayofinterst = which(recDates == date)
                        if (length(dayofinterst) > 0) {
                          dsummary[di,fi] = summarysleep_tmp2$night[dayofinterst]
                          ds_names[fi] = "night_number";      fi = fi + 1
                          dsummary[di,fi] = summarysleep_tmp2$daysleeper[dayofinterst]
                          ds_names[fi] = "daysleeper";      fi = fi + 1
                          dsummary[di,fi] = summarysleep_tmp2$cleaningcode[dayofinterst]
                          ds_names[fi] = "cleaningcode";      fi = fi + 1
                          dsummary[di,fi] = summarysleep_tmp2$guider[dayofinterst]
                          ds_names[fi] = "guider";      fi = fi + 1
                          dsummary[di,fi] = summarysleep_tmp2$sleeplog_used[dayofinterst]
                          ds_names[fi] = "sleeplog_used";      fi = fi + 1
                          dsummary[di,fi] = summarysleep_tmp2$acc_available[dayofinterst]
                          ds_names[fi] = "acc_available";      fi = fi + 1
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
                        wlih = ((qqq2-qqq1)+1)/((60/ws3)*60)
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
                        # dsummary[di,fi] = (length(which(nonwear[qqq1:qqq2] == 1 & diur[qqq1:qqq2] == 0)) / length(which(diur[qqq1:qqq2] == 0)))  *100
                        # ds_names[fi] = "nonwear_perc_waking";      fi = fi + 1
                        # dsummary[di,fi] = (length(which(nonwear[qqq1:qqq2] == 1 & diur[qqq1:qqq2] == 1)) / length(which(diur[qqq1:qqq2] == 1))) * 100
                        # ds_names[fi] = "nonwear_perc_SPT";      fi = fi + 1
                        #============================================================
                        # percentage of available data
                        zt_hrs_nonwear = (length(which(diur[qqq1:qqq2] == 0 & nonwear[qqq1:qqq2] == 1)) * ws3) / 3600 #day
                        zt_hrs_total = (length(which(diur[qqq1:qqq2] == 0)) * ws3) / 3600 #day
                        dsummary[di,fi] = (zt_hrs_nonwear/zt_hrs_total)  * 10000 / 100
                        ds_names[fi] = "nonwear_perc_wakinghours";      fi = fi + 1
                        zt_hrs_nonwear = (length(which(diur[qqq1:qqq2] == 1 & nonwear[qqq1:qqq2] == 1)) * ws3) / 3600 #night
                        zt_hrs_total = (length(which(diur[qqq1:qqq2] == 1)) * ws3) / 3600 #night
                        dsummary[di,fi] =  (zt_hrs_nonwear/zt_hrs_total)  * 10000 / 100
                        ds_names[fi] = "nonwear_perc_sleepperiod";      fi = fi + 1
                        zt_hrs_nonwear = (length(which(nonwear[qqq1:qqq2] == 1)) * ws3) / 3600
                        zt_hrs_total = (length(diur[qqq1:qqq2]) * ws3) / 3600 #night and day
                        dsummary[di,fi] =  (zt_hrs_nonwear/zt_hrs_total)  * 10000 / 100
                        ds_names[fi] = "nonwear_perc_fulldaywindow";      fi = fi + 1
                        #===============================================
                        # TIME SPENT IN WINDOWS (window is either midnight-midnight or waking up-waking up)
                        test_remember = c(di,fi)
                        for (levelsc in 0:(length(Lnames)-1)) {
                          dsummary[di,fi] = (length(which(LEVELS[qqq1:qqq2] == levelsc)) * ws3) / 60
                          ds_names[fi] = paste0("dur_",Lnames[levelsc+1],"_min");      fi = fi + 1
                        }
                        for (g in 1:5) {
                          dsummary[di,(fi+(g-1))] = (length(which(OLEVELS[qqq1:qqq2] == g )) * ws3) / 60
                        }
                        ds_names[fi:(fi+4)] = c("dur_TSIBday_min","dur_TOINday_min",
                                                "dur_TLIGday_min","dur_TMODday_min","dur_TVIGday_min")
                        fi = fi + 5
                        dsummary[di,fi] = (length(which(OLEVELS[qqq1:qqq2] == 1 | OLEVELS[qqq1:qqq2] == 2)) * ws3) / 60
                        ds_names[fi] = "dur_TINday_min";      fi = fi + 1 #total inactivity (SIB or OIN)
                        dsummary[di,fi] = (length(which(diur[qqq1:qqq2] == 0)) * ws3) / 60
                        ds_names[fi] = "dur_wakinghours_min";      fi = fi + 1
                        dsummary[di,fi] = (length(which(diur[qqq1:qqq2] == 1)) * ws3) / 60
                        ds_names[fi] = "dur_sleepperiod_min";      fi = fi + 1
                        dsummary[di,fi] = (length(c(qqq1:qqq2)) * ws3) / 60
                        ds_names[fi] = "dur_fulldaywindow_min";      fi = fi + 1
                        
                        #============================================
                        # Number of long wake periods (defined as > 5 minutes) during the night
                        Nawake = length(which(abs(diff(which(LEVELS[qqq1:qqq2] == 0))) > (300 / ws3))) - 2
                        if (Nawake < 0) Nawake = 0
                        dsummary[di,fi] = Nawake
                        ds_names[fi] = "N_atleast5minwakenight";      fi = fi + 1
                        #=============================
                        # sleep efficiency
                        dsummary[di,fi] = length(which(sibdetection[qqq1:qqq2] == 1 & diur[qqq1:qqq2] == 1)) / length(which(diur[qqq1:qqq2] == 1))
                        ds_names[fi] = "sleep_efficiency";      fi = fi + 1
                        #===============================================
                        # AVERAGE ACC PER WINDOW
                        sse = qqq1:qqq2
                        for (levelsc in 0:(length(Lnames)-1)) {
                          dsummary[di,fi] = mean(ACC[sse[which(LEVELS[sse] == levelsc)]])
                          ds_names[fi] = paste("ACC_",Lnames[levelsc+1],"_mg",sep="");      fi = fi + 1
                        }
                        for (g in 1:5) {
                          dsummary[di,(fi+(g-1))] = mean(ACC[sse[which(OLEVELS[sse] == g)]])
                        }
                        ds_names[fi:(fi+4)] = c("ACC_TSIBday_mg","ACC_TOINday_mg","ACC_TLIGday_mg","ACC_TMODday_mg","ACC_TVIGday_mg")
                        fi = fi + 5
                        dsummary[di,fi] = mean(ACC[sse[which(OLEVELS[sse] == 1 | OLEVELS[sse] == 2)]])
                        ds_names[fi] = "ACC_TINday_min";      fi = fi + 1 #total inactivity (SIB or OIN)
                        dsummary[di,fi] = mean(ACC[sse[which(diur[sse] == 0)]])
                        ds_names[fi] = "ACC_wakinghours_mg";      fi = fi + 1
                        dsummary[di,fi] = mean(ACC[sse[which(diur[sse] == 1)]])
                        ds_names[fi] = "ACC_sleepperiod_mg";      fi = fi + 1
                        dsummary[di,fi] = mean(ACC[sse])
                        ds_names[fi] = "ACC_fulldaywindow_mg";      fi = fi + 1             
                        #===============================================
                        # QUANTILES...
                        # sse = qqq1:qqq2
                        WLH = ((qqq2-qqq1)+1)/((60/ws3)*60) #windowlength_hours = 
                        if (WLH <= 1) WLH = 1.001
                        dsummary[di,fi] = quantile(ACC[sse],probs=((WLH-1)/WLH),na.rm=TRUE)
                        ds_names[fi] = paste("quantile_mostactive60min_mg",sep="");      fi = fi + 1
                        dsummary[di,fi] = quantile(ACC[sse],probs=((WLH-0.5)/WLH),na.rm=TRUE)
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
                              e1 = (hri*reso*(60/ws3))+1
                              e2 = (hri+(wini*(60/reso)))*reso*(60/ws3)
                              if (e2 > length(sse)) e2 = length(sse)
                              ACCrunwin[(hri+1),1] = mean(ACC[sse[e1:e2]])
                              TIMErunwin[(hri+1),1]= as.character(time[sse[e1]])
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
                          ds_names[fi+(bci-1)] = paste0("Nbouts_MVPA_D",boutdur.mvpa[bci],"T",TRMi)
                        }
                        fi = fi + bci
                        bc.in = checkshape(bc.in)
                        for (bci in 1:nrow(bc.in)) {
                          dsummary[di,fi+(bci-1)] = length(which(diff(bc.in[bci,])[sse] == 1))
                          ds_names[fi+(bci-1)] = paste0("Nbouts_INB_D",boutdur.in[bci],"T",TRLi)
                        }
                        fi = fi + bci
                        bc.lig = checkshape(bc.lig)
                        for (bci in 1:nrow(bc.lig)) {
                          dsummary[di,fi+(bci-1)] = length(which(diff(bc.lig[bci,])[sse] == 1))
                          ds_names[fi+(bci-1)] = paste0("Nbouts_LIGB_D",boutdur.lig[bci],"T",TRLi,"_",TRMi)
                        }
                        fi = fi + bci
                        #===============================================
                        # NUMBER OF WINDOWS
                        for (levelsc in 0:(length(Lnames)-1)) {
                          dsummary[di,fi] = length(which(diff(which(LEVELS[sse] != levelsc)) > 1)) #qqq1:qqq2
                          if (dsummary[di,fi] == 0 & LEVELS[qqq1] == levelsc) dsummary[di,fi] = 1
                          ds_names[fi] = paste("Nblocks_",Lnames[levelsc+1],sep="");      fi = fi + 1
                        }
                        for (g in 1:5) {
                          dsummary[di,(fi+(g-1))] = length(which(diff(which(OLEVELS[qqq1:qqq2] != g))> 1))
                        }
                        ds_names[fi:(fi+4)] = c("Nblocks_TSIBday","Nblocks_TOINday","Nblocks_TLIGday","Nblocks_TMODday","Nblocks_TVIGday")
                        fi = fi + 5
                        dsummary[di,fi] = length(which(diff(which(OLEVELS[qqq1:qqq2] != 1 & OLEVELS[qqq1:qqq2] != 2))> 1))
                        ds_names[fi] = "Nblocks_TINday";      fi = fi + 1 #total inactivity (SIB or OIN)
                        dsummary[di,fi] = boutcriter.in 
                        ds_names[fi] = "boutcriter.in"; fi = fi + 1 
                        dsummary[di,fi] = boutcriter.lig 
                        ds_names[fi] = "boutcriter.lig"; fi = fi + 1 
                        dsummary[di,fi] = boutcriter.mvpa 
                        ds_names[fi] = "boutcriter.mvpa"; fi = fi + 1 
                        dsummary[di,fi] = paste(boutdur.in,collapse="_")
                        ds_names[fi] = "boutdur.in"; fi = fi + 1 
                        dsummary[di,fi] = paste(boutdur.lig,collapse="_")
                        ds_names[fi] = "boutdur.lig"; fi = fi + 1 
                        dsummary[di,fi] = paste(boutdur.mvpa,collapse="_")
                        ds_names[fi] = "boutdur.mvpa"; fi = fi + 1 
                        dsummary[di,fi] = bout.metric
                        ds_names[fi] = "bout.metric"; fi = fi + 1 
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
        
        if (ncol(output) > lastcolumn) {
          emptycols = sapply(output, function(x)all(x==""))# Find columns filled with missing values which(output[1,] == "" & output[2,] == "")
          emptycols = which(emptycols == TRUE)
          if (length(emptycols) > 0) emptycols = emptycols[which(emptycols > lastcolumn)]
          if (length(emptycols) > 0) output = output[-emptycols]
        }
        if (length(output) > 0) {
          save(output,file=paste(metadatadir,ms5.out,"/",fnames.ms3[i],sep=""))
        }
        rm(output,dsummary)
      }
    }
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

