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
                   overwrite=FALSE,desiredtz="Europe/London",bout.metric=4, dayborder = 0, save_ms5rawlevels = FALSE) {
  options(encoding = "UTF-8")
  # description: function called by g.shell.GGIR
  # aimed to merge the milestone output from g.part2, g.part3, and g.part4
  # in order to create a merged report of both physical activity and sleep
  # if store.ms = TRUE then it will work with stored milestone data per accelerometer file
  # if store.ms = FALSE then it will work with the stored spreadsheets from g.part2 and g.part4 and milestone data from part g.part1
  # this distinction is needed to facilitate both parallel analyses of multiple files (set store.ms to TRUE) and to facilitate
  # serial analysis of small data pools
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
  SUM = nightsummary = M = sib.cla.sum= c()
  #======================================================================
  # compile lists of milestone data filenames
  fnames.ms1 = sort(dir(paste(metadatadir,"/meta/basic",sep="")))
  # fnames.ms2 = sort(dir(paste(metadatadir,"/meta/ms2.out",sep="")))
  fnames.ms3 = sort(dir(paste(metadatadir,"/meta/ms3.out",sep="")))
  fnames.ms4 = sort(dir(paste(metadatadir,"/meta/ms4.out",sep="")))
  fnames.ms5 = sort(dir(paste(metadatadir,"/meta/ms5.out",sep="")))
  # fnames.ms5raw = sort(dir(paste(metadatadir,"/meta/ms5.outraw",sep="")))
  # results
  results = paste(metadatadir,"/results",sep="")
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
  # fnames_original= sort(fnames_original)
  if (f1 == 0) length(fnames.ms4)
  if (f1 > length(fnames.ms4)) f1 = length(fnames.ms4)
  #--------------------------------
  # get full file path and folder name if requested by end-user and keep this for storage in output
  if (storefolderstructure == TRUE) {
    extractfilenames = function(x) as.character(unlist(strsplit(x,".RDa"))[1])
    referencefnames = sapply(fnames.ms3,extractfilenames)
    folderstructure = getfolderstructure(datadir,referencefnames)
    fullfilenames = folderstructure$fullfilenames
    foldername = folderstructure$foldername
  }
  #======================================================================
  # loop through milestone data-files (in case of store.ms=TRUE)
  # or filenames stored in output of g.part2 and g.part4 (in case of store.ms=FALSE)
  t0 = t1 = Sys.time()
  for (i in f0:f1) {
    if (length(ffdone) > 0) { #& store.ms == TRUE
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
    if (file.exists(paste(metadatadir,"/meta/ms4.out/",fnames.ms4[selp],sep="")) == FALSE) {
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
      # selp = which(fnames.ms2 == fnames.ms3[i]) # so, fnames.ms3[i] is the reference point for filenames
      # load(file=paste(metadatadir,"/meta/ms2.out/",fnames.ms2[selp],sep=""))
      # daysummary = SUM$daysummary # commented out because not used
      # summary = SUM$summary # commented out  because not used
      # load output g.part4
      selp = which(fnames.ms4 == fnames.ms3[i])
      load(file=paste(metadatadir,"/meta/ms4.out/",fnames.ms4[selp],sep=""))
      summarysleep = nightsummary
      rm(nightsummary)
      idindex = which(summarysleep$filename == fnames.ms3[i])
      id = summarysleep$id[idindex[1]]
      ndays = nrow(summarysleep) #/ length(unique(summarysleep$acc_def))
      dsummary = matrix("",(40*length(unique(summarysleep$acc_def))
                            *length(unique(threshold.lig))
                            *length(unique(threshold.mod))
                            *length(unique(threshold.vig))
                            *length(unique(timewindow))),nfeatures)
      di = 1
      fi = 1
      if (length(idindex) > 0 & nrow(summarysleep) > 1) { #only attempt to load file if it was processed for sleep
        summarysleep_tmp = summarysleep
        #======================================================================
        # load output g.part1
        selp = which(fnames.ms1 == paste("meta_",fnames.ms3[i],sep=""))
        if (length(selp) != 1) {
          cat("Warning: File not processed with part1")
        }
        load(paste0(metadatadir,"/meta/basic/",fnames.ms1[selp]))
        # load output g.part3
        load(paste0(metadatadir,"/meta/ms3.out/",fnames.ms3[i]))
        # extract key variables from the mile-stone data: time, acceleration and elevation angle
        IMP = g.impute(M,I,strategy=strategy,hrs.del.start=hrs.del.start,
                       hrs.del.end=hrs.del.end,maxdur=maxdur)
        time = IMP$metashort[,1]
        ACC = IMP$metashort[,acc.metric] * 1000 #note that this is imputed ACCELERATION because we use this for describing behaviour
        
        if (length(which(names(IMP$metashort) == "anglez")) == 0) {
          cat("Warning: anglez not extracted. Please check that do.anglez == TRUE")
        }
        angle = as.numeric(as.matrix(M$metashort[,which(names(IMP$metashort) == "anglez")])) #note that this is the non-imputed angle because we use this here only for the visualisation
        nonwear = IMP$rout[,5]
        nonwear = rep(nonwear,each=(IMP$windowsizes[2]/IMP$windowsizes[1]))
        if (length(nonwear) > length(time)) {
          nonwear = nonwear[1:length(time)]
        } else if (length(nonwear) < length(time)) {
          nonwear = c(nonwear,rep(0,(length(time)-length(nonwear))))
        }
        rm(IMP,M,I)
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
          Sbackup = S
          S2 = S[which(S$definition==j),] # simplify to one definition
          sibdetection = rep(0,length(time))
          tmp = S2 #[which(S2$night==h),]
          s0s1 = c()
          pr0 = 1
          pr1 = pr0 + ((60/ws3)*1440*6)
          pr2 = length(time)
          if (nrow(S2) > 0) {
            gik.ons = as.character(S2$sib.onset.time)
            gik.end = as.character(S2$sib.end.time)
            for (g in 1:nrow(S2)) { # sleep periods
              lastpr0 = pr0
              pr1 = pr0 + ((60/ws3)*1440*6)
              if (pr1 > pr2) pr1 = pr2
              if (pr0 > pr1) pr0 = pr1
              s0 = which(time[pr0:pr1] == gik.ons[g])[1]
              s1 = which(time[pr0:pr1] == gik.end[g])[1]
              timebb = as.character(time[pr0:pr1]) 
              if(length(unlist(strsplit(timebb[1],"[+]"))) > 1) { # only do this for ISO8601 format
                timebb = iso8601chartime2POSIX(timebb,tz=desiredtz)
              }
              s0 = which(timebb == gik.ons[g])[1]
              s1 = which(timebb == gik.end[g])[1]
              #Change 1 - 03/10/2017
              if ( timebb[1] != as.character(timebb[1])){ #not s0 because s0 does not exist yet if classes differ
                timebb = as.character(timebb)
                s0 = which(timebb == gik.ons[g])[1]
                s1 = which(timebb == gik.end[g])[1]
              }
              #End of change 1
              if (is.na(s0) == TRUE) s0 = which(timebb == paste(gik.ons[g]," 00:00:00",sep=""))[1]
              if (is.na(s1) == TRUE) s1 = which(timebb == paste(gik.end[g]," 00:00:00",sep=""))[1]
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
          tempp = unclass(as.POSIXlt(iso8601chartime2POSIX(time,tz=desiredtz),tz=desiredtz))
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
          summarysleep_tmp2 = summarysleep_tmp[which(summarysleep_tmp$acc_def == j),]
          # following code was move to here, because otherwise it would repeated remove the last night in the loop          
          #           ignore last night because the following day is potentially not complete e.g. by reduce protocol compliance
          #=========
          # SUSTAINED INACTIVITY BOUTS are normally not assessed for the time between the first midnight and the first noon
          # because it is not used for sleep reports (first night is ignored)
          # Therefore, expand the SIB sibdetection to include this time segment.
          # Step 1: Identify time window that needs to be processed
          redo1 = nightsi[1] - ((60/ws3)*60)
          if (redo1 < 1) redo1 = 1
          redo2 = nightsi[1] + (14*(60/ws3)*60)
          # specify defintion of sustained inactivity bout    
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
          # DIURNAL BINARY CLASSIFICATION INTO SLEEP OR WAKE PERIOD 
          # Note that the sleep date timestamp corresponds to day before night
          w0 = w1 = rep(0,length(summarysleep_tmp2$calendardate))
          diur = rep(0,length(time)) #diur is a variable to indicate the diurnal rhythm: 0 if wake/daytime, 1 if sleep/nighttime
          pko = which(summarysleep_tmp2$acc_onset == 0 & summarysleep_tmp2$acc_wake == 0 & summarysleep_tmp2$acc_dur_noc == 0)
          if (length(pko) > 0) {
            summarysleep_tmp2 = summarysleep_tmp2[-pko,]
          }
          if (nrow(summarysleep_tmp2) > 0) {
            for (k in 1:length(summarysleep_tmp2$calendardate)){
              tt = unlist(strsplit(as.character(summarysleep_tmp2$calendardate[k]),"/"))
              w0[k] = paste(tt[3],"-",tt[2],"-",tt[1]," ",as.character(summarysleep_tmp2$acc_onset_ts[k]),sep="")
              w1[k] = paste(tt[3],"-",tt[2],"-",tt[1]," ",as.character(summarysleep_tmp2$acc_wake_ts[k]),sep="")
              if (summarysleep_tmp2$acc_onset[k] >= 24) {
                # I am making the hard assumption here that a day has 24 hours...future improvement needed
                w0[k] = as.character(as.POSIXlt(w0[k],tz=desiredtz) + (24*3600))
              }
              if (summarysleep_tmp2$acc_wake[k] >= 24 |
                  (summarysleep_tmp2$daysleeper[k] == 1 & summarysleep_tmp2$acc_wake[k] < 18)) {
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
              if(length(unlist(strsplit(timebb[1],"[+]"))) > 1) { # only do this for ISO8601 format
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
                diur[s0:s1] = 1
              }
            }
            
            if (diur[1] != 0 & (60/ws3)*60 < nightsi[1]) { # the statement with unique that was here results is ambiguous because it can have multiple values
              # waketi = which(diff(detection) == -1)[1]
              # onseti = which(diff(detection) == 1)[1] # added 20/4/2018 by VvH
              waketi = which(diff(diur) == -1)[1]
              onseti = which(diff(diur) == 1)[1] # added 20/4/2018 by VvH
            } else {
              waketi = 0
              onseti = 0 # added 20/4/2018 by VvH
            }
            if (waketi > 0 & onseti == 0) {
              diur[1:waketi] = 1
            } else if (waketi > 0 & onseti > 0) { # added 20/4/2018 by VvH
              diur[onseti:waketi] = 1
            }
            for (TRLi in threshold.lig) {
              for (TRMi in threshold.mod) {
                for (TRVi in threshold.vig) {
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
                    rawlevels_fname = paste(metadatadir,ms5.outraw,"/",fnames.ms5[i],"_",TRLi,"_",TRMi,"_",TRVi,"raw.csv",sep="")
                    if (length(time) == length(LEVELS)) {
                      ind = 1:length(time) #c(1,which(diff(LEVELS)!=0) + 1)
                      ms5rawlevels = data.frame(date_time = time[ind],class_id = LEVELS[ind])
                      ms5rawlevels[rep(seq_len(nrow(ms5rawlevels)), each=ws3),]
                      # ms5rawlevels$time[1]
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
                    if (timewindowi == "WW") {
                      plusrow = 1
                      if (length(FM) > 0) {
                        # ignore first and last midnight because we did not do sleep detection on it
                        nightsi = nightsi[which(nightsi > FM[1] & nightsi < FM[length(FM)])]
                      }
                    } else {
                      plusrow = 1
                    }
                    for (wi in 1:(nrow(summarysleep_tmp2)+plusrow)) { #loop through 7 windows (+1 to include the data after last awakening)
                      #check that this is a meaningful day
                      qqq = rep(0,2)
                      # check that it is possible to find both windows in the data for this day
                      # qqq definitions changed so we get acc_onset and acc_wake regarding to the same day of measurement in the same row. 
                      # Also, it allows for the analysis of the first day for those studies in which the accelerometer is 
                      # started during the morning and the first day is of interest.
                      if (timewindowi == "MM") {
                        if (wi==1) {
                          qqq[1] = 1
                          qqq[2] = nightsi[wi]
                        } else if (wi<=nrow(summarysleep_tmp2)) {
                          qqq[1] = nightsi[wi-1] + 1
                          qqq[2] = nightsi[wi]
                        } else if (wi>nrow(summarysleep_tmp2)) {
                          qqq[1] = qqq[2] + 1
                          #qqq[2] = length(diur)
                          qqq[2] = nightsi[wi]
                          if(is.na(qqq[2])==TRUE | length(diur) < qqq[2]) qqq[2] = length(diur)
                        }
                      } else if(timewindowi == "WW") {
                        if (wi==1){
                          if (waketi > 0) {
                            qqq[1] = waketi +1
                          } else {
                            qqq[1] = 1
                          }
                          qqq[2]=which(diff(diur) == -1)[wi] + 1
                        } else if (wi<=nrow(summarysleep_tmp2)) {
                          qqq[1] = which(diff(diur) == -1)[wi-1] + 1
                          qqq[2] = which(diff(diur) == -1)[wi]
                        } else if (wi>nrow(summarysleep_tmp2)) {
                          qqq[1] = qqq[2] + 1
                          qqq[2] = length(diur)
                        }
                      }
                      if (length(which(is.na(qqq)==TRUE)) == 0) { #if it is a meaningful day then none of the values in qqq should be NA
                        fi = 1
                        # START STORING BASIC INFORMATION
                        dsummary[di,fi] = id #summarysleep_tmp2$id[1]
                        ds_names[fi] = "id";      fi = fi + 1
                        dsummary[di,fi] = fnames.ms3[i]
                        ds_names[fi] = "filename";      fi = fi + 1
                        if (timewindowi == "WW") {
                          plusb = 0
                        } else {
                          plusb = 1
                        }
                        if(wi>nrow(summarysleep_tmp2)+plusb) {
                          dsummary[di,fi:(fi+1)] = c(weekdays(as.Date(summarysleep_tmp2$calendardate[wi-1], format="%e/%m/%Y")+1),
                                                     as.character(as.Date(summarysleep_tmp2$calendardate[wi-1], format="%e/%m/%Y")+1))
                          ds_names[fi:(fi+1)] = c("weekday","calendardate");  fi = fi + 2
                          dsummary[di,fi] = j
                          ds_names[fi] = "acc_def";      fi = fi + 1
                          dsummary[di,fi] = summarysleep_tmp2$night[wi-1]+1
                          ds_names[fi] = "night number";      fi = fi + 1
                          dsummary[di,fi:(fi+11)] = NA  #Since this is data afte last night, we don't have sleep information here
                          ds_names[fi:(fi+11)] = c("acc_onset","acc_wake","sleeplog_onset","sleeplog_wake",
                                                   "acc_onset_ts","acc_wake_ts","sleeplog_onset_ts","sleeplog_wake_ts",
                                                   "daysleeper","cleaningcode","sleeplog_used","acc_available"); fi = fi + 12
                          
                        } else {
                          if(timewindowi == "MM" & wi == nrow(summarysleep_tmp2)+plusb) { # for the last day a ot of information is missing, so fill in defaults 
                            dsummary[di,fi:(fi+1)] = c(weekdays(as.Date(summarysleep_tmp2$calendardate[wi-1], format="%e/%m/%Y")+1),
                                                       as.character(as.Date(summarysleep_tmp2$calendardate[wi-1], format="%e/%m/%Y")+1))
                            ds_names[fi:(fi+1)] = c("weekday","calendardate");  fi = fi + 2
                            dsummary[di,fi] = j
                            ds_names[fi] = "acc_def";      fi = fi + 1
                            dsummary[di,fi] = summarysleep_tmp2$night[wi-1]+1
                            ds_names[fi] = "night number";      fi = fi + 1
                          } else {
                            
                            dsummary[di,fi:(fi+1)] = c(as.character(summarysleep_tmp2$weekday[wi]),
                                                       as.character(as.Date(summarysleep_tmp2$calendardate[wi], format="%e/%m/%Y")))
                            ds_names[fi:(fi+1)] = c("weekday","calendardate");  fi = fi + 2
                            dsummary[di,fi] = j
                            ds_names[fi] = "acc_def";      fi = fi + 1
                            dsummary[di,fi] = summarysleep_tmp2$night[wi]
                            ds_names[fi] = "night number";      fi = fi + 1
                          }
                          dsummary[di,fi] = summarysleep_tmp2$acc_onset[wi]
                          ds_names[fi] = "acc_onset";      fi = fi + 1
                          if (timewindowi == "WW") {
                            dsummary[di,fi] = summarysleep_tmp2$acc_wake[wi]
                          } else {
                            if (wi > 1) dsummary[di,fi] = summarysleep_tmp2$acc_wake[wi-1]
                          }
                          ds_names[fi] = "acc_wake";      fi = fi + 1
                          
                          dsummary[di,fi] = summarysleep_tmp2$sleeplog_onset[wi]
                          
                          ds_names[fi] = "sleeplog_onset";      fi = fi + 1
                          if (timewindowi == "WW") {
                            dsummary[di,fi] = summarysleep_tmp2$sleeplog_wake[wi]
                          } else {
                            if (wi > 1) dsummary[di,fi] = summarysleep_tmp2$sleeplog_wake[wi-1]
                          }
                          ds_names[fi] = "sleeplog_wake";      fi = fi + 1
                          
                          dsummary[di,fi] = summarysleep_tmp2$acc_onset_ts[wi]     
                          
                          ds_names[fi] = "acc_onset_ts";      fi = fi + 1
                          if (timewindowi == "WW") {
                            dsummary[di,fi] = summarysleep_tmp2$acc_wake_ts[wi]
                          } else {
                            if (wi >1) dsummary[di,fi] = summarysleep_tmp2$acc_wake_ts[wi-1]
                          }
                          ds_names[fi] = "acc_wake_ts";      fi = fi + 1
                          
                          dsummary[di,fi] = summarysleep_tmp2$sleeplog_onset_ts[wi]
                          
                          ds_names[fi] = "sleeplog_onset_ts";      fi = fi + 1
                          if (timewindowi == "WW") {
                            dsummary[di,fi] = summarysleep_tmp2$sleeplog_wake_ts[wi]
                          } else {
                            if (wi > 1) dsummary[di,fi] = summarysleep_tmp2$sleeplog_wake_ts[wi-1]
                          }
                          ds_names[fi] = "sleeplog_wake_ts";      fi = fi + 1
                          dsummary[di,fi] = summarysleep_tmp2$daysleeper[wi]
                          # if timewindowi is MM then the following variables are less informative,
                          # because they only refer to the evening and not the morning.
                          ds_names[fi] = "daysleeper";      fi = fi + 1
                          dsummary[di,fi] = summarysleep_tmp2$cleaningcode[wi]
                          ds_names[fi] = "cleaningcode";      fi = fi + 1
                          dsummary[di,fi] = summarysleep_tmp2$sleeplog_used[wi]
                          ds_names[fi] = "sleeplog_used";      fi = fi + 1
                          dsummary[di,fi] = summarysleep_tmp2$acc_available[wi]
                          ds_names[fi] = "acc_available";      fi = fi + 1
                        }
                        # 
                        # define time windows
                        # qqq definitions changed so we get acc_onset and acc_wake regarding to the same day of measurement in the same row.
                        # Also, it allows for the analysis of the first day for those studies in which the accelerometer is started during the morning and the first day is of interest.
                        if (timewindowi == "MM") {
                          if (wi==1) {
                            # if (waketi > 0) {
                            #   qqq1 = waketi + 1
                            # } else {
                            qqq1 = 1
                            # }
                            qqq2 = nightsi[wi]
                          } else if (wi<=nrow(summarysleep_tmp2)) {
                            qqq1 = nightsi[wi-1] + 1
                            qqq2 = nightsi[wi]
                          } else if (wi>nrow(summarysleep_tmp2)) {
                            qqq1 = qqq2 + 1
                            #qqq2 = length(diur)
                            qqq2 = nightsi[wi]
                            if(is.na(qqq2)==TRUE | length(diur) < qqq2) qqq2 = length(diur)
                          }
                          dsummary[di, fi] = "MM"
                        } else if (timewindowi == "WW") {
                          if (wi==1){
                            if (waketi > 0) qqq1 = waketi + 1
                            else {qqq1 = 1}
                            qqq2 = which(diff(diur) == -1)[wi] + 1
                          } else if (wi<=nrow(summarysleep_tmp2)) {
                            qqq1 = which(diff(diur) == -1)[wi-1] + 1
                            qqq2 = which(diff(diur) == -1)[wi]
                          } else if (wi>nrow(summarysleep_tmp2)) {
                            qqq1 = qqq2 + 1
                            # qqq2 = length(diur)
                            qqq2 = nightsi[wi]
                            if(is.na(qqq2)==TRUE | length(diur) < qqq2) qqq2 = length(diur)
                          }
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
                        if (wlih > 30 & length(summarysleep_tmp2$night) > 1) { # scenario when day is missing and code reaches out to two days before this day
                          # if (summarysleep_tmp2$night[wi] - summarysleep_tmp2$night[wi-1] != 1) {
                          qqq1 = (qqq2 - (24* ((60/ws3)*60))) + 1 # code now uses only 24hours before waking up
                          if (qqq1 < 1) qqq1 = 1
                          wlih = ((qqq2-qqq1)+1)/((60/ws3)*60)
                          # }
                        }
                        dsummary[di,fi] = wlih
                        ds_names[fi] = "window_length_in_hours";      fi = fi + 1
                        dsummary[di,fi] = (length(which(nonwear[qqq1:qqq2] == 1)) * ws3) / 3600
                        ds_names[fi] = "nonwear_hours";      fi = fi + 1
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
                        ds_names[fi] = "dur_day_min";      fi = fi + 1
                        dsummary[di,fi] = (length(which(diur[qqq1:qqq2] == 1)) * ws3) / 60
                        ds_names[fi] = "dur_night_min";      fi = fi + 1
                        dsummary[di,fi] = (length(c(qqq1:qqq2)) * ws3) / 60
                        ds_names[fi] = "dur_nightandday_min";      fi = fi + 1
                        
                        #============================================
                        # Number of long wake periods (defined as > 5 minutes) during the night
                        Nawake = length(which(abs(diff(which(LEVELS[qqq1:qqq2] == 0))) > (300 / ws3))) - 2
                        if (Nawake < 0) Nawake = 0
                        dsummary[di,fi] = Nawake
                        ds_names[fi] = "N_atleast5minwakenight";      fi = fi + 1
                        #============================================================
                        # percentage of available data
                        zt_hrs_nonwear = (length(which(diur[qqq1:qqq2] == 0 & nonwear[qqq1:qqq2] == 1)) * ws3) / 3600 #day
                        zt_hrs_total = (length(which(diur[qqq1:qqq2] == 0)) * ws3) / 3600 #day
                        dsummary[di,fi] = (zt_hrs_nonwear/zt_hrs_total)  * 10000 / 100
                        ds_names[fi] = "nonwear_perc_day";      fi = fi + 1
                        zt_hrs_nonwear = (length(which(diur[qqq1:qqq2] == 1 & nonwear[qqq1:qqq2] == 1)) * ws3) / 3600 #night
                        zt_hrs_total = (length(which(diur[qqq1:qqq2] == 1)) * ws3) / 3600 #night
                        dsummary[di,fi] =  (zt_hrs_nonwear/zt_hrs_total)  * 10000 / 100
                        ds_names[fi] = "nonwear_perc_night";      fi = fi + 1
                        zt_hrs_nonwear = (length(which(nonwear[qqq1:qqq2] == 1)) * ws3) / 3600
                        zt_hrs_total = (length(diur[qqq1:qqq2]) * ws3) / 3600 #night and day
                        dsummary[di,fi] =  (zt_hrs_nonwear/zt_hrs_total)  * 10000 / 100
                        ds_names[fi] = "nonwear_perc_nightandday";      fi = fi + 1
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
                        ds_names[fi] = "ACC_day_mg";      fi = fi + 1
                        dsummary[di,fi] = mean(ACC[sse[which(diur[sse] == 1)]])
                        ds_names[fi] = "ACC_night_mg";      fi = fi + 1
                        dsummary[di,fi] = mean(ACC[sse])
                        ds_names[fi] = "ACC_nightandday_mg";      fi = fi + 1             
                        #===============================================
                        # QUANTILES...
                        # sse = qqq1:qqq2
                        WLH = ((qqq2-qqq1)+1)/((60/ws3)*60) #windowlength_hours = 
                        if (WLH <= 1) WLH = 1.001
                        dsummary[di,fi] = quantile(ACC[sse],probs=((WLH-1)/WLH))
                        ds_names[fi] = paste("quantile_mostactive60min_mg",sep="");      fi = fi + 1
                        dsummary[di,fi] = quantile(ACC[sse],probs=((WLH-0.5)/WLH))
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
                            if(length(unlist(strsplit(L5HOUR,"[+]"))) > 1) { # only do this for ISO8601 format
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
                          # dsummary[di,fi] = length(which(diff(which(LEVELS[sse] != levelsc)) > 1)) #qqq1:qqq2 #old code
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
        
        # This is not a good solution anymore: If excludefirstlast == TRUE then part4 does not generate sleep estimates for the first and last night,
        # therefore, part5 will also mis waking up time for the second and the beforelast day.
        # So, I think if we want to facilitate that the first and last day are excluded in part5 then this will have to be handled
        # with a different input argument
        if (excludefirstlast.part5 == TRUE) { #undesirable because it will slowly remove alchanged to TRUE on 20 May 2015
          output = output[-c(which(output$night_number == min(output$night_number)),
                             which(output$night_number == max(output$night_number))),] #Moved here, first, it analyzes the whole measurement, then it selects the days to show
        }
        
        # correct definition of sleep log availability for window = WW, because now it
        # also relies on sleep log from previous night
        whoareWW = which(output$window == "WW") # look up WW
        if (length(loglocation) > 0) { #only do this if there is a sleep log
          if (length(whoareWW) > 0) {
            whoareNOSL =which(output$sleeplog_used[whoareWW] == FALSE) #look up nights with no Sleeplog
            if (length(whoareNOSL) > 0) {
              for (k23 in 1:length(whoareNOSL)) {
                k24 = whoareWW[(whoareNOSL[k23]-1)]
                if (length(k24) > 0) {
                  if (k24 > 0) {
                    output$sleeplog_used[k24] = FALSE
                  }
                }
              }
            }
          }
        }
        # tidy up output data frame, because it may have a lot of empty rows and columns
        emptyrows = which(output[,1] == "" & output[,2] == "")
        if (length(emptyrows) > 0) output = output[-emptyrows,]
        lastcolumn = which(colnames(output) == "bout.metric")
        if (ncol(output) > lastcolumn) {
          emptycols = which(output[1,] == "" & output[2,] == "")
          if (length(emptycols) > 0) emptycols = emptycols[which(emptycols > lastcolumn)]
          if (length(emptycols) > 0) output = output[-emptycols]
        }
        save(output,file=paste(metadatadir,ms5.out,"/",fnames.ms3[i],sep=""))
        rm(output,dsummary)
      }
    }
  }
}

