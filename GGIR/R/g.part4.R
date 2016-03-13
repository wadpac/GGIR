g.part4 = function(datadir=c(),metadatadir=c(),f0=f0,f1=f1,idloc=1,loglocation = c(),
                   colid = 1,coln1 = 9,nnights = 7,sleeplogidnum=FALSE,do.visual=FALSE,outliers.only = FALSE,
                   excludefirstlast=FALSE,criterror = 1,includenightcrit=4,
                   relyonsleeplog=FALSE,def.noc.sleep=c(),
                   storefolderstructure=FALSE,
                   overwrite=FALSE) {
  # description: function to load sleep detection from g.part3 and to convert it into night-specific summary measures of sleep,
  # possibly aided by sleep log/diary information (if available and provided by end-user)
  nnpp = 40
  #-------------------------------------------------
  ms3.out = "/meta/ms3.out"
  if (file.exists(paste(metadatadir,ms3.out,sep=""))) {
  } else {
    cat("Error: First run g.part3 (mode = 3) before running g.part4 (mode = 4)")
  }
  ms4.out = "/meta/ms4.out"
  if (file.exists(paste(metadatadir,ms4.out,sep=""))) {
  } else {
    dir.create(file.path(metadatadir,ms4.out))
  }
  meta.sleep.folder = paste(metadatadir,"/meta/ms3.out",sep="")
  #------------------------------------------------
  # Get sleeplog data
  reloadlog = TRUE
  if (length(loglocation) > 0) {
    dolog = TRUE
  } else {
    dolog = FALSE
  }
  if (dolog == TRUE) {         
    if (reloadlog == TRUE) {
      LL = g.loadlog(loglocation,coln1,colid,nnights,sleeplogidnum)
      sleeplog = LL$sleeplog
      save(sleeplog,file=paste(metadatadir,"/meta/sleeplog.RData",sep=""))
    } else {
      load(file=paste(metadatadir,"/meta/sleeplog.RData",sep=""))
    }
  }

  #------------------------------------------------
  # get list of accelerometer milestone data files from sleep (produced by g.part3)
  fnames = dir(meta.sleep.folder)

  if (f1 > length(fnames)) {
    # cat(paste("File index f1 automatically changed from, ",f1,"to",length(fnames)," ",sep=""))
    f1 = length(fnames)
  }
    if (f0 > length(fnames)) {
    # cat(paste("File index f1 automatically changed from, ",f1,"to",length(fnames)," ",sep=""))
    f0 = 1
  }

  logdur = rep(0,length(fnames))
  cnt = 1
  idlabels = rep(0,nnpp)
  if (f1 == 0 | length(f1) == 0 | f1 > length(fnames))  f1 = length(fnames)
  pagei = 1
  #-----------------------------------------------------
  # specify output variable names
  nightsummary = as.data.frame(matrix(0,0,29)) 
  colnames(nightsummary) = c("id", "night","acc_onset", "acc_wake", "acc_timeinbed", "acc_def", 
                             "sleeplog_onset", "sleeplog_wake", "sleeplog_timeinbed",
                             "error_onset", "error_wake", "error_dur",
                             "fraction_night_invalid",
                             "acc_dur_noc","acc_dur_sibd","acc_n_noc","acc_n_sibd",
                             "acc_onset_ts","acc_wake_ts","sleeplog_onset_ts", "sleeplog_wake_ts",
                             "page","daysleeper","weekday","calendardate","filename",
                             "cleaningcode","sleeplog_used","acc_available")
  colnamesnightsummary = colnames(nightsummary)
  sumi = 1
  if ((f1-f0) > 0) {
    sleeplog_used = rep(" ",((f1-f0)+1))
  } else {
    sleeplog_used = " "
  }

  ffdone = c()
  ms4.out = "/meta/ms4.out"
  fnames.ms4 = dir(paste(metadatadir,ms4.out,sep=""))
  fnames.ms4 = sort(fnames.ms4)
  ffdone = fnames.ms4
  #========================================================================
  # check which files have already been processed, such that no double work is done
  # ffdone a matrix with all the binary filenames that have been processed
  fnames = sort(fnames)
  #--------------------------------
  # get original file path of the accelerometer (some studies may like to keep track of original folder structure if their study structure is embodied in folder structure)
  if (storefolderstructure == TRUE) {
    filelist = FALSE
    if (length(datadir) == 1) { #could be a directory or one file
      if (length(unlist(strsplit(datadir,"[.]bi")))>1) filelist = TRUE
      if (length(unlist(strsplit(datadir,"[.]cs")))>1) filelist = TRUE
    } else { #multiple files
      filelist = TRUE    
    }
    if (filelist == FALSE) {
      fnamesfull = c(dir(datadir,recursive=TRUE,pattern="[.]csv"),dir(datadir,recursive=TRUE,pattern="[.]bin"))
    } else {
      fnamesfull = datadir
    }
    f16 = function(X) {
      out = unlist(strsplit(X,"/"))
      f16 = out[length(out)]
    }
    f17 = function(X) {
      out = unlist(strsplit(X,"/"))
      f17 = out[(length(out)-1)]
    }
    ffd = ffp = rep("",length(fnamesfull))
    if (length(fnamesfull) > 0) {
      fnamesshort = apply(X=as.matrix(fnamesfull),MARGIN=1,FUN=f16)
      foldername = apply(X=as.matrix(fnamesfull),MARGIN=1,FUN=f17)
      for (i in 1:length(fnames)) { #
        ff = as.character(unlist(strsplit(fnames[i],".RDa"))[1])
        if (length(which(fnamesshort == ff)) > 0) {
          ffd[i] = fnamesfull[which(fnamesshort == ff)]
          ffp[i] = foldername[which(fnamesshort == ff)]
        }
      }
    }
  }
  cnt67 = 1
  #=================================================================
  #=================================================================
  # start of loop through the participants
  for (i in f0:f1) {
    # decide whether file was processed before
    if (length(ffdone) > 0) {
      if (length(which(ffdone == fnames[i])) > 0) { 
        skip = 1 #skip this file because it was analysed before")
      } else {
        skip = 0 #do not skip this file
      }
    } else {
      skip = 0
    }
    if (overwrite == TRUE) skip = 0 # this will make that analyses is done regardless of whether it was done before
    if (skip ==0) {
      cat(paste(" ",i,sep=""))
      if (cnt67 == 1) { #only create new pdf if there is actually new plots to be generated
        if (do.visual == TRUE) { # keep pdf for QC purposes
          pdf(file=paste(metadatadir,"/results/visualisation_sleep.pdf",sep=""),width=8.27,height=11.69)
          par(mar=c(4,5,1,2)+0.1)
          plot(c(0,0),c(1,1),xlim=c(12,36),ylim=c(0,nnpp),col="white",axes=FALSE,xlab="time",ylab="",
               main=paste("Page ",pagei,sep=""))
          axis(side=1, at = 12:36, labels = c(12:24,1:12),cex.axis=0.7)
          abline(v=c(18,24,30),lwd=0.2,lty=2)
          abline(v=c(15,21,27,33),lwd=0.2,lty=3,col="grey")
        }
        cnt67 = 2
      }
      if (storefolderstructure == FALSE) {
        nightsummary = as.data.frame(matrix(0,0,29)) 
      } else {
        nightsummary = as.data.frame(matrix(0,0,31)) 
      }
      colnames(nightsummary) = colnamesnightsummary
      sumi = 1
      L5list = sib.cla.sum = c()
      # load data, check whether there is data, identify id numbers...
      load(paste(meta.sleep.folder,"/",fnames[i],sep=""))
      if (nrow(sib.cla.sum) != 0) { #there needs to be some information
        if (idloc == 2) {
          #get id
          cave = function(x) as.character(unlist(strsplit(x,"_")))[1]
          x = as.matrix(as.character(fnames[i]))
          accid = apply(x,MARGIN=c(1),FUN=cave)
          accid_bu = accid
          cave2 = function(x) {
            tmp = as.character(unlist(strsplit(x,"")))
            cave2 = tmp[length(tmp)]
          }
          letter = apply(as.matrix(accid),MARGIN=c(1),FUN=cave2)
          for (h in 1:length(accid)) {
            accid[h] = as.character(unlist(strsplit(accid[h],letter[h]))[1])
          } 
          accid = as.numeric(accid)
          if (is.na(accid) == TRUE) accid = accid_bu #catch for files with only id in filename and
        } else { # get id from filename
          tmp = fnames[i]
          if (length(unlist(strsplit(tmp,"_"))) > 1) tmp = unlist(strsplit(tmp,"_"))[1]
          if (length(unlist(strsplit(tmp,"[.]RDa"))) > 1) tmp = unlist(strsplit(tmp,"[.]RDa"))[1]
          if (length(unlist(strsplit(tmp,"[.]cs"))) > 1) tmp = unlist(strsplit(tmp,"[.]cs"))[1]
          accid = tmp[1]
        }
        if (dolog == TRUE) {
          if (sleeplogidnum == FALSE) {
            wi = which(as.character(sleeplog$id) == as.character(accid))
          } else {
            wi = which(sleeplog$id == as.numeric(accid))
          }
        } else {
          wi = 1
        } 
        qq_temp = sib.cla.sum #sib.cla.sum is the output from g.part3
        # create clear overview of which nights need to be procecess
        if (max(qq_temp$night) < nnights) {
          nnightlist = 1:max(qq_temp$night)
        } else {
          nnightlist = 1:nnights
        }
        nnights.list = nnightlist # unique(sleeplog.t$night)
        nnights.list = nnights.list[which(is.na(nnights.list) == FALSE & nnights.list != 0)]
        if (excludefirstlast==TRUE) {#exclude first and last night
          if (length(nnights.list) >= 3) {
            nnights.list = nnights.list[2:(length(nnights.list)-1)]
          } else {
            nnights.list = c()
          }
        }
        calendardate = wdayname = rep("",length(nnights.list))
        daysleeper = rep(FALSE,length(nnights.list))
        ###########################################################
        nightj = 1
        for (j in nnights.list) { #go through the nights
          ######################################
          # get default onset and wake
          # use default assumption if no other info is available (def.noc.sleep is an input argument people can use
          # to specify assumed time in bed in the absense of sleep diary in a study)
          if (length(def.noc.sleep) != 2) { #has a default nocturnal sleep window defined? if not, do this part:
            # NOW USE L5 to replace defaulttmp3 and tmp6 if L5 is a number
            if (length(L5list) > 0) {
              defaulttmp3 = L5list[j] - 6
              defaulttmp6 = L5list[j] + 6
            }
            if (defaulttmp3 >= 24) defaulttmp3 = defaulttmp3 - 24
            if (defaulttmp6 >= 24) defaulttmp6 = defaulttmp6 - 24
          } else {
            defaulttmp3 = def.noc.sleep[1] #onset
            defaulttmp6 = def.noc.sleep[2] #wake
          }
          defaultdur = defaulttmp6 - defaulttmp3
          # for this night OR if dolog == FALSE
          if (dolog == TRUE & length(wi)  > 0) { #if sleep log is available, use it
            sleeplog.t = sleeplog[wi,]
            sleeplog_used[i] =  "TRUE"
            cleaningcode = 0
          } else { #if sleep log is not available available, use dummy values
            if (j == nnights.list[1]) sleeplog.t = data.frame(matrix(0,length(nnightlist),5))
            sleeplog.t[nightj,1] = accid
            sleeplog.t[nightj,2] = j
            sleeplog.t[nightj,3] = defaultdur
            mintmp3 = round((defaulttmp3 - floor(defaulttmp3)) * 60)
            mintmp6 = round((defaulttmp6 - floor(defaulttmp6)) * 60)
            sleeplog.t[nightj,4] = paste(floor(defaulttmp3),":",mintmp3,":00",sep="")
            sleeplog.t[nightj,5] = paste(floor(defaulttmp6),":",mintmp6,":00",sep="") #"08:00:00"
            names(sleeplog.t) = c("id","night","duration","sleeponset","sleepwake")
            sleeplog_used[i] = "FALSE"
            cleaningcode = 1
          }
          nightj = nightj + 1
          #############################################
          acc_available = "TRUE" #default assumption
          spocum = as.data.frame(matrix(0,0,5))
          spocumi = 1
          wi2 = which(sleeplog.t$night == j)
          sleeplog.t2 = sleeplog.t[wi2,]
          #-----------------------
          # get sleep log timestamps and assess whether it is a nightworker
          ###################################
          # SLEEP DIARY BLOCK
          # onset
          tmp1 = as.character(sleeplog.t2[,which(names(sleeplog.t2) == "sleeponset")])
          tmp2 = unlist(strsplit(tmp1,":"))
          tmp3 = as.numeric(tmp2[1]) + (as.numeric(tmp2[2])/60) + (as.numeric(tmp2[3])/3600)
          # wake
          tmp4 = as.character(sleeplog.t2[,which(names(sleeplog.t2) == "sleepwake")])
          tmp5 = unlist(strsplit(tmp4,":"))
          tmp6 = as.numeric(tmp5[1]) + (as.numeric(tmp5[2])/60) + (as.numeric(tmp5[3])/3600)
          ###################################
          # DAYSLEEPER OR NIGHTSLEEPER?
          daysleeper[j] = FALSE
          if (is.na(tmp3) == FALSE & is.na(tmp6) == FALSE & tmp1 != "" & tmp4 != "") { # is the sleep log valid?
            # tidy up timestamps
            tmp1 = unlist(strsplit(tmp1,":"))
            tmp1HR = as.numeric(tmp1[1])
            tmp1MI = as.numeric(tmp1[2])
            tmp1SE = as.numeric(tmp1[3])
            if (tmp1HR < 9) tmp1HR = paste("0",tmp1HR,sep="")
            if (tmp1MI < 9) tmp1MI = paste("0",tmp1MI,sep="")
            if (tmp1SE < 9) tmp1SE = paste("0",tmp1SE,sep="")
            tmp1 = paste(tmp1HR,":",tmp1MI,":",tmp1SE,sep="")
            #----------------------------------------------
            tmp4 = unlist(strsplit(tmp4,":"))
            tmp4HR = as.numeric(tmp4[1])
            tmp4MI = as.numeric(tmp4[2])
            tmp4SE = as.numeric(tmp4[3])
            if (tmp4HR < 9) tmp4HR = paste("0",tmp4HR,sep="")
            if (tmp4MI < 9) tmp4MI = paste("0",tmp4MI,sep="")
            if (tmp4SE < 9) tmp4SE = paste("0",tmp4SE,sep="")
            tmp4 = paste(tmp4HR,":",tmp4MI,":",tmp4SE,sep="")
            #------------------------------------------------------------------
            # does sleep period overlap with noon? If yes, then classify as daysleeper
            if (tmp6 > 12 & tmp3 < 12) daysleeper[j] = TRUE
            if (tmp6 > 12 & tmp3>tmp6) daysleeper[j] = TRUE
            # change time stamps to be a continues time
            if (tmp3 < 12) tmp3 = tmp3 + 24 #shift 24 hours to create continues time
            if (tmp6 <= 12) tmp6 = tmp6 + 24 #shift 24 hours to create continues time
            if (daysleeper[j] == TRUE) {
              logdur[i] = tmp3 - tmp6
            } else {
              logdur[i] = tmp6 - tmp3
            }
          } else {
            tmp3 = defaulttmp3 #use default assumption about onset
            tmp6 = defaulttmp6 + 24 #use default assumption about wake
            logdur[i] = tmp6 - tmp3
            cleaningcode = 1 # no diary available for this night
          }
          #-----------------------------------------
          #plan analysis according to knowledge about whether it is a daysleeper or not
          if (excludefirstlast==FALSE) { #if you are not excluding the last day
            if (daysleeper[j] == TRUE & j != max(nnights.list)) { #and is a daysleeper and not the last night
              loaddays = 2
            } else {
              loaddays = 1
            }
            if (daysleeper[j] == TRUE & j == max(nnights.list)) { #and is a daysleeper and the last night
              daysleeper[j] = FALSE #treat it like a normal day
              loaddays = 1
              if (tmp6 > 36) tmp6 = 36 #round diary to noon
              logdur[i] = tmp6 - tmp3
            }
          } else { #if you are excluding the last day
            if (daysleeper[j] == TRUE) { #dont worrry about whether it is the last day, because it is not included
              loaddays = 2
            } else {
              loaddays = 1
            }
          }
          #################################################################################
          # START ANALYSIS OF NIGHT SLEEPERS (defined as people for whom sleep window does not overlap with noon)
          if (daysleeper[j] == FALSE & loaddays == 1) {
            # now get accelerometer sleep detection
            qq = sib.cla.sum #$output
            sleepdet = qq[which(qq$night == j),]
            if (nrow(sleepdet) == 0) {
              acc_available = "FALSE"
              #now generate empty overview for this night / person
              if (spocumi == 1) {
                dummyspo = matrix(0,1,5); dummyspo[1,1] = 1
                spocum = dummyspo
                spocumi = spocumi + 1
              } else {
                dummyspo = matrix(0,1,5); dummyspo[1,1] = 1
                spocum = rbind(spocum,dummyspo)
                spocumi = spocumi + 1
              }
              cleaningcode = 3
            } else {
              acc_available = "TRUE"
            }
            # we now have log data for one night (sleeplog.t2) and we have acc data for one night (sleepdet)
            defs = unique(sleepdet$definition)
            for (k in defs) {
              ki = which(sleepdet$definition == k)
              sleepdet.t = sleepdet[ki,]
              # now get sleep periods
              nsp = length(unique(sleepdet.t$sib.period)) #number of sleep periods
              spo = matrix(0,nsp,5) # overview of sleep periods
              if (nsp == 1 & unique(sleepdet.t$sib.period)[1] == 0) { # no sleep periods
                spo[1,1] = 1
                spo[1,2] = 0
                spo[1,3] = 0
                spo[1,4] = 0
                spo[1,5] = k
              } else {
                DD = g.create.sp.mat(nsp,spo,sleepdet.t,daysleep=daysleeper[j])
                wdayname[j] = DD$wdayname
                calendardate[j] = DD$calendardate
                spo = DD$spo
                # spo is now a list of onset and wake for each sleep period
                for (evi in 1:nrow(spo)) {
                  # BASIC CLASSIFICATION
                  #nocturnal = all acc periods that end after diary onset and start before diary wake
                  if (spo[evi,2] < tmp6 & spo[evi,3] > tmp3) { # = acconset < logwake  & accwake > logonset
                    spo[evi,4] = 1
                    ######
                    # REDEFINITION OF ONSET/WAKE OF THIS PERIOD OVERLAPS
                    if (relyonsleeplog == TRUE) {
                      if ((spo[evi,2] < tmp6 & spo[evi,3] > tmp6) | (spo[evi,2] < tmp6 & spo[evi,3] < spo[evi,2])) {
                        spo[evi,3] = tmp6   # accwake => logwake
                      }
                      if ((spo[evi,2] < tmp3 & spo[evi,3] > tmp3) | (spo[evi,3] > tmp3 & spo[evi,3] < spo[evi,2])) {
                        spo[evi,2] = tmp3 # acconset => logonset
                      }
                    }
                  }
                }
              }
              ###########################################################################################
              # HERE THERE SHOULD BE A VARIABLE 'spo' with all the sleep periods FOR ONE SLEEP DEFINITION
              spo[,5] = k
              if (spocumi == 1) {
                spocum = spo
                spocumi = spocumi + 1
              } else {
                spocum = rbind(spocum,spo)
                spocumi = spocumi + 1
              }
            }      
            #################################################################################
            # NIGHT WORKS / DAY SLEEPERS
          } else if (daysleeper[j] == TRUE) {
            for (loaddaysi in 1:loaddays) { #load twice if daysleeper
              # now get accelerometer sleep detection
              qq = sib.cla.sum #$output
              sleepdet = qq[which(qq$night == (j+(loaddaysi-1))),] #changed 1 October 2014
              if (nrow(sleepdet) == 0) {
                acc_available = "FALSE"
                #now generate empty overview for this night / person
                if (spocumi == 1) {
                  dummyspo = matrix(0,1,5); dummyspo[1,1] = 1
                  spocum = dummyspo
                  spocumi = spocumi + 1
                } else {
                  dummyspo = matrix(0,1,5); dummyspo[1,1] = 1
                  spocum = rbind(spocum,dummyspo)
                  spocumi = spocumi + 1
                }
                cleaningcode = 3
              } else {
                acc_available = "TRUE"
              }
              # we now have log data for one night (sleeplog.t2) and we have acc data for one night (sleepdet)
              defs = unique(sleepdet$definition)
              for (k in defs) {
                ki = which(sleepdet$definition == k)
                sleepdet.t = sleepdet[ki,]
              
                ####
                # now get sleep periods
                nsp = length(unique(sleepdet.t$sib.period)) #number of sleep periods
                spo = matrix(0,nsp,5) # overview of sleep periods
                if (nsp == 1 & unique(sleepdet.t$sib.period)[1] == 0) {
                  spo[1,1] = 1
                  spo[1,2] = 0
                  spo[1,3] = 0
                  spo[1,4] = 0
                  spo[1,5] = k
                  tmpCmd = paste("spo_day",k,"= c()",sep="")
                  eval(parse(text = tmpCmd))
                } else {
                  DD = g.create.sp.mat(nsp,spo,sleepdet.t,daysleep=daysleeper[j])
                  if (loaddaysi == 1) { # newly added 25/11/2015
                    wdayname[j] = DD$wdayname
                    calendardate[j] = DD$calendardate
                  }
                  spo = DD$spo
                  #================================================
                  if (daysleeper[j] == TRUE & loaddaysi == 1) {
                    w1 = which(spo[,3] >= 18) #only use periods ending after 6pm
                    if (length(w1) > 0) {
                      spo = as.matrix(spo[w1,])
                      if (ncol(spo) == 1) spo = t(spo)
                      if (nrow(spo) == 1) {
                        if (spo[1,2] <= 18) spo[1,2] = 18 #turn start time on 1st day before 6pm to 6pm
                      } else {
                        spo[which(spo[,2] <= 18),2] = 18 #turn start times on 1st day before 6pm to 6pm
                      }
                      tmpCmd = paste("spo_day",k,"= spo",sep="") #spo needs to be rememered specific to definition
                      eval(parse(text = tmpCmd))
                      
                    } else {
                      tmpCmd = paste("spo_day",k,"= c()",sep="")
                      eval(parse(text = tmpCmd))
                    }
                  } else if (daysleeper[j] == TRUE & loaddaysi == 2) {
                    w2 = which(spo[,2] < 18) #only use periods starting before 6pm
                    if (length(w2) > 0) {
                      spo = as.matrix(spo[w2,])
                      if (ncol(spo) == 1) spo = t(spo)
                      if (nrow(spo) == 1) {
                        if (spo[1,3] > 18) spo[1,3] = 18 #turn end time on 2nd day after 6pm to 6pm
                      } else {
                        spo[which(spo[,3] > 18),3] = 18 #turn end times on 2nd day after 6pm to 6pm
                      }
                      spo[,2:3] = spo[,2:3]+ 24
                      tmpCmd = paste("spo_day2",k,"= spo",sep="") #spo needs to be rememered specific to definition
                      eval(parse(text = tmpCmd))
                    } else {
                      tmpCmd = paste("spo_day2",k,"= c()",sep="")
                      eval(parse(text = tmpCmd))
                    }
                    # + 24 to create continues timelines for day 2 relative to day 1
                    #attach days together as being one long day
                    name1 = paste("spo_day",k,sep="")
                    name2 = paste("spo_day2",k,sep="")
                    tmpCmd = paste("spo = rbind(",name1,",",name2,")",sep="")
                    eval(parse(text=tmpCmd))       
                    #reverse back the timestamps to remember that these timestamps were coming from different days
                    spo[which(spo[,3] >= 36),3] =  spo[which(spo[,3] >= 36),3] - 24
                    spo[which(spo[,2] >= 36),2] =  spo[which(spo[,2] >= 36),2] - 24
                    ## run same classification here:
                    # spo is now a list of onset and wake for each sleep period, with continuous time line
                    for (evi in 1:nrow(spo)) {
                      # BASIC CLASSIFICATION
                      #nocturnal = all acc periods that end after diary onset and start before diary wake
                      if (spo[evi,2] < tmp6 | spo[evi,3] > tmp3) {  # = acconset < logwake | accwake > logonset
                        spo[evi,4] = 1
                        if (relyonsleeplog == TRUE) {
                          if ((spo[evi,2] < tmp6 & spo[evi,3] > tmp6) | (spo[evi,3] > tmp6 & spo[evi,3] < spo[evi,2])) {
                            spo[evi,3] = tmp6  # accwake => logwake
                          }
                          if ((spo[evi,2] < tmp3 & spo[evi,3] > tmp3) | (spo[evi,2] < tmp3 & spo[evi,3] < spo[evi,2])) {
                            spo[evi,2] = tmp3 # acconset => logonset
                          }
                        }
                      }
                    }
                    ###########################################################################################
                    # HERE YOU SHOULD HAVE A VARIABLE 'spo' with all the sleep periods FOR ONE SLEEP DEFINITION
                    # EVEN IF NO SLEEP WAS DETECTED, there should be one line with zero sleep
                    spo[,5] = k
                    if (spocumi == 1) {
                      spocum = spo
                      spocumi = spocumi + 1
                    } else {
                      spocum = rbind(spocum,spo)
                      spocumi = spocumi + 1
                    }                
                  }
                }
              } #end for (k in defs) {
            } #loop through either 1 or 2 days (latter if it is a daysleeper)
          } # end of else if (daysleeper[j] == TRUE) {
          ##########################################################################
          # START OF PLOT AND nightsummary MEASURES
          #------------------------------------------------------------------------
          # Take variables 'spocum', tmp3 and tmp6 to derive nightsummary measures and to create a plot
          # for the current night in the current participant
          #------------------------------------------------------------------------
          if (do.visual ==  TRUE) {
            # PLOT
            if (cnt == (nnpp+1)) {
              cat(" NEW ")
              pagei = pagei + 1
              # add y-axis before starting new page
              axis(side=2, at = 1:nnpp,labels = idlabels,las=1,cex.axis=0.6)
              idlabels = rep(0,nnpp)
              plot(c(0,0),c(1,1),xlim=c(12,36),ylim=c(0,nnpp),col="white",axes=FALSE,xlab="time",ylab="",
                   main=paste("Page ",pagei,sep=""))
              axis(side=1, at = 12:36, labels = c(12:24,1:12),cex.axis=0.7)
              abline(v=c(18,24,30),lwd=0.2,lty=2)
              abline(v=c(15,21,27,33),lwd=0.2,lty=3,col="grey")
              cnt = 1
            }
          }
          # PLOT
          # ------------------------------------------------------------------------
          if (nrow(spocum) > 0) {
            undef = unique(spocum[,5])
            for (defi in undef) {
              #------------------------------------------------------------------------
              # nightsummary
              spocum.t = spocum[which(spocum[,5] == defi),]
              nightsummary[sumi,1] = accid
              nightsummary[sumi,2] = j #night
              if (is.matrix(spocum.t) == FALSE) {
                spocum.t = t(as.matrix(spocum.t))
              }
              #------------------------------------
              # ACCELEROMETER
              if (length(which(as.numeric(spocum.t[,4]) == 1)) > 0) {
                if (daysleeper[j] == FALSE) {
                  nightsummary[sumi,3] = min(spocum.t[which(spocum.t[,4] == 1),2]) #onset acc
                  nightsummary[sumi,4] = max(spocum.t[which(spocum.t[,4] == 1),3]) #wake acc
                } else {
                  rtl = which(spocum.t[,4] == 1)
                  nightsummary[sumi,3] =spocum.t[rtl[1],2]
                  nightsummary[sumi,4] =spocum.t[rtl[length(rtl)],3]
                }
              } else {
                nightsummary[sumi,3:4] = 0
              }
              nightsummary[,3] = as.numeric(nightsummary[,3])
              nightsummary[,4] = as.numeric(nightsummary[,4])
              # Calculate sleep duration
              if (nightsummary[sumi,3] > nightsummary[sumi,4]) { #daysleeper #
                nightsummary[sumi,5] = (36 - nightsummary[sumi,3]) + (nightsummary[sumi,4] - 12)
              } else {
                nightsummary[sumi,5] = nightsummary[sumi,4] - nightsummary[sumi,3]
              }
              #           }
              nightsummary[,5] = as.numeric(nightsummary[,5])
              nightsummary[sumi,6] = defi #sleep definition
              #------------------------------------
              # SLEEP LOG
              #correct tmp3 and tmp6 to fall within [12-36] window and store as sleeplog_onset and sleeplog_wake
              if (tmp3 > 36) {
                nightsummary[sumi,7] = tmp3-24 #onset
              } else {
                nightsummary[sumi,7] = tmp3
              }
              if (tmp6 > 36) {
                nightsummary[sumi,8] = tmp6-24 #wake
              } else {
                nightsummary[sumi,8] = tmp6
              }
              # Calculate sleep duration
              if (nightsummary[sumi,7] > nightsummary[sumi,8]) { #daysleeper #
                nightsummary[sumi,9] = abs((36 - nightsummary[sumi,7]) + (nightsummary[sumi,8] - 12))
              } else {
                nightsummary[sumi,9] = abs(nightsummary[sumi,8] - nightsummary[sumi,7])
              }
              #------------------------------------
              # Error
              nightsummary[sumi,10] = nightsummary[sumi,3] - nightsummary[sumi,7] #error onset
              nightsummary[sumi,11] = nightsummary[sumi,4] - nightsummary[sumi,8] #error wake
              #sometimes error can be in the wrong direction, e.g. log = 11, acc is 35
              if (nightsummary[sumi,10] > 12) nightsummary[sumi,10] = -(24 - nightsummary[sumi,10])
              if (nightsummary[sumi,10] < -12) nightsummary[sumi,10] = -(nightsummary[sumi,10] + 24)
              if (nightsummary[sumi,11] > 12) nightsummary[sumi,11] = -(24 - nightsummary[sumi,11])
              if (nightsummary[sumi,11] < -12) nightsummary[sumi,11] = -(nightsummary[sumi,11] + 24)
              nightsummary[sumi,12] = nightsummary[sumi,5] - nightsummary[sumi,9] #error duration
              #------------------------------------
              # Other variables
              if (acc_available == "TRUE") {
                
                
                nightsummary[sumi,13] = sleepdet.t$fraction.night.invalid[1]
                if (sleepdet.t$fraction.night.invalid[1] > ((24-includenightcrit)/24)) {
                  cleaningcode = 2
                }
              } else {
                nightsummary[sumi,13] = 1
              }
              nocs = as.numeric(spocum.t[which(spocum.t[,4] == 1),3]) - as.numeric(spocum.t[which(spocum.t[,4] == 1),2])
              sibds = as.numeric(spocum.t[which(spocum.t[,4] == 0),3]) - as.numeric(spocum.t[which(spocum.t[,4] == 0),2])
              if (length(nocs) > 0) {
                spocum.t.dur.noc = sum(nocs)
              } else {
                spocum.t.dur.noc = 0
              }
              if (length(sibds) > 0) {
                spocum.t.dur_sibd = sum(sibds)
              } else {
                spocum.t.dur_sibd = 0
              }
              nightsummary[sumi,14] = spocum.t.dur.noc #total nocturnalsleep /accumulated sleep duration
              nightsummary[sumi,15] = spocum.t.dur_sibd #total sib (sustained inactivty bout) duration
              nightsummary[sumi,16] = length(which(spocum.t[,4] == 1)) #number of nocturnalsleep periods
              nightsummary[sumi,17] = length(which(spocum.t[,4] == 0)) #number of sib (sustained inactivty bout) periods
              #-------------------------------------------------------
              # Also report timestamps in non-numeric format:
              acc_onset = nightsummary[sumi,3]
              acc_wake = nightsummary[sumi,4]
              if (acc_onset > 24) acc_onset = acc_onset - 24
              if (acc_wake > 24) acc_wake = acc_wake - 24
              #--------------------------------------------
              # convert into timestamp
              HRO = floor(acc_onset)
              MIO = floor((acc_onset - floor(acc_onset)) * 60)
              SEO = round(((acc_onset - HRO) - (MIO/60)) * 3600)
              if (SEO == 60) {
                MIO = MIO + 1; SEO = 0
              }
              if (MIO == 60){
                HRO = HRO + 1; MIO = 0
              }
              if (HRO == 24) {
                HRO = 0
              }
              HRW = floor(acc_wake)
              MIW = floor((acc_wake - floor(acc_wake)) * 60)
              SEW = round(((acc_wake - HRW) - (MIW/60)) * 3600)
              if (SEW == 60) {
                MIW = MIW + 1; SEW = 0
              }
              if (MIW == 60) {
                HRW = HRW + 1; MIW = 0
              }
              if (HRW == 24) {
                HRW = 0
              }
              #----------------------------------------------
              if (HRO < 9) HRO = paste("0",HRO,sep="")
              if (MIO < 9) MIO = paste("0",MIO,sep="")
              if (SEO < 9) SEO = paste("0",SEO,sep="")
              if (HRW < 9) HRW = paste("0",HRW,sep="")
              if (MIW < 9) MIW = paste("0",MIW,sep="")
              if (SEW < 9) SEW = paste("0",SEW,sep="")
              acc_onsetTS = paste(HRO,":",MIO,":",SEO,sep="")
              acc_wakeTS = paste(HRW,":",MIW,":",SEW,sep="")
              nightsummary[sumi,18] = acc_onsetTS
              nightsummary[sumi,19] = acc_wakeTS
              #----------------------------------------------
              nightsummary[sumi,20] = tmp1
              nightsummary[sumi,21] = tmp4
              nightsummary[sumi,22] = pagei
              nightsummary[sumi,23] = daysleeper[j]
              nightsummary[sumi,24] = wdayname[j]
              nightsummary[sumi,25] = calendardate[j]
              nightsummary[sumi,26] = fnames[i]
              # nightsummary
              #------------------------------------------------------------------------
              # PLOT     
              if (do.visual == TRUE) {
                if (defi == undef[1]) { #only decide whether to plot the first time
                  if (outliers.only == TRUE) {
                    if (abs(nightsummary$error_onset[sumi]) > criterror | abs(nightsummary$error_wake[sumi]) > criterror |
                        abs(nightsummary$error_dur[sumi]) > (criterror * 2)) {
                      doplot = TRUE
                      cat(" PLOT ")
                    } else {
                      doplot = FALSE
                    }
                  } else {
                    doplot = TRUE
                  }
                }
                # upcoming 5 lines added to avoid ending up with meaningless visualisations of nights
                # for which no sleep log entry was available, and for which L5 method provided estimates
                if (length(loglocation) > 0) { 
                  cleaningcriterion = 1
                } else {
                   cleaningcriterion = 2
                }
                if (doplot == TRUE & cleaningcode < cleaningcriterion) {
                  idlabels[cnt] = paste("id",accid," night",j,sep="")
                  den = 20
                  defii = which(undef == defi)              
                  qtop = ((defii / length(undef))*0.6) - 0.3
                  qbot = (((defii-1) / length(undef))*0.6) - 0.3
                  # add bar for each sleep defintion of accelerometer
                  for (pli in 1:nrow(spocum.t)) { 
                    if (spocum.t[pli,2] > spocum.t[pli,3]) {
                      if (pli > 1 & pli < nrow(spocum.t) & abs(as.numeric(spocum.t[pli,2]) - as.numeric(spocum.t[pli,3])) < 2) {
                        spocum.t[pli,2:3] = spocum.t[pli,3:2] #add 15/12/2014 to deal with effect of daysaving time.
                      }
                    }
                    if (spocum.t[pli,4] == 1) {
                      colb = rainbow(length(undef),start=0.7,end=1) #"dodgerblue"
                    } else {
                      colb =  rainbow(length(undef),start=0.2,end=0.4) #"darkgreen"
                    }
                    if (spocum.t[pli,2] > spocum.t[pli,3]) {
                      rect(xleft=spocum.t[pli,2], ybottom=(cnt+qbot), xright=36, ytop=(cnt+qtop),col=colb[defii],border=NA) #lwd=0.2,
                      rect(xleft=12, ybottom=(cnt+qbot), xright=spocum.t[pli,3], ytop=(cnt+qtop),col=colb[defii],border=NA) #lwd=0.2,
                    } else {
                      rect(xleft=spocum.t[pli,2], ybottom=(cnt+qbot), xright=spocum.t[pli,3], ytop=(cnt+qtop),col=colb[defii],border=NA) #lwd=0.2,
                    }
                  }
                  tmp6n = tmp6
                  tmp3n = tmp3
                  if (tmp6 > 36) tmp6n = tmp6 - 24
                  if (tmp3 > 36) tmp3n = tmp3 - 24
                  if (defi == undef[length(undef)]) {# only plot log for last definition
                    if (tmp3n > tmp6n) { #night sleeper
                      rect(xleft=tmp3n, ybottom=(cnt-0.3), xright=36, ytop=(cnt+0.3),col="black",border=TRUE,density=den) #lwd=0.2,
                      rect(xleft=12, ybottom=(cnt-0.3), xright=tmp6n, ytop=(cnt+0.3),col="black",border=TRUE,density=den) #lwd=0.2,
                    } else { #day sleeper
                      rect(xleft=tmp3n, ybottom=(cnt-0.3), xright=tmp6n, ytop=(cnt+0.3),col="black",border=TRUE,density=den) #lwd=0.2,
                    }
                  }
                }
              }
              # PLOT
              #------------------------------------------------------------------------
              nightsummary[sumi,27] = cleaningcode
              nightsummary[sumi,28] = sleeplog_used[i]
              nightsummary[sumi,29] = acc_available
              if (storefolderstructure == TRUE) {
                nightsummary[sumi,30] = ffd[i] #full filename structure
                nightsummary[sumi,31] = ffp[i] #use the lowest foldername as foldername name
              }
              
              
              sumi = sumi + 1
            } #run through definitions
            if (do.visual == TRUE) {
              if (cleaningcode < cleaningcriterion & doplot == TRUE) { #only increase count if there was bar plotted
                lines(x=c(12,36),y=c(cnt,cnt),lwd=0.2,lty=2)  #abline(h=cnt,lwd=0.2,lty=2)
                if (daysleeper[j] == TRUE) {
                  lines(x=c(18,18),y=c((cnt-0.3),(cnt+0.3)),lwd=2,lty=2,col="black")
                }
                cnt = cnt + 1
              }
            }
          }
          # END OF PLOT AND nightsummary MEASURES
          ##########################################################################
        } #nights
        if (length(nnights.list) == 0) { #if there were no nights to analyse
          nightsummary[sumi,1] = accid
          nightsummary[sumi,2] = 0 #night
          nightsummary[sumi,3:25] = NA #night
          nightsummary[sumi,26] = fnames[i]
          nightsummary[sumi,27] = 4 #cleaningcode = 4 (no nights of accelerometer available)
          nightsummary[sumi,28] = "FALSE" #sleeplog_used[i]
          nightsummary[sumi,29] = "TRUE" #acc_available
          if (storefolderstructure == TRUE) {
            nightsummary[sumi,30] = ffd[i] #full filename structure
            nightsummary[sumi,31] = ffp[i] #use the lowest foldername as foldername name
          }
          sumi = sumi + 1
        }
        save(nightsummary,file=paste(metadatadir,ms4.out,"/",fnames[i],sep=""))
      }
    }
  } #end of loop through acc files
  if (cnt67 == 2 & do.visual == TRUE) {
    if (cnt-1 != (nnpp+1)) {
      idlabels[which(idlabels == 0)] = " "
      
      axis(side=2, at = 1:nnpp,labels = idlabels,las=1,cex.axis=0.5)
    }
    dev.off()
    cnt67 = 1
  }
}