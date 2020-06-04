g.part4 = function(datadir=c(),metadatadir=c(),f0=f0,f1=f1,idloc=1,loglocation = c(),
                   colid = 1,coln1 = 9,nnights = 7,sleeplogidnum=FALSE,do.visual=FALSE,outliers.only = FALSE,
                   excludefirstlast=FALSE,criterror = 1,includenightcrit=16,
                   relyonguider=FALSE,relyonsleeplog=FALSE, def.noc.sleep=1,
                   storefolderstructure=FALSE,
                   overwrite=FALSE,desiredtz="",data_cleaning_file=c(),
                   excludefirst.part4=FALSE,excludelast.part4=FALSE) {


  if (exists("relyonsleeplog") == TRUE & exists("relyonguider") == FALSE)  relyonguider=relyonsleeplog
  # description: function to load sleep detection from g.part3 and to convert it into night-specific summary measures of sleep,
  # possibly aided by sleep log/diary information (if available and provided by end-user)

  nnpp = 40 # number of nights to be displayed in the report (hard-coded not a critical parameter for most scenarios)
  #------------------------------------------------
  # check whether milestone 3 data exists, if not give warning
  ms3.out = "/meta/ms3.out"
  if (file.exists(paste(metadatadir,ms3.out,sep=""))) {
  } else {
    cat("Warning: First run g.part3 (mode = 3) before running g.part4 (mode = 4)")
  }
  # check whether milestone 4 data exists, if no create folder
  ms4.out = "/meta/ms4.out"
  if (file.exists(paste(metadatadir,ms4.out,sep=""))) {
  } else {
    dir.create(file.path(metadatadir,ms4.out))
  }
  meta.sleep.folder = paste(metadatadir,"/meta/ms3.out",sep="")
  #------------------------------------------------
  # Get sleeplog data
  if (length(loglocation) > 0) {
    dolog = TRUE
  } else {
    dolog = FALSE
  }
  if (dolog == TRUE) {
    LL = g.loadlog(loglocation,coln1,colid,nnights,sleeplogidnum)
    sleeplog = LL$sleeplog
    save(sleeplog,file=paste(metadatadir,"/meta/sleeplog.RData",sep=""))
  }
  #------------------------------------------------
  # get list of accelerometer milestone data files from sleep (produced by g.part3)
  fnames = dir(meta.sleep.folder)
  if (f1 > length(fnames)) f1 = length(fnames)
  if (f0 > length(fnames)) f0 = 1
  if (f1 == 0 | length(f1) == 0 | f1 > length(fnames))  f1 = length(fnames)
  #-----------------------------------------------------
  # related to plotting
  cnt = 1 #counter to keep track of how many nights have been plotted in the visual report
  idlabels = rep(0,nnpp) # initialize vaiable for plot labels
  pagei = 1
  cnt67 = 1 # counter used to decide whether to create a new pdf file for the plots
  #-----------------------------------------------------
  # initialize output variable names
  colnamesnightsummary = c("ID", "night","sleeponset", "wakeup", "SptDuration", "sleepparam",
                           "guider_onset", "guider_wakeup", "guider_SptDuration",
                           "error_onset", "error_wake", "error_dur",
                           "fraction_night_invalid",
                           "SleepDurationInSpt","duration_sib_wakinghours","number_sib_sleepperiod","number_sib_wakinghours",
                           "duration_sib_wakinghours_atleast15min",
                           "sleeponset_ts","wakeup_ts","guider_onset_ts", "guider_wakeup_ts",
                           "page","daysleeper","weekday","calendar_date","filename",
                           "cleaningcode","sleeplog_used","acc_available","guider")
  if (storefolderstructure == TRUE) {
    colnamesnightsummary  = c(colnamesnightsummary,"filename_dir","foldername")
  }
  # initialize variable to hold sleeplog derived sleep duration
  # if not sleep log was used then the estimates of the sleep period time window
  # will be used instead
  logdur = rep(0,length(fnames))
  # initialize variable to keep whether sleeplog was used
  # if ((f1-f0) > 0) {
  #   sleeplog_used = rep(" ",((f1-f0)+1))
  # } else {
  #   sleeplog_used = " "
  # }

  #========================================================================
  # check which files have already been processed, such that no double work is done
  ffdone = c()
  ms4.out = "/meta/ms4.out"
  fnames.ms4 = dir(paste(metadatadir,ms4.out,sep=""))
  fnames.ms4 = sort(fnames.ms4)
  ffdone = fnames.ms4
  # ffdone a matrix with all the binary filenames that have been processed
  fnames = sort(fnames)
  #--------------------------------
  # get original file path of the accelerometer (some studies may like to keep track of original folder structure if their study structure is embodied in folder structure)
  if (storefolderstructure == TRUE) {
    filelist = FALSE
    if (length(datadir) == 1) { #could be a directory or one file
      if (length(unlist(strsplit(datadir,split = "[.](cs|bi|cw)")))>1) filelist = TRUE
    } else { #multiple files
      filelist = TRUE
    }
    if (filelist == FALSE) {
      fnamesfull = dir(datadir,recursive=TRUE,pattern="[.](csv|bin|cwa)")
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
  convertHRsinceprevMN2Clocktime = function(x) {
    # x = hours Since Previous Midnight
    if (x > 24) x = x - 24
    HR = floor(x)
    MI = floor((x - HR) * 60)
    SE = round(((x - HR) - (MI/60)) * 3600)
    if (SE == 60) { MI = MI + 1; SE = 0 }
    if (MI == 60) { HR = HR + 1; MI = 0 }
    if (HR == 24) HR = 0
    if (HR < 10) HR = paste0("0",HR)
    if (MI < 10) MI = paste0("0",MI)
    if (SE < 10) SE = paste0("0",SE)
    return(paste0(HR,":",MI,":",SE))
  }
  if (length(data_cleaning_file) > 0) { # allow for forced relying on guider based on external data_cleaning_file
    DaCleanFile = read.csv(data_cleaning_file)
  }
  #=================================================================
  #=================================================================
  # start of loop through the participants
  for (i in f0:f1) {
    # decide whether file was processed before
    if (overwrite == TRUE) {
      skip = 0 # this will make that analyses is done regardless of whether it was done before
    } else {
      skip = 0 #do not skip this file
      if (length(ffdone) > 0) {
        if (length(which(ffdone == fnames[i])) > 0) skip = 1 #skip this file because it was analysed before")
      }
    }
    if (skip == 0) {
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
      if (storefolderstructure == FALSE) { # initialize part4 output matrix per recording (file)
        nightsummary = as.data.frame(matrix(0,0,31))
      } else {
        nightsummary = as.data.frame(matrix(0,0,33))
      }
      colnames(nightsummary) = colnamesnightsummary
      sumi = 1 # counter to keep track of where we are in filling the output matrix 'nightsummary'
      sptwindow_HDCZA_end = sptwindow_HDCZA_start = L5list = sib.cla.sum = c()
      # load milestone 3 data (RData files), check whether there is data, identify id numbers...
      load(paste(meta.sleep.folder,"/",fnames[i],sep=""))
      if (nrow(sib.cla.sum) != 0) { #there needs to be some information
        sib.cla.sum$sib.onset.time = iso8601chartime2POSIX(sib.cla.sum$sib.onset.time, tz = desiredtz)
        sib.cla.sum$sib.end.time = iso8601chartime2POSIX(sib.cla.sum$sib.end.time, tz = desiredtz)
        #------------------------------------------------------
        # extract the identifier from accelerometer data
        if (idloc == 2) { #idloc is an argument to specify where the participant identifier can be found
          getCharBeforeUnderscore = function(x) {
            return(as.character(unlist(strsplit(x,"_")))[1])
          }
          accid = apply(as.matrix(as.character(fnames[i])),MARGIN=c(1),FUN=getCharBeforeUnderscore)
          accid_bu = accid
          getLastCharacterValue = function(x) {
            tmp = as.character(unlist(strsplit(x,"")))
            return(tmp[length(tmp)])
          }
          letter = apply(as.matrix(accid),MARGIN=c(1),FUN=getLastCharacterValue)
          for (h in 1:length(accid)) {
            options(warn=-1)
            numletter = as.numeric(letter[h])
            options(warn=0)
            if (is.na(numletter) == TRUE) { # do not remove latest character if it is a number
              accid[h] = as.character(unlist(strsplit(accid[h],letter[h]))[1])
            }
          }
          accid = suppressWarnings(as.numeric(accid))
          #catch for files with only id in filename and for whom the above attempt to extract the id failed:
          if (is.na(accid) == TRUE) accid = accid_bu
        } else { # get id from filename
          newaccid = fnames[i]
          if (length(unlist(strsplit(newaccid,"_"))) > 1) newaccid = unlist(strsplit(newaccid,"_"))[1]
          if (length(unlist(strsplit(newaccid,"[.]RDa"))) > 1) newaccid = unlist(strsplit(newaccid,"[.]RDa"))[1]
          if (length(unlist(strsplit(newaccid,"[.]cs"))) > 1) newaccid = unlist(strsplit(newaccid,"[.]cs"))[1]
          accid = newaccid[1]
        }
        # get matching identifier from sleeplog
        if (dolog == TRUE) {
          accid_num = suppressWarnings(as.numeric(accid))
          if (sleeplogidnum == FALSE) {
            wi = which(as.character(sleeplog$ID) == as.character(accid))
            if (length(wi) == 0) {
              wi_alternative = which(sleeplog$ID == accid_num)
              if (length(wi_alternative) > 0) {
                warning("\nArgument sleeplogidnum is set to FALSE, but it seems the identifiers are
                    stored as numeric values, you may want to consider changing sleeplogidnum to TRUE")
              } else {
                warning(paste0("\nSleeplog id is stored as format: ", as.character(sleeplog$ID[1]),", while
                           code expects format: ",as.character(accid[1])))
              }
            }
          } else {
            wi = which(sleeplog$ID == accid_num)
            if (length(wi) == 0) {
              wi_alternative = which(as.character(sleeplog$ID) == as.character(accid))
              if (length(wi_alternative) > 0) {
                warning("\nArgument sleeplogidnum is set to TRUE, but it seems the identifiers are
                    stored as character values, you may want to consider changing sleeplogidnum to TRUE")
              } else {

                if (is.na(accid_num) == TRUE) { # format probably incorrect
                  warning(paste0("\nSleeplog id is stored as format: ", as.character(sleeplog$ID[1]),", while
                           code expects format: ",as.character(accid[1])))
                }
              }
            }
          }

        } else {
          wi = 1
        }
        #-----------------------------------------------------------
        # create overview of night numbers in the data file: nnightlist
        if (length(nnights) == 0) {
          nnightlist = 1:max(sib.cla.sum$night) # sib.cla.sum is the output from g.part3
        } else {
          if (max(sib.cla.sum$night) < nnights) {
            nnightlist = 1:max(sib.cla.sum$night)
          } else {
            nnightlist = 1:nnights
          }
        }
        if (length(nnightlist) < length(wi)) nnightlist = nnightlist[1:length(wi)]
        # create overview of which night numbers in the file that have a value and are not equal to zero
        nnights.list = nnightlist
        nnights.list = nnights.list[which(is.na(nnights.list) == FALSE & nnights.list != 0)]
        if (excludefirstlast==TRUE & excludelast.part4 == FALSE & excludefirst.part4 == FALSE) {#exclude first and last night
          if (length(nnights.list) >= 3) {
            nnights.list = nnights.list[2:(length(nnights.list)-1)]
          } else {
            nnights.list = c()
          }
        } else if (excludelast.part4 == FALSE & excludefirst.part4 == TRUE) {
          if (length(nnights.list) >= 2) {
            nnights.list = nnights.list[2:length(nnights.list)]
          } else {
            nnights.list = c()
          }
        } else if (excludelast.part4 == TRUE & excludefirst.part4 == FALSE) {
          if (length(nnights.list) >= 2) {
            nnights.list = nnights.list[1:(length(nnights.list)-1)]
          } else {
            nnights.list = c()
          }
        }
        # initialize variables calendardate and daysleeper from the nnights.list variable
        calendar_date = wdayname = rep("",length(nnights.list))
        # daysleeper variable to keep track of whether the person woke up after noon or a certain day
        daysleeper = rep(FALSE,length(nnights.list))
        ###########################################################
        nightj = 1
        if (dolog == TRUE) sleeplog.t = sleeplog[wi,]
        for (j in nnights.list) { #go through the nights
          ######################################
          # get default onset and wake (based on sleeplog or on heuristic algorithms)
          # def.noc.sleep is an input argument the GGIR user can use
          # to specify what detection strategy is used in the absense of a sleep diary

          if (length(def.noc.sleep) == 0 | length(sptwindow_HDCZA_start) == 0) {
            # use L5+/-6hr algorithm if HDCZA fails OR if the user explicitely asks for it (length zero argument)
            guider = "notavailable"
            if (length(L5list) > 0) {
              defaultSptOnset = L5list[j] - 6
              defaultSptWake = L5list[j] + 6
              guider = "L512"
            }
          } else if (length(def.noc.sleep) == 1 | length(loglocation) != 0 & length(sptwindow_HDCZA_start) != 0) {
            # use HDCZA algorithm (inside the g.sib.det function) as backup for sleeplog OR if user explicitely asks for it
            defaultSptOnset = sptwindow_HDCZA_start[j]
            defaultSptWake = sptwindow_HDCZA_end[j]
            guider = "HDCZA"
            if (is.na(defaultSptOnset) == TRUE) { # If HDCZA was not derived for this night, use average estimate for other nights
              availableestimate = which(is.na(sptwindow_HDCZA_start) == FALSE)
              cleaningcode = 6
              if (length(availableestimate) > 0) {
                defaultSptOnset = mean(sptwindow_HDCZA_start[availableestimate])
              } else {
                defaultSptOnset = L5list[j] - 6
                guider = "L512"
              }
            }
            if (is.na(defaultSptWake) == TRUE) { # If HDCZA was not derived for this night, use average estimate for other nights
              availableestimate = which(is.na(sptwindow_HDCZA_end) == FALSE)
              cleaningcode = 6
              if (length(availableestimate) > 0) {
                defaultSptWake = mean(sptwindow_HDCZA_end[availableestimate])
              } else {
                defaultSptWake = L5list[j] + 6
                guider = "L512"
              }
            }
          } else if (length(def.noc.sleep) == 2) {
            # use constant onset and waking time as specified with def.noc.sleep argument
            defaultSptOnset = def.noc.sleep[1] #onset
            defaultSptWake = def.noc.sleep[2] #wake
            guider = "setwindow"
          }
          if (defaultSptOnset >= 24) defaultSptOnset = defaultSptOnset - 24
          if (defaultSptWake >= 24) defaultSptWake = defaultSptWake - 24
          defaultdur = defaultSptWake - defaultSptOnset #default sleep duration based on sleeplog, L5+/-6hr, or HDCZA algorithm
          sleeplog_used = FALSE
          if (dolog == TRUE) {
            if (is.na(sleeplog[wi[j],3]) == FALSE) {  #length(wi)  > 0 &
              #-----------------------------------------------------------
              #If sleep log is available for a specific night then use it
              sleeplog_used =  TRUE
              cleaningcode = 0
              guider = "sleeplog"
            }
          }
          if (sleeplog_used == FALSE) {
            #-----------------------------------------------------------
            #If sleep log is not available available, use default values calculated above (with the heuristic algorithm HDCZA or if that fails L5+/-6hr.
            if (j == nnights.list[1] & dolog == FALSE) {
              sleeplog.t = data.frame(matrix(0,length(nnightlist),5))
              names(sleeplog.t) = c("ID","night","duration","sleeponset","sleepwake")
            }
            sleeplog.t[j,1:5] = c(accid, j, defaultdur,
                                       convertHRsinceprevMN2Clocktime(defaultSptOnset),
                                       convertHRsinceprevMN2Clocktime(defaultSptWake))
            cleaningcode = 1
          }
          nightj = nightj + 1
          # keep track of whether enough accelerometer data is available for each night
          acc_available = TRUE #default assumption
          # initialize dataframe to hold sleep period overview:
          spocum = as.data.frame(matrix(0,0,5))
          spocumi = 1 # counter for sleep periods
          # continue now with the specific data of the night
          sleeplog.t2 = sleeplog.t[which(sleeplog.t$night == j),]
          #================================================================================
          # get sleeplog (or HDCZA or L5+/-6hr algorithm) onset and waking time and assess whether it is a nightworker
          # onset
          tmp1 = as.character(sleeplog.t2[,which(names(sleeplog.t2) == "sleeponset")])
          tmp2 = unlist(strsplit(tmp1,":"))
          SptOnset = as.numeric(tmp2[1]) + (as.numeric(tmp2[2])/60) + (as.numeric(tmp2[3])/3600)
          # wake
          tmp4 = as.character(sleeplog.t2[,which(names(sleeplog.t2) == "sleepwake")])
          tmp5 = unlist(strsplit(tmp4,":"))
          SptWake = as.numeric(tmp5[1]) + (as.numeric(tmp5[2])/60) + (as.numeric(tmp5[3])/3600)
          # Assess whether it is a daysleeper or a nightsleeper
          daysleeper[j] = FALSE # default
          if (is.na(SptOnset) == FALSE & is.na(SptWake) == FALSE & tmp1 != "" & tmp4 != "") { # is the sleep log valid?
            # transform possible Single Digit clock times to Double Digits: hours, minutes and seconds
            doubleDigitClocktime = function(x) {
              x = unlist(strsplit(x,":"))
              xHR = as.numeric(x[1])
              xMI = as.numeric(x[2])
              xSE = as.numeric(x[3])
              if (xHR < 10) xHR = paste("0",xHR,sep="")
              if (xMI < 10) xMI = paste("0",xMI,sep="")
              if (xSE < 10) xSE = paste("0",xSE,sep="")
              x = paste(xHR,":",xMI,":",xSE,sep="")
              return(x)
            }
            tmp1 = doubleDigitClocktime(tmp1)
            tmp4 = doubleDigitClocktime(tmp4)
            #------------------------------------------------------------------
            # does sleep period overlap with noon? If yes, then classify as daysleeper
            if (SptWake > 12 & SptOnset < 12) daysleeper[j] = TRUE
            if (SptWake > 12 & SptOnset>SptWake) daysleeper[j] = TRUE
            # change time stamps to be a continues time
            if (SptOnset < 12) SptOnset = SptOnset + 24 #shift 24 hours to create continues time
            if (SptWake <= 12) SptWake = SptWake + 24 #shift 24 hours to create continues time
            if (SptWake > 12 & SptWake < 18 & daysleeper[j] == TRUE) SptWake = SptWake + 24 # NEW 10/5/2018 by Vincent
            if (daysleeper[j] == TRUE) {
              logdur[i] = SptOnset - SptWake
            } else {
              logdur[i] = SptWake - SptOnset
            }
          } else {
            SptOnset = defaultSptOnset #use default assumption about onset
            SptWake = defaultSptWake + 24 #use default assumption about wake
            logdur[i] = SptWake - SptOnset
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
              if (SptWake > 36) SptWake = 36 #round diary to noon
              logdur[i] = SptWake - SptOnset
            }
          } else { #if you are excluding the last day
            if (daysleeper[j] == TRUE) { #dont worrry about whether it is the last day, because it is not included
              loaddays = 2
            } else {
              loaddays = 1
            }
          }
          #now generate empty overview for this night / person
          dummyspo = matrix(0,1,5); dummyspo[1,1] = 1
          spo_day = c()
          #============================================================================================
          for (loaddaysi in 1:loaddays) { #load twice if daysleeper because we also need data from the afternoon on the next day
            # now get accelerometer sleep detection
            qq = sib.cla.sum
            sleepdet = qq[which(qq$night == (j+(loaddaysi-1))),] ##
            if (nrow(sleepdet) == 0) {
              if (spocumi == 1) {
                spocum = dummyspo
              } else {
                spocum = rbind(spocum,dummyspo)
              }
              spocumi = spocumi + 1
              cleaningcode = 3
              acc_available = FALSE
            } else {
              acc_available = TRUE
            }
            # we now have sleeplog (or HDCZA or L5+/-6hr) data for one night (sleeplog.t2)
            # and we have acc data for one night (sleepdet)
            defs = unique(sleepdet$definition) # definition of sleep episode metric (see van Hees 2015 PLoSONE paper)
            for (k in defs) {
              ki = which(sleepdet$definition == k)
              sleepdet.t = sleepdet[ki,]
              if (loaddaysi == 1) remember_fraction_invalid_day1 = sleepdet.t$fraction.night.invalid[1]
              # now get sleep periods
              nsp = length(unique(sleepdet.t$sib.period)) #number of sleep periods
              spo = matrix(0,nsp,5) # overview of sleep periods
              if (nsp <= 1 & unique(sleepdet.t$sib.period)[1] == 0) { # no sleep periods
                spo[1,1] = 1
                spo[1,2:4] = 0
                spo[1,5] = k
                if (daysleeper[j] == TRUE) {
                  tmpCmd = paste("spo_day",k,"= c()",sep="") ##
                  eval(parse(text = tmpCmd)) ##
                }
              } else {
                DD = g.create.sp.mat(nsp,spo,sleepdet.t,daysleep=daysleeper[j])
                if (loaddaysi == 1) { # newly added 25/11/2015
                  wdayname[j] = DD$wdayname
                  calendar_date[j] = DD$calendar_date
                }
                spo = DD$spo
                reversetime2 = reversetime3 = c()
                if (daysleeper[j] == TRUE) {
                  if (loaddaysi == 1) {
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
                  } else if (loaddaysi == 2 & length(eval(parse(text = paste0("spo_day",k)))) > 0) { #length check added because day may have been skipped
                    w2 = which(spo[,2] < 18) #only use periods starting before 6pm
                    if (length(w2) > 0) {
                      spo = as.matrix(spo[w2,])
                      if (ncol(spo) == 1) spo = t(spo)
                      if (nrow(spo) == 1) {
                        if (spo[1,3] > 18) spo[1,3] = 18 #turn end time on 2nd day after 6pm to 6pm
                      } else {
                        spo[which(spo[,3] > 18),3] = 18 #turn end times on 2nd day after 6pm to 6pm
                      }
                      spo[,2:3] = spo[,2:3]+ 24 # + 24 to create continues timelines for day 2 relative to day 1
                      tmpCmd = paste("spo_day2",k,"= spo",sep="") #spo needs to be rememered specific to definition
                      eval(parse(text = tmpCmd))

                    } else {
                      tmpCmd = paste("spo_day2",k,"= c()",sep="")
                      eval(parse(text = tmpCmd))
                    }
                    #attach days together as being one day
                    name1 = paste("spo_day",k,sep="")
                    name2 = paste("spo_day2",k,sep="")
                    tmpCmd = paste("spo = rbind(",name1,",",name2,")",sep="")
                    eval(parse(text=tmpCmd))
                  }
                }
                if (daysleeper[j] == TRUE) {
                  if (SptWake < 21 & SptWake > 12 &  SptOnset > SptWake) # waking up in the afternoon should have value above 36
                  SptWake = SptWake + 24
                }
                # Detect whether none of the SIBs overlaps with the SPT, if this is the case
                # then probably the night is not going to be very useful for sleep analysis
                # however, for part 5 we may still want to have some estimate to define the
                # waking-up to waking windows.
                # So, we will now add two tiny artificial sibs (1-minute) at the beginning and end
                # of the guider-based SPT to make that the SPT is still detected guided entirely by the guider.
                # and add cleaningcode = 5, this means that the night
                # will be omitted from the part 4 cleaned results,
                # but is still available for inspection in the full part 4
                # results and available
                # for part 5, because in part 5 we are only interested in the edges of the SPT and not what
                # happens in it.
                relyonguider_thisnight = FALSE
                if (length(data_cleaning_file) > 0) {
                  if (length(which(DaCleanFile$relyonguider_part4 == j & DaCleanFile$ID == accid)) > 0) {
                    relyonguider_thisnight = TRUE
                  }
                }
                if (length(which(spo[,2] < SptWake & spo[,3] > SptOnset)) == 0 | # If no SIBs overlap with the SPT window
                    relyonguider_thisnight == TRUE ) { # If night is explicitely listed
                  cleaningcode = 5
                  newlines = rbind(spo[1,],spo[1,])
                  newlines[1,1:4] = c(nrow(spo)+1, SptOnset, SptOnset + 1/60, 1)
                  newlines[2,1:4] = c(nrow(spo)+1, SptWake - 1/60, SptWake, 1)
                  spo = rbind(spo, newlines)
                  spo = spo[order(spo[,2]),]
                  spo[,1] = 1:nrow(spo)
                  relyonguider_thisnight = TRUE
                }
                # spo is now a matrix of onset and wake for each sleep period (episode)
                for (evi in 1:nrow(spo)) { #Now classify as being part of the SPT window or not
                  if (spo[evi,2] < SptWake & spo[evi,3] > SptOnset) { # = acconset < logwake  & accwake > logonset
                    spo[evi,4] = 1 #nocturnal = all acc periods that end after diary onset and start before diary wake
                    # REDEFINITION OF ONSET/WAKE OF THIS PERIOD OVERLAPS
                    if (relyonguider == TRUE | relyonguider_thisnight == TRUE) { #if TRUE then sleeplog value is assigned to accelerometer-based value for onset and wake up
                      if ((spo[evi,2] < SptWake & spo[evi,3] > SptWake) | (spo[evi,2] < SptWake & spo[evi,3] < spo[evi,2])) {
                        spo[evi,3] = SptWake
                      }
                      if ((spo[evi,2] < SptOnset & spo[evi,3] > SptOnset) | (spo[evi,3] > SptOnset & spo[evi,3] < spo[evi,2])) {
                        spo[evi,2] = SptOnset
                      }
                    }
                  }
                }
                if (daysleeper[j] == TRUE) {
                  # for the labelling above it was needed to have times > 36, but for the plotting
                  # time in the second day needs to be returned to a normal 24 hour scale.
                  reversetime2 = which(spo[,2] >= 36)
                  reversetime3 = which(spo[,3] >= 36)
                  if (length(reversetime2) > 0) spo[reversetime2,2] = spo[reversetime2,2] - 24
                  if (length(reversetime3) > 0) spo[reversetime3,3] =  spo[reversetime3,3] - 24
                }
                #------------------------------------------------------------------------
                # Variable 'spo' contains all the sleep periods FOR ONE SLEEP DEFINITION
                # Variable 'spocum' contains all the sleep periods FOR MULTIPLE SLEEP DEFINITIONS
                spo[,5] = k
                if (spocumi == 1) {
                  spocum = spo
                } else {
                  spocum = rbind(spocum,spo)
                }
                spocumi = spocumi + 1
              }
            }
          }
          #------------------------------------------------------------------------
          # Take variables 'spocum', SptOnset and SptWake to derive nightsummary measures and to create a plot
          # for the current night in the current participant
          #------------------------------------------------------------------------
          # PLOTTING related
          if (do.visual ==  TRUE) {
            if (cnt == (nnpp+1)) {
              cat(" NEW ")
              pagei = pagei + 1
              # add y-axis before starting new page
              if (length(idlabels) < nnpp) {
                idlabels = c(idlabels, rep(" ",length(idlabels) - nnpp))
              } else if (length(idlabels) > nnpp) {
                idlabels = idlabels[1:nnpp]
              }
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
          if (length(spocum) > 0) {
            if (length(which(spocum[,5] == "0")) > 0){
              spocum = spocum[-which(spocum[,5]== "0"),]
            }
          }
          # Fill matrix 'nightsummary' with key sleep parameters
          if (length(spocum) > 0 & class(spocum)[1] == "matrix" & length(calendar_date) >= j) {
            if (nrow(spocum) > 1 & ncol(spocum) >= 5 & calendar_date[j] != "") {
              undef = unique(spocum[,5])
              for (defi in undef) {
                #------------------------------------------------------------------------
                # nightsummary
                rowswithdefi = which(spocum[,5] == defi)
                if(length(rowswithdefi) > 1) { # only process day if there are at least 2 sustained inactivity bouts
                  spocum.t = spocum[rowswithdefi,]

                  # in DST it can be that a double hour is not recognized as part of the SPT
                  correct01010pattern = function(x) {
                    x = as.numeric(x)
                    if (length(which(diff(x) == 1)) > 1) {
                      minone = which(diff(x) == -1)+1
                      plusone = which(diff(x) == 1)
                      matchingvalue = which(minone %in% plusone == TRUE)
                      if(length(matchingvalue) > 0) x[minone[matchingvalue]] = 1
                    }
                    return(x)
                  }
                  #sbefore = spocum.t[,4]
                  delta_t1 = diff(as.numeric(spocum.t[,3]))

                  spocum.t[,4] = correct01010pattern(spocum.t[,4])
                  #----------------------------
                  nightsummary[sumi,1] = accid
                  nightsummary[sumi,2] = j #night
                  if (is.matrix(spocum.t) == FALSE) {
                    spocum.t = t(as.matrix(spocum.t))
                  }
                  #remove double rows
                  spocum.t = spocum.t[!duplicated(spocum.t),]
                  #------------------------------------
                  # ACCELEROMETER
                  if (is.matrix(spocum.t) == FALSE) spocum.t = as.matrix(spocum.t) # seems needed in rare occasions
                  if (ncol(spocum.t) < 4 & nrow(spocum.t) > 3) spocum.t = t(spocum.t) # seems needed in rare occasions
                  if (length(which(as.numeric(spocum.t[,4]) == 1)) > 0) {
                    rtl = which(spocum.t[,4] == 1)
                    nightsummary[sumi,3] =spocum.t[rtl[1],2]
                    nightsummary[sumi,4] =spocum.t[rtl[length(rtl)],3]
                  } else {
                    cleaningcode = 5 # only for first day, other cleaningcode is assigned to wrong day
                    nightsummary[sumi,3] = SptOnset #use default assumption about onset
                    nightsummary[sumi,4] = SptWake #use default assumption about wake
                  }
                  nightsummary[,3] = as.numeric(nightsummary[,3]) # onset
                  nightsummary[,4] = as.numeric(nightsummary[,4]) # wake
                  if (nightsummary[sumi,3] > nightsummary[sumi,4] & # onset after wake is impossible
                      nightsummary[sumi,4] < 36 & daysleeper[j] == TRUE) {  # even more impossible if wake occurs before none, while we previously labelled it as daysleep
                    nightsummary[sumi,4] = nightsummary[sumi,4] + 24 # correction for overcorrection in waking time
                  }
                  if (nightsummary[sumi,3] == nightsummary[sumi,4] & nightsummary[sumi,4] == 18) { # sleeping from 6pm to 6pm (probably non-wear)
                    nightsummary[sumi,4] = nightsummary[sumi,4] + 24
                  }
                  # if (nightsummary[sumi,3] > nightsummary[sumi,4]) {
                  #   nightsummary[sumi,5] = (36 - nightsummary[sumi,3]) + (nightsummary[sumi,4] - 12)
                  # } else {
                  nightsummary[sumi,5] = nightsummary[sumi,4] - nightsummary[sumi,3] #sleep duration within Spt
                  # }
                  nightsummary[,5] = as.numeric(nightsummary[,5])
                  nightsummary[sumi,6] = defi #sleep definition
                  #------------------------------------
                  # SLEEP LOG
                  #correct SptOnset and SptWake to fall within [12-36] window and store as sleeplog_onset and sleeplog_wake, except when it is a daysleeper
                  if (SptOnset > 36) {
                    nightsummary[sumi,7] = SptOnset-24 #onset
                  } else {
                    nightsummary[sumi,7] = SptOnset
                  }
                  if (SptWake > 36 & daysleeper[j] == FALSE) {
                    nightsummary[sumi,8] = SptWake-24 #wake
                  } else {
                    nightsummary[sumi,8] = SptWake
                  }
                  if (nightsummary[sumi,7] > nightsummary[sumi,8]) {
                    nightsummary[sumi,9] = abs((36 - nightsummary[sumi,7]) + (nightsummary[sumi,8] - 12))
                  } else {
                    nightsummary[sumi,9] = abs(nightsummary[sumi,8] - nightsummary[sumi,7])
                  }
                  #------------------------------------
                  # Calculate errors between accelerometer estimate and sleeplog (or HDCZA or L5+/-6hr)
                  nightsummary[sumi,10] = nightsummary[sumi,3] - nightsummary[sumi,7] #error onset
                  nightsummary[sumi,11] = nightsummary[sumi,4] - nightsummary[sumi,8] #error wake
                  #sometimes difference calculations (error) can be in the wrong direction, e.g. log = 11, acc is 35
                  if (nightsummary[sumi,10] > 12)   nightsummary[sumi,10] = -(24 - nightsummary[sumi,10])
                  if (nightsummary[sumi,10] < -12)  nightsummary[sumi,10] = -(nightsummary[sumi,10] + 24)
                  if (nightsummary[sumi,11] > 12)   nightsummary[sumi,11] = -(24 - nightsummary[sumi,11])
                  if (nightsummary[sumi,11] < -12)  nightsummary[sumi,11] = -(nightsummary[sumi,11] + 24)
                  nightsummary[sumi,12] = nightsummary[sumi,5] - nightsummary[sumi,9] #error duration
                  #------------------------------------
                  # Other variables
                  if (acc_available == TRUE) {
                    nightsummary[sumi,13] = remember_fraction_invalid_day1 #sleepdet.t$fraction.night.invalid[1]
                    if (remember_fraction_invalid_day1 > ((24-includenightcrit)/24)) {
                      cleaningcode = 2 # only for first day, other cleaningcode is assigned to wrong day
                    }
                  } else {
                    nightsummary[sumi,13] = 1
                  }
                  # Accumulated nocturnal sleep and daytime sustained inactivity bouts
                  nocs = as.numeric(spocum.t[which(spocum.t[,4] == 1),3]) - as.numeric(spocum.t[which(spocum.t[,4] == 1),2])
                  sibds = as.numeric(spocum.t[which(spocum.t[,4] == 0),3]) - as.numeric(spocum.t[which(spocum.t[,4] == 0),2])
                  # it is possible that nocs is negative if when sleep episode starts before dst
                  # in the autumn and ends inside the dst hour
                  negval = which(nocs < 0)
                  if (length(negval) > 0) {
                    kk0 = as.numeric(spocum.t[which(spocum.t[,4] == 1),2]) # episode onsets
                    kk1 = as.numeric(spocum.t[which(spocum.t[,4] == 1),3]) # episode endings
                    kk1[negval] = kk1[negval] + 1
                    nocs = kk1 - kk0
                  }
                  if (length(nocs) > 0) {
                    spocum.t.dur.noc = sum(nocs)
                  } else {
                    spocum.t.dur.noc = 0
                  }
                  #======================================================================================================
                  # check whether it is day saving time (DST) the next day (= the night connected to the present day)
                  is_this_a_dst_night_output = is_this_a_dst_night(calendar_date=calendar_date[j],tz=desiredtz)
                  dst_night_or_not = is_this_a_dst_night_output$dst_night_or_not
                  dsthour = is_this_a_dst_night_output$dsthour
                  # if yes, then check whether any of the sleep episodes overlaps
                  if (dst_night_or_not == 1) { # dst in spring, one hour skipped
                    checkoverlap = spocum.t[which(spocum.t[,4] == 1),2:3]
                    if (length(checkoverlap) > 0 & is.matrix(checkoverlap) == TRUE) {
                      overlaps = which(checkoverlap[,1] <= (dsthour+24) & checkoverlap[,2] >= (dsthour+25))
                    } else {
                      overlaps = c()
                    }
                    if (length(overlaps) > 0) {
                      # if yes, then reduce the length of those sleep episodes
                      spocum.t.dur.noc = spocum.t.dur.noc - 1
                      nightsummary[sumi,5] = nightsummary[sumi,5] - 1
                      nightsummary[sumi,9] = nightsummary[sumi,9] - 1
                    }
                  } else if (dst_night_or_not == -1) { # dst in autumn, one double hour
                    # spocum.t.dur.noc has been calculated correctly including the double hour
                    # However, time elapse between onset and wake needs to be expanded by 1 if onset was
                    # before dst and waking up was after dst.
                    if (nightsummary[sumi,3] <= (dsthour+24) & nightsummary[sumi,4] >= (dsthour+25)) {
                      nightsummary[sumi,5] = nightsummary[sumi,5] + 1 # accelerometer derived sleep duration
                    }
                    if (nightsummary[sumi,7] <= (dsthour+24) & nightsummary[sumi,8] >= (dsthour+25)) {
                      nightsummary[sumi,9] = nightsummary[sumi,9] + 1 # sleep log sleepduration
                    }
                    # does SPT end within double hour?
                    correctSptEdgingInDoubleHour = function(nightsummary,onsetcol,wakecol,durcol,dsthour,delta_t1) {
                      wakeInDoubleHour = nightsummary[,wakecol] >= (dsthour+24) & nightsummary[,wakecol] <= (dsthour+25)
                      onsetInDoubleHour = nightsummary[,onsetcol] >= (dsthour+24) & nightsummary[,onsetcol] <= (dsthour+25)
                      onsetBeforeDoubleHour = nightsummary[,onsetcol] <= (dsthour+24)
                      wakeAfterDoubleHour = nightsummary[,wakecol] >= (dsthour+25)
                      timeWentBackward = length(which(delta_t1 < 0)) > 0
                      if (onsetBeforeDoubleHour == TRUE & wakeInDoubleHour == TRUE ) {
                        if (timeWentBackward == TRUE) { # if time went back then it ended in the second hour of the double hour and the + 1 is definitely justified
                          nightsummary[,durcol] = nightsummary[,durcol] + 1
                        } else if (timeWentBackward == FALSE) {
                          # if time did not go back then SPT may have ended in either the first or second of the double hour
                          # TO DO: distinguish these rare cases, for now assume it ends in the second hour to avoid sleep efficiency above 100%.
                          nightsummary[,durcol] = nightsummary[,durcol] + 1
                        }
                      }
                      if (wakeAfterDoubleHour == TRUE & onsetInDoubleHour == TRUE) {
                        if (timeWentBackward == TRUE) { # if time went back then it ended in the second hour of the double hour and the + 1 is definitely justified
                          nightsummary[,durcol] = nightsummary[,durcol] + 1 # accelerometer derived sleep duration
                        } else if (timeWentBackward == FALSE) {
                          # if time did not go back then SPT may have ended in either the first or second of the double hour
                          # TO DO: distinguish these rare cases, for now assume it ends in the second hour to avoid sleep efficiency above 100%.
                          nightsummary[,durcol] = nightsummary[,durcol] + 1 # accelerometer derived sleep duration
                        }
                      }
                      return(nightsummary)
                    }
                    nightsummary[sumi,] = correctSptEdgingInDoubleHour(nightsummary[sumi,],onsetcol=3,wakecol=4,durcol=5,dsthour=dsthour,delta_t1=delta_t1)
                    nightsummary[sumi,] = correctSptEdgingInDoubleHour(nightsummary[sumi,],onsetcol=7,wakecol=8,durcol=9,dsthour=dsthour,delta_t1=delta_t1)
                  }

                  #======================================================================================================
                  sibds_atleast15min = 0
                  if (length(sibds) > 0) {
                    spocum.t.dur_sibd = sum(sibds)
                    atleast15min = which(sibds >= 1/4)
                    if (length(atleast15min) > 0) {
                      sibds_atleast15min = sibds[atleast15min]
                      spocum.t.dur_sibd_atleast15min = sum(sibds_atleast15min)
                    } else {
                      spocum.t.dur_sibd_atleast15min = 0
                    }
                  } else {
                    spocum.t.dur_sibd = 0
                    spocum.t.dur_sibd_atleast15min = 0
                  }
                  nightsummary[sumi,14] = spocum.t.dur.noc #total nocturnalsleep /accumulated sleep duration
                  nightsummary[sumi,15] = spocum.t.dur_sibd #total sib (sustained inactivty bout) duration during wakinghours
                  nightsummary[sumi,16] = length(which(spocum.t[,4] == 1)) #number of nocturnalsleep periods
                  nightsummary[sumi,17] = length(which(spocum.t[,4] == 0)) #number of sib (sustained inactivty bout) during wakinghours
                  nightsummary[sumi,18] = as.numeric(spocum.t.dur_sibd_atleast15min) #total sib (sustained inactivty bout) duration during wakinghours of at least 5 minutes
                  #-------------------------------------------------------
                  # Also report timestamps in non-numeric format:
                  acc_onset = nightsummary[sumi,3]
                  acc_wake = nightsummary[sumi,4]
                  if (acc_onset > 24) acc_onset = acc_onset - 24
                  if (acc_wake > 24) acc_wake = acc_wake - 24
                  #--------------------------------------------
                  # convert into clocktime
                  acc_onsetTS = convertHRsinceprevMN2Clocktime(acc_onset)
                  acc_wakeTS = convertHRsinceprevMN2Clocktime(acc_wake)
                  nightsummary[sumi,19] = acc_onsetTS
                  nightsummary[sumi,20] = acc_wakeTS
                  #----------------------------------------------
                  nightsummary[sumi,21] = tmp1
                  nightsummary[sumi,22] = tmp4
                  nightsummary[sumi,23] = pagei
                  nightsummary[sumi,24] = daysleeper[j]
                  nightsummary[sumi,25] = wdayname[j]
                  nightsummary[sumi,26] = calendar_date[j]
                  nightsummary[sumi,27] = fnames[i]
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
                      idlabels[cnt] = paste("ID",accid," night",j,sep="")
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
                      SptWaken = SptWake
                      SptOnsetn = SptOnset
                      if (SptWake > 36) SptWaken = SptWake - 24
                      if (SptOnset > 36) SptOnsetn = SptOnset - 24
                      if (defi == undef[length(undef)]) {# only plot log for last definition
                        if (SptOnsetn > SptWaken) { #night sleeper
                          rect(xleft=SptOnsetn, ybottom=(cnt-0.3), xright=36, ytop=(cnt+0.3),col="black",border=TRUE,density=den) #lwd=0.2,
                          rect(xleft=12, ybottom=(cnt-0.3), xright=SptWaken, ytop=(cnt+0.3),col="black",border=TRUE,density=den) #lwd=0.2,
                        } else { #day sleeper
                          rect(xleft=SptOnsetn, ybottom=(cnt-0.3), xright=SptWaken, ytop=(cnt+0.3),col="black",border=TRUE,density=den) #lwd=0.2,
                        }
                      }
                    }
                  }
                  # PLOT
                  #------------------------------------------------------------------------
                  nightsummary[sumi,28] = cleaningcode
                  nightsummary[sumi,29] = sleeplog_used
                  nightsummary[sumi,30] = acc_available
                  nightsummary[sumi,31] = guider

                  if (storefolderstructure == TRUE) {
                    nightsummary[sumi,32] = ffd[i] #full filename structure
                    nightsummary[sumi,33] = ffp[i] #use the lowest foldername as foldername name
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
            }
          }
          # END OF PLOT AND nightsummary MEASURES
          ##########################################################################
        } #nights
        if (length(nnights.list) == 0) { #if there were no nights to analyse
          nightsummary[sumi,1:2] = c(accid, 0)
          nightsummary[sumi,3:26] = NA
          nightsummary[sumi,27] = fnames[i]
          nightsummary[sumi,28] = 4 #cleaningcode = 4 (no nights of accelerometer available)
          nightsummary[sumi,29:31] = c(FALSE, TRUE, "NA") #sleeplog_used acc_available
          if (storefolderstructure == TRUE) {
            nightsummary[sumi,32:33] = c(ffd[i], ffp[i]) #full filename structure and use the lowest foldername as foldername name
          }
          sumi = sumi + 1
        }
        save(nightsummary,file=paste(metadatadir,ms4.out,"/",fnames[i],sep=""))
      }
    }
  } #end of loop through acc files
  if (cnt67 == 2 & do.visual == TRUE) {
    if (cnt-1 != (nnpp+1)) {
      zerolabel = which(idlabels == 0)
      if (length(zerolabel) > 0) idlabels[zerolabel] = " "
      axis(side=2, at = 1:nnpp,labels = idlabels,las=1,cex.axis=0.5)
    }
    dev.off()
    cnt67 = 1
  }

  SI = sessionInfo()
  sessionInfoFile = paste(metadatadir,"/results/QC/sessioninfo_part4.RData",sep="")
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
