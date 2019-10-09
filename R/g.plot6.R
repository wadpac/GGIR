g.plot6 = function(metadatadir=c(),dofirstpage=FALSE, viewingwindow = 1,f0=c(),f1=c(),overwrite=FALSE,
                   desiredtz = "Europe/London",threshold.lig=30,threshold.mod=100,threshold.vig=400) {
  metric="ENMO" 
  if (file.exists(paste(metadatadir,"/results/file summary reports",sep=""))) {
    fnames.fsr = sort(dir(paste(metadatadir,"/results/file summary reports",sep="")))
    ffdone = fnames.fsr #ffdone is now a list of files that have already been processed by g.part5
  } else {
    dir.create(file.path(paste(metadatadir,"/results",sep=""),"file summary reports"))
    ffdone = c()
  }
  # directories
  meta = paste(metadatadir,"/meta/basic",sep="")
  metasleep = paste(metadatadir,"/meta/ms3.out",sep="")
  ms4 = paste(metadatadir,"/meta/ms4.out",sep="")
  results = paste(metadatadir,"/results",sep="")
  # get list of filenames
  fname_m = dir(meta)
  fname_ms = dir(metasleep)
  cave = function(x) as.character(unlist(strsplit(x,".RDa")))[1]
  x = as.matrix(as.character(fname_m))
  temp1 = apply(x,MARGIN=c(1),FUN=cave)
  cave2 = function(x) as.character(unlist(strsplit(x,"eta_")))[2]
  x = as.matrix(as.character(temp1))
  fnamesmeta = apply(x,MARGIN=c(1),FUN=cave2)
  x = as.matrix(as.character(fname_ms))
  fnamesmetasleep = apply(x,MARGIN=c(1),FUN=cave)
  # create list of day names
  wdaynames = c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday")
  # load summary spreadsheets for this study
  if (file.exists(paste(results,"/part2_daysummary.csv",sep=""))) {
  } else {
    stop("Warning: File daysummary.csv not generated yet")
  }
  daysummary = read.csv(paste(results,"/part2_daysummary.csv",sep=""))
  summary = read.csv(paste(results,"/part2_summary.csv",sep=""))
  checkfiles = dir(results)
  M = c()
  # loop through files
  for (i in f0:f1) {  #1:length(fnamesmeta)
    if (length(ffdone) > 0) {
      if (length(which(ffdone == paste("Report_2_",fnamesmeta[i],".pdf",sep=""))) > 0) {
        skip = 1 #skip this file because it was analysed before")
      } else {
        skip = 0 #do not skip this file
      }
    } else {
      skip = 0
    }
    if (overwrite == TRUE) skip = 0
    if (skip ==0) {
      sel = which(fnamesmetasleep == fnamesmeta[i])
      if (length(sel) > 0) {
        pdf(paste(metadatadir,"/results/file summary reports/Report_2_",fnamesmeta[i],".pdf",sep=""),
            paper="a4",width = 0, height = 0) #width=8.27,height=11.69
        print(paste("File ",fnamesmeta[i],sep=""))
        sib.cla.sum = c()
        load(paste(metasleep,"/",fname_ms[sel],sep=""))
        load(paste(meta,"/",fname_m[i],sep=""))
        ws3 = M$windowsizes[1]
        ws2 = M$windowsizes[2]
        daysummary_tmp = daysummary[which(daysummary$filename == fnamesmeta[i]),]
        nightsummary = c()
        load(paste(ms4,"/",dir(ms4)[which(dir(ms4) == dir(metasleep)[i])],sep="")) #to load summary sleep
        summarysleep_tmp = nightsummary
        # note that the reports are generated from the raw sleep classification (part4) and no attempt is made to clean it up,
        # by deleting nights for which no diary was available or not enough accelerometer data was available
        if (length(unique(summarysleep_tmp$acc_def)) > 1) {
          if (length(which(unique(summarysleep_tmp$acc_def) == "T5A5")) == 1) {
            della = which(summarysleep_tmp$acc_def == "T5A5")
            
          } else {
            della = which(summarysleep_tmp$acc_def == unique(summarysleep_tmp$acc_def)[1])
          }
          if (length(della) > 0) summarysleep_tmp = summarysleep_tmp[-della,]
        }
        # do not include days with no meaningful data
        d2excludeb = d2exclude = which(daysummary_tmp$N.valid.hours < 10)
        n2excludeb = n2exclude = which(summarysleep_tmp$fraction_night_invalid > 0.66
                                       | summarysleep_tmp$acc_SptDuration == 0)
        if (length(d2exclude) > 0) {
          d2excludeb = daysummary_tmp$measurmentday[d2exclude]
          daysummary_tmp = daysummary_tmp[-d2exclude,] #ignore days with non-wear
          d2exclude = d2excludeb
        }
        if (length(n2exclude) > 0) {
          n2excludeb = summarysleep_tmp$night[n2exclude]
          summarysleep_tmp = summarysleep_tmp[-n2exclude,]
          n2exclude = n2excludeb
        }
        # detect which column is mvpa
        n45 = names(daysummary_tmp)
        c45 = c()
        for (i45 in 1:length(n45)) {
          s45 = unlist(strsplit(n45[i45],"VPA_"))
          if (length(s45) > 1) {
            c45 = c(c45,i45)
          }
        }
        
        c45 = c45[length(c45)]
        #######################
        # First page of the report
        MainMetric_1 = paste0("mean_",metric,"_mg_24hr")
        MainMetric_2 = paste0("mean_",metric,"_mg_0-24hr")
        MainMetric_3 = paste0("mean_",metric,"_mg_0.24hr")
        if (length(which(colnames(daysummary_tmp) == MainMetric_1)) == 1) {
          MainMetric = MainMetric_1
        } else if (length(which(colnames(daysummary_tmp) == MainMetric_2)) == 1) {
          MainMetric = MainMetric_2
        } else if (length(which(colnames(daysummary_tmp) == MainMetric_3)) == 1) {
          MainMetric = MainMetric_3
        }
        if (dofirstpage == TRUE & length(which(is.na(daysummary_tmp[,MainMetric]) == FALSE)) > 1
            & length(which(is.na(daysummary_tmp[,c45]) == FALSE)) > 1
            & nrow(summarysleep_tmp) > 0) {
          # abbreviate names of days
          days1 = daysummary_tmp$weekday
          daynames = as.character(days1)
          days_PA = g.abr.day.names(daynames)
          days2 = summarysleep_tmp$weekday
          daynames = as.character(days2)
          days_SLEEP = g.abr.day.names(daynames)
          # extract variables
          lengthnight = summarysleep_tmp$sleeplog_SptDuration #including wake periods
          nocsleepdur = summarysleep_tmp$acc_SleepDurationInSpt
          sleepefficiency = (nocsleepdur /lengthnight) * 100

          f01 = daysummary_tmp[,c45]
          f02 = daysummary_tmp[,MainMetric]
          f05 = nocsleepdur #runif(length(days), 4, 10)
          f06 = sleepefficiency #runif(length(days), 30, 100)
          #           if (length(which(f06 > 100)) > 0) f06[which(f06 > 100)] =0
          f07 = daysummary_tmp$N.valid.hours #runif(7, 20, 24)
          # allocate colours
          CLS = c("white","black")
          CLS_A = rep(CLS[1],length(days_PA))
          CLS_B = rep(CLS[1],length(days_SLEEP))
          CLS_A[which(days_PA == "SUN" | days_PA == "SAT")] = CLS[2]
          CLS_B[which(days_SLEEP == "SUN" | days_SLEEP == "SAT")] = CLS[2]
          # headers
          vars = c(paste("Time spent in moderate or vigorous activity (average is ",round(mean(f01,na.rm=TRUE))," minutes per day)",sep=""),
                   paste("Total physical activity (average per day is ",round(mean(f02,na.rm=TRUE))," mg)",sep=""),
                   paste("Sleep duration (average is ",round(mean(f05,na.rm=TRUE),digits=1)," hours per night)",sep=""),
                   paste("Sleep efficiency (average is ",round(mean(f06,na.rm=TRUE)),"% per night)",sep=""),
                   paste("Duration monitor worn (hours per day)",sep="")) #(mean = ",round(mean(f07))," hours)
          # plot data
          CEXN = 0.9
          par(mfrow=c(5,1),omi=c(0,0,0.2,0),mar=c(3,2,2,2)+0.1)
          #MVPA
          YXLIM = c(0,(max(f01,na.rm=TRUE)*1.3))
          if (YXLIM[2] == 0) YXLIM[2] = 60
          B3 = barplot(as.matrix(f01),names.arg=days_PA,beside=TRUE,#axes=FALSE,
                       ylim=YXLIM,cex.names=CEXN,las=0,col=CLS_A,density = 20) #
          abline(h=30,lty=2,lwd=2)

          topp = mean(as.matrix(round(f01)))*0.1
          text(y= as.matrix(round(f01))+topp+5, x= B3, labels=as.character(as.matrix(round(f01))), xpd=TRUE,cex=1)
          text(x=1,y=(max(YXLIM)*0.95),labels=vars[1],pos=4,font=2,cex=1.2)
          #Acceleration
          YXLIM = c(0,(max(f02,na.rm=TRUE)*1.3))
          B6 = barplot(as.matrix(f02),names.arg=days_PA,beside=TRUE,#axes=FALSE,
                       ylim=YXLIM,cex.names=CEXN,las=0,col=CLS_A,density = 20) #
          abline(h=25,lty=2,lwd=2)
          topp = mean(as.matrix(round(f02)))*0.1
          text(y= as.matrix(round(f02))+topp, x= B6, labels=as.character(as.matrix(round(f02))), xpd=TRUE,cex=1)
          text(x=1,y=(max(YXLIM)*0.95),labels=vars[2],pos=4,font=2,cex=1.2)
          #Monitor wear duration
          YXLIM = c(0,(max(f07,na.rm=TRUE)*1.3))
          B8 =  barplot(as.matrix(f07),names.arg=days_PA,beside=TRUE, #axes=FALSE,
                        ylim=YXLIM,cex.names=CEXN,las=0,col=CLS_A,density = 20) #,density = 20
          topp = mean(as.matrix(round(f07)))*0.1
          text(y= as.matrix(round(f07))+topp, x= B8, labels=as.character(as.matrix(round(f07))), xpd=TRUE,cex=1.1)
          abline(h=16,lty=2,lwd=2)
          text(x=1,y=(max(YXLIM)*0.95),labels=vars[5],pos=4,font=2,cex=1.2)
          #Sleep duration
          YXLIM =c(0,(max(f05,na.rm=TRUE)*1.3))
          B4 = barplot(as.matrix(f05),names.arg=days_SLEEP,beside=TRUE,#axes=FALSE,
                       ylim=YXLIM,cex.names=CEXN,las=0,col=CLS_B,density = 20) #
          abline(h=6,lty=2,lwd=2)
          topp = mean(as.matrix(round(f05,digits=1)))*0.1
          text(y= as.matrix(round(f05,digits=1))+topp, x= B4, labels=as.character(as.matrix(round(f05,digits=1))), xpd=TRUE,cex=1)
          text(x=1,y=(max(YXLIM)*0.95),labels=vars[3],pos=4,font=2,cex=1.2)
          #Sleep efficiency
          YXLIM = c(0,120)
          B5 = barplot(as.matrix(f06),names.arg=days_SLEEP,beside=TRUE,#axes=FALSE,
                       ylim=YXLIM,cex.names=CEXN,las=0,col=CLS_B,density = 20) #,density = 20
          abline(h=60,lty=2,lwd=2)
          abline(h=100,lty=3,lwd=1)
          topp = mean(as.matrix(round(f06)))*0.1
          text(y= as.matrix(round(f06))+topp, x= B5, labels=as.character(as.matrix(round(f06))), xpd=TRUE,cex=1)
          text(x=1,y=(max(YXLIM)-10),labels=vars[4],pos=4,font=2,cex=1.2)
          #-----------------------------------------------------------------------------------
          mtext(paste("Activity and sleep report 2: ",fnamesmeta[i],sep=""), side = 3, line = 0, outer = TRUE,font=2,cex=0.7)
        }
        LWDX = 2.5 #linewidth for coloured lines
        LWDA = 0.2
        BLX = 0.6
        #=================================================
        # Next pages with day specific graphs
        #get variables - activity:
        ACC = as.numeric(as.matrix(M$metashort[,metric])) * 1000
        nonwearscore = as.numeric(as.matrix(M$metalong[,"nonwearscore"]))
        
        time =  as.character(M$metashort[,1])
        nw_time = as.character(M$metalong[,1])
        if (length(unlist(strsplit(time[1],"T"))) > 1) { # ISO timestamp format
          time = as.character(iso8601chartime2POSIX(time,desiredtz))
          nw_time = as.character(iso8601chartime2POSIX(nw_time,desiredtz))
        }
        
        sec = unclass(as.POSIXlt(time))$sec
        min = unclass(as.POSIXlt(time))$min
        hour = unclass(as.POSIXlt(time))$hour
        
        # Prepare nonwear information for plotting
        NONWEAR = rep(NA,length(ACC))
        day = unclass(as.POSIXlt(time))$mday
        month = unclass(as.POSIXlt(time))$mon + 1
        year = unclass(as.POSIXlt(time))$year + 1900
        # take instances where nonwear was detected (on ws2 time vector) and map results onto a ws3 length vector for plotting purposes
        if (sum(which(nonwearscore > 1))) {
          nonwear_elements = which(nonwearscore > 1)
          for (j in 1:length(nonwear_elements)) {
            # could add try/catch in here in case 'which' fails..
            match_loc = which(nw_time[nonwear_elements[j]]==time)
            NONWEAR[match_loc:(match_loc+(ws2/ws3)-1)] <- 1
          }
        }

        INACT = rep(NA,length(ACC))
        LIGPA = rep(NA,length(ACC))
        MODPA = rep(NA,length(ACC))
        VIGPA = rep(NA,length(ACC))
        boutdur2 = 10 * (60/ws3)
        boutcriter = 0.8
        #moderate and vigorous activity
        rr1 = matrix(0,length(ACC),1)
        p = which(ACC >= 100)
        rr1[p] = 1
        rr1t = rr1
        jmvpa = 1

        INACT[which(ACC < threshold.lig)] = 1
        LIGPA[which(ACC >= threshold.lig & ACC < threshold.mod)] = 1
        MODPA[which(ACC >= threshold.mod & ACC < threshold.vig)] = 1
        VIGPA[which(ACC >= threshold.vig)] = 1

        #get variables - sleep:
        angle = as.matrix(M$metashort[,which(colnames(M$metashort) == "anglez")])
        if (I$monc == 2) {
          LP = as.character(M$metalong[,which(names(M$metalong)=="lightpeak")])
          LP = rep(LP,each=c(ws2/ws3))
        }
        detection = rep(NA,length(time))
        S = sib.cla.sum #SLES$output
        S$sib.onset.time = iso8601chartime2POSIX(S$sib.onset.time, tz = desiredtz)
        S$sib.end.time = iso8601chartime2POSIX(S$sib.end.time, tz = desiredtz)
        def = unique(S$definition)[1]
        S = S[which(S$definition==def),] # simplify to one definition
        for (j in 1:length(unique(S$night))) { #nights
          tmp = S[which(S$night==j),]
          for (h in 1:nrow(tmp)) { # sleep periods
            s0 = which(time == as.character(tmp$sib.onset.time[h]))[1]
            s1 = which(time == as.character(tmp$sib.end.time[h]))[1]
            if (length(s0) == 0 | is.na(s0) == TRUE) {
              s0 = which(time == paste(as.character(tmp$sib.onset.time[h])," 00:00:00",sep=""))[1]
            }
            if (length(s1) == 0 | is.na(s1) == TRUE) {
              s1 = which(time == paste(as.character(tmp$sib.end.time[h])," 00:00:00",sep=""))[1]
            }
            if (is.na(s0) == FALSE & is.na(s1) == FALSE) { #new on 18 May 2015
              detection[s0:s1] = 1 #new on 18 May 2015
            } #new on 18 May 2015
          }
        }
        night_wake_full <- rep(NA,length(detection)) # opposite of detection (sleep/inactivity detection)
        night_wake_full[is.na(detection)] <- 1 # for plotting wake during SPT window
        
        # prepare to search for sleeplog_onst / sleeplog_wake
        sleep_dates = as.Date(summarysleep_tmp$calendardate,format='%d/%m/%Y')
        
        # detect midnights
        if (viewingwindow == 1) {
          nightsi = which(sec == 0 & min == 0 & hour == 0)
          xaxislabels = c("midnight","2am","4am","6am","8am","10am","noon",
                          "2pm","4pm","6pm","8pm","10pm","midnight")
        } else if (viewingwindow == 2) {
          nightsi = which(sec == 0 & min == 0 & hour == 12)
          xaxislabels = c("noon","2pm","4pm","6pm","8pm","10pm","midnight",
                          "2am","4am","6am","8am","10am","noon")
        }
        if (length(nightsi) > 0) { # Do not attempt to create a plot when there is no midnight in the data, because calculation of t1 will be complicated.
          nplots = length(nightsi)+1
          # plot
          npointsperday = (60/ws3)*1440
          x = 1:npointsperday
          NGPP = 7 #number of graphs per page
          par(mfcol=c(NGPP,1),mar=c(2,0.5,1,0.5)+0.1,omi=c(0,0,0.1,0),mgp=c(2,0.8,0))
          daycount = 1
          for (g in 1:nplots) { # 1:nplots
            skip = FALSE
            if (g == 1) {
              t0 = 1
              t1 = nightsi[g]-1
              if ((t1 - t0) < (6*(60/ws3)*60)) skip = TRUE
            } else if (g > 1 & g < nplots) {
              t0 = nightsi[g-1]
              t1 = nightsi[g]-1
            }  else if (g == nplots) {
              t0 = nightsi[g-1]
              t1 = length(time)
              if ((t1 - t0) < (6*(60/ws3)*60)) skip = TRUE
            }
            if (((t1-t0) + 1) / (60*60/ws3) == 25) { # day with 25 hours, just pretend that 25th hour did not happen
              t1 = t1 - (60*60/ws3)
            }
            if (((t1-t0) + 1) / (60*60/ws3) == 23) { # day with 23 hours, just extend timeline with 1 hour
              t1 = t1 + (60*60/ws3)
            }
            
            # Initialize daily 'what we think you did' vectors:
            acc = abs(ACC[t0:t1])
            ang = angle[t0:t1]
            night_sleep <- detection[t0:t1]
            night_wake <- night_wake_full[t0:t1]
            inactive <- INACT[t0:t1]
            light_pa <- LIGPA[t0:t1]
            mod_pa <- MODPA[t0:t1]
            vig_pa <- VIGPA[t0:t1]
            non_wear <- NONWEAR[t0:t1]
            
            # check to see if there is a sleeplog_onset on this day:
            sleeponset_loc = 0
            curr_date = as.Date(substr(time[t0],start=1,stop=10),format = '%Y-%m-%d')  # what day is it?
            check_date = match(curr_date,sleep_dates)
            if (is.na(check_date) == FALSE) {
              sleeponset_time = summarysleep_tmp$sleeplog_onset_ts[check_date]  # get the time of sleep_onset
              # find the first index that matches sleeponset_hour and _min
              sleeponset_hour = as.integer(substr(sleeponset_time,start=1,stop=2))
              sleeponset_min = as.integer(substr(sleeponset_time,start=4,stop=5))
              sleeponset_loc = which(hour[t0:t1] == sleeponset_hour & min[t0:t1] == sleeponset_min)
              sleeponset_loc = sleeponset_loc[1]
              if (is.na(sleeponset_loc)) sleeponset_loc = 0 
            }
            # check to see if there is a wake on this day (check prev day):
            prev_date = curr_date - 1
            wake_loc = 0
            check_date = match(prev_date,sleep_dates)
            if (is.na(check_date) == FALSE) {
              wake_time = summarysleep_tmp$sleeplog_wake_ts[check_date] # get wake time
              # find the first index that matches wake time hour and min
              wake_hour = as.integer(substr(wake_time,start=1,stop=2))
              wake_min = as.integer(substr(wake_time,start=4,stop=5))
              wake_loc = which(hour[t0:t1] == wake_hour & min[t0:t1] == wake_min)
              wake_loc = wake_loc[1]
              if (is.na(wake_loc)) wake_loc = 0 
            }
            
            # add extensions if <24hr of data
            if (((t1-t0)+1) != npointsperday & t0 == 1) {
              extension = rep(NA,(npointsperday-((t1-t0)+1)))
              acc = c(extension,acc)
              ang = c(extension,ang)
              if (length(acc) == (length(x)+1)) {
                extension = extension[2:(length(extension))]
                acc = acc[2:(length(acc))]
                ang = ang[2:(length(ang))]
              }
              inactive = c(extension,inactive)
              light_pa = c(extension,light_pa)
              mod_pa = c(extension,mod_pa)
              vig_pa = c(extension,vig_pa)
              night_sleep = c(extension,night_sleep)
              night_wake = c(extension,night_wake)
              non_wear = c(extension,non_wear)
              if (sleeponset_loc != 0) sleeponset_loc = length(extension) + sleeponset_loc
              if (wake_loc != 0) wake_loc = length(extension) + wake_loc
            }
            if (((t1-t0)+1) != npointsperday & t1 == length(time)) {
              extension = rep(NA,(npointsperday-((t1-t0)+1)))
              acc = c(acc,extension)
              ang = c(ang,extension)
              if (length(acc) == (length(x)+1)) {
                extension = extension[1:(length(extension)-1)]
                acc = acc[1:(length(acc)-1)]
                ang = ang[1:(length(ang)-1)]
              }
              inactive = c(inactive,extension)
              light_pa = c(light_pa,extension)
              mod_pa = c(mod_pa,extension)
              vig_pa = c(vig_pa,extension)
              night_sleep = c(night_sleep,extension)
              night_wake = c(night_wake,extension)
              non_wear = c(non_wear,extension)
              if (I$monc == 2) LPd = c(LPd,extension)
            }
            acc = as.numeric(acc)
            acc[which(acc >= 900)] = 900
            acc = (acc/9) - 210
            
            if (sleeponset_loc == 0 & wake_loc == 0){
              #
            } else if (sleeponset_loc>0 & wake_loc==0) {
              # awake then asleep
              night_sleep[1:sleeponset_loc] <- NA
              night_wake[1:sleeponset_loc] <- NA
              inactive[sleeponset_loc:length(inactive)] <- NA
              light_pa[sleeponset_loc:length(inactive)] <- NA
              mod_pa[sleeponset_loc:length(inactive)] <- NA
              vig_pa[sleeponset_loc:length(inactive)] <- NA
            } else if (sleeponset_loc == 0 & wake_loc > 0) {
              # asleep then awake
              night_sleep[wake_loc:length(night_sleep)] <- NA
              night_wake[wake_loc:length(night_wake)] <- NA
              inactive[1:wake_loc] <- NA
              light_pa[1:wake_loc] <- NA
              mod_pa[1:wake_loc] <- NA
              vig_pa[1:wake_loc] <- NA
            } else if (sleeponset_loc > 0 & wake_loc > 0) {
              if (wake_loc < sleeponset_loc) {
                # wake is before sleep onset
                night_sleep[wake_loc:sleeponset_loc] <- NA
                night_wake[wake_loc:sleeponset_loc] <- NA
                inactive[1:wake_loc] <- NA
                inactive[sleeponset_loc:length(inactive)] <- NA
                light_pa[1:wake_loc] <- NA
                light_pa[sleeponset_loc:length(inactive)] <- NA
                mod_pa[1:wake_loc] <- NA
                mod_pa[sleeponset_loc:length(inactive)] <- NA
                vig_pa <- VIGPA[t0:t1]
                vig_pa[1:wake_loc] <- NA
                vig_pa[sleeponset_loc:length(inactive)] <- NA
              } else {
                # sleeponset is before wake
                night_sleep[1:sleeponset_loc] <- NA
                night_sleep[wake_loc:length(night_sleep)] <- NA
                night_wake[1:sleeponset_loc] <- NA
                night_wake[wake_loc:length(night_sleep)] <- NA
                inactive[sleeponset_loc:wake_loc] <- NA
                light_pa[sleeponset_loc:wake_loc] <- NA
                mod_pa[sleeponset_loc:wake_loc] <- NA
                vig_pa[sleeponset_loc:wake_loc] <- NA
              }
              
            } else {
              #
            }
            
            minutesvigorous = (length(which(vig_pa == 1))*5)/60
            minutesmoderate = (length(which(mod_pa == 1))*5)/60
            minutesMVPA = (length(which(mod_pa == 1 | vig_pa == 1))*5)/60
            if (viewingwindow == 1) { #focus on day
              if (length(d2exclude) > 0) {
                if (length(which(d2exclude == g)) > 0) skip = TRUE
              }
            } else { #focus on night
              if (length(n2exclude) > 0) {
                if (length(which(n2exclude == g)) > 0) skip = TRUE
              }
            }
            title = paste("Day ",daycount,": ",
                          wdaynames[unclass(as.POSIXlt(time[t0]))$wday+1],
                          " ",
                          unclass(as.POSIXlt(time[t0]))$mday,"/",
                          unclass(as.POSIXlt(time[t0]))$mon+1,"/",
                          unclass(as.POSIXlt(time[t0]))$year+1900,sep="")

            inactive = inactive * 143
            light_pa = light_pa * 143
            mod_pa = mod_pa * 143
            vig_pa = vig_pa * 143
            night_sleep = night_sleep * 143
            night_wake = night_wake * 143
            non_wear = non_wear * 143
            
            # if (I$monc == 2) {
            #   LPd_dark = as.numeric(LPd)
            #   LPd_light = as.numeric(LPd)
            #   is.na(LPd_light[which(LPd_light < 4)]) = TRUE
            #   is.na(LPd_dark[which(LPd_dark >= 4)]) = TRUE
            #   LPd_dark[which(is.na(LPd_dark) == FALSE)] = -220
            #   LPd_light[which(is.na(LPd_light) == FALSE)] = -220
            # }
            # 
            if (skip == FALSE) {
              YXLIM = c(-230,300)
              LJ = 2
              # accelerometer
              plot(x,acc, type="l",lwd=LWDA,bty="l",axes=FALSE,ylim=YXLIM,
                   xlab="",ylab="",main="",cex.main=0.9,lend=LJ) #,axes=FALSE,ylim=YXLIM,xlab="",ylab="",main="",cex=0.3
              # angle
              lines(x,ang, type="l",lwd=LWDA,bty="l",xlab="",ylab="",cex=0.3,lend=LJ)
              # add sleeponset annotation to plot:
              if (sleeponset_loc != 0){
                if (sleeponset_loc <= 10) { # check to see if wake_loc is at the start or end of the plot and adjust accordingly
                  start_idx <- 1
                  end_idx <- 20
                  ar_idx <- 320
                } else if (sleeponset_loc > (length(x)-10)) {
                  start_idx <- length(x) - 20
                  end_idx <- length(x)
                  ar_idx <- start_idx - 300
                } else {
                  start_idx <- sleeponset_loc-10
                  end_idx <- sleeponset_loc+10
                  ar_idx <- sleeponset_loc + 300
                }
                set_pos = 4
                ar_start_idx <- end_idx
                ar_end_idx <- end_idx + 300
                if (sleeponset_loc > (0.8 * length(x))) {  # check to see if text should be placed on the left side of the line
                  set_pos = 2
                  ar_start_idx <- start_idx
                  ar_end_idx <- start_idx - 300
                }
                rect(start_idx,-270,end_idx,100,border="black",lwd=0.3)#,col="blue")
                arrows(ar_start_idx,-150,ar_end_idx,-150,length=0.05,angle = 20,code=1,lwd=0.5)
                segments(ar_start_idx,-150,ar_end_idx,-150,col="black",lwd=0.5)
                text(ar_end_idx,-160,labels="Sleep-onset",pos=set_pos,font=1.8,cex=0.8,col="darkgrey")
              }
              # add wake annotation to plot:
              if (wake_loc != 0) {
                if (wake_loc <= 10) { # check to see if wake_loc is at the start or end of the plot and adjust accordingly
                  start_idx <- 1
                  end_idx <- 20
                } else if (wake_loc > (length(x)-10)) {
                  start_idx <- length(x) - 20
                  end_idx <- length(x)
                } else {
                  start_idx <- wake_loc-10
                  end_idx <- wake_loc+10
                }
                set_pos <- 4
                ar_start_idx <- end_idx
                ar_end_idx <- end_idx + 300
                if (wake_loc > (0.8 * length(x))) {  # check to see if text should be placed on the left side of the line
                  set_pos = 2
                  ar_start_idx <- start_idx
                  ar_end_idx <- start_idx - 300
                }
                rect(start_idx,-270,end_idx,100,border="black",lwd=0.3)#,col="green")
                arrows(ar_start_idx,-150,ar_end_idx,-150,length=0.05,angle = 20,code=1,lwd=0.5)
                segments(ar_start_idx,-150,ar_end_idx,-150,col="black",lwd=0.5)
                text(ar_end_idx,-160,labels="Wake",pos=set_pos,font=1.8,cex=0.8,col="darkgrey")
              }
              lines(x,night_sleep,type="l",lwd=LWDX,col="black",lend=LJ)
              lines(x,night_wake,type="l",lwd=LWDX,col="grey",lend=LJ)
              lines(x,inactive,type="l",lwd=LWDX,col="lightblue",lend=LJ)
              lines(x,light_pa,type="l",lwd=LWDX,col="yellow",lend=LJ)
              lines(x,mod_pa,type="l",lwd=LWDX,col="orange",lend=LJ)
              lines(x,vig_pa,type="l",lwd=LWDX,col="red",lend=LJ)
              lines(x,non_wear,type="l",lwd=5.5,col="greenyellow",lend=LJ)
              axis(side=1,at=seq(1,(((60/ws3)*60*24)+1),by=(2*(60/ws3)*60)),labels=xaxislabels,cex.axis=0.7)
              abline(h=0,untf = FALSE,lty=3,lwd=1,col="grey")
              rect(xleft=-10,xright=(25*60*(60/ws3)),ybottom=160,
                   ytop=220,col="white",border=NA)
              text(x=-700,y=285,labels=title,pos=4,font=2,cex=1)
              text(x=-700,y=180,labels="Annotation:",pos=4,font=1.8,cex=0.9)
              text(x=-700,y=-120,labels="Arm movement:",pos=4,font=1.8,cex=0.9)
              text(x=-700,y=80,labels="Angle of sensor's z-axis relative to horizontal plane:",pos=4,font=1.8,cex=0.9)
              box("figure",col="black")
              legend("topright",legend=c( "SPT Window: Sleep", "SPT Window: Wake", "Inactivity", #"arm angle (top) activity (bottom)",
                                            "Light PA","Moderate PA","Vigorous PA","Non-Wear"),
                       lty=c(1,1),col=c("black","grey","lightblue","yellow","orange","red","greenyellow"),
                       lwd=c(LWDX,LWDX,LWDX),bg="white",cex=0.6,ncol=7,box.lwd=BLX)
              if (daycount==1 | ((daycount-1)/NGPP) == (round((daycount-1)/NGPP))) {
                mtext(paste("Filename: ",fnamesmeta[i],sep=""),side = 3,line=0,outer=TRUE,font=2,cex=0.6)
              }
            }
            daycount = daycount + 1
          }
          dev.off()
        }
      }
    }
  }
}
