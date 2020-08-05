g.analyse.avday = function(qlevels, doquan, averageday, M, IMP, t_TWDI, quantiletype,
                           winhr, L5M5window, M5L5res, ws3, IVIS_epochsize_seconds, 
                           IVIS_windowsize_minutes, IVIS.activity.metric, doiglevels,
                           firstmidnighti, ws2, midnightsi, iglevels, qM5L5, MX.ig.min.dur = 10) {
  if (doquan == TRUE) {
    QLN = rep(" ",length(qlevels))
    for (QLNi in 1:length(qlevels)) {
      QLN[QLNi] = paste("p",(round((qlevels[QLNi]) * 10000))/100,sep="")
    }
  }
  QUAN = qlevels_names = c()
  ML5AD = ML5AD_names = c()
  one2sixname = ""
  if (is.data.frame(t_TWDI) == TRUE) { # If an activity log was used then use for average day only 24 hour window.
    t_TWDI= c(0,24)
  }
  if (doquan == TRUE) {
    for (quani in 1:ncol(averageday)) { # these columns of averageday correspond to the metrics from part 1
      if (colnames(M$metashort)[(quani+1)] %in% c("anglex", "angley", "anglez") == FALSE) {
        #--------------------------------------
        # quantiles
        QUANtmp =  quantile(averageday[((t_TWDI[1]*(3600/ws3))+1):(t_TWDI[length(t_TWDI)]*(3600/ws3)),quani],probs=qlevels,na.rm=T,type=quantiletype)
        QUAN = c(QUAN,QUANtmp)
        qlevels_namestmp = rep(" ",length(qlevels))
        for (QLNi in 1:length(qlevels)) {
          qlevels_namestmp[QLNi] = paste(QLN[QLNi],"_",colnames(M$metashort)[(quani+1)],"_mg_",
                                         t_TWDI[1],"-",t_TWDI[length(t_TWDI)],"h",sep="")
        }
        qlevels_names = c(qlevels_names,qlevels_namestmp)
        rm(QUANtmp)
        #--------------------------------------
        #M5L5 - (Note: averageday is relative to starttime of measurement, but so is firstmidnighti (index of midnights), so M5L5 are correct)
        #loop through winhr
        for (winhr_value in winhr) {
          # Time window for L5 & M5 analysis
          t0_LFMF = L5M5window[1] #start in 24 hour clock hours
          t1_LFMF = L5M5window[2]+(winhr_value-(M5L5res/60)) #end in 24 hour clock hours (if a value higher than 24 is chosen, it will take early hours of previous day to complete the 5 hour window
          avday = averageday[,quani]
          avday = c(avday[(firstmidnighti*(ws2/ws3)):length(avday)],avday[1:((firstmidnighti*(ws2/ws3))-1)])
          # Note that t_TWDI[length(t_TWDI)] in the next line makes that we only calculate ML5 over the full day
          ML5ADtmp = g.getM5L5(avday,ws3,t0_LFMF=t_TWDI[1],t1_LFMF=t_TWDI[length(t_TWDI)],
                               M5L5res,winhr_value, qM5L5=qM5L5, MX.ig.min.dur=MX.ig.min.dur)
          ML5AD = as.data.frame(c(ML5AD,ML5ADtmp), stringsAsFactors = TRUE)
        }
        ML5N = names(ML5AD)
        for (ML5ADi in 1:length(ML5N)) {
          if (length(grep(pattern = "1to6", ML5N[ML5ADi])) == 0) {
            ML5AD_names[ML5ADi] = paste(ML5N[ML5ADi],"_",colnames(M$metashort)[(quani+1)],"_mg_",
                                        L5M5window[1],"-",L5M5window[2],"h",sep="")
          }
        }
        # Acceleration 1-6am
        one2sixname = paste0("1to6am_",colnames(M$metashort)[(quani+1)],"_mg")
        ML5AD_names = c(ML5AD_names, one2sixname)
        ML5AD$one2six = mean(avday[((1*60*(60/ws3))+1):(6*60*(60/ws3))]) * 1000
        colnames(ML5AD)[which(colnames(ML5AD) == "one2six")] = one2sixname
      }	
    }	
  }
  igfullr_names = igfullr = c()
  if (doiglevels == TRUE) { 
    # intensity gradient (as described by Alex Rowlands 2018)
    # applied to the averageday per metric (except from angle metrics)
    igfullr = igfullr_names = c()
    for (igi in 1:ncol(averageday)) {
      if (colnames(M$metashort)[(igi+1)] %in% c("anglex", "angley", "anglez") == FALSE) {
        y_ig = c()
        avday = averageday[,igi] 
        avday = c(avday[(firstmidnighti*(ws2/ws3)):length(avday)],avday[1:((firstmidnighti*(ws2/ws3))-1)])
        # we are not taking the segment of the day now (too much output)
        q60 = cut((avday*1000),breaks = iglevels,right=FALSE)
        y_ig  = (as.numeric(table(q60)) * ws3)/60 #converting to minutes
        x_ig = zoo::rollmean(iglevels,k=2)
        igout = g.intensitygradient(x_ig, y_ig)
        if (length(avday) > 0) {
          igfullr = c(igfullr,as.vector(unlist(igout)))
        } else {
          igfullr = c(igfullr,rep("",3))
        }
        igfullr_names = c(igfullr_names,paste0(c("ig_gradient","ig_intercept","ig_rsquared"),
                                               paste0("_",colnames(IMP$metashort)[igi+1], "_0-24hr")))
      }
    }
  }
  # IS and IV variables
  fmn = midnightsi[1] * (ws2/ws3) # select data from first midnight to last midnight
  lmn = midnightsi[length(midnightsi)] * (ws2/ws3)
  # By using the metahosrt from the IMP we do not need to ignore segments, because data imputed
  Xi = IMP$metashort[fmn:lmn,which(colnames(IMP$metashort) %in% c("anglex","angley","anglez","timestamp") == FALSE)[1]]
  IVISout = g.IVIS(Xi, epochsizesecondsXi = ws3, IVIS_epochsize_seconds = IVIS_epochsize_seconds, 
                   IVIS_windowsize_minutes = IVIS_windowsize_minutes, IVIS.activity.metric=IVIS.activity.metric)
  InterdailyStability = IVISout$InterdailyStability
  IntradailyVariability = IVISout$IntradailyVariability
  invisible(list(InterdailyStability=InterdailyStability,
                 IntradailyVariability=IntradailyVariability, 
                 igfullr_names=igfullr_names,
                 igfullr=igfullr, QUAN=QUAN,
                 qlevels_names=qlevels_names,
                 ML5AD=ML5AD,
                 ML5AD_names=ML5AD_names))
}