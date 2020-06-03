g.analyse.perday = function(selectdaysfile, ndays, firstmidnighti, time, nfeatures, 
                            window.summary.size, qwindow, midnightsi, metashort, averageday,
                            ENMOi, LFENMOi, BFENi, ENi, HFENi, HFENplusi, MADi, ENMOai,
                            doiglevels, nfulldays,lastmidnight, ws3, ws2, qcheck,
                            fname, idloc, BodyLocation, wdayname, tooshort, includedaycrit,
                            winhr, L5M5window, M5L5res,
                            doquan, qlevels, quantiletype, doilevels, ilevels, iglevels, domvpa,
                            mvpathreshold, boutcriter, closedbout,
                            bout.metric, mvpadur, mvpanames, wdaycode, IDd, ID, ID2,
                            deviceSerialNumber, qM5L5, ExtFunColsi, myfun) {
  if (length(selectdaysfile) > 0 & ndays == 2) {
    ndays = 1
    startatmidnight = endatmidnight  = 1
  } else {
    startatmidnight = endatmidnight = 0
    if (nfulldays >= 1) {
      if (firstmidnighti == 1) {  #if measurement starts at midnight
        ndays = ndays - 1
        startatmidnight =  1
        cat("measurement starts at midnight or there is no midnight")
      }
      if (lastmidnight == time[length(time)] & nrow(metashort) < ((60/ws3) * 1440)) {	#if measurement ends at midnight
        ndays = ndays - 1
        endatmidnight = 1
        cat("measurement ends at midnight or there is no midnight")
      }
    }
  }
  daysummary = matrix("",ceiling(ndays),nfeatures)
  ds_names = rep("",nfeatures)
  windowsummary = ws_names = c()
  #=============================
  if (length(selectdaysfile) > 0) {   # Millenium cohort related:
    ndays = ceiling(ndays)
    if (ndays > 2) ndays = 2 # this is now hardcoded to be a maximum of two days
    nwindows = 1440 / window.summary.size # now defining it as X windows per 24
    windowL = 24/nwindows # now defined up hear, because window length should be a constant
    windowsummary = matrix("",(ndays*nwindows),(ncol(metashort)*7)+5)
    ws_names = rep("",ncol(windowsummary))
    # Features per day (based on on single variables)
    if (ndays > 2) ndays = 2
    gikb = 0
  }
  #===============================================
  # Features per day (based on on single variables)
  qwindowbackup = qwindow
  for (di in 1:ndays) { #run through days
    qwindow = qwindowbackup
    #extract day from matrix D and qcheck
    if (length(selectdaysfile) > 0 & startatmidnight == 1 & endatmidnight == 1) { #added on 14/9/2016
      qqq1 = 1#midnightsi[di]*(ws2/ws3) 	#a day starts at 00:00
      qqq2 = midnightsi[di]*(ws2/ws3) #(midnightsi[(di+1)]*(ws2/ws3))-1
    } else if (length(selectdaysfile) == 0 & startatmidnight == 1 & endatmidnight == 1) {
      qqq1 = midnightsi[di]*(ws2/ws3) 	#a day starts at 00:00
      qqq2 = (midnightsi[(di+1)]*(ws2/ws3))-1
    } else if (startatmidnight == 1 & endatmidnight == 0) {
      if (di < floor(ndays)) { #applying floor because when day there is day saving time then ndays is not an integer
        qqq1 = (midnightsi[di]-1)*(ws2/ws3)
        qqq2 = ((midnightsi[(di+1)]-1)*(ws2/ws3))+1
      } else if (di == floor(ndays)) {
        qqq1 = (midnightsi[di]-1)*(ws2/ws3)
        qqq2 = nrow(metashort)
      }
    } else if (startatmidnight == 0 & endatmidnight == 0) {
      if (di == 1) {
        qqq1 = 1
        qqq2 = ((midnightsi[di]-1)*(ws2/ws3))
      } else if (di > 1 & di < floor(ndays)) {
        qqq1 = (midnightsi[(di-1)]-1)*(ws2/ws3)+1 # a day starts at 00:00
        qqq2 = ((midnightsi[di]-1)*(ws2/ws3))
      } else if (di == floor(ndays)) {
        qqq1 = (midnightsi[(di-1)]-1)*(ws2/ws3)+1 # a day starts at 00:00
        qqq2 = nrow(metashort)
      }
    } else if (startatmidnight == 0 & endatmidnight == 1) {
      if (di == 1) {
        qqq1 = 1
        qqq2 = (midnightsi[di]*(ws2/ws3))-1
      } else if (di > 1 & di <= floor(ndays)) {
        qqq1 = midnightsi[(di-1)]*(ws2/ws3) # a day starts at 00:00
        qqq2 = (midnightsi[di]*(ws2/ws3))-1
      }
    }
    if (qqq2 > nrow(metashort)) qqq2 = nrow(metashort)
    vari = as.matrix(metashort[qqq1:qqq2,])
    val = qcheck[qqq1:qqq2]
    nvalidhours_qwindow = rep(0,length(qwindow) - 1)
    nhours_qwindow = rep(0,length(qwindow) - 1)
    # Ignore qwindow values that are not possible for this day
    LENVAL_hours = length(val)/ (60*(60/ws3)) #11.2
    if (length(which(round(LENVAL_hours) %in% 23:25 == TRUE)) == 0) {
      if (di == 1) {
        # Following 8 lines were turned off on June 5 2018 because first and last day are imputed,
        # but turned on again on 14 March 2019 because when the first day is half the relative start
        # of the windows must be different. Yes, the half days are inputed, but we are still only
        # interested in the non-imputed part. This is probably were the confusion came from.
        hours2delta = 24 - LENVAL_hours
        qw_select = which(qwindow > hours2delta)
        if(qw_select[1] > 1) qw_select = c(qw_select[1] - 1,qw_select)
        qwindow = qwindow[qw_select]
        qwindowindices = qwindow - hours2delta # - LENVAL_hours # because 1 is now different
        if (length(which(qwindowindices < 0)) > 0) qwindowindices[which(qwindowindices < 0)] = 0
      } else if (di == ndays) {
        qwindowindices = qwindow
      }
    } else {
      hours2delta = 0
      qwindowindices = qwindow
    }
    deltaLengthQwindow = 0
    if (length(qwindow) < 2) qwindow = c()
    if (length(qwindow) > 0) {
      if (length(qwindowbackup) == 1) {
        cat("Argument to qwindow is invalid, requires a vector of at least length 2")
      }
      if (length(qwindowbackup) == 2) {
        if (qwindow[1] != 0 | qwindow[2] != 24) {
          if((qwindowindices[2]*60*(60/ws3)) <= length(val)) {
            valq = val[((qwindowindices[1]*60*(60/ws3))+1):(qwindowindices[2]*60*(60/ws3))]
          } else {
            valq = val[((qwindowindices[1]*60*(60/ws3))+1):length(val)]
          }
          nvalidhours_qwindow =length(which(valq == 0))/ (3600/ws3)
          nhours_qwindow = length(valq)/ (3600/ws3) #valid hours per day (or half a day)
        }
      } else if (length(qwindowbackup) > 2) {
        deltaLengthQwindow = length(qwindowbackup) - length(qwindowindices)
        for (qwi in 1:(length(qwindowindices)-1)) { #
          startindex = qwindowindices[qwi]*60*(60/ws3)
          endindex = qwindowindices[qwi+1]*60*(60/ws3)
          if(startindex <= length(val) & endindex <= length(val)) {
            valq = val[(startindex+1):endindex]
          } else if (startindex <= length(val) & endindex >= length(val)) {
            valq = val[(startindex+1):length(val)]
          } else if (startindex >= length(val) & endindex >= length(val)) {
            valq = c()
          }
          if (length(valq) > 0) {
            nvalidhours_qwindow[qwi + deltaLengthQwindow] =length(which(valq == 0))/ (3600/ws3)
            nhours_qwindow[qwi + deltaLengthQwindow] = length(valq)/ (3600/ws3) #valid hours per day (or half a day)
          } else {
            nvalidhours_qwindow[qwi + deltaLengthQwindow] = 0
            nhours_qwindow[qwi + deltaLengthQwindow] = 0 #valid hours per day (or half a day)
          }
        }
      }
    }
    val = as.numeric(val)
    nvalidhours = length(which(val == 0))/ (3600/ws3) #valid hours per day (or half a day)
    nhours = length(val) / (3600/ws3) #valid hours per day (or half a day)
    #start collecting information about the day
    fi = 1
    daysummary[di,fi] = unlist(strsplit(fname,"_"))[1] #participant ID
    if (idloc == 2) {
      daysummary[di,fi] = unlist(strsplit(fname,"_"))[1] #id
    } else if (idloc == 4) {
      daysummary[di,fi] = IDd
    } else if (idloc == 1) {
      daysummary[di,fi] = ID
    } else if (idloc == 3) {
      daysummary[di,fi] = ID2
    }
    idremember = daysummary[di,fi]
    ds_names[fi] = "ID";      fi = fi + 1
    daysummary[di,fi] = fname
    ds_names[fi] = "filename";  fi = fi + 1
    calendardate = unlist(strsplit(as.character(vari[1,1])," "))[1]
    daysummary[di,fi] = calendardate
    daysummary[di,(fi+1)] =BodyLocation
    daysummary[di,(fi+2)] = nvalidhours
    daysummary[di,(fi+3)] = nhours
    ds_names[fi:(fi+3)] = c("calendar_date","bodylocation","N valid hours","N hours")
    fi = fi + 4
    if (length(qwindow > 0)) {
      if (length(qwindow) > 2 | qwindow[1] != 0 | qwindow[2] != 24) {
        for (qwi in 1:(length(qwindowbackup)-1)) { # create variables regardless of
          if (length(which(qwindowbackup[qwi] %in% qwindow == TRUE)) > 0) {
            daysummary[di,(fi)] = nvalidhours_qwindow[qwi]
            daysummary[di,(fi+1)] = nhours_qwindow[qwi]
          } else {
            daysummary[di,(fi)] = 0 # note that we only consider qwindows when there is data for the entire window, otherwise duration is 0
            daysummary[di,(fi+1)] = 0
          }
          ds_names[fi:(fi+1)] = c(paste0("N_valid_hours_",qwindowbackup[qwi],"-",qwindowbackup[qwi+1],"hr"),
                                  paste0("N_hours_",qwindowbackup[qwi],"-",qwindowbackup[qwi+1],"hr"))
          fi = fi + 2
        }
      }
    }
    
    #--------------------------------------
    weekdays = c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday")
    weekdays = rep(weekdays,26) # hardcoded maximum number of weeks of 26, not ideal
    if (di == 1) {
      daysummary[di,fi] = wdayname
    } else {
      daysummary[di,fi] = weekdays[wdaycode + (di-1)]
    }
    daysummary[di,(fi+1)] = di #day number relative to start of measurement
    ds_names[fi:(fi+1)] = c("weekday","measurementday")
    fi = fi + 2
    if (length(selectdaysfile) > 0) {
      #---------------------------
      # Description per timewindow for millenium cohort:
      for (metrici in  2:ncol(vari)) {
        Nfeatures = 7
        nhrs = (nrow(vari)/(3600/ws3))
        if (ceiling(nhrs/windowL) < nwindows) nwindows = ceiling(nhrs/windowL)
        for (gik in 1: nwindows) {
          bbb1 = (windowL * (3600/ws3) * (gik-1)) +1
          bbb2 = (windowL * (3600/ws3) * (gik))
          if (bbb2 > length(val)) bbb2 = length(val)
          windowsummary[gikb+gik,1] = idremember #id number
          windowsummary[gikb+gik,2] = deviceSerialNumber #sn number
          windowsummary[gikb+gik,3] = fname #filename
          windowsummary[gikb+gik,4] = as.character(vari[bbb1,1])
          windowsummary[gikb+gik,5] = length(which(val[bbb1:bbb2] == 0))/ (3600/ws3) #hours of valid data
          windowsummary[gikb+gik,((metrici-2)*Nfeatures)+6] = mean(as.numeric(vari[bbb1:bbb2,metrici]))
          windowsummary[gikb+gik,((metrici-2)*Nfeatures)+7] = sd(as.numeric(vari[bbb1:bbb2,metrici]))
          if (nrow(vari) > 10 & (bbb2 - bbb1) > 10) {
            windowsummary[gikb+gik,((metrici-2)*Nfeatures)+8:12] =
              quantile(as.numeric(vari[bbb1:bbb2,metrici]),probs=c(0.05,0.25,0.5,0.75,0.95),na.rm =TRUE)
          } else {
            windowsummary[gikb+gik,(((metrici-2)*Nfeatures)+8):(((metrici-2)*Nfeatures)+12)] = NA
          }
        }
      }
      ws_names = c("ID","serial number","filename","time","Nhoursvalid")
      if (ncol(vari) >= 3) {
        for (columnvari in 2:ncol(vari)) {
          metricname_end = colnames(vari)[columnvari]
          metricname_begin = c("mean metric ","sd metric ","p5 metric ","p25 metric ","p50 metric ",
                               "p75 metric ","p95 metric ")
          ws_names = c(ws_names,paste(metricname_begin,metricname_end,sep=""))
        }
      }
      gikb = gikb + gik
    }
    if (tooshort == 0) {
      if (nvalidhours >= includedaycrit) {
        #--------------------------------------
        # Features per day and per segment of the day
        # guided by qwindow, which is a vector of indices to slice up the day
        keepindex_46 = keepindex_48 = matrix(NA, length(2:ncol(metashort)), 2)
        # generate objects to help with selecting the slices and to give names to the respective variables
        anwi_t0 = 1 # analysis window time 0
        anwi_t1 = nrow(as.matrix(vari)) # analysis window time 1
        anwi_nameindices = "_0-24hr"
        anwi_index = 1
        if (length(qwindow) > 0) {
          if (length(qwindow) == 2 & qwindow[1] == 0 & qwindow[2] == 24) {
          } else {
            oddqwindow = 1:(length(qwindowindices)-1)
            evenqwindow = 2:length(qwindowindices)
            anwi_t0 = c(anwi_t0,((qwindowindices[oddqwindow]*60*(60/ws3))+1))
            anwi_t1 = c(anwi_t1,(qwindowindices[evenqwindow]*60*(60/ws3)))
            for (iin in 1:length(oddqwindow)) {
              anwi_nameindices = c(anwi_nameindices,paste0("_",qwindow[oddqwindow[iin]],"-",qwindow[evenqwindow[iin]],"hr"))
            }
          }
        } else {
          anwi_t0 = c(anwi_t0,((0*60*(60/ws3))+1))
          anwi_t1 = c(anwi_t1,(24*60*(60/ws3)))
          anwi_nameindices = c(anwi_nameindices,"")
        }
        fi_remember = fi
        for (anwi_index in 1:length(anwi_t0)) {
          if(anwi_index == 2 & di == 1) {
            # increase value of fi to leave enough space for the variables to be calculated in second day of measurement
            shift = (deltaLengthQwindow * (fi-fi_remember))
            fi = fi + shift
          }
          L5M5window_name = anwi_nameindices[anwi_index]
          anwindices = anwi_t0[anwi_index]:anwi_t1[anwi_index] # indices of varnum corresponding to a segment
          if (length(anwindices) > 0) {
            for (mi in 2:ncol(metashort)) { #run through metrics (for features based on single metrics)
              NRV = length(which(is.na(as.numeric(as.matrix(vari[,mi]))) == FALSE))
              varnum = as.numeric(as.matrix(vari[,mi])) #varnum is one column of vari
              # # if this is the first or last day and it has more than includedaycrit number of days then expand it
              # Comment out the following 10 lines if you want to include only the actual data
              if (NRV != length(averageday[,(mi-1)])) { #
                difference = NRV - length(averageday[,(mi-1)])
                if (di == 1) {
                  varnum = c(averageday[1:abs(difference),(mi-1)],varnum)
                } else {
                  a56 = length(averageday[,(mi-1)]) - abs(difference)
                  a57 = length(averageday[,(mi-1)])
                  varnum = c(varnum,averageday[a56:a57,(mi-1)])
                }
              }
              if (anwi_index != 1) {
                if (length(anwindices) > 0) {
                  if (max(anwindices) > length(varnum)) {
                    anwindices = anwindices[which(anwindices <= length(varnum))]
                  }
                  varnum = as.numeric(varnum[anwindices]) #cut short varnum to match day segment of interest
                } else {
                  varnum = c()
                }
              }
              # Starting filling output matrix daysummary with variables per day segment and full day.
              if (mi == ENMOi | mi == LFENMOi | mi == BFENi |
                  mi == ENi | mi == HFENi | mi == HFENplusi | mi == MADi | mi == ENMOai) {
                if (length(varnum) > ((60/ws3)*60*min(winhr)*1.2)) { # Calculate values
                  exfi = 0
                  for (winhr_value in winhr) {
                    if (length(varnum) > (60/ws3)*60*winhr_value*3) { # Calculate values
                      # Time window for L5 & M5 analysis
                      t0_LFMF = L5M5window[1] #start in 24 hour clock hours
                      t1_LFMF = L5M5window[2]+(winhr_value-(M5L5res/60)) #end in 24 hour clock hours (if a value higher than 24 is chosen, it will take early hours of previous day to complete the 5 hour window
                      ML5 = g.getM5L5(varnum,ws3,t0_LFMF,t1_LFMF,M5L5res,winhr_value, qM5L5=qM5L5)
                      ML5colna = colnames(ML5)
                      ML5 = as.numeric(ML5)
                      if (anwi_index > 1) {
                        L5M5shift = qwindow[anwi_index - 1]
                      } else {
                        L5M5shift = 0
                      }
                      if (length(ML5) > 3) {
                        ML5 = as.numeric(ML5)
                        ML5[c(1,3)] = ML5[c(1,3)] + L5M5shift
                        daysummary[di,(exfi+fi):(exfi+fi+length(ML5)-1)] = ML5
                      } else {
                        daysummary[di,(exfi+fi):(exfi+fi+length(ML5)-1)] = ""
                      }
                    } else {
                      daysummary[di,(exfi+fi):(exfi+fi+(4*length(winhr))-1+(length(qM5L5)*2))] = ""
                    }
                    # (Below) 4 is the length of ML5 and then 2 extra variables for every qM5L5 value
                    # winhr is not considered because we are in the winhr loop:
                    exfi = exfi+4+(length(qM5L5)*2) 
                  }
                  
                } else {
                  daysummary[di,fi:(fi+(4*length(winhr))-1+(length(qM5L5)*2))] = ""
                }
                for (winhr_value in winhr) { # Variable (column) names
                  ML5colna = c(paste0("L",winhr_value,"hr"), paste0("L",winhr_value), 
                               paste0("M",winhr_value,"hr"), paste0("M",winhr_value))
                  if (length(qM5L5) > 0) {
                    ML5colna = c(ML5colna,
                                 paste0("L",winhr_value,"_q",round(qM5L5 *100)), 
                                 paste0("M",winhr_value,"_q",round(qM5L5 *100)))
                  }
                  # add metric name and timewindow name
                  ds_names[fi:(fi-1+length(ML5colna))] = paste0(ML5colna,"_", colnames(metashort)[mi], "_mg",L5M5window_name)
                  fi=fi+length(ML5colna)
                }
                daysummary[di,fi] = mean(varnum[((1*60*(60/ws3))+1):(6*60*(60/ws3))]) * 1000#from 1am to 6am
                ds_names[fi] = paste0("mean_",colnames(metashort)[mi],"_mg_1-6am"); fi=fi+1
                if (anwi_nameindices[anwi_index] == "") { # for consistency with previous GGIR version
                  anwi_nameindices[anwi_index] = "_24hr"
                }
                if (length(varnum) > 0) {
                  daysummary[di,fi] = mean(varnum) * 1000;
                } else {
                  daysummary[di,fi] = ""
                }
                cn_metashort = colnames(metashort)
                ds_names[fi] = paste0("mean_",cn_metashort[mi],"_mg",anwi_nameindices[anwi_index]); fi=fi+1
                if (anwi_nameindices[anwi_index] == "_24hr") {
                  anwi_nameindices[anwi_index] = ""
                }
                if (doquan == TRUE) {
                  q46 = c()
                  q46 = quantile(varnum,probs=qlevels,na.rm=T,type=quantiletype) * 1000 #times 1000 to convert to mg
                  keepindex_46[mi-1,] = c(fi,(fi+(length(qlevels)-1)))
                  namesq46 = rep(0,length(rownames(as.matrix(q46))))
                  for (rq46i in 1:length(rownames(as.matrix(q46)))) {
                    tmp1 = rownames(as.matrix(q46))[rq46i]
                    tmp2 = as.character(unlist(strsplit(tmp1,"%")))
                    namesq46[rq46i] = paste0("p",tmp2,"_",colnames(metashort)[mi],"_mg",anwi_nameindices[anwi_index]) # t_TWDI[1],"-",t_TWDI[2],"h",sep="")
                  }
                  if (length(varnum) > 0) {
                    daysummary[di,fi:(fi+(length(qlevels)-1))] = q46
                  } else {
                    daysummary[di,fi:(fi+(length(qlevels)-1))] = ""
                  }
                  ds_names[fi:(fi+(length(qlevels)-1))] = namesq46
                  fi = fi+length(qlevels)
                }
                if (doilevels == TRUE) {
                  q48 = c()
                  q47 = cut((varnum*1000),breaks = ilevels,right=FALSE)
                  q47 = table(q47)
                  q48  = (as.numeric(q47) * ws3)/60 #converting to minutes
                  keepindex_48[mi-1,] = c(fi,(fi+(length(q48)-1)))
                  namesq47 = rep(0,length(rownames(q47)))
                  
                  for (rq47i in 1:length(rownames(q47))) {
                    namesq47[rq47i] = paste0(rownames(q47)[rq47i],"_",colnames(metashort)[mi],"_mg",anwi_nameindices[anwi_index]) #t_TWDI[1],"-",t_TWDI[2],"h",sep="")
                  }
                  if (length(varnum) > 0) {
                    daysummary[di,fi:(fi+(length(q48)-1))] = q48
                  } else {
                    daysummary[di,fi:(fi+(length(q48)-1))] = ""
                  }
                  ds_names[fi:(fi+(length(q48)-1))] = namesq47
                  fi = fi+length(q48)
                }
                
                if (doiglevels == TRUE) { # intensity gradient (as described by Alex Rowlands 2018)
                  q49 = c()
                  q50 = cut((varnum*1000),breaks = iglevels,right=FALSE)
                  q50 = table(q50)
                  q49  = (as.numeric(q50) * ws3)/60 #converting to minutes
                  x_ig = zoo::rollmean(iglevels,k=2)
                  y_ig = q49
                  igout = g.intensitygradient(x_ig, y_ig)
                  if (length(varnum) > 0) {
                    daysummary[di,fi:(fi+2)] = as.vector(unlist(igout))
                  } else {
                    daysummary[di,fi:(fi+2)] = rep("",3)
                  }
                  ds_names[fi:(fi+2)] = paste0(c("ig_gradient","ig_intercept","ig_rsquared"),paste0("_",colnames(metashort)[mi], anwi_nameindices[anwi_index]))
                  fi = fi+3
                }
                #=========================================
                if (domvpa == TRUE) {
                  for (mvpai in 1:length(mvpathreshold)) {
                    mvpa = rep(0,6)
                    if (length(varnum) < 100) {
                      mvpa[1:6] = 0
                    } else {
                      # METHOD 1: time spent above threhold based on 5 sec epoch
                      mvpa[1] = length(which(varnum*1000 >= mvpathreshold[mvpai])) / (60/ws3) #time spent MVPA in minutes
                      # METHOD 2: time spent above threshold based on 1minute epoch
                      varnum2 = cumsum(c(0,varnum))
                      select = seq(1,length(varnum2),by=60/ws3)
                      varnum3 = diff(varnum2[round(select)]) / abs(diff(round(select)))
                      mvpa[2] = length(which(varnum3*1000 >= mvpathreshold[mvpai])) #time spent MVPA in minutes
                      # METHOD 3: time spent above threshold based on 5minute epoch
                      select = seq(1,length(varnum2),by=300/ws3)
                      varnum3 = diff(varnum2[round(select)]) / abs(diff(round(select)))
                      mvpa[3] = length(which(varnum3*1000 >= mvpathreshold[mvpai])) * 5 #time spent MVPA in minutes
                      # METHOD 4: time spent above threshold
                      boutduration = mvpadur[1] * (60/ws3) # per minute
                      rr1 = matrix(0,length(varnum),1)
                      p = which(varnum*1000 >= mvpathreshold[mvpai]); rr1[p] = 1
                      getboutout = g.getbout(x=rr1,boutduration=boutduration,boutcriter=boutcriter,closedbout=closedbout,
                                             bout.metric=bout.metric,ws3=ws3)
                      mvpa[4] = length(which(getboutout$x == 1))   / (60/ws3) #time spent MVPA in minutes
                      
                      # METHOD 5: time spent above threshold 5 minutes
                      boutduration = mvpadur[2] * (60/ws3) #per five minutes
                      rr1 = matrix(0,length(varnum),1)
                      p = which(varnum*1000 >= mvpathreshold[mvpai]); rr1[p] = 1
                      getboutout = g.getbout(x=rr1,boutduration=boutduration,boutcriter=boutcriter,closedbout=closedbout,
                                             bout.metric=bout.metric,ws3=ws3)
                      mvpa[5] = length(which(getboutout$x == 1))   / (60/ws3) #time spent MVPA in minutes
                      # METHOD 6: time spent above threshold 10 minutes
                      boutduration = mvpadur[3] * (60/ws3) # per ten minutes
                      rr1 = matrix(0,length(varnum),1)
                      p = which(varnum*1000 >= mvpathreshold[mvpai]); rr1[p] = 1
                      getboutout = g.getbout(x=rr1,boutduration=boutduration,boutcriter=boutcriter,closedbout=closedbout,
                                             bout.metric=bout.metric,ws3=ws3)
                      mvpa[6] = length(which(getboutout$x == 1))   / (60/ws3) #time spent MVPA in minutes
                    }
                    if (length(which(is.nan(mvpa) == TRUE)) > 0) mvpa[which(is.nan(mvpa) == TRUE)] = 0
                    mvpanames[,mvpai] = c( paste0("MVPA_E",ws3,"S_T",mvpathreshold[mvpai]),
                                           paste0("MVPA_E1M_T",mvpathreshold[mvpai]),
                                           paste0("MVPA_E5M_T",mvpathreshold[mvpai]),
                                           paste0("MVPA_E",ws3,"S_B",mvpadur[1],"M",(boutcriter * 100),"%_T",mvpathreshold[mvpai]),
                                           paste0("MVPA_E",ws3,"S_B",mvpadur[2],"M",(boutcriter * 100),"%_T",mvpathreshold[mvpai]),
                                           paste0("MVPA_E",ws3,"S_B",mvpadur[3],"M",(boutcriter * 100),"%_T",mvpathreshold[mvpai]))
                    for (fillds in 1:6) {
                      daysummary[di,fi] = mvpa[fillds]
                      ds_names[fi] = paste(mvpanames[fillds,mvpai],"_",colnames(metashort)[mi] ,anwi_nameindices[anwi_index],sep=""); fi=fi+1
                    }
                  }
                }
              }
              if (mi %in% ExtFunColsi == TRUE) { # INSERT HERE VARIABLES DERIVED WITH EXTERNAL FUNCTION
                if (myfun$reporttype == "event") { # For the event report type we take the sum
                  daysummary[di,fi] = sum(varnum)
                  ds_names[fi] = paste0(colnames(metashort)[mi],"_sum",anwi_nameindices[anwi_index],sep=""); fi=fi+1
                } else if (myfun$reporttype == "scalar") { # For the scalar report type we take the mean
                  daysummary[di,fi] = mean(varnum)
                  ds_names[fi] = paste0(colnames(metashort)[mi],"_mean",anwi_nameindices[anwi_index],sep=""); fi=fi+1
                } else if (myfun$reporttype == "type") { # For type we calculate time spent in each class 
                  # Not implemented yet
                }
              }
            }
          }
        }
      }
      rm(val); rm(vari)
    }
  }
  invisible(list(daysummary=daysummary,ds_names=ds_names, windowsummary=windowsummary, ws_names=ws_names))
}
