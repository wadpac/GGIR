g.calibrate = function(datafile,use.temp=TRUE,spherecrit=0.3,minloadcrit=72,printsummary=TRUE,
                       chunksize=c(),windowsizes=c(5,900,3600),selectdaysfile=c(),dayborder=0) {
  if (length(chunksize) == 0) chunksize = 1
  if (chunksize > 1) chunksize = 1
  if (chunksize < 0.4) chunksize = 0.4
  filename = unlist(strsplit(as.character(datafile),"/"))
  filename = filename[length(filename)]
  # set parameters
  ws4 = 10 #epoch for recalibration, don't change
  ws2 = windowsizes[2] #dummy variable
  ws =  windowsizes[3] # window size for assessing non-wear time (seconds)
  i = 1 #counter to keep track of which binary block is being read
  count = 1 #counter to keep track of the number of seconds that have been read
  LD = 2 #dummy variable used to identify end of file and to make the process stop
  cal.error.start=cal.error.end=c()
  spheredata=c()
  tempoffset=c()
  npoints=c()
  scale = c(1,1,1)
  offset = c(0,0,0)
  bsc_cnt = 0
  bsc_qc = data.frame(time=c(),size=c())
  #inspect file  
  options(warn=-1) #turn off warnings
  INFI = g.inspectfile(datafile)  # Check which file type and monitor brand it is
  options(warn=0) #turn off warnings
  mon = INFI$monc
  dformat = INFI$dformc
  sf = INFI$sf
  if (sf == 0) sf = 80 #assume 80Hertz in the absense of any other info
  options(warn=-1) #turn off warnings
  suppressWarnings(expr={decn = g.dotorcomma(datafile,dformat,mon)}) #detect dot or comma dataformat
  options(warn=0) #turn off warnings
  #creating matrixes for storing output
  S = matrix(0,0,4) #dummy variable needed to cope with head-tailing succeeding blocks of data
  NR = ceiling((90*10^6) / (sf*ws4)) + 1000 #NR = number of 'ws4' second rows (this is for 10 days at 80 Hz) 
  if (mon == 2) {
    meta = matrix(99999,NR,8) #for meta data
  } else if (mon == 1 | mon == 3 | mon == 4){
    meta = matrix(99999,NR,7)
  }
  # setting size of blocks that are loaded (too low slows down the process)
  # the setting below loads blocks size of 12 hours (modify if causing memory problems)
  blocksize = round((14512 * (sf/50)) * (chunksize*0.5)) 
  blocksizegenea = round((20608 * (sf/80)) * (chunksize*0.5))
  if (mon == 1) {
    blocksize = blocksizegenea
  }
  if (mon == 4) {
    blocksize = round(1440 * chunksize)
  }
  #===============================================
  # Read file
  switchoffLD = 0 #dummy variable part of "end of loop mechanism"
  while (LD > 1) {
    P = c()
    print(paste("Loading block: ",i,sep=""))
    #try to read data blocks based on monitor type and data format
    options(warn=-1) #turn off warnings (code complains about unequal rowlengths
    #when trying to read files of a different format)
    if (mon == 1 & dformat == 1) {
      use.temp = FALSE
      try(expr={P = g.binread(datafile,(blocksize*(i-1)),(blocksize*i))},silent=TRUE)
      if (length(P) > 1) {
        if (nrow(P$rawxyz) < ((sf*ws)+1)) {
          P = c()
          switchoffLD = 1 #added 30-6-2012
        }
      } else {
        P = c()
      }
#     } else  if (mon == 4 & dformat == 3) {
#       use.temp = FALSE
#       try(expr={P = g.wavread(datafile,(blocksize*(i-1)),(blocksize*i))},silent=TRUE)
#       if (length(P) > 1) {
#         if (nrow(P$rawxyz) < ((sf*ws)+1)) {
#           P = c()
#           switchoffLD = 1 #added 30-6-2012
#         }
#       } else {
#         P = c()
#       }
    } else if (mon == 2 & dformat == 1) {
      ###
      if (length(selectdaysfile) > 0) { # code to only read fragments of the data (Millenium cohort)
        #===================================================================
        # All of the below needed for Millenium cohort
        SDF = read.csv(selectdaysfile)
        I = g.inspectfile(datafile) #, useRDA = useRDA
        hvars = g.extractheadervars(I)
        SN = hvars$SN
        SDFi = which(as.numeric(SDF$Monitor) == as.numeric(SN))
        # print(SDF[SDFi,])
        #==========================================================================
        # STEP 1: now derive table with start and end times of intervals to load
        # STEP 2: now (based on i and chunksize)  decide which section of these intervals needs to be loaded
        # STEP 3: load data
        # thoughts: maybe treat second day as continuation of first such that clock times can be continuous: 4am-4am & 4am-4am
        tmp1 = unlist(strsplit(as.character(SDF[SDFi,2]),"/"))
        nextday = as.numeric(tmp1[1]) + 1
        nextday = paste0(nextday,"/",tmp1[2],"/",tmp1[3])
        
        tint = matrix(0,5,2)
        if (dayborder > 0) {
          fivebefore = paste0(" 0",dayborder-1,":55:00")
          endday = paste0(" 0",dayborder,":00:00")
        } else if (dayborder < 0) {
          fivebefore = paste0(" ",24+dayborder-1,":55:00")
          endday = paste0(" ",24+dayborder,":00:00")
        } else if (dayborder == 0) {
          fivebefore = paste0(" ",24+dayborder-1,":55:00")
          endday = paste0(" 00:00:00")
        }
        tint[1,1] = paste0(SDF[SDFi,2],fivebefore) #" 03:55:00"
        tint[1,2] =paste0(nextday,endday) #" ","04:00:00"
        tmp2 = unlist(strsplit(as.character(SDF[SDFi,3]),"/"))
        nextday = as.numeric(tmp2[1]) + 1
        nextday = paste0(nextday,"/",tmp2[2],"/",tmp2[3])
        tint[2,1] = paste0(SDF[SDFi,3],endday)
        tint[2,2] =paste0(nextday,endday)
        
        if (i == nrow(tint)) {
          #all data read now make sure that it does not try to re-read it with mmap on
          switchoffLD = 1
          P = c()
        } else {
          ## JH CHANGE #####	 # turned off because we only do g.calibrate once (before RDA file is created)
          #           if(useRDA == TRUE) {
          #             P <- getCalibrateData(datafile, tint[i,1], tint[i,2])
          #           } else {
          try(expr={P = GENEAread::read.bin(binfile=datafile,start=tint[i,1],
                                            end=tint[i,2],calibrate=TRUE,do.temp=TRUE,mmap.load=FALSE)},silent=TRUE)
          # }
          ##################
        }
        # All of the above needed for Millenium cohort
        #======================================================================
      } else {
        ###
        try(expr={P = GENEAread::read.bin(binfile=datafile,start=(blocksize*(i-1)),end=(blocksize*i),calibrate=TRUE,do.temp=TRUE,mmap.load=FALSE)},silent=TRUE)
      }
      # try(expr={P = GENEAread::read.bin(binfile=datafile,start=(blocksize*(i-1)),end=(blocksize*i),calibrate=TRUE,do.temp=TRUE,mmap.load=FALSE)},silent=TRUE)
      #       if (length(P) > 0) {
      #         if (nrow(P$data.out) < blocksize*300) { #last block
      #           cat("\nlast block\n")
      #           switchoffLD = 1 #this section is not needed for mon = 1 as genea script deals with file-ends automatically
      #         }
      #       }
      
      if (length(P) > 0) {
        if (length(selectdaysfile) > 0) { 
          if (tint[i,1] == "0") {
            print("last block")
            switchoffLD = 1
          }
        } else {
          if (nrow(P$data.out) < blocksize*300) { #last block
            print("last block")
            switchoffLD = 1 #this section is not needed for mon = 1 as genea script deals with file-ends automatically
          }
        }
      }
      
      if (length(P) == 0) { #if first block doens't read then probably corrupt
        if (i == 1) {
          try(expr={P = GENEAread::read.bin(binfile=datafile,calibrate=TRUE,do.temp=TRUE,mmap.load=FALSE)},silent=TRUE)
          if(length(P) ==0) {
            P= c()
            switchoffLD = 1
          } #if not then P is now filled with data
        } else {
          P= c() #just no data in this last block
        }
      }
      if (length(P) > 0) { #check whether there is enough data
        if (nrow(P$data.out) < ((sf*ws)+1)) {
          P = c()
          switchoffLD = 1
        }
      }
    } else if (mon == 2 & dformat == 2) {
      try(expr={P = read.csv(datafile,nrow = (blocksize*300), skip=(100+(blocksize*300*(i-1))),dec=decn)},silent=TRUE)
      if (length(P) > 1) {
        P = as.matrix(P)
        if (nrow(P) < ((sf*ws*2)+1) & i == 1) {
          P = c() ; switchoffLD = 1 #added 30-6-2012
          cat("\nError code 1: data too short for doing non-wear detection\n")  	
          filetooshort = TRUE
        }
      } else {
        P = c()
        cat("\nEnd of file reached\n")
      }
    } else if (mon == 3 & dformat == 2) {
      try(expr={P = read.csv(datafile,nrow = (blocksize*300), skip=(10+(blocksize*300*(i-1))),dec=decn)},silent=TRUE)
      use.temp = FALSE
      if (length(P) > 1) {
        P = as.matrix(P)
        if (nrow(P) < ((sf*ws*2)+1) & i == 1) {
          P = c() ; switchoffLD = 1 #added 30-6-2012
          cat("\nError code 1: data too short for doing non-wear detection\n")
          filetooshort = TRUE
        }
      } else {
        P = c()
        cat("\nEnd of file reached\n")
      }
    }
    options(warn=0) #turn on warnings
    #process data as read from binary file
    if (length(P) > 0) { #would have been set to zero if file was corrupt or empty
      if (mon == 1) {
        data = P$rawxyz / 1000 #convert g output to mg for genea
      } else if (mon == 4) {
        data = P$rawxyz #change scalling for Axivity?
      } else if (mon == 2  & dformat == 1) {
        data = P$data.out
      } else if (dformat == 2) {
        data = as.matrix(P)
      }
      #add left over data from last time 
      if (min(dim(S)) > 1) {
        data = rbind(S,data)
      }
      LD = nrow(data)
      #store data that could not be used for this block, but will be added to next block
      use = (floor(LD / (ws*sf))) * (ws*sf) #number of datapoint to use
      if (length(use) > 0) {
        if (use > 0) {
          if (use != LD) {
            S = as.matrix(data[(use+1):LD,]) #store left over
          }
          data = as.matrix(data[1:use,])
          LD = nrow(data) #redefine LD because there is less data
          ##==================================================
          dur = nrow(data)	#duration of experiment in data points
          durexp = nrow(data) / (sf*ws)	#duration of experiment in hrs
          # Initialization of variables
          if (mon == 1) {
            Gx = as.numeric(data[,1]); Gy = as.numeric(data[,2]); Gz = as.numeric(data[,3])
            use.temp = FALSE
          } else if (mon == 4) {
            Gx = as.numeric(data[,1]); Gy = as.numeric(data[,2]); Gz = as.numeric(data[,3])
            use.temp = FALSE
          } else if (mon == 2 & dformat == 1) {
            Gx = as.numeric(data[,2]); Gy = as.numeric(data[,3]); Gz = as.numeric(data[,4]); temperaturre = as.numeric(data[,7])
            temperature = as.numeric(data[,7])
          } else if (dformat == 2) {
            data2 = matrix(NA,nrow(data),3)
            if (ncol(data) == 3) extra = 0
            if (ncol(data) >= 4) extra = 1
            for (jij in 1:3) {
              data2[,jij] = as.numeric(data[,(jij+extra)])
            }
            Gx = as.numeric(data2[,1]); Gy = as.numeric(data2[,2]); Gz = as.numeric(data2[,3])
            if (mon == 2) {
              temperature = as.numeric(data[,7])
            } else if (mon == 1 | mon == 3) {
              use.temp = FALSE
            }
          }
          if (mon == 2 & use.temp == TRUE) { #also ignore temperature for GENEActive if temperature values are unrealisticly high or NA
            if (length(which(is.na(mean(as.numeric(data[1:10,7]))) == T)) > 0) {
              cat("\ntemperature is NA\n")
              use.temp = FALSE
            } else if (length(which(mean(as.numeric(data[1:10,7])) > 40)) > 0) {
              cat("\ntemperature is too high\n")
              use.temp = FALSE
            }
          }
          #=============================================
          # non-integer sample frequency is a pain for deriving epoch based sd
          # however, with an epoch of 10 seconds it is an integer number of samples per epoch
          EN = sqrt(Gx^2 + Gy^2 + Gz^2)
          D1 = g.downsample(EN,sf,ws4,ws2)
          EN2 = D1$var2
          #mean acceleration
          D1 = g.downsample(Gx,sf,ws4,ws2); 	GxM2 = D1$var2
          D1 = g.downsample(Gy,sf,ws4,ws2); 	GyM2 = D1$var2
          D1 = g.downsample(Gz,sf,ws4,ws2); 	GzM2 = D1$var2
          if (use.temp == TRUE) {
            D1 = g.downsample(temperature,sf,ws4,ws2);
            TemperatureM2 = D1$var2
          }
          #sd acceleration
          dim(Gx) = c(sf*ws4,ceiling(length(Gx)/(sf*ws4))); 	GxSD2 = apply(Gx,2,sd)
          dim(Gy) = c(sf*ws4,ceiling(length(Gy)/(sf*ws4))); 	GySD2 = apply(Gy,2,sd)
          dim(Gz) = c(sf*ws4,ceiling(length(Gz)/(sf*ws4))); 	GzSD2 = apply(Gz,2,sd)
          #-----------------------------------------------------
          #expand 'out' if it is expected to be too short
          if (count > (nrow(meta) - (2.5*(3600/ws4) *24))) {  
            extension = matrix(" ",((3600/ws4) *24),ncol(meta))
            meta = rbind(meta,extension)
            cat("\nvariabel meta extended\n")
          }
          #storing in output matrix
          meta[count:(count-1+length(EN2)),1] = EN2
          meta[count:(count-1+length(EN2)),2] = GxM2
          meta[count:(count-1+length(EN2)),3] = GyM2
          meta[count:(count-1+length(EN2)),4] = GzM2
          meta[count:(count-1+length(EN2)),5] = GxSD2
          meta[count:(count-1+length(EN2)),6] = GySD2
          meta[count:(count-1+length(EN2)),7] = GzSD2
          if (use.temp == TRUE) {
            meta[count:(count-1+length(EN2)),8] = TemperatureM2
          }
          count = count + length(EN2) #increasing "count": the indicator of how many seconds have been read
          rm(Gx); rm(Gy); rm(Gz)
          # reduce blocksize if memory is getting higher
          gco = gc()
          memuse = gco[2,2] #memuse in mb
          bsc_qc = rbind(bsc_qc,c(memuse,Sys.time()))
          if (memuse > 4000) {
            if (bsc_cnt < 5) {
              if ((chunksize * (0.8 ^ bsc_cnt)) > 0.2) {
                blocksize = round(blocksize * 0.8)
                
                bsc_cnt = bsc_cnt + 1
              }
            }
          }
        }
        #--------------------------------------------
      }
    } else {
      LD = 0 #once LD < 1 the analysis stops, so this is a trick to stop it
      cat("\nstop reading because there is not enough data in this block\n")
    }
    spherepopulated = 0
    if (switchoffLD == 1) {
      LD = 0
    }
    meta_temp = data.frame(V = meta)
    cut = which(meta_temp[,1] == 99999)
    if (length(cut) > 0) {
      meta_temp = as.matrix(meta_temp[-cut,])
    }
    nhoursused = (nrow(meta_temp) * 10)/3600
    if (nrow(meta_temp) > (minloadcrit-21)) {  # enough data for the sphere?
      meta_temp = as.matrix(meta_temp[-1,])
      #select parts with no movement
      if (mon == 1) {
        sdcriter = 0.013 #0.003 (changed, because seemed to be too critical)
      } else if (mon == 2) {
        sdcriter = 0.013 #0.0109 in rest test
      } else if (mon == 3) {
        sdcriter = 0.013 # NO IDEA WHAT REST NOISE IS FOR ACTIGRAPH....test needed
      } else if (mon == 4) {
        sdcriter = 0.013 # NO IDEA WHAT REST NOISE IS FOR AXIVITY....test needed
      }
      nomovement = which(meta_temp[,5] < sdcriter & meta_temp[,6] < sdcriter & meta_temp[,7] < sdcriter &
                           abs(as.numeric(meta_temp[,2])) < 2 & abs(as.numeric(meta_temp[,3])) < 2 &
                           abs(as.numeric(meta_temp[,4])) < 2) #the latter three are to reduce chance of including clipping periods
      meta_temp = as.matrix(meta_temp[nomovement,])
      if (min(dim(meta_temp)) > 1) {
        meta_temp = as.matrix(meta_temp[(is.na(meta_temp[,4]) == F & is.na(meta_temp[,1]) == F),])
        npoints = nrow(meta_temp)
        cal.error.start = sqrt(as.numeric(meta_temp[,2])^2 + as.numeric(meta_temp[,3])^2 + as.numeric(meta_temp[,4])^2)			
        cal.error.start = mean(abs(cal.error.start - 1))
        #check whether sphere is well populated
        tel = 0
        for (axis in 2:4) {
          if ( min(meta_temp[,axis]) < -spherecrit & max(meta_temp[,axis]) > spherecrit) {
            tel = tel + 1
          }
        }
        if (tel == 3) {
          spherepopulated = 1
        } else {
          spherepopulated = 0
          QC = "recalibration not done because not enough points on all sides of the sphere"
        }
      } else {
        cat("\nno non-movement found\n")
        QC = "recalibration not done because no non-movement data available"
        meta_temp = c()
      }
    } else {
      QC = "recalibration not done because not enough data in the file or because file is corrupt"
    }
    if (spherepopulated == 1) { #only try to improve calibration if there are enough datapoints around the sphere
      #---------------------------------------------------------------------------
      # START of Zhou Fang's code (slightly edited by vtv21 to use matrix meta_temp from above
      # instead the similar matrix generated by Zhou Fang's original code. This to allow for
      # more data to be used as meta_temp can now be based on 10 or more days of raw data
      input = meta_temp[,2:4] #as.matrix()
      if (use.temp == TRUE) { #at the moment i am always using temperature if mon == 2
        # mon == 2 & removed 19-11-2014 because it is redundant and would not allow for newer monitors to use it
        inputtemp = cbind(as.numeric(meta_temp[,8]),as.numeric(meta_temp[,8]),as.numeric(meta_temp[,8])) #temperature
      } else {
        inputtemp = matrix(0, nrow(input), ncol(input)) #temperature, here used as a dummy variable
      }
      meantemp = mean(as.numeric(inputtemp[,1]),na.rm=TRUE)
      inputtemp = inputtemp - meantemp
      offset = rep(0, ncol(input))
      scale = rep(1, ncol(input))
      tempoffset = rep(0, ncol(input))
      weights = rep(1, nrow(input))
      res = Inf
      maxiter = 1000
      tol = 1e-10
      for (iter in 1:maxiter) {
        curr = scale(input, center = -offset, scale = 1/scale) + scale(inputtemp, center = F, scale = 1/tempoffset)
        closestpoint = curr/ sqrt(rowSums(curr^2))
        k = 1
        offsetch = rep(0, ncol(input))
        scalech = rep(1,ncol(input))
        toffch = rep(0, ncol(inputtemp))
        for (k in 1:ncol(input)){
          #-----------------------------------------------------------------
          # Next few lines added 23 on april 2015 to deal with NaN values in
          # some of the sphere data for Actigraph monitor brand
          if (mon == 3 & length(which(is.na(closestpoint[,k, drop = F]) == TRUE)) > 0 &
              length(which(is.na(closestpoint[,k, drop = F]) == FALSE)) > 10) { #needed for some Actigraph data
            invi = which(is.na(closestpoint[,k, drop = F]) == TRUE)
            closestpoint = closestpoint[-invi,]
            curr = curr[-invi,]
            inputtemp = inputtemp[-invi,]
            input = input[-invi,]
            weights = weights[-invi]
          }
          #------------------------------------------------
          fobj = lm.wfit(cbind(1, curr[,k],inputtemp[,k]) , closestpoint[,k, drop = F], w = weights)
          offsetch[k] = fobj$coef[1]
          scalech[k] = fobj$coef[2]
          if (use.temp == TRUE) {
            toffch[k] = fobj$coeff[3]
          }
          curr[,k] = fobj$fitted.values
        }
        offset = offset + offsetch / (scale  * scalech)
        if (use.temp == TRUE) {
          tempoffset = tempoffset * scalech + toffch
        }
        scale = scale * scalech
        res = c(res,  3 * mean(weights*(curr-closestpoint)^2/ sum(weights)))
        weights = pmin(1/ sqrt(rowSums((curr - closestpoint)^2)), 1/0.01)
        if (abs(res[iter+1] - res[iter]) < tol)  break
      }
      if (use.temp == FALSE) {
        meta_temp2 = scale(as.matrix(meta_temp[,2:4]),center = -offset, scale = 1/scale)
      } else {
        yy = as.matrix(cbind(as.numeric(meta_temp[,8]),as.numeric(meta_temp[,8]),as.numeric(meta_temp[,8])))
        meta_temp2 = scale(as.matrix(meta_temp[,2:4]),center = -offset, scale = 1/scale) +
          scale(yy, center = rep(meantemp,3), scale = 1/tempoffset)
      }     #equals: D2[,axis] = (D[,axis] + offset[axis]) / (1/scale[axis])
      # END of Zhou Fang's code
      #-------------------------------------------
      cal.error.end = sqrt(meta_temp2[,1]^2 + meta_temp2[,2]^2 + meta_temp2[,3]^2)
      cal.error.end = mean(abs(cal.error.end-1))
      # assess whether calibration error has sufficiently been improved
      if (cal.error.end < cal.error.start & cal.error.end < 0.01 & nhoursused > minloadcrit) { #do not change scaling if there is no evidence that calibration improves
        if (use.temp == TRUE & mon == 2) {
          QC = "recalibration done, no problems detected"
        } else if (use.temp == FALSE & mon == 2)  {
          QC = "recalibration done, but temperature values not used"
        } else if (mon != 2)  {
          QC = "recalibration done, no problems detected"
        }
        LD = 0 #stop loading 
      } else { #continue loading data
        if (nhoursused > minloadcrit) {
          print(paste("new error: ",cal.error.end," g",sep=""))
          print(paste("npoints around sphere: ", npoints,sep=""))
        }
        QC = "recalibration attempted with all available data, but possibly not good enough: Check calibration error variable to varify this"
      }
    }
    i = i + 1 #go to next block (12 hours-isch)
  }
  if (length(cal.error.end) > 0) {
    if (cal.error.end > cal.error.start) {
      QC = "recalibration not done because recalibration does not decrease error"
    }
  }
  if (length(ncol(meta_temp)) != 0) {
    spheredata = data.frame(A = meta_temp)
    if (use.temp == TRUE) {
      names(spheredata) = c("Euclidean Norm","meanx","meany","meanz","sdx","sdy","sdz","temperature")
    } else {
      names(spheredata) = c("Euclidean Norm","meanx","meany","meanz","sdx","sdy","sdz")
    }
  } else {
    spheredata = c()
  }
  QCmessage = QC
  if (printsummary == TRUE) {
    cat("\n----------------------------------------\n")
    cat("Summary of autocalibration procedure:\n")
    cat("\nStatus:\n")
    print(QCmessage)
    cat("\nError (g) before:\n")
    print(cal.error.start)
    cat("\nError (g) after:\n")
    print(cal.error.end)
    cat("\nOffset correction:\n")
    print(offset)
    cat("\nScale correction:\n")
    print(scale)
    cat("\nNumber of hours used:\n")
    print(nhoursused)
    cat("\nNumber of 10 second windows around the sphere:\n")
    print(npoints)
    cat("\nTemperature used (if available):\n")
    print(use.temp)
    cat("\nTemperature offset (if temperature is available):\n")
    print(tempoffset)
    cat("\n----------------------------------------\n")
  }
  if (use.temp==TRUE) {
    if (length(spheredata) > 0) {
      meantempcal = mean(spheredata[,8],na.rm=TRUE)
    } else {
      meantempcal = c()
    }
  } else {
    meantempcal = c()
  }
  invisible(list(scale=scale,offset=offset,tempoffset=tempoffset,
                 cal.error.start=cal.error.start,cal.error.end=cal.error.end,
                 spheredata=spheredata,npoints=npoints,nhoursused=nhoursused,
                 QCmessage=QCmessage,use.temp=use.temp,meantempcal=meantempcal,bsc_qc=bsc_qc))
}