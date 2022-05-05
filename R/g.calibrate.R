g.calibrate = function(datafile, params_rawdata = c(),
                       params_general = c(),
                       params_cleaning = c(),
                       ...) {
  
  #get input variables
  input = list(...)
  expectedArgs = c("datadir", "params_rawdata", "params_general", "params_cleaning")
  if (any(names(input) %in% expectedArgs == FALSE) |
      any(!unlist(lapply(expectedArgs, FUN = exists)))) {
    # Extract and check parameters if user provides more arguments than just the parameter arguments
    # So, inside GGIR this will not be used, but it is used when g.calibrate is used on its own
    # as if it was still the old g.calibrate function
    params = extract_params(params_rawdata = params_rawdata,
                            params_cleaning = params_cleaning,
                            params_general = params_general,
                            input = input) # load default parameters
    params_rawdata = params$params_rawdata
    params_cleaning = params$params_cleaning
    params_general = params$params_general
  }
  
  use.temp = TRUE
  filename = unlist(strsplit(as.character(datafile),"/"))
  filename = filename[length(filename)]
  # set parameters
  filequality = data.frame(filetooshort = FALSE, filecorrupt = FALSE,
                           filedoesnotholdday = FALSE, stringsAsFactors = TRUE)
  ws4 = 10 #epoch for recalibration, don't change
  ws2 = params_general[["windowsizes"]][2] #dummy variable
  ws =  params_general[["windowsizes"]][3] # window size for assessing non-wear time (seconds)
  i = 1 #counter to keep track of which binary block is being read
  count = 1 #counter to keep track of the number of seconds that have been read
  LD = 2 #dummy variable used to identify end of file and to make the process stop
  cal.error.start = cal.error.end = c()
  spheredata = c()
  tempoffset = c()
  npoints = c()
  PreviousEndPage = c() # needed for g.readaccfile
  scale = c(1,1,1)
  offset = c(0,0,0)
  bsc_qc = data.frame(time = c(), size = c(), stringsAsFactors = FALSE)
  #inspect file
  op <- options(stringsAsFactors = FALSE)
  on.exit(options(op))
  options(warn = -1) #turn off warnings
  INFI = g.inspectfile(datafile, desiredtz = params_general[["desiredtz"]],
                       rmc.dec = params_rawdata[["rmc.dec"]],
                       rmc.firstrow.acc = params_rawdata[["rmc.firstrow.acc"]],
                       rmc.firstrow.header = params_rawdata[["rmc.firstrow.header"]],
                       rmc.header.length = params_rawdata[["rmc.header.length"]],
                       rmc.col.acc = params_rawdata[["rmc.col.acc"]],
                       rmc.col.temp = params_rawdata[["rmc.col.temp"]],
                       rmc.col.time = params_rawdata[["rmc.col.time"]],
                       rmc.unit.acc = params_rawdata[["rmc.unit.acc"]],
                       rmc.unit.temp = params_rawdata[["rmc.unit.temp"]],
                       rmc.unit.time = params_rawdata[["rmc.unit.time"]],
                       rmc.format.time = params_rawdata[["rmc.format.time"]],
                       rmc.bitrate = params_rawdata[["rmc.bitrate"]],
                       rmc.dynamic_range = params_rawdata[["rmc.dynamic_range"]],
                       rmc.unsignedbit = params_rawdata[["rmc.unsignedbit"]],
                       rmc.origin = params_rawdata[["rmc.origin"]],
                       rmc.desiredtz = params_rawdata[["rmc.desiredtz"]],
                       rmc.sf = params_rawdata[["rmc.sf"]],
                       rmc.headername.sf = params_rawdata[["rmc.headername.sf"]],
                       rmc.headername.sn = params_rawdata[["rmc.headername.sn"]],
                       rmc.headername.recordingid = params_rawdata[["rmc.headername.sn"]],
                       rmc.header.structure = params_rawdata[["rmc.header.structure"]],
                       rmc.check4timegaps = params_rawdata[["rmc.check4timegaps"]])  # Check which file type and monitor brand it is
  options(warn = 0) #turn on warnings
  mon = INFI$monc
  if (mon == 6) mon = 3
  dformat = INFI$dformc
  sf = INFI$sf
  # if GENEActiv csv, deprecated function
  if (mon == 2 & dformat == 2 & length(params_rawdata[["rmc.firstrow.acc"]]) == 0) {
    stop("The GENEActiv csv reading functionality is deprecated in GGIR from the version 2.6-4 onwards. Please, use either the GENEActiv bin files or try to read the csv files with GGIR::read.myacc.csv")
  }
  if (length(sf) == 0) { # if sf is not available then try to retrieve sf from rmc.sf
    if (length(params_rawdata[["rmc.sf"]]) == 0) {
      stop("Could not identify sample frequency")
    } else {
      if (params_rawdata[["rmc.sf"]] == 0) {
        stop("Could not identify sample frequency")
      } else {
        sf = params_rawdata[["rmc.sf"]]
      }
    }
  }
  if (sf == 0) stop("Sample frequency not recognised")
  options(warn = -1) #turn off warnings
  suppressWarnings(expr = {decn = g.dotorcomma(datafile, dformat, mon, 
                                               desiredtz = params_general[["desiredtz"]],
                                               rmc.dec = params_rawdata[["rmc.dec"]])}) #detect dot or comma dataformat
  options(warn = 0) #turn on warnings
  #creating matrixes for storing output
  S = matrix(0,0,4) #dummy variable needed to cope with head-tailing succeeding blocks of data
  NR = ceiling((90*10^6) / (sf*ws4)) + 1000 #NR = number of 'ws4' second rows (this is for 10 days at 80 Hz)
  if (mon == 2 | (mon == 4 & dformat == 4) | mon == 5 | (mon == 0 & length(params_rawdata[["rmc.col.temp"]]) > 0)) {
    meta = matrix(99999,NR,8) #for meta data
  } else if (mon == 1 | mon == 3 | (mon == 4 & dformat == 3) |
             (mon == 4 & dformat == 2) | (mon == 0 & length(params_rawdata[["rmc.col.temp"]]) == 0)) {
    meta = matrix(99999,NR,7)
  }
  # setting size of blocks that are loaded (too low slows down the process)
  # the setting below loads blocks size of 12 hours (modify if causing memory problems)
  blocksize = round((14512 * (sf/50)) * (params_rawdata[["chunksize"]]*0.5))
  blocksizegenea = round((20608 * (sf/80)) * (params_rawdata[["chunksize"]]*0.5))
  if (mon == 1) blocksize = blocksizegenea
  if (mon == 4 & dformat == 3) blocksize = round(1440 * params_rawdata[["chunksize"]])
  if (mon == 4 & dformat == 2) blocksize = round(blocksize)
  if (mon == 5) blocksize = (sf * 60 * 1440) / 2   #Around 12 hours of data for movisens
  if (mon == 3 & dformat == 6) blocksize = (12 * 3600) * params_rawdata[["chunksize"]]
  #===============================================
  # Read file
  switchoffLD = 0 #dummy variable part of "end of loop mechanism"
  while (LD > 1) {
    P = c()
    if (i  == 1) {
      cat(paste("\nLoading chunk: ",i,sep=""))
    } else {
      cat(paste(" ",i,sep=""))
    }
    #try to read data blocks based on monitor type and data format
    options(warn=-1) #turn off warnings (code complains about unequal rowlengths
    accread = g.readaccfile(filename = datafile, blocksize = blocksize, blocknumber = i,
                            filequality = filequality, decn = decn, ws = ws,
                            PreviousEndPage = PreviousEndPage, inspectfileobject = INFI,
                            params_rawdata = params_rawdata, params_general = params_general)
    P = accread$P
    filequality = accread$filequality
    filetooshort = filequality$filetooshort
    filecorrupt = filequality$filecorrupt
    filedoesnotholdday = filequality$filedoesnotholdday
    switchoffLD = accread$switchoffLD
    PreviousEndPage = accread$endpage
    PreviousStartPage = accread$startpage
    rm(accread);
    options(warn = 0) #turn on warnings
    #process data as read from binary file
    if (length(P) > 0) { #would have been set to zero if file was corrupt or empty
      if (mon == 1) {
        data = P$rawxyz / 1000 #convert g output to mg for genea
      } else if (mon == 4 & dformat == 3) {
        data = P$rawxyz #change scalling for Axivity?
      } else if (mon == 2 & dformat == 1) {
        data = P$data.out
      } else if (dformat == 2 | mon == 5) {
        data = as.matrix(P)
      } else if (dformat == 4) {
        if (P$header$hardwareType == "AX6") { # cwa AX6
          # Note 18-Feb-2020: For the moment GGIR ignores the AX6 gyroscope signals until robust sensor
          # fusion algorithms and gyroscope metrics have been prepared
          data = P$data[,-c(2:4)]
        } else {
          # cwa AX3
          data = P$data
        }
      } else if (dformat == 5) {
        data = P$data
      } else if (dformat == 6) {
        data = as.matrix(P[,2:4])
      }
      rm(P)
      #add left over data from last time
      if (min(dim(S)) > 1) {
        data = rbind(S,data)
      }
      # remove 0s if ActiGraph csv (idle sleep mode)
      if (mon == 3 & dformat == 2) {
        data = g.imputeTimegaps(x = as.data.frame(data), xyzCol = 1:3, timeCol = c(), sf = sf, impute = FALSE)
        data = as.matrix(data)
      }
      LD = nrow(data)
      #store data that could not be used for this block, but will be added to next block
      use = (floor(LD / (ws*sf))) * (ws*sf) #number of datapoint to use
      if (length(use) > 0) {
        if (use > 0) {
          if (use != LD) {
            S = as.matrix(data[(use + 1):LD,]) #store left over # as.matrix removed on 22May2019 because redundant
            #S = data[(use+1):LD,] #store left over
          }
          data = as.matrix(data[1:use,])
          LD = nrow(data) #redefine LD because there is less data
          ##==================================================
          dur = nrow(data)	#duration of experiment in data points
          durexp = nrow(data) / (sf*ws)	#duration of experiment in hrs
          # Initialization of variables
          if (dformat != 5) {
            suppressWarnings(storage.mode(data) <- "numeric")
          } 
          if (mon == 1) { # GENEA
            Gx = data[,1]; Gy = data[,2]; Gz = data[,3]
            use.temp = FALSE
          } else if (dformat == 3 & mon == 4) { #AX wav
            Gx = data[,1]; Gy = data[,2]; Gz = data[,3]
            use.temp = FALSE
          } else if (dformat == 4 & mon == 4) { #AX cwa
            Gx = data[,2]; Gy = data[,3]; Gz = data[,4]
            use.temp = TRUE
          } else if (dformat == 2 & mon == 4) { #AX csv
            Gx = data[,2]; Gy = data[,3]; Gz = data[,4]
            use.temp = FALSE
          } else if (dformat == 6 & mon == 3) { #gt3x
            Gx = data[,1]; Gy = data[,2]; Gz = data[,3]
            use.temp = FALSE
          } else if (mon == 2 & dformat == 1) { #GENEActiv bin
            Gx = data[,2]; Gy = data[,3]; Gz = data[,4]; temperature = data[,7]
            temperature = as.numeric(data[,7])
          } else if (mon == 5) { # Movisense
            Gx = data[,1]; Gy = data[,2]; Gz = data[,3]
            use.temp = TRUE
          } else if (mon == 0 & dformat == 5 & length(params_rawdata[["rmc.col.temp"]]) > 0) { # ad-hoc format csv with temperature
            Gx = as.numeric(data[,2]); Gy = as.numeric(data[,3]); Gz = as.numeric(data[,4])
            temperature = as.numeric(data[, params_rawdata[["rmc.col.temp"]]])
            use.temp = TRUE
          } else if (mon == 0 & dformat == 5 & length(params_rawdata[["rmc.col.temp"]]) == 0) { # ad-hoc format csv without temperature
            Gx = as.numeric(data[,2]); Gy = as.numeric(data[,3]); Gz = as.numeric(data[,4])
            use.temp = FALSE
          } else if (dformat == 2 & mon != 4) { # csv and not AX (so, GENEAcitv)
            data2 = matrix(NA,nrow(data),3)
            if (ncol(data) == 3) extra = 0
            if (ncol(data) >= 4) extra = 1
            for (jij in 1:3) {
              data2[,jij] = data[,(jij+extra)]
            }
            Gx = data[,1]; Gy = data[,2]; Gz = data[,3]
          }
          
          if (mon == 2 | (mon == 4 & dformat == 4) | (mon == 0 & use.temp == TRUE)) {
            if (mon == 2) { # GENEActiv
              temperaturecolumn = 7
            } else if (mon == 4 | mon == 5) { #AX | Movisense
              temperaturecolumn = 5
            }
            temperature = as.numeric(data[,temperaturecolumn])
          } else if (mon == 1 | mon == 3) { # GENEA or Actigraph
            use.temp = FALSE
          } else if (mon == 5) { #Movisense
            temperature = g.readtemp_movisens(datafile, params_general[["desiredtz"]], PreviousStartPage, PreviousEndPage,
                                              interpolationType = params_rawdata[["interpolationType"]])
            data = cbind(data, temperature[1:nrow(data)])
            colnames(data)[4] = "temp"
            temperaturecolumn = 4
          }
          if ((mon == 2 | (mon == 4 & dformat == 4) | mon == 5 | mon == 0) & use.temp == TRUE) {
            # GENEACTIV \ AX cwa \ Movisense \ ad-hoc monitor
            #also ignore temperature for GENEActive/movisens if temperature values are unrealisticly high or NA
            if (length(which(is.na(mean(as.numeric(data[1:10,temperaturecolumn]))) == T)) > 0) {
              cat("\ntemperature is NA\n")
              use.temp = FALSE
            } else if (length(which(mean(as.numeric(data[1:10,temperaturecolumn])) > 40)) > 0) {
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
          if (count > (nrow(meta) - (2.5 * (3600/ws4) * 24))) {
            extension = matrix(99999, ((3600/ws4) * 24), ncol(meta))
            meta = rbind(meta,extension)
            cat("\nvariabel meta extended\n")
          }
          #storing in output matrix
          meta[count:(count - 1 + length(EN2)), 1] = EN2
          meta[count:(count - 1 + length(EN2)), 2] = GxM2
          meta[count:(count - 1 + length(EN2)), 3] = GyM2
          meta[count:(count - 1 + length(EN2)), 4] = GzM2
          meta[count:(count - 1 + length(EN2)), 5] = GxSD2
          meta[count:(count - 1 + length(EN2)), 6] = GySD2
          meta[count:(count - 1 + length(EN2)), 7] = GzSD2
          if (use.temp == TRUE) {
            meta[count:(count - 1 + length(EN2)), 8] = TemperatureM2
          }
          count = count + length(EN2) #increasing "count": the indicator of how many seconds have been read
          rm(Gx); rm(Gy); rm(Gz)
          # Update blocksize depending on available memory:
          BlocksizeNew = updateBlocksize(blocksize = blocksize, bsc_qc = bsc_qc)
          bsc_qc = BlocksizeNew$bsc_qc
          blocksize = BlocksizeNew$blocksize
        }
        #--------------------------------------------
      }
    } else {
      LD = 0 #once LD < 1 the analysis stops, so this is a trick to stop it
      # cat("\nstop reading because there is not enough data in this block\n")
    }
    spherepopulated = 0
    if (switchoffLD == 1) {
      LD = 0
    }
    meta_temp = data.frame(V = meta, stringsAsFactors = FALSE)
    cut = which(meta_temp[,1] == 99999)
    if (length(cut) > 0) {
      meta_temp = meta_temp[-cut,]
    }
    nhoursused = (nrow(meta_temp) * 10) / 3600
    if (nrow(meta_temp) > (params_rawdata[["minloadcrit"]] - 21)) {  # enough data for the sphere?
      meta_temp = meta_temp[-1,]
      #select parts with no movement
      if (mon %in% c(1, 2, 3, 4, 5)) {
        sdcriter = 0.013
      } else if (mon == 0) {
        if (length(params_rawdata[["rmc.noise"]]) == 0) {
          warning("Argument rmc.noise not specified, please specify expected noise level in g-units")
        }
        sdcriter = params_rawdata[["rmc.noise"]] * 1.2
        if (length(params_rawdata[["rmc.noise"]]) == 0) {
          stop("Please provide noise level for the acceleration sensors in g-units with argument rmc.noise to aid non-wear detection")
        }
      }
      nomovement = which(meta_temp[,5] < sdcriter & meta_temp[,6] < sdcriter & meta_temp[,7] < sdcriter &
                           abs(as.numeric(meta_temp[,2])) < 2 & abs(as.numeric(meta_temp[,3])) < 2 &
                           abs(as.numeric(meta_temp[,4])) < 2) #the latter three are to reduce chance of including clipping periods
      meta_temp = meta_temp[nomovement,]
      rm(nomovement)
      if (min(dim(meta_temp)) > 1) {
        meta_temp = meta_temp[(is.na(meta_temp[,4]) == F & is.na(meta_temp[,1]) == F),]
        npoints = nrow(meta_temp)
        cal.error.start = sqrt(as.numeric(meta_temp[,2])^2 + as.numeric(meta_temp[,3])^2 + as.numeric(meta_temp[,4])^2)
        cal.error.start = round(mean(abs(cal.error.start - 1)), digits = 5)
        #check whether sphere is well populated
        tel = 0
        for (axis in 2:4) {
          if ( min(meta_temp[,axis]) < -params_rawdata[["spherecrit"]] & max(meta_temp[,axis]) > params_rawdata[["spherecrit"]]) {
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
        cat(" No non-movement found\n")
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
      meantemp = mean(as.numeric(inputtemp[, 1]), na.rm = TRUE)
      inputtemp = inputtemp - meantemp
      offset = rep(0, ncol(input))
      scale = rep(1, ncol(input))
      tempoffset = rep(0, ncol(input))
      weights = rep(1, nrow(input))
      res = Inf
      maxiter = 1000
      tol = 1e-10
      for (iter in 1:maxiter) {
        curr = c()
        try(expr = {curr = scale(input, center = -offset, scale = 1/scale) +
          scale(inputtemp, center = F, scale = 1/tempoffset)}, silent = TRUE)
        if (length(curr) == 0) {
          # set coefficients to default, because it did not work.
          cat("\nObject curr has length zero.")
          break
        }
        closestpoint = curr / sqrt(rowSums(curr^2))
        k = 1
        offsetch = rep(0, ncol(input))
        scalech = rep(1, ncol(input))
        toffch = rep(0, ncol(inputtemp))
        for (k in 1:ncol(input)) {
          #-----------------------------------------------------------------
          # Next few lines added 23 on april 2015 to deal with NaN values in
          # some of the sphere data for Actigraph monitor brand
          # Expanded on 17-Sep-2017 for the generic data format too
          if ((mon == 3 | mon == 5) & length(which(is.na(closestpoint[,k, drop = F]) == TRUE)) > 0 &
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
        res = c(res,  3 * mean(weights*(curr - closestpoint)^2 / sum(weights)))
        weights = pmin(1 / sqrt(rowSums((curr - closestpoint)^2)), 1 / 0.01)
        if (abs(res[iter + 1] - res[iter]) < tol)  break
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
      rm(meta_temp2)
      cal.error.end = round(mean(abs(cal.error.end - 1)), digits = 5)
      # assess whether calibration error has sufficiently been improved
      if (cal.error.end < cal.error.start & cal.error.end < 0.01 & nhoursused > params_rawdata[["minloadcrit"]]) { #do not change scaling if there is no evidence that calibration improves
        if (use.temp == TRUE & (mon == 2 | (mon == 4 & dformat == 4) | mon == 5 | (mon == 0 & use.temp == FALSE))) {
          QC = "recalibration done, no problems detected"
        } else if (use.temp == FALSE & (mon == 2 | (mon == 4 & dformat == 4) |
                                        (mon == 4 & dformat == 2) | mon == 5 | (mon == 0 & use.temp == TRUE)))  {
          QC = "recalibration done, but temperature values not used"
        } else if (mon != 2 & dformat != 3)  {
          QC = "recalibration done, no problems detected"
        }
        LD = 0 #stop loading
      } else {
        #continue loading data
        if (nhoursused > params_rawdata[["minloadcrit"]]) {
          cat(paste0("\nnew calibration error: ",cal.error.end, " g"))
          cat(paste0("\nnpoints around sphere: ", npoints))
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
    spheredata = data.frame(A = meta_temp, stringsAsFactors = TRUE)
    if (use.temp == TRUE) {
      names(spheredata) = c("Euclidean Norm","meanx","meany","meanz","sdx","sdy","sdz","temperature")
    } else {
      names(spheredata) = c("Euclidean Norm","meanx","meany","meanz","sdx","sdy","sdz")
    }
  } else {
    spheredata = c()
  }
  rm(meta_temp)
  QCmessage = QC
  if (params_rawdata[["printsummary"]] == TRUE) {
    cat("\nSummary of autocalibration procedure:")
    cat("\n")
    cat(paste0("\nStatus: ",QCmessage))
    cat(paste0("\nCalibration error (g) before: ", cal.error.start))
    cat(paste0("\nCalibration error (g) after: ", cal.error.end))
    cat(paste0("\nOffset correction ",c("x","y","z"),": ", offset))
    cat(paste0("\nScale correction ",c("x","y","z"),": ", scale))
    cat(paste0("\nNumber of hours used: ",nhoursused))
    cat(paste0("\nNumber of 10 second windows around the sphere: ", npoints))
    cat(paste0("\nTemperature used (if available): ", use.temp))
    cat(paste0("\nTemperature offset (if temperature is available) ", c("x", "y", "z"),": ", tempoffset))
    cat("\n")
  }
  if (use.temp == TRUE) {
    if (length(spheredata) > 0) {
      meantempcal = mean(spheredata[,8], na.rm = TRUE)
    } else {
      meantempcal = c()
    }
  } else {
    meantempcal = c()
  }
  invisible(list(scale = scale, offset = offset, tempoffset = tempoffset,
                 cal.error.start = cal.error.start, cal.error.end = cal.error.end,
                 spheredata = spheredata, npoints = npoints, nhoursused = nhoursused,
                 QCmessage = QCmessage, use.temp = use.temp, meantempcal = meantempcal, bsc_qc = bsc_qc))
}
