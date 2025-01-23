g.calibrate = function(datafile, params_rawdata = c(),
                       params_general = c(),
                       params_cleaning = c(),
                       inspectfileobject = c(),
                       verbose = TRUE,
                       ...) {

  #get input variables
  input = list(...)
  if (length(input) > 0 ||
      length(params_rawdata) == 0 || length(params_general) == 0 || length(params_cleaning) == 0) {
    # Extract and check parameters if user provides more arguments than just the parameter arguments,
    # or if params_[...] aren't specified (so need to be filled with defaults).
    # So, inside GGIR this will not be used, but it is used when g.calibrate is used on its own
    # as if it was still the old g.calibrate function
    params = extract_params(params_rawdata = params_rawdata,
                            params_cleaning = params_cleaning,
                            params_general = params_general,
                            input = input,
                            params2check = c("rawdata", "cleaning", "general")) # load default parameters
    params_rawdata = params$params_rawdata
    params_cleaning = params$params_cleaning
    params_general = params$params_general
  }
  #-----------------
  # Define local functions:
  rollMean = function(x, fs, epochSize) {
    x = cumsum(c(0, x))
    select = seq(1, length(x), by = fs * epochSize)
    y = diff(x[round(select)]) / abs(diff(round(select)))
    return(y)
  }
  rollSD = function(x, sf, epochSize) {
    dim(x) = c(sf * epochSize, ceiling(length(x) / (sf * epochSize)))
    y = apply(x, 2, sd)
    return(y)
  }
  #-----------------
  use.temp = temp.available = TRUE
  
  filequality = data.frame(filetooshort = FALSE, filecorrupt = FALSE,
                           filedoesnotholdday = FALSE, stringsAsFactors = FALSE)
  calibEpochSize = 10 # epoch for recalibration as used in the 2014 paper
  blockResolution = 3600 # resolution at which raw data is loaded and processed (seconds)
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
  if (length(inspectfileobject) > 0) {
    INFI = inspectfileobject
  } else {
    stop("argument inspectfileobject not specified")
  }
  mon = INFI$monc
  if (mon == MONITOR$VERISENSE) mon = MONITOR$ACTIGRAPH
  dformat = INFI$dformc
  sf = INFI$sf
  
  if (is.null(sf)) {
    # If function g.inspectfile which produces the inspectfileobject
    # identifies a corrupt GT3X file then it sets the sf value to NULL
    # this is then used here to skip the calibration procedure
    return()
  }
  
  #creating matrices for storing output
  S = matrix(0,0,4) #dummy variable needed to cope with head-tailing succeeding blocks of data
  NR = ceiling((90*10^6) / (sf*calibEpochSize)) + 1000 #NR = number of rows to initialise features matrix with

  # setting size of blocks that are loaded (too low slows down the process)
  # the setting below loads blocks size of 12 hours (modify if causing memory problems)
  blocksize = round((14512 * (sf/50)) * (params_rawdata[["chunksize"]]*0.5))
  if (mon == MONITOR$MOVISENS) blocksize = (sf * 60 * 1440) / 2   #Around 12 hours of data for movisens
  if (mon == MONITOR$ACTIGRAPH && dformat == FORMAT$GT3X) blocksize = (12 * 3600) * params_rawdata[["chunksize"]]
  if (mon == MONITOR$AXIVITY && dformat == FORMAT$CWA) {
    if (utils::packageVersion("GGIRread") >= "0.3.1") {
      # CWA data blocks can have 40, 80 or 120 samples each; we'll take 80 as the average number
      blocksize = round(12 * 3600 * sf / 80 * params_rawdata[["chunksize"]])
    }
  }
  if (mon == MONITOR$PARMAY_MTX && dformat == FORMAT$BIN) {
    # Matrix data packets have ~2 minutes of data
    blocksize = round(60 * 12 / 2 * params_rawdata[["chunksize"]])
  }
  #===============================================
  # Read file
  i = 1 #counter to keep track of which binary block is being read
  count = 1 #counter to keep track of the number of seconds that have been read
  LD = 2 #dummy variable used to identify end of file and to make the process stop
  isLastBlock = FALSE # dummy variable part of "end of loop mechanism"
  header = NULL
  while (LD > 1) {
    if (verbose == TRUE) {
      if (i == 1) {
        cat(paste("\nLoading chunk: ",i,sep=""))
      } else {
        cat(paste(" ",i,sep=""))
      }
    }
    #try to read data blocks based on monitor type and data format
    options(warn=-1) #turn off warnings (code complains about unequal rowlengths
    accread = g.readaccfile(filename = datafile, blocksize = blocksize, blocknumber = i,
                            filequality = filequality, ws = blockResolution,
                            PreviousEndPage = PreviousEndPage, inspectfileobject = INFI,
                            params_rawdata = params_rawdata, params_general = params_general,
                            header = header)
    header = accread$header
    isLastBlock = accread$isLastBlock
    PreviousEndPage = accread$endpage

    if (i == 1) {
      use.temp = temp.available = ("temperature" %in% colnames(accread$P$data))
      if (use.temp) {
        features = matrix(99999, NR, 8)
      } else {
        features = matrix(99999, NR, 7)
      }
    }

    options(warn = 0) #turn on warnings
    #process data as read from binary file
    if (length(accread$P) > 0) { # would have been set to zero if file was corrupt or empty
      data = as.matrix(accread$P$data[,which(colnames(accread$P$data) %in% c("x", "y", "z", "time", "temperature"))]) # convert to matrix b/c rbind is much faster on matrices
      if (exists("accread")) {
        rm(accread)
      }
      # add leftover data from last time
      if (min(dim(S)) > 1) {
        data = rbind(S,data)
      }
      # remove 0s if ActiGraph csv (idle sleep mode) OR if similar imputation done in ad-hoc csv
      # current ActiGraph csv's are not with zeros but with last observation carried forward
      zeros = c()
      if (!(mon == MONITOR$ACTIGRAPH && dformat == FORMAT$CSV)) {
        zeros = which(data[, "x"] == 0 & data[, "y"] == 0 & data[, "z"] == 0)
      }
      if ((mon == MONITOR$ACTIGRAPH && dformat == FORMAT$CSV) || length(zeros) > 0) {
        data = g.imputeTimegaps(x = as.data.frame(data), sf = sf, impute = FALSE)
        data = as.matrix(data$x)
      }
      LD = nrow(data)
      #store data that could not be used for this block, but will be added to next block
      use = (floor(LD / (blockResolution*sf))) * (blockResolution*sf) #number of datapoint to use
      if (length(use) > 0) {
        if (use > 0) {
          if (use != LD) {
            S = data[(use + 1):LD,] # store leftover data

            # if S is only one row, it gets coersed to a vector, and what we need is a 1-row matrix
            if (is.vector(S)) {
              S = t(S)
            }
          }
          data = data[1:use,]
          LD = nrow(data) #redefine LD because there is less data

          Gx = data[, "x"]
          Gy = data[, "y"]
          Gz = data[, "z"]

          if (use.temp) {
            if (mean(data[1:10, "temperature"], na.rm = TRUE) > 120) {
              warning("\ntemperature ignored for auto-calibration because values are too high\n")
              use.temp = FALSE
            } else if (sd(data[, "temperature"], na.rm = TRUE) < 0.01) {
              warning("\ntemperature ignored for auto-calibration because no variance in values\n")
              use.temp = FALSE
            }
          }
          #=============================================
          #expand 'out' if it is expected to be too short
          if (count > (nrow(features) - (2.5 * (3600/calibEpochSize) * 24))) {
            extension = matrix(99999, ((3600/calibEpochSize) * 24), ncol(features))
            features = rbind(features, extension)
          }
          # mean acceleration for EN, x, y, and z
          EN = sqrt(Gx^2 + Gy^2 + Gz^2)
          EN2 = rollMean(EN, sf, calibEpochSize)
          endCount = count - 1 + length(EN2)
          features[count:endCount, 1] = EN2
          features[count:endCount, 2] = rollMean(Gx, sf, calibEpochSize)
          features[count:endCount, 3] = rollMean(Gy, sf, calibEpochSize)
          features[count:endCount, 4] = rollMean(Gz, sf, calibEpochSize)
          # sd acceleration
          features[count:endCount, 5] = rollSD(Gx, sf, calibEpochSize)
          features[count:endCount, 6] = rollSD(Gy, sf, calibEpochSize)
          features[count:endCount, 7] = rollSD(Gz, sf, calibEpochSize)
          if (use.temp == TRUE) {
            features[count:endCount, 8] = rollMean(data[, "temperature"], sf, calibEpochSize)
          }
          count = endCount + 1 # increasing count, the indicator of how many timesteps (calibEpochSize) have been read
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
    }
    spherepopulated = 0
    if (isLastBlock) {
      LD = 0
    }
    features_temp = data.frame(V = features, stringsAsFactors = FALSE)
    # remove 999999, missing values
    cut = which(features_temp[,1] == 99999 |
                  is.na(features_temp[,4]) == TRUE | is.na(features_temp[,1]) == TRUE)
    if (length(cut) > 0) {
      features_temp = features_temp[-cut,]
    }
    # remove duplicates, e.g. caused by periods of nonwear
    if (nrow(features_temp) > 0) {
      cut = which(rowSums(features_temp[1:(nrow(features_temp) - 1), 2:7] == features_temp[2:nrow(features_temp), 2:7]) == 3)
      if (length(cut) > 0) {
        features_temp = features_temp[-cut,]
      }
    }
    nhoursused = 0
    if (nrow(features_temp) > (params_rawdata[["minloadcrit"]] - 21)) {  # enough data for the sphere?
      nhoursused = (nrow(features_temp) * 10) / 3600
      features_temp = features_temp[-1,]
      #select parts with no movement
      if (mon == MONITOR$AD_HOC) {
        if (length(params_rawdata[["rmc.noise"]]) == 0) {
          warning("Argument rmc.noise not specified, please specify expected noise level in g-units")
        }
        sdcriter = params_rawdata[["rmc.noise"]] * 1.2
        if (length(params_rawdata[["rmc.noise"]]) == 0) {
          stop(paste0("Please provide noise level for the acceleration sensors",
                      " in g-units with argument rmc.noise to aid non-wear detection"),
               call. = FALSE)
        }
      } else {
        sdcriter = 0.013
      }
      nomovement = which(features_temp[,5] < sdcriter & features_temp[,6] < sdcriter & features_temp[,7] < sdcriter &
                           abs(as.numeric(features_temp[,2])) < 2 & abs(as.numeric(features_temp[,3])) < 2 &
                           abs(as.numeric(features_temp[,4])) < 2) #the latter three are to reduce chance of including clipping periods
      if (length(nomovement) < 10) {
        # take only one row to trigger that autocalibration is skipped 
        # with the QCmessage that there is not enough data
        features_temp = features_temp[1, ] 
      } else {
        features_temp = features_temp[nomovement,]
      }
      if (min(dim(features_temp)) > 1) {
        npoints = nrow(features_temp)
        cal.error.start = sqrt(as.numeric(features_temp[,2])^2 + as.numeric(features_temp[,3])^2 + as.numeric(features_temp[,4])^2)
        cal.error.start = round(mean(abs(cal.error.start - 1)), digits = 5)
        #check whether sphere is well populated
        tel = 0
        for (axis in 2:4) {
          if ( min(features_temp[,axis]) < -params_rawdata[["spherecrit"]] & max(features_temp[,axis]) > params_rawdata[["spherecrit"]]) {
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
        QC = "recalibration not done because no non-movement data available"
        features_temp = c()
      }
    } else {
      QC = "recalibration not done because not enough data in the file or because file is corrupt"
    }
    if (spherepopulated == 1) { #only try to improve calibration if there are enough datapoints around the sphere
      #---------------------------------------------------------------------------
      # START of Zhou Fang's code (slightly edited by vtv21 to use matrix features_temp from above
      # instead the similar matrix generated by Zhou Fang's original code. This to allow for
      # more data to be used as features_temp can now be based on 10 or more days of raw data
      input = features_temp[,2:4]
      if (use.temp == TRUE) {
        inputtemp = cbind(as.numeric(features_temp[,8]),as.numeric(features_temp[,8]),as.numeric(features_temp[,8])) #temperature
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
          if ((mon == MONITOR$ACTIGRAPH || mon == MONITOR$MOVISENS) && length(which(is.na(closestpoint[,k, drop = F]) == TRUE)) > 0 &
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
        features_temp2 = scale(as.matrix(features_temp[,2:4]),center = -offset, scale = 1/scale)
      } else {
        yy = as.matrix(cbind(as.numeric(features_temp[,8]),as.numeric(features_temp[,8]),as.numeric(features_temp[,8])))
        features_temp2 = scale(as.matrix(features_temp[,2:4]),center = -offset, scale = 1/scale) +
          scale(yy, center = rep(meantemp,3), scale = 1/tempoffset)
      }     #equals: D2[,axis] = (D[,axis] + offset[axis]) / (1/scale[axis])
      # END of Zhou Fang's code
      #-------------------------------------------
      cal.error.end = sqrt(features_temp2[,1]^2 + features_temp2[,2]^2 + features_temp2[,3]^2)
      rm(features_temp2)
      cal.error.end = round(mean(abs(cal.error.end - 1)), digits = 5)
      # assess whether calibration error has sufficiently been improved
      if (cal.error.end < cal.error.start & cal.error.end < 0.01 & nhoursused > params_rawdata[["minloadcrit"]]) { #do not change scaling if there is no evidence that calibration improves
        if (use.temp == temp.available) {
          QC = "recalibration done, no problems detected"
        } else {
          QC = "recalibration done, but temperature values not used"
        }
        LD = 0 #stop loading
      } else {
        #continue loading data
        if (nhoursused > params_rawdata[["minloadcrit"]]) {
          if (verbose == TRUE) {
            cat(paste0("\nnew calibration error: ",cal.error.end, " g"))
            cat(paste0("\nnpoints around sphere: ", npoints))
          }
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
  if (!is.null(features_temp) && all(dim(features_temp)) != 0) {
    spheredata = features_temp
    if (use.temp == TRUE) {
      names(spheredata) = c("Euclidean Norm","meanx","meany","meanz","sdx","sdy","sdz","temperature")
    } else {
      names(spheredata) = c("Euclidean Norm","meanx","meany","meanz","sdx","sdy","sdz")
    }
  } else {
    spheredata = c()
  }
  rm(features_temp)
  QCmessage = QC
  if (params_rawdata[["printsummary"]] == TRUE & verbose == TRUE) {
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
    cat(paste0("\nTemperature offset (if temperature is available) ",
               c("x", "y", "z"),": ", tempoffset))
    cat("\n")
  }
  if (use.temp == TRUE && length(spheredata) > 0) {
      meantempcal = mean(spheredata[,8], na.rm = TRUE)
  } else {
    meantempcal = c()
  }
  invisible(list(scale = scale, offset = offset, tempoffset = tempoffset,
                 cal.error.start = cal.error.start, cal.error.end = cal.error.end,
                 spheredata = spheredata, npoints = npoints, nhoursused = nhoursused,
                 QCmessage = QCmessage, use.temp = use.temp, meantempcal = meantempcal, bsc_qc = bsc_qc))
}
