read.myacc.csv = function(rmc.file=c(), rmc.nrow=Inf, rmc.skip=c(), rmc.dec=".",
                          rmc.firstrow.acc = 1, rmc.firstrow.header=c(),
                          rmc.header.length = c(),
                          rmc.col.acc = 1:3, rmc.col.temp = c(), rmc.col.time=c(),
                          rmc.unit.acc = "g", rmc.unit.temp = "C", 
                          rmc.unit.time = "POSIX",
                          rmc.format.time = "%Y-%m-%d %H:%M:%OS",
                          rmc.bitrate = c(), rmc.dynamic_range = c(),
                          rmc.unsignedbit = TRUE,
                          rmc.origin = "1970-01-01",
                          rmc.desiredtz = NULL,
                          rmc.configtz = NULL,
                          rmc.sf = c(),
                          rmc.headername.sf = c(),
                          rmc.headername.sn = c(),
                          rmc.headername.recordingid = c(),
                          rmc.header.structure = c(),
                          rmc.check4timegaps = FALSE,
                          rmc.col.wear = c(),
                          rmc.doresample=FALSE,
                          interpolationType=1,
                          PreviousLastValue = c(0, 0, 1),
                          PreviousLastTime = NULL,
                          epochsize = NULL,
                          desiredtz = NULL,
                          configtz = NULL) {


  if (!is.null(rmc.desiredtz) | !is.null(rmc.configtz)) {
    generalWarning = paste0("Argument rmc.desiredtz and rmc.configtz are scheduled to be deprecated",
                   " and will be replaced by the existing arguments desiredtz and configtz, respectively.")
    showGeneralWarning = TRUE
    # Check if both types of tz are provided:
    if (!is.null(desiredtz) & !is.null(rmc.desiredtz)) {
      if (rmc.desiredtz != desiredtz) { # if different --> error (don't know which one to use)
        showGeneralWarning = FALSE
        stop(paste0("\n", generalWarning, "Please, specify only desiredtz and set ",
             "rmc.desiredtz to NULL to ensure it is no longer used."))
      }
    }
    if (!is.null(configtz) & !is.null(rmc.configtz)) { # then both provided 
      if (rmc.configtz != configtz) { # if different --> error (don't know which one to use)
        showGeneralWarning = FALSE
        stop(paste0("\n", generalWarning, "Please, specify only configtz and set ",
             "rmc.configtz to NULL to ensure it is no longer used."))
      }
    }
    if (showGeneralWarning == TRUE) {
      warning(paste0("\n", generalWarning))
    }

    # Until deprecation still allow rmc. to be used, 
    # so us it to overwrite normal tz in this function:
    if (is.null(desiredtz)) desiredtz = rmc.desiredtz 
    if (is.null(configtz)) configtz = rmc.configtz
   
  }
  # check if none of desiredtz and rmc.desiredtz are provided
  if (is.null(desiredtz) & is.null(rmc.desiredtz)) {
    stop(paste0("Timezone not specified, please provide at least desiredtz",
                " and consider specifying configtz."))
  }
  
  
  # bitrate should be or header item name as character, or the actual numeric bit rate
  # unit.temp can take C(elsius), F(ahrenheit), and K(elvin) and converts it into Celsius
  # Note all argument names start with rmc (read myacc csv) to avoid name clashes when passed on throughout GGIR
  if (length(rmc.firstrow.header) == 0) { # no header block
    if (rmc.firstrow.acc == 2) {
      freadheader = TRUE
    } else {
      freadheader = FALSE
    }
    skip = rmc.firstrow.acc
    sf = rmc.sf
    header = "no header"
  } else {
    # extract header information:
    if (length(rmc.header.length) == 0) {
      rmc.header.length = rmc.firstrow.acc - 1
    }
    
    options(warn = -1) # fread complains about quote in first row for some file types
    header_tmp = as.data.frame(data.table::fread(file = rmc.file,
                                                 nrows = rmc.header.length, 
                                                 skip = rmc.firstrow.header - 1,
                                                 dec = rmc.dec, showProgress = FALSE, header = FALSE,
                                                 stringsAsFactors = TRUE,
                                                 blank.lines.skip = TRUE)) 
    options(warn = 0)
    if (length(rmc.header.structure) != 0) { # header is stored in 1 column, with strings that need to be split
      if (length(header_tmp) == 1) { # one header item
        header_tmp = as.matrix(unlist(strsplit(as.character(header_tmp[,1]), rmc.header.structure)))
      } else { # multiple header items
        mysplit = function(x){
          tmp = strsplit(as.character(x), rmc.header.structure)
          tmp = unlist(tmp)
          return(tmp)
        }
        header_tmp0 = header_tmp
        header_tmp = unlist(lapply(header_tmp[,1], FUN = mysplit))
        if (length(header_tmp) > 2) {
          header_tmp = data.frame(matrix(unlist(header_tmp), nrow = nrow(header_tmp0), byrow = T), stringsAsFactors = TRUE)
          colnames(header_tmp) = NULL
        } else {
          header_tmp = data.frame(matrix(unlist(header_tmp), nrow = 1, byrow = T), stringsAsFactors = TRUE)
          colnames(header_tmp) = NULL
        }
      }
      if (ncol(header_tmp) == 1) header_tmp = t(header_tmp)
      header_tmp2 = as.data.frame(as.character(unlist(header_tmp[,2])), stringsAsFactors = FALSE)
      row.names(header_tmp2) = header_tmp[,1] 
      colnames(header_tmp2) = NULL
      header = header_tmp2
    } else { # column 1 is header name, column 2 is header value
      colnames(header_tmp) = NULL
      validrows = which(is.na(header_tmp[,1]) == FALSE & header_tmp[,1] != "")
      header_tmp = header_tmp[validrows,1:2]
      header_tmp2 = as.data.frame(header_tmp[,2], stringsAsFactors = FALSE)
      row.names(header_tmp2) = header_tmp[,1]
      colnames(header_tmp2) = NULL
      header = header_tmp2
    }
    skip = rmc.firstrow.acc - 1
    freadheader = TRUE
    # assess whether accelerometer data conversion is needed
    if (length(rmc.bitrate) > 0 & length(rmc.dynamic_range) > 0 & rmc.unit.acc == "bit") {
      if (is.character(rmc.bitrate[1]) == TRUE) { # extract bitrate if it is in the header
        rmc.bitrate = as.numeric(header[which(row.names(header) == rmc.bitrate[1]),1])
      }
      if (is.character(rmc.dynamic_range[1]) ==  TRUE) { # extract dynamic range if it is in the header
        rmc.dynamic_range = as.numeric(header[which(row.names(header) == rmc.dynamic_range[1]),1])
      } 
    }
    # extract sample frequency:
    sf = as.numeric(header[which(row.names(header) == rmc.headername.sf[1]),1])
    sn = as.numeric(header[which(row.names(header) == rmc.headername.sn[1]),1])
    ID = as.numeric(header[which(row.names(header) == rmc.headername.recordingid[1]),1])
    
    # standardise key header names to ease use elsewhere in GGIR:
    if (length(rmc.headername.sf) > 0) {
      row.names(header)[which(row.names(header) == rmc.headername.sf[1])] = "sample_rate"
    }
    if (length(rmc.headername.sn) > 0) {
      row.names(header)[which(row.names(header) == rmc.headername.sn[1])] = "device_serial_number"
    }
    if (length(rmc.headername.recordingid) > 0) {
      row.names(header)[which(row.names(header) == rmc.headername.recordingid[1])] = "recordingID"
    }
    if (length(sf) == 0) {
      sf = rmc.sf # if sf not retrieved from header than use default
      header = rbind(header,1) # add it also to the header
      row.names(header)[nrow(header)] = "sample_rate"
    }
  }
  if (length(rmc.skip) > 0) {
    skip = skip + rmc.skip
  }
  # read data from file
  P = as.data.frame(data.table::fread(rmc.file,nrow = rmc.nrow, skip = skip,
                                      dec = rmc.dec, showProgress = FALSE, header = freadheader),
                    stringsAsFactors = TRUE)
  if (length(configtz) == 0) {
    configtz = desiredtz
  }
  if (length(rmc.col.wear) > 0) {
    wearIndicator = P[, rmc.col.wear] # keep wear channel seperately and reinsert at the end
  }
  # select relevant columns, add standard column names
  P = P[,c(rmc.col.time, rmc.col.acc, rmc.col.temp)]
  if (length(rmc.col.time) > 0 & length(rmc.col.temp) > 0) {
    colnames(P) = c("timestamp","accx","accy","accz","temperature")
  } else if (length(rmc.col.time) > 0 & length(rmc.col.temp) == 0) {
    colnames(P) = c("timestamp","accx","accy","accz")
  } else if (length(rmc.col.time) == 0 & length(rmc.col.temp) > 0) {
    colnames(P) = c("accx","accy","accz","temperature")
  } else if (length(rmc.col.time) == 0 & length(rmc.col.temp) == 0) {
    colnames(P) = c("accx","accy","accz")
  }
  # acceleration and temperature as numeric
  P$accx = as.numeric(P$accx)
  P$accy = as.numeric(P$accy)
  P$accz = as.numeric(P$accz)
  if (length(rmc.col.temp) > 0) P$temperature = as.numeric(P$temperature) 
  # Convert timestamps
  if (length(rmc.col.time) > 0) {
    if (rmc.unit.time == "POSIX") {
      P$timestamp = as.POSIXlt(format(P$timestamp), origin = rmc.origin, tz = configtz, format = rmc.format.time)

      checkdec = function(x) {
        # function to check whether timestamp has decimal places
        return(length(unlist(strsplit(as.character(x), "[.]|[,]"))) == 1)
      }
      first_chunk_time = P$timestamp[1:pmin(nrow(P), 1000)]
      checkMissingDecPlaces = unlist(lapply(first_chunk_time, FUN = checkdec))
      if (all(checkMissingDecPlaces) &
          !is.null(rmc.sf) &
          length(which(duplicated(first_chunk_time) == TRUE)) > 0) {
        # decimal places are not present and there are duplicated timestamps,
        # so insert decimal places
        #-----
        # dummy data, to test the following code:
        # ttt = as.POSIXlt("2022-11-02 14:46:50", tz = "Europe/Amsterdam")
        # rmc.sf = 10
        # P = data.frame(timestamps = c(rep(ttt - 1, 3), rep(ttt, 10), rep(ttt + 1, 9), rep(ttt + 2, 10), rep(ttt + 3, 4)))
        #------
        trans = unique(c(1, which(diff(P$timestamp) > 0), nrow(P)))
        sf_tmp = diff(trans)
        timeIncrement = seq(0, 1 - (1/rmc.sf), by = 1/rmc.sf) # expected time increment per second
        
        # All seconds with exactly the sample frequency
        trans_1 = trans[which(sf_tmp == rmc.sf)]
        indices_1 = sort(unlist(lapply(trans_1, FUN = function(x){x + (1:rmc.sf)})))
        P$timestamp[indices_1] =  P$timestamp[indices_1] + rep(timeIncrement, length(trans_1))
        
        # First second
        if (sf_tmp[1] != rmc.sf) {
          indices_2 = 1:trans[2]
          P$timestamp[indices_2] = P$timestamp[indices_2] + seq(1 - (trans[2]/rmc.sf), 1 - (1/rmc.sf), by = 1/rmc.sf)
        }
        # Last second
        if (sf_tmp[length(sf_tmp)] != rmc.sf) {
          indices_3 = (trans[length(trans)-1] + 1):trans[length(trans)]
          P$timestamp[indices_3] = P$timestamp[indices_3] + timeIncrement[1:length(indices_3)]
        }
        # All seconds with other sample frequency and not the first or last second
        # This code now assumes that most samples in the second were sampled
        # at the correct rate but that some samples were dropped or doubled
        # as a result of which the sample rate appears different
        # It seems that this may be a safer assumption than the assumption
        # that the entire second had a different sample rate
        if (length(trans) > 4) {
          trans_cut = trans[2:(length(trans)-1)]
          sf_tmp_cut = sf_tmp[2:(length(sf_tmp)-1)]
          sf_tmp_odd = unique(sf_tmp_cut[which(sf_tmp_cut != rmc.sf)])
          if (length(sf_tmp_odd) > 0) {
            for (ji in 1:length(sf_tmp_odd)) {
              sf2 = sf_tmp_odd[ji]
              trans_4 = trans_cut[which(sf_tmp_cut == sf2)]
              indices_4 = sort(unlist(lapply(trans_4, FUN = function(x){x + (1:sf2)})))
              if (length(timeIncrement) > sf2) {
                timeIncrement2 = timeIncrement[1:sf2]
              } else if (length(timeIncrement) < sf2) {
                timeIncrement2 = c(timeIncrement, rep(timeIncrement[rmc.sf], sf2 - rmc.sf))
              }
              P$timestamp[indices_4] =  P$timestamp[indices_4] + rep(timeIncrement2, length(trans_4))
            }
          }
        }
        # print(diff(P$timestamp))
      }
    } else if (rmc.unit.time == "character") {
      P$timestamp = as.POSIXlt(P$timestamp,format = rmc.format.time, tz = configtz)
    } else if (rmc.unit.time == "UNIXsec") {
      P$timestamp = as.POSIXlt(P$timestamp, origin = rmc.origin, tz = configtz)
    } else if (rmc.unit.time == "ActivPAL") {
      # origin should be specified as: "1899-12-30"
      rmc.origin = "1899-12-30"
      datecode = round(P$timestamp) * 3600*24
      tmp2 = P$timestamp - round(P$timestamp)
      timecode = ((tmp2 * 10^10) * 8.64) / 1000000
      numerictime = datecode + timecode
      P$timestamp = as.POSIXlt(numerictime, origin = rmc.origin, tz = configtz)
    }
    if (length(which(is.na(P$timestamp) == FALSE)) == 0) {
      stop("\nExtraction of timestamps unsuccesful, check timestamp format arguments")
    }
  }
  
  if (configtz != desiredtz) {
    P$timestamp = as.POSIXlt(as.numeric(P$timestamp),
                                 tz = desiredtz, origin = "1970-01-01")
  }
  
  # If acceleration is stored in mg units then convert to gravitational units
  if (rmc.unit.acc == "mg") {
    P$accx = P$accx * 1000
    P$accy = P$accy * 1000
    P$accz = P$accz * 1000
  }
  # If acceleration is stored in bit values then convert to gravitational unit
  if (length(rmc.bitrate) > 0 & length(rmc.dynamic_range) > 0 & rmc.unit.acc == "bit") {
    if (rmc.unsignedbit == TRUE) {
      P$accx = ((P$accx / (2^rmc.bitrate)) - 0.5) * 2 * rmc.dynamic_range
      P$accy = ((P$accy / (2^rmc.bitrate)) - 0.5) * 2 * rmc.dynamic_range
      P$accz = ((P$accz / (2^rmc.bitrate)) - 0.5) * 2 * rmc.dynamic_range
    } else if (rmc.unsignedbit == FALSE) { # signed bit
      P$accx = (P$accx / ((2^rmc.bitrate)/2)) * rmc.dynamic_range
      P$accy = (P$accy / ((2^rmc.bitrate)/2)) * rmc.dynamic_range
      P$accz = (P$accz / ((2^rmc.bitrate)/2)) * rmc.dynamic_range
    }
  }
  # Convert temperature units
  if (rmc.unit.temp == "K") {
    P$temperature = P$temperature + 272.15 # From Kelvin to Celsius
  } else if (rmc.unit.temp == "F") {
    P$temperature = (P$temperature - 32) * (5/9) # From Fahrenheit to Celsius
  }
  if (length(rmc.col.wear) > 0) { # reinsert the nonwear channel
    P$wear = wearIndicator
  }
  # check for jumps in time and impute
  if (rmc.check4timegaps == TRUE) {
    if (length(sf) == 0) { # estimate sample frequency if not given in header
      deltatime = abs(diff(as.numeric(P$timestamp)))
      gapsi = which(deltatime > 0.25)
      sf = (P$timestamp[gapsi[1]] - P$timestamp[1]) / (gapsi[1] - 1)
    }
    P = g.imputeTimegaps(P, xyzCol = c("accx", "accy", "accz"), timeCol = "timestamp", sf = sf, k = 0.25, 
                         PreviousLastValue = PreviousLastValue,
                         PreviousLastTime = PreviousLastTime, epochsize = NULL)
    PreviousLastValue = as.numeric(P[nrow(P), c("accx", "accy", "accz")])
    PreviousLastTime = as.POSIXct(P[nrow(P), "timestamp"])
  }
  if (rmc.doresample == TRUE) { #resample
    rawTime = vector(mode = "numeric", nrow(P))
    rawTime = as.numeric(as.POSIXlt(P$timestamp,tz = configtz))
    rawAccel = as.matrix(P[,-c(which(colnames(P) == "timestamp"))])
    step = 1/sf
    start = rawTime[1]
    end = rawTime[length(rawTime)]
    timeRes = seq(start, end, step)
    nr = length(timeRes) - 1
    timeRes = as.vector(timeRes[1:nr])
    accelRes = matrix(0,nrow = nr, ncol = ncol(rawAccel), dimnames = list(NULL,colnames(rawAccel)))
    rawLast = nrow(rawAccel)
    accelRes = GGIRread::resample(rawAccel, rawTime, timeRes, rawLast, interpolationType) # this is now the resampled acceleration data
    colnamesP = colnames(P)
    timeRes = as.POSIXlt(timeRes, origin = rmc.origin, tz = configtz)
    P = as.data.frame(accelRes, stringsAsFactors = TRUE)
    P$timestamp = timeRes
    P = P[,c(ncol(P),1:(ncol(P) - 1))]
    colnames(P) = colnamesP
    P$timestamp = as.POSIXlt(as.numeric(P$timestamp),
                             tz = desiredtz, origin = "1970-01-01")
  }
  return(list(data = P, header = header, 
              PreviousLastValue = PreviousLastValue,
              PreviousLastTime = PreviousLastTime))
}
