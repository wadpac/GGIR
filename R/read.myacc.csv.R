read.myacc.csv = function(rmc.file=c(), rmc.nrow=c(), rmc.skip=c(), rmc.dec=".",
                          rmc.firstrow.acc = 1, rmc.firstrow.header=c(),
                          rmc.header.length = c(),
                          rmc.col.acc = 1:3, rmc.col.temp = c(), rmc.col.time=c(),
                          rmc.unit.acc = "g", rmc.unit.temp = "C", 
                          rmc.unit.time = "POSIX",
                          rmc.format.time = "%Y-%m-%d %H:%M:%OS",
                          rmc.bitrate = c(), rmc.dynamic_range = c(),
                          rmc.unsignedbit = TRUE,
                          rmc.origin = "1970-01-01",
                          rmc.desiredtz = "Europe/London", rmc.sf = c(),
                          rmc.headername.sf = c(),
                          rmc.headername.sn = c(),
                          rmc.headername.recordingid = c(),
                          rmc.header.structure = c(),
                          rmc.check4timegaps = FALSE,
                          rmc.col.wear = c(),
                          rmc.doresample=FALSE) {
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
      rmc.header.length = rmc.firstrow.acc-1
    }
    
    options(warn=-1) # fread complains about quote in first row for some file types
    header_tmp = as.data.frame(data.table::fread(file = rmc.file,
                                                 nrow = rmc.header.length, 
                                                 skip=rmc.firstrow.header-1,
                                                 dec=rmc.dec, showProgress = FALSE, header = FALSE,
                                                 stringsAsFactors = TRUE))
    options(warn=0)
    if (length(rmc.header.structure) != 0) { # header is stored in 1 column, with strings that need to be split
      if (length(header_tmp) == 1) { # one header item
        header_tmp = as.matrix(unlist(strsplit(as.character(header_tmp[,1]), rmc.header.structure)))
      } else { # multiple header items
        if (ncol(header_tmp) > 1) {
          # collapse columns to one
          for (i in 1:length(header_tmp)) { "remove quotes in character"
            header_tmp[i] = gsub(pattern = "\"",replacement = "",x = header_tmp[i])
          }
          header_tmp = do.call(paste0, header_tmp)
        }
        mysplit = function(x){
          tmp = strsplit(as.character(x), rmc.header.structure)
          tmp = unlist(tmp)
          return(tmp)
        }
        header_tmp0 = header_tmp
        header_tmp = unlist(lapply(header_tmp, FUN=mysplit))
        if (length(header_tmp) > 2) {
          header_tmp = data.frame(matrix(unlist(header_tmp), nrow=nrow(header_tmp0), byrow=T))
        } else {
          header_tmp = data.frame(matrix(unlist(header_tmp), nrow=1, byrow=T))
          colnames(header_tmp) = NULL
        }
      }
      if (ncol(header_tmp) == 1) header_tmp = t(header_tmp)
      header_tmp2 = data.frame(a = header_tmp[,2])
      colnames(header_tmp2) = NULL
      row.names(header_tmp2) = header_tmp[,1] 
      header = header_tmp2
    } else { # column 1 is header name, column 2 is header value
      colnames(header_tmp) = NULL
      validrows = which(is.na(header_tmp[,1]) == FALSE & header_tmp[,1] != "")
      header_tmp = header_tmp[validrows,1:2]
      header_tmp2 = as.data.frame(header_tmp[,2])
      row.names(header_tmp2) = header_tmp[,1]
      colnames(header_tmp2) = NULL
      header = header_tmp2
    }
    skip = rmc.firstrow.acc-1
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
  P = as.data.frame(data.table::fread(rmc.file,nrow = rmc.nrow, skip=skip,
                                      dec=rmc.dec, showProgress = FALSE, header = freadheader))
  
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
      P$timestamp = as.POSIXlt(P$timestamp, origin=rmc.origin,tz = rmc.desiredtz, format = rmc.format.time)
    } else if (rmc.unit.time == "character") {
      P$timestamp = as.POSIXlt(P$timestamp,format= rmc.format.time,tz = rmc.desiredtz)
    } else if (rmc.unit.time == "UNIXsec") {
      P$timestamp = as.POSIXlt(P$timestamp, origin=rmc.origin,tz = rmc.desiredtz)
    } else if (rmc.unit.time == "ActivPAL") {
      # origin should be specified as: "1899-12-30"
      rmc.origin = "1899-12-30"
      datecode = round(P$timestamp) * 3600*24
      tmp2 = P$timestamp - round(P$timestamp)
      timecode = ((tmp2 * 10^10)*8.64) / 1000000
      numerictime = datecode +timecode
      P$timestamp= as.POSIXlt(numerictime,origin=rmc.origin,tz=rmc.desiredtz)
    }
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
      P$accx = ((P$accx / (2^rmc.bitrate)) - 0.5) * 2* rmc.dynamic_range
      P$accy = ((P$accy / (2^rmc.bitrate)) - 0.5) * 2* rmc.dynamic_range
      P$accz = ((P$accz / (2^rmc.bitrate)) - 0.5) * 2* rmc.dynamic_range
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
    deltatime = abs(diff(P$timestamp))
    gapsi = which(deltatime > 1) # gaps indices
    newP = c()
    if (length(gapsi) > 0) { # if gaps exist
      if (length(sf) == 0) { # estimate sample frequency if not given in header
        sf = (P$timestamp[gapsi[jk]] - P$timestamp[1]) / (gapsi[1]-1)
      }
      newP = rbind(newP,P[1:gapsi[1],])
      NumberOfGaps = length(gapsi)
      for (jk in 1:NumberOfGaps) { # fill up gaps
        dt = P$timestamp[gapsi[jk]+1] - P$timestamp[gapsi[jk]] # difference in time
        newblock = as.data.frame(matrix(0,dt*sf,ncol(P)))
        colnames(newblock) = colnames(P)
        seqi = seq(P$timestamp[gapsi[jk]],P$timestamp[gapsi[jk]+1] - (1/sf),by=1/sf)
        if (length(seqi) >= length(newblock$timestamp)) {
          newblock$timestamp = seqi[1:length(newblock$timestamp)]
        }
        newP = rbind(newP, newblock)
        if (jk != NumberOfGaps) {
          newP = rbind(newP,P[((gapsi[jk]+1):gapsi[jk+1]),])
        } else {
          newP = rbind(newP,P[((gapsi[jk]+1):nrow(P)),]) # last block
        }
      }
      P = newP
    }
  }
  if (rmc.doresample == TRUE) { #resample
    rawTime = vector(mode = "numeric", nrow(P))
    rawTime = as.numeric(as.POSIXlt(P$timestamp,tz = rmc.desiredtz))
    rawAccel = as.matrix(P[,-c(which(colnames(P)=="timestamp"))])
    step = 1/sf
    start = rawTime[1]
    end = rawTime[length(rawTime)]
    timeRes = seq(start, end, step)
    nr = length(timeRes) - 1
    timeRes = as.vector(timeRes[1:nr])
    accelRes = matrix(0,nrow = nr, ncol = ncol(rawAccel), dimnames = list(NULL,colnames(rawAccel)))
    rawLast = nrow(rawAccel)
    accelRes = resample(rawAccel, rawTime, timeRes, rawLast) # this is now the resampled acceleration data
    colnamesP = colnames(P)
    timeRes = as.POSIXlt(timeRes, origin=rmc.origin, tz = rmc.desiredtz)
    P = as.data.frame(accelRes)
    P$timestamp = timeRes
    P = P[,c(ncol(P),1:(ncol(P)-1))]
    colnames(P) = colnamesP
  }
  return(list(data=P,header=header))
}