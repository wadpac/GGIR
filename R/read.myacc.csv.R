read.myacc.csv = function(rmc.file=c(), rmc.nrow=Inf, rmc.skip=c(), rmc.dec=".",
                          rmc.firstrow.acc = c(), rmc.firstrow.header=c(),
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
                          rmc.scalefactor.acc = 1,
                          interpolationType = 1,
                          PreviousLastValue = c(0, 0, 1),
                          PreviousLastTime = NULL,
                          desiredtz = NULL,
                          configtz = NULL,
                          header = NULL) {

  if (length(rmc.col.time) > 0 && !(rmc.unit.time %in% c("POSIX", "character", "UNIXsec", "UNIXmsec", "ActivPAL"))) {
    stop(paste0("\nUnrecognized rmc.col.time value. The only accepted values are \"POSIX\", ",
                "\"character\", \"UNIXsec\", \"UNIXmsec\", and \"ActivPAL\"."), call. = FALSE)
  }

  if (!is.null(rmc.desiredtz) || !is.null(rmc.configtz)) {
    generalWarning = paste0("Argument rmc.desiredtz and rmc.configtz are scheduled to be deprecated",
                   " and will be replaced by the existing arguments desiredtz and configtz, respectively.")

    # Check if both types of tz are provided:
    if (!is.null(desiredtz) && desiredtz != "" && !is.null(rmc.desiredtz)) {
      if (rmc.desiredtz != desiredtz) { # if different --> error (don't know which one to use)
        stop(paste0("\n", generalWarning, "Please, specify only desiredtz and set ",
             "rmc.desiredtz to NULL to ensure it is no longer used."))
      }
    }
    if (!is.null(configtz) && !is.null(rmc.configtz)) { # then both provided 
      if (rmc.configtz != configtz) { # if different --> error (don't know which one to use)
        stop(paste0("\n", generalWarning, "Please, specify only configtz and set ",
             "rmc.configtz to NULL to ensure it is no longer used."))
      }
    }
    warning(paste0("\n", generalWarning))

    # Until deprecation still allow rmc. to be used, 
    # so use it to overwrite normal tz in this function:
    if (is.null(desiredtz)) desiredtz = rmc.desiredtz 
    if (desiredtz == "" && !is.null(rmc.desiredtz)) desiredtz = rmc.desiredtz
    if (is.null(configtz)) configtz = rmc.configtz
   
  }
  # check if none of desiredtz and rmc.desiredtz are provided
  if (is.null(desiredtz) && is.null(rmc.desiredtz)) {
    stop(paste0("Timezone not specified, please provide at least desiredtz",
                " and consider specifying configtz."))
  }
  
  if (is.null(rmc.firstrow.acc) || rmc.firstrow.acc < 1) {
    stop(paste0("\nParameter rmc.firstrow.acc always need to be specified ",
                "when working with ad-hoc csv format data"))
  }
  skip = rmc.firstrow.acc - 1
  if (!is.null(rmc.skip) && length(rmc.skip) > 0) {
    skip = skip + rmc.skip
  }

  # only extract the header if it hasn't been extracted for this file before
  if (is.null(header)) {
    # bitrate should be either header item name as character, or the actual numeric bit rate.
    # unit.temp can take C(elsius), F(ahrenheit), and K(elvin) and converts it into Celsius
    # Note all argument names start with rmc (read myacc csv) to avoid name clashes when passed on throughout GGIR
    if (length(rmc.firstrow.header) == 0) { # no header block
      sf = rmc.sf
      header = "no header"
    } else {
      # extract header information:
      if (length(rmc.header.length) == 0) {
        rmc.header.length = rmc.firstrow.acc - 1
      }
      
      options(warn = -1) # fread complains about quote in first row for some file types
      header_tmp = data.table::fread(file = rmc.file,
                                     nrows = rmc.header.length, 
                                     skip = rmc.firstrow.header - 1,
                                     dec = rmc.dec, showProgress = FALSE, header = FALSE,
                                     blank.lines.skip = TRUE,
                                     data.table=FALSE, stringsAsFactors=FALSE)
      validrows = which(is.na(header_tmp[,1]) == FALSE & header_tmp[,1] != "")
      header_tmp = header_tmp[validrows,1:2]
      
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
            header_tmp = data.frame(matrix(unlist(header_tmp), nrow = nrow(header_tmp0), byrow = T), stringsAsFactors = FALSE)
            colnames(header_tmp) = NULL
          } else {
            header_tmp = data.frame(matrix(unlist(header_tmp), nrow = 1, byrow = T), stringsAsFactors = FALSE)
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
        header_tmp2 = as.data.frame(header_tmp[,2], stringsAsFactors = FALSE)
        row.names(header_tmp2) = header_tmp[,1]
        colnames(header_tmp2) = NULL
        header = header_tmp2
      }
      # assess whether accelerometer data conversion is needed
      if (length(rmc.bitrate) > 0 && length(rmc.dynamic_range) > 0 && rmc.unit.acc == "bit") {
        if (is.character(rmc.bitrate[1]) == TRUE) { # extract bitrate if it is in the header
          rmc.bitrate = as.numeric(header[which(row.names(header) == rmc.bitrate[1]),1])
        }
        if (is.character(rmc.dynamic_range[1]) ==  TRUE) { # extract dynamic range if it is in the header
          rmc.dynamic_range = as.numeric(header[which(row.names(header) == rmc.dynamic_range[1]),1])
        } 
      }
      # extract sample frequency:
      sf = as.numeric(header[which(row.names(header) == rmc.headername.sf[1]),1])
      
      if (is.na(sf)) { # sf not retrieved from header
        # first see if maybe sf *is* in the header, just not under the rmc.headername.sf name
        sf = as.numeric(header[which(row.names(header) == "sample_rate"),1])
        # if sf isn't in the header under the default name either, then use the default value
        if (is.na(sf)) {
          sf = rmc.sf # this could be null, that's fine. At least we can only end up with a null, not either null or NA
          if (!is.null(sf)) {
            header = rbind(header, sf) # also add it to the header
            row.names(header)[nrow(header)] = "sample_rate"
          }
        }
      }

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
    }
  }
  # read data from file
  P = data.table::fread(rmc.file, nrows = rmc.nrow, skip = skip,
                        dec = rmc.dec, showProgress = FALSE, header = "auto",
                        data.table=FALSE, stringsAsFactors=FALSE)
  
  if (length(configtz) == 0) {
    configtz = desiredtz
  }
  if (length(rmc.col.wear) > 0) {
    wearIndicator = P[, rmc.col.wear] # keep wear channel seperately and reinsert at the end
  }
  # select relevant columns, add standard column names
  P = P[,c(rmc.col.time, rmc.col.acc, rmc.col.temp)]
  if (length(rmc.col.time) > 0 && length(rmc.col.temp) > 0) {
    colnames(P) = c("time","x","y","z","temperature")
  } else if (length(rmc.col.time) > 0 && length(rmc.col.temp) == 0) {
    colnames(P) = c("time","x","y","z")
  } else if (length(rmc.col.time) == 0 && length(rmc.col.temp) > 0) {
    colnames(P) = c("x","y","z","temperature")
  } else if (length(rmc.col.time) == 0 && length(rmc.col.temp) == 0) {
    colnames(P) = c("x","y","z")
  }
  # acceleration and temperature as numeric
  P$x = as.numeric(P$x)
  P$y = as.numeric(P$y)
  P$z = as.numeric(P$z)
  if (length(rmc.col.temp) > 0) P$temperature = as.numeric(P$temperature) 
  # Convert timestamps
  if (length(rmc.col.time) > 0) {
    if (rmc.unit.time == "POSIX") {
      P$time = as.POSIXct(format(P$time), origin = rmc.origin, tz = configtz, format = rmc.format.time)
      checkdec = function(x) {
        # function to check whether timestamp has decimal places
        return(length(unlist(strsplit(as.character(x), "[.]|[,]"))) == 1)
      }
      first_chunk_time = P$time[1:pmin(nrow(P), 1000)]
      checkMissingDecPlaces = unlist(lapply(first_chunk_time, FUN = checkdec))
      if (all(checkMissingDecPlaces) &&
          !is.null(sf) && sf != 0 &&
          length(which(duplicated(first_chunk_time) == TRUE)) > 0) {
        # decimal places are not present and there are duplicated timestamps,
        # so insert decimal places
        #-----
        # dummy data, to test the following code:
        # ttt = as.POSIXlt("2022-11-02 14:46:50", tz = "Europe/Amsterdam")
        # sf = 10
        # P = data.frame(timestamps = c(rep(ttt - 1, 3), rep(ttt, 10), rep(ttt + 1, 9), rep(ttt + 2, 10), rep(ttt + 3, 4)))
        #------
        trans = unique(c(1, which(diff(P$time) > 0), nrow(P)))
        sf_tmp = diff(trans)
        timeIncrement = seq(from = 0, length.out = sf, by = 1/sf) # expected time increment per second
        
        # All seconds with exactly the sample frequency
        trans_1 = trans[which(sf_tmp == sf)]
        indices_1 = sort(unlist(lapply(trans_1, FUN = function(x){x + (1:sf)})))
        P$time[indices_1] =  P$time[indices_1] + rep(timeIncrement, length(trans_1))
        # First second
        if (sf_tmp[1] != sf) {
          indices_2 = 1:trans[2]
          P$time[indices_2] = P$time[indices_2] + seq(1 - (trans[2]/sf), 1 - 1/sf, by = 1/sf)
        }
        # Last second
        if (sf_tmp[length(sf_tmp)] != sf) {
          indices_3 = (trans[length(trans)-1] + 1):trans[length(trans)]
          P$time[indices_3] = P$time[indices_3] + timeIncrement[1:length(indices_3)]
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
          sf_tmp_odd = unique(sf_tmp_cut[which(sf_tmp_cut != sf)])
          if (length(sf_tmp_odd) > 0) {
            for (ji in 1:length(sf_tmp_odd)) {
              sf2 = sf_tmp_odd[ji]
              trans_4 = trans_cut[which(sf_tmp_cut == sf2)]
              indices_4 = sort(unlist(lapply(trans_4, FUN = function(x){x + (1:sf2)})))
              if (length(timeIncrement) > sf2) {
                timeIncrement2 = timeIncrement[1:sf2]
              } else if (length(timeIncrement) < sf2) {
                timeIncrement2 = c(timeIncrement, rep(timeIncrement[sf], sf2 - sf))
              }
              P$time[indices_4] =  P$time[indices_4] + rep(timeIncrement2, length(trans_4))
            }
          }
        }
      }
    } else if (rmc.unit.time == "character") {
      P$time = as.POSIXct(P$time, format = rmc.format.time, origin = rmc.origin, tz = configtz)
    } else if (rmc.unit.time == "UNIXsec" || rmc.unit.time == "UNIXmsec") {
      if (rmc.unit.time == "UNIXmsec") {
        P$time = P$time / 1000
      }
      if (rmc.origin != "1970-01-01") {
        P$time = as.POSIXct(P$time, origin = rmc.origin, tz = desiredtz)
      }
    } else if (rmc.unit.time == "ActivPAL") {
      # origin should be specified as: "1899-12-30"
      P$time = lubridate::force_tz(as.POSIXct(P$time * 86400, origin = "1899-12-30", tz = "UTC"), tz = desiredtz)
    }
    if (length(which(is.na(P$time) == FALSE)) == 0) {
      stop("\nExtraction of timestamps unsuccesful, check timestamp format arguments")
    }
    if (!is.numeric(P$time)) { # we'll return Unix timestamps
      P$time = as.numeric(P$time)
    }
  }
  
  # If acceleration is stored in mg units then convert to gravitational units
  if (rmc.unit.acc == "mg") {
    P$x = P$x / 1000
    P$y = P$y / 1000
    P$z = P$z / 1000
  }
  if (rmc.scalefactor.acc != 1) {
    P$x = P$x * rmc.scalefactor.acc
    P$y = P$y * rmc.scalefactor.acc
    P$z = P$z * rmc.scalefactor.acc
  }
  # If acceleration is stored in bit values then convert to gravitational unit
  if (length(rmc.bitrate) > 0 && length(rmc.dynamic_range) > 0 && rmc.unit.acc == "bit") {
    if (rmc.unsignedbit == TRUE) {
      P$x = ((P$x / (2^rmc.bitrate)) - 0.5) * 2 * rmc.dynamic_range
      P$y = ((P$y / (2^rmc.bitrate)) - 0.5) * 2 * rmc.dynamic_range
      P$z = ((P$z / (2^rmc.bitrate)) - 0.5) * 2 * rmc.dynamic_range
    } else if (rmc.unsignedbit == FALSE) { # signed bit
      P$x = (P$x / ((2^rmc.bitrate)/2)) * rmc.dynamic_range
      P$y = (P$y / ((2^rmc.bitrate)/2)) * rmc.dynamic_range
      P$z = (P$z / ((2^rmc.bitrate)/2)) * rmc.dynamic_range
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
  if (rmc.check4timegaps == TRUE && ("time" %in% colnames(P))) {
    sfBackup = sf
    if (is.null(sf) || sf == 0) { # estimate sample frequency if not given in header
      deltatime = abs(diff(as.numeric(P$time)))
      gapsi = which(deltatime > 0.25)
      sf = (P$time[gapsi[1]] - P$time[1]) / (gapsi[1] - 1)
    }
    P = g.imputeTimegaps(P, sf = sf, k = 0.25, 
                         PreviousLastValue = PreviousLastValue,
                         PreviousLastTime = PreviousLastTime, epochsize = NULL)
    sf = sfBackup
    P = P$x
    PreviousLastValue = P[nrow(P), c("x", "y", "z")]
    PreviousLastTime = as.POSIXct(P[nrow(P), "time"], origin = "1970-01-01")
  }
  if (rmc.doresample == TRUE && ("time" %in% colnames(P)) && !is.null(sf) && sf != 0) { # resample
    rawTime = P$time
    rawAccel = as.matrix(P[,-c(which(colnames(P) == "time"))])
    timeRes = seq(from = rawTime[1], to = rawTime[length(rawTime)], by = 1/sf)
    accelRes = GGIRread::resample(rawAccel, rawTime, timeRes, nrow(rawAccel), interpolationType) # this is now the resampled acceleration data
    colnamesP = colnames(P)[-which(colnames(P) == "time")]
    P = as.data.frame(accelRes, stringsAsFactors = FALSE)
    colnames(P) = colnamesP
    P$time = timeRes
  }
  return(list(data = P, header = header, 
              PreviousLastValue = PreviousLastValue,
              PreviousLastTime = PreviousLastTime))
}
