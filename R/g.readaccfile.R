g.readaccfile = function(filename, blocksize, blocknumber, filequality,
                         ws, PreviousEndPage = 1, inspectfileobject = c(),
                         PreviousLastValue = c(0, 0, 1), PreviousLastTime = NULL,
                         params_rawdata = c(), params_general = c(), header = NULL, ...) {
  #get input variables
  input = list(...)
  if (length(input) > 0 ||
      length(params_rawdata) == 0 || length(params_general) == 0) {
    # Extract and check parameters if user provides more arguments than just the parameter arguments,
    # or if params_[...] aren't specified (so need to be filled with defaults).
    # So, inside GGIR this will not be used, but it is used when g.getmeta is used on its own
    # as if it was still the old g.getmeta function
    params = extract_params(params_rawdata = params_rawdata,
                            params_general = params_general,
                            input = input) # load default parameters
    params_rawdata = params$params_rawdata
    params_general = params$params_general
  }
  
  desiredtz = params_general[["desiredtz"]]
  configtz = params_general[["configtz"]]
  if (length(configtz) == 0) configtz = desiredtz

  I = inspectfileobject
  mon = I$monc
  if (mon == MONITOR$VERISENSE) mon = MONITOR$ACTIGRAPH
  dformat = I$dformc
  sf = I$sf
  decn = I$decn

  if ((mon == MONITOR$ACTIGRAPH && dformat == FORMAT$CSV) ||
      (mon == MONITOR$AXIVITY && dformat == FORMAT$CSV) || 
      dformat == FORMAT$AD_HOC_CSV) {
    blocksize = blocksize * 300
  }

  if (blocknumber < 1) blocknumber = 1

  # startpage should only be specified for blocknumber 1.
  # The next time (blocknumber > 1) the startpage will be derived from the previous
  # endpage and the blocksize.

  if ((mon == MONITOR$GENEACTIV && dformat == FORMAT$BIN) || dformat == FORMAT$GT3X ||
      (mon == MONITOR$MOVISENS && dformat == FORMAT$BIN) || 
      (mon == MONITOR$PARMAY_MTX && dformat == FORMAT$BIN)) {
    # for GENEActiv binary data, gt3x format data, Movisens data, and Parmay Matrix data, 
    # page selection is defined from start to end **including end**
    if (blocknumber > 1 && length(PreviousEndPage) != 0) {
      startpage = PreviousEndPage + 1
    } else {
      startpage = blocksize * (blocknumber - 1) + 1 # pages are numbered starting with page 1
    }
    endpage = startpage + blocksize - 1 # -1 because both startpage and endpage will be read,
                                        # and we want to read blocksize # of samples
  } else {
    # for other monitor brands and data formats
    # page selection is defined from start to end **excluding end itself**,
    # so start page of one block equals the end page of previous block
    if (blocknumber > 1 && length(PreviousEndPage) != 0) {
      startpage = PreviousEndPage
    } else {
      startpage = blocksize * (blocknumber - 1) # pages are numbered starting with page 0

      if (mon == MONITOR$ACTIGRAPH && dformat == FORMAT$CSV) {
        headerlength = 10
        startpage = startpage + headerlength
      }
    }
    endpage = startpage + blocksize
  }

  P = c()
  isLastBlock = FALSE

  if (mon == MONITOR$GENEACTIV && dformat == FORMAT$BIN) {    
    try(expr = {P = GGIRread::readGENEActiv(filename = filename, start = startpage,
                                            end = endpage, desiredtz = desiredtz,
                                            configtz = configtz)}, silent = TRUE)
    if (length(P) > 0 && ("data.out" %in% names(P))) {
      names(P)[names(P) == "data.out"] = "data"

      if (nrow(P$data) < (blocksize*300)) {
        isLastBlock = TRUE
      }
    }
  } else if (mon == MONITOR$ACTIGRAPH && dformat == FORMAT$CSV) {    
    # load rows 11:13  to investigate whether the file has a header
    # invisible because R complains about poor Actigraph file format,
    # this is an an ActiGraph problem not a GGIR problem, so we ignore it
    quiet <- function(x) {
      # from https://stackoverflow.com/a/54136863/5311763
      sink(tempfile())
      on.exit(sink())
      invisible(force(x))
    }

    # skip 1 more row only if the file has a header. Only the first chunk of data can have a header.
    if (blocknumber == 1) {
      testheader =  quiet(data.table::fread(filename, nrows = 2, skip = 10,
                                            dec = decn, showProgress = FALSE,
                                            header = TRUE, data.table=FALSE, stringsAsFactors=FALSE))
      if (suppressWarnings(is.na(as.numeric(colnames(testheader)[1])))) { # first value is *not* a number, so file starts with a header
        startpage = startpage + 1
        endpage = endpage + 1
      }
    }
    
    try(expr = {
      P$data = quiet(data.table::fread(filename, nrows = blocksize, skip = startpage,
                                       dec = decn, showProgress = FALSE,
                                       header = FALSE, # header should always be FALSE to prevent acceleration values from being mistaken for header when reading chunks 2 and on
                                       data.table=FALSE, stringsAsFactors=FALSE))
    }, silent = TRUE)
    if (length(P$data) > 0) {
      if (ncol(P$data) < 3) {
        P$data = c()
      } else {
        if (ncol(P$data) > 3) {
          P$data = P$data[, 2:4] # remove timestamp column, keep only XYZ columns
        }
        colnames(P$data) = c("x", "y", "z")
      }
    }
  } else if (mon == MONITOR$AXIVITY && dformat == FORMAT$CWA) {
    if (utils::packageVersion("GGIRread") < "0.3.0") {
      # ignore frequency_tol parameter
      apply_readAxivity = function(bstart, bend) {
        try(expr = {P = GGIRread::readAxivity(filename = filename, start = bstart, end = bend,
                                              progressBar = FALSE,
                                              desiredtz = desiredtz,
                                              configtz = configtz,
                                              interpolationType = params_rawdata[["interpolationType"]],
                                              header = header)
            }, silent = TRUE)
        return(P)
      }
    } else {
      # pass on frequency_tol parameter to GGIRread::readAxivity function
      apply_readAxivity = function(bstart, bend) {
        try(expr = {P = GGIRread::readAxivity(filename = filename, start = bstart, end = bend, 
                                              progressBar = FALSE,
                                              desiredtz = desiredtz,
                                              configtz = configtz,
                                              interpolationType = params_rawdata[["interpolationType"]],
                                              frequency_tol = params_rawdata[["frequency_tol"]],
                                              header = header)
            }, silent = TRUE)
        return(P)
      }
    }

    P = apply_readAxivity(bstart = startpage, bend = endpage)
    if (length(P) == 0) { 
      # If data reading is not successful then try following steps to retrieve issue
      # I am not sure if this is still relevant after all the improvements to GGIRread
      # but leaving this in just in case it is still needed
      
      PtestLastPage = PtestStartPage = NULL
      # Try to read the last page of the block because if it exists then there might be
      # something wrong with the first page(s).
      PtestLastPage = apply_readAxivity(bstart = endpage, bend = endpage)
      if (length(PtestLastPage) > 1) { 
        # Last page exist, so there must be something wrong with the first page
        NFilePagesSkipped = 0
        while (length(PtestStartPage) == 0) { # Try loading the first page of the block by iteratively skipping a page
          NFilePagesSkipped = NFilePagesSkipped + 1
          startpage = startpage + NFilePagesSkipped
          PtestStartPage = apply_readAxivity(bstart = startpage, bend = startpage)
          if (NFilePagesSkipped == 10 & length(PtestStartPage) == 0) PtestStartPage = FALSE # stop after 10 attempts
        }
      }
      if (length(PtestStartPage) > 1) {
        # Now we know on which page we can start and end the block, we can try again to
        # read the entire block:
        P = apply_readAxivity(bstart = startpage, bend = endpage)
        if (length(P) > 1) { # data reading succesful
          filequality$NFilePagesSkipped = NFilePagesSkipped # store number of pages jumped

          # Add replications of Ptest to the beginning of P to achieve same data 
          # length as under normal conditions
          P$data = rbind(do.call("rbind",
                                 replicate(NFilePagesSkipped, PtestStartPage$data, simplify = FALSE)),
                         P$data)
        }
      }
    }
    if ("temp" %in% colnames(P$data)) {
      colnames(P$data)[colnames(P$data) == "temp"] = "temperature"
    }
  } else if (mon == MONITOR$AXIVITY && dformat == FORMAT$CSV) {
    try(expr = {
      rawData = data.table::fread(filename, nrows = blocksize,
                            skip = startpage,
                            dec = decn, showProgress = FALSE, header = FALSE,
                            data.table=FALSE, stringsAsFactors=FALSE)
    }, silent = TRUE)
    if (length(rawData) > 0) {
      if (nrow(rawData) < blocksize) {
        isLastBlock = TRUE
      }

      rawTime = rawData[,1]

      if(class(rawTime)[1] == "character") {
        # If timestamps in the csv file are formatted (Y-M-D h:m:s.f), but some of the dates are formatted poorly,
        # data.table::fread() might have trouble parsing them as timestamps, and will instead return them as strings. 
        # as.POSIXct() is less brittle and might still be able to parse such timestamps, but it will be a very slow process.
        # For instance, one omGUI export contained timestamps like "2023-11-11 15:22:60.000" (which should normally be "2023-11-11 15:23:00.000").
        # data.table::fread() couldn't parse this but as.POSIXct() could, albeit very slowly.

        rawTime = as.POSIXct(rawTime, tz=configtz, origin = "1970-01-01")

        # if as.POSIXct() also failed to parse this data as timestamps, there's nothing else we can do.
        if(class(rawTime)[1] != "POSIXct") {
          stop(paste0("Corrupt timestamp data in ", filename),  call. = FALSE)
        } else {
          warning(paste0("Corrupt timestamp data in ", filename, 
                         ". This will greatly slow down processing. To avoid this, use the original .cwa file, ",
                         "or export your data with Unix timestamps instead."),  call. = FALSE)
        }
      } else {
        # If timestamps in the csv file are formatted (Y-M-D h:m:s.f), data.table::fread assumes them to be
        # in the timezone of the current device (i.e. as if configtz == "").
        # If this is not the case, we need to force the correct timezone (force, as opposed to converting to that timezone).
        if (!is.numeric(rawTime) && configtz != "") {
          rawTime = lubridate::force_tz(rawTime, configtz)
        }

        # A similar thing needs to be done if the csv file contains Unix timestamps as well.
        # OmGui converts device timestamps to Unix timestamps as if the timestamps were originally in UTC.
        # So we need to convert the Unix timestamp into a hh:mm:ss format, then force that timestamp
        # from  UTC into configtz timzone.
        if (is.numeric(rawTime)) {
          rawTime = as.POSIXct(rawTime, tz="UTC", origin = "1970-01-01")
          rawTime = lubridate::force_tz(rawTime, configtz)
        }
      }

      rawTime = as.numeric(rawTime)

      # resample the acceleration data, because AX3 data is stored at irregular time points
      rawAccel = as.matrix(rawData[,2:4])
      step = 1/sf
      timeRes = seq(rawTime[1], rawTime[length(rawTime)], step)
      timeRes = timeRes[1 : (length(timeRes) - 1)]

      # at the moment the function is designed for reading the 3 acceleration channels only,
      # because that is the situation of the use-case we had.
      accelRes = GGIRread::resample(rawAccel, rawTime, timeRes, nrow(rawAccel), params_rawdata[["interpolationType"]]) # this is now the resampled acceleration data
      P$data = data.frame(timeRes, accelRes)
      colnames(P$data) = c("time", "x", "y", "z")
    }
  } else if (mon == MONITOR$MOVISENS && dformat == FORMAT$BIN) {
    file_length = unisensR::getUnisensSignalSampleCount(dirname(filename), "acc.bin")
    if (endpage > file_length) {
      endpage = file_length
      isLastBlock = TRUE
    }
    P$data = unisensR::readUnisensSignalEntry(dirname(filename), "acc.bin",
                                              startIndex = startpage,
                                              endIndex = endpage)
    if (length(P$data) > 0) {
      if (ncol(P$data) < 3) {
        P$data = c()
      } else {
        colnames(P$data) = c("x", "y", "z")
        # there may or may not be a temp.bin file containing temperature
        try(expr = {P$data$temperature = g.readtemp_movisens(filename,
                                                             from = startpage, to = endpage,
                                                             acc_sf = sf, acc_length = nrow(P$data),
                                                             interpolationType = params_rawdata[["interpolationType"]])
        }, silent = TRUE)
      }
    } 
  } else if (mon == MONITOR$ACTIGRAPH && dformat == FORMAT$GT3X) {
    P$data = try(expr = {read.gt3x::read.gt3x(path = filename, batch_begin = startpage,
                                              batch_end = endpage, asDataFrame = TRUE)}, silent = TRUE)
    if (length(P$data) == 0 || inherits(P$data, "try-error") == TRUE) { # too short or no data at all
      P$data = c()
    } else { # If data passes these checks then it is usefull
      colnames(P$data)[colnames(P$data) == "X"] = "x"
      colnames(P$data)[colnames(P$data) == "Y"] = "y"
      colnames(P$data)[colnames(P$data) == "Z"] = "z"

      # read.gt3x::read.gt3x returns timestamps as POSIXct with GMT timezone, but they are actally in local time of the device.
      # Don't just convert timezones, instead force the correct local timezone of the device (configtz)
      # while keeping the same hh:mm:ss time.
      P$data$time = lubridate::force_tz(P$data$time, configtz)
    }
  } else if (mon == MONITOR$AD_HOC && dformat == FORMAT$AD_HOC_CSV) { # user-specified csv format
    # skip 1 more row only if rmc.firstrow.acc points at a row containing column names.
    # This is only relevant for the first chunk of data.
    if (blocknumber == 1) {
      testheader =  data.table::fread(filename, nrows = 2, skip = params_rawdata[["rmc.firstrow.acc"]]-1,
                                      dec = decn, showProgress = FALSE,
                                      header = TRUE, data.table=FALSE, stringsAsFactors=FALSE)
      if (suppressWarnings(is.na(as.numeric(colnames(testheader)[1])))) { # first value is *not* a number, so file starts with a header
        startpage = startpage + 1
        endpage = endpage + 1
      }
    }

    try(expr = {P = read.myacc.csv(rmc.file = filename,
                                   rmc.nrow = blocksize, rmc.skip = startpage,
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
                                   rmc.configtz = params_rawdata[["rmc.configtz"]],
                                   rmc.sf = params_rawdata[["rmc.sf"]],
                                   rmc.headername.sf = params_rawdata[["rmc.headername.sf"]],
                                   rmc.headername.sn = params_rawdata[["rmc.headername.sn"]],
                                   rmc.headername.recordingid = params_rawdata[["rmc.headername.sn"]],
                                   rmc.header.structure = params_rawdata[["rmc.header.structure"]],
                                   rmc.check4timegaps = params_rawdata[["rmc.check4timegaps"]],
                                   rmc.col.wear = params_rawdata[["rmc.col.wear"]],
                                   rmc.doresample = params_rawdata[["rmc.doresample"]],
                                   rmc.scalefactor.acc = params_rawdata[["rmc.scalefactor.acc"]],
                                   interpolationType = params_rawdata[["interpolationType"]],
                                   PreviousLastValue = PreviousLastValue,
                                   PreviousLastTime = PreviousLastTime,
                                   desiredtz = desiredtz,
                                   configtz = configtz,
                                   header = header)
    }, silent = TRUE)
    if (length(sf) == 0) sf = params_rawdata[["rmc.sf"]]
  } else if (mon == MONITOR$PARMAY_MTX && dformat == FORMAT$BIN) {
    try(expr = {P = GGIRread::readParmayMatrix(bin_file = filename, output = "all",
                                               start = startpage, end = endpage, 
                                               desiredtz = desiredtz, configtz = configtz,
                                               interpolationType = params_rawdata[["interpolationType"]])}, silent = TRUE)
    # fix colnames to match expectations of GGIR
    colnames(P$data) = gsub("acc_", "", colnames(P$data))
    colnames(P$data) = gsub("ambient_temp", "temperature", colnames(P$data))
    if (P$lastchunk) {
      isLastBlock = TRUE
    }
  } 

  # if first block isn't read then the file is probably corrupt
  if (length(P$data) <= 1 || nrow(P$data) == 0) {
    P = c()
    isLastBlock = TRUE
    if (blocknumber == 1) {
      warning('\nFile empty, possibly corrupt.\n')
      filequality$filetooshort = TRUE
      filequality$filecorrupt = TRUE
    }
  } else if (nrow(P$data) < (sf * ws * 2 + 1)) {
    # a shorter chunk of data than expected was read
    isLastBlock = TRUE

    if (blocknumber == 1) {
      # not enough data for analysis
      P = c()
      filequality$filetooshort = TRUE
    }
  }

  # remove any columns we don't need/expect
  P$data = P$data[,which(colnames(P$data) %in% c("x", "y", "z", "time", "light", "temperature", "wear"))]

  # every column except for time and wear should be numeric.
  # If it isn't, some non-numeric input was present, and we don't know how to deal with it.
  for (col in c("x", "y", "z", "light", "temperature")) {
    if ((col %in% colnames(P$data)) && !is.numeric(P$data[, col])) {
      stop(paste0("Corrupt file. ", col, " column contains non-numeric data."))
    }
  }

  # the wear column should be logical, but we will coerse it to numeric right away,
  # so that later it could be combined with the other numeric columns into a numeric matrix

  if ("wear" %in% colnames(P$data)) {
    if (!is.logical(P$data$wear)) {
      stop("Corrupt file. The wear column should contail TRUE/FALSE values.")
    }
    P$data$wear = as.numeric(P$data$wear)
  }

  # the time column at this point will be either Unix timestamps or POSIXct objects.
  # If POSIXct, we'll convert them to Unix timestamps, so that later this column 
  # could be combined with the other numeric columns into a numeric matrix
  if (("time" %in% colnames(P$data)) && !is.numeric(P$data$time)) { 
    P$data$time = as.numeric(P$data$time)
  }

  invisible(list(P = P,
                 filequality = filequality,
                 isLastBlock = isLastBlock,
                 endpage = endpage,  startpage = startpage))
}
