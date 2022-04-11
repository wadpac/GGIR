g.readaccfile = function(filename, blocksize, blocknumber, selectdaysfile = c(), filequality,
                         decn, ws, PreviousEndPage = 1, inspectfileobject = c(),
                         params_rawdata = c(), params_general = c(), ...) {
  #get input variables
  input = list(...)
  if (any(names(input) %in% c("filename", "blocksize", "blocknumber",
                              "selectdaysfile", "filequality",
                              "decn", "ws", "PreviousEndPage",
                              "inspectfileobject", "params_rawdata",
                              "params_general")) == FALSE) {
    # Extract and check parameters if user provides more arguments than just the parameter arguments
    # So, inside GGIR this will not be used, but it is used when g.getmeta is used on its own
    # as if it was still the old g.getmeta function
    params = extract_params(params_rawdata = params_rawdata,
                            params_general = params_general,
                            input = input) # load default parameters
    params_rawdata = params$params_rawdata
    params_general = params$params_general
  }
  # function wrapper to read blocks of accelerationd data from various brands
  # the code identifies which accelerometer brand and data format it is
  # blocksize = number of pages to read at once
  # blocknumber = block count relative to beginning of measurement
  # mon 0 = Other
  # mon 1 =  GENEA
  # mon 2 = GENEACtiv
  # mon 3 = Actigraph
  # mon 4 = Axivity
  # mon 5 = Movisens
  # mon 6 = Verisense
  
  # dformat 1 = binary
  # dformat 2 = csv
  # dformat 3 = wav
  # dformat 4 = cwa
  # dformat 5 = your own adhoc csv format
  # dformat 6 = gt3x
  # sf = sample frequency (Hertz)
  # ws = large window size (default 3600 seconds)
  
  switchoffLD = 0
  useRDA = TRUE
  if (length(unlist(strsplit(filename,"[.]RD"))) <= 1) useRDA = FALSE
  if (useRDA == FALSE) {
    I = inspectfileobject
    mon = I$monc
    if (mon == 6) mon = 3
    dformat = I$dformc
    sf = I$sf
  }
  P = c()
  updatepageindexing = function(startpage = c(), deltapage = c(), blocknumber = c(), PreviousEndPage = c(),
                                mon = c(), dformat = c()) {
    # This function ensures that startpage is only specified for blocknumber 1.
    # The next time (blocknumber > 1) the startpage will be derived from the previous
    # endpage and the blocksize.
    if (blocknumber != 1 & length(PreviousEndPage) != 0) {
      if ((mon == 2 & dformat == 1) | dformat == 2) {
        # only in GENEActiv binary data and for csv format data
        # page selection is defined from start to end (including end)
        startpage = PreviousEndPage + 1
      } else {
        # for other monitor brands and data formats
        # page selection is defined from start to end (excluding end itself)
        # so start page of one block equals the end page of previous block
        startpage = PreviousEndPage
      }
    }
    endpage = startpage + deltapage
    return(list(startpage = startpage, endpage = endpage))
  }
  if (mon == 1 & dformat == 1) { # genea binary
    startpage = blocksize * (blocknumber - 1)
    deltapage = blocksize
    UPI = updatepageindexing(startpage = startpage, deltapage = deltapage,
                             blocknumber = blocknumber, PreviousEndPage = PreviousEndPage, mon = mon, dformat = dformat)
    startpage = UPI$startpage;    endpage = UPI$endpage
    try(expr = {P = g.binread(binfile = filename, startpage, endpage)}, silent = TRUE)
    if (length(P) > 1) {
      if (nrow(P$rawxyz) < ((sf * ws * 2) + 1) & blocknumber == 1) {
        P = c() ; switchoffLD = 1 #added 30-6-2012
        filequality$filetooshort = TRUE
      }
    } else {
      P = c()
      if (blocknumber == 1) {
        filequality$filecorrupt = TRUE
      }
    }
  } else if (mon == 4 & dformat == 3) { # axivity wav
    startpage = blocksize * (blocknumber - 1)
    deltapage = blocksize
    UPI = updatepageindexing(startpage = startpage, deltapage = deltapage,
                             blocknumber = blocknumber, PreviousEndPage = PreviousEndPage, mon = mon, dformat = dformat)
    startpage = UPI$startpage;    endpage = UPI$endpage
    try(expr = {P = g.wavread(wavfile = filename, startpage, endpage)}, silent = TRUE)
    if (length(P) > 1) {
      if (nrow(P$rawxyz) < ((sf * ws * 2) + 1) & blocknumber == 1) {
        P = c() ; switchoffLD = 1 #added 30-6-2012
        filequality$filetooshort = TRUE
      }
    } else {
      P = c()
      if (blocknumber == 1) filequality$filecorrupt = TRUE
    }
  } else if (mon == 2 & dformat == 1 & useRDA == FALSE) { # GENEActiv binary non-RDA format
    if (length(selectdaysfile) > 0) { # code to only read fragments of the data (Millenium cohort)
      #===================================================================
      # All of the below needed for Millenium cohort
      SDF = read.csv(selectdaysfile, stringsAsFactors = FALSE) # small change by CLS
      hvars = g.extractheadervars(I)
      deviceSerialNumber = hvars$deviceSerialNumber
      SDFi = which(basename(SDF$binFile) == basename(filename))
      if (length(SDFi) != 1) {
        save(SDF, SDFi, file = "debuggingFile.Rda")
        stop(paste0("CLS error: there are zero or more than one files: ",
                    filename, "in the wearcodes file"))
      }
      if ("GENEAread" %in% rownames(installed.packages()) == FALSE) {
        cat("\nWarning: R package GENEAread has not been installed, please install it before continuing")
      }
      hhr <- GENEAread::header.info(filename)
      tint <- rbind(getStartEndNumeric(SDF$Day1[SDFi], hhr = hhr, startHour = params_general[["dayborder"]]),
                    getStartEndNumeric(SDF$Day2[SDFi], hhr = hhr, startHour = params_general[["dayborder"]]))

      if (blocknumber == nrow(tint) + 1 | nrow(tint) == 0) {
        #all data read now make sure that it does not try to re-read it with mmap on
        switchoffLD = 1
      } else {
        try(expr = {
          P = GENEAread::read.bin(binfile = filename, start = tint[blocknumber, 1],
                                  end = tint[blocknumber, 2], calibrate = TRUE, 
                                  do.temp = TRUE, mmap.load = FALSE)
          if (sf != P$freq) sf = P$freq
        }, silent = TRUE)
        if (length(P) <= 2) {
          #try again but now with mmap.load turned on
          if (length(P) == 0) switchoffLD = 1
        }
      }
      if (length(P) > 0) {
        if (length(selectdaysfile) > 0) {
          if (tint[blocknumber,1] == "0") {
            switchoffLD = 1
          }
        } else {
          if (nrow(P$data.out) < (blocksize*300)) switchoffLD = 1 #last block
        }
      }
      if (length(P) == 0) { #if first block doens't read then probably corrupt
        if (blocknumber == 1) {
          #try to read without specifying blocks (file too short)
          try(expr = {P = GENEAread::read.bin(binfile = filename, start = 1, end = 10,
                                            calibrate = TRUE, do.temp = TRUE,
                                            mmap.load = FALSE)}, silent = TRUE)
          if (length(P) == 0) {
            warning('\nFile possibly corrupt\n')
            P = c(); switchoffLD = 1
            filequality$filecorrupt = TRUE
          } else { #if not then P is now filled with data, but we are not interested in readin this
            P = c() # we do not want to analyse this data, the above lines are onnly to check that file is not corrupt
            filequality$filedoesnotholdday = TRUE
          }
        } else {
          P = c() #just no data in this last block
        }
      } else { #check whether there is enough data
        if (nrow(P$data.out) < ((sf * ws * 2) + 1) & blocknumber == 1) {
          P = c();  switchoffLD = 1
          cat("\nError code 2: data too short for doing non-wear detection\n")
          filequality$filetooshort = filequality$filedoesnotholdday = TRUE
        }
      }
      # All of the above needed for Millenium cohort
      #======================================================================
    } else {
      startpage = blocksize * (blocknumber - 1) + 1 # GENEActiv starts with page 1
      deltapage = blocksize
      UPI = updatepageindexing(startpage = startpage, deltapage = deltapage,
                               blocknumber = blocknumber, PreviousEndPage = PreviousEndPage, mon = mon, dformat = dformat)
      startpage = UPI$startpage;    endpage = UPI$endpage
      try(expr = {P = GENEAread::read.bin(binfile = filename, start = startpage,
                                        end = endpage, calibrate = TRUE, do.temp = TRUE,
                                        mmap.load = FALSE)}, silent = TRUE)
      if (length(P) <= 2) {
        #try again but now with mmap.load turned on
        options(warn = -1) # to ignore warnings relating to failed mmap.load attempt
        try(expr = {
          P = GENEAread::read.bin(binfile = filename, start = startpage,
                                          end = endpage, calibrate = TRUE, do.temp = TRUE, mmap.load = TRUE)
        }, silent = TRUE)
        options(warn = 0) # to ignore GENEAread warnings
        if (length(P) != 0) {
          if (sf != P$freq) sf = P$freq
        } else {
          switchoffLD = 1
        }
      }
      if (length(P) > 0) {
        if (length(selectdaysfile) > 0) {
          if (tint[blocknumber,1] == "0") switchoffLD = 1
        } else {
          if (nrow(P$data.out) < (blocksize*300)) switchoffLD = 1 #last block
        }
      }
      if (length(P) == 0) { #if first block doens't read then probably corrupt
        if (blocknumber == 1) {
          #try to read without specifying blocks (file too short)
          try(expr = {
            P = GENEAread::read.bin(binfile = filename, calibrate = TRUE, do.temp = TRUE, mmap.load = FALSE)
            }, silent = TRUE)
          if (length(P) == 0) {
            warning('\nFile possibly corrupt\n')
            P = c(); switchoffLD = 1
            filequality$filecorrupt = TRUE
          } #if not then P is now filled with data
        } else {
          P = c() #just no data in this last block
        }
      }
      if (length(P) > 0) { #check whether there is enough data
        if (nrow(P$data.out) < ((sf * ws * 2) + 1) & blocknumber == 1) {
          P = c();  switchoffLD = 1
          filequality$filetooshort = TRUE
        }
      }
    }
    #===============
  } else if (mon == 3 & dformat == 2) { # Actigraph csv format
    headerlength = 10
    #--------------
    startpage = (headerlength + (blocksize * 300 * (blocknumber - 1)))
    deltapage = blocksize * 300
    UPI = updatepageindexing(startpage = startpage, deltapage = deltapage,
                             blocknumber = blocknumber, PreviousEndPage = PreviousEndPage, mon = mon, dformat = dformat)
    startpage = UPI$startpage;    endpage = UPI$endpage
    
    # load rows 11:13  to investigate whether the file has a header
    # invisible because R complains about poor Actigraph file format,
    # this is an an ActiGraph problem not a GGIR problem, so we ignore it
    quiet <- function(x) { 
      # from https://stackoverflow.com/a/54136863/5311763
      sink(tempfile()) 
      on.exit(sink()) 
      invisible(force(x)) 
    } 
    testheader =  quiet(as.data.frame(data.table::fread(filename, nrows = 2, skip = 10,
                                                                          dec = decn, showProgress = FALSE,
                                                        header = TRUE),
                                                        stringsAsFactors = FALSE))
    if (suppressWarnings(is.na(as.numeric(testheader[1, 1]))) ==  FALSE) { # it has no header, first value is a number
      freadheader = FALSE
    } else { # it has a header, first value is a character
      freadheader = TRUE
      headerlength = 11
    }
    if (startpage == 10) {
      startpage = 11
    }
    #--------------
    try(expr = {
      P = quiet(as.data.frame(
        data.table::fread(filename, nrows = deltapage,
                          skip = startpage,
                          dec = decn, showProgress = FALSE, header = freadheader),
        stringsAsFactors = TRUE))
    }, silent = TRUE)
    if (length(P) > 1) {
      # data.matrix turnes num to char if there are missing values.
      if (ncol(P) == 3) {
        P = data.matrix(P) 
      } else {
        P = data.matrix(P[, 2:ncol(P)]) # avoid timestamp column
      }
      if (nrow(P) < ((sf * ws * 2) + 1) & blocknumber == 1) {
        P = c() ; switchoffLD = 1
        filequality$filetooshort = TRUE
      }
    } else {
      P = c()
    }
  } else if (mon == 4 & dformat == 4) { # axivity cwa
    startpage = blocksize * (blocknumber - 1)
    deltapage = blocksize
    UPI = updatepageindexing(startpage = startpage, deltapage = deltapage,
                             blocknumber = blocknumber, PreviousEndPage = PreviousEndPage, mon = mon, dformat = dformat)
    startpage = UPI$startpage;    endpage = UPI$endpage
    try(expr = {P = g.cwaread(fileName = filename, start = startpage, # try to read block first time
                            end = endpage, progressBar = FALSE, desiredtz = params_general[["desiredtz"]],
                            configtz = params_general[["configtz"]], interpolationType = params_rawdata[["interpolationType"]])}, silent = TRUE)
    if (length(P) > 1) { # data reading succesful
      if (length(P$data) == 0) { # too short?
        P = c() ; switchoffLD = 1
        if (blocknumber == 1) filequality$filetooshort = TRUE
      } else {
        if (nrow(P$data) < ((sf * ws * 2) + 1)) {
          P = c() ; switchoffLD = 1
          if (blocknumber == 1) filequality$filetooshort = TRUE
        }
      }
    } else { #data reading not succesful
      # Now only load the last page, to assess whether there may be something wrong with this block of data:
      # I (VvH) implemented this as a temporary fix on 17Nov2018, but it would be better if we understood the source of this error
      # and address it inside the g.cwaread function. For example, are the page corrupted, and if so then why?
      PtestLastPage = PtestStartPage = c()
      # try to read the last page of the block, because if it exists then there might be something wrong with the first page(s).
      try(expr = {PtestLastPage = g.cwaread(fileName = filename, start = endpage, #note this is intentionally endpage
                                          end = endpage, progressBar = FALSE, desiredtz = params_general[["desiredtz"]],
                                          configtz = params_general[["configtz"]],
                                          interpolationType = params_rawdata[["interpolationType"]])}, silent = TRUE)
      if (length(PtestLastPage) > 1) { # Last page exist, so there must be something wrong with the first page
        NFilePagesSkipped = 0
        while (length(PtestStartPage) == 0) { # Try loading the first page of the block by iteratively skipping a page
          NFilePagesSkipped = NFilePagesSkipped + 1
          startpage = startpage + NFilePagesSkipped
          try(expr = {PtestStartPage = g.cwaread(fileName = filename, start = startpage , # note: end is intentionally startpage
                                               end = startpage, progressBar = FALSE,
                                               desiredtz = params_general[["desiredtz"]],
                                               configtz = params_general[["configtz"]],
                                               interpolationType = params_rawdata[["interpolationType"]])}, silent = TRUE)
          if (NFilePagesSkipped == 10 & length(PtestStartPage) == 0) PtestStartPage = FALSE # stop after 10 attempts
        }
        cat(paste0("\nWarning (4): ",NFilePagesSkipped," page(s) skipped in cwa file in order to read data-block, this may indicate data corruption."))
      }
      if (length(PtestStartPage) > 1) {
        # Now we know on which page we can start and end the block, we can try again to
        # read the entire block:
        try(expr = {P = g.cwaread(fileName = filename, start = startpage,
                                end = endpage, progressBar = FALSE,
                                desiredtz = params_general[["desiredtz"]],
                                configtz = params_general[["configtz"]],
                                interpolationType = params_rawdata[["interpolationType"]])}, silent = TRUE)
        if (length(P) > 1) { # data reading succesful
          if (length(P$data) == 0) { # if this still does not work then
            P = c() ; switchoffLD = 1
            if (blocknumber == 1) filequality$filetooshort = TRUE
          } else {
            if (nrow(P$data) < ((sf * ws * 2) + 1)) {
              P = c() ; switchoffLD = 1
              if (blocknumber == 1) filequality$filetooshort = TRUE
            } else {
              filequality$NFilePagesSkipped = NFilePagesSkipped # store number of pages jumped
            }
          }
          # Add replications of Ptest to the beginning of P to achieve same data length as under nuormal conditions
          P$data = rbind(do.call("rbind",replicate(NFilePagesSkipped,PtestStartPage$data,simplify = FALSE)), P$data)
        } else { # Data reading still not succesful, so classify file as corrupt
          P = c()
          if (blocknumber == 1) filequality$filecorrupt = TRUE
        }
      } else {
        P = c()
        if (blocknumber == 1) filequality$filecorrupt = TRUE
      }
    }
  } else if (mon == 4 & dformat == 2) { # axivity (ax3) csv format
    freadheader = FALSE
    headerlength = 0
    startpage = (headerlength + (blocksize * 300 * (blocknumber - 1)))
    deltapage = (blocksize*300)
    UPI = updatepageindexing(startpage = startpage, deltapage = deltapage,
                             blocknumber = blocknumber,
                             PreviousEndPage = PreviousEndPage, mon = mon, dformat = dformat)
    startpage = UPI$startpage;    endpage = UPI$endpage
    try(expr = {
      P = as.data.frame(
        data.table::fread(filename, nrows = deltapage,
                          skip = startpage,
                          dec = decn, showProgress = FALSE, header = freadheader),
        stringsAsFactors = TRUE)
    }, silent = TRUE)
    if (length(P) > 1) {
      if (nrow(P) < ((sf * ws * 2) + 1) & blocknumber == 1) {
        P = c() ; switchoffLD = 1 #added 30-6-2012
        filequality$filetooshort = TRUE
      }
      if (nrow(P) < (deltapage)) { #last block
        switchoffLD = 1
      }
      # resample the acceleration data, because AX3 data is stored at irregular time points
      rawTime = vector(mode = "numeric", nrow(P))
      if (length(params_general[["desiredtz"]]) == 0 & blocknumber == 1) {
        cat("Forgot to specify argument desiredtz? Now Europe/London assumed")
        params_general[["desiredtz"]] = "Europe/London"
      }
      rawTime = as.numeric(as.POSIXlt(P[,1],tz = params_general[["desiredtz"]]))
      rawAccel = as.matrix(P[,2:4])
      step = 1/sf
      start = rawTime[1]
      end = rawTime[length(rawTime)]
      timeRes = seq(start, end, step)
      nr = length(timeRes) - 1
      timeRes = as.vector(timeRes[1:nr])
      accelRes = matrix(0,nrow = nr, ncol = 3, dimnames = list(NULL, c("x", "y", "z")))
      # at the moment the function is designed for reading the r3 acceleration channels only,
      # because that is the situation of the use-case we had.
      rawLast = nrow(rawAccel)
      accelRes = resample(rawAccel, rawTime, timeRes, rawLast, params_rawdata[["interpolationType"]]) # this is now the resampled acceleration data
      P = cbind(timeRes,accelRes)
    } else {
      P = c()
    }
  } else if (mon == 5 & dformat == 1) { #movisens
      startpage = blocksize * (blocknumber - 1) + 1
      deltapage = blocksize
      UPI = updatepageindexing(startpage = startpage, deltapage = deltapage,
                               blocknumber = blocknumber, PreviousEndPage = PreviousEndPage, mon = mon, dformat = dformat)
      startpage = UPI$startpage;    endpage = UPI$endpage
      file_length = unisensR::getUnisensSignalSampleCount(dirname(filename), "acc.bin")
      if (endpage > file_length) {
          endpage = file_length
          switchoffLD = 1
          }
      P = unisensR::readUnisensSignalEntry(dirname(filename), "acc.bin",
                                           startIndex = startpage,
                                           endIndex = endpage)
      P = as.matrix(P)
      if (nrow(P) < ((sf * ws * 2) + 1) & blocknumber == 1) {
          P = c()
          switchoffLD = 1
          filequality$filetooshort = TRUE
      }
  } else if (mon == 3 & dformat == 6) { #actigraph .gt3x
    startpage = blocksize * (blocknumber - 1) + 1
    deltapage = blocksize
    UPI = updatepageindexing(startpage = startpage, deltapage = deltapage,
                             blocknumber = blocknumber, PreviousEndPage = PreviousEndPage, mon = mon, dformat = dformat)
    startpage = UPI$startpage;    endpage = UPI$endpage
    try(expr = {P = as.data.frame(read.gt3x_ggir(path = filename, batch_begin = startpage,
                                                     batch_end = endpage,asDataFrame = TRUE))}, silent = TRUE)
    if (length(P) == 0) { # too short or not data at all
      P = c() ; switchoffLD = 1
      if (blocknumber == 1) filequality$filetooshort = TRUE
      if (blocknumber == 1) filequality$filecorrupt = TRUE
    } else {
      if (nrow(P) < ((sf * ws * 2) + 1)) {
        P = c() ; switchoffLD = 1
        if (blocknumber == 1) filequality$filetooshort = TRUE
      } # If data passes these checks then it is usefull
    }
  } else if (mon == 0 & dformat == 5) { # user specified csv format
    startpage = (1 + (blocksize * 300 * (blocknumber - 1)))
    deltapage = (blocksize*300)
    UPI = updatepageindexing(startpage = startpage,deltapage = deltapage,
                             blocknumber = blocknumber,PreviousEndPage = PreviousEndPage, mon = mon, dformat = dformat)
    startpage = UPI$startpage;    endpage = UPI$endpage
    try(expr = {P = read.myacc.csv(rmc.file = filename,
                                 rmc.nrow = deltapage, rmc.skip = startpage,
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
                                 rmc.check4timegaps = params_rawdata[["rmc.check4timegaps"]],
                                 rmc.col.wear = params_rawdata[["rmc.col.wear"]],
                                 rmc.doresample = params_rawdata[["rmc.doresample"]],
                                 interpolationType = params_rawdata[["interpolationType"]])
    }, silent = TRUE)
    if (length(sf) == 0) sf = params_rawdata[["rmc.sf"]]
    if (length(P) == 2) {
      # P = as.matrix(P) # turned off 21-5-2019
      if (nrow(P$data) < ((sf * ws * 2) + 1) & blocknumber == 1) {
        P = c() ; switchoffLD = 1 #added 30-6-2012
        filequality$filetooshort = TRUE
      }
    } else {
      P = c()
    }
  }
  invisible(list(P = P, 
                 filequality = filequality,
                 switchoffLD = switchoffLD,
                 endpage = endpage,  startpage = startpage))
}
