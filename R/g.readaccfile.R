g.readaccfile = function(filename,blocksize,blocknumber,selectdaysfile=c(),filequality,
                         decn,dayborder,ws, desiredtz = c()) {
  # function wrapper to read blocks of accelerationd data from various brands
  # the code identifies which accelerometer brand and data format it is
  # blocksize = number of pages to read at once
  # blocknumber = block count relative to beginning of measurement
  # mon 1 =  GENEA
  # mon 2 = GENEACtiv
  # mon 3 = Actigraph
  # mon 4 = Axivity
  # dformat 1 = binary
  # dformat 2 = csv
  # dformat 3 = wav
  # dformat 4 = cwa
  # sf = sample frequency (Hertz)
  # ws = large window size (default 3600 seconds)
  switchoffLD = 0
  
  if (length(unlist(strsplit(filename,"[.]RD"))) > 1) {
    useRDA = TRUE
  } else {
    useRDA = FALSE
  }
  if (useRDA == FALSE) {
    I = g.inspectfile(filename, desiredtz) ## modified by JH
    mon = I$monc
    dformat = I$dformc
    sf = I$sf
  }
  P = c()
  
  if (mon == 1 & dformat == 1) { # genea binary
    try(expr={P = g.binread(binfile=filename,(blocksize*(blocknumber-1)),(blocksize*blocknumber))},silent=TRUE)
    if (length(P) > 1) {
      if (nrow(P$rawxyz) < ((sf*ws*2)+1) & blocknumber == 1) {
        P = c() ; switchoffLD = 1 #added 30-6-2012
        cat("\nError: data too short for doing non-wear detection 1\n")		
        filequality$filetooshort = TRUE
      }
    } else {
      P = c()
      if (blocknumber == 1) {
        filequality$filecorrupt = TRUE
      }
      cat("\nEnd of file reached\n")
    }
  } else if (mon == 4 & dformat == 3) { # axivity wav
    try(expr={P = g.wavread(wavfile=filename,(blocksize*(blocknumber-1)),(blocksize*blocknumber))},silent=TRUE)
    if (length(P) > 1) {
      if (nrow(P$rawxyz) < ((sf*ws*2)+1) & blocknumber == 1) {
        P = c() ; switchoffLD = 1 #added 30-6-2012
        cat("\nError: data too short for doing non-wear detection 1\n")		
        filequality$filetooshort = TRUE
      }
    } else {
      P = c()
      if (blocknumber == 1) {
        filequality$filecorrupt = TRUE
      }
      cat("\nEnd of file reached\n")
    }
  } else if (mon == 2 & dformat == 1 & useRDA == FALSE) { # GENEActiv binary non-RDA format
    if (length(selectdaysfile) > 0) { # code to only read fragments of the data (Millenium cohort)
      #===================================================================
      # All of the below needed for Millenium cohort
      SDF = read.csv(selectdaysfile, stringsAsFactors = FALSE) # small change by CLS
      
      hvars = g.extractheadervars(I)
      SN = hvars$SN
      SDFi = which(basename(SDF$binFile) == basename(filename))
      
      if(length(SDFi) != 1) {
        save(SDF, SDFi, file = "debuggingFile.Rda")
        stop(paste0("CLS error: there are zero or more than one files: ",
                    filename, "in the wearcodes file"))
      }
      if("GENEAread" %in% rownames(installed.packages()) == FALSE) {
        cat("\nWarning: R package GENEAread has not been installed, please install it before continuing")
      }
      hhr <- GENEAread::header.info(filename)
      tint <- rbind(getStartEndNumeric(SDF$Day1[SDFi], hhr = hhr, startHour = dayborder),
                    getStartEndNumeric(SDF$Day2[SDFi], hhr = hhr, startHour = dayborder))
      
      if (blocknumber == nrow(tint)+1 | nrow(tint) == 0) {
        #all data read now make sure that it does not try to re-read it with mmap on
        switchoffLD = 1
      } else {
        try(expr= {
          P = GENEAread::read.bin(binfile=filename,start=tint[blocknumber,1],
                                  end=tint[blocknumber,2],calibrate=TRUE,do.temp=TRUE,mmap.load=FALSE)
          if (sf != P$freq) sf = P$freq
        },silent=TRUE)
        
        # llll
        if (length(P) <= 2) {
          cat("\ninitial attempt to read data unsuccessful, try again with mmap turned on:\n")
          #try again but now with mmap.load turned on
          if (length(P) != 0) {
            cat("\ndata read succesfully\n")
          } else {
            switchoffLD = 1
          }
        }
      }
      ############################
      if (length(P) > 0) {
        if (length(selectdaysfile) > 0) { 
          if (tint[blocknumber,1] == "0") {
            print("last block")
            switchoffLD = 1
          }
        } else {
          if (nrow(P$data.out) < (blocksize*300)) { #last block
            print("last block")
            switchoffLD = 1
          }
        }
      }
      if (length(P) == 0) { #if first block doens't read then probably corrupt
        if (blocknumber == 1) {
          #try to read without specifying blocks (file too short)
          try(expr={P = GENEAread::read.bin(binfile=filename,start=1,end=10,calibrate=TRUE,do.temp=TRUE,mmap.load=FALSE)},silent=TRUE)
          if (length(P) == 0) {
            cat("\nError: file possibly corrupt\n")
            P= c(); switchoffLD = 1
            filequality$filecorrupt = TRUE
          } else { #if not then P is now filled with data, but we are not interested in readin this
            P = c() # we do not want to analyse this data, the above lines are onnly to check that file is not corrupt
            filequality$filedoesnotholdday = TRUE
          }
        } else {
          P= c() #just no data in this last block
          # cat("\nnot enough data in this block 3\n")
        }
      } else { #check whether there is enough data
        if (nrow(P$data.out) < ((sf*ws*2)+1) & blocknumber == 1) {
          P = c();  switchoffLD = 1
          cat("\nError code 2: data too short for doing non-wear detection\n")
          filequality$filetooshort = TRUE
          filequality$filedoesnotholdday = TRUE
        }
      }
      # All of the above needed for Millenium cohort
      #======================================================================
    } else { 
      try(expr={P = GENEAread::read.bin(binfile=filename,start=(blocksize*(blocknumber-1)),
                                        end=(blocksize*blocknumber),calibrate=TRUE,do.temp=TRUE,mmap.load=FALSE)},silent=TRUE)
      if (length(P) <= 2) {
        cat("\ninitial attempt to read data unsuccessful, try again with mmap turned on:\n")
        #try again but now with mmap.load turned on
        try(expr={P = GENEAread::read.bin(binfile=filename,start=(blocksize*(blocknumber-1)),
                                          end=(blocksize*blocknumber),calibrate=TRUE,do.temp=TRUE,mmap.load=TRUE)},silent=TRUE)
        if (length(P) != 0) {
          cat("\ndata read succesfully\n")
          if (sf != P$freq) sf = P$freq
        } else {
          switchoffLD = 1
        }
      }
      if (length(P) > 0) {
        if (length(selectdaysfile) > 0) { 
          if (tint[blocknumber,1] == "0") {
            print("last block")
            switchoffLD = 1
          }
        } else {
          if (nrow(P$data.out) < (blocksize*300)) { #last block
            print("last block")
            switchoffLD = 1
          }
        }
      }
      if (length(P) == 0) { #if first block doens't read then probably corrupt
        if (blocknumber == 1) {
          #try to read without specifying blocks (file too short)
          try(expr={P = GENEAread::read.bin(binfile=filename,calibrate=TRUE,do.temp=TRUE,mmap.load=FALSE)},silent=TRUE)
          if (length(P) == 0) {
            cat("\nError: file possibly corrupt\n")
            P= c(); switchoffLD = 1
            filequality$filecorrupt = TRUE
          } #if not then P is now filled with data
        } else {
          P= c() #just no data in this last block
          # cat("\nnot enough data in this block 3\n")
        }
      }
      if (length(P) > 0) { #check whether there is enough data
        if (nrow(P$data.out) < ((sf*ws*2)+1) & blocknumber == 1) {
          P = c();  switchoffLD = 1
          cat("\nWarning (2): data in block too short for doing non-wear detection\n")
          filequality$filetooshort = TRUE
        }
      }
    }
    #===============
  } else if (mon == 2 & dformat == 2) { # GENEActiv csv format
    cat("\nGeneactiv in csv-format\n")
    try(expr={P = read.csv(filename,nrow = (blocksize*300), skip=(100+(blocksize*300*(blocknumber-1))),header = FALSE,dec=decn)},silent=TRUE)
    if (length(P) > 1) {
      P = as.matrix(P)
      if (nrow(P) < ((sf*ws*2)+1) & blocknumber == 1) {
        P = c() ; switchoffLD = 1 #added 30-6-2012
        cat("\nWarning (1): data in block too short for doing non-wear detection\n")		
        filequality$filetooshort = TRUE
      }
    } else {
      P = c()
      cat("\nEnd of file reached\n")
    }
  } else if (mon == 3 & dformat == 2) { # Actigraph csv format
    headerlength = 10
    #--------------
    skiprows = (headerlength+(blocksize*300*(blocknumber-1)))
    # load rows 11:13  to investigate whether the file has a header
    testheader = as.data.frame(data.table::fread(filename,nrow = 2, 
                                                 skip=10, 
                                                 dec=decn,showProgress = FALSE, header = FALSE))
    if (suppressWarnings(is.na(as.numeric(testheader[1,1]))) ==  FALSE) { # it has no header, first value is a number
      freadheader = FALSE
    } else { # it has a header, first value is a character
      freadheader = TRUE
      headerlength = 11
      if (skiprows == 10) {
        skiprows = 11
        freadheader = FALSE
      }
    }
    #--------------
    try(expr={
      P = as.data.frame(
        data.table::fread(filename,nrow = (blocksize*300), 
                          skip=skiprows, 
                          dec=decn,showProgress = FALSE, header = freadheader))
    },silent=TRUE)
    if (length(P) > 1) {
      P = as.matrix(P)
      if (nrow(P) < ((sf*ws*2)+1) & blocknumber == 1) {
        P = c() ; switchoffLD = 1 #added 30-6-2012
        cat("\nWarning (1): data in block too short for doing non-wear detection\n")
        filequality$filetooshort = TRUE
      }
    } else {
      P = c()
      cat("\nEnd of file reached\n")
    }
  } else if (mon == 4 & dformat == 4) { # axivity cwa
    try(expr={P = g.cwaread(fileName=filename, start = (blocksize*(blocknumber-1)), # try to read block first time
                            end = (blocksize*blocknumber), progressBar = FALSE, desiredtz = desiredtz)},silent=TRUE)
    
    if (length(P) > 1) { # data reading succesful
      if (length(P$data) == 0) { # too short?
        P = c() ; switchoffLD = 1
        cat("\nWarning (1): Data in block too short for doing non-wear detection\n")
        if (blocknumber == 1) filequality$filetooshort = TRUE
      } else { # too short for non-wear detection
        if (nrow(P$data) < ((sf*ws*2)+1)) {
          P = c() ; switchoffLD = 1
          cat("\nError: Data too short for doing non-wear detection 1\n")		
          if (blocknumber == 1) filequality$filetooshort = TRUE
        }
      }
    } else { #data reading not succesful
      # Now only load the last page, to assess whether there may be something wrong with this block of data:
      # I (VvH) implemented this as a temporary fix on 17Nov2018, but it would be better if we understood the source of this error
      # and address it inside the g.cwaread function. For example, are the page corrupted, and if so then why?
      PtestLastPage = c()
      # try to read the last page of the block
      try(expr={PtestLastPage = g.cwaread(fileName=filename, start = (blocksize*blocknumber),
                                          end = (blocksize*blocknumber), progressBar = FALSE, desiredtz = desiredtz)},silent=TRUE)
      if (length(PtestLastPage) > 1) { # Last page exist, so there must be something wrong with the first page
        NFilePagesSkipped = 0
        PtestStartPage = c()
        while (length(PtestStartPage) == 0) { # Try loading the first page of the block by iteratively skipping a page
          NFilePagesSkipped = NFilePagesSkipped + 1 
          try(expr={PtestStartPage = g.cwaread(fileName=filename, start = (blocksize*(blocknumber-1)) + NFilePagesSkipped,
                                               end = (blocksize*(blocknumber-1)) + NFilePagesSkipped, progressBar = FALSE, desiredtz = desiredtz)},silent=TRUE)
          if (NFilePagesSkipped == 10 & length(PtestStartPage) == 0) PtestStartPage = FALSE # stop after 10 attempts
        }
        cat(paste0("\nWarning (4): ",NFilePagesSkipped," page(s) skipped in cwa file in order to read data-block, this may indicate data corruption."))
      }
      if (length(PtestStartPage) > 1) { 
        # Now we know on which page we can start and end the block, we can try again to
        # read the entire block:
        try(expr={P = g.cwaread(fileName=filename, start = (blocksize*(blocknumber-1))+NFilePagesSkipped,
                                end = (blocksize*blocknumber), progressBar = FALSE, desiredtz = desiredtz)},silent=TRUE)
        if (length(P) > 1) { # data reading succesful
          if (length(P$data) == 0) { # if this still does not work then
            P = c() ; switchoffLD = 1
            cat("\nWarning (3): data in block too short for doing non-wear detection\n")
            if (blocknumber == 1) filequality$filetooshort = TRUE
          } else {
            if (nrow(P$data) < ((sf*ws*2)+1)) {
              P = c() ; switchoffLD = 1
              cat("\nError: data too short for doing non-wear detection 1\n")		
              if (blocknumber == 1) filequality$filetooshort = TRUE
            } else {
              filequality$NFilePagesSkipped = NFilePagesSkipped # store number of pages jumped
            }
          }
          # Add replications of Ptest to the beginning of P to achieve same data length as under nuormal conditions
          P$data = rbind(do.call("rbind",replicate(NFilePagesSkipped,PtestStartPage$data,simplify = FALSE)), P$data) 
          
        } else { # Data reading still not succesful, so classify file as corrupt
          P = c()
          if (blocknumber == 1) {
            filequality$filecorrupt = TRUE
          }
          cat("\nEnd of file reached\n")
        }
      } else { 
        P = c()
        if (blocknumber == 1) {
          filequality$filecorrupt = TRUE
        }
        cat("\nEnd of file reached\n")
      }
    }
  } else if (mon == 4 & dformat == 2) { # axivity (ax3) csv format
    freadheader = FALSE
    headerlength = 0
    skiprows = (headerlength+(blocksize*300*(blocknumber-1)))
    try(expr={
      P = as.data.frame(
        data.table::fread(filename,nrow = (blocksize*300), 
                          skip=skiprows, 
                          dec=decn,showProgress = FALSE, header = freadheader))
    },silent=TRUE)
    if (length(P) > 1) {
      # P = as.matrix(P)
      if (nrow(P) < ((sf*ws*2)+1) & blocknumber == 1) {
        P = c() ; switchoffLD = 1 #added 30-6-2012
        cat("\nWarning (1): data in block too short for doing non-wear detection\n")
        filequality$filetooshort = TRUE
      }
      if (nrow(P) < (blocksize*300)) { #last block
        print("last block")
        switchoffLD = 1
      }
      # resample the acceleration data, because AX3 data is stored at irregular time points
      rawTime = vector(mode = "numeric", nrow(P))
      if (length(desiredtz) == 0 & blocknumber == 1) {
        cat("Forgot to specify argument desiredtz? Now Europe/London assumed")
        desiredtz = "Europe/London"
      }
      rawTime = as.numeric(as.POSIXlt(P[,1],tz = desiredtz))
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
      accelRes = resample(rawAccel, rawTime, timeRes, rawLast) # this is now the resampled acceleration data
      P = cbind(timeRes,accelRes)
    } else {
      P = c()
      cat("\nEnd of file reached\n")
    }
  }
  invisible(list(P=P,filequality=filequality, switchoffLD = switchoffLD))
}
