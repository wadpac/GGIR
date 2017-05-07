g.readaccfile = function(filename,blocksize,blocknumber,selectdaysfile=c(),filequality,
                         decn,dayborder) {
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
    I = g.inspectfile(filename) ## modified by JH
    mon = I$monc
    dformat = I$dformc
    sf = I$sf
  }
  ws=3600
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
          cat("\nnot enough data in this block 3\n")
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
          cat("\nnot enough data in this block 3\n")
        }
      }
      if (length(P) > 0) { #check whether there is enough data
        if (nrow(P$data.out) < ((sf*ws*2)+1) & blocknumber == 1) {
          P = c();  switchoffLD = 1
          cat("\nError code 2: data in block too short for doing non-wear detection\n")
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
        cat("\nError code 1: data in block too short for doing non-wear detection\n")		
        filequality$filetooshort = TRUE
      }
    } else {
      P = c()
      cat("\nEnd of file reached\n")
    }
  } else if (mon == 3 & dformat == 2) { # Actigraph csv format
    try(expr={P = read.csv(filename,nrow = (blocksize*300), skip=(10+(blocksize*300*(blocknumber-1))),dec=decn)},silent=TRUE)
    if (length(P) > 1) {
      P = as.matrix(P)
      if (nrow(P) < ((sf*ws*2)+1) & blocknumber == 1) {
        P = c() ; switchoffLD = 1 #added 30-6-2012
        cat("\nError code 1: data in block too short for doing non-wear detection\n")
        filequality$filetooshort = TRUE
      }
    } else {
      P = c()
      cat("\nEnd of file reached\n")
    }
  } else if (mon == 4 & dformat == 4) { # axivity cwa
    # Rcpp::sourceCpp('src/numUnpack.cpp')
    # Rcpp::sourceCpp('src/resample.cpp')
    try(expr={P = g.cwaread(fileName=filename, start = (blocksize*(blocknumber-1)),
                            end = (blocksize*blocknumber), progressBar = FALSE)},silent=TRUE)
    
    if (length(P) > 1) {
      if (length(P$data) == 0) {
        P = c() ; switchoffLD = 1
        cat("\nError code 1: data in block too short for doing non-wear detection\n")
        if (blocknumber == 1) filequality$filetooshort = TRUE
      } else {
        if (nrow(P$data) < ((sf*ws*2)+1)) { # & blocknumber == 1 #VvH 23/4/2017
          P = c() ; switchoffLD = 1
          cat("\nError: data too short for doing non-wear detection 1\n")		
          if (blocknumber == 1) filequality$filetooshort = TRUE
        }
      }
    } else {
      P = c()
      if (blocknumber == 1) {
        filequality$filecorrupt = TRUE
      }
      cat("\nEnd of file reached\n")
    }
  }
  invisible(list(P=P,filequality=filequality, switchoffLD = switchoffLD))
}