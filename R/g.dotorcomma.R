g.dotorcomma = function(inputfile,dformat,mon, desiredtz = c()) {
  decn = getOption("OutDec") # extract system decimal separator
  if (length(decn) == 0) decn = "." # assume . if not retrieved
  if (dformat == 2) {
    deci = as.matrix(read.csv(inputfile,skip = 100,nrow=10))
    if(is.na(suppressWarnings(as.numeric(deci[2,2]))) == T & getOption("OutDec") == ".") decn = ","
  } else if (dformat == 1) {
    if (mon == 1) {
      # GENEA values are stroed in g without decimal place.
      # try(expr={deci = g.binread(inputfile,0,2)},silent=TRUE)
      # if(is.na(suppressWarnings(as.numeric(deci[2,2]))) == T) decn = ","
    } else if (mon == 2 ){
      if("GENEAread" %in% rownames(installed.packages()) == FALSE) {
        cat("\nWarning: R package GENEAread has not been installed, please install it before continuing")
      }
      try(expr={deci = GENEAread::read.bin(binfile=inputfile,start=1,end=3,mmap.load=FALSE,calibrate=TRUE)},silent=TRUE)
      # on.exit(closeAllConnections())
      if(is.na(as.numeric(deci$data.out[2,2])) == T & getOption("OutDec") == ".") decn = ","
    }
  } else if (dformat == 3) { 
    try(expr={deci = g.wavread(wavfile=inputfile,start=1,end=10)},silent=TRUE)
    if(is.na(suppressWarnings(as.numeric(deci$rawxyz[2,2]))) == T & getOption("OutDec") == ".") decn = ","
  } else if (dformat == 4) {
    try(expr={deci = g.cwaread(inputfile,start = 1, end = 10, desiredtz = desiredtz)$data},silent=TRUE)
    if(is.na(suppressWarnings(as.numeric(deci[2,2]))) == T & getOption("OutDec") == ".") decn = ","
  }
  dotorcomma = decn
}