g.dotorcomma = function(inputfile,dformat,mon, desiredtz = c()) {
  decn = "."
  if (dformat == 2) {
    deci = as.matrix(read.csv(inputfile,skip = 100,nrow=10))
    if(is.na(suppressWarnings(as.numeric(deci[2,2]))) == T) decn = ","
  } else if (dformat == 1) {
    if (mon == 1) {
      try(expr={deci = as.matrix(g.binread(inputfile,0,2))},silent=TRUE)
      if(is.na(suppressWarnings(as.numeric(deci[2,2]))) == T) decn = ","
    } else if (mon == 2 ){
      if("GENEAread" %in% rownames(installed.packages()) == FALSE) {
        cat("\nWarning: R package GENEAread has not been installed, please install it before continuing")
      }
      try(expr={deci = as.matrix(GENEAread::read.bin(binfile=inputfile,start=1,end=3,mmap.load=FALSE,calibrate=TRUE))},silent=TRUE)
      if(is.na(as.numeric(deci[2,2])) == T) decn = ","
    }
  } else if (dformat == 3) { 
    try(expr={deci = as.matrix(g.wavread(wavfile=inputfile,start=1,end=10))},silent=TRUE)
    if(is.na(suppressWarnings(as.numeric(deci[2,2]))) == T) decn = ","
  } else if (dformat == 4) {
    try(expr={deci =as.matrix(g.cwaread(inputfile,start = 1, end = 10, desiredtz = desiredtz))},silent=TRUE)
    if(is.na(suppressWarnings(as.numeric(deci[2,2]))) == T) decn = ","
  }
  dotorcomma = decn
}