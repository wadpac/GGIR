g.dotorcomma = function(inputfile,dformat,mon) {
  if (dformat == 2) {
    deci = read.csv(inputfile,skip = 100,nrow=10)
    deci = as.matrix(deci)
    if(is.na(suppressWarnings(as.numeric(deci[2,2]))) == T) {
      decn = "," #comma
    } else {
      decn = "." #dot
    }
  } else if (dformat == 1) { #!decn is detect, but currently not used in the rest of the code!
    if (mon == 1) {
      try(expr={deci = g.binread(inputfile,0,2)},silent=TRUE)
      deci = as.matrix(deci$rawxyz)
      if(is.na(suppressWarnings(as.numeric(deci[2,2]))) == T) {
        decn = "," #comma
      } else {
        decn = "." #dot
      }
    } else if (mon == 2 ){
      try(expr={deci = GENEAread::read.bin(binfile=inputfile,start=1,end=3,mmap.load=FALSE,calibrate=TRUE)},silent=TRUE)
      deci = as.matrix(deci$data.out)
      if(is.na(as.numeric(deci[2,2])) == T) {
        decn = "," #comma
      } else {
        decn = "." #dot
      }
    }
  } else if (dformat == 3) { #!decn is detect, but currently not used in the rest of the code!
    print(inputfile)
    try(expr={deci = g.wavread(binfile=inputfile,start=1,end=10)},silent=TRUE)
    deci = as.matrix(deci$rawxyz)
    if(is.na(suppressWarnings(as.numeric(deci[2,2]))) == T) {
      decn = "," #comma
    } else {
      decn = "." #dot
    }
  }
  dotorcomma = decn
}