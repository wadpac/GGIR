g.dotorcomma = function(inputfile,dformat,mon, desiredtz = c(), ...) {
  #get input variables (relevant when read.myacc.csv is used)
  input = list(...)
  if (length(input) > 0) {
    for (i in 1:length(names(input))) {
      txt = paste(names(input)[i],"=",input[i],sep="")
      if (class(unlist(input[i])) == "character") {
        txt = paste(names(input)[i],"='",unlist(input[i]),"'",sep="")
      }
      eval(parse(text=txt))
    }
  }
  if (length(which(ls() == "rmc.dec")) == 0) rmc.dec="."
  if (length(which(ls() == "rmc.firstrow.acc")) == 0) rmc.firstrow.acc = c()
  if (length(which(ls() == "rmc.firstrow.header")) == 0) rmc.firstrow.header=c()
  if (length(which(ls() == "rmc.header.length")) == 0)  rmc.header.length= c()
  if (length(which(ls() == "rmc.col.acc")) == 0) rmc.col.acc = 1:3
  if (length(which(ls() == "rmc.col.temp")) == 0) rmc.col.temp = c()
  if (length(which(ls() == "rmc.col.time")) == 0) rmc.col.time=c()
  if (length(which(ls() == "rmc.unit.acc")) == 0) rmc.unit.acc = "g"
  if (length(which(ls() == "rmc.unit.temp")) == 0) rmc.unit.temp = "C"
  if (length(which(ls() == "rmc.unit.time")) == 0) rmc.unit.time = "POSIX"
  if (length(which(ls() == "rmc.format.time")) == 0) rmc.format.time = "%Y-%m-%d %H:%M:%OS"
  if (length(which(ls() == "rmc.bitrate")) == 0) rmc.bitrate = c()
  if (length(which(ls() == "rmc.dynamic_range")) == 0) rmc.dynamic_range = c()
  if (length(which(ls() == "rmc.unsignedbit")) == 0) rmc.unsignedbit = TRUE
  if (length(which(ls() == "rmc.origin")) == 0) rmc.origin = "1970-01-01"
  if (length(which(ls() == "rmc.desiredtz")) == 0) rmc.desiredtz= "Europe/London"
  if (length(which(ls() == "rmc.sf")) == 0) rmc.sf  = c()
  if (length(which(ls() == "rmc.headername.sf")) == 0) rmc.headername.sf = c()
  if (length(which(ls() == "rmc.headername.sn")) == 0) rmc.headername.sn = c()
  if (length(which(ls() == "rmc.headername.recordingid")) == 0) rmc.headername.recordingid = c()
  if (length(which(ls() == "rmc.header.structure")) == 0) rmc.header.structure = c()
  if (length(which(ls() == "rmc.check4timegaps")) == 0) rmc.check4timegaps = FALSE
  #------------------------------------------------------------
  if (length(rmc.firstrow.acc) == 1) {
    dformat = 5
    mon = 5
    Pusercsvformat = read.myacc.csv(rmc.file=filename, rmc.nrow=5, rmc.dec=rmc.dec,
                                    rmc.firstrow.acc = rmc.firstrow.acc,
                                    rmc.firstrow.header = rmc.firstrow.header,
                                    rmc.header.length = rmc.header.length,
                                    rmc.col.acc = rmc.col.acc,
                                    rmc.col.temp = rmc.col.temp, rmc.col.time=rmc.col.time,
                                    rmc.unit.acc = rmc.unit.acc, rmc.unit.temp = rmc.unit.temp,
                                    rmc.unit.time = rmc.unit.time,
                                    rmc.format.time = rmc.format.time,
                                    rmc.bitrate = rmc.bitrate, rmc.dynamic_range = rmc.dynamic_range,
                                    rmc.unsignedbit = rmc.unsignedbit,
                                    rmc.origin = rmc.origin,
                                    rmc.desiredtz = rmc.desiredtz, rmc.sf = rmc.sf,
                                    rmc.headername.sf = rmc.headername.sf,
                                    rmc.headername.sn = rmc.headername.sn,
                                    rmc.headername.recordingid = rmc.headername.sn,
                                    rmc.header.structure = rmc.header.structure,
                                    rmc.check4timegaps = rmc.check4timegaps)
    sf = Pusercsvformat$header$sample_rate
  }
  
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
      if("GENEAread" %in% rownames(installed.packages()) == FALSE) {
        cat("\nWarning: R package GENEAread has not been installed, please install it before continuing")
      }
      try(expr={deci = GENEAread::read.bin(binfile=inputfile,start=1,end=3,mmap.load=FALSE,calibrate=TRUE)},silent=TRUE)
      deci = as.matrix(deci$data.out)
      if(is.na(as.numeric(deci[2,2])) == T) {
        decn = "," #comma
      } else {
        decn = "." #dot
      }
    }
  } else if (dformat == 3) { #!decn is detect, but currently not used in the rest of the code!
    try(expr={deci = g.wavread(wavfile=inputfile,start=1,end=10)},silent=TRUE)
    deci = as.matrix(deci$rawxyz)
    if(is.na(suppressWarnings(as.numeric(deci[2,2]))) == T) {
      decn = "," #comma
    } else {
      decn = "." #dot
    }
  } else if (dformat == 4) { #!decn is detect, but currently not used in the rest of the code!
    # Rcpp::sourceCpp('src/numUnpack.cpp')
    # Rcpp::sourceCpp('src/resample.cpp')
    try(expr={deci = g.cwaread(inputfile,start = 1, end = 10, desiredtz = desiredtz)},silent=TRUE)
    deci = as.matrix(deci$data)
    if(is.na(suppressWarnings(as.numeric(deci[2,2]))) == T) {
      decn = "," #comma
    } else {
      decn = "." #dot
    }
  } else if (dformat == 5) { #!decn is detect, but currently not used in the rest of the code!
    decn = rmc.dec # simply pass on the input
  }
  dotorcomma = decn
}