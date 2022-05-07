g.dotorcomma = function(inputfile, dformat, mon, desiredtz = "", ...) {
  #get input variables (relevant when read.myacc.csv is used)
  input = list(...)
  decn = getOption("OutDec") # extract system decimal separator
  if (length(decn) == 0) decn = "." # assume . if not retrieved
  if (length(input) > 0) {
    for (i in 1:length(names(input))) {
      txt = paste0(names(input)[i], "=", input[i])
      if (is(unlist(input[i]), "character")) {
        txt = paste0(names(input)[i], "='", unlist(input[i]), "'")
      }
      eval(parse(text = txt))
    }
  }
  #------------------------------------------------------------
  if (exists("rmc.firstrow.acc") == FALSE) rmc.firstrow.acc = c()
  if (exists("rmc.dec") == FALSE) rmc.dec = decn
  if (length(rmc.firstrow.acc) == 1) {
    dformat = 5
    mon = 5
    decn = rmc.dec
  }
  if (dformat == 2) {
    skiprows = 100
    # Note: I have added the below lines because some ActiGraph files start with a
    # lot of zeros, which makes it impossible to detect decimal separator
    # "." will then be the default, which is not correct for "," systems.
    while (skiprows < 1000000) { #foundnonzero == FALSE & 
      deci = as.matrix(read.csv(inputfile, skip = skiprows, nrow = 10))
      skiprows = skiprows + 10000
      if (length(unlist(strsplit(as.character(deci[2,2]), ","))) > 1) {
        decn = ","
        break() 
      }
      numtemp = as.numeric(deci[2,2])
      if (is.na(numtemp) == FALSE) {
        if (numtemp != 0) break()
      }
    }
    if (!exists("deci")) stop("Problem with reading .csv file in GGIR function dotorcomma")
    if (is.na(suppressWarnings(as.numeric(deci[2,2]))) == T & decn == ".") decn = ","
  } else if (dformat == 1) {
    if (mon == 1) {
      # GENEA values are stroed in g without decimal place.
      # try(expr={deci = g.binread(inputfile,0,2)},silent=TRUE)
      # if(is.na(suppressWarnings(as.numeric(deci[2,2]))) == T) decn = ","
    } else if (mon == 2 ) {
      if ("GENEAread" %in% rownames(installed.packages()) == FALSE) {
        cat("\nWarning: R package GENEAread has not been installed, please install it before continuing")
      }
      try(expr = {deci = GENEAread::read.bin(binfile = inputfile, start = 1,end = 3,mmap.load = FALSE, calibrate = TRUE)}, silent = TRUE)
      if (!exists("deci")) stop("Problem with reading .bin file in GGIR function dotorcomma")
      if (is.na(as.numeric(deci$data.out[2, 2])) == T & decn == ".") decn = ","
    }
  } else if (dformat == 3) {
    try(expr = {deci = g.wavread(wavfile = inputfile,start = 1, end = 10)}, silent = TRUE)
    if (!exists("deci")) stop("Problem with reading .wav file in GGIR function dotorcomma")
    if (is.na(suppressWarnings(as.numeric(deci$rawxyz[2,2]))) == T & decn == ".") decn = ","
  } else if (dformat == 4) {
    try(expr = {deci = g.cwaread(fileName = inputfile,start = 1, end = 10, desiredtz = desiredtz,
                               interpolationType = 1)$data},silent = TRUE)
    if (!exists("deci")) stop("Problem with reading .cwa file in GGIR function dotorcomma")
    if (is.na(suppressWarnings(as.numeric(deci[2,2]))) == T & decn == ".") decn = ","
  } else if (dformat == 6) { # .gt3x
    if (length(grep(pattern = "[.]GT", x = inputfile)) > 0 & file.exists(inputfile) == FALSE) {
      inputfile = gsub(pattern = "[.]GT3X", replacement = "[.]gt3x", x = inputfile)
    }
    try(expr = {deci = as.data.frame(read.gt3x_ggir(path = inputfile,
                                                       batch_begin = 1, batch_end = 10, 
                                                       asDataFrame = TRUE))}, silent = TRUE)
    if (!exists("deci")) stop("Problem with reading .gt3x file in GGIR function dotorcomma")
    if (is.na(suppressWarnings(as.numeric(deci[2,2]))) == T & decn == ".") decn = ","
  }
  dotorcomma = decn
}
