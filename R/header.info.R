# GGIR used to depend on CRAN package GENEAread as developed by Joss Langford and Zhou Fang.
# Now GENEAread is depricated the essential parts of the code has been copied to GGIR to ensure
# ongoing functionality.
header.info <- function (binfile, more = TRUE) {
  nobs = 300
  info <- vector("list", 15)
  #    index <- c(2, 20:22, 26:29)
  tmpd = readLines(binfile, 300)
  #try to find index positions - so will accomodate multiple lines in the notes sections
  #change when new version of binfile is produced.
  ind.subinfo = min(which((tmpd == "Subject Info" )& (1:length(tmpd) >= 37)))
  ind.memstatus = max(which(tmpd == "Memory Status"))
  ind.recdata = (which(tmpd == "Recorded Data"))
  ind.recdata = ind.recdata[ind.recdata > ind.memstatus][1:2]
  ind.calibdata = max(which(tmpd == "Calibration Data"))
  ind.devid = min(which(tmpd == "Device Identity"))
  ind.config = min(which(tmpd == "Configuration Info"))
  ind.trial = min(which(tmpd == "Trial Info"))
  
  index = c(ind.devid +1, ind.recdata[1] + 8, ind.config + 2:3, ind.trial + 1:4, ind.subinfo + 1:7, ind.memstatus + 1)
  if (max(index) == Inf) stop("Corrupt headers or not Geneactiv file!", call = FALSE)
  #read in header info
  nm <- NULL
  for (i in 1:length(index)) {
    line = strsplit(tmpd[index[i]], split = ":")[[1]]
    el = ""
    if (length(line) > 1) el <- paste(line[2:length(line)],collapse=":")
    info[[i]] <- el
    nm[i] <- paste(strsplit(line[1], split = " ")[[1]], collapse = "_")
  }
  info <- as.data.frame(matrix(info), row.names = nm)
  colnames(info) <- "Value"
  Decimal_Separator = "."
  if (length( grep(",", paste(tmpd[ind.memstatus + 8:9], collapse = "")) ) > 0){
    Decimal_Separator = ","
  }
  info = rbind(info, Decimal_Separator = Decimal_Separator )
  if (more){
    #grab calibration data etc as well
    calibration = list()
    fc = file(binfile, "rt")
    index = sort(c(ind.config + 4, ind.calibdata + 1:8, ind.memstatus+1, ind.recdata +3, ind.recdata[1] + c(2,8)))
    tmp <- substring(scan(fc, skip = index[1] - 1, what = "", n = 3, sep = " ", 
                          quiet = TRUE)[3], c(1,2,5),c(1, 3, 6))
    calibration$tzone = ifelse(tmp[1] == "-", -1, 1) * (as.numeric(tmp[3]) + 60* as.numeric(tmp[2])) /60
    index = diff(index) - 1
    for (sk in index[1:10]){
      calibration = c(calibration,as.integer(scan(fc, skip = sk, what = "", n = 2, sep = ":", quiet = TRUE)[2]))
    }
    names(calibration) = c("tzone", "xgain", "xoffset", "ygain", "yoffset", "zgain", "zoffset", "volts", "lux", "npages", "firstpage")
    t1 <- substring(scan(fc, skip = index[11], what = "", quiet = TRUE, nlines = 1, sep = "\n"), 11)
    freq = (scan(fc, skip = index[12], what = "", n = 2, sep = ":", quiet = TRUE)[2])
    if (Decimal_Separator == ",") freq = sub(",", ".", freq, fixed = TRUE)
    freq <- as.numeric(freq)
    #stop reading freq from file, calculate from page times instead (if possible)
    t1c <- parse.time(t1, format = "POSIX", tzone = calibration$tzone)
    t1midnight = floor(parse.time(t1, format = "day")) * 60*60*24
    t1 <- parse.time(t1, format = "seconds")
    inc = 1/freq
    
    if (calibration$npages > 1){
      t2 =  parse.time(substring(scan(fc, skip = index[13], what = "", quiet = TRUE, nlines = 1, sep = "\n"), 11), format = "seconds")
      freq = nobs/(t2 - t1)
      inc = (t2-t1)/nobs
    } 
    calibration =c(calibration, list(freq = freq, t1 = t1, t1c = t1c, inc = inc, t1midnight = t1midnight, headlines = ind.recdata[1]-1))
    close(fc)
    
    if (exists("mmap", mode = "function")){
      #mmap: find start offset and shift
      tmpd = mmap::mmap(binfile, mmap::char())
      #did we mmap successfully?
      if (mmap::is.mmap(tmpd)){
        tmpd2 = tmpd[1:min(length(tmpd), 20000)]
        tmp = grepRaw("Memory Status", tmpd2, all = T)
        if (length(tmp) > 1) tmp = max(tmp)
        #find byte offset between two records
        calibration$pos.rec1 = grepRaw("Recorded Data", tmpd2, offset = tmp)
        calibration$pos.inc = grepRaw("Recorded Data", tmpd2, offset = calibration$pos.rec1+1) - calibration$pos.rec1
        mmap::munmap(tmpd) # clean up
      } else {
        warning("MMAP failed! (Not enough address space?)")
        calibration$pos.rec1 = NA
        calibration$pos.inc = NA
      }
      if (length(calibration$pos.inc) == 0){
        warning("MMAP failed! Data corrupt or compressed?")
        calibration$pos.rec1 = NA
        calibration$pos.inc = NA
      }
    }
    attr(info, "calibration") = calibration
  }
  return(info)
}
