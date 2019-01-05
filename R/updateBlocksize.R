updateBlocksize = function(blocksize=c(), bsc_qc=data.frame(time=c(),size=c())) {
  if (length(blocksize) == 0) {
    cat("\nWarning: Please provide valid blocksize unequal to zero")
  }
  SI = Sys.info()
  sysname = SI[which(names(SI) == "sysname")]
  MemoryLimit = c()
  if (sysname == "windows") {
    MemoryLimit = memory.limit() # try Windows command
  } else { # If that fails then this is a Linux system
    MemoryLimit = as.numeric(system("awk '/MemFree/ {print $2}' /proc/meminfo", intern=TRUE)) / 1000
  }
  if (length(MemoryLimit) != 0) { #System recognised
    MemoryUsed = gc()[2,2] #MemoryUsed in mb
    bsc_qc = rbind(bsc_qc,c(as.character(Sys.time()),MemoryUsed))
    MemoryRatio = MemoryUsed / MemoryLimit
    if (MemoryRatio < 0.3) {
      blocksize = blocksize * 2
    } else if (MemoryRatio >= 0.3 & MemoryRatio < 0.5) {
      blocksize = blocksize * 1.5
    } else if (MemoryRatio >= 0.5 & MemoryRatio < 0.7) {
      blocksize = blocksize * 1.1
    } else if (MemoryRatio >= 0.8) {
      blocksize = blocksize * 0.8
    }
    blocksize = round(blocksize)
  }
  
  return(list(blocksize=blocksize, bsc_qc=bsc_qc))
}