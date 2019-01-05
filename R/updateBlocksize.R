updateBlocksize = function(blocksize=c(), bsc_qc=data.frame(time=c(),size=c())) {
  if (length(blocksize) == 0) {
    cat("\nWarning: Please provide valid blocksize unequal to zero")
  }
  SI = Sys.info()
  sysname = SI[which(names(SI) == "sysname")]
  MemoryLimit = c()
  gc()
  if (sysname == "windows") {
    MemoryLimit = memory.limit() #MemoryLimit in MB
    MemoryUsed = memory.size() #MemoryUsed in MB
  } else { # If that fails then this is a Linux system
    SystemFree = system("free",intern=TRUE)
    # SystemFree is character vector
    # Now extract MemoryLimit and MemoryUsed from this
    SpaceSplitSecondRow = unlist(strsplit(SystemFree[2]," "))
    IsolatedValues = SpaceSplitSecondRow[which(SpaceSplitSecondRow != "")]
    MemoryLimit = as.numeric(IsolatedValues[2]) / 1000
    MemoryUsed = as.numeric(IsolatedValues[3]) / 1000
  }
  if (length(MemoryLimit) != 0) { #System recognised
    if (MemoryLimit < MemoryUsed) MemoryLimit = MemoryUsed
    bsc_qc = rbind(bsc_qc,c(as.character(Sys.time()),MemoryUsed))
    MemoryRatio = MemoryUsed / MemoryLimit

      gco = gc()
memuse = gco[2,2]
bsc_qc = rbind(bsc_qc,c(memuse,as.character(Sys.time())))
if (memuse > 4000) {
if (nrow(bsc_qc) <5){
blocksize = round(blocksize * 0.8)
}
}

    cat(paste0("\nMemoryRatio ",MemoryRatio," (MemoryUsed: ",MemoryUsed,"/ MemoryLimit: ",MemoryLimit,") memuse ",memuse,"   "))
#    if (MemoryRatio < 0.4) {
 #       blocksize = blocksize * 1.1
  #  } else if (MemoryRatio >= 0.8 & MemoryRatio < 0.9) {
   #   blocksize = blocksize * 0.9
   # } else if (MemoryRatio >= 0.9) {
   #   blocksize = blocksize * 0.8
   # }
   # blocksize = round(blocksize)
  }
  
  return(list(blocksize=blocksize, bsc_qc=bsc_qc))
}
