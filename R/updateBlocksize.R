updateBlocksize = function(blocksize=c(), bsc_qc=data.frame(time=c(),size=c())) {
  if (length(blocksize) == 0) {
    cat("\nWarning: Please provide valid blocksize unequal to zero")
  }
  gco = gc()
  memuse = gco[2,2] #memuse in mb
  bsc_qc = rbind(bsc_qc,c(as.character(Sys.time()),memuse))
  if (memuse > 4000) {
    if (nrow(bsc_qc) < 5) {
      blocksize = round(blocksize * 0.8)
    }
  }
  blocksize = round(blocksize)
  return(list(blocksize=blocksize, bsc_qc=bsc_qc))
}
