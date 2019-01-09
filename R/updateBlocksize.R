updateBlocksize = function(blocksize=c(), bsc_qc=data.frame(time=c(),size=c())) {
  if (length(blocksize) == 0) {
    cat("\nWarning: Please provide valid blocksize unequal to zero")
  }
  gco = gc()
  memuse = gco[2,2] #memuse in mb
  bsc_qc_new_row = data.frame(time=as.character(Sys.time()),size=memuse,stringsAsFactors = FALSE)
  if (nrow(bsc_qc) == 0) {
    bsc_qc = bsc_qc_new_row
  } else {
    bsc_qc = rbind(bsc_qc,bsc_qc_new_row)
  }
  if (memuse > 4000) {
    if (nrow(bsc_qc) < 5) {
      blocksize = round(blocksize * 0.8)
    }
  }
  blocksize = round(blocksize)
  return(list(blocksize=blocksize, bsc_qc=bsc_qc))
}
