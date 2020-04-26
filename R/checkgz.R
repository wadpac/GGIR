checkgz = function(filename) {
  # Check whether file is a gz file and if yes then open the connection
  if (length(unlist(strsplit(paste0(filename,"extra"),".gz"))) > 1) {
    datafile = gzfile(filename,'rt')
  }
  return(filename)
}