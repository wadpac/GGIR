g.imputeTimegaps = function(x, xyzCol, timeCol, sf, k=0.25) {
  zeros = which(rowSums(x[,xyzCol]) == 0)
  if (length(zeros) > 0) {
    if (zeros[1] == 1) {
      zeros = zeros[-1]
      x[1, xyzCol] = c(0, 0, 1)
    }
    x = x[-zeros,]
  }
  if (k < 2/sf) { # prevent trying to impute timegaps shorter than 2 samples
    k = 2/sf
  }
  deltatime = diff(x[, timeCol])
  units(deltatime) = "secs"
  deltatime = as.numeric(deltatime)
  gapsi = which(deltatime >= k) # limit imputation to gaps larger than 0.25 seconds
  NumberOfGaps = length(gapsi)
  if (NumberOfGaps > 0) { 
    # if gaps exist impute them by repeating the last known value
    x$gap = 1
    x$gap[gapsi] = as.integer(deltatime[gapsi] * sf) 
    x <- as.data.frame(lapply(x, rep, x$gap))
    #  normalise last known value to 1
    i_normalise = which(x$gap != 1)
    if (length(i_normalise) > 0) {
      x[i_normalise, xyzCol] = x[i_normalise, xyzCol] / sqrt(rowSums(x[i_normalise, xyzCol]^2))
    }
    # Timestamps are not imputed because from here onward GGIR does not need them
    # Any problems with sample rate should have been fixed before this point
    x = x[, which(colnames(x) != "gap")]
  }
  return(x)
}