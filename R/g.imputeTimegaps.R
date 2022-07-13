g.imputeTimegaps = function(x, xyzCol, timeCol = c(), sf, k=0.25, impute = TRUE, LastValueInPrevChunk = c(0, 0, 1)) {
  remove_time_at_end = FALSE
  if (length(timeCol) == 0) { # add temporary timecolumn to enable timegap imputation where there are zeros
    dummytime = Sys.time()
    adhoc_time = seq(dummytime, dummytime + (nrow(x) - 1) * (1/sf), by = 1/sf)
    if (length(adhoc_time) < nrow(x)) { 
      NotEnough = nrow(x) - length(adhoc_time)
      adhoc_time = seq(dummytime, dummytime + (nrow(x) + NotEnough) * (1/sf), by = 1/sf)
    }
    x$time = adhoc_time[1:nrow(x)]
    timeCol = "time"
    remove_time_at_end = TRUE
  }
  zeros = which(x[,xyzCol[1]] == 0 & x[,xyzCol[2]] == 0 & x[,xyzCol[3]] == 0)
  if (length(zeros) > 0) {
    if (zeros[1] == 1) {
      zeros = zeros[-1]
      x[1, xyzCol] = LastValueInPrevChunk
    }
    x = x[-zeros,]
  }
  if (isTRUE(impute)) { # this is default, in g.calibrate this is set to FALSE
    if (k < 2/sf) { # prevent trying to impute timegaps shorter than 2 samples
      k = 2/sf
    }
    deltatime = diff(x[, timeCol])
    if (!is.numeric(deltatime)) {  # in csv axivity, the time is directly read as numeric (seconds)
    units(deltatime) = "secs"
    deltatime = as.numeric(deltatime)
    }
    gapsi = which(deltatime >= k) # limit imputation to gaps larger than 0.25 seconds
    NumberOfGaps = length(gapsi)
    if (NumberOfGaps > 0) { 
      # if gaps exist impute them by repeating the last known value
      x$gap = 1
      x$gap[gapsi] = as.integer(deltatime[gapsi] * sf) + 1 # an extra row should be added because the last known value is not part of the gap, which causes 1-row mismatch
      x <- as.data.frame(lapply(x, rep, x$gap))
      #  normalise last known value to 1 if it deviates more than 5 mg from 1
      i_normalise = which(x$gap != 1)
      en_lastknownvalue = sqrt(rowSums(x[i_normalise[1], xyzCol]^2))
      if ((abs(en_lastknownvalue) - 1) > 0.005) {
        if (length(i_normalise) > 0) {
          x[i_normalise, xyzCol] = x[i_normalise, xyzCol] / en_lastknownvalue
        }
      }
      x = x[, which(colnames(x) != "gap")]
    }
  }
  # Note: Timestamps are not imputed because from here onward GGIR does not need them
  # Any problems with sample rate should have been fixed during data loading
  if (remove_time_at_end == TRUE) {
    x = x[,-which(colnames(x) == "time")]
  }
  return(x)
}