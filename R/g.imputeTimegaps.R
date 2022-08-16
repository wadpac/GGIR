g.imputeTimegaps = function(x, xyzCol, timeCol = c(), sf, k=0.25, impute = TRUE, 
                            PreviousLastValue = c(0, 0, 1), PreviousLastTime = NULL) {
  # dummy variables to control the process
  remove_time_at_end = dummyTime = FirstRowZeros = imputelast = FALSE
  # add temporary timecolumn to enable timegap imputation where there are zeros
  if (length(timeCol) == 1) {
    if (!(timeCol %in% colnames(x))) dummyTime = TRUE
  }
  if (length(timeCol) == 0 | isTRUE(dummyTime)) { 
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
  # find zeros and remove them from dataset
  zeros = which(x[,xyzCol[1]] == 0 & x[,xyzCol[2]] == 0 & x[,xyzCol[3]] == 0)
  if (length(zeros) > 0) {
    # if first value is a zero, remember value from previous chunk to replicate
    # if chunk = 1, then it will use c(0, 0, 1)
    if (zeros[1] == 1) {
      zeros = zeros[-1]
      x[1, xyzCol] = PreviousLastValue
      FirstRowZeros = TRUE
    }
    # if last value is a zero, we should not remove it (to keep track of the time)
    # This last row of zeros will be imputed afterwards (i.e., imputelast = TRUE)
    if (zeros[length(zeros)] == nrow(x)) {
      zeros = zeros[-length(zeros)]
      imputelast = TRUE
    }
    x = x[-zeros,]
  }
  # find missing timestamps (timegaps)
  if (isTRUE(impute)) { # this is default, in g.calibrate this is set to FALSE
    if (k < 2/sf) { # prevent trying to impute timegaps shorter than 2 samples
      k = 2/sf
    }
    deltatime = diff(x[, timeCol])
    if (!is.numeric(deltatime)) {  # in csv axivity, the time is directly read as numeric (seconds)
      units(deltatime) = "secs"
      deltatime = as.numeric(deltatime)
    }
    # refill if first value is not consecutive from last value in previous chunk
    if (!is.null(PreviousLastTime)) {
      first_deltatime = diff(c(PreviousLastTime, x[1, timeCol]))
      if (!is.numeric(first_deltatime)) {  # in csv axivity, the time is directly read as numeric (seconds)
        units(first_deltatime) = "secs"
        first_deltatime = as.numeric(first_deltatime)
      }
      if (first_deltatime >= k) { # prevent trying to impute timegaps shorter than 2 samples
        x = rbind(x[1,], x)
        x[1, timeCol] = PreviousLastTime
        x[1, xyzCol] = PreviousLastValue
        deltatime = c(first_deltatime, deltatime)
      }
    }
    # impute time gaps
    gapsi = which(deltatime >= k) # limit imputation to gaps larger than 0.25 seconds
    NumberOfGaps = length(gapsi)
    if (NumberOfGaps > 0) {
      x$gap = 1
      x$gap[gapsi] = round(deltatime[gapsi] * sf)   # as.integer was problematic many decimals close to wholenumbers (but not whole numbers) resulting in 1 row less than expected
      #  normalisation to 1 G 
      normalise = which(x$gap > 1)
      for (i_normalise in normalise) {
        en_lastknownvalue = sqrt(rowSums(x[i_normalise, xyzCol]^2))
        if ((abs(en_lastknownvalue) - 1) > 0.005) {   # only if it deviates more than 5 mg from 1 G
          x[i_normalise, xyzCol] = x[i_normalise, xyzCol] / en_lastknownvalue
        }
      }
      x <- as.data.frame(lapply(x, rep, x$gap))
      x = x[, which(colnames(x) != "gap")] # remove column gap because g.getmeta cannot handle this extra column yet, when reading multiple chunks of data
    }
  } else if (isFALSE(impute)) {
    if (isTRUE(FirstRowZeros)) x = x[-1,] # since zeros[1] was removed in line 21
    if (isTRUE(imputelast)) x = x[-nrow(x),] # since zeros[length(zeros)] was removed in line 27
  }
  # impute last value?
  if (imputelast) x[nrow(x), xyzCol] = x[nrow(x) - 1, xyzCol]
  # Note: Timestamps are not imputed because from here onward GGIR does not need them
  # Any problems with sample rate should have been fixed during data loading
  if (remove_time_at_end == TRUE) {
    x = x[,-which(colnames(x) == "time")]
  }
  return(x)
}