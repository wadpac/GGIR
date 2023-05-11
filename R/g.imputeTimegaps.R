g.imputeTimegaps = function(x, xyzCol, timeCol = c(), sf, k=0.25, impute = TRUE, 
                            PreviousLastValue = c(0, 0, 1), PreviousLastTime = NULL, 
                            epochsize = NULL) {
  if (!is.null(epochsize)) {
    shortEpochSize = epochsize[1]
    longEpochSize = epochsize[2]
  }
  # dummy variables to control the process
  remove_time_at_end = dummyTime = FirstRowZeros = imputelast = FALSE
  # add temporary timecolumn to enable timegap imputation where there are zeros
  if (length(timeCol) == 1) {
    if (!(timeCol %in% colnames(x))) dummyTime = TRUE
  }
  if (length(timeCol) == 0 | dummyTime == TRUE) { 
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
  
  # define function to imputation at raw level
  imputeRaw = function(x, sf) {
    # impute raw timestamps because timestamps still need to be meaningul when
    # resampling or when plotting
    gapp = which(x$gap != 1)
    if (length(gapp) > 0) {
      if (gapp[1] > 1) {
        newTime = x$timestamp[1:(gapp[1] - 1)]
      } else {
        newTime = NULL
      }
      for (g in 1:length(gapp)) {
        newTime = c(newTime, x$timestamp[gapp[g]] + seq(0, (x$gap[gapp[g]] - 1) / sf, by = 1/sf))
        if (g < length(gapp)) {
          newTime = c(newTime, x$timestamp[(gapp[g] + 1):(gapp[g + 1] - 1)])
        }
      }
      newTime =  c(newTime, x$time[(gapp[g] + 1):length(x$timestamp)])
    }
    x <- as.data.frame(lapply(x, rep, x$gap))
    
    if (length(gapp) > 0) {
      x$timestamp = newTime[1:nrow(x)]
    }
    x = x[, which(colnames(x) != "gap")]
    return(x)
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
    if (length(zeros) > 0) { # safe check just in case that removing zeros[1] have made length(zeros) == 0
      if (zeros[length(zeros)] == nrow(x)) {
        zeros = zeros[-length(zeros)]
        imputelast = TRUE
      }
    }
    # Again, check that length(zeros) is still > 0 
    if (length(zeros) > 0) x = x[-zeros,]
  }
  # find missing timestamps (timegaps)
  if (impute == TRUE) { # this is default, in g.calibrate this is set to FALSE
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
      imputation_done = FALSE
      if (!is.null(epochsize)) {
        # identify gaps larger than 6 long epochs (defaults to 90 minutes) or
        # 90 minutes, where the highest of the two is the criterium
        GapLimit =  max(c(((longEpochSize / 60) * 6), 90)) * 60 * sf
        gap90 = ifelse(x$gap > GapLimit, x$gap, 1) # keep track of gaps > 90 min
        gap90i = which(gap90 > 1)
        if (length(gap90i) > 0) {
          # if gap > 90 min, then impute only to fill up the long epoch
          # and keep track of how many epochs to impute
          x$remaining_epochs = 1
          x$next_epoch_delay = 0
          longEpochDayCut = seq(0, 24 * 60^2, by = longEpochSize)
          x$imputation = 0; imp = 0 # keep track of the imputation to organize data later on
          for (i in gap90i) { 
            imp = imp + 1
            # short epochs to add to fill up to next long epoch cut
            seconds = data.table::hour(x$time[i]) * 60^2 + data.table::minute(x$time[i]) * 60 + data.table::second(x$time[i]) + 1 
            seconds_from_prevCut = seconds - max(longEpochDayCut[which(longEpochDayCut <= seconds)])
            shortEpochs2add_1 = (longEpochSize - seconds_from_prevCut) / shortEpochSize
            # short epochs to add after time gap
            seconds = data.table::hour(x$time[i + 1]) * 60^2 + data.table::minute(x$time[i + 1]) * 60 + data.table::second(x$time[i + 1]) + 1 
            seconds_from_prevCut = seconds - max(longEpochDayCut[which(longEpochDayCut <= seconds)])
            shortEpochs2add_2 = (seconds_from_prevCut - 1) / shortEpochSize
            # short epochs to add - total
            shortEpochs2add = shortEpochs2add_1 + shortEpochs2add_2
            time2add = (shortEpochs2add * sf * shortEpochSize) + 1
            x$remaining_epochs[i] = ((x$gap[i] - time2add) / (sf * shortEpochSize)) + 1   # plus 1 to count current epoch
            x$gap[i] = time2add # redefine gap to only fill up to next epoch
            x$imputation[i] = imp
            # if remaining_epochs[i] has decimal places, there is part of the next 
            # epoch (after the time gap) that we also should impute now.
            decs = x$remaining_epochs[i] - floor(x$remaining_epochs[i])
            if (decs > 0) {
              x$next_epoch_delay[i] = (decs * (sf * shortEpochSize))
              x$gap[i] = x$gap[i] +  x$next_epoch_delay[i] # redefine gap to fill up the first epoch after the gap
              x$remaining_epochs[i] = x$remaining_epochs[i] - (x$next_epoch_delay[i] / (sf * shortEpochSize)) 
            } 
          }
          x$gap = round(x$gap) # to make sure that small decimals do not mess up the imputation in next line
          x$next_epoch_delay = round(x$next_epoch_delay)
          
          x = imputeRaw(x, sf)

          imputation_done = TRUE
          # when imputing, the track of remaining_epochs has been repeated through the data frame
          # let's keep only the last record for the imputation later on
          keep_remaining_epochs = data.frame(index = which(x$remaining_epochs > 1),
                                             delay = x$next_epoch_delay[which(x$remaining_epochs > 1)],
                                             imp = x$imputation[which(x$remaining_epochs > 1)])
          keep_remaining_epochs$index = keep_remaining_epochs$index - keep_remaining_epochs$delay
          points2keep = aggregate(index ~ imp, data = keep_remaining_epochs, FUN = max)
          points2keep = points2keep$index
          # when turn_to_one is 1, then the record in x should also be 1 (this way we only keep the last record)
          x$remaining_epochs[-c(points2keep)] = 1 
          # cleanup x, we only need remaining epochs
          x = x[, which(colnames(x) != "next_epoch_delay")]
          x = x[, which(colnames(x) != "imputation")]
        }
      }
      if (imputation_done == FALSE) {
        x = imputeRaw(x, sf)
      }
    }
  } else if (impute == FALSE) {
    if (FirstRowZeros == TRUE) x = x[-1,] # since zeros[1] was removed in line 21
    if (imputelast == TRUE) x = x[-nrow(x),] # since zeros[length(zeros)] was removed in line 27
  }
  # impute last value?
  if (imputelast) x[nrow(x), xyzCol] = x[nrow(x) - 1, xyzCol]
  if (remove_time_at_end == TRUE) {
    x = x[, grep(pattern = "time", x = colnames(x), invert = TRUE)]
  }
  if (all(c("time", "timestamp") %in% colnames(x))) {
    x = x[, grep(pattern = "timestamp", x = colnames(x), invert = TRUE)]
  }
  return(x)
}