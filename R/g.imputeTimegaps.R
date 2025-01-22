g.imputeTimegaps = function(x, sf, k=0.25, impute = TRUE, 
                            PreviousLastValue = c(0, 0, 1), PreviousLastTime = NULL, 
                            epochsize = NULL) {
  if (!is.null(epochsize)) {
    shortEpochSize = epochsize[1]
    longEpochSize = epochsize[2]
  }
  # dummy variables to control the process
  remove_time_at_end = FirstRowZeros = imputelast = FALSE
  # initialize numberofgaps and GapsLength
  NumberOfGaps = GapsLength = 0

  # add temporary timecolumn to enable timegap imputation where there are zeros
  if (!("time" %in% colnames(x))) { 
    x$time = seq(from = Sys.time(), by = 1/sf, length.out = nrow(x))
    remove_time_at_end = TRUE
  }
  
  xyzCol = which(colnames(x) %in% c("x", "y", "z"))

  # define function for imputation at raw level
  imputeRaw = function(x, sf) {
    # impute raw timestamps because timestamps still need to be meaningful when
    # resampling or when plotting
    gapp = which(x$gap != 1)
    if (length(gapp) > 0) {
      if (gapp[1] > 1) {
        newTime = x$time[1:(gapp[1] - 1)]
      } else {
        newTime = NULL
      }
      for (g in 1:length(gapp)) {
        newTime = c(newTime, x$time[gapp[g]] + seq(0, by = 1/sf, length.out = x$gap[gapp[g]]))
        if (g < length(gapp)) {
          newTime = c(newTime, x$time[(gapp[g] + 1):(gapp[g + 1] - 1)])
        }
      }
      newTime =  c(newTime, x$time[(gapp[g] + 1):length(x$time)])
    }
    x <- as.data.frame(lapply(x, rep, x$gap))
    
    if (length(gapp) > 0) {
      x$time = newTime[1:nrow(x)]
    }
    x = x[, which(colnames(x) != "gap")]
    return(x)
  }
  
  # find zeros and remove them from dataset
  zeros = which(x$x == 0 & x$y == 0 & x$z == 0)
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
    deltatime = diff(x$time)
    if (!is.numeric(deltatime)) {  # in csv axivity, the time is directly read as numeric (seconds)
      units(deltatime) = "secs"
      deltatime = as.numeric(deltatime)
    }
    # refill if first value is not consecutive from last value in previous chunk
    if (!is.null(PreviousLastTime)) {
      if (!inherits(x = PreviousLastTime, what = "numeric") &&
          inherits(x = x$time[1], what = "numeric")) {
        PreviousLastTime = as.numeric(PreviousLastTime)
      }
      first_deltatime = diff(c(PreviousLastTime, x$time[1]))
      if (!is.numeric(first_deltatime)) {  # in csv axivity, the time is directly read as numeric (seconds)
        units(first_deltatime) = "secs"
        first_deltatime = as.numeric(first_deltatime)
      }
      if (first_deltatime >= k) { # don't impute a timegap shorter than the minimum requested
        x = rbind(x[1,], x)
        x$time[1] = PreviousLastTime
        x[1, xyzCol] = PreviousLastValue
        deltatime = c(first_deltatime, deltatime)
      }
    }
    # impute time gaps
    gapsi = which(deltatime >= k) # limit imputation to gaps longer than the minimum requested
    NumberOfGaps = length(gapsi)
    if (NumberOfGaps > 0) {
      x$gap = 1
      x$gap[gapsi] = round(deltatime[gapsi] * sf)   # as.integer was problematic many decimals close to wholenumbers (but not whole numbers) resulting in 1 row less than expected
      GapsLength = sum(x$gap[gapsi])
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
          Rversion_lt_420 = as.numeric(R.Version()$major) >= 4 && as.numeric(R.Version()$minor) > 2
          for (i in gap90i) { 
            imp = imp + 1
            # In the next bit of code we use data.table to extract hour and minute from timestamps
            # however this does not work for older version of R
            if (Rversion_lt_420) {
              time_i = x$time[i]
              time_ip1 = x$time[i + 1]
            } else {
              # In R 4.2 and early numeric time cannot be provide to data.table
              # In GMT because at this point we do not care about the specific time
              # because this is removed at the end of the function, we only use it
              # to identify time gaps and their size
              time_i = as.POSIXct(x$time[i], origin = "1970-01-01", tz = "GMT")
              time_ip1 = as.POSIXct(x$time[i + 1], origin = "1970-01-01", tz = "GMT")
            }
            # short epochs to add to fill up to next long epoch cut
            seconds = data.table::hour(time_i) * 60^2 + data.table::minute(time_i) * 60 + data.table::second(time_i) + 1 
            seconds_from_prevCut = seconds - max(longEpochDayCut[which(longEpochDayCut <= seconds)])
            shortEpochs2add_1 = (longEpochSize - seconds_from_prevCut) / shortEpochSize
            # short epochs to add after time gap
            seconds = data.table::hour(time_ip1) * 60^2 + data.table::minute(time_ip1) * 60 + data.table::second(time_ip1) + 1 
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
    if (FirstRowZeros == TRUE) x = x[-1,] # since zeros[1] was removed
    if (imputelast == TRUE) x = x[-nrow(x),] # since zeros[length(zeros)] was removed
  }
  # impute last value?
  if (imputelast) x[nrow(x), xyzCol] = x[nrow(x) - 1, xyzCol]

  # QClog
  start = as.numeric(as.POSIXct(x$time[1], origin = "1970-1-1"))
  end = start + nrow(x)
  imputed = NumberOfGaps > 0
  QClog = data.frame(imputed = imputed, 
                     start = start, end = end,
                     blockLengthSeconds = (end - start) / sf,
                     timegaps_n = NumberOfGaps, timegaps_min = GapsLength/sf/60)

  if (remove_time_at_end == TRUE) {
    x = x[, grep(pattern = "time", x = colnames(x), invert = TRUE)]
  }

  # return data and QClog
  return(list(x = x, QClog = QClog))
}