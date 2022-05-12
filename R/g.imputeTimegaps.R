g.imputeTimegaps = function(x, xyzCol, timeCol = c(), sf, impute = TRUE) {
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

  zeros = which(rowSums(x[,xyzCol]) == 0)
  if (length(zeros) > 0) {
    if (zeros[1] == 1) {
      zeros = zeros[-1]
      x[1, xyzCol] = c(0, 0, 1)
    }
    x = x[-zeros,]
  }
  if (isTRUE(impute)) { # this is default, in g.calibrate this is set to FALSE
    print("impute gaps")
    # prevent trying to impute timegaps shorter than 5 seconds
    k = 5
    x$time_utc = as.POSIXct(x = as.numeric(x$time), tz = "UTC", origin = "1970-1-1")
    deltatime = diff(x[, "time_utc"]) #timeCol
    if (!is.numeric(deltatime)) {  # in csv axivity, the time is directly read as numeric (seconds)
      units(deltatime) = "secs"
      deltatime = as.numeric(deltatime)
    }
    gapsi = which(deltatime >= k) # limit imputation to gaps larger than 5 seconds
    NumberOfGaps = length(gapsi)
    if (sum(deltatime[gapsi]) > 3600 * 36) {
      stop(paste0("There is more than 36 hours of missing data to be imputed.",
                  " GGIR can currently not handle this. As a temporary solution: ", 
                  "If you are using ActiGraph gt3x data export the data to csv ",
                  "with the ActiLife software."))
    }
    if (NumberOfGaps > 0) { 
      # if gaps exist impute them by repeating the last known value
      x$gap = 1
      x$gap[gapsi] = as.integer(deltatime[gapsi] * sf) # convert to samples, because we impute samples not seconds
      print(summary(deltatime[gapsi]))
      largegap = which(x$gap > 120 * sf)
      if (length(largegap) > 0) {
        for (i3 in 1:length(largegap)) {
          print(x[(largegap[i3] - 1):(largegap[i3] + 1),])
        }
      }
      
      x <- as.data.frame(lapply(x, rep, x$gap))
      #  normalise last known value to 1
      i_normalise = which(x$gap != 1)
      if (length(i_normalise) > 0) {
        x[i_normalise, xyzCol] = x[i_normalise, xyzCol] / sqrt(rowSums(x[i_normalise, xyzCol]^2))
      }
      x = x[, which(colnames(x) != "gap")]
    }
    x = x[,-which(colnames(x) == "time_utc")]
  }
  # Note: Timestamps are not imputed because from here onward GGIR does not need them
  # Any problems with sample rate should have been fixed during data loading
  if (remove_time_at_end == TRUE) {
    x = x[,-which(colnames(x) == "time")]
  }
  return(x)
}