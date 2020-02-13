applyExtFunction = function(data, myfun, sf, ws3) {
  # check myfun object
  check_myfun(myfun, windowsizes=ws3) 
  # unit correction
  unitcorrection = 1 # default is g
  if (myfun$expected_unit != "g") { 
    if (myfun$expected_unit == "mg") {
      unitcorrection = 1000
    } else if (myfun$expected_unit == "ms2") {
      unitcorrection = 9.81
    }
  }
  if (is.logical(myfun$timestamp) == TRUE) {
    myfun$timestamp = c() 
    warning(paste0("Note: If function applyExtFunction is used directly",
                   " then object myfun cannnot carry a logical value",
                   " because the timestamp can only be added in the g.getmeta function",
                   " from which applyExtFunction is called. However,",
                   " you can provide a numeric value to indicate",
                   " start time in seconds since 1-1-1970"))
  }
  # sample rate correction
  if (sf != myfun$expected_sample_rate) {
    resampleAcc = function(rawAccel, sf, myfun) {
      step_old = 1/sf
      step_new = 1/myfun$expected_sample_rate
      start = 0
      end = nrow(data)/sf
      rawTime = seq(start, end, step_old)
      timeRes = seq(start, end, step_new)
      rawTime = rawTime[2:length(rawTime)]
      timeRes = timeRes[2:length(timeRes)]
      nr = length(timeRes) - 1
      timeResNew = as.vector(timeRes[1:nr])
      accelRes = matrix(0,nrow = nr, ncol = 3, dimnames = list(NULL, c("x", "y", "z")))
      # at the moment the function is designed for reading the r3 acceleration channels only,
      # because that is the situation of the use-case we had.
      rawLast = nrow(rawAccel)
      accelRes = resample(rawAccel, rawTime, timeRes, rawLast) # this is now the resampled acceleration data
      return(accelRes)
    }
    if (length(myfun$timestamp) == 1) { #resample, but do not apply function yet, because timestamp also needs to be added
        data = resampleAcc(data, sf, myfun)
    } else { # resample and apply function, because timestamp is not needed
      OutputExternalFunction = myfun$FUN(resampleAcc(data, sf, myfun) * unitcorrection, myfun$parameters)
    }
  } 
  if (length(myfun$timestamp) == 1) { # add timestamp and apply function
    st_num = as.numeric(myfun$timestamp) #POSIX converted to numeric time but relative to the desiredtz
    data = cbind(seq(st_num, (st_num + ((nrow(data)-1)*ws3)),by=ws3), data)
    OutputExternalFunction = myfun$FUN(data * unitcorrection, myfun$parameters)
  } else if (length(myfun$timestamp) == 0  & sf == myfun$expected_sample_rate ){ # if no resampling and timestamp column was needed
    OutputExternalFunction = myfun$FUN(data * unitcorrection, myfun$parameters)
  }
  # output resolution correction (either aggregate or repeat)
  if (myfun$outputtype == "numeric") { # aggregation is possible with averaging, possibly later also allow for character output
    if (is.null(dim(OutputExternalFunction))) { # if OutputExternalFunction is a simple vector then convert it to 1 column matrix
      OutputExternalFunction = as.matrix(OutputExternalFunction)
      if (ncol(OutputExternalFunction) != 1 & nrow(OutputExternalFunction) == 1) OutputExternalFunction = t(OutputExternalFunction)
    }
    if (myfun$outputres < ws3) { # if function produces higher resolution output (shorter epoch length) then aggregate rows
      agglevel = rep(1:nrow(OutputExternalFunction)+(3*(ws3/myfun$outputres)),each=ws3/myfun$outputres)
      agglevel = agglevel[1:nrow(OutputExternalFunction)]
      OEF = data.frame(OutputExternalFunction, agglevel=agglevel)
      OEFA = aggregate(OEF,by=list(OEF$agglevel),FUN=myfun$aggfunction)
      OutputExternalFunction = OEFA[,-c(1,ncol(OEFA))]
      # OutputExternalFunction is now aggregated to ws3 which will enable merging it with metashort
    } else if (myfun$outputres > ws3) { # if function produces longer epoch length then repeat rows
      OutputExternalFunction = OutputExternalFunction[rep(seq_len(nrow(OutputExternalFunction)), each = myfun$outputres/ws3), ] 
    }
  }
  return(OutputExternalFunction)
}