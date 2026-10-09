applyExtFunction = function(data, myfun, sf, ws3,interpolationType=1) {
  data = data[, c("x", "y", "z")] # Needed because code below is not able to handle time column yet
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
                   " start time in seconds since 1-1-1970"), call. = FALSE)
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
      accelRes = GGIRread::resample(rawAccel, rawTime, timeRes, rawLast, 
                                    type=interpolationType) # this is now the resampled acceleration data
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
    # Note that sample rate is now the expected sample rate, not the original sample rate
    time2beAdded = seq(st_num, (st_num + round(nrow(data)/myfun$expected_sample_rate)),by=1/myfun$expected_sample_rate) 
    LEtim = length( time2beAdded)
    NRda = nrow(data)
    if (LEtim > NRda) {
      time2beAdded = time2beAdded[1:NRda]
    } else if (LEtim < NRda) {
      time2beAdded = c(time2beAdded,time2beAdded[LEtim]+(c(1:(NRda-LEtim))/myfun$expected_sample_rate))
    }
    data = cbind(time2beAdded, data)
    OutputExternalFunction = myfun$FUN(data * unitcorrection, myfun$parameters)
  } else if (length(myfun$timestamp) == 0  & sf == myfun$expected_sample_rate ){ # if no resampling and timestamp column was needed
    OutputExternalFunction = myfun$FUN(data * unitcorrection, myfun$parameters)
  }

  # if OutputExternalFunction is a simple vector then convert it to 1 column matrix  
  if (is.null(dim(OutputExternalFunction))) { 
    OutputExternalFunction = matrix(OutputExternalFunction, ncol = 1)
  }
  
  # Validate number of output columns
  has_timestamp = length(myfun$timestamp) == 1
  n_output_columns = ncol(OutputExternalFunction)
  n_expected_columns = length(myfun$colnames)
  
  if (n_output_columns == n_expected_columns) {
    has_timestamp_output = FALSE
  } else if (has_timestamp &&
             n_output_columns == n_expected_columns + 1) {
    has_timestamp_output = TRUE
  } else {
    stop(
      paste0(
        "The number of columns returned by myfun$FUN (",
        n_output_columns,
        ") does not match the expected number of output columns (",
        n_expected_columns,
        if (has_timestamp) {
          paste0(
            " or ",
            n_expected_columns + 1,
            " when the timestamp is also returned"
          )
        } else {
          ""
        },
        ")."
      ),
      call. = FALSE
    )
  }
  
  # If timestamp is returned by FUN, temporarily separate it from
  # the output matrix so that only external function outputs are processed
  if (has_timestamp_output) {
    timestamp = OutputExternalFunction[, 1, drop = FALSE]
    OutputExternalFunction = OutputExternalFunction[, -1, drop = FALSE]
  }
  
  # Recycle outputtype across all output columns when a single value is provided
  if (length(myfun$outputtype) == 1) {
    outputtype = rep(myfun$outputtype, ncol(OutputExternalFunction))
  } else {
    outputtype = myfun$outputtype
  }
  
  # Output resolution correction for each column
  OutputExternalFunction_processed = vector("list", ncol(OutputExternalFunction))
  LevelsExternalFunction = vector("list", ncol(OutputExternalFunction))
  
  # Now process each column independently (aggregation function might be different for each column)
  is_factor = NULL
  
  for (out_idx in 1:ncol(OutputExternalFunction)) {
    
    # focus on this column
    output = OutputExternalFunction[, out_idx, drop = FALSE]
    
    # if this column is a factor, store levels
    #               if character, then turn into factor and levels = alphabetical order
    if (is.factor(output[, 1])) {
      is_factor = c(is_factor, out_idx)
    } else if (is.character(output[, 1])) {
      output[, 1] = factor(output[, 1], 
                           levels = sort(unique(output[, 1])))
      is_factor = c(is_factor, out_idx)
    }
    LevelsExternalFunction[[out_idx]] = levels(output[, 1])
    
    # Determine aggfunction for this column
    if (is.list(myfun$aggfunction)) {
      aggfunction = myfun$aggfunction[[out_idx]]
    } else {
      aggfunction = myfun$aggfunction
    }
    
    # Now correct output resolution
    if (outputtype[out_idx] %in% c("numeric", "character")) {
      
      # Function output has higher resolution than GGIR windowsize[1]
      if (myfun$outputres < ws3) { 
        
        if (ws3 %% myfun$outputres != 0) {
          stop(paste0("Output resolution (", myfun$outputres,
                      " s) for column ", myfun$colnames[out_idx],
                      "does not divide the GGIR epoch length (",
                      ws3, " s)."))
        }
        
        n_per_epoch = ws3 / myfun$outputres
        
        agglevel = rep(seq_len(ceiling(nrow(output) / n_per_epoch)), 
                       each = n_per_epoch)
        agglevel = agglevel[seq_len(nrow(output))]
        
        OEF = data.frame(value = output[, 1], agglevel = agglevel)
        OEFA = aggregate(value ~ agglevel, data = OEF, FUN = aggfunction)
        
        output = OEFA["value"]
        
        # Function output has lower resolution than GGIR windowsize[1]
      } else if (myfun$outputres > ws3) { 
        
        if (myfun$outputres %% ws3 != 0) {
          stop(paste0("Output resolution (", myfun$outputres[out_idx],
                      " s) for column '", myfun$colnames[out_idx],
                      "' is not a multiple of the GGIR epoch length (",
                      ws3, " s)."))
        }
        
        n_repeat = myfun$outputres / ws3
        
        indx = rep(seq_len(nrow(output)), each = n_repeat)
        
        output = output[indx, , drop = FALSE]
      }
    }
    # store in data frame
    OutputExternalFunction_processed[[out_idx]] = output
  }
  
  # ----------------------------------------------------------------------
  # Combine independently processed output columns
  # ----------------------------------------------------------------------
  OutputExternalFunction = do.call(cbind, OutputExternalFunction_processed)
  
  # re-factorize if any of the output columns was a factor 
  if (length(is_factor) > 0) {
    for (factor_idx in is_factor) {
      OutputExternalFunction[, factor_idx] = factor(
        OutputExternalFunction[, factor_idx],
        levels = LevelsExternalFunction[[factor_idx]]
      )
    }
  }
  
  
  # Apply user provided colnames
  colnames(OutputExternalFunction) = myfun$colnames
  
  # Add timestamp back as first column
  if (has_timestamp_output) {
    OutputExternalFunction = cbind(timestamp = timestamp, OutputExternalFunction)
  }
  
  return(list(OutputExternalFunction = OutputExternalFunction, 
              LevelsExternalFunction = LevelsExternalFunction))
}

mergeExternalFunctionLevels = function(existing_levels, new_levels) {
  if (is.null(existing_levels)) {
    return(new_levels)
  }

  lapply(seq_along(new_levels), function(level_idx) {
    union(existing_levels[[level_idx]], new_levels[[level_idx]])
  })
}
