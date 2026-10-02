check_myfun = function(myfun, windowsizes) { # Function to check myfun object
  status = 0
  # check that myfun is a list:
  if (is.list(myfun) == F) {
    status = 1
    stop("Object myfun is not a list.", call. = FALSE)
  }
  # check that there are no foreign object:
  foreignElements = which(names(myfun) %in% c("FUN", "parameters", "expected_sample_rate",
                                              "expected_unit", "colnames",
                                              "minlength", "outputres",
                                              "outputtype", "aggfunction",
                                              "timestamp","reporttype",
                                              "clevels", "ilevels", "qlevels",
                                              "ebout.dur", "ebout.th.cad", "ebout.th.acc",
                                              "ebout.criter", "ebout.condition", "name",
                                              "tbout.dur", "tbout.criter", "tbout.order") == FALSE)
  if (length(foreignElements) != 0) {
    status = 1
    stop("Object myfun has unexpected elements.", call. = FALSE)
  }
  # check that essential objects are included:
  expectedElements = c("FUN", "parameters", "expected_sample_rate", "expected_unit",
                       "colnames", "minlength", "outputres")
  missingElements = which(expectedElements %in% names(myfun) == FALSE)
  if (length(missingElements) != 0) {
    status = 1
    stop(paste0("Object myfun misses the following elements: ",
                paste(expectedElements[missingElements],collapse = ", "), "."), call. = FALSE)
  }
  # check that FUN is a function:
  if (is.function(myfun$FUN) ==  FALSE) {
    status = 1
     stop("Element FUN in myfun is not a function.", call. = FALSE)
  }
  # check that expected_sample_rate is numeric
  if (is.numeric(myfun$expected_sample_rate) == F) {
    status = 1
    stop("Element expected_sample_rate in myfun is not numeric.", call. = FALSE)
  }
  # check that unit is specified:
  if (length(which(myfun$expected_unit %in%  c("mg", "g", "ms2") == TRUE)) != 1) {
    status = 1
    stop("Object myfun lacks a clear specification of the expected_unit.", call. = FALSE)
  }
  # check that colnames has at least one character value:
  if (length(myfun$colnames) == 0) {
    status = 1
    stop("Element colnames in myfun does not have a value.", call. = FALSE)
  }
  # Check that colnames is a character:
  if (is.character(myfun$colnames) == F) {
    status = 1
    stop("Element colnames in myfun does not hold a character value.", call. = FALSE)
  }
  # check that minlegnth has one value:
  if (length(myfun$minlength) != 1) {
    status = 1
    stop("Element minlength in myfun does not have one value.", call. = FALSE)
  }
  # Check that minlength is a number
  if (is.numeric(myfun$minlength) == F) {
    status = 1
    stop("Element minlength in myfun is not numeric.", call. = FALSE)
  }
  # check that outputres has one value:
  if (length(myfun$outputres) != 1) {
    status = 1
    stop("Element outputres in myfun does not have one value.", call. = FALSE)
  }
  # Check that outputres is a number:
  if (is.numeric(myfun$outputres) == F) {
    status = 1
    stop("Element outputres in myfun is not numeric.", call. = FALSE)
  }
  # check that it is a round number can add up to 900 seconds:
  if (myfun$outputres != round(myfun$outputres)) {
    status = 1
    stop("Element outputres in myfun should be a round number.", call. = FALSE)
  }
  # check that outputres is either a multitude of the epochs size or vica versa:
  if (myfun$outputres/windowsizes[1] != round(myfun$outputres/windowsizes[1]) &
      windowsizes[1]/myfun$outputres != round(windowsizes[1]/myfun$outputres)) {
    status = 1
    stop(paste0("Element outputres and the epochsize used in GGIR",
                " (first element of windowsizes) are not a multitude",
                " of each other.", call. = FALSE))
  }
  if ("outputtype" %in% names(myfun)) {
    if (is.character(myfun$outputtype) == F) {
      status = 1
      stop(paste0("Error in check_myfun.R: Element outputtype is expected to be a",
                  " character specifying the ouput type"), call. = FALSE)
    }
    if (length(myfun$outputtype) != length(myfun$colnames)) {
      status = 1
      stop("Error in check_myfun.R: Element outputtype should have one value for each output column.",
           call. = FALSE)
    }
    if (any(myfun$outputtype %in% c("numeric", "character") == FALSE)) {
      status = 1
      stop("Error in check_myfun.R: Element outputtype contains an unsupported output type.",
           call. = FALSE)
    }
  }
  
  # aggfunction
  # if aggregation is needed, then aggfunction must be provided:
  if (myfun$outputres != windowsizes[1] && "aggfunction" %in% names(myfun) == FALSE) {
    status = 1
    stop(
      "Error in check_myfun.R: Element aggfunction must be provided when outputres differs from the epoch size used in GGIR.",
      call. = FALSE
    )
  }
  # check that aggfunction is specified for each output when outputtypes differ
  if ("aggfunction" %in% names(myfun) && "outputtype" %in% names(myfun)) { # if aggfunction is available
    if (is.function(myfun$aggfunction)) {
      # One function: applied to all output columns
      # Check that the function provided can be applied to all outputtypes
      if (length(unique(myfun$outputtype)) > 1) {
        status = 1
        stop(
          "Error in check_myfun.R: When outputtype contains both numeric and character outputs, aggfunction should be provided as a list with one function for each output column.",
          call. = FALSE
        )
      }
    } else if (is.list(myfun$aggfunction)) {
      if (length(myfun$aggfunction) != length(myfun$colnames)) {
        status = 1
        stop(
          "Error in check_myfun.R: When aggfunction is provided as a list, it should contain one function for each output column.",
          call. = FALSE
        )
      }
      
      for (fi in seq_along(myfun$aggfunction)) {
        if (is.function(myfun$aggfunction[[fi]]) == FALSE) {
          status = 1
          stop(
            "Error in check_myfun.R: Each element of aggfunction should be a function object.",
            call. = FALSE
          )
        }
      }
    } else {
      status = 1
      stop(
        "Error in check_myfun.R: Element aggfunction should be a function or a list of functions.",
        call. = FALSE
      )
    }
  }
  
  if ("reporttype" %in% names(myfun)) {
    if (is.character(myfun$reporttype) == F) {
      status = 1
      stop("Error in check_myfun.R: Element reporttype is expected to be a character.",
           call. = FALSE)
    }
    if (length(myfun$reporttype) != length(myfun$colnames)) {
      status = 1
      stop("Error in check_myfun.R: Element reporttype should have one value for each output column.",
           call. = FALSE)
    }
    if (any(myfun$reporttype %in% c("scalar", "event", "type") == FALSE)) {
      status = 1
      stop("Error in check_myfun.R: Element reporttype contains an unsupported report type.",
           call. = FALSE)
    }
  }

  # if ("timestamp" %in% names(myfun)) { # If timestamp is available:
  #   if (is.logical(myfun$timestamp) == F) {
  #     status = 1
  #     stop("Element timestamp is not of type logical.")
  #   }
  # }
  return(status)
}
