check_myfun = function(myfun, windowsizes) { # Function to check myfun object
  status = 0
  # check that myfun is a list:
  if (is.list(myfun) == F) {
    status = 1
    stop("Object myfun is not a list.") 
  }
  # check that there are no foreign object:
  foreignElements = which(names(myfun) %in% c("FUN", "parameters", "expected_sample_rate", "expected_unit", 
                                              "colnames", "minlength", "outputres", "outputtype", "aggfunction",
                                              "timestamp") == FALSE)
  if (length(foreignElements) != 0) {
    status = 1
    stop("Object myfun as unexpected elements.")
  }
  # check that essential objects are included:
  expectedElements = c("FUN", "parameters", "expected_sample_rate", "expected_unit", 
                       "colnames", "minlength", "outputres") 
  missingElements = which(expectedElements %in% names(myfun) == FALSE)
  if (length(missingElements) != 0) {
    status = 1
    stop(paste0("Object myfun misses the following elements: ",
                paste(expectedElements[missingElements],collapse=", "),"."))
  }
  # check that FUN is a function:
  if (is.function(myfun$FUN) ==  FALSE) {
    status = 1
     stop("Element FUN in myfun is not a function.")
  }
  # check that expected_sample_rate is numeric
  if (is.numeric(myfun$expected_sample_rate) == F) {
    status = 1
    stop("Element expected_sample_rate in myfun is not numeric.")
  }
  # check that unit is specified:
  if (length(which(myfun$expected_unit %in%  c("mg","g", "ms2") == TRUE)) != 1) {
    status = 1
    stop("Object myfun lacks a clear specification of the expected_unit.")
  }
  # check that colnames has at least one character value:
  if (length(myfun$colnames) == 0) {
    status = 1
    stop("Element colnames in myfun does not have a value.")
  }
  # Check that colnames is a character:
  if (is.character(myfun$colnames) == F) {
    status = 1
    stop("Element colnames in myfun does not hold a character value.")
  }
  # check that minlegnth has one value:
  if (length(myfun$minlength) != 1) {
    status = 1
    stop("Element minlength in myfun does not have one value.")
  }
  # Check that minlength is a number
  if (is.numeric(myfun$minlength) == F) {
    status = 1
    stop("Element minlength in myfun is not numeric.")
  }
  # check that outputres has one value:
  if (length(myfun$outputres) != 1) {
    status = 1
    stop("Element outputres in myfun does not have one value.")
  }
  # Check that outputres is a number:
  if (is.numeric(myfun$outputres) == F) {
    status = 1
    stop("Element outputres in myfun is not numeric.")
  }
  # check that it is a round number can add up to 900 seconds:
  if (myfun$outputres != round(myfun$outputres)) {
    status = 1
    stop("Element outputres in myfun should be a round number.")
  }
  # check that outputres is either a multitude of the epochs size or vica versa:
  if (myfun$outputres/windowsizes[1] != round(myfun$outputres/windowsizes[1]) &
      windowsizes[1]/myfun$outputres != round(windowsizes[1]/myfun$outputres)) {
    status = 1
    stop("Element outputres and the epochsize used in GGIR (first element of windowsizes) are not a multitude of each other.")
  }
  if ("outputtype" %in% names(myfun)) { # If outputtype is available:
    if (is.character(myfun$outputtype) == F) {
      status = 1
      stop("Element outputtype is expexted to be a character specifying the ouput type")
    }
  }
  if ("aggfunction" %in% names(myfun)) { # If aggfunction is available:
    if (is.function(myfun$aggfunction) == F) {
      status = 1
      stop("Element aggfunction is not a function object.")
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