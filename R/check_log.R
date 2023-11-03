check_log = function(log, dateformat, colid = 1, datecols = c(), 
                     logPath, logtype) {
  
  # at the moment this is used only for activity log and study dates log
  dateformat_param = ifelse(logtype == "activity log", "qwindow_dateformat", "study_dates_dateformat")
  
  # assume ID to be in first column
  log = log[which(log[, colid] != ""),] # ignore rows for which there is no id
  
  # replace cells with only a dot by empty character
  # the documentation clearly states that no dots should be used
  # to indicate missing value, but this is just in case users still forget about it
  for (jn in 1:ncol(log)) {
    dotcells = which(log[,jn] == ".")
    if (length(dotcells) > 0) {
      # is.na(actlog_vec[dotcells]) = TRUE
      log[dotcells, jn] = ""
    }
  }
  # Check dates
  # extract example date value
  if (is.null(datecols)) {
    datecols = grep(pattern = "date|Date|DATE",  x = colnames(log), value = FALSE)
  }
  if (length(datecols) > 0) {
    exampledates = unlist(log[,datecols])
    exampledates = exampledates[which(!is.na(exampledates))]
    
    e1 = strsplit(exampledates, split = "/|-")
    e1 = e1[lengths(e1) > 0L]
    date_consistency_check = t(as.data.frame(e1))
    numrange = apply(X = date_consistency_check, MARGIN = 2, FUN = function(x) diff(range(sapply(x, nchar))))
    if (any(numrange > 1)) {
      stop(paste0("\nDifferent date formats were encountered in your ", 
                  logtype, ": ", logPath,". ",
                  "Please note that this may not be visible when you open the ", logtype,
                  " in MS Excel, but it should be visible in a plain text editor. ",
                  "To resolve this update the format for all date cells in the ",
                  "log to be consistent and in line with the value of argument",
                  dateformat_param, ", which is currently: ", dateformat), call. = FALSE)
    }
  } else {
    exampledates = c()
  }
  
  Nhyphen = length(grep(pattern = "-", x = log[, datecols]))
  Ndash = length(grep(pattern = "/", x = log[, datecols]))
  if (Nhyphen > 0 & Ndash > 0) {
    warning(paste0("\nYou may be mixing slash and hyphen separated dates ",
                   "inside the same ", logtype , ", which could lead to errors. ",
                   "Please fix."),
            call. = FALSE)
  }
  # Replace / by - if that is what is expected dateformat
  if (Ndash > 0 & length(grep(pattern = "/", x = dateformat)) == 0) {
    for (rc in 1:ncol(log)) {
      log[, rc] = gsub(pattern = "/", replacement = "-", x = log[, rc])
    }
  }
  # Replace - by / if that is what is expected dateformat
  if (Nhyphen > 0 & length(grep(pattern = "-", x = dateformat)) == 0) {
    for (rc in 1:ncol(log)) {
      log[, rc] = gsub(pattern = "-", replacement = "/", x = log[, rc])
    }
  }
  # return
  return(log)
}