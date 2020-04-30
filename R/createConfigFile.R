createConfigFile = function(config.parameters = c()) {
  if (length(config.parameters) == 0) {
    stop("Configuration parameters not be found.")
  }
  out = matrix("",length(config.parameters)+2,3)
  for (i in 1:length(config.parameters)) {
    NM = names(config.parameters)[i]
    out[i,1] = NM
    Value = config.parameters[[i]]
    if (length(Value) == 0) Value = 'c()'
    if (length(Value) > 1) Value = paste0("c(",paste(Value,collapse = ","),")")
    if (is.function(Value) == FALSE | is.list(Value) == FALSE) {
      out[i,2] = Value
    } else {
      out[i,2] = 'c()' # function or list objects are not stored in the config file, the user will have to provide these explicitely
    }
    out[i,3] = "General parameters"
    if (NM %in% c("windowsizes", "chunksize",
                  "minloadcrit", "do.enmo", "do.lfenmo", "do.en", "do.bfen", "do.hfen",
                  "do.hfenplus", "do.mad", "do.anglex", "do.angley", "do.anglez", 
                  "do.roll_med_acc_x", "do.roll_med_acc_y", "do.roll_med_acc_z",
                  "do.dev_roll_med_acc_x", "do.dev_roll_med_acc_y", "do.dev_roll_med_acc_z",
                  "do.enmoa", "printsummary", "do.cal", "print.filename", 
                  "backup.cal.coef", "dayborder", "dynrange",
                  "configtz", "do.lfen", "hb", "lb", "n", "myfun") ==  TRUE) {
      out[i,3] = "Calibration, Feature extraction, Epoch size, Time zone"
    } else if (NM %in% c("strategy", "hrs.del.start", "hrs.del.end", "maxdur",
                         "includedaycrit", "M5L5res", "winhr", "qwindow", "qlevels",
                         "ilevels", "mvpathreshold", "boutcriter", "ndayswindow",
                         "do.imp", "epochvalues2csv", "mvpadur",
                         "window.summary.size", "dayborder", "closedbout",
                         "IVIS_windowsize_minutes", "IVIS_epochsize_seconds", "iglevels",
                         "IVIS.activity.metric", "TimeSegments2ZeroFile", "qM5L5", "do.part3.pdf") ==  TRUE) {
      out[i,3] = "Study design, Parameters descriptive analysis"
    } else if (NM %in% c("anglethreshold", "timethreshold",
                         "ignorenonwear", "constrain2range") ==  TRUE) {
      out[i,3] = "Parameters sleep detection"
    } else if (NM %in% c("colid", "coln1", "nnights", "outliers.only",
                         "excludefirstlast", "criterror", "includenightcrit", "loglocation",
                         "relyonsleeplog", "sleeplogidnum", "def.noc.sleep", "do.visual") ==  TRUE) {
      out[i,3] = "Parameters sleep period time detection with or wihout sleeplog"
    } else if (NM %in% c("excludefirstlast.part5", "windowsizes", "boutcriter.in",
                         "boutcriter.lig", "boutcriter.mvpa", "threshold.lig", "threshold.mod", 
                         "threshold.vig", "timewindow", "boutdur.mvpa", 
                         "boutdur.in", "boutdur.lig", "save_ms5rawlevels") ==  TRUE) {
      out[i,3] = "Parameters time-use variables"
    } else if (NM %in% c("viewingwindow", "visualreport", "dofirstpage") ==  TRUE) {
      out[i,3] = "Visual report"
    }
  }
  GGIRversion = ""
  SI = sessionInfo()
  try(expr = {GGIRversion = SI$loadedOnly$GGIR$Version},silent=TRUE)
  if (length(GGIRversion) == 0) GGIRversion = "Could not retrieve GGIR version"
  out[nrow(out) - 1,] = c("GGIR_version", GGIRversion, " not applicable")
  out[nrow(out),] = c("R_version", SI$R.version$version.string, " not applicable")
  out = as.data.frame(out, stringsAsFactors = TRUE)
  colnames(out) = c("argument","value","context")
  out = out[order(out$context),]
  return(out)
}