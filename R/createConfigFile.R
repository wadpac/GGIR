createConfigFile = function(config.parameters = c(), GGIRversion = "") {
  if (length(config.parameters) == 0) {
    stop("Configuration parameters not be found.")
  }
  out = matrix("", length(config.parameters) + 2, 3)
  possible_params_objectnames = c("params_247", "params_cleaning", "params_general",
                                  "params_metrics", "params_output",
                                  "params_phyact", "params_rawdata", "params_sleep")
  for (i in 1:length(config.parameters)) {
    NM = names(config.parameters)[i]
    if (NM %in% possible_params_objectnames) {
      # Replace NULL values before converting to data.frame
      config.parameters[[i]] <- lapply(config.parameters[[i]], lapply, function(x)ifelse(is.null(x), "c()", x))
      Value = as.data.frame(t(data.frame(t(sapply(config.parameters[[i]],c)), stringsAsFactors = FALSE)))
      Value$col1 = row.names(Value)
      Value$col3 = NM
      Value = Value[,c("col1", "V1", "col3")]
      colnames(out) = colnames(Value)
      # Replace empty lists by c(), replace vector lists by collapse vector
      # Replace non-empty lists by same value but without the list
      myfun = function(x) {
        x = ifelse(test = is.list(x) & length(unlist(x) == 1),
                   yes = ifelse(test = length(unlist(x)) > 1,
                                yes = paste0("c(", paste0(unlist(x), collapse = ","),")", collapse = ""),
                                no = unlist(x)),
                   no =  x)
        x = ifelse(test = is.list(x) & length(unlist(x)) == 0,
                   yes = "c()",
                   no =  x)
        return(x)
      }
      Value$V1 <- as.character(lapply(Value$V1, myfun))
      out = rbind(out, as.matrix(Value)) # append to the end
    } else {
      out[i,1] = NM
      Value = config.parameters[[i]]
      if (length(Value) == 0) {
        Value = 'c()'
      }
      if (length(Value) > 1) {
        Value = paste0("c(",paste(Value,collapse = ","),")")
      }
      if (is.function(Value) == FALSE | is.list(Value) == FALSE) {
        out[i,2] = as.character(Value)
      } else {
        out[i,2] = 'c()' # function or list objects are not stored in the config file, the user will have to provide these explicitely
      }
      if (NM %in% c("windowsizes", "chunksize",
                    "minloadcrit", "do.enmo", "do.lfenmo", "do.en", "do.bfen", "do.hfen",
                    "do.hfenplus", "do.mad", "do.anglex", "do.angley", "do.anglez",
                    "do.roll_med_acc_x", "do.roll_med_acc_y", "do.roll_med_acc_z",
                    "do.dev_roll_med_acc_x", "do.dev_roll_med_acc_y", "do.dev_roll_med_acc_z",
                    "do.enmoa", "printsummary", "do.cal", "print.filename",
                    "backup.cal.coef", "dayborder", "dynrange",
                    "configtz", "do.lfen", "hb", "lb", "n", "myfun") ==  TRUE) {
        out[i,3] = "Calibration, Feature extraction, Epoch size, Time zone"
      } else if (NM %in% c("strategy", "data_masking_strategy", "hrs.del.start", "hrs.del.end", "maxdur",
                           "includedaycrit", "M5L5res", "winhr", "qwindow", "qlevels",
                           "ilevels", "mvpathreshold", "boutcriter", "ndayswindow",
                           "do.imp", "epochvalues2csv", "mvpadur",
                           "dayborder", "closedbout",
                           "IVIS_windowsize_minutes", "IVIS_epochsize_seconds", "iglevels",
                           "IVIS.activity.metric", "TimeSegments2ZeroFile", "qM5L5", "do.part3.pdf") ==  TRUE) {
        out[i,3] = "Study design, Parameters descriptive analysis"
      } else if (NM %in% c("anglethreshold", "timethreshold",
                           "ignorenonwear", "constrain2range", "sensor.location") == TRUE) {
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
  }
  SI = sessionInfo()
  GGIRread_version = "could not extract version"
  if (is.element('GGIRread', installed.packages()[,1])) {
    GGIRread_version = as.character(utils::packageVersion("GGIRread"))
    if (length(GGIRread_version) != 1) GGIRread_version = sessionInfo()$otherPkgs$GGIRread$Version
  }
  out = rbind(out, matrix(c("GGIRread_version", GGIRread_version, "not applicable"), nrow = 1))
  out = rbind(out, matrix(c("GGIRversion", GGIRversion, "not applicable"), nrow = 1))
  out = rbind(out, matrix(c("R_version", SI$R.version$version.string, "not applicable"), nrow = 1)) 
  out = out[which(!is.na(out[,1])),]
  out = as.data.frame(out, stringsAsFactors = TRUE)
  row.names(out) <- NULL
  colnames(out) = c("argument","value","context")
  out$value = as.character(out$value)
  out$argument = as.character(out$argument)
  out$context = factor(as.character(out$context))
  out = out[which(out$argument %in% c("", possible_params_objectnames) == FALSE),]
  out = out[order(out$context),]
  return(out)
}
