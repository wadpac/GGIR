g.report.part6_dictionary = function(metadatadir, params_output) {
  # fixed variable names ----------------------------------------------------
  fixed = c(
    ID = "File/participant identifier",
    starttime = "Start time of the time series analysed in part 6",
    filename = "File name",
    N_days = "Number of days in the time series analysed in part 6",
    N_valid_days = "Number of valid days in the time series analysed in part 6",
    N_valid_weekend_days = "Number of valid weekend days in the time series analysed in part 6",
    cosinor_timeOffsetHours = "Cosinor analysis: time offset in hours between midnight and the start of the time series (for code development purposes)",
    cosinor_mes = "Cosinor analysis: MESOR (midline statistic of rhythm), the rhythm-adjusted mean",
    cosinor_amp = "Cosinor analysis: amplitude, half the extent of the predictable variation within a cycle",
    cosinor_acrophase = "Cosinor analysis: acrophase, time of the peak (radians)",
    cosinor_acrotime = "Cosinor analysis: acrotime, time of the peak (hours)",
    cosinor_ndays = "Cosinor analysis: number of days modelled",
    cosinor_R2 = "Cosinor analysis: squared correlation coefficient between fitted and log-transformed original time series",
    cosinorExt_minimum = "Extended cosinor analysis: minimum value of the fitted function",
    cosinorExt_amp = "Extended cosinor analysis: amplitude",
    cosinorExt_alpha = "Extended cosinor analysis: alpha, determines whether the peaks of the curve are wider than the troughs",
    cosinorExt_beta = "Extended cosinor analysis: beta, determines how steeply the function rises and falls compared to a cosine curve",
    cosinorExt_acrotime = "Extended cosinor analysis: time of day of the peak (hours)",
    cosinorExt_UpMesor = "Extended cosinor analysis: time of day of the switch from low to high activity (hours)",
    cosinorExt_DownMesor = "Extended cosinor analysis: time of day of the switch from high to low activity (hours)",
    cosinorExt_MESOR = "Extended cosinor analysis: MESOR calculated as minimum + amplitude / 2",
    cosinorExt_ndays = "Extended cosinor analysis: number of days modelled",
    cosinorExt_F_pseudo = "Extended cosinor analysis: pseudo F statistic, improvement of the fit compared to the cosine model",
    cosinorExt_R2 = "Extended cosinor analysis: squared correlation coefficient between fitted and log-transformed original time series",
    IS = "Interdaily stability",
    IV = "Intradaily variability",
    phi = "Autocorrelation coefficient (phi) of the acceleration time series at a lag of one day",
    SSP = "Self-similarity parameter (scaling exponent alpha) from detrended fluctuation analysis over all time scales",
    ABI = "Activity balance index, transformation of SSP",
    SSP_short = "Short-term self-similarity parameter (time scales up to 90 minutes)",
    ABI_short = "Activity balance index derived from SSP_short",
    SSP_long = "Long-term self-similarity parameter (time scales from 2 to 10 hours)",
    ABI_long = "Activity balance index derived from SSP_long",
    SSP_diff = "Difference between SSP_short and SSP_long",
    SleepRegularityIndex2 = "Sleep Regularity Index based on the sleep and nap classification of part 5, calculated over the full time series",
    SleepRegularityIndex2_Ndaypairs = "Number of day pairs used to calculate SleepRegularityIndex2",
    GGIRversion = "GGIR version used to process this recording in part 6 (stored in the milestone data), which may differ from the version used to generate the report"
  )
  classes = c(IN = "inactivity", PA = "physical activity", LIPA = "light physical activity",
              MVPA = "moderate-to-vigorous physical activity", sleep = "sleep", wake = "wakefulness")
  classname = function(k) ifelse(k %in% names(classes), classes[k], k)
  get_definition = function(x) {
    if (x %in% names(fixed)) return(fixed[[x]])
    # LX and MX variables
    if (grepl("^[LM][0-9.]+(VALUE|TIME_num|TIME_clock)$", x)) {
      LM = substr(x, 1, 1)
      X = sub("^[LM]([0-9.]+).*$", "\\1", x)
      window = paste0("the ", X, " consecutive hours with the ",
                      ifelse(LM == "L", "lowest", "highest"), " acceleration (", LM, X,
                      ") per 24 hours, averaged across days")
      if (grepl("VALUE$", x)) return(paste0("Mean acceleration during ", window, " (mg)"))
      if (grepl("TIME_num$", x)) return(paste0("Start time of ", window, ", in hours since the previous midnight (e.g., 26 = 2am)"))
      if (grepl("TIME_clock$", x)) return(paste0("Start time of ", window, " (hh:mm:ss)"))
    }
    if (grepl("^M[0-9.]+_(mean|max)_peakLUX$", x)) {
      X = sub("^M([0-9.]+)_.*$", "\\1", x)
      stat = sub("^M[0-9.]+_(mean|max)_.*$", "\\1", x)
      return(paste0(ifelse(stat == "mean", "Mean", "Maximum"), " peak light (lux) during the ",
                    X, " consecutive hours with the highest acceleration (M", X, ")"))
    }
    # fragmentation variables
    if (grepl("^FRAG_", x)) {
      elements = unlist(strsplit(x, "_"))
      window = ifelse(elements[length(elements)] == "spt",
                      "during the sleep period time", "during the waking hours")
      # class or transition
      cl = elements[which(elements %in% c(names(classes)) | grepl("2", elements))]
      cl = cl[cl != "FRAG"]
      if (length(cl) > 0 && grepl("2", cl[1])) {
        fromto = unlist(strsplit(cl[1], "2"))
        clname = paste0("from ", classname(fromto[1]), " to ", classname(fromto[2]))
      } else if (length(cl) > 0) {
        clname = classname(cl[1])
      } else {
        clname = ""
      }
      what = NULL
      if ("TP" %in% elements) {
        what = paste("transition probability", clname)
      } else if ("Nfrag" %in% elements) {
        what = paste("number of fragments", ifelse(grepl("^from", clname), "followed by a transition", "of"), clname)
      } else if ("NFragPM" %in% elements) {
        what = paste("number of fragments of", clname, "per minute of", clname)
      } else if ("mean" %in% elements) {
        what = paste("mean duration of the fragments of", clname, "(number of epochs)")
      } else if ("SD" %in% elements) {
        what = paste("standard deviation of the duration of the fragments of", clname, "(number of epochs)")
      } else if ("Gini" %in% elements) {
        what = paste("Gini index of the duration of the fragments of", clname)
      } else if ("CoV" %in% elements) {
        what = paste("coefficient of variation of the duration of the fragments of", clname)
      } else if ("alpha" %in% elements) {
        what = paste("power law exponent alpha of the duration of the fragments of", clname)
      } else if ("x0.5" %in% elements) {
        what = paste("x0.5, median fragment duration estimated from the power law distribution of the fragments of", clname)
      } else if ("W0.5" %in% elements) {
        what = paste("W0.5, fraction of the time accumulated in fragments longer than x0.5, for the fragments of", clname)
      }
      if (!is.null(what)) return(paste("Fragmentation analysis:", what, window))
    }
    return(NA)
  }
  # main script -------------------------------------------------------------
  resultsdir = file.path(metadatadir, "results")
  reports = file.path(resultsdir, "part6_summary.csv")
  reports = reports[file.exists(reports)]
  if (length(reports) == 0) return()
  directory = file.path(resultsdir, "variableDictionary")
  if (!dir.exists(directory)) dir.create(directory)
  for (ri in 1:length(reports)) {
    R = data.table::fread(reports[ri], verbose = FALSE, nrows = 2, check.names = FALSE)
    cnames = colnames(R)
    dictionary = data.frame(Variable = cnames, Definition = NA)
    for (coli in 1:length(cnames)) {
      dictionary[coli, "Definition"] = get_definition(cnames[coli])
    }
    fn = gsub("^part6_", "part6_dictionary_", basename(reports[ri]))
    data.table::fwrite(dictionary, file = file.path(directory, fn),
                       row.names = FALSE, na = "", sep = params_output[["sep_reports"]],
                       dec = params_output[["dec_reports"]])
  }
}
