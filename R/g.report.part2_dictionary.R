g.report.part2_dictionary = function(metadatadir, params_output) {
  # internal functions -------------------------------------------------------
  # describe the day segment encoded at the end of a variable name, e.g. "0-24hr"
  segment_text = function(seg) {
    if (is.null(seg) || is.na(seg) || seg == "") return(NULL)
    seg = gsub("hr$|h$", "", seg)
    edges = unlist(strsplit(seg, "-"))
    if (length(edges) != 2) return(paste0("in day segment ", seg))
    num = suppressWarnings(as.numeric(edges))
    if (all(!is.na(num))) {
      if (num[1] == 0 & num[2] == 24) return("over the full day (0-24h)")
      return(paste0("in the day segment from ", num[1], "h to ", num[2], "h"))
    }
    return(paste0("in the day segment from ", edges[1], " to ", edges[2]))
  }
  # describe acceleration or cadence range, e.g. "0-100" or "atleast250"
  range_text = function(x, unit) {
    if (grepl("^atleast", x)) {
      return(paste0("of at least ", gsub("^atleast", "", x), " ", unit))
    }
    edges = unlist(strsplit(x, "-"))
    return(paste0("from ", edges[1], " (included) to ", edges[2], " (excluded) ", unit))
  }
  window_text = function(LM, X) {
    paste0("the ", X, " consecutive hours with the ",
           ifelse(LM == "L", "lowest", "highest"))
  }
  # aggregation across days (only person level summaries)
  agg_prefix = c(AD = "average of all valid days",
                 WD = "average of valid weekdays",
                 WE = "average of valid weekend days",
                 WWD = "weighted average of valid weekdays (repeated weekdays are first averaged)",
                 WWE = "weighted average of valid weekend days (repeated weekend days are first averaged)")
  # fixed variable names ----------------------------------------------------
  fixed = c(
    # identification and recording characteristics
    ID = "File/participant identifier",
    device_sn = "Device serial number",
    bodylocation = "Body location extracted from file header",
    filename = "File name",
    start_time = "Timestamp when the recording started",
    startday = "Day of the week on which the recording started",
    samplefreq = "Sampling frequency (Hz)",
    device = "Accelerometer brand",
    split1_name = "Name of the first part of the file name when the file name is split with idloc (if used)",
    split2_name = "Name of the second part of the file name when the file name is split with idloc (if used)",
    clipping_score = "Fraction of long epochs (windowsizes[2], default 15 minutes) in which acceleration in one of the axes was close to the dynamic range of the device for at least 80% of the time (should be 0)",
    meas_dur_dys = "Measurement duration (days)",
    complete_24hcycle = "Fraction of the 24-hour cycle (in long epochs) for which valid data is available on at least one day of the recording",
    meas_dur_def_proto_day = "Measurement duration according to the study protocol (days), i.e., after removing the hours ignored by data_masking_strategy",
    wear_dur_def_proto_day = "Wear duration according to the study protocol (days)",
    nonwear_hours_filtered = "Total number of non-wear hours filtered with nonwearFiltermaxHours",
    nonwear_events_filtered = "Total number of non-wear events filtered with nonwearFiltermaxHours",
    calib_err = "Calibration error after auto-calibration (g), estimated from non-movement periods",
    calib_status = "Summary statement about the status of the auto-calibration",
    `N valid weekend days (WE)` = "Number of valid weekend days (based on includedaycrit)",
    `N valid weekdays (WD)` = "Number of valid weekdays (based on includedaycrit)",
    # data masking and other logs
    `n hours ignored at start of meas (if data_masking_strategy=1)` = "Number of hours ignored at the start of the recording (data_masking_strategy = 1) or of the ndayswindow (data_masking_strategy = 3 or 5)",
    `n hours ignored at end of meas (if data_masking_strategy=1)` = "Number of hours ignored at the end of the recording (data_masking_strategy = 1) or of the ndayswindow (data_masking_strategy = 3 or 5)",
    `n days of measurement after which all data is ignored (if data_masking_strategy=1)` = "Number of days of measurement after which all data is ignored (maxdur)",
    `epoch size to which acceleration was averaged (seconds)` = "Short epoch length used to aggregate the acceleration metrics (seconds)",
    if_hip_long_axis_id = "Identifier of the longitudinal axis of the accelerometer (only for hip-worn data, sensor.location = 'hip')",
    `GGIR version` = "GGIR version used to process this recording in part 2 (stored in the milestone data), which may differ from the version used to generate the report",
    GGIRversion = "GGIR version used to process this recording in part 2 (stored in the milestone data), which may differ from the version used to generate the report",
    # circadian rhythm
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
    # day level and long format
    calendar_date = "Calendar date",
    `N valid hours` = "Number of hours with valid data in the day",
    `N hours` = "Number of hours of measurement in the day",
    N_valid_hours = "Number of hours with valid data in the day",
    N_hours = "Number of hours of measurement in the day",
    N_valid_hours_in_window = "Number of hours with valid data in the day segment",
    N_hours_in_window = "Number of hours of measurement in the day segment",
    weekday = "Day of the week (full name)",
    measurementday = "Day number relative to the start of the recording",
    qwindow_timestamps = "Start and end times of the day segments as defined with qwindow",
    qwindow_names = "Names of the day segments as defined with qwindow",
    qwindow_name = "Name of the day segment as defined with qwindow",
    # data quality report
    file.corrupt = "Whether the file could not be read (TRUE/FALSE): TRUE when the sampling frequency could not be extracted from the file header or when the first block of data was empty. Corrupt files are not analysed",
    file.too.short = "Whether the file had too little data to be analysed (TRUE/FALSE): TRUE when the first block of data was shorter than two non-wear windows (2 x windowsizes[3], default 2 hours) or when no epoch level data could be derived. Files that are too short are not analysed",
    use.temperature = "Whether temperature was used for auto-calibration (TRUE/FALSE)",
    scale.x = "Auto-calibration scaling coefficient for the x-axis",
    scale.y = "Auto-calibration scaling coefficient for the y-axis",
    scale.z = "Auto-calibration scaling coefficient for the z-axis",
    offset.x = "Auto-calibration offset coefficient for the x-axis (g)",
    offset.y = "Auto-calibration offset coefficient for the y-axis (g)",
    offset.z = "Auto-calibration offset coefficient for the z-axis (g)",
    temperature.offset.x = "Auto-calibration temperature offset coefficient for the x-axis",
    temperature.offset.y = "Auto-calibration temperature offset coefficient for the y-axis",
    temperature.offset.z = "Auto-calibration temperature offset coefficient for the z-axis",
    cal.error.start = "Calibration error before auto-calibration (g)",
    cal.error.end = "Calibration error after auto-calibration (g)",
    n.10sec.windows = "Number of 10-second non-movement windows used for auto-calibration",
    n.hours.considered = "Number of hours of data considered for auto-calibration",
    QCmessage = "Message produced by the auto-calibration",
    mean.temp = "Mean temperature in the non-movement windows used for auto-calibration (degrees Celsius)",
    device.serial.number = "Device serial number",
    NFilePagesSkipped = "Number of raw data blocks skipped (only Axivity .cwa files)",
    filehealth_totimp_min = "Total time with raw data imputed while reading the file (minutes): faulty data blocks in Axivity .cwa files, or time gaps in ActiGraph .gt3x files (e.g., idle sleep mode)",
    filehealth_checksumfail_min = "Total time with raw data blocks that failed the checksum (minutes, only Axivity .cwa files)",
    filehealth_niblockid_min = "Total time with non-incremental raw data block ids (minutes, only Axivity .cwa files)",
    filehealth_fbias0510_min = "Total time with a sampling frequency bias between 5 and 10% (minutes, only Axivity .cwa files)",
    filehealth_fbias1020_min = "Total time with a sampling frequency bias between 10 and 20% (minutes, only Axivity .cwa files)",
    filehealth_fbias2030_min = "Total time with a sampling frequency bias between 20 and 30% (minutes, only Axivity .cwa files)",
    filehealth_fbias30_min = "Total time with a sampling frequency bias higher than 30% (minutes, only Axivity .cwa files)",
    filehealth_totimp_N = "Number of imputed raw data blocks (Axivity .cwa files) or time gaps (ActiGraph .gt3x files)",
    filehealth_checksumfail_N = "Number of raw data blocks that failed the checksum (only Axivity .cwa files)",
    filehealth_niblockid_N = "Number of raw data blocks with non-incremental block ids (only Axivity .cwa files)",
    filehealth_fbias0510_N = "Number of raw data blocks with a sampling frequency bias between 5 and 10% (only Axivity .cwa files)",
    filehealth_fbias1020_N = "Number of raw data blocks with a sampling frequency bias between 10 and 20% (only Axivity .cwa files)",
    filehealth_fbias2030_N = "Number of raw data blocks with a sampling frequency bias between 20 and 30% (only Axivity .cwa files)",
    filehealth_fbias30_N = "Number of raw data blocks with a sampling frequency bias higher than 30% (only Axivity .cwa files)",
    filehealth_0vars_missing = "Percentage of epochs with no variables missing (only Fitbit data)",
    filehealth_1vars_missing = "Percentage of epochs with one variable missing (only Fitbit data)",
    filehealth_2vars_missing = "Percentage of epochs with two variables missing (only Fitbit data)",
    filehealth_3vars_missing = "Percentage of epochs with three variables missing (only Fitbit data)",
    filehealth_4vars_missing = "Percentage of epochs with four variables missing (only Fitbit data)",
    filehealth_ExtAct_missing = "Percentage of epochs with missing activity data (only Fitbit data)",
    filehealth_ExtStep_missing = "Percentage of epochs with missing step data (only Fitbit data)",
    filehealth_ExtHeartRate_missing = "Percentage of epochs with missing heart rate data (only Fitbit data)",
    filehealth_ExtSleep_missing = "Percentage of epochs with missing sleep data (only Fitbit data)",
    filehealth_ExtSleep_y_ExtAct_missing = "Fraction (not percentage) of epochs with both sleep and activity data missing (only Fitbit data)"
  )
  # function to derive the definition of one variable ------------------------
  get_definition = function(cname) {
    if (cname %in% names(fixed)) return(fixed[[cname]])
    if (grepl("^data exclusion st", cname)) {
      return("Data masking strategy used (data_masking_strategy, value=1: ignore specific hours; value=2: ignore all data before the first midnight and after the last midnight; see GGIR documentation for values 3, 4 and 5)")
    }
    x = cname
    # aggregation across days
    agg = NULL
    prefix = regmatches(x, regexpr("^(AD|WD|WE|WWD|WWE)_", x))
    if (length(prefix) == 1) {
      agg = agg_prefix[[gsub("_", "", prefix)]]
      x = sub("^(AD|WD|WE|WWD|WWE)_", "", x)
    }
    # full recording (average day) variables
    fullrec = FALSE
    if (grepl("_fullRecordingMean$", x)) {
      metric = sub("^ExtFunEvent_", "", sub("_fullRecordingMean$", "", x))
      # same rule as g.analyse.perfile: only metrics in g-units are expressed in mg
      notInG = grepl("BrondCount|ZCX|ZCY|ZCZ|NeishabouriCount|ExtAct|ExtHeartRate|step_count", metric)
      return(paste0("Mean ", metric, " over the full recording, calculated from the average day",
                    " (invalid data imputed by the average of similar time points on other days)",
                    ifelse(notInG, "", " (mg)")))
    }
    if (grepl("_fullRecording$", x)) {
      fullrec = TRUE
      x = sub("_fullRecording$", "", x)
    }
    # day segment
    seg = NULL
    segmatch = regmatches(x, regexpr("_[^_]+-[^_]+h(r)?$", x))
    if (length(segmatch) == 1) {
      seg = sub("^_", "", segmatch)
      x = sub("_[^_]+-[^_]+h(r)?$", "", x)
    }
    what = NULL
    if (length(type_metrics) > 0) {
      # type report: only type definitions apply to the remaining variables
      what = get_type_definition(x)
      if (is.null(what)) return(NA)
    }
    if (!is.null(what)) {
      # categorical (type) output of external functions, see get_type_definition
    } else if (grepl("^[LM][0-9.]+(hr)?_cad(_|$)", x)) {
      # LX and MX based on cadence (events derived with external functions)
      what = get_event_definition(x)
    } else if (grepl("^N_valid_hours$", x)) {
      what = "Number of hours with valid data"
    } else if (grepl("^N_hours$", x)) {
      what = "Number of hours of measurement"
    } else if (grepl("^[LM][0-9.]+hr_.+_(mg|cnt)$", x)) {
      LM = substr(x, 1, 1); X = sub("^[LM]([0-9.]+)hr_.*$", "\\1", x)
      metric = sub("^[LM][0-9.]+hr_(.+)_(mg|cnt)$", "\\1", x)
      what = paste0("Start time of ", window_text(LM, X), " ", metric,
                    " (", LM, X, "), in hours since the previous midnight (e.g., 26 = 2am)")
    } else if (grepl("^[LM][0-9.]+_q[0-9.]+_.+_(mg|cnt)$", x)) {
      LM = substr(x, 1, 1); X = sub("^[LM]([0-9.]+)_q.*$", "\\1", x)
      q = sub("^[LM][0-9.]+_q([0-9.]+)_.*$", "\\1", x)
      metric = sub("^[LM][0-9.]+_q[0-9.]+_(.+)_(mg|cnt)$", "\\1", x)
      what = paste0("Percentile ", q, " of ", metric, " within ", window_text(LM, X),
                    " ", metric, " (", LM, X, ")")
    } else if (grepl("^[LM][0-9.]+_ig_(gradient|intercept|rsquared)_.+_(mg|cnt)$", x)) {
      LM = substr(x, 1, 1); X = sub("^[LM]([0-9.]+)_ig_.*$", "\\1", x)
      igpart = sub("^[LM][0-9.]+_ig_(gradient|intercept|rsquared)_.*$", "\\1", x)
      metric = sub("^[LM][0-9.]+_ig_(gradient|intercept|rsquared)_(.+)_(mg|cnt)$", "\\2", x)
      what = paste0(switch(igpart, gradient = "Intensity gradient",
                           intercept = "Intercept of the intensity gradient regression",
                           rsquared = "R-squared of the intensity gradient regression"),
                    " within ", window_text(LM, X), " ", metric, " (", LM, X, ")")
    } else if (grepl("^[LM][0-9.]+_.+_(mg|cnt)$", x)) {
      LM = substr(x, 1, 1); X = sub("^[LM]([0-9.]+)_.*$", "\\1", x)
      metric = sub("^[LM][0-9.]+_(.+)_(mg|cnt)$", "\\1", x)
      what = paste0("Mean ", metric, " during ", window_text(LM, X), " ", metric,
                    " (", LM, X, ")")
    } else if (grepl("^mean_.+_(mg|cnt)_1-6am$", x)) {
      metric = sub("^mean_(.+)_(mg|cnt)_1-6am$", "\\1", x)
      what = paste0("Mean ", metric, " between 1am and 6am")
    } else if (grepl("^1to6am_.+_mg$", x)) {
      metric = sub("^1to6am_(.+)_mg$", "\\1", x)
      what = paste0("Mean ", metric, " between 1am and 6am")
    } else if (grepl("^mean_.+_(mg|cnt)$", x)) {
      metric = sub("^mean_(.+)_(mg|cnt)$", "\\1", x)
      what = paste0("Mean ", metric)
    } else if (grepl("^p[0-9.]+_.+_(mg|cnt)$", x)) {
      p = sub("^p([0-9.]+)_.*$", "\\1", x)
      metric = sub("^p[0-9.]+_(.+)_(mg|cnt)$", "\\1", x)
      what = paste0("Percentile ", p, " of the ", metric, " distribution")
    } else if (grepl("^\\[.+,.+\\)_.+_(mg|cnt)$", x)) {
      edges = unlist(strsplit(sub("^\\[(.+)\\)_.*$", "\\1", x), ","))
      edges = suppressWarnings(format(as.numeric(edges), scientific = FALSE, trim = TRUE))
      metric = sub("^\\[.+,.+\\)_(.+)_(mg|cnt)$", "\\1", x)
      unit = ifelse(grepl("_cnt$", x), "counts", "mg")
      what = paste0("Time accumulated with ", metric, " from ", edges[1],
                    " (included) to ", edges[2], " (excluded) ", unit, " (minutes)")
    } else if (grepl("^ig_(gradient|intercept|rsquared)_.+$", x)) {
      igpart = sub("^ig_(gradient|intercept|rsquared)_.*$", "\\1", x)
      metric = sub("^ig_(gradient|intercept|rsquared)_(.+)$", "\\2", x)
      metric = sub("_mg$", "", metric)
      what = paste0(switch(igpart, gradient = "Intensity gradient",
                           intercept = "Intercept of the log-log time to intensity regression used to calculate the intensity gradient",
                           rsquared = "R-squared of the log-log time to intensity regression used to calculate the intensity gradient"),
                    " based on ", metric)
    } else if (grepl("^MVPA_E[0-9.]+S_B[0-9.]+M[0-9.]+%_T[0-9.]+_.+$", x)) {
      E = sub("^MVPA_E([0-9.]+)S_.*$", "\\1", x)
      B = sub("^MVPA_E[0-9.]+S_B([0-9.]+)M.*$", "\\1", x)
      crit = sub("^MVPA_E[0-9.]+S_B[0-9.]+M([0-9.]+)%.*$", "\\1", x)
      Tr = sub("^.*%_T([0-9.]+)_.*$", "\\1", x)
      metric = sub("^.*%_T[0-9.]+_(.+)$", "\\1", x)
      what = paste0("Time accumulated in moderate-to-vigorous physical activity bouts of at least ", B,
                    " minutes (", crit, "% of the ", E, "-second epochs with ", metric, " >= ", Tr,
                    " mg) (minutes)")
    } else if (grepl("^MVPA_E[0-9.]+(S|M)_T[0-9.]+_.+$", x)) {
      E = sub("^MVPA_E([0-9.]+(S|M))_.*$", "\\1", x)
      E = gsub("S$", " seconds", gsub("M$", " minute(s)", E))
      Tr = sub("^MVPA_E[0-9.]+(S|M)_T([0-9.]+)_.*$", "\\2", x)
      metric = sub("^MVPA_E[0-9.]+(S|M)_T[0-9.]+_(.+)$", "\\2", x)
      what = paste0("Time accumulated in moderate-to-vigorous physical activity (", metric,
                    " >= ", Tr, " mg) without bout criteria, after aggregating the data to epochs of ",
                    E, " (minutes)")
    } else {
      # events derived with external functions (e.g. step counts)
      what = get_event_definition(x)
    }
    if (is.null(what)) return(NA)
    def = what
    if (fullrec) def = paste(def, "over the full recording, calculated from the average day")
    if (!is.null(seg) & !fullrec) def = paste(def, segment_text(seg))
    if (!is.null(agg)) def = paste0(def, " - ", agg)
    def = gsub("\\s+", " ", def)
    return(def)
  }
  get_event_definition = function(x) {
    # variable names from part2_eventsummary and part2_dayeventsummary
    # (prefix ExtFunEvent_ is removed in the reports)
    x = sub("^ExtFunEvent_", "", x)
    what = NULL
    if (grepl("^(totdur|number|meandur)_B[0-9.]+M[0-9.]+%_cad[0-9.]+(AND|OR)acc[0-9.]+_.+$", x)) {
      type = sub("^(totdur|number|meandur)_.*$", "\\1", x)
      B = sub("^.+_B([0-9.]+)M.*$", "\\1", x)
      crit = sub("^.+_B[0-9.]+M([0-9.]+)%.*$", "\\1", x)
      cad = sub("^.+%_cad([0-9.]+)(AND|OR).*$", "\\1", x)
      cond = sub("^.+%_cad[0-9.]+(AND|OR)acc.*$", "\\1", x)
      acc = sub("^.+(AND|OR)acc([0-9.]+)_.*$", "\\2", x)
      metric = sub("^.+(AND|OR)acc[0-9.]+_(.+)$", "\\2", x)
      what = paste0(switch(type, totdur = "Total time (minutes) in",
                           number = "Number of", meandur = "Mean duration (minutes) of"),
                    " event bouts of at least ", B, " minutes in which ", crit,
                    "% of the epochs meet cadence >= ", cad, " steps per minute ", cond,
                    " ", metric, " >= ", acc, " mg")
    } else if (grepl("^[LM][0-9.]+hr_cad$", x)) {
      LM = substr(x, 1, 1); X = sub("^[LM]([0-9.]+)hr_cad$", "\\1", x)
      what = paste0("Start time of ", window_text(LM, X), " cadence (", LM, X,
                    "), in hours since the previous midnight (e.g., 26 = 2am)")
    } else if (grepl("^[LM][0-9.]+_cad_meancad$", x)) {
      LM = substr(x, 1, 1); X = sub("^[LM]([0-9.]+)_.*$", "\\1", x)
      what = paste0("Mean cadence (steps per minute) during ", window_text(LM, X), " cadence (", LM, X, ")")
    } else if (grepl("^[LM][0-9.]+_cad_mean_.+_mg$", x)) {
      LM = substr(x, 1, 1); X = sub("^[LM]([0-9.]+)_.*$", "\\1", x)
      metric = sub("^[LM][0-9.]+_cad_mean_(.+)_mg$", "\\1", x)
      what = paste0("Mean ", metric, " during ", window_text(LM, X), " cadence (", LM, X, ")")
    } else if (grepl("^[LM][0-9.]+_cad_q[0-9.]+_.+_mg$", x)) {
      LM = substr(x, 1, 1); X = sub("^[LM]([0-9.]+)_.*$", "\\1", x)
      q = sub("^[LM][0-9.]+_cad_q([0-9.]+)_.*$", "\\1", x)
      metric = sub("^[LM][0-9.]+_cad_q[0-9.]+_(.+)_mg$", "\\1", x)
      what = paste0("Percentile ", q, " of ", metric, " (mg) during ", window_text(LM, X),
                    " cadence (", LM, X, ")")
    } else if (grepl("^[LM][0-9.]+_cad_q[0-9.]+(cad|acc)$", x)) {
      LM = substr(x, 1, 1); X = sub("^[LM]([0-9.]+)_.*$", "\\1", x)
      q = sub("^[LM][0-9.]+_cad_q([0-9.]+)(cad|acc)$", "\\1", x)
      var = ifelse(grepl("cad$", x), "cadence", "acceleration")
      what = paste0("Percentile ", q, " of ", var, " during ", window_text(LM, X),
                    " cadence (", LM, X, ")")
    } else if (grepl("^cad_p[0-9.]+$", x)) {
      what = paste0("Percentile ", sub("^cad_p", "", x), " of the cadence distribution (steps per minute)")
    } else if (grepl("^mn_cad$", x)) {
      what = "Mean cadence (steps per minute)"
    } else if (grepl("^mn_cad_acc.+mg_.+$", x)) {
      rng = sub("^mn_cad_acc(.+)mg_.+$", "\\1", x)
      metric = sub("^mn_cad_acc.+mg_(.+)$", "\\1", x)
      what = paste0("Mean cadence (steps per minute) in epochs with ", metric, " ", range_text(rng, "mg"))
    } else if (grepl("^mn_cad_cad.+spm$", x)) {
      rng = sub("^mn_cad_cad(.+)spm$", "\\1", x)
      what = paste0("Mean cadence (steps per minute) in epochs with cadence ", range_text(rng, "steps per minute"))
    } else if (grepl("^dur_cad.+spm$", x)) {
      rng = sub("^dur_cad(.+)spm$", "\\1", x)
      what = paste0("Time accumulated (minutes) with cadence ", range_text(rng, "steps per minute"))
    } else if (grepl("^mn_.+_cad.+spm$", x)) {
      metric = sub("^mn_(.+)_cad.+spm$", "\\1", x)
      rng = sub("^mn_.+_cad(.+)spm$", "\\1", x)
      what = paste0("Mean ", metric, " (mg) in epochs with cadence ", range_text(rng, "steps per minute"))
    } else if (grepl("^tot_.+_acc.+mg_.+$", x)) {
      event = sub("^tot_(.+)_acc.+mg_.+$", "\\1", x)
      rng = sub("^tot_.+_acc(.+)mg_.+$", "\\1", x)
      metric = sub("^tot_.+_acc.+mg_(.+)$", "\\1", x)
      what = paste0("Total ", event, " in epochs with ", metric, " ", range_text(rng, "mg"))
    } else if (grepl("^tot_.+_cad.+spm$", x)) {
      event = sub("^tot_(.+)_cad.+spm$", "\\1", x)
      rng = sub("^tot_.+_cad(.+)spm$", "\\1", x)
      what = paste0("Total ", event, " in epochs with cadence ", range_text(rng, "steps per minute"))
    } else if (grepl("^tot_.+$", x)) {
      what = paste0("Total ", sub("^tot_", "", x))
    }
    return(what)
  }
  get_type_definition = function(x) {
    # variable names from part2_typesummary and part2_daytypesummary
    # (external function outputs with reporttype "type", prefix ExtFunType_ is
    # removed in the reports)
    what = NULL
    # duplicated column names (suffix .1, .2) cannot be interpreted
    if (grepl("hr?\\.[0-9]+$", x)) return(what)
    if (grepl("^(totdur|number|meandur)_B[0-9.]+M[0-9.]+%_.+$", x)) {
      type = sub("^(totdur|number|meandur)_.*$", "\\1", x)
      B = sub("^[a-z]+_B([0-9.]+)M.*$", "\\1", x)
      crit = sub("^[a-z]+_B[0-9.]+M([0-9.]+)%.*$", "\\1", x)
      category = sub("^[a-z]+_B[0-9.]+M[0-9.]+%_(.+)$", "\\1", x)
      what = paste0(switch(type, totdur = "Total time (minutes) in",
                           number = "Number of", meandur = "Mean duration (minutes) of"),
                    " bouts of at least ", B, " minutes in which ", crit,
                    "% of the epochs are classified as '", category,
                    "' by the external function")
    } else if (grepl("^tot_", x)) {
      rest = sub("^tot_", "", x)
      tm = type_metrics[startsWith(rest, paste0(type_metrics, "_"))]
      if (length(tm) > 0) {
        tm = tm[which.max(nchar(tm))]
        rest = substring(rest, nchar(tm) + 2)
        accpattern = "^(.+)_acc((atleast)?[0-9.]+(-[0-9.]+)?)mg_(.+)$"
        if (rest == "unclassified") {
          what = paste0("Time (minutes) with valid data that external function output ", tm,
                        " did not classify (NA)")
        } else if (rest == "invalid") {
          what = paste0("Time (minutes) with invalid data (e.g., non-wear), which is not classified",
                        " for external function output ", tm)
        } else if (grepl(accpattern, rest)) {
          what = paste0("Time (minutes) classified as '", sub(accpattern, "\\1", rest),
                        "' by external function output ", tm, " in epochs with ",
                        sub(accpattern, "\\5", rest), " ",
                        range_text(sub(accpattern, "\\2", rest), "mg"))
        } else {
          what = paste0("Time (minutes) classified as '", rest,
                        "' by external function output ", tm)
        }
      }
    }
    return(what)
  }
  # -------------------------------------------------------------------------
  # main script -------------------------------------------------------------
  # -------------------------------------------------------------------------
  resultsdir = file.path(metadatadir, "results")
  reports = c(file.path(resultsdir, c("part2_summary.csv", "part2_daysummary.csv",
                                      "part2_daysummary_longformat.csv")),
              dir(resultsdir, pattern = "^part2_.*eventsummary\\.csv$", full.names = TRUE),
              dir(resultsdir, pattern = "^part2_.*typesummary\\.csv$", full.names = TRUE),
              file.path(resultsdir, "QC", "data_quality_report.csv"))
  reports = unique(reports[file.exists(reports)])
  if (length(reports) == 0) return()
  directory = file.path(resultsdir, "variableDictionary")
  if (!dir.exists(directory)) dir.create(directory)
  for (ri in 1:length(reports)) {
    # read column names of the report
    R = data.table::fread(reports[ri], verbose = FALSE, nrows = 2, check.names = FALSE)
    cnames = colnames(R)
    # names of external function outputs with reporttype "type", derived from
    # the tot_<output>_unclassified variables in the type reports
    type_metrics = NULL
    if (grepl("typesummary", basename(reports[ri]))) {
      unclassified = grep("tot_.+_unclassified", cnames, value = TRUE)
      type_metrics = unique(sub("^((AD|WD|WE|WWD|WWE)_)?tot_(.+)_unclassified.*$", "\\3", unclassified))
    }
    dictionary = data.frame(Variable = cnames,
                            Definition = NA)
    for (coli in 1:length(cnames)) {
      dictionary[coli, "Definition"] = get_definition(cnames[coli])
    }
    # write csv reports with data dictionaries
    fn = paste0("part2_dictionary_", gsub("^part2_", "", basename(reports[ri])))
    data.table::fwrite(dictionary, file = file.path(directory, fn),
                       row.names = FALSE, na = "", sep = params_output[["sep_reports"]],
                       dec = params_output[["dec_reports"]])
  }
}
