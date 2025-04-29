check_params = function(params_sleep = c(), params_metrics = c(),
                        params_rawdata = c(), params_247 = c(),
                        params_phyact = c(), params_cleaning = c(),
                        params_output = c(), params_general = c()) {
  
  check_class = function(category, params, parnames, parclass) {
    for (parname in parnames) {
      if (length(params[[parname]]) > 0) {
        if (params[[parname]][1] %in% c("c()","NULL") == FALSE) { # because some variables are initialised empty
          x = params[[parname]]
          if (parclass == "numeric") {
            if (!is.numeric(x)) {
              stop(paste0("\n", category, " parameter ", parname, " is not ", parclass))
            }
          }
          if (parclass == "boolean") {
            if (!is.logical(x)) {
              stop(paste0("\n", category, " parameter ", parname, " is not ", parclass))
            }
          }
          if (parclass == "character") {
            if (!is.character(x)) {
              stop(paste0("\n", category, " parameter ", parname, " is not ", parclass))
            }
          }
        }
      }
    }
  }
  #-----------------------------------------------------------------------------------------
  if (length(params_sleep) > 0) { # Check class of sleep parameters
    numeric_params = c("anglethreshold", "timethreshold", "longitudinal_axis", 
                       "possible_nap_window", "possible_nap_dur",
                       "colid", "coln1", "def.noc.sleep", "nnights", 
                       "sleepefficiency.metric", "possible_nap_edge_acc", "HDCZA_threshold",
                       "possible_nap_gap", "oakley_threshold",
                       "nap_markerbutton_method",
                       "nap_markerbutton_max_distance",
                       "SRI1_smoothing_wsize_hrs",
                       "SRI1_smoothing_frac")
    boolean_params = c("ignorenonwear", "HASPT.ignore.invalid",
                       "relyonguider", "sleeplogidnum",
                       "impute_marker_button", "consider_marker_button",
                       "sib_must_fully_overlap_with_TimeInBed")
    character_params = c("HASPT.algo", "HASIB.algo", "Sadeh_axis", "nap_model",
                         "sleeplogsep", "sleepwindowType", "loglocation")
    check_class("Sleep", params = params_sleep, parnames = numeric_params, parclass = "numeric")
    check_class("Sleep", params = params_sleep, parnames = boolean_params, parclass = "boolean")
    check_class("Sleep", params = params_sleep, parnames = character_params, parclass = "character")
  }
  if (length(params_metrics) > 0) { # Check class of metrics parameters
    boolean_params = c("do.anglex", "do.angley", "do.anglez",
                       "do.zcx", "do.zcy", "do.zcz",
                       "do.enmo", "do.lfenmo", "do.en", "do.mad", "do.enmoa",
                       "do.roll_med_acc_x", "do.roll_med_acc_y", "do.roll_med_acc_z",
                       "do.dev_roll_med_acc_x", "do.dev_roll_med_acc_y", "do.dev_roll_med_acc_z",
                       "do.bfen", "do.hfen", "do.hfenplus", "do.lfen",
                       "do.lfx", "do.lfy", "do.lfz", "do.hfx", "do.hfy", "do.hfz",
                       "do.bfx", "do.bfy", "do.bfz", "do.brondcounts")
    check_class("Metrics", params = params_metrics, parnames = boolean_params, parclass = "boolean")
    check_class("Metrics", params = params_metrics, parnames = c("hb", "lb", "n", "zc.lb", "zc.hb",
                                                                 "zc.sb", "zc.order", "zc.scale"), parclass = "numeric")
  }
  if (length(params_rawdata) > 0) {
    numeric_params = c("chunksize", "spherecrit", "minloadcrit", "minimumFileSizeMB", "dynrange",
                       "rmc.col.acc", "interpolationType",
                       "rmc.firstrow.acc", "rmc.firstrow.header", "rmc.header.length",
                       "rmc.col.temp", "rmc.col.time",
                       "rmc.sf", "rmc.col.wear", "rmc.noise", "frequency_tol",
                       "rmc.scalefactor.acc", "nonwear_range_threshold")
    boolean_params = c("printsummary", "do.cal", "rmc.unsignedbit", "rmc.check4timegaps", "rmc.doresample",
                       "imputeTimegaps")
    character_params = c("backup.cal.coef", "rmc.dec", "rmc.unit.acc",
                         "rmc.unit.temp", "rmc.unit.time", "rmc.format.time",
                         "rmc.origin", "rmc.desiredtz", "rmc.configtz", "rmc.headername.sf",
                         "rmc.headername.sn", "rmc.headername.recordingid",
                         "rmc.header.structure")
    if (is.logical(params_rawdata[["rmc.noise"]])) {
      # Older config files used this, so overwrite with NULL value
      params_rawdata[["rmc.noise"]] = c()
    }
    check_class("Raw data", params = params_rawdata, parnames = numeric_params, parclass = "numeric")
    check_class("Raw data", params = params_rawdata, parnames = boolean_params, parclass = "boolean")
    check_class("Raw data", params = params_rawdata, parnames = character_params, parclass = "character")

    if (params_rawdata[["chunksize"]] < 0.1) params_rawdata[["chunksize"]] = 0.1
  }
  if (length(params_247) > 0) {
    # iglevels and qwindow can be numeric or character, so not tested
    numeric_params = c("qlevels", "ilevels", "IVIS_windowsize_minutes", "IVIS_epochsize_seconds",
                       "IVIS.activity.metric", "IVIS_acc_threshold",
                       "qM5L5", "MX.ig.min.dur", "M5L5res", "winhr", "LUXthresholds", "LUX_cal_constant",
                       "LUX_cal_exponent", "LUX_day_segments", "L5M5window", "clevels", "SRI2_WASOmin")
    boolean_params = c("cosinor", "part6CR", "part6HCA", "part6DFA")
    character_params = c("qwindow_dateformat", "part6Window")
    check_class("247", params = params_247, parnames = numeric_params, parclass = "numeric")
    check_class("247", params = params_247, parnames = boolean_params, parclass = "boolean")
    check_class("247", params = params_247, parnames = character_params, parclass = "character")
  }
  if (length(params_phyact) > 0) {
    numeric_params = c("mvpathreshold", "boutcriter", "mvpadur",
                       "boutcriter.in", "boutcriter.lig", "boutcriter.mvpa",
                       "threshold.lig", "threshold.mod", "threshold.vig", "boutdur.mvpa",
                       "boutdur.in", "boutdur.lig")
    character_params = c("frag.metrics", "part6_threshold_combi")
    check_class("phyact", params = params_phyact, parnames = numeric_params, parclass = "numeric")
    # check_class("phyact", params = params_phyact, parnames = boolean_params, parclass = "boolean")
    check_class("phyact", params = params_phyact, parnames = character_params, parclass = "character")
    
  }
  if (length(params_cleaning) > 0) {
    numeric_params = c("includedaycrit", "ndayswindow", "data_masking_strategy", "maxdur", "hrs.del.start",
                       "hrs.del.end", "includedaycrit.part5", "minimum_MM_length.part5",
                       "includenightcrit", "max_calendar_days", "includecrit.part6", "includenightcrit.part5",
                       "nonwearFiltermaxHours", "nonwearFilterWindow")

    boolean_params = c("excludefirstlast.part5", "do.imp", "excludefirstlast",
                       "excludefirst.part4", "excludelast.part4", "nonWearEdgeCorrection")
    character_params = c("data_cleaning_file", "TimeSegments2ZeroFile")
    check_class("cleaning", params = params_cleaning, parnames = numeric_params, parclass = "numeric")
    check_class("cleaning", params = params_cleaning, parnames = boolean_params, parclass = "boolean")
    check_class("cleaning", params = params_cleaning, parnames = character_params, parclass = "character")
  }
  if (length(params_output) > 0) {
    numeric_params = c("viewingwindow", "criterror", "visualreport_hrsPerRow",
                       "visualreport_validcrit")
    boolean_params = c("epochvalues2csv", "save_ms5rawlevels", "save_ms5raw_without_invalid",
                       "storefolderstructure", "dofirstpage", "visualreport", "week_weekend_aggregate.part5",
                       "do.part3.pdf", "outliers.only", "do.visual", "do.sibreport", "visualreport_without_invalid",
                       "do.part2.pdf", "old_visualreport", "require_complete_lastnight_part5")

    character_params = c("save_ms5raw_format", "timewindow", "sep_reports", "sep_config",
                         "dec_reports", "dec_config", "visualreport_focus", "method_research_vars")
    check_class("output", params = params_output, parnames = numeric_params, parclass = "numeric")
    check_class("output", params = params_output, parnames = boolean_params, parclass = "boolean")
    check_class("output", params = params_output, parnames = character_params, parclass = "character")
  }
  if (length(params_general) > 0) {
    numeric_params = c("maxNcores", "windowsizes", "idloc", "dayborder",
                       "expand_tail_max_hours", "maxRecordingInterval",
                       "recording_split_overlap")
    boolean_params = c("overwrite", "print.filename", "do.parallel", "part5_agg2_60seconds",
                       "recording_split_ignore_edges")
    character_params = c("acc.metric", "desiredtz", "configtz", "sensor.location", 
                         "dataFormat", "extEpochData_timeformat", "recording_split_times",
                         "recording_split_timeformat")
    check_class("general", params = params_general, parnames = numeric_params, parclass = "numeric")
    check_class("general", params = params_general, parnames = boolean_params, parclass = "boolean")
    check_class("general", params = params_general, parnames = character_params, parclass = "character")

    ws3 = params_general[["windowsizes"]][1]; ws2 = params_general[["windowsizes"]][2]; ws = params_general[["windowsizes"]][3]
    if (ws2/60 != round(ws2/60)) {
      ws2 = as.numeric(60 * ceiling(ws2/60))
      warning(paste0("The long windowsize needs to be a multitude of 1 minute periods.\n",
                     "Long windowsize has now been automatically adjusted to ",
                     ws2, " seconds in order to meet this criteria."), call. = FALSE)
    }
    if (ws2/ws3 != round(ws2/ws3)) {
      def = c(1,5,10,15,20,30,60)
      def2 = abs(def - ws3)
      ws3 = as.numeric(def[which(def2 == min(def2))])
      warning(paste0("The long windowsize needs to be a multitude of short windowsize.\n",
                     "The short windowsize has now been automatically adjusted to ",
                     ws3, " seconds in order to meet this criteria.\n"), call. = FALSE)
    }
    if (ws/ws2 != round(ws/ws2)) {
      ws = ws2 * ceiling(ws/ws2)
      warning(paste0("The third value of parameter windowsizes needs to be a multitude of the second value.\n",
                     "The third value has been automatically adjusted to ",
                     ws, " seconds in order to meet this criteria.\n"), call. = FALSE)
    }    
    params_general[["windowsizes"]] = c(ws3, ws2, ws)
  }
  #-----------------------------------------------------------------------------------
  # Check value combinations and apply corrections if not logical
  if (length(params_metrics) > 0) {
    if (params_metrics[["do.brondcounts"]] == TRUE) {
      stop(paste0("\nThe brondcounts option has been deprecated following issues with the ",
                  "activityCounts package. We will reinsert brondcounts ",
                  "once the issues are resolved. Consider using parameter do.neishabouricounts, ",
                  "for more information see package documentation."), call. = FALSE)
    }
  }
  
  if (length(params_rawdata) > 0) {
    if (params_rawdata[["frequency_tol"]] < 0 | params_rawdata[["frequency_tol"]] > 1) {
      stop(paste0("\nParameter frequency_tol is ", params_rawdata[["frequency_tol"]],
                  " , please adjust such that it is a number between 0 and 1"))
    }
  }
  
  if (length(params_sleep) > 0) {
    if (length(params_sleep[["def.noc.sleep"]]) != 2) {
      if (params_sleep[["HASPT.algo"]][1] %in% c("HorAngle", "NotWorn", "MotionWare", "HLRB") == FALSE) {
        params_sleep[["HASPT.algo"]] = "HDCZA"
      }
      if (length(params_sleep[["HASPT.algo"]]) == 2 && params_sleep[["HASPT.algo"]][2] == "NotWorn") {
        params_sleep[["HASPT.algo"]] = params_sleep[["HASPT.algo"]][2:1] # NotWorn is expected to be first
      }
    } else if (length(params_sleep[["def.noc.sleep"]]) == 2) {
      params_sleep[["HASPT.algo"]] = "notused"
    }
    if (length(params_sleep[["possible_nap_gap"]]) != 1) {
      stop(paste0("Parameter possible_nap_gap has length ", length(params_sleep[["possible_nap_gap"]]), 
                  " while length 1 is expected"), call. = FALSE)
    }
    if (!is.null(params_sleep[["possible_nap_window"]]) &&
        length(params_sleep[["possible_nap_window"]]) != 2) {
      stop(paste0("Parameter possible_nap_window has length ", length(params_sleep[["possible_nap_window"]]), 
                  " while length 2 is expected"), call. = FALSE)
    }
    if (!is.null(params_sleep[["possible_nap_dur"]]) &&
        length(params_sleep[["possible_nap_dur"]]) != 2) {
      stop(paste0("Parameter possible_nap_dur has length ", length(params_sleep[["possible_nap_dur"]]), 
                  " while length 2 is expected"), call. = FALSE)
    }
    if (!is.null(params_sleep[["possible_nap_window"]]) &&
        !is.null(params_sleep[["possible_nap_dur"]])) {
      params_output[["do.sibreport"]] = TRUE
      params_output[["save_ms5raw_format"]] = unique(c(params_output[["save_ms5raw_format"]], "RData"))
      params_output[["save_ms5rawlevels"]] = TRUE
      params_output[["save_ms5raw_without_invalid"]] = FALSE
    }
  }
  
  if (length(params_metrics) > 0 & length(params_sleep) > 0) {
    
    sib_90s_algo_names = c("Sadeh1994", "Galland2012", "ColeKripke1992", "Oakley1997")
    if (any(params_sleep[["HASIB.algo"]] %in% sib_90s_algo_names == TRUE)) {
      if (params_sleep[["Sadeh_axis"]] %in% c("X","Y","Z") == FALSE) {
        warning("Parameter Sadeh_axis does not have meaningful value, it needs to be X, Y or Z (capital)", call. = FALSE)
      }
      if (params_sleep[["Sadeh_axis"]] == "X" & params_metrics[["do.zcx"]] == FALSE) params_metrics[["do.zcx"]] =  TRUE
      if (params_sleep[["Sadeh_axis"]] == "Y" & params_metrics[["do.zcy"]] == FALSE) params_metrics[["do.zcy"]] =  TRUE
      if (params_sleep[["Sadeh_axis"]] == "Z" & params_metrics[["do.zcz"]] == FALSE) params_metrics[["do.zcz"]] =  TRUE
    } else { # vanHees2015
      params_sleep[["Sadeh_axis"]] = "" # not used
    }
  }
  
  if (length(params_general) > 0 & length(params_metrics) > 0 & length(params_sleep) > 0) {
    if (params_general[["sensor.location"]] == "hip" &&
        params_sleep[["HASPT.algo"]][1] %in% c("notused", "NotWorn") == FALSE) {
      if (params_metrics[["do.anglex"]] == FALSE | params_metrics[["do.angley"]] == FALSE | params_metrics[["do.anglez"]] == FALSE) {
        warning(paste0("\nWhen working with hip data all three angle metrics are needed,",
                       "so GGIR now auto-sets parameters do.anglex, do.angley, and do.anglez to TRUE."), call. = FALSE)
        params_metrics[["do.anglex"]] = params_metrics[["do.angley"]] = params_metrics[["do.anglez"]] = TRUE
      }
      if (length(params_sleep[["HASPT.algo"]]) == 1 && params_sleep[["HASPT.algo"]][1] != "HorAngle") {
        warning("\nChanging HASPT.algo value to HorAngle, because sensor.location is set as hip", call. = FALSE)
        params_sleep[["HASPT.algo"]] = "HorAngle"; params_sleep[["def.noc.sleep"]] = 1
      }
    }
  }
  
  if (length(params_sleep) > 0) {
    if (length(params_sleep[["loglocation"]]) == 1) {
      if (params_sleep[["loglocation"]] == "") {
        params_sleep[["loglocation"]] = c() #inserted because some users mistakingly use this
      } else {
        # Convert paths from Windows specific slashed to generic slashes
        params_sleep[["loglocation"]] = gsub(pattern = "\\\\", replacement = "/", x = params_sleep[["loglocation"]])
      }
    }
    
    if (length(params_sleep[["loglocation"]]) > 0 & length(params_sleep[["def.noc.sleep"]]) != 1) {
      warning(paste0("\nloglocation was specified and def.noc.sleep does not have length of 1, this is not compatible. ",
                     " We assume you want to use the sleeplog and misunderstood",
                     " parameter def.noc.sleep. Therefore, we will reset def.noc.sleep to its default value of 1"), call. = FALSE)
      params_sleep[["def.noc.sleep"]] = 1
    }
    
    if (params_sleep[["HASPT.algo"]][1] == "HorAngle" & params_sleep[["sleepwindowType"]] != "TimeInBed") {
      warning("\nHASPT.algo is set to HorAngle, therefore auto-updating sleepwindowType to TimeInBed", call. = FALSE)
      params_sleep[["sleepwindowType"]] = "TimeInBed"
    }
    
    if (length(params_sleep[["loglocation"]]) == 0 &
        params_sleep[["HASPT.algo"]][1] != "HorAngle" &
        params_sleep[["HASPT.algo"]][1] != "NotWorn" &
        params_sleep[["sleepwindowType"]] != "SPT") {
      warning("\nAuto-updating sleepwindowType to SPT because no sleeplog used and neither HASPT.algo HorAngle or NotWorn used.", call. = FALSE)
      params_sleep[["sleepwindowType"]] = "SPT"
    }
  }
  
  if (length(params_cleaning) > 0) {
    # overwrite data_masking_strategy with strategy in case the latter is used
    if (params_cleaning[["strategy"]] != params_cleaning[["data_masking_strategy"]]
        & params_cleaning[["strategy"]] != 1) {
      params_cleaning[["data_masking_strategy"]] = params_cleaning[["strategy"]]
    }
    if (params_cleaning[["data_masking_strategy"]] %in% c(2, 4) & params_cleaning[["hrs.del.start"]] != 0) {
      warning(paste0("\nSetting parameter hrs.del.start in combination with data_masking_strategy = ",
                     params_cleaning[["data_masking_strategy"]]," is not meaningful, because this is only used when straytegy = 1"), call. = FALSE)
    }
    if (params_cleaning[["data_masking_strategy"]] %in% c(2, 4) & params_cleaning[["hrs.del.end"]] != 0) {
      warning(paste0("\nSetting parameter hrs.del.end in combination with data_masking_strategy = ",
                     params_cleaning[["data_masking_strategy"]]," is not meaningful, because this is only used when straytegy = 1"), call. = FALSE)
    }
    if (!(params_cleaning[["data_masking_strategy"]] %in% c(3, 5)) & params_cleaning[["ndayswindow"]] != 7) {
      warning(paste0("\nSetting parameter ndayswindow in combination with data_masking_strategy = ",
                     params_cleaning[["data_masking_strategy"]]," is not meaningful, because this is only used when data_masking_strategy = 3 or data_masking_strategy = 5"), call. = FALSE)
    }
    if (params_cleaning[["data_masking_strategy"]] == 5 &
        params_cleaning[["ndayswindow"]] != round(params_cleaning[["ndayswindow"]])) {
      newValue = round(params_cleaning[["ndayswindow"]])
      warning(paste0("\nParameter ndayswindow has been rounded from ",
                     params_cleaning[["ndayswindow"]], " to ", newValue, " days",
                     "because when data_masking_strategy == 5 we expect an integer value", call. = FALSE))
      params_cleaning[["ndayswindow"]] = newValue
    }
    if (length(params_cleaning[["data_cleaning_file"]]) > 0) {
      # Convert paths from Windows specific slashed to generic slashes
      params_cleaning[["data_cleaning_file"]] = gsub(pattern = "\\\\",
                                                     replacement = "/", x = params_cleaning[["data_cleaning_file"]])
    }
    if (params_cleaning[["includedaycrit.part5"]] < 0) {
      stop("\nNegative value of includedaycrit.part5 is not allowed, please change.")
    } else if (params_cleaning[["includedaycrit.part5"]]  > 24) {
      stop(paste0("\nIncorrect value of includedaycrit.part5, this should be",
                     " a fraction of the day between zero and one or the number ",
                     "of hours in a day."))
    }
    if (!is.null(params_cleaning[["nonwearFiltermaxHours"]])) {
      if (params_cleaning[["nonwearFiltermaxHours"]] < 0 ||
          params_cleaning[["nonwearFiltermaxHours"]] > 12) {
        stop("Parameters nonwearFiltermaxHours is expected to have a value > 0 and < 12")
      }
      if (!is.null(params_cleaning[["nonwearFilterWindow"]])) {
        if (length(params_cleaning[["nonwearFilterWindow"]]) != 2) {
          stop("Parameter nonwearFilterWindow does not have expected length of 2, please fix.", call. = FALSE)
        }
        if (params_cleaning[["nonwearFilterWindow"]][1] < params_cleaning[["nonwearFilterWindow"]][2] &&
            params_cleaning[["nonwearFilterWindow"]][2] > 18 &&
            params_cleaning[["nonwearFilterWindow"]][1] < 12) {
          warning(paste0("The NonwearFilter applied to window starting at ", 
                         params_cleaning[["nonwearFilterWindow"]][1], " and ending at ",
                         params_cleaning[["nonwearFilterWindow"]][2], 
                         " this is probably not the night, please check that order of",
                         " values in nonwearFilterWindow is correct"), call. = FALSE)
        }
      }
    }

    if (params_cleaning[["includenightcrit.part5"]] < 0) {
      stop("\nNegative value of includenightcrit.part5 is not allowed, please change.")
    } else if (params_cleaning[["includenightcrit.part5"]]  > 24) {
      stop(paste0("\nIncorrect value of includenightcrit.part5, this should be",
                     " a fraction of the day between zero and one or the number ",
                     "of hours in a day."))
    }
    if (any(params_cleaning[["includecrit.part6"]] < 0) | any(params_cleaning[["includecrit.part6"]] > 1)) {
      stop("Values of includecrit.part6 are not in the range [0, 1]. Please fix.")
    }
  }
  if (length(params_phyact) > 0) {
    if (length(params_phyact[["bout.metric"]]) > 0 |
        length(params_phyact[["closedbout"]]) > 0) {
      warning(paste0("\nParameters bout.metric and closedbout are no longer used",
                     " by GGIR and ignored, we now use one piece of code stored in",
                     " function g.getbout."), call. = FALSE)
    }
    if (length(params_phyact[["mvpadur"]]) != 3) {
      params_phyact[["mvpadur"]] = c(1,5,10)
      warning("\nmvpadur needs to be a vector with length three, value now reset to default c(1, 5, 10)", call. = FALSE)
    }
    if (is.null(params_phyact[["mvpathreshold"]]) == TRUE) {
      params_phyact[["mvpathreshold"]] = params_phyact[["threshold.mod"]]
    }
    if (is.null(params_phyact[["boutcriter"]]) == TRUE) {
      params_phyact[["boutcriter"]] = params_phyact[["boutcriter.mvpa"]]
    }
    if ((length(params_phyact[["threshold.lig"]]) == 1 &&
        length(params_phyact[["threshold.mod"]]) == 1 &&
        length(params_phyact[["threshold.vig"]]) == 1) | is.null(params_phyact[["part6_threshold_combi"]])) {
      params_phyact[["part6_threshold_combi"]] = paste(params_phyact[["threshold.lig"]][1],
                                                       params_phyact[["threshold.mod"]][1],
                                                       params_phyact[["threshold.vig"]][1], sep = "_")
    }
  }
  # params output 
  if (length(params_output) > 0) {
    if (!all(params_output[["save_ms5raw_format"]] %in% c("RData", "csv"))) {
      formats2keep = which(params_output[["save_ms5raw_format"]] %in% c("RData", "csv"))
      if (length(formats2keep) > 0) {
        params_output[["save_ms5raw_format"]] = params_output[["save_ms5raw_format"]][formats2keep]
      } else {
        stop("Parameter save_ms5raw_format incorrectly specified, please fix.", call. = FALSE)
      }
    }
    if (params_output[["sep_reports"]] == params_output[["dec_reports"]]) {
      stop(paste0("\nYou have set sep_reports and dec_reports both to ",
                  params_output[["sep_reports"]], " this is ambiguous. Please fix."))
    }
    if (params_output[["sep_config"]] == params_output[["dec_config"]]) {
      stop(paste0("\nYou have set sep_config and dec_config both to ",
                  params_output[["sep_config"]], " this is ambiguous. Please fix."))
    }
    if (params_output[["visualreport_hrsPerRow"]] < 24 ||
        params_output[["visualreport_hrsPerRow"]] > 48) {
      stop("Parameter visualreport_hrsPerRow is expected to be set in the range 24-48")
    }
  }
  # params 247
  if (length(params_247) > 0) {
    if (length(params_247[["iglevels"]]) > 0) {
      if (length(params_247[["iglevels"]]) == 1) {
        params_247[["iglevels"]] = c(seq(0, 4000, by = 25), 8000) # to introduce option to just say TRUE
      }
    }
    if (length(params_247[["qwindow"]]) > 0) {
      if (is.character(params_247[["qwindow"]])) {
        # Convert paths from Windows specific slashed to generic slashes
        params_247[["qwindow"]] = gsub(pattern = "\\\\", replacement = "/", x = params_247[["qwindow"]])
      }
    }
    if (length(params_247[["LUX_day_segments"]]) > 0) {
      params_247[["LUX_day_segments"]] = sort(unique(round(params_247[["LUX_day_segments"]])))
      if (params_247[["LUX_day_segments"]][1] != 0) {
        params_247[["LUX_day_segments"]] = c(0, params_247[["LUX_day_segments"]])
      }
      if (params_247[["LUX_day_segments"]][length(params_247[["LUX_day_segments"]])] != 24) {
        params_247[["LUX_day_segments"]] = c(params_247[["LUX_day_segments"]], 24)
      }
    }
    # params 247 & params output
    if (length(params_output[["save_ms5raw_format"]]) == 1 && 
        params_output[["save_ms5raw_format"]] == "csv") {
      # always add RData if only csv is specified, because otherwise visualreport cannot be generated
      params_output[["save_ms5raw_format"]] = c(params_output[["save_ms5raw_format"]], "RData")
    }

    if (length(params_247[["clevels"]]) == 1) {
      warning("\nParameter clevels expects a number vector of at least 2 values, current length is 1", call. = FALSE)
    }
  }
  
  if (length(params_output) > 0 && length(params_247) > 0) {
    if (params_247[["part6HCA"]] == TRUE || params_247[["part6CR"]] == TRUE || 
        params_output[["visualreport"]] == TRUE) {
      # Add RData because part 6 / visualreport will need it
      params_output[["save_ms5raw_format"]] = unique(c(params_output[["save_ms5raw_format"]], "RData"))
      params_output[["save_ms5rawlevels"]] = TRUE
      params_output[["save_ms5raw_without_invalid"]] = FALSE
    }
  }
  if (!is.null(params_general[["expand_tail_max_hours"]])) {
    if (is.null(params_general[["recordingEndSleepHour"]]) & params_general[["expand_tail_max_hours"]] != 0) {
      params_general[["recordingEndSleepHour"]] = 24 - params_general[["expand_tail_max_hours"]] # redefine the argument
      params_general[["expand_tail_max_hours"]] = NULL # set to null so that it keeps this configuration in the config file for the next run of the script.
      stop("\nThe parameter expand_tail_max_hours has been replaced by",
           " recordingEndSleepHour which has a different definition. Please",
           " see the documentation for further details and replace",
           " expand_tail_max_hour in your function call and config.csv file.", call. = FALSE)
    } else {
      # If both are defined, this is probably because expand_tail_max_hours is
      # in the config file from a previous run
      params_general[["expand_tail_max_hours"]] = NULL # set to null so that it keeps this configuration in the config file for the next run of the script.
      warning("\nBoth expand_tail_max_hours and recordingEndSleepHour",
              " are defined. GGIR will only use recordingEndSleepHour",
              " and expand_tail_max_hours will be set to NULL.", call. = FALSE)
    }
  }
  
  if (length(params_general) > 0 & length(params_sleep) > 0) {
    if (params_sleep[["HASPT.algo"]][1] == "HorAngle") {
      # Not everywhere in the code we check that when HASPT.algo is HorAngle, sensor.location is hip.
      # However, the underlying assumption is that they are linked. Even if a study would
      # use a waist or chest worn sensor we refer to it as hip as the orientation and need
      # for detecting longitudinal axis are the same. 
      # Therefore, sensor.location should be forced to hip if HASPT.algo is HorAngle.
      # On the other hand hip does not mean that HorAngle needs to be used, because
      # when using count data from the hip the user may prefer the HASPT.algo=NotWorn.
      params_general[["sensor.location"]] = "hip"
    }
    
    if (params_general[["dataFormat"]] %in% c("sensewear_xls", "fitbit_json")) {
      if (params_sleep[["HASIB.algo"]][1] != "data") {
        params_sleep[["HASIB.algo"]] = "data"
        warning(paste0("HASIB.algo has been set to \"data\", because dataFormat is",
                       " ", params_general[["dataFormat"]], " for which the sleep ",
                       " classification inside the data is the only route."), call. = FALSE)
      }
    }
    
  }
  
  if (length(params_metrics) > 0 & length(params_general) > 0) {
    if (params_general[["dataFormat"]] %in% c("actiwatch_awd", "actiwatch_csv", "phb_xlsx")) {
      params_metrics[["do.enmo"]] = params_metrics[["do.anglez"]] = FALSE
      params_metrics[["do.anglex"]] = params_metrics[["do.angley"]] = FALSE
      params_metrics[["do.anglez"]] = params_metrics[["do.enmoa"]] = FALSE
      params_metrics[["do.enmo"]] = params_metrics[["do.bfen"]] = FALSE
      params_metrics[["do.mad"]] = params_metrics[["do.lfenmo"]] = FALSE
      params_general[["acc.metric"]] = "ExtAct"
      if (length(params_sleep) > 0) {
        if (any(params_sleep[["HASIB.algo"]] == "vanHees2015")) {
          stop(paste0("\nSleep algorithm ", params_sleep[["HASIB.algo"]], " is not a valid",
                      " setting in combination with dataFormat set to ",
                      params_general[["dataFormat"]], " Please fix"), call. = FALSE)
        }
      }
    } else if (params_general[["dataFormat"]] == "ukbiobank") {
      # Turn all commonly used metrics to FALSE
      params_metrics[["do.anglex"]] = params_metrics[["do.angley"]] = FALSE
      params_metrics[["do.anglez"]] = params_metrics[["do.enmoa"]] = FALSE
      params_metrics[["do.enmo"]] = params_metrics[["do.bfen"]] = FALSE
      params_metrics[["do.mad"]] = FALSE
      params_general[["acc.metric"]] = "LFENMO"
    } else if (params_general[["dataFormat"]] == "sensewear_xls") {
      params_general[["acc.metric"]] = "ExtAct"
    }
  }
  if (!is.null(params_general[["recordingEndSleepHour"]])) {
    # stop if expand_tail_max_hours was defined before 7pm
    if (params_general[["recordingEndSleepHour"]] < 19) {
      stop(paste0("\nrecordingEndSleepHour expects the latest time at which",
                  " the participant is expected to fall asleep. recordingEndSleepHour",
                  " has been defined as ", params_general[["recordingEndSleepHour"]],
                  ", which does not look plausible, please specify time at or later than 19:00",
                  " . Please note that it is your responsibility as user to verify that the
                  assumption is credible."), call. = FALSE)
    }
  }
  
  if (length(params_general) > 0) {
    if (!is.null(params_general[["maxRecordingInterval"]])) {
      if (params_general[["maxRecordingInterval"]] > 24 * 21) {
        stop(paste0("A maxRecordingInterval value higher than 21 days (504 hours) is not permitted,",
                    " please specify a lower value."), call. = FALSE)
      }
    }
    if (!is.null(params_general[["recording_split_times"]])) {
      if (!file.exists(params_general[["recording_split_times"]])) {
        stop(paste0("File .../", basename(params_general[["recording_split_times"]]),
                    " as specified with parameter recording_split_times does not exist, ",
                    " please fix."), call. = FALSE)
        
      }
    }
  }
  
  # cleaning parameters for segments
  if (length(params_cleaning) > 0) {
    if (is.null(params_cleaning[["includedaycrit.part5"]]) == TRUE) {
      stop(paste0("\nSetting includedaycrit.part5 to an empty value is not allowed",
                  ", please change."), call. = FALSE)
    } else if (params_cleaning[["includedaycrit.part5"]] < 0) {
      stop(paste0("\nNegative value of includedaycrit.part5 is not allowed",
                     ", please change."), call. = FALSE)
    } else if (params_cleaning[["includedaycrit.part5"]] > 25) {
      stop(paste0("\nIncorrect value of includedaycrit.part5, this should ",
                     "be a fraction of the day between zero and one or the ",
                     "number of hours in a day."), call. = FALSE)
    }
    if (is.null(params_cleaning[["segmentWEARcrit.part5"]])) {
      # if null, then assign default value
      params_cleaning[["segmentWEARcrit.part5"]] = 0.5
      warning(paste0("\nsegmentWEARcrit.part5 is expected to be a number between 0 and 1",
                     ", the default value has been assigned (i.e., 0.5) "), call. = FALSE)
    } else if (params_cleaning[["segmentWEARcrit.part5"]] < 0 | 
               params_cleaning[["segmentWEARcrit.part5"]] > 1) {
      stop(paste0("Incorrect value of segmentWEARcrit.part5, this should be a ",
                  "fraction between zero and one, please change."), 
           call. = FALSE)
    }
    if (length(params_cleaning[["segmentDAYSPTcrit.part5"]]) != 2) {
      stop("\nParameter segmentDAYSPTcrit.part5 is expected to be a numeric vector of length 2", call. = FALSE)
    }
    if (any(params_cleaning[["segmentDAYSPTcrit.part5"]] < 0) | 
        any(params_cleaning[["segmentDAYSPTcrit.part5"]] > 1)) {
      stop(paste0("Incorrect values of segmentDAYSPTcrit.part5, these should be a ",
                  "fractions between zero and one, please change."), 
           call. = FALSE)
    }
  }
  
  invisible(list(params_sleep = params_sleep,
                 params_metrics = params_metrics,
                 params_rawdata = params_rawdata,
                 params_247 = params_247,
                 params_phyact = params_phyact,
                 params_cleaning = params_cleaning,
                 params_output = params_output,
                 params_general = params_general))
}
