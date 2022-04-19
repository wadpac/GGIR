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
              stop(paste0("\n", category, " argument ", parname, " is not ", parclass))
            }
          }
          if (parclass == "boolean") {
            if (!is.logical(x)) {
              stop(paste0("\n", category, " argument ", parname, " is not ", parclass))
            }
          }
          if (parclass == "character") {
            if (!is.character(x)) {
              stop(paste0("\n", category, " argument ", parname, " is not ", parclass))
            }
          }
        }
      }
    }
  }
  #-----------------------------------------------------------------------------------------
  if (length(params_sleep) > 0) { # Check class of sleep parameters
    numeric_params = c("anglethreshold", "timethreshold", "longitudinal_axis", "possible_nap_window", "possible_nap_dur",
                       "colid", "coln1", "def.noc.sleep", "nnights")
    boolean_params = c("ignorenonwear", "constrain2range", "HASPT.ignore.invalid",
                       "relyonguider", "sleeplogidnum")
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
    check_class("Metrics", params = params_metrics, parnames = c("hb", "lb", "n"), parclass = "numeric")
  }
  if (length(params_rawdata) > 0) {
    numeric_params = c("chunksize", "spherecrit", "minloadcrit", "minimumFileSizeMB", "dynrange",
                       "rmc.col.acc", "interpolationType",
                       "rmc.firstrow.acc", "rmc.firstrow.header", "rmc.header.length",
                       "rmc.col.temp", "rmc.col.time", "rmc.bitrate", "rmc.dynamic_range",
                       "rmc.sf", "rmc.col.wear", "rmc.noise")
    boolean_params = c("printsummary", "do.cal", "rmc.unsignedbit", "rmc.check4timegaps", "rmc.doresample",
                       "imputeTimegaps")
    character_params = c("backup.cal.coef", "rmc.dec", "rmc.unit.acc", 
                         "rmc.unit.temp", "rmc.unit.time", "rmc.format.time", 
                         "rmc.origin", "rmc.desiredtz", "rmc.headername.sf", 
                         "rmc.headername.sn", "rmc.headername.recordingid", 
                         "rmc.header.structure")
    if (is.logical(params_rawdata[["rmc.noise"]])) {
      # Older config files used this, so overwrite with NULL value
      params_rawdata[["rmc.noise"]] = c() 
    }
    check_class("Raw data", params = params_rawdata, parnames = numeric_params, parclass = "numeric")
    check_class("Raw data", params = params_rawdata, parnames = boolean_params, parclass = "boolean")
    check_class("Raw data", params = params_rawdata, parnames = character_params, parclass = "character")
  }
  if (length(params_247) > 0) {
    # iglevels and qwindow can be numeric or character, so not tested
    numeric_params = c("qlevels", "ilevels", "IVIS_windowsize_minutes", "IVIS_epochsize_seconds", "IVIS.activity.metric", 
                       "qM5L5", "MX.ig.min.dur", "M5L5res", "winhr", "LUXthresholds", "LUX_cal_constant", 
                       "LUX_cal_exponent", "LUX_day_segments", "window.summary.size", "L5M5window")
    character_params = c("qwindow_dateformat")
    check_class("247", params = params_247, parnames = numeric_params, parclass = "numeric")
    check_class("247", params = params_247, parnames = character_params, parclass = "character")
  }
  if (length(params_phyact) > 0) {
    numeric_params = c("mvpathreshold", "boutcriter", "mvpadur", 
                       "boutcriter.in", "boutcriter.lig", "boutcriter.mvpa", 
                       "threshold.lig", "threshold.mod", "threshold.vig", "boutdur.mvpa", 
                       "boutdur.in", "boutdur.lig", "bout.metric")
    boolean_params = "closedbout"
    check_class("phyact", params = params_phyact, parnames = numeric_params, parclass = "numeric")
    check_class("phyact", params = params_phyact, parnames = boolean_params, parclass = "boolean")
    check_class("phyact", params = params_phyact, parnames = "frag.metrics", parclass = "character")
  }
  if (length(params_cleaning) > 0) {
    numeric_params = c("includedaycrit", "ndayswindow", "strategy", "maxdur", "hrs.del.start",
                       "hrs.del.end", "includedaycrit.part5", "minimum_MM_length.part5", "includenightcrit", "max_calendar_days")
    boolean_params = c("excludefirstlast.part5", "do.imp", "excludefirstlast", "excludefirst.part4", "excludelast.part4")
    character_params = c("selectdaysfile", "data_cleaning_file", "TimeSegments2ZeroFile")
    check_class("cleaning", params = params_cleaning, parnames = numeric_params, parclass = "numeric")
    check_class("cleaning", params = params_cleaning, parnames = boolean_params, parclass = "boolean")
    check_class("cleaning", params = params_cleaning, parnames = character_params, parclass = "character")
  }
  if (length(params_output) > 0) {
    numeric_params = c("viewingwindow", "criterror")
    boolean_params = c("epochvalues2csv", "save_ms5rawlevels", "save_ms5raw_without_invalid", 
                       "storefolderstructure", "dofirstpage", "visualreport", "week_weekend_aggregate.part5",
                       "do.part3.pdf", "outliers.only", "do.visual", "do.sibreport")
    character_params = c("save_ms5raw_format", "timewindow")
    check_class("output", params = params_output, parnames = numeric_params, parclass = "numeric")
    check_class("output", params = params_output, parnames = boolean_params, parclass = "boolean")
    check_class("output", params = params_output, parnames = character_params, parclass = "character")
  }
  if (length(params_general) > 0) {
    numeric_params = c("maxNcores", "windowsizes", "idloc", "dayborder")
    boolean_params = c("overwrite", "print.filename", "do.parallel", "part5_agg2_60seconds")
    character_params = c("acc.metric", "desiredtz", "configtz", "sensor.location")
    check_class("general", params = params_general, parnames = numeric_params, parclass = "numeric")
    check_class("general", params = params_general, parnames = boolean_params, parclass = "boolean")
    check_class("general", params = params_general, parnames = character_params, parclass = "character")
  }
  
  #-----------------------------------------------------------------------------------
  # Check value combinations and apply corrections if not logical
  if (length(params_metrics) > 0 & length(params_sleep) > 0) {
    if (length(params_sleep[["def.noc.sleep"]]) != 2) {
      if (params_sleep[["HASPT.algo"]] != "HorAngle") {
        params_sleep[["HASPT.algo"]] = "HDCZA"
      }
    } else if (length(params_sleep[["def.noc.sleep"]]) == 2) {
      params_sleep[["HASPT.algo"]] = "notused"
    }
    if (params_general[["sensor.location"]] == "hip" &  params_sleep[["HASPT.algo"]] != "notused") {
      if (params_metrics[["do.anglex"]] == FALSE | params_metrics[["do.angley"]] == FALSE | params_metrics[["do.anglez"]] == FALSE) {
        warning(paste0("\nWhen working with hip data all three angle metrics are needed,",
                       "so GGIR now auto-sets arguments do.anglex, do.angley, and do.anglez to TRUE."), call. = FALSE)
        params_metrics[["do.anglex"]] = params_metrics[["do.angley"]] = params_metrics[["do.anglez"]] = TRUE
      }
      if (params_sleep[["HASPT.algo"]] != "HorAngle") {
        warning("\nChanging HASPT.algo value to HorAngle, because sensor.location is set as hip", call. = FALSE)
        params_sleep[["HASPT.algo"]] = "HorAngle"; params_sleep[["def.noc.sleep"]] = 1
      }
    }
    if (params_sleep[["HASIB.algo"]] %in% c("Sadeh1994", "Galland2012") == TRUE) {
      if (params_sleep[["Sadeh_axis"]] %in% c("X","Y","Z") == FALSE) {
        warning("\nArgument Sadeh_axis does not have meaningful value, it needs to be X, Y or Z (capital)", call. = FALSE)
      }
      if (params_sleep[["Sadeh_axis"]] == "X" & params_metrics[["do.zcx"]] == FALSE) params_metrics[["do.zcx"]] =  TRUE
      if (params_sleep[["Sadeh_axis"]] == "Y" & params_metrics[["do.zcy"]] == FALSE) params_metrics[["do.zcy"]] =  TRUE
      if (params_sleep[["Sadeh_axis"]] == "Z" & params_metrics[["do.zcz"]] == FALSE) params_metrics[["do.zcz"]] =  TRUE
    } else { # vanHees2015
      params_sleep[["Sadeh_axis"]] = "" # not used
    }
    if (length(params_sleep[["loglocation"]]) == 1) {
      if (params_sleep[["loglocation"]] == "") params_sleep[["loglocation"]] = c() #inserted because some users mistakingly use this
    }
    if (length(params_sleep[["loglocation"]]) == 0 & length(params_sleep[["def.noc.sleep"]]) != 1) {
      warning(paste0("\nloglocation was specified and def.noc.sleep does not have length of 1, this is not compatible. ",
                     " We assume you want to use the sleeplog and misunderstood",
                     " argument def.noc.sleep. Therefore, we will reset def.noc.sleep to its default value of 1"), call. = FALSE)
      params_sleep[["def.noc.sleep"]] = 1
    }
    if (params_sleep[["HASPT.algo"]] == "HorAngle" & params_sleep[["sleepwindowType"]] != "TimeInBed") {
      warning("\nHASPT.algo is set to HorAngle, therefor auto-updating sleepwindowType to TimeInBed", call. = FALSE)
      params_sleep[["sleepwindowType"]] = "TimeInBed"
    }
    if (length(params_sleep[["loglocation"]]) == 0 & params_sleep[["HASPT.algo"]] != "HorAngle" & params_sleep[["sleepwindowType"]] != "SPT") {
      warning("\nAuto-updating sleepwindowType to SPT because no sleeplog used and neither HASPT.algo HorAngle used.", call. = FALSE)
      params_sleep[["sleepwindowType"]] = "SPT"
    }
  }
  
  if (length(params_cleaning) > 0) {
    if (params_cleaning[["strategy"]] != 1 & params_cleaning[["hrs.del.start"]] != 0) {
      warning(paste0("\nSetting argument hrs.del.start in combination with strategy = ", 
                     params_cleaning[["strategy"]]," is not meaningful, because this is only used when straytegy = 1"), call. = FALSE)
    }
    if (params_cleaning[["strategy"]] != 1 & params_cleaning[["hrs.del.end"]] != 0) {
      warning(paste0("\nSetting argument hrs.del.end in combination with strategy = ",
                     params_cleaning[["strategy"]]," is not meaningful, because this is only used when straytegy = 1"), call. = FALSE)
    }
    if (params_cleaning[["strategy"]] != 3 & params_cleaning[["ndayswindow"]] != 7) {
      warning(paste0("\nSetting argument ndayswindow in combination with strategy = ", 
                     params_cleaning[["strategy"]]," is not meaningful, because this is only used when straytegy = 3"), call. = FALSE)
    }
  }
  if (length(params_phyact) > 0) {
    if (length(params_phyact[["mvpadur"]]) != 3) {
      params_phyact[["mvpadur"]] = c(1,5,10)
      warning("\nmvpadur needs to be a vector with length three, value now reset to default c(1, 5, 10)", call. = FALSE)
    }
  }
  if (length(params_247) > 0) {
    if (length(params_247[["iglevels"]]) > 0) {
      if (length(params_247[["iglevels"]]) == 1) {
        params_247[["iglevels"]] = c(seq(0, 4000, by = 25), 8000) # to introduce option to just say TRUE
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