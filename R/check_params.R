check_params = function(params_sleep = c(), params_metrics = c(), params_rawdata = c()) {
  check_class = function(category, params, parname, parclass) {
    if (length(params[[parname]]) > 0) {
      if (parname %in% names(params) == FALSE) {
        stop(paste0("\nParameter ", parname," is missing in object "))
      } else {
        x = params[[parname]]
      }
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
  #-----------------------------------------------------------------------------------------
  if (length(params_sleep) > 0) { # Check class of sleep parameters
    numeric_params = c("anglethreshold", "timethreshold", "longitudinal_axis")
    boolean_params = c("ignorenonwear", "constrain2range", "HASPT.ignore.invalid")
    character_params = c("HASPT.algo", "HASIB.algo", "Sadeh_axis")
    for (mi in numeric_params) {
      check_class("Sleep", params = params_sleep, parname = mi, parclass = "numeric")
    }
    for (mi in boolean_params) {
      check_class("Sleep", params = params_sleep, parname = mi, parclass = "boolean")
    }
    for (mi in character_params) {
      check_class("Sleep", params = params_sleep, parname = mi, parclass = "character")
    }
  } 
  if (length(params_metrics) > 0) { # Check class of metrics parameters
    metrics2check = c("do.anglex", "do.angley", "do.anglez",
                      "do.zcx", "do.zcy", "do.zcz",
                      "do.enmo", "do.lfenmo", "do.en", "do.mad", "do.enmoa",
                      "do.roll_med_acc_x", "do.roll_med_acc_y", "do.roll_med_acc_z", 
                      "do.dev_roll_med_acc_x", "do.dev_roll_med_acc_y", "do.dev_roll_med_acc_z", 
                      "do.bfen", "do.hfen", "do.hfenplus", "do.lfen", 
                      "do.lfx", "do.lfy", "do.lfz", "do.hfx", "do.hfy", "do.hfz",
                      "do.bfx", "do.bfy", "do.bfz")
    for (mi in metrics2check) {
      check_class("Metrics", params = params_metrics, parname = mi, parclass = "boolean")
    }
    check_class("Metrics", params = params_metrics, parname = "hb", parclass = "numeric")
    check_class("Metrics", params = params_metrics, parname = "lb", parclass = "numeric")
    check_class("Metrics", params = params_metrics, parname = "n", parclass = "numeric")
  }
  if (length(params_rawdata) > 0) { # Check class of metrics parameters
    numeric_params = c("chunksize", "spherecrit", "minloadcrit", "minimumFileSizeMB", "dynrange",
                       "rmc.col.acc", "interpolationType",
                       "rmc.firstrow.acc", "rmc.firstrow.header", "rmc.header.length",
                       "rmc.col.temp", "rmc.col.time", "rmc.bitrate", "rmc.dynamic_range",
                       "rmc.sf", "rmc.col.wear", "rmc.noise")
    boolean_params = c("printsummary", "do.cal", "rmc.unsignedbit", "rmc.check4timegaps", "rmc.doresample")
    character_params = c("backup.cal.coef", "rmc.dec", "rmc.unit.acc", 
                         "rmc.unit.temp", "rmc.unit.time", "rmc.format.time", 
                         "rmc.origin", "rmc.desiredtz", "rmc.headername.sf", 
                         "rmc.headername.sn", "rmc.headername.recordingid", 
                         "rmc.header.structure")
    for (mi in numeric_params) {
      check_class("Raw data", params = params_rawdata, parname = mi, parclass = "numeric")
    }
    for (mi in boolean_params) {
      check_class("Raw data", params = params_rawdata, parname = mi, parclass = "boolean")
    }
    for (mi in character_params) {
      check_class("Raw data", params = params_rawdata, parname = mi, parclass = "character")
    }
  }
  #-----------------------------------------------------------------------------------
  # Check value combinations and apply corrections if not logical
  if (length(params_metrics) > 0 & length(params_metrics) > 0) {
    if (length(params_sleep[["def.noc.sleep"]]) != 2) {
      params_sleep[["HASPT.algo"]] = "HDCZA"
    } else if (length(params_sleep[["def.noc.sleep"]]) == 2) {
      params_sleep[["HASPT.algo"]] = "notused"
    }
    if (params_sleep[["sensor.location"]] == "hip" &  params_sleep[["HASPT.algo"]] != "notused") {
      if (params_metrics[["do.anglex"]] == FALSE | params_metrics[["do.angley"]] == FALSE | params_metrics[["do.anglez"]] == FALSE) {
        warning(paste0("\nWhen working with hip data all three angle metrics are needed,",
                       "so GGIR now auto-sets arguments do.anglex, do.angley, and do.anglez to TRUE."))
        params_metrics[["do.anglex"]] = params_metrics[["do.angley"]] = params_metrics[["do.anglez"]] = TRUE
      }
      if (params_sleep[["HASPT.algo"]] != "HorAngle") {
        warning("\nChanging HASPT.algo to HorAngle, because sensor.location is set as hip")
        params_sleep[["HASPT.algo"]] = "HorAngle"; params_sleep[["def.noc.sleep"]] = 1
      }
    }
    if (params_sleep[["HASIB.algo"]] %in% c("Sadeh1994", "Galland2012") == TRUE) {
      if (params_sleep[["Sadeh_axis"]] %in% c("X","Y","Z") == FALSE) {
        warning("\nArgument Sadeh_axis does not have meaningful value, it needs to be X, Y or Z (capital)")
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
                     " argument def.noc.sleep. Therefore, we will reset def.noc.sleep to its default value of 1"))
      params_sleep[["def.noc.sleep"]] = 1
    }
    if (params_sleep[["HASPT.algo"]] == "HorAngle" & params_sleep[["sleepwindowType"]] != "TimeInBed") {
      warning("\nHASPT.algo is set to HorAngle, therefor auto-updating sleepwindowType to TimeInBed")
      params_sleep[["sleepwindowType"]] = "TimeInBed"
    }
    if (length(params_sleep[["loglocation"]]) == 0 & params_sleep[["HASPT.algo"]] != "HorAngle" & params_sleep[["sleepwindowType"]] != "SPT") {
      warning("\nAuto-updating sleepwindowType to SPT because no sleeplog used and neither HASPT.algo HorAngle used.")
      params_sleep[["sleepwindowType"]] = "SPT"
    }
  }
  invisible(list(params_sleep = params_sleep, 
                 params_metrics = params_metrics, 
                 params_rawdata = params_rawdata))
}