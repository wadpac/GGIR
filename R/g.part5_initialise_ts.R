g.part5_initialise_ts = function(IMP, M, params_247, params_general, 
                                 longitudinal_axis = c(), myfun = c()) {
  # extract key variables from the mile-stone data: time, acceleration and elevation angle
  # note that this is imputed ACCELERATION because we use this for describing behaviour:
  scale = ifelse(test = grepl("^Brond|^Neishabouri|^ZC|^ExtAct|^ExtHeartRate", params_general[["acc.metric"]]), yes = 1, no = 1000)

  # Use anglez by default or longitudinal axis if specified when sensor is worn on hip
  
  if (params_general[["sensor.location"]] != "hip") {
    longitudinal_axis = NULL
  }
  if (is.null(longitudinal_axis) && "anglez" %in% names(IMP$metashort)) {
    angleName = "anglez"
  } else if (longitudinal_axis == 1 && "anglex" %in% names(IMP$metashort)) {
    angleName = "anglex"
  } else if (longitudinal_axis == 2 && "anglex" %in% names(IMP$metashort)) {
    angleName = "angley"
  } else if (longitudinal_axis == 3 && "anglex" %in% names(IMP$metashort)) {
    angleName = "anglez"
  } else {
    angleName = NULL
  }
  if (!is.null(angleName) && angleName %in% names(IMP$metashort)) {
    ts = data.frame(time = IMP$metashort[,1], ACC = IMP$metashort[,params_general[["acc.metric"]]] * scale,
                    guider = rep("unknown", nrow(IMP$metashort)),
                    angle = as.numeric(as.matrix(IMP$metashort[,which(names(IMP$metashort) == angleName)])))
    # also store other angles if available:
    for (otherAngle in c("anglex", "angley", "anglez")) {
      if (otherAngle %in% names(IMP$metashort) == TRUE && angleName != otherAngle) {
        if (otherAngle == "anglex") {
          ts$anglex = as.numeric(as.matrix(IMP$metashort[, otherAngle]))
        } else if (otherAngle == "angley") {
          ts$angley = as.numeric(as.matrix(IMP$metashort[, otherAngle]))
        } else if (otherAngle == "anglez") {
          ts$anglez = as.numeric(as.matrix(IMP$metashort[, otherAngle]))
        }
      }
    }
  } else {
    ts = data.frame(time = IMP$metashort[,1], ACC = IMP$metashort[,params_general[["acc.metric"]]] * scale,
                    guider = rep("unknown", nrow(IMP$metashort)))
  }
  if ("step_count" %in% colnames(IMP$metashort)) {
    ts$step_count = 0
    ts$step_count = IMP$metashort$step_count
  }

  if (length(myfun) > 0 &&
      "reporttype" %in% names(myfun)) {
    
    if (myfun$reporttype == "type") {
      
      # Identify "type" column and its levels
      type_col = myfun$colnames
      type_levels = unique(IMP$metashort[,type_col])
      type_levels = type_levels[!is.na(type_levels)]
      type_levels = tolower(type_levels) # to lower to being case insensitive in subsequent calculations
      
      # seconds in each row
      epoch_s = IMP$windowsizes[1]
      
      # identify the type of each epoch
      type_index = match(IMP$metashort[, type_col], type_levels)
      
      # create output matrix: 
      # each row represents seconds in epoch classified in each type_levels
      type_mat = matrix(0,
                        nrow = nrow(IMP$metashort),
                        ncol = length(type_levels))
      valid = !is.na(type_index) # safe-guard for implausible NA values
      type_mat[cbind(which(valid), type_index[valid])] = epoch_s
      
      # add columns to ts 
      type_colnames = paste0("ExtFunType_", type_col, "_", type_levels, "_s")
      colnames(type_mat) = type_colnames
      ts[type_colnames] = type_mat
    } 
  }
  
  Nts = nrow(ts)
  # add non-wear column
  nonwear = IMP$rout[,5]
  nonwear = rep(nonwear, each = (IMP$windowsizes[2]/IMP$windowsizes[1]))
  if (length(nonwear) > Nts) {
    nonwear = nonwear[1:Nts]
  } else if (length(nonwear) < Nts) {
    nonwear = c(nonwear, rep(0, (Nts - length(nonwear))))
  }
  ts$nonwear = 0 # initialise column
  ts$nonwear = nonwear
  
  # Add temperature and light, if present
  lightpeak_available = "lightpeak" %in% colnames(M$metalong)
  temperature_available = "temperaturemean" %in% colnames(M$metalong)
  repeatvalues = function(x, windowsizes, Nts) {
    x = rep(x, each = (windowsizes[2]/windowsizes[1]))
    if (length(x) > Nts) {
      x = x[1:Nts]
    } else if (length(x) < Nts) {
      x = c(x, rep(0, (Nts - length(x))))
    }
    return(x)
  }
  if (lightpeak_available == TRUE) {
    luz = M$metalong$lightpeak
    if (length(params_247[["LUX_cal_constant"]]) > 0 &
        length(params_247[["LUX_cal_exponent"]]) > 0) { # re-calibrate light
      luz = params_247[["LUX_cal_constant"]] * exp(params_247[["LUX_cal_exponent"]] * luz)
    }
    handle_luz_extremes = g.part5.handle_lux_extremes(luz)
    luz = handle_luz_extremes$lux
    correction_log = handle_luz_extremes$correction_log
    # repeate values to match resolution of other data
    luz = repeatvalues(x = luz, windowsizes = IMP$windowsizes, Nts)
    correction_log = repeatvalues(x = correction_log, windowsizes = IMP$windowsizes, Nts)
    ts$lightpeak_imputationcode = ts$lightpeak = 0 # initialise column
    ts$lightpeak = luz
    ts$lightpeak_imputationcode = correction_log
  }
  if (temperature_available == TRUE) {
    temperature = M$metalong$temperaturemean
    # repeate values to match resolution of other data
    ts$temperature = repeatvalues(x = temperature, windowsizes = IMP$windowsizes, Nts)
  }
  if ("marker" %in% colnames(M$metashort)) {
    ts$marker = NA
    ts$marker = IMP$metashort$marker
  }
  return(ts)
}

