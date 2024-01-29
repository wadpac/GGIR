g.part5_initialise_ts = function(IMP, M, params_247, params_general, longitudinal_axis = c()) {
  # extract key variables from the mile-stone data: time, acceleration and elevation angle
  # note that this is imputed ACCELERATION because we use this for describing behaviour:
  scale = ifelse(test = grepl("^Brond|^Neishabouri|^ZC|^ExtAct", params_general[["acc.metric"]]), yes = 1, no = 1000)

  # Use anglez by default or longitudinal axis if specified when sensor is worn on hip
  if (is.null(longitudinal_axis)) {
    angleName = "anglez"
  } else if (longitudinal_axis == 1) {
    angleName = "anglex"
  } else if (longitudinal_axis == 2) {
    angleName = "angley"
  } else if (longitudinal_axis == 3) {
    angleName = "anglez"
  }
  if (angleName %in% names(IMP$metashort)) {
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
  lightpeak_available = "lightpeak" %in% colnames(M$metalong)
  # Check if temperature and light are availble
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
    repeatvalues = function(x, windowsizes, Nts) {
      x = rep(x, each = (windowsizes[2]/windowsizes[1]))
      if (length(x) > Nts) {
        x = x[1:Nts]
      } else if (length(x) < Nts) {
        x = c(x, rep(0, (Nts - length(x))))
      }
      return(x)
    }
    luz = repeatvalues(x = luz, windowsizes = IMP$windowsizes, Nts)
    correction_log = repeatvalues(x = correction_log, windowsizes = IMP$windowsizes, Nts)
    ts$lightpeak_imputationcode = ts$lightpeak = 0 # initialise column
    ts$lightpeak = luz
    ts$lightpeak_imputationcode = correction_log
  }
  return(ts)
}

