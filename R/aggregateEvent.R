aggregateEvent = function(metric_name, epochsize,
                          daysummary,  ds_names, fi, di,
                          vari, segmentInfo, myfun = NULL, params_247) {
  anwi_nameindices = segmentInfo$anwi_nameindices
  anwi_index = segmentInfo$anwi_index
  anwi_t0 = segmentInfo$anwi_t0
  anwi_t1 = segmentInfo$anwi_t1
  if ("ilevels" %in% names(myfun) == FALSE) myfun$ilevels = c(0, 80)
  if ("clevels" %in% names(myfun) == FALSE) myfun$clevels = c(0, 30)
  if ("qlevels" %in% names(myfun) == FALSE) myfun$qlevels = c(0.25, 0.5, 0.75)
  if (length(myfun$ilevels) == 0) myfun$ilevels = 0
  if (length(myfun$clevels) == 0) myfun$clevels = 0
  if (length(myfun$qlevels) == 0) myfun$qlevels = 0.5
  acc.thresholds = myfun$ilevels
  cadence.thresholds = myfun$clevels
  qlevels = myfun$qlevels
  fi_start = fi
  cadence = vari[, metric_name] * (60/epochsize) # assumption now that cadence is also relevant if the event detected is not steps
  # aggregate per window total
  varnameevent = paste0("ExtFunEvent_tot_", metric_name, anwi_nameindices[anwi_index])
  daysummary[di, fi] = sum(vari[, metric_name])
  ds_names[fi] = varnameevent; fi = fi + 1
  
  # cadence
  varnameevent = paste0("ExtFunEvent_mn_cad", anwi_nameindices[anwi_index])
  daysummary[di, fi] = mean(cadence, na.rm = TRUE)
  ds_names[fi] = varnameevent; fi = fi + 1
  
  # cadence percentiles
  cadence_quantiles = quantile(cadence, probs = qlevels)
  varnameevent = paste0("ExtFunEvent_cad_p", qlevels * 100, anwi_nameindices[anwi_index])
  daysummary[di, fi:(fi + length(qlevels) - 1)] = as.numeric(cadence_quantiles)
  ds_names[fi:(fi + length(qlevels) - 1)] = varnameevent; fi = fi + length(qlevels)
  
  #========================================
  # per acceleration level
  cn_vari = colnames(vari)
  acc.metrics = cn_vari[cn_vari %in% c("timestamp", "anglex", "angley", "anglez", metric_name) == FALSE]
  
  for (ami in 1:length(acc.metrics)) {
    vari[, acc.metrics[ami]] = as.numeric(vari[, acc.metrics[ami]])
    for (ti in 1:length(acc.thresholds)) {
      # step_count per acceleration level
      if (ti < length(acc.thresholds)) {
        acc_level_name = paste0(acc.thresholds[ti], "-", acc.thresholds[ti + 1], "mg",  "_",  acc.metrics[ami])
        whereAccLevel = which(vari[, acc.metrics[ami]] >= (acc.thresholds[ti]/1000) &
                                vari[, acc.metrics[ami]] < (acc.thresholds[ti + 1]/1000))
      } else {
        acc_level_name = paste0("atleast", acc.thresholds[ti], "mg",  "_",  acc.metrics[ami])
        whereAccLevel = which(vari[,acc.metrics[ami]] >= (acc.thresholds[ti]/1000))
      }
      varnameevent = paste0("ExtFunEvent_tot_", metric_name, "_acc", acc_level_name, anwi_nameindices[anwi_index])
      if (length(whereAccLevel) > 0)  {
        daysummary[di, fi] = sum(vari[whereAccLevel, metric_name], na.rm = TRUE)
      } else {
        daysummary[di, fi] = 0
      }
      ds_names[fi] = varnameevent; fi = fi + 1
      
      # cadence per acceleration level
      varnameevent = paste0("ExtFunEvent_mn_cad_acc", 
                            acc_level_name, anwi_nameindices[anwi_index])
      if (length(whereAccLevel) > 0)  {
        daysummary[di, fi] = mean(cadence[whereAccLevel], na.rm = TRUE)
      } else {
        daysummary[di, fi] = 0
      }
      ds_names[fi] = varnameevent; fi = fi + 1
    }
  }
  
  #========================================
  # per cadence level
  for (ti in 1:length(cadence.thresholds)) {
    # define cadence level
    if (ti < length(cadence.thresholds)) {
      cadence_level_name = paste0(cadence.thresholds[ti], "-", cadence.thresholds[ti + 1], "spm")
      whereCadenceLevel = which(cadence >= cadence.thresholds[ti] &
                                  cadence < cadence.thresholds[ti + 1])
    } else {
      cadence_level_name = paste0("atleast", cadence.thresholds[ti], "spm")
      whereCadenceLevel = which(cadence >= cadence.thresholds[ti])
    }
    # cadence per cadence level
    varnameevent = paste0("ExtFunEvent_mn_cad_cad", cadence_level_name, anwi_nameindices[anwi_index])
    if (length(whereCadenceLevel) > 0)  {
      daysummary[di, fi] = mean(cadence[whereCadenceLevel], na.rm = TRUE)
    } else {
      daysummary[di, fi] = 0
    }
    ds_names[fi] = varnameevent; fi = fi + 1
    
    # step count per cadence level
    varnameevent = paste0("ExtFunEvent_tot_", metric_name, "_cad", 
                          cadence_level_name, anwi_nameindices[anwi_index])
    if (length(whereCadenceLevel) > 0)  {
      daysummary[di, fi] = sum(vari[whereCadenceLevel, metric_name], na.rm = TRUE)
    } else {
      daysummary[di, fi] = 0
    }
    ds_names[fi] = varnameevent; fi = fi + 1
    
    # time per cadence level
    varnameevent = paste0("ExtFunEvent_dur_cad", cadence_level_name, anwi_nameindices[anwi_index])
    daysummary[di, fi] = length(whereCadenceLevel) / (60/epochsize)
    ds_names[fi] = varnameevent; fi = fi + 1
    
    for (ami in 1:length(acc.metrics)) {
      # acceleration per cadence level
      varnameevent = paste0("ExtFunEvent_mn_",  acc.metrics[ami],"_cad",
                            cadence_level_name, anwi_nameindices[anwi_index])
      if (length(whereCadenceLevel) > 0)  {
        daysummary[di,fi] = mean(vari[whereCadenceLevel, acc.metrics[ami]], na.rm = TRUE) * 1000
      } else {
        daysummary[di, fi] = 0
      }
      ds_names[fi] = varnameevent; fi = fi + 1
    }
  }
  if (length(params_247[["winhr"]]) > 0) { 
    # Event based MXLX
    tseg = c((anwi_t0[anwi_index] - 1) / (3600 / epochsize), anwi_t1[anwi_index] / (3600 / epochsize))
    for (winhr_value in params_247[["winhr"]]) {
      MXLXout = MXLX(Y = cadence, X = winhr_value, epochSize = epochsize,
                     tseg = tseg,
                     resolutionMin = params_247[["M5L5res"]])
      # Describe for MX and LX:
      LXMXwindow_name = anwi_nameindices[anwi_index]
      for (WX in c("L", "M")) {
        if (!all(is.na(MXLXout[, grep("M", names(MXLXout))]))) {
          i0 = MXLXout[, grep(paste0("start_", WX, winhr_value), names(MXLXout))]
          i1 = MXLXout[, grep(paste0("end_", WX, winhr_value), names(MXLXout))]
          WXi = i0:i1
          # # Code used to help check that MXLX makes sense with real study data
          # x11()
          # plot(cadence, type = "l")
          # lines(WXi, cadence[WXi], type = "l", col = "red")
          
          # Timing
          daysummary[di, fi] = MXLXout[, which(names(MXLXout) == paste0(WX, winhr_value, "hr"))]
          ds_names[fi] = paste0("ExtFunEvent_", WX, winhr_value,  "hr_cad", LXMXwindow_name); fi = fi + 1
          # cadence in the form of mean, percentiles.
          daysummary[di, fi] = mean(cadence[WXi])
          ds_names[fi] = paste0("ExtFunEvent_", WX, winhr_value, "_cad_meancad", LXMXwindow_name); fi = fi + 1
          if (length(params_247[["qM5L5"]]) > 0) {
            for (qle in params_247[["qM5L5"]]) {
              daysummary[di, fi] = quantile(x = cadence[WXi], probs = qle)
              ds_names[fi] = paste0("ExtFunEvent_", WX, winhr_value, "_cad_q", qle*100, "cad", LXMXwindow_name); fi = fi + 1
            }
          }
          for (ami in 1:length(acc.metrics)) {
            # Acceleration in the form of mean, percentiles.
            daysummary[di, fi] = mean(vari[WXi, acc.metrics[ami]]) * 1000
            ds_names[fi] = paste0("ExtFunEvent_", WX, winhr_value, "_cad_mean_",  acc.metrics[ami], "_mg", LXMXwindow_name); fi = fi + 1
            if (length(params_247[["qM5L5"]]) > 0) {
              for (qle in params_247[["qM5L5"]]) {
                daysummary[di, fi] = quantile(x = vari[WXi, acc.metrics[ami]], probs = qle)
                ds_names[fi] = paste0("ExtFunEvent_", WX, winhr_value, "_cad_q", qle*100, "acc", LXMXwindow_name); fi = fi + 1
              }
            }
          }
        }
      }
    }
  }
  invisible(list(ds_names = ds_names, daysummary = daysummary, fi = fi))
}