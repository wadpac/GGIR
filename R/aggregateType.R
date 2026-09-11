aggregateType = function(metric_name, epochsize,
                         daysummary,  ds_names, fi, di,
                         vari, segmentInfo, myfun = NULL) {
  anwi_nameindices = segmentInfo$anwi_nameindices
  anwi_index = segmentInfo$anwi_index
  if ("ilevels" %in% names(myfun) == FALSE) myfun$ilevels = c(0, 80)
  if (length(myfun$ilevels) == 0) myfun$ilevels = 0
  acc.thresholds = myfun$ilevels
  # make type levels case insensitive (convert to lower)
  type_values = tolower(as.character(vari[, metric_name]))
  type_levels = unique(type_values)
  type_levels = type_levels[!is.na(type_levels)]
  #========================================
  # aggregate per window total
  varnametype = paste0("ExtFunType_tot_", metric_name, "_", type_levels, anwi_nameindices[anwi_index])
  fi2 = fi + length(varnametype) - 1
  type_table = table(factor(type_values, levels = type_levels))
  daysummary[di, fi:fi2] = type_table * epochsize / 60
  ds_names[fi:fi2] = varnametype; fi = fi2 + 1
  
  #========================================
  # per acceleration level
  cn_vari = colnames(vari)
  acc.metrics = cn_vari[cn_vari %in% c("timestamp", "anglex", "angley", "anglez", metric_name) == FALSE]
  
  for (ami in 1:length(acc.metrics)) {
    vari[, acc.metrics[ami]] = as.numeric(vari[, acc.metrics[ami]])
    for (ti in 1:length(acc.thresholds)) {
      # activity types per acceleration level
      if (ti < length(acc.thresholds)) {
        acc_level_name = paste0(acc.thresholds[ti], "-", acc.thresholds[ti + 1], "mg",  "_",  acc.metrics[ami])
        whereAccLevel = which(vari[, acc.metrics[ami]] >= (acc.thresholds[ti]/1000) &
                                vari[, acc.metrics[ami]] < (acc.thresholds[ti + 1]/1000))
      } else {
        acc_level_name = paste0("atleast", acc.thresholds[ti], "mg",  "_",  acc.metrics[ami])
        whereAccLevel = which(vari[,acc.metrics[ami]] >= (acc.thresholds[ti]/1000))
      }
      varnametype = paste0("ExtFunType_tot_", metric_name, "_", type_levels, "_acc", acc_level_name, anwi_nameindices[anwi_index])
      fi2 = fi + length(varnametype) - 1
      if (length(whereAccLevel) > 0)  {
        type_table = table(factor(type_values[whereAccLevel], levels = type_levels))
        daysummary[di, fi:fi2] = type_table * epochsize / 60
      } else {
        daysummary[di, fi:fi2] = 0
      }
      ds_names[fi:fi2] = varnametype; fi = fi2 + 1
    }
  }
  invisible(list(ds_names = ds_names, daysummary = daysummary, fi = fi))
}
