aggregateType = function(metric_name, epochsize,
                         daysummary,  ds_names, fi, di,
                         vari, segmentInfo, myfun = NULL, qcheck) {
  anwi_nameindices = segmentInfo$anwi_nameindices
  anwi_index = segmentInfo$anwi_index
  if ("ilevels" %in% names(myfun) == FALSE) myfun$ilevels = c(0, 80)
  if (length(myfun$ilevels) == 0) myfun$ilevels = 0
  acc.thresholds = myfun$ilevels
  # type values and levels available
  type_values = vari[, metric_name]
  type_levels = levels(type_values)
  
  # Detect: 
  #   invalid (GGIR invalid epochs)
  #   unclassified (valid epochs without a classification -external function returns NA-)
  #   valid
  invalid_idx = qcheck != 0 & qcheck != -1
  unclassified_idx = is.na(type_values) & !invalid_idx
  valid_idx = !invalid_idx & !is.na(type_values)
  
  # Exclude invalid from classified epochs
  type_values[invalid_idx] = NA
  
  #========================================
  # aggregate per window total
  type_table = type_table = table(factor(type_values[valid_idx], levels = type_levels))
  varnametype = paste0("ExtFunType_tot_", metric_name, "_", type_levels, anwi_nameindices[anwi_index])
  fi2 = fi + length(varnametype) - 1
  daysummary[di, fi:fi2] = type_table * epochsize / 60
  ds_names[fi:fi2] = varnametype
  fi = fi2 + 1
  
  #========================================
  # Unclassified and invalid time
  daysummary[di, fi] = sum(unclassified_idx) * epochsize / 60
  ds_names[fi] = paste0(
    "ExtFunType_tot_", metric_name,
    "_unclassified", anwi_nameindices[anwi_index]
  )
  fi = fi + 1
  
  daysummary[di, fi] = sum(invalid_idx) * epochsize / 60
  ds_names[fi] = paste0(
    "ExtFunType_tot_", metric_name,
    "_invalid", anwi_nameindices[anwi_index]
  )
  fi = fi + 1
  
  #========================================
  # per acceleration level (acc metric definition copied from g.analyse.perday)
  cn_vari = colnames(vari)
  acc.metrics = cn_vari[cn_vari %in% c("ENMO","LFENMO", "BFEN", "EN", "HFEN", "HFENplus", "MAD", "ENMOa",
                                       "ZCX", "ZCY", "ZCZ", "BrondCount_x", "BrondCount_y",
                                       "BrondCount_z", "NeishabouriCount_x", "NeishabouriCount_y",
                                       "NeishabouriCount_z", "NeishabouriCount_vm", "ExtAct",
                                       "ExtHeartRate")]
  
  for (ami in 1:length(acc.metrics)) {
    vari[, acc.metrics[ami]] = as.numeric(vari[, acc.metrics[ami]])
    for (ti in 1:length(acc.thresholds)) {
      # activity types per acceleration level
      if (ti < length(acc.thresholds)) {
        acc_level_name = paste0(acc.thresholds[ti], "-", acc.thresholds[ti + 1], "mg",  "_",  acc.metrics[ami])
        whereAccLevel = which(!is.na(type_values) &
                                vari[, acc.metrics[ami]] >= (acc.thresholds[ti]/1000) &
                                vari[, acc.metrics[ami]] < (acc.thresholds[ti + 1]/1000))
      } else {
        acc_level_name = paste0("atleast", acc.thresholds[ti], "mg",  "_",  acc.metrics[ami])
        whereAccLevel = which(!is.na(type_values) &
                                vari[,acc.metrics[ami]] >= (acc.thresholds[ti]/1000))
      }
      varnametype = paste0("ExtFunType_tot_", metric_name, "_", type_levels, "_acc", acc_level_name, anwi_nameindices[anwi_index])
      fi2 = fi + length(varnametype) - 1
      if (length(whereAccLevel) > 0)  {
        type_table = table(factor(type_values[whereAccLevel], levels = type_levels))
        daysummary[di, fi:fi2] = type_table * epochsize / 60
      } else {
        daysummary[di, fi:fi2] = 0
      }
      ds_names[fi:fi2] = varnametype
      fi = fi2 + 1
    }
  }
  
  #========================================
  # Type bouts

  typeBouts = detectTypeBouts(
    myfun = myfun,
    varnum_type = type_values,
    daysummary = daysummary,
    ds_names = ds_names,
    di = di,
    fi = fi,
    ws3 = epochsize,
    boutnameEnding = anwi_nameindices[anwi_index]
  )
  
  daysummary = typeBouts$daysummary
  ds_names = typeBouts$ds_names
  fi = typeBouts$fi
  
  invisible(list(ds_names = ds_names, daysummary = daysummary, fi = fi))
}
