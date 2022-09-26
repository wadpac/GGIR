aggregateEvent = function(metric_name, varnum,
                          epochsize, anwi_nameindices,
                          anwi_index, ds_names,
                          fi, di, daysummary,
                          metashort, anwindices, myfun) {
  acc.thresholds = myfun$ilevels
  cadence.thresholds = myfun$clevels
  qlevels = myfun$qlevels
  
  fi_start = fi
  
  cadence = varnum * (60/epochsize) # assumption now that cadence is also relevant if the event detected is not steps
  # aggregate per window total
  varnameevent = paste0("tot_", metric_name, anwi_nameindices[anwi_index])
  # fi = correct_fi(di, ds_names, fi, varname = varnameevent)
  daysummary[di,fi] = sum(varnum)
  ds_names[fi] = varnameevent; fi = fi + 1
  
  # cadence
  varnameevent = paste0("mn_cad", anwi_nameindices[anwi_index])
  daysummary[di,fi] = mean(cadence, na.rm = TRUE)
  ds_names[fi] = varnameevent; fi = fi + 1
  
  # cadence precentiles
  cadence_quantiles = quantile(cadence, probs = qlevels)
  varnameevent = paste0("cad_p", qlevels * 100, anwi_nameindices[anwi_index])
  daysummary[di, fi:(fi + length(qlevels) - 1)] = as.numeric(cadence_quantiles)
  ds_names[fi:(fi + length(qlevels) - 1)] = varnameevent; fi = fi + length(qlevels)
  
  #========================================
  # per acceleration level
  cn_metashort = colnames(metashort)
  acc.metrics = cn_metashort[cn_metashort %in% c("timestamp","anglex","angley","anglez", metric_name) == FALSE]
  # acc.thresholds = c(0, 50, 100) # hard-coded make this a user input arguments myfun
  for (ami in 1:length(acc.metrics)) {
    for (ti in 1:length(acc.thresholds)) {
      # step_count per acceleration level
      if (ti < length(acc.thresholds)) {
        acc_level_name = paste0(acc.thresholds[ti], "-", acc.thresholds[ti + 1], "mg",  "_",  acc.metrics[ami])
        whereAccLevel = which(metashort[anwindices, acc.metrics[ami]] >= (acc.thresholds[ti]/1000) &
                                metashort[anwindices, acc.metrics[ami]] < (acc.thresholds[ti + 1]/1000))
      } else {
        acc_level_name = paste0("atleast", acc.thresholds[ti], "mg",  "_",  acc.metrics[ami])
        whereAccLevel = which(metashort[anwindices,acc.metrics[ami]] >= (acc.thresholds[ti]/1000))
      }
      varnameevent = paste0("tot_", metric_name, "_acc", acc_level_name, anwi_nameindices[anwi_index])
      if (length(whereAccLevel) > 0)  {
        daysummary[di,fi] = sum(varnum[whereAccLevel], na.rm = TRUE)
      } else {
        daysummary[di,fi] = 0
      }
      ds_names[fi] = varnameevent; fi = fi + 1
      
      # cadence per acceleration level
      varnameevent = paste0("mn_cad_acc", 
                            acc_level_name, anwi_nameindices[anwi_index])
      if (length(whereAccLevel) > 0)  {
        daysummary[di,fi] = mean(cadence[whereAccLevel], na.rm = TRUE)
      } else {
        daysummary[di,fi] = 0
      }
      ds_names[fi] = varnameevent; fi = fi + 1
    }
  }
  
  #========================================
  # per cadence level
  acc.metrics = cn_metashort[cn_metashort %in% c("timestamp","anglex","angley","anglez", metric_name) == FALSE]
  # cadence.thresholds = c(0, 30, 60) # hard-coded make this a user input arguments myfun
  for (ti in 1:length(cadence.thresholds)) {
    # define cadence level
    if (ti < length(cadence.thresholds)) {
      cadence_level_name = paste0(cadence.thresholds[ti], "-", cadence.thresholds[ti + 1], "spm")
      whereCadenceLevel = which(cadence >= (cadence.thresholds[ti]/1000) &
                                  cadence < (cadence.thresholds[ti + 1]/1000))
    } else {
      cadence_level_name = paste0("atleast", cadence.thresholds[ti], "spm")
      whereCadenceLevel = which(cadence >= (cadence.thresholds[ti]/1000))
    }
    # cadence per cadence level
    varnameevent = paste0("mn_cad_cad", cadence_level_name, anwi_nameindices[anwi_index])
    if (length(whereCadenceLevel) > 0)  {
      daysummary[di,fi] = mean(cadence[whereCadenceLevel], na.rm = TRUE)
    } else {
      daysummary[di,fi] = 0
    }
    ds_names[fi] = varnameevent; fi = fi + 1
    
    # step count per cadence level
    varnameevent = paste0("tot_", metric_name, "_cad", 
                          cadence_level_name, anwi_nameindices[anwi_index])
    if (length(whereCadenceLevel) > 0)  {
      daysummary[di,fi] = sum(varnum[whereCadenceLevel], na.rm = TRUE)
    } else {
      daysummary[di,fi] = 0
    }
    ds_names[fi] = varnameevent; fi = fi + 1
    
    # time per cadence level
    varnameevent = paste0("dur_cad", cadence_level_name, anwi_nameindices[anwi_index])
    daysummary[di,fi] = length(whereCadenceLevel) / (60/epochsize)
    ds_names[fi] = varnameevent; fi = fi + 1
    
    for (ami in 1:length(acc.metrics)) {
      # acceleration per cadence level
      varnameevent = paste0("mn_",  acc.metrics[ami],"_cad",
                            cadence_level_name, anwi_nameindices[anwi_index])
      if (length(whereCadenceLevel) > 0)  {
        daysummary[di,fi] = mean(metashort[anwindices[whereCadenceLevel], acc.metrics[ami]], na.rm = TRUE) * 1000
      } else {
        daysummary[di,fi] = 0
      }
      # browser()
      ds_names[fi] = varnameevent; fi = fi + 1
    }
    
  }
  # order variablenames
  CoI = fi_start:(fi - 1) # columns of interest
  neworder = sort(ds_names[CoI])
  daysummary[, CoI] = daysummary[,CoI[match(x = neworder, table = ds_names)]]
  ds_names = neworder
  invisible(list(ds_names = ds_names, daysummary = daysummary, fi = fi))
}