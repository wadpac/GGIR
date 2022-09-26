aggregateEvent = function(metric_name, varnum,
                          epochsize, anwi_nameindices,
                          anwi_index, ds_names,
                          fi, di, daysummary,
                          acc.thresholds, metashort, 
                          anwindices, cadence.thresholds = c(0, 30, 60)) {
  
  if (metric_name == "step_count") {
    cadence = varnum * (60/epochsize)
  }
  # aggregate per window total
  varnameevent = paste0("total_", metric_name, anwi_nameindices[anwi_index])
  # fi = correct_fi(di, ds_names, fi, varname = varnameevent)
  daysummary[di,fi] = sum(varnum)
  ds_names[fi] = varnameevent; fi = fi + 1
  
  if (metric_name == "step_count") {
    # cadence
    varnameevent = paste0("mean_cadence", anwi_nameindices[anwi_index])
    daysummary[di,fi] = mean(cadence, na.rm = TRUE)
    ds_names[fi] = varnameevent; fi = fi + 1
  }
  
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
        acc_level_name = paste0("atleast_", acc.thresholds[ti], "mg",  "_",  acc.metrics[ami])
        whereAccLevel = which(metashort[anwindices,acc.metrics[ami]] >= (acc.thresholds[ti]/1000))
      }
      varnameevent = paste0("total_", metric_name, "_", acc_level_name, anwi_nameindices[anwi_index])
      if (length(whereAccLevel) > 0)  {
        daysummary[di,fi] = sum(varnum[whereAccLevel], na.rm = TRUE)
      } else {
        daysummary[di,fi] = 0
      }
      ds_names[fi] = varnameevent; fi = fi + 1
      
      if (metric_name == "step_count") {
        # cadence per acceleration level
        varnameevent = paste0("mean_cadence_", 
                              acc_level_name, anwi_nameindices[anwi_index])
        if (length(whereAccLevel) > 0)  {
          daysummary[di,fi] = mean(cadence[whereAccLevel], na.rm = TRUE)
        } else {
          daysummary[di,fi] = 0
        }
        ds_names[fi] = varnameevent; fi = fi + 1
      }
    }
  }
  if (metric_name == "step_count") {
    varnamescalar = paste0("mean_cadence", anwi_nameindices[anwi_index])
    daysummary[di,fi] = mean(cadence)
    ds_names[fi] = varnamescalar; fi = fi + 1
    #========================================
    # per cadence level
    acc.metrics = cn_metashort[cn_metashort %in% c("timestamp","anglex","angley","anglez", metric_name) == FALSE]
    # cadence.thresholds = c(0, 30, 60) # hard-coded make this a user input arguments myfun
    for (ti in 1:length(cadence.thresholds)) {
      # define cadence level
      if (ti < length(cadence.thresholds)) {
        cadence_level_name = paste0(cadence.thresholds[ti], "-", cadence.thresholds[ti + 1], "steppm")
        whereCadenceLevel = which(cadence >= (cadence.thresholds[ti]/1000) &
                                    cadence < (cadence.thresholds[ti + 1]/1000))
      } else {
        cadence_level_name = paste0("atleast_", cadence.thresholds[ti], "steppm")
        whereCadenceLevel = which(cadence >= (cadence.thresholds[ti]/1000))
      }
      # cadence per cadence level
      varnameevent = paste0("mean_cadence_", cadence_level_name, anwi_nameindices[anwi_index])
      if (length(whereCadenceLevel) > 0)  {
        daysummary[di,fi] = mean(cadence[whereCadenceLevel], na.rm = TRUE)
      } else {
        daysummary[di,fi] = 0
      }
      ds_names[fi] = varnameevent; fi = fi + 1
      
      # step count per cadence level
      varnameevent = paste0("total_", metric_name, "_", 
                            cadence_level_name, anwi_nameindices[anwi_index])
      if (length(whereCadenceLevel) > 0)  {
        daysummary[di,fi] = sum(varnum[whereCadenceLevel], na.rm = TRUE)
      } else {
        daysummary[di,fi] = 0
      }
      ds_names[fi] = varnameevent; fi = fi + 1
      
      # time per cadence level
      varnameevent = paste0("dur_", cadence_level_name, anwi_nameindices[anwi_index])
      daysummary[di,fi] = length(whereCadenceLevel) / (60/epochsize)
      ds_names[fi] = varnameevent; fi = fi + 1
      
      for (ami in 1:length(acc.metrics)) {
        # acceleration per cadence level
        varnameevent = paste0("mean_",  acc.metrics[ami],"_",
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
  }
  invisible(list(ds_names = ds_names, daysummary = daysummary, fi = fi))
}