detectEventBouts = function(myfun, varnum_event, varnum, 
                            UnitReScale, daysummary, ds_names,
                            di, fi, ws3, boutnameEnding) {
  if ("ebout.dur" %in% names(myfun) == FALSE) myfun$ebout.dur = c(1, 5, 10)
  if ("ebout.th.cad" %in% names(myfun) == FALSE) myfun$ebout.th.cad = 30
  if ("ebout.th.acc" %in% names(myfun) == FALSE) myfun$ebout.th.acc = 50
  if ("ebout.criter" %in% names(myfun) == FALSE) myfun$ebout.criter = 0.8
  if ("ebout.condition" %in% names(myfun) == FALSE) myfun$ebout.condition = "AND"
  
  # varnum = metashort[anwindices, mi]
  cadence = varnum_event * (60/ws3)
  # Event bouts
  # Loop over bout durations
  for (boutdur in myfun$ebout.dur) {
    boutduration = boutdur * (60/ws3) # per minute
    
    # Loop over cadence thresholds
    for (cad_th in myfun$ebout.th.cad) {
      
      # Loop over acceleration thresholds
      for (acc_th in myfun$ebout.th.acc) {
        rr1 = matrix(0, length(varnum), 1)
        if (myfun$ebout.condition == "AND") {
          p = which(varnum * UnitReScale >= acc_th &
                      cadence >= cad_th); rr1[p] = 1
        } else if (myfun$ebout.condition == "OR") {
          p = which(varnum * UnitReScale >= acc_th |
                      cadence >= cad_th); rr1[p] = 1
        }
        getboutout = g.getbout(x = rr1, boutduration = boutduration,
                               boutcriter = myfun$ebout.criter,
                               ws3 = ws3)
        # time spent in bouts in minutes
        eventbout = length(which(getboutout == 1)) / (60/ws3)
        eboutname = paste0("ExtFunEvent_totdur_B", boutdur,
                           "M", (myfun$ebout.criter  * 100),
                           "%_cad",cad_th, myfun$ebout.condition,
                           "acc", acc_th)
        ebout_varname = paste0(eboutname, "_", boutnameEnding)
        # fi = correct_fi(di, ds_names, fi, varname = ebout_varname)
        daysummary[di,fi] = eventbout # total time in ebouts
        ds_names[fi] = ebout_varname;
        fi = fi + 1
        # number of bouts
        rle_bout = rle(as.numeric(getboutout))
        rle_bout1 = which(rle_bout$values == 1)
        number_of_bouts = length(rle_bout1)
        
        eboutname = paste0("ExtFunEvent_number_B", boutdur,
                           "M", (myfun$ebout.criter  * 100),
                           "%_cad",cad_th, myfun$ebout.condition,
                           "acc", acc_th)
        ebout_varname = paste0(eboutname, "_", boutnameEnding)
        daysummary[di,fi] = number_of_bouts
        ds_names[fi] = ebout_varname;
        fi = fi + 1
        # average bout duration in minutes
        if (number_of_bouts > 0) {
          mn_dur_bouts = mean(rle_bout$lengths[which(rle_bout$values == 1)]) / (60/ws3)
        } else {
          mn_dur_bouts = 0
        }
        eboutname = paste0("ExtFunEvent_meandur_B", boutdur,
                           "M", (myfun$ebout.criter  * 100),
                           "%_cad",cad_th, myfun$ebout.condition,
                           "acc", acc_th)
        ebout_varname = paste0(eboutname, "_", boutnameEnding)
        daysummary[di,fi] = mn_dur_bouts
        ds_names[fi] = ebout_varname;
        fi = fi + 1
      }
    }
  }
  invisible(list(daysummary = daysummary, ds_names = ds_names,
                 fi = fi, di = di))
}