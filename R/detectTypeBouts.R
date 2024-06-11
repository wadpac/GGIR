detectTypeBouts = function(myfun, varnum_type, varnum, 
                           UnitReScale, daysummary, ds_names,
                           di, fi, ws3, boutnameEnding) {
  if ("tbout.dur" %in% names(myfun) == FALSE) myfun$tbout.dur = c(1, 5, 10)
  if ("tbout.th.acc" %in% names(myfun) == FALSE) myfun$tbout.th.acc = 50
  if ("tbout.criter" %in% names(myfun) == FALSE) myfun$tbout.criter = 0.8
  if ("tbout.condition" %in% names(myfun) == FALSE) myfun$tbout.condition = "AND"
  
  types = levels(varnum_type)
  for (ti in 1:length(types)) {
    # Type bouts
    for (boutdur in myfun$tbout.dur) {
      boutduration = boutdur * (60/ws3) # per minute
      rr1 = matrix(0, length(varnum), 1)
      if (myfun$tbout.condition == "AND") {
        p = which(varnum * UnitReScale >= myfun$tbout.th.acc &
                    varnum_type == types[ti]); rr1[p] = 1
      } else if (myfun$tbout.condition == "OR") {
        p = which(varnum * UnitReScale >= myfun$tbout.th.acc |
                    varnum_type == types[ti]); rr1[p] = 1
      }
      getboutout = g.getbout(x = rr1, boutduration = boutduration,
                             boutcriter = myfun$tbout.criter,
                             ws3 = ws3)
      # time spent in bouts in minutes
      typebout = length(which(getboutout == 1)) / (60/ws3)
      tboutname = paste0("ExtFunType_Bout_totdur_E", ws3, "S_B", boutdur,
                         "M", (myfun$tbout.criter  * 100),
                         "%_TYPE",types[ti],"_",
                         myfun$tbout.condition,
                         "_accT", myfun$tbout.th.acc)
      tbout_varname = paste0(tboutname, "_", boutnameEnding)
      # fi = correct_fi(di, ds_names, fi, varname = tbout_varname)
      daysummary[di,fi] = typebout # total time in tbouts
      ds_names[fi] = tbout_varname;
      fi = fi + 1
      # number of bouts
      rle_bout = rle(as.numeric(getboutout))
      rle_bout1 = which(rle_bout$values == 1)
      number_of_bouts = length(rle_bout1)
      
      tboutname = paste0("ExtFunType_Bout_number_E", ws3, "S_B", boutdur,
                         "M", (myfun$tbout.criter  * 100),
                         "%_TYPE",types[ti],"_",
                         myfun$tbout.condition,
                         "_accT", myfun$tbout.th.acc)
      tbout_varname = paste0(tboutname, "_", boutnameEnding)
      daysummary[di,fi] = number_of_bouts
      ds_names[fi] = tbout_varname;
      fi = fi + 1
      # average bout duration in minutes
      if (number_of_bouts > 0) {
        mn_dur_bouts = mean(rle_bout$lengths[which(rle_bout$values == 1)]) / (60/ws3)
      } else {
        mn_dur_bouts = 0
      }
      tboutname = paste0("ExtFunType_Bout_meandur_E", ws3, "S_B", boutdur,
                         "M", (myfun$tbout.criter  * 100),
                         "%_TYPE",types[ti],"_",
                         myfun$tbout.condition,
                         "_accT", myfun$tbout.th.acc)
      tbout_varname = paste0(tboutname, "_", boutnameEnding)
      daysummary[di,fi] = mn_dur_bouts
      ds_names[fi] = tbout_varname;
      fi = fi + 1
    }
  }
  invisible(list(daysummary = daysummary, ds_names = ds_names,
                 fi = fi, di = di))
}
