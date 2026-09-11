detectTypeBouts = function(myfun, varnum_type, varnum, 
                           UnitReScale, daysummary, ds_names,
                           di, fi, ws3, boutnameEnding) {
  if ("tbout.dur" %in% names(myfun) == FALSE) myfun$tbout.dur = c(1, 5, 10)
  if ("tbout.criter" %in% names(myfun) == FALSE) myfun$tbout.criter = 0.8
  
  # Type bouts
  classes = names(table(varnum_type))
  valid_classes = gsub("invalid_", "", classes)
  valid_classes = grep("invalid", classes, value = T, invert = T)
  
  # Loop over type classes
  for (class in valid_classes) {
    
    # Loop over bout durations
    for (boutdur in myfun$tbout.dur) {
      boutduration = boutdur * (60/ws3) # per minute
      
      # 1. Create binary
      rr1 = matrix(0, length(varnum), 1)
      p = which(varnum_type == class)
      rr1[p] = 1
      
      # 2. Run bout detection
      getboutout = g.getbout(x = rr1, boutduration = boutduration,
                             boutcriter = myfun$tbout.criter,
                             ws3 = ws3)
      
      # 3. Calculate Total Duration
      typebout = length(which(getboutout == 1)) / (60/ws3)
      tboutname = paste0("ExtFunType_totdur_B", boutdur,
                         "M", (myfun$tbout.criter  * 100),
                         "%_", class)
      tbout_varname = paste0(tboutname, "_", boutnameEnding)
      
      daysummary[di, fi] = typebout # total time in bouts
      ds_names[fi] = tbout_varname
      fi = fi + 1
      
      # 4. Calculate Number of Bouts
      rle_bout = rle(as.numeric(getboutout))
      rle_bout1 = which(rle_bout$values == 1)
      number_of_bouts = length(rle_bout1)
      
      tboutname = paste0("ExtFunType_number_B", boutdur,
                         "M", (myfun$tbout.criter  * 100),
                         "%_", class)
      tbout_varname = paste0(tboutname, "_", boutnameEnding)
      
      daysummary[di, fi] = number_of_bouts
      ds_names[fi] = tbout_varname
      fi = fi + 1
      
      # 5. Calculate Mean Bout Duration
      if (number_of_bouts > 0) {
        mn_dur_bouts = mean(rle_bout$lengths[which(rle_bout$values == 1)]) / (60/ws3)
      } else {
        mn_dur_bouts = 0
      }
      
      tboutname = paste0("ExtFunType_meandur_B", boutdur,
                         "M", (myfun$tbout.criter  * 100),
                         "%_", class)
      tbout_varname = paste0(tboutname, "_", boutnameEnding)
      
      daysummary[di, fi] = mn_dur_bouts
      ds_names[fi] = tbout_varname
      fi = fi + 1
    }
    
  }
  invisible(list(daysummary = daysummary, ds_names = ds_names,
                 fi = fi, di = di))
}