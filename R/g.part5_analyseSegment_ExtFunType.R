g.part5_analyseSegment_ExtFunType = function(ts, sse, TOLEVELS, TLEVELS,
                                             Tnames, myfun, ws3new, dsummary,
                                             ds_names, si, fi) {
  
  #===============================================
  # EXTERNAL FUNCTION TYPE METRICS
  
  # Prefix to be appended to all columns generated here
  # So that we can easily identify these columns in subsequent calculations
  # and store reports with this output in separate files
  prefix = paste0("ExtFunType_")
  type_levels = names(TLEVELS)
  
  #=================================================
  # BOUTED AND UNBOUTED TIME
  # As type levels might be applicable either during
  # spt or day, depending on the types being 
  # identified by the external function. Here 
  # we calculate durations in both spt and day
  #=================================================
  

  # -------------------------------
  # SPT
  spt_sse = sse[ts$diur[sse] == 1]
  
  for (type_level in type_levels) {
    
    type_seconds = TOLEVELS[spt_sse, type_level]
    
    #---------------------------------------------
    # UNBOUT
    # Time outside any bout definition
    any_bout = colSums(
      TLEVELS[[type_level]][, spt_sse, drop = FALSE]
    ) > 0
    
    unbt_seconds = sum(
      type_seconds[!any_bout],
      na.rm = TRUE
    )
    
    dsummary[si, fi] = unbt_seconds / 60
    ds_names[fi] = paste0(
      prefix, "dur_spt_", type_level, "_unbt_min"
    )
    fi = fi + 1
    
    #---------------------------------------------
    # BOUT DURATION
    type_index = match(type_level, names(TLEVELS))
    
    if (type_index > 1) {
      n_previous_bouts = sum(
        sapply(
          TLEVELS[seq_len(type_index - 1)],
          nrow
        )
      )
    } else {
      n_previous_bouts = 0
    }
    
    for (bci in 1:nrow(TLEVELS[[type_level]])) {
      
      bout_mask = TLEVELS[[type_level]][bci, spt_sse]
      
      bout_seconds = sum(
        type_seconds[bout_mask == 1],
        na.rm = TRUE
      )
      
      dsummary[si, fi] = bout_seconds / 60
      
      bout_name = Tnames[n_previous_bouts + bci]
      
      ds_names[fi] = paste0(
        prefix, "dur_spt_", bout_name, "_min"
      )
      
      fi = fi + 1
    }
    
    #---------------------------------------------
    # TOTAL DURATION
    total_seconds = sum(
      type_seconds,
      na.rm = TRUE
    )
    
    dsummary[si, fi] = total_seconds / 60
    ds_names[fi] = paste0(
      prefix, "dur_spt_total_", type_level, "_min"
    )
    fi = fi + 1
    
    #---------------------------------------------
    # AVERAGE ACC
    type_sse = spt_sse[type_seconds > 0]
    
    dsummary[si, fi] = mean(
      ts$ACC[type_sse],
      na.rm = TRUE
    )
    
    ds_names[fi] = paste0(
      prefix, "ACC_spt_", type_level, "_mg"
    )
    fi = fi + 1
    
    #---------------------------------------------
    # NUMBER OF BLOCKS
    RLE = rle(type_seconds > 0)
    
    dsummary[si, fi] = length(
      which(RLE$values == TRUE)
    )
    
    ds_names[fi] = paste0(
      prefix, "Nblocks_spt_", type_level
    )
    fi = fi + 1
    
    #---------------------------------------------
    # NUMBER OF BOUTS
    for (bci in 1:nrow(TLEVELS[[type_level]])) {
      
      RLE = rle(
        TLEVELS[[type_level]][bci, spt_sse]
      )
      
      dsummary[si, fi] = length(
        which(RLE$values == 1)
      )
      
      bout_name = Tnames[n_previous_bouts + bci]
      
      ds_names[fi] = paste0(
        prefix, "Nbouts_spt_", bout_name
      )
      
      fi = fi + 1
    }
  }
  
  # -------------------------------
  # DAY
  day_sse = sse[ts$diur[sse] == 0]
  
  for (type_level in type_levels) {
    
    type_seconds = TOLEVELS[day_sse, type_level]
    
    #---------------------------------------------
    # UNBOUT
    # Time outside any bout definition
    any_bout = colSums(
      TLEVELS[[type_level]][, day_sse, drop = FALSE]
    ) > 0
    
    unbt_seconds = sum(
      type_seconds[!any_bout],
      na.rm = TRUE
    )
    
    dsummary[si, fi] = unbt_seconds / 60
    ds_names[fi] = paste0(
      prefix, "dur_day_", type_level, "_unbt_min"
    )
    fi = fi + 1
    
    #---------------------------------------------
    # BOUT DURATION
    type_index = match(type_level, names(TLEVELS))
    
    if (type_index > 1) {
      n_previous_bouts = sum(
        sapply(
          TLEVELS[seq_len(type_index - 1)],
          nrow
        )
      )
    } else {
      n_previous_bouts = 0
    }
    
    for (bci in 1:nrow(TLEVELS[[type_level]])) {
      
      bout_mask = TLEVELS[[type_level]][bci, day_sse]
      
      bout_seconds = sum(
        type_seconds[bout_mask == 1],
        na.rm = TRUE
      )
      
      dsummary[si, fi] = bout_seconds / 60
      
      bout_name = Tnames[n_previous_bouts + bci]
      
      ds_names[fi] = paste0(
        prefix, "dur_day_", bout_name, "_min"
      )
      
      fi = fi + 1
    }
    
    #---------------------------------------------
    # TOTAL DURATION
    total_seconds = sum(
      type_seconds,
      na.rm = TRUE
    )
    
    dsummary[si, fi] = total_seconds / 60
    ds_names[fi] = paste0(
      prefix, "dur_day_total_", type_level, "_min"
    )
    fi = fi + 1
    
    #---------------------------------------------
    # AVERAGE ACC
    type_sse = day_sse[type_seconds > 0]
    
    dsummary[si, fi] = mean(
      ts$ACC[type_sse],
      na.rm = TRUE
    )
    
    ds_names[fi] = paste0(
      prefix, "ACC_day_", type_level, "_mg"
    )
    fi = fi + 1
    
    #---------------------------------------------
    # NUMBER OF BLOCKS
    RLE = rle(type_seconds > 0)
    
    dsummary[si, fi] = length(
      which(RLE$values == TRUE)
    )
    
    ds_names[fi] = paste0(
      prefix, "Nblocks_day_", type_level
    )
    fi = fi + 1
    
    #---------------------------------------------
    # NUMBER OF BOUTS
    for (bci in 1:nrow(TLEVELS[[type_level]])) {
      
      RLE = rle(
        TLEVELS[[type_level]][bci, day_sse]
      )
      
      dsummary[si, fi] = length(
        which(RLE$values == 1)
      )
      
      bout_name = Tnames[n_previous_bouts + bci]
      
      ds_names[fi] = paste0(
        prefix, "Nbouts_day_", bout_name
      )
      
      fi = fi + 1
    }
  }
  
  
  #===============================================
  # return
  invisible(
    list(
      dsummary = dsummary,
      ds_names = ds_names,
      fi = fi
    )
  )
}