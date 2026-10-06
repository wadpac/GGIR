g.part5_analyseSegment_ExtFunType = function(ts, sse, TOLEVELS, TLEVELS,
                                             Tnames, myfun, ws3new, dsummary,
                                             ds_names, si, fi) {
  #===============================================
  # Helper function to calculate duration, mean ACC, and number of blocks
  # for the selected epochs of a given external-function type
  get_output_vars = function(mask, output, type_col, type_level, bci = NULL) {
    if (output == "bout") {
      duration = TLEVELS[[type_col]][[type_level]][bci, ] * ws3new
    } else {
      duration = TOLEVELS[[type_col]][, type_level]
    }
    
    selected = duration > 0 & mask
    
    dur = sum(duration[selected], na.rm = TRUE) / 60
    
    if (any(selected) && any(!is.na(ts$ACC[selected]))) {
      acc = mean(ts$ACC[selected], na.rm = TRUE)
    } else {
      acc = NA_real_
    }
    
    RLE = rle(selected)
    Nb = sum(RLE$values)
    
    matrix(c(dur, acc, Nb), nrow = 1)
  }
  
  #===============================================
  # EXTERNAL FUNCTION TYPE METRICS
  
  # Prefix to be appended to all columns generated here
  # So that we can easily identify these columns in subsequent calculations
  # and store reports with this output in separate files
  prefix = "ExtFunType_"
  type_columns = names(TLEVELS)
  
  # save initial variables for sortering of columns at the end
  fi_initial = fi
  
  # binary indicator of epochs in ts belonging to current segment
  sse_mask = rep(FALSE, nrow(ts)) 
  sse_mask[sse] = TRUE
    
  #=================================================
  # BOUTED AND UNBOUTED TIME
  # As type levels might be applicable either during
  # spt or day, depending on the types being 
  # identified by the external function. Here 
  # we calculate durations in both spt and day
  #=================================================
  
  
  # -------------------------------
  # Loop through type columns available
  for (type_col in type_columns) {
    
    # levels in this column
    type_levels = names(TLEVELS[[type_col]])
    
    # -------------------------------
    # Loop through spt (diur = 1) and day (diur = 0) windows
    for (window in c("spt", "day")) {
      # diur_mask = binary indicator of epochs belonging to window of interest within current segment
      if (window == "spt") diur_mask = ts$diur == 1 & sse_mask 
      if (window == "day") diur_mask = ts$diur == 0 & sse_mask
      
      # -------------------------------
      # Loop through type levels returned by external function
      for (type_level in type_levels) {
        #---------------------------------------------
        # Select epochs of interest for each calculation
        # then, calculate dur, ACC, and Nblocks/Nbouts
        
        # UNBOUTED TIME ---------------------------------------------
        # Time outside any bout (i.e., bout sum == 0)
        unbt_mask = colSums(TLEVELS[[type_col]][[type_level]]) == 0 & diur_mask
        unbt = get_output_vars(
          unbt_mask,
          output = "unbt",
          type_col = type_col,
          type_level = type_level
        )
        dsummary[si, fi:(fi + 2)] = unbt
        ds_names[fi:(fi + 2)] = c(
          paste0(prefix, type_col, "_dur_", window, "_", type_level, "_unbt_min"),
          paste0(prefix, type_col, "_ACC_", window, "_", type_level, "_unbt_mg"),
          paste0(prefix, type_col, "_Nblocks_", window, "_", type_level, "_unbt")
        )
        fi = fi + 3 
        
        # BOUTS ---------------------------------------------
        # Time in bouts
        for (bci in 1:nrow(TLEVELS[[type_col]][[type_level]])) {
          bout_mask = TLEVELS[[type_col]][[type_level]][bci,] > 0 & diur_mask
          bts = get_output_vars(
            bout_mask,
            output = "bout",
            type_col = type_col,
            type_level = type_level,
            bci = bci
          )
          dsummary[si, fi:(fi + 2)] = bts
          bout_name = grep(paste0("^", type_level, "_bts_"), Tnames, value = TRUE)[bci]
          ds_names[fi:(fi + 2)] = c(
            paste0(prefix, type_col, "_dur_", window, "_", bout_name, "_min"),
            paste0(prefix, type_col, "_ACC_", window, "_", bout_name, "_mg"),
            paste0(prefix, type_col, "_Nbouts_", window, "_", bout_name)
          )
          fi = fi + 3
        }
        
        # TOTALS ---------------------------------------------
        # Total time in type
        totals = get_output_vars(
          diur_mask,
          output = "total",
          type_col = type_col,
          type_level = type_level
        )
        dsummary[si, fi:(fi + 2)] = totals
        ds_names[fi:(fi + 2)] = c(
          paste0(prefix, type_col, "_dur_", window, "_total_", type_level, "_min"),
          paste0(prefix, type_col, "_ACC_", window, "_total_", type_level, "_mg"),
          paste0(prefix, type_col, "_Nblocks_", window, "_total_", type_level)
        )
        fi = fi + 3
      }
    }
  }
  
  #===============================================
  # return
  invisible(list(dsummary = dsummary, ds_names = ds_names, fi = fi))
}