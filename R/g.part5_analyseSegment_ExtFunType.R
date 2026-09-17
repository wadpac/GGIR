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
  
  # save initial variables for sortering of columns at the end
  fi_initial = fi
  
  #=================================================
  # BOUTED AND UNBOUTED TIME
  # As type levels might be applicable either during
  # spt or day, depending on the types being 
  # identified by the external function. Here 
  # we calculate durations in both spt and day
  #=================================================
  

  # -------------------------------
  # Loop through spt (diur = 1) and day (diur = 0) windows
  for (window in c("spt", "day")) {
    if (window == "spt") sse_mask = ts$diur[sse] == 1
    if (window == "day") sse_mask = ts$diur[sse] == 0
    
    # -------------------------------
    # Loop through type levels returned by external function
    for (type_level in type_levels) {
      
      # Calculate duration, mean ACC, and number of blocks
      # for the selected epochs of a given external-function type
      get_output_vars = function(mask) {
        type_seconds = TOLEVELS[mask, type_level]
        type_mask = type_seconds > 0
        
        # Duration of the type in minutes
        dur = sum(type_seconds, na.rm = TRUE) / 60
        
        # Mean ACC during epochs containing the type
        acc = mean(
          ts$ACC[which(mask)[type_mask]],
          na.rm = TRUE
        )
        
        # Number of contiguous blocks containing the type
        RLE = rle(type_mask)
        Nb = sum(RLE$values)
        
        matrix(c(dur, acc, Nb), nrow = 1)
      }
      
      #---------------------------------------------
      # Select epochs of interest for each calculation
      # then, calculate dur, ACC, and Nblocks/Nbouts
      
      # UNBOUTED TIME ---------------------------------------------
      # Time outside any bout (i.e., bout sum == 0)
      unbt_mask = colSums(TLEVELS[[type_level]][, sse]) == 0 & sse_mask
      unbt = get_output_vars(unbt_mask)
      dsummary[si, fi:(fi + 2)] = unbt
      ds_names[fi:(fi + 2)] = c(
        paste0(prefix, "dur_", window, "_", type_level, "_unbt_min"),
        paste0(prefix, "ACC_", window, "_", type_level, "_unbt_mg"),
        paste0(prefix, "Nblocks_", window, "_", type_level, "_unbt")
      )
      fi = fi + 3 
      
      # BOUTS ---------------------------------------------
      # Time in bouts
      for (bci in 1:nrow(TLEVELS[[type_level]])) {
        bout_mask = TLEVELS[[type_level]][bci, sse] > 0 & sse_mask
        bts = get_output_vars(bout_mask)
        dsummary[si, fi:(fi + 2)] = bts
        bout_name = grep(type_level, Tnames, value = T)[bci]
        ds_names[fi:(fi + 2)] = c(
          paste0(prefix, "dur_", window, "_", bout_name, "_min"),
          paste0(prefix, "ACC_", window, "_", bout_name, "_mg"),
          paste0(prefix, "Nbouts_", window, "_", bout_name)
        )
        fi = fi + 3
      }
      
      # TOTALS ---------------------------------------------
      # Total time in type
      total_mask = sse_mask
      totals = get_output_vars(total_mask)
      dsummary[si, fi:(fi + 2)] = totals
      ds_names[fi:(fi + 2)] = c(
        paste0(prefix, "dur_", window, "_total_", type_level, "_min"),
        paste0(prefix, "ACC_", window, "_total_", type_level, "_mg"),
        paste0(prefix, "Nblocks_", window, "_total_", type_level)
      )
      fi = fi + 3
    }
  }
  
  #===============================================
  # Sort output (replicating order in part 5 reports)
  valid = fi_initial:(fi - 1)
  x = ds_names[valid]
  
  #-----------------------------------------------
  # Define order of group variables (spt > day, unbt > bt > total,...)
  
  group_order = rep(99L, length(x))
  
  group_order[grepl("^ExtFunType_dur_spt_.+_unbt_min$", x)]  = 1
  group_order[grepl("^ExtFunType_dur_spt_.+_bts_", x)]       = 2
  group_order[grepl("^ExtFunType_dur_spt_total_", x)]        = 3
  
  group_order[grepl("^ExtFunType_ACC_spt_.+_unbt_mg$", x)]   = 4
  group_order[grepl("^ExtFunType_ACC_spt_.+_bts_", x)]       = 5
  group_order[grepl("^ExtFunType_ACC_spt_total_", x)]        = 6
  
  group_order[grepl("^ExtFunType_Nblocks_spt_.+_unbt$", x)]  = 7
  group_order[grepl("^ExtFunType_Nbouts_spt_.+_bts_", x)]    = 8
  group_order[grepl("^ExtFunType_Nblocks_spt_total_", x)]    = 9
  
  group_order[grepl("^ExtFunType_dur_day_.+_unbt_min$", x)]  = 10
  group_order[grepl("^ExtFunType_dur_day_.+_bts_", x)]       = 11
  group_order[grepl("^ExtFunType_dur_day_total_", x)]        = 12
  
  group_order[grepl("^ExtFunType_ACC_day_.+_unbt_mg$", x)]   = 13
  group_order[grepl("^ExtFunType_ACC_day_.+_bts_", x)]       = 14
  group_order[grepl("^ExtFunType_ACC_day_total_", x)]        = 15
  
  group_order[grepl("^ExtFunType_Nblocks_day_.+_unbt$", x)]  = 16
  group_order[grepl("^ExtFunType_Nbouts_day_.+_bts_", x)]    = 17
  group_order[grepl("^ExtFunType_Nblocks_day_total_", x)]    = 18
  
  #-----------------------------------------------
  # Bout duration: longest to shortest
  bout_order = rep(0, length(x))
  is_bout = grepl("_bts_", x)
  
  bout_order[is_bout] = as.numeric(
    sub(".*_bts_([0-9]+).*", "\\1", x[is_bout])
  )
  
  # Longest lower bound first
  bout_order[is_bout] = -bout_order[is_bout]
  
  #-----------------------------------------------
  # Final ordering
  #
  # group_order determines the main structure.
  # bout_order determines the order within bout groups.
  # x provides deterministic ordering for types.
  
  ord = order(group_order, bout_order, x)
  
  # final order
  final_idx = c(
    seq_len(fi_initial - 1), # columns that were here before entering the function
    valid[ord],              # ordered external-function type columns
    fi:ncol(dsummary)        # empty columns (space for additional output)
  )
  
  ds_names = ds_names[final_idx]
  dsummary = dsummary[, final_idx, drop = FALSE]

  
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