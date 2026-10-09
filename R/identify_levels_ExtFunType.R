identify_levels_ExtFunType = function(ts, myfun, ws3) {
  
  #===============================================
  # EXTERNAL FUNCTION TYPES
  
  TLEVELS = list()    # type bout masks
  TOLEVELS = list()   # type durations
  Tnames = c()        # type bout names
  
  if (!is.null(myfun) && length(myfun) > 0 &&
      "reporttype" %in% names(myfun)) {
    
    reporttypes = rep(myfun$reporttype, length.out = length(myfun$colnames))
    type_indices = which(reporttypes == "type")
    
    if (length(type_indices) > 0) {
      
      type_columns_myfun = myfun$colnames[type_indices]
      
      #---------------------------------------------
      # Identify columns created for each type output
      type_columns = list()
      
      for (type_col in type_columns_myfun) {
        this_type_columns = grep(
          paste0("^ExtFunType_", type_col, "_.*_s$"),
          names(ts),
          value = TRUE
        )
        
        if (length(this_type_columns) > 0) {
          type_columns[[type_col]] = this_type_columns
        }
      }
      
      if (length(type_columns) > 0) {
        
        #---------------------------------------------
        # Extract type levels and create TOLEVELS
        for (type_col in names(type_columns)) {
          
          this_type_columns = type_columns[[type_col]]
          
          this_type_levels = sub(
            pattern = paste0("^ExtFunType_", type_col, "_"),
            replacement = "",
            x = this_type_columns
          )
          this_type_levels = sub("_s$", "", this_type_levels)
          this_type_levels = tolower(this_type_levels)
          
          this_TOLEVELS = as.matrix(
            ts[, this_type_columns, drop = FALSE]
          )
          
          colnames(this_TOLEVELS) = this_type_levels
          
          TOLEVELS[[type_col]] = this_TOLEVELS
        }
        
        #---------------------------------------------
        # Default bout parameters
        
        if (!"tbout.dur" %in% names(myfun)) {
          myfun[["tbout.dur"]] = c(1, 5, 10)
        }
        
        if (!"tbout.criter" %in% names(myfun)) {
          myfun[["tbout.criter"]] = 0.8
        }
        
        #---------------------------------------------
        # Identify bouts separately for each type output
        
        for (type_col in names(type_columns)) {
          
          this_type_columns = type_columns[[type_col]]
          
          # Extract levels for this type output
          this_type_levels = sub(
            pattern = paste0("^ExtFunType_", type_col, "_"),
            replacement = "",
            x = this_type_columns
          )
          this_type_levels = sub("_s$", "", this_type_levels)
          this_type_levels = tolower(this_type_levels)
          
          # Default order
          if (!"tbout.order" %in% names(myfun)) {
            type_order = sort(this_type_levels)
          } else {
            type_order = tolower(myfun[["tbout.order"]])
            
            # Keep only levels belonging to this type output
            type_order = type_order[type_order %in% this_type_levels]
            
            # Add levels not explicitly included in tbout.order
            type_order = c(
              type_order,
              setdiff(this_type_levels, type_order)
            )
          }
          
          # Longest bout duration first
          bout_durations = sort(
            myfun[["tbout.dur"]],
            decreasing = TRUE
          )
          
          # Convert bout durations from minutes to epochs
          boutduration = bout_durations * (60 / ws3)
          
          NBL = length(boutduration)
          
          # refe is used to ensure that bout classification
          # is mutually exclusive across bout durations
          refe.type = rep(0, nrow(ts))
          
          #---------------------------------------------
          # Identify bouts for each type level
          
          TLEVELS[[type_col]] = list()
          
          for (type_level in type_order) {
            
            type_col_ts = this_type_columns[
              this_type_levels == type_level
            ]
            
            TLEVELS[[type_col]][[type_level]] = c()
            
            for (BL in 1:NBL) {
              
              rr1 = rep(0, nrow(ts))
              
              # Identify candidates for bouts
              p = which(
                ts[, type_col_ts] > 0 &
                  refe.type == 0
              )
              rr1[p] = 1
              
              # Identify bouts
              out1 = g.getbout(
                x = rr1,
                boutduration = boutduration[BL],
                boutcriter = myfun[["tbout.criter"]],
                ws3 = ws3
              )
              
              TLEVELS[[type_col]][[type_level]] =
                rbind(
                  TLEVELS[[type_col]][[type_level]],
                  out1
                )
              
              refe.type = refe.type + out1
              
              # Name the bout according to its duration
              if (BL == 1) {
                bout_name = paste0(
                  type_level, "_bts_",
                  bout_durations[BL]
                )
              } else {
                bout_name = paste0(
                  type_level, "_bts_",
                  bout_durations[BL],
                  "_",
                  bout_durations[BL - 1]
                )
              }
              
              Tnames = c(Tnames, bout_name)
            }
          }
        }
      }
    }
  }
  
  # ----------------------
  # Return
  
  invisible(list(
    TLEVELS = TLEVELS,
    TOLEVELS = TOLEVELS,
    Tnames = Tnames
  ))
}
