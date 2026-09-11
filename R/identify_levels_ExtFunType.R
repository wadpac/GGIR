identify_levels_ExtFunType = function(ts, myfun, ws3) {
  
  #===============================================
  # EXTERNAL FUNCTION TYPES
  
  TLEVELS = list()  # type bout masks
  TOLEVELS = NULL   # type durations
  Tnames = c()      # type bout names
  
  if (!is.null(myfun) &&
      length(myfun) > 0 &&
      "reporttype" %in% names(myfun) &&
      myfun$reporttype == "type") {
    
    type_columns = grep(
      "^ExtFunType_.*_s$",
      names(ts),
      value = TRUE
    )
    
    if (length(type_columns) > 0) {
      
      # Extract type levels from column names
      type_levels = sub(
        pattern = paste0("^ExtFunType_", myfun$colnames, "_"),
        replacement = "",
        x = type_columns
      )
      
      type_levels = sub(
        "_s$",
        "",
        type_levels
      )
      
      type_levels = tolower(type_levels)
      
      # Seconds assigned to each type in each epoch
      TOLEVELS = as.matrix(
        ts[, type_columns, drop = FALSE]
      )
      
      colnames(TOLEVELS) = type_levels
      
      #---------------------------------------------
      # Default bout parameters
      
      if ("tbout.dur" %in% names(myfun) == FALSE) {
        myfun[["tbout.dur"]] = c(1, 5, 10)
      }
      
      if ("tbout.criter" %in% names(myfun) == FALSE) {
        myfun[["tbout.criter"]] = 0.8
      }
      
      if ("tbout.order" %in% names(myfun) == FALSE) {
        myfun[["tbout.order"]] = sort(type_levels)
      }
      
      # Longest bout duration first
      myfun[["tbout.dur"]] = sort(
        myfun[["tbout.dur"]],
        decreasing = TRUE
      )
      
      # Make bout order case insensitive
      type_order = tolower(
        myfun[["tbout.order"]]
      )
      
      # Add any types not explicitly included in tbout.order
      type_order = c(
        type_order,
        setdiff(type_levels, type_order)
      )
      
      # Convert bout durations from minutes to epochs
      boutduration = myfun[["tbout.dur"]] * (60 / ws3)
      
      NBL = length(boutduration)
      
      # Used to ensure that an epoch is assigned to only one
      # type according to tbout.order
      refe.type = rep(0, nrow(ts))
      
      #---------------------------------------------
      # Identify bouts for each type
      
      for (type_level in type_order) {
        
        type_col = type_columns[
          tolower(type_levels) == type_level
        ]
        
        TLEVELS[[type_level]] = c()
        
        for (BL in 1:NBL) {
          
          rr1 = rep(0, nrow(ts))
          
          # Epochs classified as this type and not already
          # assigned to a higher-priority type
          p = which(
            ts[, type_col] > 0 &
              refe.type == 0
          )
          
          rr1[p] = 1
          
          out1 = g.getbout(
            x = rr1,
            boutduration = boutduration[BL],
            boutcriter = myfun[["tbout.criter"]],
            ws3 = ws3
          )
          
          TLEVELS[[type_level]] = rbind(
            TLEVELS[[type_level]],
            out1
          )
          
          refe.type = refe.type + out1
          
          # Name the bout according to its duration
          if (BL == 1) {
            bout_name = paste0(
              type_level,
              "_bts_",
              myfun[["tbout.dur"]][BL]
            )
          } else {
            bout_name = paste0(
              type_level,
              "_bts_",
              myfun[["tbout.dur"]][BL],
              "_",
              myfun[["tbout.dur"]][BL - 1]
            )
          }
          
          Tnames = c(
            Tnames,
            bout_name
          )
        }
      }
    }
  }
  
  #===============================================
  # Return
  
  invisible(
    list(
      TLEVELS = TLEVELS,
      TOLEVELS = TOLEVELS,
      Tnames = Tnames
    )
  )
}