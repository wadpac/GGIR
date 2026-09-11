identify_levels = function(ts, TRLi, TRMi, TRVi, ws3, params_phyact = c(), myfun = c(),...) {
  #get input variables
  input = list(...)
  if (length(input) > 0 || length(params_phyact) == 0) {
    # Extract and check parameters if user provides more arguments than just the parameter arguments,
    # or if params_[...] aren't specified (so need to be filled with defaults).
    # So, inside GGIR this will not be used, but it is used when identify_level is used on its own
    # as if it was still the old identify_level function
    params = extract_params(params_phyact = params_phyact,
                            input = input) # load default parameters
    params_phyact = params$params_phyact
  }
  
  #=======================================================
  # LABEL INSENTITY LEVELS
  LEVELS = rep(0, length(ts$time))
  OLEVELS = rep(0, length(ts$time)) #to capture moderate and vigorous seperately
  LEVELS[ts$sibdetection == 1 & ts$diur == 1] = 0 #Sleep during the Sleep Period Time Window
  LEVELS[ts$sibdetection == 0 & ts$diur == 1] = 1 #Wakefullness during Sleep Period Time Window
  # activity during the night
  LEVELS[ts$sibdetection == 0 & ts$diur == 1 & ts$ACC >= TRLi & ts$ACC < TRMi] = 2 #LIGHT
  LEVELS[ts$sibdetection == 0 & ts$diur == 1 & ts$ACC >= TRMi & ts$ACC < TRVi] = 3 #MODERATE
  LEVELS[ts$sibdetection == 0 & ts$diur == 1 & ts$ACC >= TRVi] = 4 #VIGOROUS
  Lnames = c("spt_sleep","spt_wake_IN", "spt_wake_LIG", "spt_wake_MOD", "spt_wake_VIG")
  # activity during the day
  #======================================
  LEVELS[ts$diur == 0 & ts$ACC < TRLi] = 5 # Inactivit
  LEVELS[ts$diur == 0 & ts$ACC >= TRLi & ts$ACC < TRMi] = 6 #LIGHT
  LEVELS[ts$diur == 0 & ts$ACC >= TRMi & ts$ACC < TRVi] = 7 #MODERATE
  LEVELS[ts$diur == 0 & ts$ACC >= TRVi] = 8 #VIGOROUS
  Lnames = c(Lnames,"day_IN_unbt", "day_LIG_unbt", "day_MOD_unbt","day_VIG_unbt")
  # store separate copy of moderate and vigorous levels
  # note that when OLEVELS equal zero this is sleep period time window
  OLEVELS[LEVELS == 5] = 1 #IN
  OLEVELS[LEVELS == 6] = 2 #LIGHT
  OLEVELS[LEVELS == 7] = 3 #MOD
  OLEVELS[LEVELS == 8] = 4 #VIG
  
  
  #-------------------------------------
  # MVPA BOUTS
  LN = length(ts$time)
  
  boutduration = params_phyact[["boutdur.mvpa"]] * (60 / ws3) 
  NBL = length(boutduration) #number of bout lengths
  CL = 9 #current level
  refe = rep(0, LN)
  bc.mvpa = c()
  for (BL in 1:NBL) { # needs to be flexible to varibale number of bout lengths
    rr1 = rep(0, LN)
    p = which(ts$ACC >= TRMi & refe == 0 & ts$diur == 0); rr1[p] = 1
    out1 = g.getbout(x = rr1, boutduration = boutduration[BL],
                     boutcriter = params_phyact[["boutcriter.mvpa"]], ws3 = ws3)
    LEVELS[ts$diur == 0 & out1 == 1] = CL
    bc.mvpa = rbind(bc.mvpa,out1)
    refe = refe + out1
    if (BL == 1) {
      Lnames = c(Lnames,paste0("day_MVPA_bts_", params_phyact[["boutdur.mvpa"]][BL]))
    } else {
      Lnames = c(Lnames,paste0("day_MVPA_bts_", params_phyact[["boutdur.mvpa"]][BL], "_",
                               params_phyact[["boutdur.mvpa"]][BL - 1]))
    }
    CL = CL + 1
  }
  #-------------------------------------
  # INACTIVITY BOUTS
  LN = length(ts$time)
  boutduration = params_phyact[["boutdur.in"]] * (60/ws3)
  NBL = length(boutduration) #number of bout lengths
  bc.in = c()
  for (BL in 1:NBL) {
    rr1 = rep(0, LN)
    p = which(ts$ACC < TRLi & refe == 0 & ts$diur == 0); rr1[p] = 1
    out1 = g.getbout(x = rr1, boutduration = boutduration[BL],
                     boutcriter = params_phyact[["boutcriter.in"]], ws3 = ws3)
    LEVELS[ts$diur == 0 & out1 == 1] = CL
    bc.in = rbind(bc.in, out1)
    refe = refe + out1
    if (BL == 1) {
      Lnames = c(Lnames,paste0("day_IN_bts_", params_phyact[["boutdur.in"]][BL]))
    } else {
      Lnames = c(Lnames,paste0("day_IN_bts_", params_phyact[["boutdur.in"]][BL], "_",
                               params_phyact[["boutdur.in"]][BL - 1]))
    }
    CL = CL + 1
  }
  #-------------------------------------
  # LIGHT BOUTS
  LN = length(ts$time)
  boutduration = params_phyact[["boutdur.lig"]] * (60 / ws3)
  NBL = length(boutduration) #number of bout lengths
  bc.lig = c()
  for (BL in 1:NBL) {
    rr1 = rep(0, LN)
    p = which(ts$ACC >= TRLi & refe == 0 & ts$ACC < TRMi & ts$diur == 0); rr1[p] = 1
    out1 = g.getbout(x = rr1, boutduration = boutduration[BL],
                     boutcriter = params_phyact[["boutcriter.lig"]], ws3 = ws3)                    
    LEVELS[ts$diur == 0 & out1 == 1] = CL
    bc.lig = rbind(bc.lig,out1)
    refe = refe + out1
    if (BL == 1) {
      Lnames = c(Lnames,paste0("day_LIG_bts_",params_phyact[["boutdur.lig"]][BL]))
    } else {
      Lnames = c(Lnames,paste0("day_LIG_bts_", params_phyact[["boutdur.lig"]][BL],
                               "_", params_phyact[["boutdur.lig"]][BL - 1]))
    }
    CL = CL + 1
  }
  #-------------------------------------
  # TYPE BOUTS
  bc.type = list()
  Tnames = c()
  
  if (!is.null(myfun) &&
      length(myfun) > 0 &&
      "reporttype" %in% names(myfun) &&
      myfun$reporttype == "type") {
    
    # Identify external function type columns
    type_columns = grep("^ExtFunType_", names(ts), value = TRUE)
    
    if (length(type_columns) > 0) {
      
      # Get type levels from the ExtFunType columns
      type_levels  = sub(
        pattern = paste0("^ExtFunType_", myfun$colnames, "_"),
        replacement = "",
        x = type_columns
      )
      type_levels  = sub("_s$", "", type_levels)
      
      # set defaults when user has not defined bout parameters for types
      if ("tbout.dur" %in% names(myfun) == FALSE) myfun[["tbout.dur"]]  = c(1, 5, 10)
      if ("tbout.criter" %in% names(myfun) == FALSE) myfun[["tbout.criter"]] = 0.8
      if ("tbout.order" %in% names(myfun) == FALSE) myfun[["tbout.order"]] = sort(type_levels)
      
      # Bout duration priority (longer to shorter)
      myfun[["tbout.dur"]] = sort(myfun[["tbout.dur"]], decreasing = TRUE)
      
      # Bout detection priority
      type_order = tolower(myfun[["tbout.order"]])
      type_order = c(
        type_order,
        setdiff(tolower(type_levels), type_order) # just in case tbout.order does not contain all categories 
      )
      
      # Bout duration in number of epochs
      boutduration = myfun[["tbout.dur"]] * (60 / ws3)
      NBL = length(boutduration)
      
      # Keep track of epochs already assigned to a type bout
      refe.type = rep(0, LN)
      
      for (type_level in type_order) {
        
        # Find column corresponding to this type
        type_col = type_columns[tolower(type_levels) == type_level]
        
        # Store bout results for this type
        bc.type[[type_level]] = c()
        
        for (BL in 1:NBL) {
          # construct type candidate for bouts
          rr1 = rep(0, LN)
          p = which(ts[, type_col] > 0 & refe.type == 0 & ts$diur == 0)
          rr1[p] = 1
          
          # run bout detection
          out1 = g.getbout(
            x = rr1,
            boutduration = boutduration[BL],
            boutcriter = myfun[["tbout.criter"]],
            ws3 = ws3
          )
          
          # store result
          bc.type[[type_level]] = rbind(bc.type[[type_level]], out1)
          
          # make epochs in this bout unavailable to subsequent types
          refe.type = refe.type + out1
          
          # names for type bout levels
          if (BL == 1) {
            Tnames = c(Tnames, paste0("day_", type_level, "_bts_", myfun[["tbout.dur"]][BL]))
          } else {
            Tnames = c(Tnames, paste0("day_", type_level, "_bts_", 
                                      myfun[["tbout.dur"]][BL], "_",
                                      myfun[["tbout.dur"]][BL - 1]
            )
            )
          }
        }
      }
    }
  }
  invisible(list(LEVELS = LEVELS, OLEVELS = OLEVELS,
                 Lnames = Lnames, Tnames = Tnames,
                 bc.mvpa = bc.mvpa, bc.lig = bc.lig, bc.in = bc.in,  bc.type = bc.type,
                 ts = ts, threshold = c(TRLi, TRMi, TRVi)))
}