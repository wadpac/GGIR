g.fragmentation = function(frag.metrics = c("mean", "TP", "Gini", "power",
                                            "CoV", "all"),
                           ACC = c(), intensity.thresholds = c(), LEVELS = c(),
                           Lnames=c(), xmin=1) { 
  
  # This function is inspired from R package ActFrag as developed by Junrui Di.
  #
  # frag.metrics: metric to define fragmentation
  # in contract to R package ActFrag this function assumes
  # that non-wear and missing values have already been taken care of outside this
  # function.
  # xmin is shortest recordable boutlength
  
  if ("all" %in% frag.metrics) {
    frag.metrics = c("mean", "TP", "Gini", "power",
                     "CoV", "all")
  }
  min_Nfragments = 10 # minimum number of required fragments
  intensity.thresholds = c(0, intensity.thresholds) 
  output = list()
  if (length(LEVELS) > 0) {
    # convert to class names to numeric class ids for inactive, light and MVPA:
    classes.in = c("day_IN_unbt", Lnames[grep(pattern ="day_IN_bts", x = Lnames)])
    class.in.ids = which(Lnames %in%  classes.in) - 1 
    classes.lig = c("day_LIG_unbt", Lnames[grep(pattern ="day_LIG_bts", x = Lnames)])
    class.lig.ids = which(Lnames %in%  classes.lig) - 1
    classes.mvpa = c("day_MOD_unbt", "day_VIG_unbt", Lnames[grep(pattern ="day_MVPA_bts", x = Lnames)])
    class.mvpa.ids = which(Lnames %in% classes.mvpa) - 1
  }
  if (length(ACC) > 1) { # metrics that require more than just binary
    #====================================================
    # Convert ACC into categorical multi-class behaviours
    y = rep(0,length(ACC))
    if (length(LEVELS) == 0) {
      for (ij in 1:length(intensity.thresholds)) {
        if (ij != length(intensity.thresholds)) {
          ij_indices = which(ACC >= intensity.thresholds[ij] & ACC < intensity.thresholds[ij+1])
          if (length(ij_indices) > 0) y[ij_indices] = ij
        } else {
          ij_indices = which(ACC >= intensity.thresholds[ij])
          if (length(ij_indices) > 0) y[ij_indices] = ij
        }
      }
      y[which(y == 4)] = 3 # collapse moderate and vigorous into mvpa
    } else {
      y[which(LEVELS %in% class.in.ids)] = 1
      y[which(LEVELS %in% class.lig.ids)] = 2
      y[which(LEVELS %in% class.mvpa.ids)] = 3
    }
    
    #====================================================
    # TP (transition probability) metrics that depend on multiple classes
    if ("TP" %in% frag.metrics) {
      fragments3 = rle(y) # fragments is now data.frame with value (intensity) and length (duration of fragment) 
      Nfrag3 = length(fragments3$value[which(fragments3$value == 1)])
      output[["TP_IN2PA"]] = output[["TP_PA2IN"]] = 0
      output[["TP_IN2LIPA"]] = output[["Nfrag_IN2LIPA"]] = 0
      output[["TP_IN2MVPA"]] = output[["Nfrag_IN2MVPA"]] = 0
      output[["Nfrag_LIPA"]] = output[["Nfrag_MVPA"]] = 0
      output[["mean_dur_LIPA"]] = output[["mean_dur_MVPA"]] = 0
      if (Nfrag3 > 0) { # at least 2 inactivity fragment
        Duration0 = fragments3$length[which(fragments3$value != 1)] # all activity fragments
        Duration1 = fragments3$length[which(fragments3$value == 1)] # all inactivity fragments
        DurationLIPA = fragments3$length[which(fragments3$value == 2)] # all light fragments
        DurationMVPA = fragments3$length[which(fragments3$value == 3)] # all MVPA fragments
        output[["Nfrag_LIPA"]] = length(DurationLIPA)
        output[["Nfrag_MVPA"]] = length(DurationMVPA)
        # Get only indices of inactive fragments that transition to light:
        Nfrag4 = length(fragments3$value)
        inact_2_light_trans = which(fragments3$value[1:(Nfrag4-1)] == 1 & fragments3$value[2:Nfrag4] == 2)
        # Get only indices of inactive fragments that transition to mvpa:
        inact_2_mvpa_trans = which(fragments3$value[1:(Nfrag4-1)] == 1 & fragments3$value[2:Nfrag4] == 3)
        if (length(inact_2_light_trans) > 0) {
          output[["mean_dur_LIPA"]] = mean(DurationLIPA)
          Duration2 = fragments3$length[inact_2_light_trans] # durations of all inactivity fragments followed by light.
          output[["TP_IN2LIPA"]] = (sum(Duration2)/sum(Duration1)) / mean(Duration1) # transition from inactive to mvpa
          output[["Nfrag_IN2LIPA"]] = length(Duration2)
        } 
        if (length(inact_2_mvpa_trans) > 0) {
          output[["mean_dur_MVPA"]] = mean(DurationMVPA)
          Duration3 = fragments3$length[inact_2_mvpa_trans] # durations of all inactivity fragments followed by light.
          output[["TP_IN2MVPA"]] = (sum(Duration3)/sum(Duration1)) / mean(Duration1) # transition from inactive to mvpa
          output[["Nfrag_IN2MVPA"]] = length(Duration3)
        }
        output[["TP_IN2PA"]] = 1 / mean(Duration1) # transition from inactive to active (no distinction light or MVPA)
        output[["TP_PA2IN"]] = 1 / mean(Duration0) # transition from active to inactive (no distinction light or MVPA)
        if (length(inact_2_light_trans) == 0 & length(inact_2_mvpa_trans) != 0) { # only IN2mvpa transitions
          output[["TP_IN2MVPA"]] = 1 / mean(Duration1)
          output[["Nfrag_IN2MVPA"]] = length(Duration1)
          output[["TP_IN2LIPA"]] = output[["Nfrag_IN2LIPA"]] = 0
        }
        if (length(inact_2_light_trans) != 0 & length(inact_2_mvpa_trans) == 0) { # only IN2LIPA transitions
          output[["TP_IN2LIPA"]] = 1 / mean(Duration1)
          output[["Nfrag_IN2LIPA"]] = length(Duration1)
          output[["TP_IN2MVPA"]] = output[["Nfrag_IN2MVPA"]] = 0
        }
      }
    }
    rm(y)
  }
  #====================================================
  # Binary fragmentation for the metrics that do not depend on multiple classes
  x = rep(0,length(ACC))
  if (length(LEVELS) == 0) {
    ij_indices = which(ACC >= intensity.thresholds[1] & ACC < intensity.thresholds[2])
    if (length(ij_indices) > 0) x[ij_indices] = 1
  } else {
    x[which(LEVELS %in% class.in.ids)] = 1 # inactivity becomes 1 because this is behaviour of interest
  }
  x = as.integer(x)
  ACCcs = c(0,cumsum(ACC))
  ACCmean = diff(ACCcs[c(1,which(diff(x) != 0)+1,length(ACCcs))]) # mean acceleration per segment
  fragments = rle(x)
  fragments$ACCmean = ACCmean
  fragments$volume = fragments$ACCmean * fragments$length
  Nfragments = length(fragments$lengths) / 2
  output[["Nfrag_PA"]] = output[["Nfrag_IN"]] = 0
  if ("mean" %in% frag.metrics) {
    output[["mean_dur_PA"]] = output[["mean_dur_IN"]] = 0
  }
  if ("Gini" %in% frag.metrics){
    output[["Gini_dur_PA"]] = output[["Gini_dur_IN"]] = NA
  }
  if ("CoV" %in% frag.metrics){ #coefficient of variation
    output[["CoV_dur_PA"]] = output[["CoV_dur_IN"]] = NA
  }
  if ("power" %in% frag.metrics){
    output[["alpha_dur_PA"]] = output[["alpha_dur_IN"]] = NA
    output[["x0.5_dur_PA"]] = output[["x0.5_dur_IN"]] = NA
    output[["W0.5_dur_PA"]] = output[["W0.5_dur_IN"]] = NA
  }
  if (Nfragments > 0) {
    Duration1 = fragments$length[which(fragments$value == 1)]
    Duration0 = fragments$length[which(fragments$value == 0)]
    output[["Nfrag_PA"]] = length(Duration0)
    output[["Nfrag_IN"]] = length(Duration1)
    if ("mean" %in% frag.metrics){
      output[["mean_dur_PA"]] = mean(Duration0)
      output[["mean_dur_IN"]] = mean(Duration1)
    }
    if (Nfragments >= min_Nfragments) {
      if ("Gini" %in% frag.metrics){
        output[["Gini_dur_PA"]] = ineq::Gini(Duration0,corr = T)
        output[["Gini_dur_IN"]] = ineq::Gini(Duration1,corr = T)
      }
      if ("CoV" %in% frag.metrics){ #coefficient of variation as described by Boerema 2020
        output[["CoV_dur_PA"]] = sd(Duration0) / mean(log(Duration0))
        output[["CoV_dur_IN"]] = sd(Duration1) / mean(log(Duration1))
      }
      if ("power" %in% frag.metrics){
        calc_alpha = function(x, xmin) {
          nr = length(x)
          alpha = 1+ nr/sum(log(x/(xmin))) # adapted to match Chastin 2010
          return(alpha)
        }
        output[["alpha_dur_PA"]] = calc_alpha(Duration0, xmin)
        output[["alpha_dur_IN"]] = calc_alpha(Duration1, xmin)
        # From this we can calculate (according to Chastin 2010):
        output[["x0.5_dur_PA"]] = 2^ (1 / (output[["alpha_dur_PA"]]-1) * xmin)
        output[["x0.5_dur_IN"]] = 2^ (1 / (output[["alpha_dur_IN"]]-1) * xmin)
        output[["W0.5_dur_PA"]] = sum(Duration0[which(Duration0 > output[["x0.5_dur_PA"]])]) / sum(Duration0)
        output[["W0.5_dur_IN"]] = sum(Duration1[which(Duration1 > output[["x0.5_dur_IN"]])]) / sum(Duration1)
      }
    }
  }
  return(output)
}
