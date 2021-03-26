g.fragmentation = function(frag.metrics = c("mean", "TP", "Gini", "power",
                                            "CoV", "all"),
                           ACC = c(), intensity.thresholds = c(), do.multiclass=c(), LEVELS = c(),
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
  min_Nfragments_power = min_Nfragments = 10 # minimum number of required fragments
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
  if (length(ACC) > 1 & do.multiclass == TRUE) { # metrics that require more than just binary
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
      Nfrag3 = length(fragments3$value)
      output[["IN2PA_TP"]] = output[["PA2IN_TP"]] = NA
      output[["IN2LIPA_TP"]] = output[["Nfragments_IN2LIPA"]] = NA
      output[["IN2MVPA_TP"]] = output[["Nfragments_IN2MVPA"]] = NA
      output[["Nfragments_LIPA"]] = output[["Nfragments_MVPA"]] = NA
      output[["mean_dur_LIPA"]] = output[["mean_dur_MVPA"]] = NA
      if (Nfrag3 >= min_Nfragments) {
        Duration0 = fragments3$length[which(fragments3$value[1:(Nfrag3-1)] != 1)] # all activity fragments
        Duration1 = fragments3$length[which(fragments3$value[1:(Nfrag3-1)] == 1)] # all inactivity fragments
        DurationLIPA = fragments3$length[which(fragments3$value[1:(Nfrag3-1)] == 2)] # all light fragments
        DurationMVPA = fragments3$length[which(fragments3$value[1:(Nfrag3-1)] == 3)] # all MVPA fragments
        output[["Nfragments_LIPA"]] = length(DurationLIPA)
        output[["Nfragments_MVPA"]] = length(DurationMVPA)
        output[["mean_dur_LIPA"]] = mean(DurationLIPA)
        output[["mean_dur_MVPA"]] = mean(DurationMVPA)
        # Get only indices of inactive fragments that transition to light:
        inact_2_light_trans = which(fragments3$value[1:(Nfrag3-1)] == 1 & fragments3$value[2:Nfrag3] == 2)
        # Get only indices of inactive fragments that transition to mvpa:
        inact_2_mvpa_trans = which(fragments3$value[1:(Nfrag3-1)] == 1 & fragments3$value[2:Nfrag3] == 3)
        if (length(inact_2_light_trans) >=  1) {
          Duration2 = fragments3$length[inact_2_light_trans] # durations of all inactivity fragments followed by light.
          output[["IN2LIPA_TP"]] = (sum(Duration2)/sum(Duration1)) / mean(Duration1) # transition from inactive to mvpa
          output[["Nfragments_IN2LIPA"]] = length(Duration2)
        } 
        if (length(inact_2_mvpa_trans) >= 1) {
          Duration3 = fragments3$length[inact_2_mvpa_trans] # durations of all inactivity fragments followed by light.
          output[["IN2MVPA_TP"]] = (sum(Duration3)/sum(Duration1)) / mean(Duration1) # transition from inactive to mvpa
          output[["Nfragments_IN2MVPA"]] = length(Duration3)
        } 
        output[["IN2PA_TP"]] = 1 / mean(Duration1) # transition from inactive to active (no distinction light or MVPA)
        output[["PA2IN_TP"]] = 1 / mean(Duration0) # transition from active to inactive (no distinction light or MVPA)
        if (length(inact_2_light_trans) == 0 & length(inact_2_mvpa_trans) != 0) { # only IN2mvpa transitions
          output[["IN2MVPA_TP"]] = 1 / mean(Duration1)
          output[["Nfragments_IN2MVPA"]] = length(Duration1)
          output[["IN2LIPA_TP"]] = output[["Nfragments_IN2LIPA"]] = 0
        }
        if (length(inact_2_light_trans) != 0 & length(inact_2_mvpa_trans) == 0) { # only IN2LIPA transitions
          output[["IN2LIPA_TP"]] = 1 / mean(Duration1)
          output[["Nfragments_IN2LIPA"]] = length(Duration1)
          output[["IN2MVPA_TP"]] = output[["Nfragments_IN2MVPA"]] = 0
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
  # - 1 = behaviour of interest defined by frag.classes.day/frag.classes.spt in g.part5
  # - 0 = all remaining time, which we may consider breaks in our behaviour of interest
  fragments = rle(x)
  fragments$ACCmean = ACCmean
  fragments$volume = fragments$ACCmean * fragments$length
  Nfragments = length(fragments$lengths) / 2
  if (Nfragments >= min_Nfragments) {
    Acc1 = fragments$ACCmean[which(fragments$value == 1)]
    Acc0 = fragments$ACCmean[which(fragments$value == 0)]
    Volume1 = fragments$volume[which(fragments$value == 1)]
    Volume0 = fragments$volume[which(fragments$value == 0)]
    Duration1 = fragments$length[which(fragments$value == 1)]
    Duration0 = fragments$length[which(fragments$value == 0)]
    output[["Nfragments_0"]] = length(Duration0)
    output[["Nfragments_1"]] = length(Duration1)
    # mean
    if ("mean" %in% frag.metrics){
      output[["mean_acc_0"]] = mean(Acc0)
      output[["mean_acc_1"]] = mean(Acc1)
      output[["mean_vol_0"]] = mean(Volume0)
      output[["mean_vol_1"]] = mean(Volume1)
      output[["mean_dur_0"]] = mean(Duration0)
      output[["mean_dur_1"]] = mean(Duration1)
    }
    # Gini
    if ("Gini" %in% frag.metrics){
      output[["Gini_dur_0"]] = ineq::Gini(Duration0,corr = T)
      output[["Gini_dur_1"]] = ineq::Gini(Duration1,corr = T)
      output[["Gini_acc_0"]] = ineq::Gini(Acc0,corr = T)
      output[["Gini_acc_1"]] = ineq::Gini(Acc1,corr = T)
      output[["Gini_vol_0"]] = ineq::Gini(Volume0,corr = T)
      output[["Gini_vol_1"]] = ineq::Gini(Volume1,corr = T)
    }
    if ("TP" %in% frag.metrics){
      output[["TP01_acc"]] = 1/mean(Acc0) # transition towards behaviour of interest
      output[["TP10_acc"]] = 1/mean(Acc1) # transition away from behaviour of interest
      output[["TP01_vol"]] = 1/mean(Volume0) # transition towards behaviour of interest
      output[["TP10_vol"]] = 1/mean(Volume1) # transition away from behaviour of interest
    }
    if ("CoV" %in% frag.metrics){ #coefficient of variation as described by Boerema 2020
      output[["CoV_dur_0"]] = sd(Duration0) / mean(log(Duration0))
      output[["CoV_dur_1"]] = sd(Duration1) / mean(log(Duration1))
      output[["CoV_acc_0"]] = sd(Acc0) / mean(log(Acc0))
      output[["CoV_acc_1"]] = sd(Acc1) / mean(log(Acc1))
      output[["CoV_vol_0"]] = sd(Volume0) / mean(log(Volume0))
      output[["CoV_vol_1"]] = sd(Volume1) / mean(log(Volume1))
    }
    
    if ("power" %in% frag.metrics & Nfragments >= min_Nfragments_power){
      calc_alpha = function(x, xmin) {
        nr = length(x)
        # xmin = min(x)
        alpha = 1+ nr/sum(log(x/(xmin))) # adapted to match Chastin 2010
        return(alpha)
      }
      output[["alpha_dur_0"]] = calc_alpha(Duration0, xmin)
      output[["alpha_dur_1"]] = calc_alpha(Duration1, xmin)
      # From this we can calculate (according to Chastin 2010):
      output[["x0.5_dur_0"]] = 2^ (1 / (output[["alpha_dur_0"]]-1) * xmin)
      output[["x0.5_dur_1"]] = 2^ (1 / (output[["alpha_dur_1"]]-1) * xmin)
      output[["W0.5_dur_0"]] = sum(Duration0[which(Duration0 > output[["x0.5_dur_0"]])]) / sum(Duration0)
      output[["W0.5_dur_1"]] = sum(Duration1[which(Duration1 > output[["x0.5_dur_1"]])]) / sum(Duration1)
    }
  } else {
    # Create empty variables:
    output[["Nfragments_0"]] = NA
    output[["Nfragments_1"]] = NA
    if ("mean" %in% frag.metrics){
      output[["mean_acc_0"]] = output[["mean_acc_1"]] = output[["mean_vol_0"]] = NA
      output[["mean_vol_1"]] = output[["mean_dur_0"]] = output[["mean_dur_1"]] = NA
    }
    if ("Gini" %in% frag.metrics){
      output[["Gini_dur_0"]] = output[["Gini_dur_1"]] = output[["Gini_acc_0"]] = NA
      output[["Gini_acc_1"]] = output[["Gini_vol_0"]] = output[["Gini_vol_1"]] = NA
    }
    if ("TP" %in% frag.metrics){
      output[["TP01_acc"]] = output[["TP10_acc"]] = output[["TP01_vol"]] = output[["TP10_vol"]] = NA
    }
    if ("CoV" %in% frag.metrics){ #coefficient of variation
      output[["CoV_dur_0"]] = output[["CoV_dur_1"]] = output[["CoV_acc_0"]] = NA
      output[["CoV_acc_1"]] = output[["CoV_vol_0"]] = output[["CoV_vol_1"]] = NA
    }
    if ("power" %in% frag.metrics){
      output[["alpha_dur_0"]] = output[["alpha_dur_1"]] = output[["x0.5_dur_0"]] = NA
      output[["x0.5_dur_1"]] = output[["W0.5_dur_0"]] = output[["W0.5_dur_1"]] = NA
    }
  }
  return(output)
}
