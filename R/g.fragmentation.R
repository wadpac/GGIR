g.fragmentation = function(frag.metrics = c("mean", "TP", "Gini", "power",
                                            "CoV", "all"),
                           # "dfa", "InfEn", "SampEn", "ApEn", "RQA",
                           ACC = c(), intensity.thresholds = c(), do.multiclass=c(), LEVELS = c(),
                           Lnames=c(), xmin=1) { #sleepclass=c()
  
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
  min_Nfragments_TP_only = 1
  intensity.thresholds = c(0, intensity.thresholds) 
  output = list()
  if (length(LEVELS) > 0) {
    classes.in = c("day_IN_unbt", Lnames[grep(pattern ="day_IN_bts", x = Lnames)])
    class.in.ids = which(Lnames %in%  classes.in) - 1 # convert to numberic class id
    
    classes.lig = c("day_LIG_unbt", Lnames[grep(pattern ="day_LIG_bts", x = Lnames)])
    class.lig.ids = which(Lnames %in%  classes.lig) - 1 # convert to numberic class id
    
    classes.mvpa = c("day_MOD_unbt", "day_VIG_unbt", Lnames[grep(pattern ="day_MVPA_bts", x = Lnames)])
    class.mvpa.ids = which(Lnames %in% classes.mvpa) - 1 # convert to numberic class id
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
      Duration1 = fragments3$length[which(fragments3$value[1:(Nfrag3-1)] == 1)] # all inactivity fragments
      # Get only indices of inactive fragments that transition to light:
      inact_2_light_trans = which(fragments3$value[1:(Nfrag3-1)] == 1 & fragments3$value[2:Nfrag3] == 2)
      # Get only indices of inactive fragments that transition to mvpa:
      inact_2_mvpa_trans = which(fragments3$value[1:(Nfrag3-1)] == 1 & fragments3$value[2:Nfrag3] == 3)
      # Duration1 = Duration1[1:(length(Duration1)-1)] # shorten by 1 do match length of Duration 2 and 3
      # initialise variables:
      output[["IN2PA_TP"]] = NA
      output[["IN2LIPA_TPsum"]] = output[["IN2LIPA_TPlen"]] = output[["Nfragments_IN2LIPA"]] = NA
      output[["IN2MVPA_TPsum"]] = output[["IN2MVPA_TPlen"]] = output[["Nfragments_IN2MVPA"]] = NA
      # only calculate this if there are enough fragments for both transitions
      if (length(inact_2_light_trans) >=  min_Nfragments_TP_only & 
          length(inact_2_mvpa_trans) >= min_Nfragments_TP_only) {
        Duration2 = fragments3$length[inact_2_light_trans] # durations of all inactivity fragments followed by light.
        output[["IN2LIPA_TP"]] = (sum(Duration2)/sum(Duration1)) / mean(Duration1) # transition from inactive to mvpa
        # output[["IN2LIPA_TPlen"]] = (length(Duration2)/length(Duration1)) / mean(Duration1) # transition from inactive to mvpa
        output[["Nfragments_IN2LIPA"]] = length(Duration2)
    
        Duration3 = fragments3$length[inact_2_mvpa_trans] # durations of all inactivity fragments followed by light.
        output[["IN2MVPA_TP"]] = (sum(Duration3)/sum(Duration1)) / mean(Duration1) # transition from inactive to mvpa
        # output[["IN2MVPA_TPlen"]] = (length(Duration3)/length(Duration1)) / mean(Duration1) # transition from inactive to mvpa
        output[["Nfragments_IN2MVPA"]] = length(Duration3)
      }
      if (length(inact_2_light_trans) >= min_Nfragments_TP_only & length(inact_2_mvpa_trans) >= min_Nfragments_TP_only) {
        output[["IN2PA_TP"]] = 1 / mean(Duration1) # transition from inactive to mvpa
      }
    }
    rm(y)
  }
  #====================================================
  # Binary fragmentation
  x = rep(0,length(ACC))
  if (length(LEVELS) == 0) {
    ij_indices = which(ACC >= intensity.thresholds[1] & ACC < intensity.thresholds[2])
    if (length(ij_indices) > 0) x[ij_indices] = 1
  } else {
    x[which(LEVELS %in% class.in.ids)] = 1 # inactivity becomes 1 because this is behaviour of interest
  }
  x = as.integer(x)
  # try-out dummy example:
  # A = 1:20
  # D = c(rep(0,4),rep(1,4),rep(0,4),rep(1,4),rep(0,4))
  # rle(D)
  ACCcs = c(0,cumsum(ACC))
  ACCmean = diff(ACCcs[c(1,which(diff(x) != 0)+1,length(ACCcs))]) # mean acceleration per segment
  
  # - 1 = behaviour of interest defined by frag.classes.day/frag.classes.spt in g.part
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
      output[["B01_TP_acc"]] = 1/mean(Acc0) # transition towards behaviour of interest
      output[["B10_TP_acc"]] = 1/mean(Acc1) # transition away from behaviour of interest
      output[["B01_TP_vol"]] = 1/mean(Volume0) # transition towards behaviour of interest
      output[["B10_TP_vol"]] = 1/mean(Volume1) # transition away from behaviour of interest
      output[["B01_TP_dur"]] = 1/mean(Duration0) # transition towards behaviour of interest
      output[["B10_TP_dur"]] = 1/mean(Duration1) # transition away from behaviour of interest
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
      calc_alpha = function(x) {
        nr = length(x)
        rmin = min(x)
        alpha = 1+ nr/sum(log(x/(rmin))) # adapted to match Chastin 2010
        return(alpha)
      }
      output[["alpha_dur_0"]] = calc_alpha(Duration0)
      output[["alpha_dur_1"]] = calc_alpha(Duration1)
      # From this we can calculate (according to Chastin 2010):
      output[["x0.5_dur_0"]] = 2^ (1 / (output[["alpha_dur_0"]]-1) * xmin)
      output[["x0.5_dur_1"]] = 2^ (1 / (output[["alpha_dur_1"]]-1) * xmin)
      output[["W0.5_dur_0"]] = sum(Duration0[which(Duration0 > output[["x0.5_dur_0"]])]) / sum(Duration0)
      output[["W0.5_dur_1"]] = sum(Duration1[which(Duration1 > output[["x0.5_dur_1"]])]) / sum(Duration1)
    }
    # if ("hazard" %in% frag.metrics){
    #   fitr = survival::survfit(survival::Surv(Duration0,rep(1,length(Duration0)))~1)
    #   fita = survival::survfit(survival::Surv(Duration1,rep(1,length(Duration1)))~1)
    #   output[["h_dur_0"]] =  mean(fitr$n.event/fitr$n.risk)
    #   output[["h_dur_1"]] = mean(fita$n.event/fita$n.risk)
    # }
  }
  return(output)
}
