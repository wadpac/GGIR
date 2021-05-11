g.fragmentation = function(frag.metrics = c("mean", "TP", "Gini", "power",
                                            "CoV", "NFragPM", "all"),
                           LEVELS = c(),
                           Lnames=c(), xmin=1) { 
  
  # This function is loosely inspired by R package ActFrag by Junrui Di.
  # In contrast to R package ActFrag this function assumes
  # that non-wear and missing values have already been taken care of (imputed)
  # outside this function. Further, the algorithms are not all exactly the same,
  # and there are some additional metrics.
  # This function is called from GGIR g.part5 function and applied per waking
  # hours of a day. This avoids the issue of dealing with ppending days,
  # and allows us to test for behavioural differences between days of the week.
  # It is well known that human behaviour can be driven by weekly rhythm. 
  # Knowing fragmentation per day of the week allows us to account for this
  # variation.
  # Further, I am avoiding the term sedentary because sedentary implies
  # that the activity type sitting  was classified, which is generally
  # difficult to justify.
  
    # LEVELS: vector with behavioural classes produced by GGIR
  # Lnames: Names of brehavioural classes.
  # frag.metrics: metric to define fragmentation
  # xmin is shortest recordable (not necessarily observed) boutlength
  
  
  if ("all" %in% frag.metrics) {
    frag.metrics = c("mean", "TP", "Gini", "power",
                     "CoV", "NFragPM", "all")
  }
  output = list()
  if (length(LEVELS) > 0) {
    # convert to class names to numeric class ids for inactive, LIPA and MVPA:
    classes.in = c("day_IN_unbt", Lnames[grep(pattern ="day_IN_bts", x = Lnames)])
    class.in.ids = which(Lnames %in%  classes.in) - 1 
    classes.lig = c("day_LIG_unbt", Lnames[grep(pattern ="day_LIG_bts", x = Lnames)])
    class.lig.ids = which(Lnames %in%  classes.lig) - 1
    classes.mvpa = c("day_MOD_unbt", "day_VIG_unbt", Lnames[grep(pattern ="day_MVPA_bts", x = Lnames)])
    class.mvpa.ids = which(Lnames %in% classes.mvpa) - 1
  }
  Nepochs = length(LEVELS)
  if (Nepochs > 1) { # metrics that require more than just binary
    #====================================================
    # Convert LEVELS in three classes: Inactivity (1), Light = LIPA (2), and MVPA (3)
    y = rep(0,Nepochs)
    y[which(LEVELS %in% class.in.ids)] = 1
    y[which(LEVELS %in% class.lig.ids)] = 2
    y[which(LEVELS %in% class.mvpa.ids)] = 3
    #====================================================
    # TP (transition probability) metrics that depend on multiple classes
    if ("TP" %in% frag.metrics) {
      frag3levels = rle(y) # frag3levels is data.frame with value (intensity) and length (duration of fragment) 
      Nfrag_IN = length(frag3levels$value[which(frag3levels$value == 1)])
      output[["TP_IN2PA"]] = output[["TP_PA2IN"]] = 0
      output[["TP_IN2LIPA"]] = output[["Nfrag_IN2LIPA"]] = 0
      output[["TP_IN2MVPA"]] = output[["Nfrag_IN2MVPA"]] = 0
      DurationLIPA = frag3levels$length[which(frag3levels$value == 2)] # all LIPA fragments
      DurationMVPA = frag3levels$length[which(frag3levels$value == 3)] # all MVPA fragments
      Nfrag_LIPA = length(DurationLIPA)
      Nfrag_MVPA = length(DurationMVPA)
      if (Nfrag_LIPA > 0) {
        output[["Nfrag_LIPA"]] = Nfrag_LIPA
        output[["mean_dur_LIPA"]] = mean(DurationLIPA)
      } else {
        output[["mean_dur_LIPA"]] = output[["Nfrag_LIPA"]] = 0
      }
      if (Nfrag_MVPA > 0) {
        output[["Nfrag_MVPA"]] = Nfrag_MVPA
        output[["mean_dur_MVPA"]] = mean(DurationMVPA)
      } else {
        output[["mean_dur_MVPA"]] = output[["Nfrag_MVPA"]] = 0
      }
      if (Nfrag_IN > 0 & (Nfrag_LIPA > 0 | Nfrag_MVPA > 0)) {
        DurationPA = frag3levels$length[which(frag3levels$value != 1)]
        DurationIN = frag3levels$length[which(frag3levels$value == 1)]
        Nfrag3levels = length(frag3levels$value)
        inact_2_light_trans = which(frag3levels$value[1:(Nfrag3levels-1)] == 1 & frag3levels$value[2:Nfrag3levels] == 2)
        inact_2_mvpa_trans = which(frag3levels$value[1:(Nfrag3levels-1)] == 1 & frag3levels$value[2:Nfrag3levels] == 3)
        if (length(inact_2_light_trans) > 0) { # transitions from inactive to LIPA
          DurationIN2LIPA = frag3levels$length[inact_2_light_trans]
          output[["TP_IN2LIPA"]] = (sum(DurationIN2LIPA)/sum(DurationIN)) / mean(DurationIN)
          output[["Nfrag_IN2LIPA"]] = length(DurationIN2LIPA)
        } 
        if (length(inact_2_mvpa_trans) > 0) { # transitions from inactive to MVPA
          DurationIN2MVPA = frag3levels$length[inact_2_mvpa_trans]
          output[["TP_IN2MVPA"]] = (sum(DurationIN2MVPA)/sum(DurationIN)) / mean(DurationIN)
          output[["Nfrag_IN2MVPA"]] = length(DurationIN2MVPA)
        }
        output[["TP_IN2PA"]] = 1 / mean(DurationIN)
        output[["TP_PA2IN"]] = 1 / mean(DurationPA)
        if (length(inact_2_light_trans) == 0 & length(inact_2_mvpa_trans) != 0) {
          output[["TP_IN2MVPA"]] = 1 / mean(DurationIN)
          output[["Nfrag_IN2MVPA"]] = length(DurationIN)
          output[["TP_IN2LIPA"]] = output[["Nfrag_IN2LIPA"]] = 0
        }
        if (length(inact_2_light_trans) != 0 & length(inact_2_mvpa_trans) == 0) {
          output[["TP_IN2LIPA"]] = 1 / mean(DurationIN)
          output[["Nfrag_IN2LIPA"]] = length(DurationIN)
          output[["TP_IN2MVPA"]] = output[["Nfrag_IN2MVPA"]] = 0
        }
      }
    }
    rm(y)
  }
  #====================================================
  # Binary fragmentation for the metrics that do not depend on multiple classes
  x = rep(0,Nepochs)
  x[which(LEVELS %in% class.in.ids)] = 1 # inactivity becomes 1 because this is behaviour of interest
  x = as.integer(x)
  frag2levels = rle(x)
  Nfrag2levels = length(frag2levels$lengths)
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
  if ("power" %in% frag.metrics | "CoV" %in% frag.metrics | "Gini" %in% frag.metrics) {
    output[["SD_dur_PA"]] = output[["SD_dur_IN"]] = NA
  }
  DurationIN = frag2levels$length[which(frag2levels$value == 1)]
  DurationPA = frag2levels$length[which(frag2levels$value == 0)]
  output[["Nfrag_PA"]] = length(DurationPA)
  output[["Nfrag_IN"]] = length(DurationIN)
  if (Nfrag2levels > 1) {
      if ("mean" %in% frag.metrics){
      output[["mean_dur_PA"]] = mean(DurationPA)
      output[["mean_dur_IN"]] = mean(DurationIN)
    }
    if ("NFragPM" %in% frag.metrics){
      # Identify to metric named fragmentation by Chastin,
      # but renamed into Number of Fragments Per Minutes to be 
      # a better reflection of the calculation
      output[["NFragPM_PA"]] = output[["Nfrag_PA"]] / sum(DurationPA)
      output[["NFragPM_IN"]] = output[["Nfrag_IN"]] / sum(DurationIN)
    }
    # minimum number of required fragments (10 here this is the sum of the number
    # of PA and IN fragments, so basically we allow for 5 PA and 5 IN fragments)
    # because the metrics below are less informative with a few metrics
    if (Nfrag2levels >= 10) { 
      SD0 = sd(DurationPA)
      SD1 = sd(DurationIN)
      output[["SD_dur_PA"]] = SD0 # maybe not a fragmentation metric, but helpful to understand other metrics
      output[["SD_dur_IN"]] = SD1
      if ("Gini" %in% frag.metrics){
        output[["Gini_dur_PA"]] = ineq::Gini(DurationPA,corr = T)
        output[["Gini_dur_IN"]] = ineq::Gini(DurationIN,corr = T)
      }
      if ("CoV" %in% frag.metrics){ #coefficient of variation as described by Blikman 2015
        output[["CoV_dur_PA"]] = sd(DurationPA) / mean(log(DurationPA))
        output[["CoV_dur_IN"]] = sd(DurationIN) / mean(log(DurationIN))
      }
      if ("power" %in% frag.metrics){
        calc_alpha = function(x, xmin) {
          nr = length(x)
          alpha = 1+ nr/sum(log(x/(xmin))) # adapted to match Chastin 2010 instead of ActFrag
          return(alpha)
        }
        if (SD0 != 0) {
          output[["alpha_dur_PA"]] = calc_alpha(DurationPA, xmin)
          output[["x0.5_dur_PA"]] = 2^ (1 / (output[["alpha_dur_PA"]]-1) * xmin) # according to Chastin 2010
          output[["W0.5_dur_PA"]] = sum(DurationPA[which(DurationPA > output[["x0.5_dur_PA"]])]) / sum(DurationPA)
        }
        if (SD1 != 0) {
          output[["alpha_dur_IN"]] = calc_alpha(DurationIN, xmin)
          output[["x0.5_dur_IN"]] = 2^ (1 / (output[["alpha_dur_IN"]]-1) * xmin) # according to Chastin 2010
          output[["W0.5_dur_IN"]] = sum(DurationIN[which(DurationIN > output[["x0.5_dur_IN"]])]) / sum(DurationIN)
        }
      }
    }
  }
  return(output)
}
