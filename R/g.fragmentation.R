g.fragmentation = function(frag.metrics = c("mean", "TP", "Gini", "power",
                                            "CoV", "NFragPM", "all"),
                           LEVELS = c(),
                           Lnames=c(), xmin=1,
                           mode = "day") { 
  # This function was originally loosely inspired by R package ActFrag by Junrui Di.
  # In contrast to R package ActFrag this function assumes
  # that non-wear and missing values have already been taken care of (imputed)
  # outside this function or set to NA. Further, the algorithms are not all exactly the same,
  # and there are some additional metrics. For further dicussion see
  # https://wadpac.github.io/GGIR/articles/chapter11_DescribingDataCutPoints.html
  # Further, I am avoiding the term sedentary because sedentary implies
  # that the activity type sitting  was classified, which is generally
  # difficult to justify.
  
  # LEVELS: vector with behavioural classes produced by GGIR
  # Lnames: Names of brehavioural classes.
  # frag.metrics: metric to define fragmentation
  # xmin is shortest recordable (not necessarily observed) boutlength
  
  TransProb = function(x, a = 1, b = c(2,3)) {
    TPab = TPba = NA
    Nab = Nba = 0
    totDur_ab = totDur_ba = 0
    if (length(x) > 0) {
      frag = rle(x)
      Nsegments = length(frag$value)
      if (Nsegments > 1) {
        # Scenario where we have more than 1 segment
        ab = which(frag$value[1:(Nsegments - 1)] %in% a &
                     frag$value[2:Nsegments] %in% b)
        ba = which(frag$value[1:(Nsegments - 1)] %in% b &
                     frag$value[2:Nsegments] %in% a)
        Nab = length(ab)
        Nba = length(ba)
        
        # In GGIR part 6 we set day or spt to NA, by which the series is split in parts
        # separate by strings of NA values. In GGIR part 5 this would not happen
        # and we only need the last item of the time series for that day. However,
        # if last value is NA then ignore it.
        lastitems = which(is.na(frag$value[1:(Nsegments - 1)]) == FALSE &
                                      is.na(frag$value[2:Nsegments] == TRUE))
        if (!is.na(frag$value[Nsegments])) {
          lastitems = unique(c(lastitems, Nsegments))
        }
        tmp = frag$value[-lastitems]
        # Next use this to detect how often last value of each string is reference class (a or b)
        if (length(lastitems) > 0 & length(which(is.na(tmp) == FALSE)) > 1) {
          count_a_ending = sum(ifelse(frag$value[lastitems] %in% a, 1, 0))
          count_b_ending = sum(ifelse(frag$value[lastitems] %in% b, 1, 0))
        } else {
          count_a_ending = 0
          count_b_ending = 0
        }
        # Total duration from a to b
        totDur_ab = sum(frag$length[which(frag$value %in% a)]) - count_a_ending
        totDur_ba = sum(frag$length[which(frag$value %in% b)]) - count_b_ending
        epsilon = 1e-6
        TPab = (Nab + epsilon) / (totDur_ab + epsilon)
        TPba = (Nba + epsilon) / (totDur_ba + epsilon)
        # Round to 6 digits because preceding step introduces bias at 7 decimal places
        TPab = round(TPab, digits = 6)
        TPba = round(TPba, digits = 6)
        durations_ab = frag$length[ab]
        durations_ba = frag$length[ba]
        totDur_ab = sum(durations_ab)
        totDur_ba = sum(durations_ba)
      } else if (Nsegments == 1) {
        # Scenario where we have only one segment of data
        if (frag$value[1] %in% a) { # Only a
          TPab = 0
          TPba = 1
          totDur_ab = frag$length[1]
          totDur_ba = 0
          Nab = 1
          Nba = 0
        } else { # Only b
          TPab = 1
          TPba = 0
          totDur_ab = 0
          totDur_ba = frag$length[1]
          Nab = 0
          Nba = 1
        }
      }
    }
    invisible(list(TPab = TPab,
                   TPba = TPba,
                   Nab = Nab, Nba = Nba,
                   totDur_ab = totDur_ab,
                   totDur_ba = totDur_ba))
  }
  
  if ("all" %in% frag.metrics) {
    frag.metrics = c("mean", "TP", "Gini", "power",
                     "CoV", "NFragPM", "all")
  }
  output = list()
  Nepochs = length(LEVELS)
  if (mode == "day") {
    # convert to class names to numeric class ids for inactive, LIPA and MVPA:
    classes.in = c("day_IN_unbt", Lnames[grep(pattern = "day_IN_bts", x = Lnames)])
    class.in.ids = which(Lnames %in%  classes.in) - 1 
    classes.lig = c("day_LIG_unbt", Lnames[grep(pattern = "day_LIG_bts", x = Lnames)])
    class.lig.ids = which(Lnames %in%  classes.lig) - 1
    classes.mvpa = c("day_MOD_unbt", "day_VIG_unbt", Lnames[grep(pattern = "day_MVPA_bts", x = Lnames)])
    class.mvpa.ids = which(Lnames %in% classes.mvpa) - 1
  } else {
    do.frag = FALSE
  }
  if (Nepochs > 1 & mode == "day") { # metrics that require more than just binary
    #====================================================
    # Convert LEVELS in three classes: Inactivity (1), Light = LIPA (2), and MVPA (3)
    y = rep(0, Nepochs)
    is.na(y[is.na(LEVELS)]) = TRUE
    y[which(LEVELS %in% class.in.ids)] = 1
    y[which(LEVELS %in% class.lig.ids)] = 2
    y[which(LEVELS %in% class.mvpa.ids)] = 3
    #====================================================
    # TP (transition probability) metrics that depend on multiple classes
    if ("TP" %in% frag.metrics) {
      out = TransProb(y, a = 1, b = c(2, 3)) #IN <-> PA
      output[["TP_IN2PA"]] = out$TPab
      output[["TP_PA2IN"]] = out$TPba
      output[["Nfrag_IN2PA"]] = out$Nab
      output[["Nfrag_PA2IN"]] = out$Nba
      
      out = TransProb(y, a = 1, b = 2) #IN to LIPA
      output[["TP_IN2LIPA"]] = out$TPab
      output[["Nfrag_IN2LIPA"]] = out$Nab

      out = TransProb(y, a = 1, b = 3) #IN to MVPA
      output[["TP_IN2MVPA"]] = out$TPab
      output[["Nfrag_IN2MVPA"]] = out$Nab

      out = TransProb(y, a = 2, b = c(1, 3)) #LIPA <-> rest
      output[["Nfrag_LIPA"]] = out$Nab
      output[["mean_dur_LIPA"]] = out$totDur_ab / out$Nab

      out = TransProb(y, a = 3, b = c(1, 2)) #MVPA <-> rest
      output[["Nfrag_MVPA"]] = out$Nab
      output[["mean_dur_MVPA"]] = out$totDur_ab / out$Nab
    }
  }
  #====================================================
  # Binary fragmentation for the metrics that do not depend on multiple classes
  
  if (mode == "day") {
    x = rep(0,Nepochs)
    is.na(x[is.na(LEVELS)]) = TRUE
    x[which(LEVELS %in% class.in.ids)] = 1 # inactivity becomes 1 because this is behaviour of interest
    x = as.integer(x)
    frag2levels = rle(x)
    Nfrag2levels = length(which(is.na(frag2levels$values) == FALSE))
    out = TransProb(x, a = 1, b = 0) #IN <-> PA
    output[["Nfrag_PA"]] = out$Nba
    output[["Nfrag_IN"]] = out$Nab
    
    # Define default values
    if ("mean" %in% frag.metrics) {
      output[["mean_dur_PA"]] = output[["mean_dur_IN"]] = 0
    }
    if ("Gini" %in% frag.metrics) {
      output[["Gini_dur_PA"]] = output[["Gini_dur_IN"]] = NA
    }
    if ("CoV" %in% frag.metrics) { #coefficient of variation
      output[["CoV_dur_PA"]] = output[["CoV_dur_IN"]] = NA
    }
    if ("power" %in% frag.metrics) {
      output[["alpha_dur_PA"]] = output[["alpha_dur_IN"]] = NA
      output[["x0.5_dur_PA"]] = output[["x0.5_dur_IN"]] = NA
      output[["W0.5_dur_PA"]] = output[["W0.5_dur_IN"]] = NA
    }
    if ("power" %in% frag.metrics | "CoV" %in% frag.metrics | "Gini" %in% frag.metrics) {
      output[["SD_dur_PA"]] = output[["SD_dur_IN"]] = NA
    }
    if ("NFragPM" %in% frag.metrics) {
      output[["NFragPM_PA"]] = 0
      output[["NFragPM_IN"]] = 0
    }
    if (Nfrag2levels > 1) {
      if ("mean" %in% frag.metrics) {
        output[["mean_dur_PA"]] = out$totDur_ba / out$Nba
        output[["mean_dur_IN"]] = out$totDur_ab / out$Nab
      }
      if ("NFragPM" %in% frag.metrics) {
        # Identify to metric named fragmentation by Chastin,
        # but renamed into Number of Fragments Per Minutes to be 
        # a better reflection of the calculation
        output[["NFragPM_PA"]] = output[["Nfrag_PA"]] / out$totDur_ba
        output[["NFragPM_IN"]] = output[["Nfrag_IN"]] / out$totDur_ab
      }
      # minimum number of required fragments (10 here this is the sum of the number
      # of PA and IN fragments, so basically we allow for 5 PA and 5 IN fragments)
      # because the metrics below are less informative with a few metrics
      if (Nfrag2levels >= 10) {
        DurationIN = frag2levels$length[which(frag2levels$value == 1)]
        DurationPA = frag2levels$length[which(frag2levels$value == 0)]
        SD0 = sd(DurationPA, na.rm = TRUE)
        SD1 = sd(DurationIN, na.rm = TRUE)
        output[["SD_dur_PA"]] = SD0 # maybe not a fragmentation metric, but helpful to understand other metrics
        output[["SD_dur_IN"]] = SD1
        if ("Gini" %in% frag.metrics) {
          output[["Gini_dur_PA"]] = ineq::Gini(DurationPA,corr = T)
          output[["Gini_dur_IN"]] = ineq::Gini(DurationIN,corr = T)
        }
        if ("CoV" %in% frag.metrics) { #coefficient of variation as described by Blikman 2015
          output[["CoV_dur_PA"]] = sd(log(DurationPA)) / mean(log(DurationPA))
          output[["CoV_dur_IN"]] = sd(log(DurationIN)) / mean(log(DurationIN))
        }
        if ("power" %in% frag.metrics) {
          calc_alpha = function(x, xmin) {
            nr = length(x)
            alpha = 1 + nr/sum(log(x/(xmin))) # adapted to match Chastin 2010 instead of ActFrag
            return(alpha)
          }
          if (SD0 != 0) {
            output[["alpha_dur_PA"]] = calc_alpha(DurationPA, xmin)
            output[["x0.5_dur_PA"]] = 2^(1 / (output[["alpha_dur_PA"]] - 1) * xmin) # according to Chastin 2010
            output[["W0.5_dur_PA"]] = sum(DurationPA[which(DurationPA > output[["x0.5_dur_PA"]])]) / sum(DurationPA)
          }
          if (SD1 != 0) {
            output[["alpha_dur_IN"]] = calc_alpha(DurationIN, xmin)
            output[["x0.5_dur_IN"]] = 2^(1 / (output[["alpha_dur_IN"]] - 1) * xmin) # according to Chastin 2010
            output[["W0.5_dur_IN"]] = sum(DurationIN[which(DurationIN > output[["x0.5_dur_IN"]])]) / sum(DurationIN)
          }
        }
      }
    }
  } else if (mode == "spt") {
    # Binary fragmentation metrics for spt:
    # Active - Rest transitions during SPT:
    x = rep(0, Nepochs)
    is.na(x[is.na(LEVELS)]) = TRUE
    # convert to class names to numeric class ids for inactive, LIPA and MVPA:
    classes.pa = c("spt_wake_LIG", "spt_wake_MOD", "spt_wake_VIG")
    class.pa = which(Lnames %in% classes.pa) - 1
    PAi = which(LEVELS %in% class.pa)
    if (length(PAi) > 0) {
      x[PAi] = 1
    }
    out = TransProb(x = x, a = 0, b = 1)
    output[["Nfrag_IN"]] = out$Nab
    output[["Nfrag_PA"]] = out$Nba
    output[["TP_IN2PA"]] = out$TPab
    output[["TP_PA2IN"]] = out$TPba
    # Wake - Sleep transitions during SPT:
    x = rep(0, Nepochs)
    is.na(x[is.na(LEVELS)]) = TRUE
    classes.wake = c("spt_wake_IN", "spt_wake_LIG", "spt_wake_MOD", "spt_wake_VIG")
    class.wake = which(Lnames %in% classes.wake) - 1 
    wakei = which(LEVELS %in% class.wake)
    if (length(wakei) > 0) {
      x[wakei] = 1
    }
    out = TransProb(x = x, a = 0, b = 1)
    output[["Nfrag_sleep"]] = out$Nab
    output[["Nfrag_wake"]] = out$Nba
    output[["TP_sleep2wake"]] = out$TPab
    output[["TP_wake2sleep"]] = out$TPba
  }
  return(output)
}
