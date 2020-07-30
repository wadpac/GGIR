g.fragmentation = function(frag.metrics = c("mean_bout", "TP", "Gini", "power", "hazard", "h", "CoV",
                                            "dfa", "InfEn",
                                            "SampEn", "ApEn", "RQA", "all"),
                           ACC = c(), intensity.thresholds = c(), do.multiclass=c(), LEVELS = c(),
                           Lnames=c()) { #sleepclass=c()
  
  # This function is based on code from R package ActFrag as developed by Junrui Di.
  # Dependencies: ineq and survival package for Gini and Hazard metric respecitvely.
  #
  # frag.metrics: metric to define fragmentation
  # in contract to R package ActFrag this function assumes
  # that non-wear and missing values have already been taken care of outside this
  # function.
  
  if ("all" %in% frag.metrics) {
    frag.metrics = c("mean_bout","TP","Gini","power","hazard", "h", "CoV",
                     "dfa", "InfEn","SampEn","ApEn")
  }
  min_Nfragments = 10 # minimum number of required fragments
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
    # Approximate Entropy
    if ("ApEn" %in% frag.metrics) {
      FastApEn = c()
      try(expr = {FastApEn = TSEntropies::FastApEn(TS = ACC, dim = 2, lag = 1, r = 0.15 * sd(ACC))})
      if (length(FastApEn) == 0) {
        try(expr = {FastApEn = TSEntropies::ApEn(TS = ACC, dim = 2, lag = 1, r = 0.15 * sd(ACC))})
      }
      output[["FastApEn_contin"]] = FastApEn
    }
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
      State1 = fragments3$length[which(fragments3$value[1:(Nfrag3-1)] == 1)] # all inactivity fragments
      # Get only indices of inactive fragments that transition to light:
      inact_2_light_trans = which(fragments3$value[1:(Nfrag3-1)] == 1 & fragments3$value[2:Nfrag3] == 2)
      # Get only indices of inactive fragments that transition to mvpa:
      inact_2_mvpa_trans = which(fragments3$value[1:(Nfrag3-1)] == 1 & fragments3$value[2:Nfrag3] == 3)
      # State1 = State1[1:(length(State1)-1)] # shorten by 1 do match length of State 2 and 3
      # initialise variables:
      output[["IN2PA_TP"]] = NA
      output[["IN2LIPA_TPsum"]] = output[["IN2LIPA_TPlen"]] = output[["Nfragments_IN2LIPA"]] = NA
      output[["IN2MVPA_TPsum"]] = output[["IN2MVPA_TPlen"]] = output[["Nfragments_IN2MVPA"]] = NA
      # only calculate this if there are enough fragments for both transitions
      if (length(inact_2_light_trans) >=  min_Nfragments_TP_only & 
          length(inact_2_mvpa_trans) >= min_Nfragments_TP_only) {
        State2 = fragments3$length[inact_2_light_trans] # durations of all inactivity fragments followed by light.
        output[["IN2LIPA_TPsum"]] = (sum(State2)/sum(State1)) / mean(State1) # transition from inactive to mvpa
        output[["IN2LIPA_TPlen"]] = (length(State2)/length(State1)) / mean(State1) # transition from inactive to mvpa
        output[["Nfragments_IN2LIPA"]] = length(State2)
    
        State3 = fragments3$length[inact_2_mvpa_trans] # durations of all inactivity fragments followed by light.
        output[["IN2MVPA_TPsum"]] = (sum(State3)/sum(State1)) / mean(State1) # transition from inactive to mvpa
        output[["IN2MVPA_TPlen"]] = (length(State3)/length(State1)) / mean(State1) # transition from inactive to mvpa
        output[["Nfragments_IN2MVPA"]] = length(State3)
      }
      if (length(inact_2_light_trans) >= min_Nfragments_TP_only & length(inact_2_mvpa_trans) >= min_Nfragments_TP_only) {
        output[["IN2PA_TP"]] = 1 / mean(State1) # transition from inactive to mvpa
      }
    }
    if ("InfEn" %in% frag.metrics) {
      alpha = length(unique(y))
      N = length(y)
      IEnt = 0
      uvalues = unique(y)
      for (i in 1:alpha) {
        p_i = length(which(y == uvalues[i])) / N
        IEnt = IEnt - (p_i * log(p_i,2))
      }
      output[["InfEn_multiclass"]] = IEnt/ log(alpha, 2)
    }
    if ("SampEn" %in% frag.metrics) {
      SampEn = c()
      try(expr = {SampEn = TSEntropies::FastSampEn(TS = y, dim = 2, lag = 1)})
      if (length(SampEn) == 0) {
        try(expr = {SampEn = TSEntropies::SampEn(TS = y, dim = 2, lag = 1)})
      }
      output[["SampEn_multiclass"]] = SampEn
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
  # - 1 = behaviour of interest defined by frag.classes.day/frag.classes.spt in g.part
  # - 0 = all remaining time, which we may consider breaks in our behaviour of interest
  fragments = rle(x)
  Nfragments = length(fragments$lengths) / 2
  if (Nfragments >= min_Nfragments) {
    State1 = fragments$length[which(fragments$value == 1)]
    State0 = fragments$length[which(fragments$value == 0)]
    output[["nfragments_0"]] = length(State0)
    output[["nfragments_1"]] = length(State1)
    
    if ("CoV" %in% frag.metrics){ #coefficient of variation as described by Boerema 2020
      output[["CoV_0"]] = sd(State0) / mean(log(State0))
      output[["CoV_1"]] = sd(State1) / mean(log(State1))
    }
    if ("mean_bout" %in% frag.metrics){
      output[["mean_0"]] = mean(State0)
      output[["mean_1"]] = mean(State1)
    }
    if ("TP" %in% frag.metrics){
      output[["towardsTP"]] = 1/mean(State0) # transition towards behaviour of interest
      output[["awayTP"]] = 1/mean(State1) # transition away from behaviour of interest
    }
    if ("Gini" %in% frag.metrics){
      output[["Gini_0"]] = ineq::Gini(State0,corr = T)
      output[["Gini_1"]] = ineq::Gini(State1,corr = T)
    }
    if ("power" %in% frag.metrics){
      output[["alpha_0"]] = output[["alpha_1"]] = output[["x0.5_0"]] = output[["x0.5_1"]] = output[["W0.5_0"]] = output[["W0.5_1"]] = NA
      nr = length(State0)
      na = length(State1)
      rmin = min(State0)
      amin = min(State1)
      output[["alpha_0"]] = 1+ nr/sum(log(State0/(rmin-0.5)))
      output[["alpha_1"]] = 1+ na/sum(log(State1/(amin-0.5)))
      # From this we can calculate (according to Chastin 2010):
      output[["x0.5_0"]] = 2^ (1 / (output[["alpha_0"]]-1) * min(State0))
      output[["x0.5_1"]] = 2^ (1 / (output[["alpha_1"]]-1) * min(State1))
      # From this we can calculate (according to Chastin 2010):
      output[["W0.5_0"]] = sum(State0[which(State0 > output[["x0.5_0"]])]) / sum(State0)
      output[["W0.5_1"]] = sum(State1[which(State1 > output[["x0.5_1"]])]) / sum(State1)
    }
    if ("hazard" %in% frag.metrics){
      fitr = survival::survfit(survival::Surv(State0,rep(1,length(State0)))~1)
      fita = survival::survfit(survival::Surv(State1,rep(1,length(State1)))~1)
      output[["h_0"]] =  mean(fitr$n.event/fitr$n.risk)
      output[["h_1"]] = mean(fita$n.event/fita$n.risk)
    }
    # , TSEntropies, nonlinearTseries
    if ("dfa" %in% frag.metrics) {
      dfa = nonlinearTseries::dfa(time.series = x,
                                  window.size.range = c(3, 30),
                                  npoints = 20,
                                  do.plot = FALSE)
      output[["dfa"]] = nonlinearTseries::estimate(dfa,do.plot=FALSE)[1]
    }
    # #------------------------------------------------------------------------
    # # For the following metrics it is a bit of a question mark at the moment whether
    # # applying them to binary time series is actually meaningful, maybe these
    # # should be moved to a seperate function to be applied to all
    # # behavioural classes/intensities, which we could name g.complexity
    if ("InfEn" %in% frag.metrics) {
      TS = x
      alpha = length(unique(TS))
      N = length(TS)
      IEnt = 0
      uvalues = unique(TS)
      for (i in 1:alpha) {
        p_i = length(which(TS == uvalues[i])) / N
        IEnt = IEnt - (p_i * log(p_i,2))
      }
      output[["InfEn_binary"]] = IEnt/ log(alpha, 2)
    }
    if ("SampEn" %in% frag.metrics) {
      output[["SampEn_binary"]] = TSEntropies::SampEn_C(TS = x, dim = 2, lag = 1)
    }
  }
  return(output)
}
