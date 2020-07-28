g.fragmentation = function(x=c(), frag.classes =c(),
                           frag.metrics = c("mean_bout", "TP", "Gini", "power", "hazard", "h", "CoV",
                                            "dfa", "InfEn",
                                            "SampEn", "ApEn", "RQA", "all"),
                           ACC = c(), intensity.thresholds = c()) {
  # This function is based on code from R package ActFrag as developed by Junrui Di.
  # Dependencies: ineq and survival package for Gini and Hazard metric respecitvely.
  #
  # x: time series of classess
  # frag.metrics: metric to define fragmentation
  # in contract to R package ActFrag this function assumes
  # that non-wear and missing values have already been taken care of outside this
  # function.
  if(!is.integer(x)){
    warning("Fragmentation input has to be integers!")
  } else {
    x = as.integer(x)
  }
  if ("all" %in% frag.metrics){
    frag.metrics = c("mean_bout","TP","Gini","power","hazard", "h", "CoV",
                     "dfa", "InfEn","SampEn","ApEn", "RQA")
  }
  output = list()
  if (length(ACC) > 1 & length(intensity.thresholds) > 2) {
    #====================================================
    # Space for complexity metrics that depends on continuous data:
    # ...
    # RQA analysis
    # Approximate Entropy
    if ("ApEn" %in% frag.metrics) {
      # output[["FastApEn_contin"]] = NA
      FastApEn = c()
      try(expr = {FastApEn = TSEntropies::FastApEn(TS = ACC, dim = 2, lag = 1, r = 0.15 * sd(ACC))})
      if (length(FastApEn) == 0) {
        try(expr = {FastApEn = TSEntropies::ApEn(TS = ACC, dim = 2, lag = 1, r = 0.15 * sd(ACC))})
      }
      output[["FastApEn_contin"]] = FastApEn
    }
    # if ("RQA" %in% frag.metrics) { 
    #   cat("A")
    #   RP = nonlinearTseries::rqa(time.series=ACC, radius = 10, embedding.dim = 2) #[1:(60*10)] #S$class_id
    #   cat("B")
    #   output[["REC_rqa_contin"]] = RP$REC
    #   output[["DET_rqa_contin"]] = RP$DET
    #   output[["RATIO_rqa_contin"]] = RP$RATIOImpl
    #   output[["DIV_rqa_contin"]] = RP$DIV
    #   output[["Lmax_rqa_contin"]] = RP$Lmax
    #   output[["Lmean_rqa_contin"]] = RP$Lmean
    #   output[["ENTR_rqa_contin"]] = RP$ENTR
    #   output[["LAM_rqa_contin"]] = RP$LAM
    #   output[["Vmax_rqa_contin"]] = RP$Vmax
    #   output[["Vmean_rqa_contin"]] = RP$Vmean
    # }
    
    #====================================================
    # Complexity metrics that depend on multi-class data
    # Convert ACC into categorical multi-class behaviours
    y = rep(0,length(ACC))
    intensity.thresholds = c(0, intensity.thresholds)
    for (ij in 1:length(intensity.thresholds)) {
      if (ij != length(intensity.thresholds)) {
        ij_indices = which(ACC >= intensity.thresholds[ij] & ACC < intensity.thresholds[ij+1])
        if (length(ij_indices) > 0) y[ij_indices] = ij
      } else {
        ij_indices = which(ACC >= intensity.thresholds[ij])
        if (length(ij_indices) > 0) y[ij_indices] = ij
      }
    }
    if ("TP" %in% frag.metrics) {
      y2 = y # y is a four class variables for inactivity, light, moderate, and vigorous
      y2[which(y2 == 4)] = 3 # collapse moderate and vigorous into mvpa
      fragments3 = rle(y2) # fragments is now data.frame with value (intensity) and length (duration of fragment) 
      State1 = fragments3$length[which(fragments3$value == 1)] # all inactivity fragments
      Nfrag3 = length(fragments3$value)
      # Get only indices of inactive fragments that transition to light:
      inact_2_light_trans = which(fragments3$value[1:(Nfrag3-1)] == 1 & fragments3$value[2:Nfrag3] == 2)
      # Get only indices of inactive fragments that transition to mvpa:
      inact_2_mvpa_trans = which(fragments3$value[1:(Nfrag3-1)] == 1 & fragments3$value[2:Nfrag3] == 3)
      # State1 = State1[1:(length(State1)-1)] # shorten by 1 do match length of State 2 and 3
      # initialise variables
      output[["IN2PA_TP"]] = output[["IN2LIPA_TP"]] =  output[["Nfragments_IN2LIPA"]] = NA
      output[["IN2MVPA_TP"]] = output[["Nfragments_IN2MVPA"]] = NA
      if (length(inact_2_light_trans) >= 10) {
        State2 = fragments3$length[inact_2_light_trans] # durations of all inactivity fragments followed by light.
        output[["IN2LIPA_TP"]] = (sum(State2)/sum(State1)) / mean(State1) # transition from inactive to mvpa
        output[["Nfragments_IN2LIPA"]] = length(State2)
      }
      if (length(inact_2_mvpa_trans) >= 10) {
        State3 = fragments3$length[inact_2_mvpa_trans] # durations of all inactivity fragments followed by light.
        output[["IN2MVPA_TP"]] = (sum(State3)/sum(State1)) / mean(State1) # transition from inactive to mvpa
        output[["Nfragments_IN2MVPA"]] = length(State3)
      }
      if (length(inact_2_light_trans) >= 10 & length(inact_2_mvpa_trans) >= 10) {
        output[["IN2PA_TP"]] = 1 / mean(State1) # transition from inactive to mvpa
      }
      rm(y2)
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
      # output[["SampEn_multiclass"]] = NA
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
  binarize = function(x, values) {
    # converts input into binary signal, with a 1 for x values that match the vector values
    # and a 0 for all other values
    tmp = as.integer(ifelse(test = x %in% values, yes = 1, no = 0))
    return(tmp)
  }
  print(frag.classes)
  x = binarize(x, values = frag.classes)
  # x are now integers 0 or 1 to indicate bouts, where
  # - 1 = behaviour of interest defined by frag.classes.day/frag.classes.spt in g.part
  # - 0 = all remaining time, which we may consider breaks in our behaviour of interest
  
  fragments = rle(x)
  Nfragments = length(fragments$lengths)
  output[["Nfragments"]] = Nfragments
  if (Nfragments >= 10) {
    
    State1 = fragments$length[which(fragments$value == 1)]
    State0 = fragments$length[which(fragments$value == 0)]
    
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
