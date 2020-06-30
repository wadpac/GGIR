g.fragmentation = function(x=c(),
                         frag.metrics = c("mean_bout", "TP", "Gini", "power", "hazard", "h", "CoV",
                                          "x0.5_0", "W0.5_0", "dfa", "InfEn", "SampEn", "FastApEn", "all")) {
  # This function is based on code from R package ActFrag as developed by Junrui Di.
  # Dependencies: ineq and survival package for Gini and Hazard metric respecitvely.
  #
  # x: time series of integers 0 or 1 to indicate bouts, where
  # - 1 = behaviour of interest defined by frag.classes.day/frag.classes.spt in g.part
  # - 0 = all remaining time, which we may consider breaks in our behaviour of interest
  # frag.metrics: metric to define fragmentation
  # in contract to R package ActFrag this function assumes
  # that non-wear and missing values have already been taken care of outside this
  # function.
  if(!is.integer(x)){
    warning("Fragmentation input has to be integers!")
  } else {
    x = as.integer(x)
  }
  fragments = rle(x)
  output = list()
  Nfragments = length(fragments$lengths)
  output[["Nfragments"]] = Nfragments
  if (Nfragments > 1) {
    
    if ("all" %in% frag.metrics){
      frag.metrics = c("mean_bout","TP","Gini","power","hazard", "h", "CoV",
                       "x0.5_0", "W0.5_0", "dfa", "InfEn","SampEn","FastApEn")
    }
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
      nr = length(State0)
      na = length(State1)
      rmin = min(State0)
      amin = min(State1)
      output[["alpha_0"]] = 1+ nr/sum(log(State0/(rmin-0.5)))
      output[["alpha_1"]] = 1+ na/sum(log(State1/(amin-0.5)))
      # From this we can calculate (according to Chastin 2010):
      output[["x0.5_0"]] = 2^ (1 / (output[["alpha_0"]]-1) * min(State0))
      output[["x0.5_1"]] =  2^ (1 / (output[["alpha_1"]]-1) * min(State1))
      # From this we can calculate (according to Chastin 2010):
      output[["W0.5_0"]] = sum(State0[which(State0 > output[["x0.5_0"]])]) / sum(State0)
      output[["W0.5_1"]] = sum(State1[which(State1 > output[["x0.5_1"]])]) / sum(State1)
    }
    # if ("hazard" %in% frag.metrics){
    #   fitr = survival::survfit(survival::Surv(State0,rep(1,length(State0)))~1)
    #   fita = survival::survfit(survival::Surv(State1,rep(1,length(State1)))~1)
    #   output[["h_0"]] =  mean(fitr$n.event/fitr$n.risk)
    #   output[["h_1"]] = mean(fita$n.event/fita$n.risk)
    # }
    # , TSEntropies, nonlinearTseries
    # if ("dfa" %in% frag.metrics) {
    #   dfa_0 = nonlinearTseries::dfa(time.series = State0,
    #               window.size.range = c(3, 30),
    #               npoints = 20,
    #               do.plot = FALSE)
    #   dfa_1 = nonlinearTseries::dfa(time.series = State0,
    #               window.size.range = c(3, 30),
    #               npoints = 20,
    #               do.plot = FALSE)
    #   output[["dfa_0"]] = nonlinearTseries::estimate(dfa_0,do.plot=FALSE)
    #   output[["dfa_1"]] = nonlinearTseries::estimate(dfa_1,do.plot=FALSE)
    # }
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
      output[["InfEn"]] = IEnt/ log(alpha, 2)
    }
    if ("ApEnt" %in% frag.metrics) {
      TS = x
      output[["FastApEn"]] = TSEntropies::FastApEn(TS, dim = 2, lag = 1, r = 0.15 * sd(TS))
    }
    if ("SanpEn" %in% frag.metrics) {
      output[["SampEn"]] = TSEntropies::SampEn_C(TS = x, dim = 2, lag = 1)
    }
    

  }
  return(output)
}
