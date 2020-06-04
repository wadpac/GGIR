g.fragmentation = function(x=c(),
                         frag.metrics = c("mean_bout","TP","Gini","power","hazard","all")) {
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
      frag.metrics = c("mean_bout","TP","Gini","power","hazard")
    }
    State1 = fragments$length[which(fragments$value == 1)]
    State0 = fragments$length[which(fragments$value == 0)]
    
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
    }
    
    # if ("hazard" %in% frag.metrics){
    #   fitr = survival::survfit(survival::Surv(State0,rep(1,length(State0)))~1)
    #   fita = survival::survfit(survival::Surv(State1,rep(1,length(State1)))~1)
    #   output[["h_0"]] =  mean(fitr$n.event/fitr$n.risk)
    #   output[["h_1"]] = mean(fita$n.event/fita$n.risk)
    # }
  }
  return(output)
}
