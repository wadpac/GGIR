g.getbout = function(x,boutduration,boutcriter=0.8,closedbout=FALSE,bout.metric=1,ws3=5) {
  p = which(x == 1)
  if (bout.metric == 1) { # MVPA definition as used in 2014
    # p are the indices for which the intensity criteria are met
    xt = x
    boutcount = rep(0,length(x)) #initialize zeros as long as there are epochs in the timewindow
    jmvpa = 1 # index for going stepwise through vector p
    Lx = length(x) # Lx is length of vector x
    while(jmvpa <= length(p)) { # go through all epochs that are possibly part of a bout
      endi = p[jmvpa]+boutduration
      if (endi <= Lx) { #does bout fall without measurement?
        if (sum(x[p[jmvpa]:endi]) > (boutduration*boutcriter)) {
          while(sum(x[p[jmvpa]:endi]) > ((endi-p[jmvpa])*boutcriter) & endi <= Lx) {
            endi = endi + 1
          }
          select = p[jmvpa:max(which(p < endi))]
          jump = length(select)
          xt[select] = 2 #remember that this was a bout
          boutcount[p[jmvpa]:p[max(which(p < endi))]] = 1
        } else {
          jump = 1
          x[p[jmvpa]] = 0
        }
      } else { #bout does not fall within measurement
        jump = 1
        if (length(p) > 1 & jmvpa > 2) {
          x[p[jmvpa]] = x[p[jmvpa-1]]
        }        
      }
      jmvpa = jmvpa + jump
    }
    x[xt == 2] = 1
    if (length(which(xt == 1)) > 0) x[xt == 1] = 0
    
    if (closedbout == TRUE) { #only difference is that bouts are closed
      x = boutcount
    }
  } else if (bout.metric == 2) { # MVPA based on percentage relative to start of bout
    xt = x
    jmvpa = 1
    while(jmvpa <= length(p)) {
      endi = p[jmvpa]+boutduration
      if (endi <= length(x)) { #does bout fall without measurement?
        lengthbout = sum(x[p[jmvpa]:endi])
        if (lengthbout > (boutduration*boutcriter)) {
          xt[p[jmvpa]:endi] = 2 #remember that this was a bout in r1t
        } else {
          x[p[jmvpa]] = 0
        }    	
      } else { #bout does not fall within measurement
        if (length(p) > 1 & jmvpa > 2) {
          x[p[jmvpa]] = x[p[jmvpa-1]]
        }				
      }
      jmvpa = jmvpa + 1
    }
    x[xt == 2] = 1
    boutcount = x
  } else if (bout.metric == 3) { # bout metric simply looks at percentage of moving window that meets criterium
    x[is.na(x)] = 0 # ignore NA values in the unlikely event that there are any
    xt = x
    #look for breaks larger than 1 minute
    lookforbreaks = zoo::rollmean(x=x,k=(60/ws3),align="center",fill=rep(0,3)) #
    #insert negative numbers to prevent these minutes to be counted in bouts
    xt[lookforbreaks == 0] = -(60/ws3) * boutduration 
    #in this way there will not be bouts breaks lasting longer than 1 minute
    RM = zoo::rollmean(x=xt,k=boutduration,align="center",fill=rep(0,3)) #,
    p = which(RM > boutcriter)
    starti = round(boutduration/2)
    for (gi in 1:boutduration) {
      inde = p-starti+(gi-1)
      xt[inde[inde > 0 & inde < length(xt)]] = 2
    }
    x[xt != 2] = 0
    x[xt == 2] = 1
    boutcount = x
  } else if (bout.metric == 4) { # bout metric simply looks at percentage of moving window that meets criterium
    x[is.na(x)] = 0 # ignore NA values in the unlikely event that there are any
    xt = x
    #look for breaks larger than 1 minute
    lookforbreaks = zoo::rollmean(x=x,k=(60/ws3),align="center",fill=rep(0,3)) #
    #insert negative numbers to prevent these minutes to be counted in bouts
    #in this way there will not be bouts breaks lasting longer than 1 minute
    xt[lookforbreaks == 0] = -(60/ws3) * boutduration 
    RM = zoo::rollmean(x=xt,k=boutduration,align="center",fill=rep(0,3)) #,
    p = which(RM > boutcriter)
    starti = round(boutduration/2)
    # only consider windows that at least start and end with value that meets criterium
    tri = p-starti
    kep = which(tri > 0 & tri < (length(x)-(boutduration-1)))
    if (length(kep) > 0) tri = tri[kep]
    p = p[which(x[tri] == 1 & x[tri+(boutduration-1)] == 1)]
    # now mark all epochs that are covered by the remaining windows
    for (gi in 1:boutduration) {
      inde = p-starti+(gi-1)
      xt[inde[which(inde > 0 & inde < length(xt))]] = 2
    }
    x[xt != 2] = 0
    x[xt == 2] = 1
    boutcount = x
  }  else if (bout.metric == 5) { # bout metric simply looks at percentage of moving window that meets criterium
      x[is.na(x)] = 0 # ignore NA values in the unlikely event that there are any
      xt = x
      #look for breaks larger than 1 minute
      # Note: we do + 1 to make sure we look for breaks larger than but not equal to a minute,
      # this is critical when working with 1 minute epoch data
      lookforbreaks = zoo::rollmean(x=x,k=(60/ws3)+1,align="center",fill=rep(0,3))
      #insert negative numbers to prevent these minutes to be counted in bouts
      #in this way there will not be bouts breaks lasting longer than 1 minute
      xt[lookforbreaks == 0] = -(60/ws3) * boutduration 
      RM = zoo::rollmean(x=xt,k=boutduration,align="center",fill=rep(0,3)) #,
      # p = which(RM > boutcriter)
      p = which(RM >=boutcriter) # changed to be able to detect bouts with bout criteria 1.0
      starti = round(boutduration/2)
      # only consider windows that at least start and end with value that meets criterium
      tri = p-starti
      kep = which(tri > 0 & tri < (length(x)-(boutduration-1)))
      if (length(kep) > 0) tri = tri[kep]
      p = p[which(x[tri] == 1 & x[tri+(boutduration-1)] == 1)]
      # now mark all epochs that are covered by the remaining windows
      for (gi in 1:boutduration) {
        inde = p-starti+(gi-1)
        xt[inde[which(inde > 0 & inde < length(xt))]] = 2
      }
      x[xt != 2] = 0
      x[xt == 2] = 1
      boutcount = x
  }  
  invisible(list(x=x,boutcount=boutcount))
}