g.getbout = function(x,boutduration,boutcriter=0.8,closedbout=FALSE,bout.metric=1,ws3=5) {
  p = which(x == 1)
  if (bout.metric == 1) { # MVPA definition as used in 2014
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
    x[which(xt == 2)] = 1
    if (length(which(xt == 1)) > 0) x[which(xt == 1)] = 0
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
    x[which(xt == 2)] = 1
  } else if (bout.metric == 3) { # bout metric simply looks at percentage of moving window that meets criterium
    x[which(is.na(x) == TRUE)] = 0 # ignore NA values in the unlikely event that there are any
    xt = x
    #look for breaks larger than 1 minute
    lookforbreaks = zoo::rollmean(x=x,k=(60/ws3),align="center",fill=rep(0,3))
    #insert negative numbers to prevent these minutes to be counted in bouts
    xt[which(lookforbreaks == 0)] = -(60/ws3) * boutduration 
    #in this way there will not be bouts breaks lasting longer than 1 minute
    RM = zoo::rollmean(x=xt,k=boutduration,align="center",fill=rep(0,3)) #,
    p = which(RM > boutcriter)
    starti = round(boutduration/2)
    for (gi in 1:boutduration) {
      inde = p-starti+(gi-1)
      xt[inde[which(inde > 0 & inde < length(xt))]] = 2
    }
    x[which(xt != 2)] = 0
    x[which(xt == 2)] = 1
    boutcount = x
  }  
  invisible(list(x=x,boutcount=boutcount))
}