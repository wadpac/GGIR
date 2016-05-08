g.getbout = function(rr,boutdur2,boutcriter=100,p,closedbout=FALSE) {
  # p are the indices for which the intensity criteria are met
  rrt = rr # zeros as long as there are epochs in the timewindow
  boutcount = rep(0,length(rr)) # zeros as long as there are epochs in the timewindow
  jmvpa = 1
  LRR = length(rr)
  while(jmvpa <= length(p)) { # go through all epochs that are possibly part of a bout
    endi = p[jmvpa]+boutdur2
    if (endi <= LRR) { #does bout fall without measurement?
      if (sum(rr[p[jmvpa]:endi]) > (boutdur2*boutcriter)) {
        while(sum(rr[p[jmvpa]:endi]) > ((endi-p[jmvpa])*boutcriter) & endi <= LRR) {
          endi = endi + 1
        }
        select = p[jmvpa:max(which(p < endi))]
        # jump = (max(select) - min(select)) + 1
        jump = length(select)
        rrt[select] = 2 #remember that this was a bout
        boutcount[p[jmvpa]:p[max(which(p < endi))]] = 1
      } else {
        jump = 1
        rr[p[jmvpa]] = 0
      }
    } else { #bout does not fall within measurement
      jump = 1
      if (length(p) > 1 & jmvpa > 2) {
        rr[p[jmvpa]] = rr[p[jmvpa-1]]
      }        
    }
    jmvpa = jmvpa + jump
  }
  rr[which(rrt == 2)] = 1
  if (length(which(rrt == 1)) > 0) rr[which(rrt == 1)] = 0
  
  if (closedbout == TRUE) { #only difference is that bouts are closed
    rr = boutcount
  }
  invisible(list(rr=rr,boutcount=boutcount))
}