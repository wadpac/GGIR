g.getbout = function(x, boutduration, boutcriter = 0.8, closedbout = FALSE, bout.metric = 6, ws3 = 5) {
  p = which(x == 1)
  if (bout.metric == 1) { # MVPA definition as used in 2014
    # p are the indices for which the intensity criteria are met
    xt = x
    boutcount = rep(0, length(x)) #initialize zeros as long as there are epochs in the timewindow
    jmvpa = 1 # index for going stepwise through vector p
    Lx = length(x) # Lx is length of vector x
    while (jmvpa <= length(p)) { # go through all epochs that are possibly part of a bout
      endi = p[jmvpa] + boutduration
      if (endi <= Lx) { #does bout fall without measurement?
        if (sum(x[p[jmvpa]:endi]) > (boutduration*boutcriter)) {
          while (sum(x[p[jmvpa]:endi]) > ((endi - p[jmvpa])*boutcriter) & endi <= Lx) {
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
          x[p[jmvpa]] = x[p[jmvpa - 1]]
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
    while (jmvpa <= length(p)) {
      endi = p[jmvpa] + boutduration
      if (endi <= length(x)) { #does bout fall without measurement?
        lengthbout = sum(x[p[jmvpa]:endi])
        if (lengthbout > (boutduration*boutcriter)) {
          xt[p[jmvpa]:endi] = 2 #remember that this was a bout in r1t
        } else {
          x[p[jmvpa]] = 0
        }    	
      } else { #bout does not fall within measurement
        if (length(p) > 1 & jmvpa > 2) {
          x[p[jmvpa]] = x[p[jmvpa - 1]]
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
    lookforbreaks = zoo::rollmean(x = x, k = (60/ws3), align = "center", fill = rep(0,3)) #
    #insert negative numbers to prevent these minutes to be counted in bouts
    xt[lookforbreaks == 0] = -(60/ws3) * boutduration 
    #in this way there will not be bouts breaks lasting longer than 1 minute
    RM = zoo::rollmean(x = xt, k = boutduration, align = "center", fill = rep(0,3)) #,
    p = which(RM > boutcriter)
    starti = round(boutduration/2)
    for (gi in 1:boutduration) {
      inde = p - starti + (gi - 1)
      xt[inde[inde > 0 & inde < length(xt)]] = 2
    }
    x[xt != 2] = 0
    x[xt == 2] = 1
    boutcount = x
  } else if (bout.metric == 4) { # Was default from April - September 2020
    x[is.na(x)] = 0 # ignore NA values in the unlikely event that there are any
    xt = x
    #look for breaks larger than 1 minute
    # 30-7-2020, I do + 1 to make sure we look for breaks larger than but not equal to a minute,
    # this is critical when working with 1 minute epoch data
    lookforbreaks = zoo::rollmean(x=x,k=(60/ws3)+1,align="center",fill=rep(0,3)) 
    #insert negative numbers to prevent these minutes to be counted in bouts
    #in this way there will not be bouts breaks lasting longer than 1 minute
    xt[lookforbreaks == 0] = -(60/ws3) * boutduration 
    RM = zoo::rollmean(x=xt,k=boutduration,align="center",fill=rep(0,3)) #,
    p = which(RM >=boutcriter)
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
  } else if (bout.metric == 5) { # Used between September 2020 and April 2021 (GitHub branch only)
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
  } else if (bout.metric == 6) {
      x[is.na(x)] = 0 # ignore NA values in the unlikely event that there are any
      xt = x
      #look for breaks larger than 1 minute
      # Note: we do + 1 to make sure we look for breaks larger than but not equal to a minute,
      # this is critical when working with 1 minute epoch data
      # we need to append 0's at the beginning and end so that first and last epochs in timeseries do not get ignored
      zeroes = rep(0, ceiling(60/ws3/2))
      xtmp = c(zeroes, x, zeroes)
      lookforbreaks = zoo::rollmean(x = xtmp, k = (60/ws3) + 1, align = "center", fill = rep(0,3))
      # remove appended zeroes
      keep = (length(zeroes) + 1):(length(lookforbreaks) - length(zeroes))
      #insert negative numbers to prevent these minutes to be counted in bouts
      #in this way there will not be bouts breaks lasting longer than 1 minute
      xtmp[lookforbreaks == 0] = -boutduration
      xt = xtmp[keep]
      # append 0's to define boutcriter
      append = rep(-boutduration, ceiling(boutduration/2))
      xtmp = c(append, xt, append)
      RM = zoo::rollmean(x = xtmp, k = boutduration, align = "center", fill = rep(0,3))
      keep = (length(append) + 1):(length(RM) - length(append))
      RM = RM[keep]
      p = which(RM >= boutcriter)
      half1 = floor(boutduration/2)
      half2 = boutduration - half1
      # # only consider windows that at least start and end with value that meets criterium
      # p = c(0, p, 0)
      # if (ws3 > 60) {
      #   epochs2check = 1
      # } else {
      #   epochs2check = (60/ws3)
      # }
      # p = p[which(p != 0)]
      # now mark all epochs that are covered by the detected bout/s
      detected_bouts = split(p, cumsum(c(1, diff(p) != 1)))
      if (length(detected_bouts) == 1 & length(detected_bouts[[1]]) == 0) {
        # if no bouts are detected
        x[which(xt != 2)] = 0
        x[which(xt == 2)] = 1
        boutcount = x # distinction not made anymore, but object kept to preserve output structure
      } else {
        # if bouts are detected
        for (bout_i in 1:length(detected_bouts)) {
          bout = detected_bouts[[bout_i]]
          # find start of bout
          start_found = FALSE
          adjust = 0
          while (isFALSE(start_found)) {
            start = min(bout) - half1 + adjust
            if (start < 1) {
              adjust = adjust + 1
              next
            }
            if (xt[start] != 1) {
              # a bout cannot start without meetin threshold crit
              adjust = adjust + 1
              next
            }
            start_period = start:max(bout)
            look4start = split(xt[start_period], cumsum(c(1, diff(xt[start_period]) != 0)))
            max_gap = 60/ws3
            zeros = c()
            for (i in 1:length(look4start)) {
              zeros = c(zeros, sum(look4start[[i]] == 0))
            }
            if (all(zeros <= max_gap)) start_found = TRUE
            adjust = adjust + 1
          }
          # find end of bout
          end_found = FALSE
          adjust = 0
          while (isFALSE(end_found)) {
            end = max(bout) + half1 - adjust
            if (end > length(xt)) {
              adjust = adjust + 1
              next
            }
            if (xt[end] != 1) {
              # a bout cannot start without meetin threshold crit
              adjust = adjust + 1
              next
            }
            end_period = min(bout):end
            look4end = split(xt[end_period], cumsum(c(1, diff(xt[end_period]) != 0)))
            max_gap = 60/ws3
            zeros = c()
            for (i in 1:length(look4start)) {
              zeros = c(zeros, sum(look4start[[i]] == 0))
            }
            if (all(zeros <= max_gap)) end_found = TRUE
            adjust = adjust + 1
          }
          # while (x[start] != 1) start = start + 1
          # while (x[end] != 1) end = end - 1
          xt[start:end] = 2
        }
        x[which(xt != 2)] = 0
        x[which(xt == 2)] = 1
        boutcount = x # distinction not made anymore, but object kept to preserve output structure
      }
  }  
  invisible(list(x = x, boutcount = boutcount))
}