g.getbout = function(x, boutduration, boutcriter = 0.8, ws3 = 5) {
  p = which(x == 1)
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
      while (start_found == FALSE) {
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
      while (end_found == FALSE) {
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
      xt[start:end] = 2
    }
    x[which(xt != 2)] = 0
    x[which(xt == 2)] = 1
  }
  return(x)
}