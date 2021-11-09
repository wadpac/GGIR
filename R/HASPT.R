HASPT = function(angle, perc = 10, spt_threshold = 15,
                 sptblocksize = 30, spt_max_gap = 60, ws3 = 5,
                 constrain2range = FALSE, HASPT.algo="HDCZA", invalid,
                 HASPT.ignore.invalid=FALSE) {
  tib.threshold = SPTE_start = SPTE_end = c()
  
  adjustlength = function(x, invalid) {
    if (length(invalid) > length(x)) {
      invalid = invalid[1:length(x)]
    } else if (length(invalid) < length(x)) {
      invalid = c(invalid, rep(0, length(x) - length(invalid)))
    }
    return(invalid)
  }
  if (HASPT.algo != "notused") {
    if (HASPT.algo == "HDCZA") { # original, default
      medabsdi = function(angle) {
        #50th percentile, do not use mean because that will be outlier sensitive
        angvar = stats::median(abs(diff(angle))) 
        return(angvar)
      }
      k1 = 5 * (60/ws3)
      x = zoo::rollapply(angle, width=k1, FUN=medabsdi) # 5 minute rolling median of the absolute difference
      nomov = rep(0,length(x)) # no movement
      pp = quantile(x,probs=c(perc/100)) * spt_threshold
      if (constrain2range == TRUE) {
        if (pp < 0.13) pp = 0.13
        if (pp > 0.50) pp = 0.50
      } else {
        if (pp == 0) pp = 0.20
      }
      if (HASPT.ignore.invalid == TRUE) {
        invalid = adjustlength(x, invalid)
        nomov[which(x < pp & invalid == 0)] = 1
      } else {
        nomov[which(x < pp)] = 1
      }
    } else if (HASPT.algo == "HorAngle") {  # if hip, then require horizontal angle
      x = angle
      if (HASPT.ignore.invalid == TRUE) {
        invalid = adjustlength(x, invalid)
        horizontal = which(abs(x) < 45 & invalid == 0)
      } else {
        horizontal = which(abs(x) < 45)
      }
      nomov = rep(0,length(x)) # no movement
      pp = NA
      if (length(horizontal) > 0) {
        nomov[horizontal] = 1
      }
    }
    inspttime = rep(NA,length(x))
    nomov = c(0,nomov,0)
    s1 = which(diff(nomov) == 1) #start of blocks in spt
    e1 = which(diff(nomov) == -1) #end of blocks in spt
    sptblock = which((e1 - s1) > ((60/ws3)*sptblocksize*1)) #which are the blocks longer than sptblocksize in minutes?
    if (length(sptblock) > 0) { #
      s2 = s1[sptblock] # only keep the sptblocks that are long enough
      e2 = e1[sptblock] # only keep the sptblocks that are long enough
      for (j in 1:length(s2)){
        inspttime[s2[j]:e2[j]] = 1 #record these blocks in the inspttime vector
      }
      # fill up gaps in time between spt blocks
      outofspt = rep(0,length(inspttime))
      outofspt[which(is.na(inspttime) == TRUE)] = 1
      outofspt = c(0,outofspt,0)
      s3 = which(diff(outofspt) == 1) #start of blocks out of spt?
      e3 = which(diff(outofspt) == -1) #end blocks out of spt?
      outofsptblock = which((e3 - s3) < ((60/ws3)*spt_max_gap*1))
      if (length(outofsptblock) > 0) { # only fill up gap if there are gaps
        s4 = s3[outofsptblock]
        e4 = e3[outofsptblock]
        if (length(s4) > 0) {
          for (j in 1:length(s4)){
            inspttime[ s4[j]:e4[j]] = 1
          }
        }
      }
      if (length(inspttime) == (length(x)+1)) inspttime = inspttime[1:(length(inspttime)-1)]
      # keep indices for longest in spt block:
      inspttime2 = rep(1,length(inspttime))
      inspttime2[which(is.na(inspttime) == TRUE)] = 0
      s5 = which(diff(c(0,inspttime2,0)) == 1) #start of blocks out of spt
      e5 = which(diff(c(0,inspttime2,0)) == -1) #end of blocks out of spt
      insptdurations = e5 - s5
      longestinspt = which(insptdurations == max(insptdurations))
      if (length(longestinspt) > 1) longestinspt = longestinspt[ceiling(length(longestinspt)/2)]
      SPTE_start = s5[longestinspt] - 1
      SPTE_end = e5[longestinspt] - 1
      if (SPTE_start == 0) SPTE_start = 1
    } else {
      SPTE_end = c()
      SPTE_start = c()
      tib.threshold = c()
    }
    tib.threshold = pp
  }
  invisible(list(SPTE_start=SPTE_start,
                 SPTE_end=SPTE_end, tib.threshold=tib.threshold))
}