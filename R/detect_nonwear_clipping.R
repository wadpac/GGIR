detect_nonwear_clipping = function(data = c(), windowsizes = c(5, 900, 3600), sf = 100,
                                   clipthres = 7.5, sdcriter = 0.013, racriter = 0.05,
                                   nonwear_approach = "2013",
                                   params_rawdata = c()) {
  ws3 = windowsizes[1]; ws2 = windowsizes[2]; ws = windowsizes[3]
  window3 = ws3 * sf #window size in samples
  window2 = ws2 * sf #window size in samples
  window = ws * sf #window size in samples
  nmin = floor(nrow(data)/(window2)) # nmin = minimum number of windows that fit in this block of data
  CW = NW = matrix(0,nmin,3) #CW is clipping, NW is non-wear
  CWav = NWav = rep(0, nmin)
  crit = ((window/window2)/2) + 1
  
  if (nonwear_approach %in% c("2013", "2023")) {
    # define windows to check:
    for (h in 1:nmin) { #number of windows
      
      # clip detection based on window2 (do not use window)
      cliphoc1 = (((h - 1) * window2) + window2 * 0.5 ) - window2 * 0.5
      cliphoc2 = (((h - 1) * window2) + window2 * 0.5 ) + window2 * 0.5
      
      # Flag nonwear based on window instead of window2 (2023-02-18)
      if (nonwear_approach == "2013") {
        NWflag = h
        if (h <= crit) {
          hoc1 = 1
          hoc2 = window
        } else if (h >= (nmin - crit)) {
          hoc1 = (nmin - crit) * window2
          hoc2 = nmin * window2 #end of data
        } else if (h > crit & h < (nmin - crit)) {
          hoc1 = (((h - 1) * window2) + window2 * 0.5 ) - window * 0.5
          hoc2 = (((h - 1) * window2) + window2 * 0.5 ) + window * 0.5
        }
      
      } else if (nonwear_approach == "2023") {
        # long-epoch windows to flag (nonwear)
        NWflag = h:(h + window/window2 - 1)
        if (NWflag[length(NWflag)] > nmin) NWflag = NWflag[-which(NWflag > nmin)]
        # window to check (not aggregated values)
        hoc1 = h * window2 - window2
        hoc2 = hoc1 + window
        if (hoc2 > nrow(data)) {
          hoc2 = nrow(data)
        }
      }
      # ---
      if ("wear" %in% colnames(data)) {
        wearTable = table(data[(1 + hoc1):hoc2, "wear"], useNA = "no")
        NWav[h] = as.numeric(tail(names(sort(wearTable)), 1)) * 3 # times 3 to simulate heuristic approach
      }
      xyzCol = which(colnames(data) %in% c("x", "y", "z"))
      for (jj in seq(3)) {
        # Clipping
        aboveThreshold = which(abs(data[(1 + cliphoc1):cliphoc2, xyzCol[jj]]) > clipthres)
        CW[h, jj] = length(aboveThreshold)
        if (length(aboveThreshold) > 0) {
          if (length(which(abs(data[c((1 + cliphoc1):cliphoc2)[aboveThreshold],  xyzCol[jj]]) > clipthres * 1.5)) > 0) {
            CW[h, jj] = window2 # If there is a a value that is more than 150% the dynamic range then ignore entire block.
          }
        }
        # Non-wear
        #hoc1 & hoc2 = edges of windows
        #window is bigger& window2 is smaller one
        if (nonwear_approach == "2013") {
          indices = (1 + hoc1):hoc2
        } else if (nonwear_approach == "2023") {
          indices = seq((1 + hoc1), hoc2, by = ceiling(sf / 5))
        }
        maxwacc = max(data[indices, xyzCol[jj]], na.rm = TRUE)
        minwacc = min(data[indices, xyzCol[jj]], na.rm = TRUE)
        absrange = abs(maxwacc - minwacc)
        if (absrange < racriter) {
          sdwacc = sd(data[indices, xyzCol[jj]], na.rm = TRUE)
          if (sdwacc < sdcriter) {
            NW[NWflag,jj] = 1
          }
        }
      }
      CW = CW / (window2)
      if (!("wear" %in% colnames(data))) {
        NWav[h] = (NW[h,1] + NW[h,2] + NW[h,3]) #indicator of non-wear
      }
      CWav[h] = max(c(CW[h, 1], CW[h, 2], CW[h, 3])) #indicator of clipping
    }
  } 
  # In NWav: single 1's surrounded by 2's or 3's --> 2 (so it is considered nonwear)
  ones = which(NWav == 1)
  if (length(ones) > 0) {
    for (one_i in ones) {
      if (one_i - 1 < 1) next # skip if we are in the very begining
      if (one_i + 1 > length(NWav)) next # skip if we are in the very end
      if (NWav[one_i - 1] > 1 & NWav[one_i + 1] > 1) NWav[one_i] = 2
    }
  }
  return(list(NWav = NWav, CWav = CWav, nmin = nmin))
}
