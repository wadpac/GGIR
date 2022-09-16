detect_nonwear_clipping = function(data = c(), windowsizes = c(5, 900, 3600), sf = 100,
                                   clipthres = 7.5, sdcriter = 0.013, racriter = 0.05,
                                   params_rawdata = params_rawdata) {
  ws3 = windowsizes[1]; ws2 = windowsizes[2]; ws = windowsizes[3]
  window3 = ws3 * sf #window size in samples
  window2 = ws2 * sf #window size in samples
  window = ws * sf #window size in samples
  nmin = floor(nrow(data)/(window2)) # nmin = minimum number of windows that fit in this block of data
  CW = NW = matrix(0,nmin,3) #CW is clipping, NW is non-wear
  CWav = NWav = rep(0, nmin)
  crit = ((window/window2)/2) + 1
  for (h in 1:nmin) { #number of windows
    cliphoc1 = (((h - 1) * window2) + window2 * 0.5 ) - window2 * 0.5 #does not use "window"
    cliphoc2 = (((h - 1) * window2) + window2 * 0.5 ) + window2 * 0.5
    if (h <= crit) {
      hoc1 = 1
      hoc2 = window
    } else if (h >= (nmin - crit)) {
      hoc1 = (nmin - crit)*window2
      hoc2 = nmin*window2 #end of data
    } else if (h > crit & h < (nmin - crit)) {
      hoc1 = (((h - 1) * window2) + window2 * 0.5 ) - window * 0.5
      hoc2 = (((h - 1) * window2) + window2 * 0.5 ) + window * 0.5
    }
    if (length(params_rawdata[["rmc.col.wear"]]) > 0) {
      wearcol = as.character(data[, which(colnames(data) == "wear")])
      suppressWarnings(storage.mode(wearcol) <- "logical")
      wearTable = table(wearcol[(1 + hoc1):hoc2], useNA = FALSE)
      NWav[h] = as.logical(tail(names(sort(wearTable)), 1)) * 3 # times 3 to simulate heuristic approach
    }
    for (jj in  1:3) {
      #hoc1 & hoc2 = edges of windows
      #window is bigger& window2 is smaller one
      sdwacc = sd(as.numeric(data[(1 + hoc1):hoc2,jj]), na.rm = TRUE)
      maxwacc = max(as.numeric(data[(1 + hoc1):hoc2,jj]), na.rm = TRUE)
      minwacc = min(as.numeric(data[(1 + hoc1):hoc2,jj]), na.rm = TRUE)
      CW[h,jj] = length(which(abs(as.numeric(data[(1 + cliphoc1):cliphoc2,jj])) > clipthres))
      if (length(which(abs(as.numeric(data[(1 + cliphoc1):cliphoc2,jj])) > clipthres*1.5)) > 0) {
        CW[h,jj] = window2 # If there is a a value that is more than 150% the dynamic range then ignore entire block.
      }
      absrange = abs(maxwacc - minwacc)
      if (is.numeric(absrange) == TRUE & is.numeric(sdwacc) == TRUE & is.na(sdwacc) == FALSE) {
        if (sdwacc < sdcriter) {
          if (absrange < racriter) {
            NW[h,jj] = 1
          }
        }
      } else {
        NW[h,jj] = 1 # if sdwacc, maxwacc, or minwacc could not be derived then label as non-wear
      }
    }
    CW = CW / (window2) #changed 30-1-2012, was window*sf
    if (length(params_rawdata[["rmc.col.wear"]]) == 0) {
      NWav[h] = (NW[h,1] + NW[h,2] + NW[h,3]) #indicator of non-wear
    }
    CWav[h] = max(c(CW[h,1],CW[h,2],CW[h,3])) #indicator of clipping
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
  # Revise edges of nonwear score to find the change from wear to nonwear by ws3 seconds
  look4NWedges = NWav > 0
  NWedges = diff(look4NWedges)
  windows2check = round(window/window2)
  NWav2 = rep(NWav, each = ws2/ws3) # nonwear score in ws3 resolution
  NWstart = which(NWedges > 0) + 1
  NWend = which(NWedges < 0)
  # Function to look for wear 2 nonwear (or vice versa) transitions
  wear2nonwear = function(data, NWedge = NULL, step, direction) {
    # function to find first step in which nonwear crit are not me
    # 1 - revise first "step" within defined nonwear)
    if (length(NWedge) == 1) {
      if (direction == "left") {from = NWedge * step; to = from + step - 1}
      if (direction == "right") {to = (NWedge * step) + 1; from = to - step}
      for (jj in 1:3) {
        if (jj == 1) {sdacc = c(); absrange = c()}
        sdacc[jj] = sd(data[from:to, jj]) > sdcriter
        absrange[jj] = abs(diff(range(data[from:to, jj]))) > racriter
      }
      score = sum(sdacc + absrange == 2)
      if (score > 1) return(list(from = from, to = to, score = score))
    }
    # 2 - if step 1 meets nonwear, then look to closest "step" which does not
    if (length(NWedge) == 1) {
      if (direction == "left") {from = (NWedge * step) - step; to = (NWedge * step) - 1}
      if (direction == "right") {from = (NWedge * step) + 1; to = (NWedge * step) + step}
    } else {
      if (direction == "left") {from = NWedge[2] - step; to = NWedge[2] - 1}
      if (direction == "right") {from = NWedge[1]; to = NWedge[1] + step - 1}
    }
    transition = 0
    while (transition == 0) {
      # safe check for begining and end of recording
      if (from < 1) return(list(from = 1, to = to, score = score))
      if (to > nrow(data)) return(list(from = from, to = nrow(data), score = score))
      for (jj in 1:3) {
        if (jj == 1) {sdacc = c(); absrange = c()}
        sdacc[jj] = sd(data[from:to, jj]) > sdcriter
        absrange[jj] = abs(diff(range(data[from:to, jj]))) > racriter
      }
      score = sum(sdacc + absrange == 2)
      if (score > 1) {
        transition = 1
        return(list(from = from, to = to, score = score))
      } else { # redefine from and to for next iteration
        if (direction == "left") {to = from - 1; from = from - step}
        if (direction == "right") {from = to + 1; to = to + step}
      }
    }
  }
  # Check start points (wear to nonwear)
  if (length(NWstart) > 0) {
    for (NWstart_i in NWstart) {
      window2block = wear2nonwear(data, NWedge = NWstart_i, step = window2, direction = "left")
      window1block = wear2nonwear(data, NWedge = c(window2block$from, window2block$to), step = window3, direction = "left")
      ws3_0 = (window1block$from + 1) / window3
      ws3_1_tmp = which(NWav2 > 0)
      ws3_1 = min(ws3_1_tmp[ws3_1_tmp > ws3_0]) - 1
      NWav2[ws3_0:ws3_1] = window1block$score
    }
  }
  # Check end points (nonwear to wear)
  if (length(NWend) > 0) {
    for (NWend_i in NWend) {
      window2block = wear2nonwear(data, NWedge = NWend_i, step = window2, direction = "right")
      window1block = wear2nonwear(data, NWedge = c(window2block$from, window2block$to), step = window3, direction = "right")
      ws3_1 = (window1block$to) / window3
      ws3_0_tmp = which(NWav2 > 0)
      ws3_0 = max(ws3_0_tmp[ws3_0_tmp < ws3_1]) + 1
      NWav2[ws3_0:ws3_1] = window1block$score
    } 
  }
  return(list(NWav = NWav, NWav2 = NWav2, CWav = CWav, nmin = nmin))
}