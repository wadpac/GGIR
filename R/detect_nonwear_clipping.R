detect_nonwear_clipping = function(data = c(), windowsizes = c(900, 3600), sf = 100,
                                   clipthres = 7.5, sdcriter = 0.013, racriter = 0.05,
                                   params_rawdata = params_rawdata) {
  ws2 = windowsizes[1]; ws = windowsizes[2]
  window2 = ws2 * sf #window size in samples
  window = ws * sf #window size in samples
  nmin = floor(nrow(data)/(window2)) # nmin = minimum number of windows that fit in this block of data
  CW = NW = matrix(0,nmin,3) #CW is clipping, NW is non-wear
  TS1W = TS2W = TS3W = TS4W = TS5W = TS6W = TS7W = CWav = NWav = matrix(0, nmin, 1)
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
      wearTable = table(wearcol[(1 + hoc1):hoc2], useNA = FALSE)
      NWav[h,1] = as.logical(tail(names(sort(wearTable)), 1)) * 3 # times 3 to simulate heuristic approach
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
      NWav[h,1] = (NW[h,1] + NW[h,2] + NW[h,3]) #indicator of non-wear
    }
    CWav[h,1] = max(c(CW[h,1],CW[h,2],CW[h,3])) #indicator of clipping
  }
  # Revise edges of nonwear score
  # they might be imputed in g.imputeTimegaps and not revised for nonwear
  # browser()
  look4NWedges = NWav > 0
  NWedges = diff(look4NWedges)
  NWstarts = which(NWedges > 0) + 1
  NWends = which(NWedges < 0)
  windows2check = round(window/window2)
  # check startpoint of nonwears
  for (start_i in NWstarts) {
    for (wi in 1:windows2check) {
      if (wi == 1) {
        to = (start_i * ws2 * sf) - 1
        from = (start_i * ws2 * sf) - window2 - 1
      }
      if (from < 1) from = 1 # in case the nonwear occurs in the very begining
      if (length(from:to) < window2) break # break if we are getting too close to the begining of the file
      sdacc_i = apply(data[from:to,], MARGIN = 2, FUN = sd)
      NWav[(start_i - wi)] = sum(sdacc_i < sdcriter)
      if (sum(sdacc_i < sdcriter) == 0) break # break if the end of nonwear has been found
      to = from - 1
      from = from - window2
    }
  }
  # check end point of nonwears
  for (end_i in NWends) {
    for (wi in 1:windows2check) {
      if (wi == 1) {
        from = (end_i * ws2 * sf) + 1
        to = (end_i * ws2 * sf) + 1 + window2
      }
      if (to > nrow(data)) to = nrow(data) # in case the nonwear occurs in the very end
      if (length(from:to) < window2) break # break if we are getting too close to the end of the file
      sdacc_i = apply(data[from:to,], MARGIN = 2, FUN = sd)
      NWav[(end_i + wi)] = sum(sdacc_i < sdcriter)
      from = to + 1
      to = to + window2
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