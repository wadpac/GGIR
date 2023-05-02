detect_nonwear_clipping = function(data = c(), windowsizes = c(5, 900, 3600), sf = 100,
                                   clipthres = 7.5, sdcriter = 0.013, racriter = 0.05,
                                   nonwear_approach = "old",
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
      if (nonwear_approach == "old") {
        NWflag = h
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
      
      } else if (nonwear_approach == "new") {
        # long-epoch windows to flag (nonwear)
        NWflag = h:(h + window/window2 - 1)
        if (NWflag[length(NWflag)] > nmin) NWflag = NWflag[-which(NWflag > nmin)]
        # window to check (not aggregated values)
        hoc1 = h*window2 - window2 + 1
        hoc2 = hoc1 + window - 1
        if (hoc2 > nrow(data)) {
          hoc2 = nrow(data)
        }
      }
      # ---
      if (length(params_rawdata[["rmc.col.wear"]]) > 0) {
        wearcol = as.character(data[, which(colnames(data) == "wear")])
        suppressWarnings(storage.mode(wearcol) <- "logical")
        wearTable = table(wearcol[(1 + hoc1):hoc2], useNA = FALSE)
        NWav[h] = as.logical(tail(names(sort(wearTable)), 1)) * 3 # times 3 to simulate heuristic approach
      }
      for (jj in  1:3) {
        # Clipping
        CW[h,jj] = length(which(abs(as.numeric(data[(1 + cliphoc1):cliphoc2,jj])) > clipthres))
        if (length(which(abs(as.numeric(data[(1 + cliphoc1):cliphoc2,jj])) > clipthres*1.5)) > 0) {
          CW[h,jj] = window2 # If there is a a value that is more than 150% the dynamic range then ignore entire block.
        }
        # Non-wear
        #hoc1 & hoc2 = edges of windows
        #window is bigger& window2 is smaller one
        sdwacc = sd(as.numeric(data[(1 + hoc1):hoc2,jj]), na.rm = TRUE)
        maxwacc = max(as.numeric(data[(1 + hoc1):hoc2,jj]), na.rm = TRUE)
        minwacc = min(as.numeric(data[(1 + hoc1):hoc2,jj]), na.rm = TRUE)
        absrange = abs(maxwacc - minwacc)
        if (is.numeric(absrange) == TRUE & is.numeric(sdwacc) == TRUE & is.na(sdwacc) == FALSE) {
          if (sdwacc < sdcriter) {
            if (absrange < racriter) {
              NW[NWflag,jj] = 1
            }
          }
        } else {
          NW[NWflag,jj] = 1 # if sdwacc, maxwacc, or minwacc could not be derived then label as non-wear
        }
      }
      CW = CW / (window2) #changed 30-1-2012, was window*sf
      if (length(params_rawdata[["rmc.col.wear"]]) == 0) {
        NWav[h] = (NW[h,1] + NW[h,2] + NW[h,3]) #indicator of non-wear
      }
      CWav[h] = max(c(CW[h,1],CW[h,2],CW[h,3])) #indicator of clipping
    }
  } else if (nonwear_approach == "sapply") {
    # clipping detection
    getclipping = function(data, cliphoc, window, clipthres) {
      # function to get criteria for nonwear for a single axis
      # data: numeric vector with one-axis data for this chunk
      # cliphoc: integer with starting points for windows
      # window: window length (= window for clipping)
      # clipthres: criteria for clipping
      
      sapply(cliphoc, function(cliphoc) {
        sum(abs(data[cliphoc:(cliphoc + window - 1)])) > clipthres
      })
    }
    
    cliphoc = seq(1, nrow(data), by = window2)
    CW = sapply(1:3, function(jj) cbind(
      getclipping(data[, jj], cliphoc, window2, clipthres)))
    CWav = apply(CW/window2, 1, max)
    
    # Nonwear detection --------
    getnonwear = function(data, nwhoc, window, sdcriter, racriter) {
      # function to get criteria for nonwear for a single axis
      # data: numeric vector with one-axis data for this chunk
      # nwhoc: integer with starting points for windows
      # window: window length (= window for nonwear)
      # sdcriter: criteria for standard deviation
      # racriter: criteria for range of accelerations
      
      # get standard deviations below sdcriter
      SDs = sapply(nwhoc, function(nwhoc) {
        endpoint = nwhoc + window - 1
        if (endpoint > length(data)) window = length(nwhoc:length(data))
        sd(data[nwhoc:(nwhoc + window - 1)]) < sdcriter
      })
      
      # get ranges below racriter
      RAs = sapply(nwhoc, function(nwhoc) {
        endpoint = nwhoc + window - 1
        if (endpoint > length(data)) window = length(nwhoc:length(data))
        diff(range(data[nwhoc:(nwhoc + window - 1)])) < racriter
      })
      
      # Nonwear indicator
      ifelse(SDs == TRUE & RAs == TRUE, 1, 0) 
    }
    
    nwhoc = seq(1, nrow(data), by = window2)
    NW = sapply(1:3, function(jj) cbind(
      getnonwear(data[, jj], nwhoc, window, sdcriter, racriter)))
    NWav = rowSums(NW)
    
  } else if (nonwear_approach == "vapply") {
    # clipping detection
    getclipping = function(data, cliphoc, window, clipthres) {
      # function to get criteria for nonwear for a single axis
      # data: numeric vector with one-axis data for this chunk
      # cliphoc: integer with starting points for windows
      # window: window length (= window for clipping)
      # clipthres: criteria for clipping
      
      vapply(cliphoc, FUN.VALUE = numeric(length = 1), 
             FUN = function(cliphoc) {
               sum(abs(data[cliphoc:(cliphoc + window - 1)]) > clipthres) / window
             })
    }
    
    cliphoc = seq(1, nrow(data), by = window2)
    CW = vapply(1:3, FUN.VALUE = numeric(length = length(cliphoc)), 
                FUN = function(jj) cbind(
                  getclipping(data[, jj], cliphoc, window2, clipthres)))
    CWav = apply(CW, 1, max)
    
    # Nonwear detection --------
    getnonwear = function(data, nwhoc, window, sdcriter, racriter) {
      # function to get criteria for nonwear for a single axis
      # data: numeric vector with one-axis data for this chunk
      # nwhoc: integer with starting points for windows
      # window: window length (= window for nonwear)
      # sdcriter: criteria for standard deviation
      # racriter: criteria for range of accelerations
      
      # get standard deviations below sdcriter
      SDs = vapply(nwhoc, FUN.VALUE = logical(length = 1),
                   FUN = function(nwhoc) {
                     endpoint = nwhoc + window - 1
                     if (endpoint > length(data)) window = length(nwhoc:length(data))
                     sd(data[nwhoc:(nwhoc + window - 1)]) < sdcriter
                   })
      
      # get ranges below racriter
      RAs = vapply(nwhoc, FUN.VALUE = logical(length = 1),
                   FUN = function(nwhoc) {
                     endpoint = nwhoc + window - 1
                     if (endpoint > length(data)) window = length(nwhoc:length(data))
                     diff(range(data[nwhoc:(nwhoc + window - 1)])) < racriter
                   })
      
      # Nonwear indicator
      ifelse(SDs == TRUE & RAs == TRUE, 1, 0) 
    }
    
    nwhoc = seq(1, nrow(data), by = window2)
    NW = vapply(1:3, FUN.VALUE = double(length = length(nwhoc)),
                FUN = function(jj) cbind(
                  getnonwear(data[, jj], nwhoc, window, sdcriter, racriter)))
    NWav = rowSums(NW)
    
  } else if (nonwear_approach == "data_table") {
    DT = data.table(data)
    
    # clipping detection ----
    clip = function(x, clipthres) sum(abs(x) > clipthres)
    
    DT$index = rep(1:nmin, each = window2)
    CW = DT[, lapply(.SD, clip, clipthres = clipthres), by = .(index)] / window2
    CWav = apply(CW[, -1], 1, max)
    
    # Nonwear detection --------
     getnonwear = function(data, nwhoc, window, sdcriter, racriter) {
      # function to get criteria for nonwear for a single axis
      # data: numeric vector with one-axis data for this chunk
      # nwhoc: integer with starting points for windows
      # window: window length (= window for nonwear)
      # sdcriter: criteria for standard deviation
      # racriter: criteria for range of accelerations
      
      # get standard deviations below sdcriter
      SDs = vapply(nwhoc, FUN.VALUE = logical(length = 1),
                   FUN = function(nwhoc) {
                     endpoint = nwhoc + window - 1
                     if (endpoint > length(data)) window = length(nwhoc:length(data))
                     sd(data[nwhoc:(nwhoc + window - 1)]) < sdcriter
                   })
      
      # get ranges below racriter
      RAs = vapply(nwhoc, FUN.VALUE = logical(length = 1),
                   FUN = function(nwhoc) {
                     endpoint = nwhoc + window - 1
                     if (endpoint > length(data)) window = length(nwhoc:length(data))
                     diff(range(data[nwhoc:(nwhoc + window - 1)])) < racriter
                   })
      
      # Nonwear indicator
      ifelse(SDs == TRUE & RAs == TRUE, 1, 0) 
    }
    
    nwhoc = seq(1, nrow(data), by = window2)
    NW = vapply(1:3, FUN.VALUE = double(length = length(nwhoc)),
                FUN = function(jj) cbind(
                  getnonwear(data[, jj], nwhoc, window, sdcriter, racriter)))
    NWav = rowSums(NW)
    
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
