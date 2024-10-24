detect_nonwear_clipping = function(data = c(), windowsizes = c(5, 900, 3600), sf = 100,
                                   clipthres = 7.5, sdcriter = 0.013, racriter = 0.05,
                                   nonwear_approach = "2013",
                                   params_rawdata = c()) {
  MediumEpochSize = windowsizes[2] * sf # Medium window size in samples
  LongEpochSize = windowsizes[3] * sf #Long window size in samples
  NMediumEpochs = floor(nrow(data)/(MediumEpochSize)) # Number of non-overlapping Medium Epochs that fit in data
  # Initialise matrices to collect clipping and nonwear info per axis
  ClipLog = NonwearLog = matrix(0,NMediumEpochs,3)
  ClipLogCollapsed = NonwearLogCollapsed = rep(0, NMediumEpochs) # averages
  # Define minimum number of Medium epochs that need to be present in Long Epoch
  minimumEpochCount = ((LongEpochSize/MediumEpochSize)/2) + 1
  
  if (nonwear_approach %in% c("2013", "2023")) {
    for (h in 1:NMediumEpochs) {
      # define start and end index for clipping detection (clipstart and clipend)
      clipstart = (((h - 1) * MediumEpochSize) + MediumEpochSize * 0.5 ) - MediumEpochSize * 0.5
      clipend = (((h - 1) * MediumEpochSize) + MediumEpochSize * 0.5 ) + MediumEpochSize * 0.5
      # define start and end index for nonwear detection (nwstart and nwend)
      if (nonwear_approach == "2013") {
        NonwearLogflag = h
        if (h <= minimumEpochCount) {
          nwstart = 1
          nwend = LongEpochSize
        } else if (h >= (NMediumEpochs - minimumEpochCount)) {
          nwstart = (NMediumEpochs - minimumEpochCount) * MediumEpochSize
          nwend = NMediumEpochs * MediumEpochSize
        } else if (h > minimumEpochCount & h < (NMediumEpochs - minimumEpochCount)) {
          nwstart = (((h - 1) * MediumEpochSize) + MediumEpochSize * 0.5 ) - LongEpochSize * 0.5
          nwend = (((h - 1) * MediumEpochSize) + MediumEpochSize * 0.5 ) + LongEpochSize * 0.5
        }
      } else if (nonwear_approach == "2023") {
        NonwearLogflag = h:(h + LongEpochSize/MediumEpochSize - 1)
        if (NonwearLogflag[length(NonwearLogflag)] > NMediumEpochs) NonwearLogflag = NonwearLogflag[-which(NonwearLogflag > NMediumEpochs)]
        # LongEpochSize to check (not aggregated values)
        nwstart = h * MediumEpochSize - MediumEpochSize
        nwend = nwstart + LongEpochSize
        if (nwend > nrow(data)) {
          nwend = nrow(data)
        }
      }
      # ---
      xyzCol = which(colnames(data) %in% c("x", "y", "z"))
      for (jj in seq(3)) {
        # Detect clipping
        aboveThreshold = which(abs(data[(1 + clipstart):clipend, xyzCol[jj]]) > clipthres)
        ClipLog[h, jj] = length(aboveThreshold)
        if (length(aboveThreshold) > 0) {
          if (length(which(abs(data[c((1 + clipstart):clipend)[aboveThreshold],  xyzCol[jj]]) > clipthres * 1.5)) > 0) {
            ClipLog[h, jj] = MediumEpochSize # If there is a a value that is more than 150% the dynamic range then ignore entire block.
          }
        }
        # Detect nonwear
        if (nonwear_approach == "2013") {
          indices = (1 + nwstart):nwend
        } else if (nonwear_approach == "2023") {
          indices = seq((1 + nwstart), nwend, by = ceiling(sf / 5))
        }
        maxwacc = max(data[indices, xyzCol[jj]], na.rm = TRUE)
        minwacc = min(data[indices, xyzCol[jj]], na.rm = TRUE)
        absrange = abs(maxwacc - minwacc)
        if (absrange < racriter) {
          sdwacc = sd(data[indices, xyzCol[jj]], na.rm = TRUE)
          if (sdwacc < sdcriter) {
            NonwearLog[NonwearLogflag,jj] = 1
          }
        }
      }
      # Summarise clipping
      ClipLog = ClipLog / (MediumEpochSize) # Express as fraction of epoch length
      ClipLogCollapsed[h] = max(c(ClipLog[h, 1], ClipLog[h, 2], ClipLog[h, 3])) #indicator of clipping
      
      # Summarise nonwear
      if ("wear" %in% colnames(data)) {
        wearTable = table(data[(1 + nwstart):nwend, "wear"], useNA = "no")
        NonwearLogCollapsed[h] = as.numeric(tail(names(sort(wearTable)), 1)) * 3 # times 3 to simulate heuristic approach
      }
      if (!("wear" %in% colnames(data))) {
        NonwearLogCollapsed[h] = (NonwearLog[h,1] + NonwearLog[h,2] + NonwearLog[h,3]) #indicator of non-wear
      }
      
    }
  }
  # In NonwearLogCollapsed: single 1's surrounded by 2's or 3's --> 2 (so it is considered nonwear)
  ones = which(NonwearLogCollapsed == 1)
  if (length(ones) > 0) {
    for (one_i in ones) {
      if (one_i - 1 < 1) next # skip if we are in the very beginning
      if (one_i + 1 > length(NonwearLogCollapsed)) next # skip if we are in the very end
      if (NonwearLogCollapsed[one_i - 1] > 1 & NonwearLogCollapsed[one_i + 1] > 1) NonwearLogCollapsed[one_i] = 2
    }
  }
  return(list(NWav = NonwearLogCollapsed, CWav = ClipLogCollapsed, nmin = NMediumEpochs))
}
