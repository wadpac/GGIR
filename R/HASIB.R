HASIB = function(HASIB.algo = "vanHees2015", timethreshold = c(), anglethreshold = c(), 
                 time = c(), anglez = c(), ws3 = c(), 
                 zeroCrossingCount = c(), NeishabouriCount = c(),
                 activity = NULL, oakley_threshold = NULL) {
  epochsize = ws3 #epochsize in seconds
  sumPerWindow = function(x, epochsize, summingwindow = 60) {
    x2 = cumsum(c(0, x))
    select = seq(1, length(x2), by = summingwindow/epochsize)
    x3 = diff(x2[select])
  }
  create_rollfun_mat = function(x, Ncol) {
    Nz = length(x)
    # Generate matrix to ease applying rolling function
    x_matrix = matrix(0, Nz + (Ncol - 1), Ncol)
    for (jj in 1:Ncol) {
      x_matrix[jj:(Nz + jj - 1), jj] = x
      if (jj > 1) {
        x_matrix[1:(jj - 1), jj] = x[1]
      }
      if (jj < Ncol) {
        x_matrix[((Nz - ((Ncol - 1) - jj)):Nz) + (Ncol - 1), jj] = tail(x, 1)
      }
    }
    return(x_matrix)
  }
  
  reformat_output = function(x, time, new_epochsize = 5, current_epochsize = 60) {
    # resample to original resolution
    x = rep(x, each = (current_epochsize / new_epochsize))
    # resize to match length of time
    if (length(x) < length(time)) {
      x = c(x,rep(0, length(time) - length(x)))
    } else if (length(x) > length(time)) {
      x = x[1:length(time)]
    }
    # format as expected matrix
    sib_classification = matrix(0,length(x), 1)
    sib_classification[,1] = as.matrix(x)
    return(sib_classification)
  }
  #===============================
  Nvalues = max(length(anglez), length(zeroCrossingCount), length(NeishabouriCount),
                length(activity))
  if (HASIB.algo == "vanHees2015") { # default
    cnt = 1
    Ndefs = length(timethreshold) * length(anglethreshold)
    sib_classification = matrix(0,Nvalues, Ndefs)
    for (i in timethreshold) {
      for (j in anglethreshold) {
        sdl1 = rep(0,length(time))
        postch = which(abs(diff(anglez)) > j) #posture change of at least j degrees
        # count posture changes that happen less than once per ten minutes
        q1 = c()
        if (length(postch) > 1) {
          q1 = which(diff(postch) > (i*(60/epochsize))) #less than once per i minutes
        }
        if (length(q1) > 0) {
          for (gi in 1:length(q1)) {
            sdl1[postch[q1[gi]]:postch[q1[gi] + 1]] = 1 #periods with no posture change
          }
        } else { #possibly a day without wearing
          if (length(postch) < 10) {  #possibly a day without wearing
            sdl1[1:length(sdl1)] = 1 #periods with no posture change
          } else {  #possibly a day with constantly posture changes
            sdl1[1:length(sdl1)] = 0 #periodsposture change
          }
        }
        sib_classification[,cnt] = sdl1
        cnt = cnt + 1
      }
    }
    cnt = 1
    sib_classification = as.data.frame(sib_classification, stringsAsFactors = TRUE)
    for (i in timethreshold) {
      for (j in anglethreshold) {
        colnames(sib_classification)[cnt] = paste("T", i, "A", j, sep = "")
        cnt = cnt + 1
      }
    }
  } else if (HASIB.algo == "Sadeh1994") {
    count_types = c()
    if (length(zeroCrossingCount) > 0) count_types = "zeroCrossingCount"
    if (length(NeishabouriCount) > 0) count_types = c(count_types, "NeishabouriCount")
    sib_classification = as.data.frame(matrix(0, Nvalues, length(count_types)))
    cti = 1
    for (count_type in count_types) {
      if (count_type == "zeroCrossingCount") {
        Countpermin = sumPerWindow(zeroCrossingCount, epochsize = epochsize, summingwindow = 60)
      } else if (count_type == "NeishabouriCount") {
        Countpermin = sumPerWindow(NeishabouriCount, epochsize = epochsize, summingwindow = 60)
      }
      if (count_type != "zeroCrossingCount") {
        # because this is what ActiLife does for their counts
        # https://actigraphcorp.my.site.com/support/s/article/Where-can-I-find-documentation-for-the-Sadeh-and-Cole-Kripke-algorithms#:~:text=The%20Sadeh%20algorithm%20uses%20the%20y%2Daxis%20epoch%20data.%20If%20any%20of%20the%20epoch%20counts%20are%20over%20300%2C%20it%20reduces%20them%20to%20300.
        Countpermin = ifelse(test = Countpermin > 300, yes = 300, no = Countpermin)
      }
      Countpermin_matrix = create_rollfun_mat(Countpermin, Ncol = 11)
      Countpermin_matrix = Countpermin_matrix[11:(nrow(Countpermin_matrix) - 10),]
      CalcSadehFT = function(x) {
        MeanW5 = mean(x, na.rm = TRUE)
        SDlast = sd(x[6:11]) #last five in this matrix means columns 6:11
        NAT = length(which(x > 50 & x < 100))
        LOGact = log(x[6] + 1)
        return(data.frame(MeanW5 = MeanW5, SDlast = SDlast,
                          NAT = NAT, LOGact = LOGact))
      }
      SadehFT1 = apply(X = Countpermin_matrix, MARGIN = 1, FUN = CalcSadehFT)
      rm(Countpermin_matrix)
      SadehFT = data.frame(matrix(unlist(SadehFT1), nrow = length(SadehFT1), byrow = TRUE))
      rm(SadehFT1)
      colnames(SadehFT) = c("MeanW5", "SDlast", "NAT", "LOGact")
      # apply Sadeh algorithm
      PS = 7.601 - (0.065 * SadehFT$MeanW5) - (1.08 * SadehFT$NAT) - (0.056 * SadehFT$SDlast) - (0.703 * SadehFT$LOGact)
      PS = c(rep(-2, 5), PS) # add five zeros because first 5 epochs are not classified by the algorithm
      
      PSscores = rep(0, length(PS))
      PSsibs = which(PS >= 0) # sleep
      if (length(PSsibs) > 0) {
        PSscores[PSsibs] = 1 # sleep
      }
      # resample to original resolution and ensure length matches length of time
      sib_classification[,cti] = reformat_output(x = PSscores, time, new_epochsize = epochsize,
                                                 current_epochsize = 60)
      if (count_type == "zeroCrossingCount") colnames(sib_classification)[cti] = paste0(HASIB.algo, "_ZC")
      if (count_type == "NeishabouriCount") colnames(sib_classification)[cti] = paste0(HASIB.algo, "_Neishabouri")
      cti = cti + 1
    }
  } else if (HASIB.algo == "ColeKripke1992") {
    count_types = c()
    if (length(zeroCrossingCount) > 0) count_types = "zeroCrossingCount"
    if (length(NeishabouriCount) > 0) count_types = c(count_types, "NeishabouriCount")
    sib_classification = as.data.frame(matrix(0, Nvalues, length(count_types)))
    
    if (epochsize <= 60) {
      aggwindow = 60
    } else if (epochsize > 60) {
      stop("Cole Kripke algorithm is not designed for epochs larger than 1 minute")
    }
    cti = 1
    for (count_type in count_types) {
      # We use the algorithms on page 6 of Cole, R. J., Kripke, D. F., http://doi.org/10.1093/sleep/15.5.461
      # which corresponds to non-overlapping 1 minute epochs of activity per minute.
      # The other algorithms proposed are less clearly described:
      # - How does aggration per epoch takes place, sum or average? Without knowing this it is hard
      # to oversee whether it matters whether we use 2 or 5 seconds as input. 
      # - Are maximum 10 seconds converted back to the unit of counts per minute?
      # For now we assume that the unit is counts per 10 seconds
      if (count_type == "zeroCrossingCount") {
        CountperWindow = sumPerWindow(zeroCrossingCount, epochsize = epochsize, summingwindow = aggwindow)
      } else if (count_type == "NeishabouriCount") {
        CountperWindow = sumPerWindow(NeishabouriCount, epochsize = epochsize, summingwindow = aggwindow)
      }
      # Convert unit to counts per minute if aggwindow is not 60
      if (aggwindow != 60) CountperWindow = CountperWindow * (60/aggwindow)
      # Convert back to Counts per 10 seconds as impression is that this provides more plausible output
      CountperWindow = CountperWindow / 6 
      # Prepare matrix to ease applying weights
      CountperWindow_matrix = create_rollfun_mat(CountperWindow, Ncol = 7)
      CountperWindow_matrix = CountperWindow_matrix[7:(nrow(CountperWindow_matrix) - 6),]
      # Apply weights
      CKweights = c(67, 74, 230, 76, 58, 54, 106) # reversed to match order of matrix
      PS = 0.001 * rowSums(CKweights * CountperWindow_matrix)
      # Add 4 wake score because first 4 epochs are not classified by the algorithm
      PS = c(rep(2, 4), PS) 
      # Not applying rescoring as described by Cole 1992, because accuracy improvements
      # were reported to be marginal which makes the added complexity not justified
      PSscores = rep(0, length(PS))
      PSsibs = which(PS < 1) # sleep
      if (length(PSsibs) > 0) {
        PSscores[PSsibs] = 1 #sleep
      }
      # re sample to original resolution and ensure length matches length of time
      sib_classification[,cti] = reformat_output(x = PSscores, time, new_epochsize = epochsize,
                                                 current_epochsize = 60)
      if (count_type == "zeroCrossingCount") colnames(sib_classification)[cti] = paste0(HASIB.algo, "_ZC")
      if (count_type == "NeishabouriCount") colnames(sib_classification)[cti] = paste0(HASIB.algo, "_Neishabouri")
      cti = cti + 1
    }
  } else if (HASIB.algo == "Galland2012") {  
    count_types = c()
    if (length(zeroCrossingCount) > 0) count_types = "zeroCrossingCount"
    if (length(NeishabouriCount) > 0) count_types = c(count_types, "NeishabouriCount")
    sib_classification = as.data.frame(matrix(0, Nvalues, length(count_types)))
    cti = 1
    for (count_type in count_types) {
      # Aggregate per minute
      if (count_type == "zeroCrossingCount") {
        Countpermin = sumPerWindow(zeroCrossingCount, epochsize = epochsize, summingwindow = 60)
      } else if (count_type == "NeishabouriCount") {
        Countpermin = sumPerWindow(NeishabouriCount, epochsize = epochsize, summingwindow = 60)
      }
      mean_nonzero = mean(Countpermin[which(Countpermin != 0)])
      CountScaled = Countpermin / mean_nonzero
      CountScaled_matrix = create_rollfun_mat(CountScaled, Ncol = 7)
      # In next line I use intentionally a reversed order relative to Galland publication
      # because our matrix is reversed relative to paper
      WeightCounts = abs(rowSums(CountScaled_matrix * c(1,3,5:1))) * 2.7 
      WeightCounts = WeightCounts[-c(1:2)] # remove first two time stamps, to align with 5th element (7-5=2)
      GallandScore = rep(0, length(WeightCounts))
      GallandScore[which(WeightCounts < 1)] = 1
      sib_classification[,cti] = reformat_output(x = GallandScore, time, new_epochsize = epochsize,
                                                 current_epochsize = 60)
      if (count_type == "zeroCrossingCount") colnames(sib_classification)[cti] = paste0(HASIB.algo, "_ZC")
      if (count_type == "NeishabouriCount") colnames(sib_classification)[cti] = paste0(HASIB.algo, "_Neishabouri")
      cti = cti + 1
    }
  } else if (HASIB.algo == "Oakley1997") {
    # Sleep Detection based on description in
    # Information bulletin no.3 sleep algorithms by Cambrige Neurotechnologies
    if (epochsize > 60) {
      stop("GGIR only facilitates the Oakley algorithm for epoch sizes up to 1 minute.", call. = FALSE)
    }
    count_types = c()
    if (length(zeroCrossingCount) > 0) count_types = "zeroCrossingCount"
    if (length(NeishabouriCount) > 0) count_types = c(count_types, "NeishabouriCount")
    sib_classification = as.data.frame(matrix(0, Nvalues, length(count_types)))
    cti = 1
    for (count_type in count_types) {
      if (epochsize %in% c(15, 30, 60) == FALSE) {
        # aggregate to 5, 30 or 60 seconds
        if (epochsize < 15 && 15 %% epochsize == 0) {
          aggwindow = 15
        } else if (epochsize < 30 && 30 %% epochsize == 0) {
          aggwindow = 30
        } else {
          aggwindow = 60
        }
        if (count_type == "zeroCrossingCount") {
          counts = sumPerWindow(zeroCrossingCount, epochsize = epochsize, summingwindow = aggwindow)
        } else if (count_type == "NeishabouriCount") {
          counts = sumPerWindow(NeishabouriCount, epochsize = epochsize, summingwindow = aggwindow)
        }
      } else {
        counts = zeroCrossingCount
        aggwindow = epochsize
      }
      # Oakley coefficients are epoch size specific
      if (aggwindow == 15) {
        oakley_coef = c(rep(0.04, 4), rep(0.20, 4), 4, rep(0.20, 4), rep(0.04, 4))
      } else if (aggwindow == 30) {
        oakley_coef = c(0.04, 0.04, 0.20, 0.20, 2.0, 0.2, 0.2, 0.04, 0.04)
      } else if (aggwindow == 60) {
        oakley_coef = c(0.04, 0.2, 1, 0.20, 0.04)
      }
      # Apply coefficients to estimate sleep
      Noak = length(oakley_coef)
      counts_matrix = create_rollfun_mat(counts, Ncol = Noak)
      counts_matrix = counts_matrix[Noak:(nrow(counts_matrix) - (Noak - 1)),]
      PS = rowSums(counts_matrix * oakley_coef)
      rm(counts_matrix)
      PSscores = rep(0, length(PS))
      PSsibs = which(PS <= oakley_threshold)
      if (length(PSsibs) > 0) {
        PSscores[PSsibs] = 1 # sleep
      }
      # Resample to original resolution and ensure length matches length of time
      sib_classification[,cti] = reformat_output(x = PSscores, time,
                                                 new_epochsize = epochsize,
                                                 current_epochsize = aggwindow)
      if (count_type == "zeroCrossingCount") colnames(sib_classification)[cti] = paste0(HASIB.algo, "_ZC")
      if (count_type == "NeishabouriCount") colnames(sib_classification)[cti] = paste0(HASIB.algo, "_Neishabouri")
      cti = cti + 1
    }
  } else if (HASIB.algo == "NotWorn") {  
    # For the rare study protocols where sensor is not worn during the night
    # there is no point in looking at sleep. Nonetheless for the GGIR 
    # framework to work we need some estimate. This is not ideal but seems a pragmatic work
    # around to avoid having to completely redesign GGIR just for those studies.
    
    sib_classification = as.data.frame(matrix(0, Nvalues, 1))
    activity2 = zoo::rollmax(x = activity, k = 300 / epochsize, fill = 1)
    # ignore zeros because in ActiGraph with many zeros it skews the distribution
    nonzero = which(activity2 != 0)
    if (length(nonzero) > 0) {
      activityThreshold = sd(activity2[nonzero], na.rm = TRUE) * 0.05
      if (activityThreshold < min(activity)) {
        activityThreshold = quantile(activity2[nonzero], probs = 0.1)
      }
    } else {
      activityThreshold = 0
    }
    zeroMovement = which(activity2 <= activityThreshold)
    if (length(zeroMovement) > 0) {
      sib_classification[zeroMovement, 1] = 1
    }
    colnames(sib_classification) = "NotWorn"
  }
  return(sib_classification)
}