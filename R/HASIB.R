HASIB = function(HASIB.algo = "vanHees2015", timethreshold = c(), anglethreshold = c(), 
                 time = c(), anglez = c(), ws3 = c(), zeroCrossingCount = c(), BrondCount = c()) {
  epochsize = ws3 #epochsize in seconds
  sumPerMinute = function(x, epochsize) {
    x2 = cumsum(c(0, x))
    select = seq(1, length(x2), by = 60/epochsize)
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
  
  reformat_output = function(x, time, epochsize) {
    # resample to original resolution
    x = rep(x, each = (60/epochsize))
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
  Nvalues = max(length(anglez),length(zeroCrossingCount), length(BrondCount))
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
    if (length(BrondCount) > 0) count_types = c(count_types, "BrondCount")
    sib_classification = as.data.frame(matrix(0, Nvalues, length(count_types)))
    cti = 1
    for (count_type in count_types) {
      if (count_type == "zeroCrossingCount") {
        Countpermin = sumPerMinute(zeroCrossingCount, epochsize = epochsize)
      } else {
        Countpermin = sumPerMinute(BrondCount, epochsize = epochsize)
      }
      Countpermin_matrix = create_rollfun_mat(Countpermin, Ncol = 11)
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
      PSscores = rep(0, length(PS))
      PSsibs = which(PS >= 0)
      if (length(PSsibs) > 0) {
        PSscores[PSsibs] = 1
      }
      # resample to original resolution and ensure length matches length of time
      sib_classification[,cti] = reformat_output(x = PSscores, time, epochsize)
      colnames(sib_classification)[cti] = ifelse(test = count_type == "BrondCount",
                                                 yes = paste0(HASIB.algo, "_Brond"),no=paste0(HASIB.algo, "_ZC"))
      cti = cti + 1
    }
  } else if (HASIB.algo == "Galland2012") {  
    count_types = c()
    if (length(zeroCrossingCount) > 0) count_types = "zeroCrossingCount"
    if (length(BrondCount) > 0) count_types = c(count_types, "BrondCount")
    sib_classification = as.data.frame(matrix(0, Nvalues, length(count_types)))
    cti = 1
    for (count_type in count_types) {
      # Aggregate per minute
      if (count_type == "zeroCrossingCount") {
        Countpermin = sumPerMinute(zeroCrossingCount, epochsize = epochsize)
      } else {
        Countpermin = sumPerMinute(BrondCount, epochsize = epochsize)
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
      sib_classification[,cti] = reformat_output(x = GallandScore, time, epochsize)
      colnames(sib_classification)[cti] = ifelse(test = count_type == "BrondCount",
                                                 yes = paste0(HASIB.algo, "_Brond"),no=paste0(HASIB.algo, "_ZC"))
      cti = cti + 1
    }
  }
  return(sib_classification)
}