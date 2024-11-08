g.weardec = function(M,wearthreshold, ws2, nonWearEdgeCorrection = TRUE) {
  metalong = M$metalong
  nsi = which(colnames(metalong) == "nonwearscore")
  csi = which(colnames(metalong) == "clippingscore")
  NLongEpochs = nrow(metalong)
  clipsig = as.numeric(as.matrix(metalong[,csi]))
  # clipsig is a value between 0 and 1 for each long window (default 15 minutes) 
  # and indicates the ratio of datapoints in the window where signal clips
  LC = length(which(clipsig > 0.05))
  turnoffclip = which(clipsig > 0.3)
  LC2 = length(turnoffclip)
  turnoffnonw = which(as.numeric(as.matrix(metalong[,nsi])) >= wearthreshold)
  # step 1: identify islands of invalid data
  r1 = r2 = r3 = matrix(0, nrow(metalong), 1)
  r1[turnoffnonw] = 1 # non-weartime
  r2[turnoffclip] = 1 # clipping
  r1 = c(0, r1, 0)
  r2 = c(0, r2, 0)
  r3 = c(0, r3, 0)
  additionalNonWearDetection = function(s1, s3, ws2, EdgeCorrection = FALSE) {
    ch1 = which(diff(s1) == 1) + 1 # indices of start wear
    ch2 = which(diff(s1) == -1) + 1 # indiced of start non-wear)
    if (length(ch1) > 1) { # there are at least 2 non-wear periods
      wear = matrix(0, (length(ch1) - 1), 3) #characteristics of all wear period in between non-wear periods
      for (weari in 1:(length(ch1) - 1)) {
        wear[weari,1] = abs(ch1[weari + 1] - ch2[weari]) # length of wear
        wear[weari,2] = abs(ch2[weari + 1] - ch1[weari + 1]) # length of non-wear after it
        wear[weari,3] = abs(ch2[weari] - ch1[weari]) # length of non-wear before it  
        if (wear[weari, 1] < (6 / (ws2 / 3600)) &
            (wear[weari, 1] / (wear[weari, 2] + wear[weari, 3])) < 0.3) {
          # turning wear into non-wear if it is less then 0.3 of nearby non-wear
          # and wear lasts for less than 6 hours (24 blocks)
          s3[ch2[weari]:(ch1[weari + 1] - 1)] = 1
        }
        if (wear[weari, 1] < (3 / (ws2 / 3600)) &
            (wear[weari, 1] / (wear[weari, 2] + wear[weari, 3])) < 0.8) {
          # turning wear into non-wear if it is less then 0.8 of nearby non-wear
          # and wear lasts for less than 3 hours (12 blocks)
          s3[ch2[weari]:(ch1[weari + 1] - 1)] = 1
        }
        if (ch1[weari] > (NLongEpochs - (24 / (ws2 / 3600)))) {
          # for last 24 hours of the data
          if (wear[weari, 1] < (3 / (ws2 / 3600)) &
              wear[weari, 3] > (1 / (ws2 / 3600))) {
            # only if wear is shorter than 3 hours and non-wear before it > 1 hour
            s3[ch2[weari]:(ch1[weari + 1] - 1)] = 1
          }
        }
      }
    }
    if (nonWearEdgeCorrection == TRUE) {
      if (length(ch1) > 0) {
        if (ch1[1] < (3 / (ws2 / 3600)) &
            ch1[1] > 1) {
          # did first non-wear period started after the start of the measurement 
          # but before the start of the fourth measurement hour? If yes then 
          # consider entire first three hours as non-wear (probably monitor set
          # up and then put aside)
          s3[1:(ch1[1] - 1)] = 1
        }
        if (ch2[length(ch2)] > (length(s3) - (3 / (ws2 / 3600))) &
            ch2[length(ch2)] != length(s3)) {
          # did measurement not end with non-wear but was there a period of 
          # non-wear that ended in the last 3 hours of the measurement? If yes, 
          # then consider entire final 3 hours as nonwear (probably monitor download)
          s3[ch2[length(ch2)]:length(s3)] = 1
        }
      }
    }
    invisible(list(r1 = s1, r3 = s3))
  }
  NEWlabels = additionalNonWearDetection(s1 = r1, s3 = r3, ws2, EdgeCorrection = nonWearEdgeCorrection)
  r1 = NEWlabels$r1
  r3 = NEWlabels$r3
  r1 = r1[-c(1, length(r1))]
  r2 = r2[-c(1, length(r2))]
  r3 = r3[-c(1, length(r3))]
  #===========================================================================================================
  #===========================================================================================================
  for (i in 1:2) { #for 2 additional iterative phases
    # redo based on newly identified blocks and remove shorter periods of 
    # non-wear surround by low acceleration
    r1b = r3 + r1
    r1b[which(r1b > 1)] = 1
    r1b = c(0, r1b, 0)
    r3b = c(0, r3, 0)
    NEWlabels = additionalNonWearDetection(s1 = r1b, s3 = r3b, ws2, EdgeCorrection = FALSE)
    r1b = NEWlabels$r1
    r3b = NEWlabels$r3
    r3 = r3b[-c(1, length(r3b))]
  }
  invisible(list(
    r1 = r1,
    r2 = r2,
    r3 = r3,
    LC = LC,
    LC2 = LC2
  ))
}