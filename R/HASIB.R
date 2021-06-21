HASIB = function(HASIB.algo = "vanHees2015", timethreshold=c(), anglethreshold=c(), 
                 time=c(), anglez=c(), ws3=c(), zeroCrossingCount=c()) {
  if (HASIB.algo == "vanHees2015") { # default
    cnt = 1
    Nepochs = length(timethreshold) * length(anglethreshold)
    sib_classification = matrix(0,length(anglez), Nepochs)
    for (i in timethreshold) {
      for (j in anglethreshold) {
        sdl1 = rep(0,length(time))
        postch = which(abs(diff(anglez)) > j) #posture change of at least j degrees
        # count posture changes that happen less than once per ten minutes
        q1 = c()
        if (length(postch) > 1) {
          q1 = which(diff(postch) > (i*(60/ws3))) #less than once per i minutes
        }
        if (length(q1) > 0) {
          for (gi in 1:length(q1)) {
            sdl1[postch[q1[gi]]:postch[q1[gi]+1]] = 1 #periods with no posture change
          }
        } else { #possibly a day without wearing
          if (length(postch) < 10) {  #possibly a day without wearing
            sdl1[1:length(sdl1)] = 1 #periods with no posture change
          } else {  #possibly a day with constantly posture changes
            sdl1[1:length(sdl1)] = 0 #periodsposture change
          }
        }
        sib_classification[,cnt] = sdl1
        cnt = cnt+ 1
      }
    }
    cnt = 1
    sib_classification = as.data.frame(sib_classification, stringsAsFactors = TRUE)
    for (i in timethreshold) {
      for (j in anglethreshold) {
        colnames(sib_classification)[cnt] = paste("T",i,"A",j,sep="")
        cnt = cnt + 1
      }
    }
  } else if (HASIB.algo == "Sadeh1994") {
    sib_classification =c()
    # TO DO:
    # downsample to 1 minute
    
    # zeroCrossingCount
    
    # apply algorithm
    
    # resample to original resolution
    # convert into sib_classification obect
    
    # assign it a name?
  }
  return(sib_classification)
}