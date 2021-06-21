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
    # Aggregate per minute
    sumperminutes = function(x, ws3) {
      x2 =cumsum(c(0,x))
      select = seq(1,length(x2),by=60/ws3)
      x3 = diff(x2[select])
    }
    ZCpermin = sumperminutes(zeroCrossingCount, ws3=5)
    Nz= length(ZCpermin)
    # Fill matrix to ease applying rolling function
    ZCpermin_matrix = matrix(0, Nz+10, 11)
    for (jj in 1:11) {
      ZCpermin_matrix[jj:(Nz+jj-1), jj] = ZCpermin
      if (jj > 1) {
        ZCpermin_matrix[1:(jj-1), jj] = ZCpermin[1]
      }
      if (jj < 11) {
        ZCpermin_matrix[((Nz-(10-jj)):Nz)+10, jj] = tail(ZCpermin, 1)
      }
    }
    CalcSadehFT = function(x) {
      MeanW5 = mean(x, na.rm=TRUE)
      SDlast = sd(x[1:6])
      NAT = length(which(x > 50 & x < 100))
      LOGact = log(x[6]+1)
      return(data.frame(MeanW5=MeanW5, SDlast=SDlast,
                        NAT=NAT, LOGact=LOGact))
    }
    SadehFT1 = apply(X = ZCpermin_matrix, MARGIN = 1, FUN = CalcSadehFT)
    rm(ZCpermin_matrix)
    SadehFT = data.frame(matrix(unlist(SadehFT1), nrow=length(SadehFT1), byrow=TRUE))
    rm(SadehFT1)
    colnames(SadehFT) = c("MeanW5", "SDlast", "NAT", "LOGact")
    
    print(summary(SadehFT))
    # apply Sadeh algorithm
    PS = 7.601 - (0.065 * SadehFT$MeanW5) - (1.08 * SadehFT$NAT) - (0.056 * SadehFT$SDlast) - (0.703 * SadehFT$LOGact)
    PSscores = rep(0, length(PS))
    PSsibs = which(PS >= 0)
    if (length(PSsibs) > 0) {
      PSscores[PSsibs] = 1
    }
    # resample to original resolution
    PSscores = rep(PSscores, each=(60/ws3))
    if (length(PSscores) < length(zeroCrossingCount)) {
      PSscores=c(PSscores,rep(0, length(zeroCrossingCount) - length(PSscores)))
    }
    sib_classification = PSscores
  
  }
 
  return(sib_classification)
}