g.metric= function(Gx,Gy,Gz,n=c(),sf,ii,TW=c(),lb=c(),hb=c(),gravity = 1) {
  #--------------------------------------
  #Input:
  # G = acceleration signal
  # lb & hb = cut-off frequencies for filter
  # sf = sample frequency
  # n = filter order
  # ii = metric selector
  # TW = time window
  # gravity = value corresponding to gravity
  #--------------------------------------
  # opening required libraries
  durexp = length(Gx)	#duration of experiment
  Gxfil = matrix(0,durexp,1)
  Gyfil = matrix(0,durexp,1)
  Gzfil = matrix(0,durexp,1)
  Gfil = matrix(0,durexp,1)
  GxCP = matrix(0,durexp,1)
  GyCP = matrix(0,durexp,1)
  GzCP = matrix(0,durexp,1)
  GCP = matrix(0,durexp,1)
  if (ii == 1) {
    # high pass filter
    bf = signal::butter(n,c(lb/(sf/2)),type=c("high")) #creating filter coefficients
    Gxfil[,1] = signal::filter(bf,Gx)
    Gyfil[,1] = signal::filter(bf,Gy)
    Gzfil[,1] = signal::filter(bf,Gz)
    Gfil[,1] = sqrt((Gxfil[,1]^2) + (Gyfil[,1]^2) + (Gzfil[,1]^2))
  } else if (ii == 2) {
    # moving average
    for (j in 1:length(Gx)) {
      if (j < TW/2) {	
        Gxfil[j,1] = Gx[j] - mean(Gx[1:TW])
        Gyfil[j,1] = Gy[j] - mean(Gy[1:TW])
        Gzfil[j,1] = Gz[j] - mean(Gz[1:TW])
      } else if (j > (length(Gx) - (TW/2))) {
        Gxfil[j,1] = Gx[j] - mean(Gx[(length(Gx)-TW):length(Gx)])
        Gyfil[j,1] = Gy[j] - mean(Gy[(length(Gx)-TW):length(Gx)])
        Gzfil[j,1] = Gz[j] - mean(Gz[(length(Gx)-TW):length(Gx)])
      } else if (j < (length(Gx) - (TW/2)) & j > TW/2) {
        Gxfil[j,1] = Gx[j] - mean(Gx[(j-(TW/2)):(j+(TW/2))])
        Gyfil[j,1] = Gy[j] - mean(Gy[(j-(TW/2)):(j+(TW/2))])
        Gzfil[j,1] = Gz[j] - mean(Gz[(j-(TW/2)):(j+(TW/2))])
      }
    }
    Gfil[,1] = sqrt((Gxfil[,1]^2) + (Gyfil[,1]^2) + (Gzfil[,1]^2))
  } else if (ii == 3) { # no subtraction, just vector magnitude
    Gfil[,1] = sqrt((Gx^2) + (Gy^2) + (Gz^2))
  } else if (ii == 4) { # vector magnitude minus one
    Gfil[,1] = sqrt((Gx^2) + (Gy^2) + (Gz^2)) - gravity
  } else if (ii == 5) { # filter + correction for centripital acc
    bf = signal::butter(n,c(lb/(sf/2)),type=c("low")) #creating filter coefficients
    GxCP[,1] = signal::filter(bf,Gx)
    GyCP[,1] = signal::filter(bf,Gy)
    GzCP[,1] = signal::filter(bf,Gz)
    GCP[,1] = (sqrt((GxCP[,1]^2) + (GyCP[,1]^2) + (GzCP[,1]^2))) - gravity #vector magnitude minus 1
    bf = signal::butter(n,c(lb/(sf/2)),type=c("high")) #creating filter coefficients
    Gxfil[,1] = signal::filter(bf,Gx)
    Gyfil[,1] = signal::filter(bf,Gy)
    Gzfil[,1] = signal::filter(bf,Gz)
    Gfil[,1] = (sqrt((Gxfil[,1]^2) + (Gyfil[,1]^2) + (Gzfil[,1]^2))) + GCP[,1]
  } else if (ii == 6) {   # moving average + correction for centripital acc
    for (j in 1:length(Gx)) {
      if (j < TW/2) {	
        GxCP[j,1] = mean(Gx[1:TW])
        GyCP[j,1] = mean(Gy[1:TW])
        GzCP[j,1] = mean(Gz[1:TW])
      } else if (j > (length(Gx) - (TW/2))) {
        GxCP[j,1] = mean(Gx[(length(Gx)-TW):length(Gx)])
        GyCP[j,1] = mean(Gy[(length(Gx)-TW):length(Gx)])
        GzCP[j,1] = mean(Gz[(length(Gx)-TW):length(Gx)])
      } else if (j < (length(Gx) - (TW/2)) & j > TW/2) {
        GxCP[j,1] = mean(Gx[(j-(TW/2)):(j+(TW/2))])
        GyCP[j,1] = mean(Gy[(j-(TW/2)):(j+(TW/2))])
        GzCP[j,1] = mean(Gz[(j-(TW/2)):(j+(TW/2))])
      }
    }
    GCP[,1] = (sqrt((GxCP[,1]^2) + (GyCP[,1]^2) + (GzCP[,1]^2))) - gravity
    for (j in 1:length(Gx)) {
      if (j < TW/2) {	
        Gxfil[j,1] = Gx[j] - mean(Gx[1:TW])
        Gyfil[j,1] = Gy[j] - mean(Gy[1:TW])
        Gzfil[j,1] = Gz[j] - mean(Gz[1:TW])
      } else if (j > (length(Gx) - (TW/2))) {
        Gxfil[j,1] = Gx[j] - mean(Gx[(length(Gx)-TW):length(Gx)])
        Gyfil[j,1] = Gy[j] - mean(Gy[(length(Gx)-TW):length(Gx)])
        Gzfil[j,1] = Gz[j] - mean( Gz[(length(Gx)-TW):length(Gx)])
      } else if (j < (length(Gx) - (TW/2)) & j > TW/2) {
        Gxfil[j,1] = Gx[j] - mean(Gx[(j-(TW/2)):(j+(TW/2))])
        Gyfil[j,1] = Gy[j] - mean(Gy[(j-(TW/2)):(j+(TW/2))])
        Gzfil[j,1] = Gz[j] - mean(Gz[(j-(TW/2)):(j+(TW/2))])
      }
    }
    Gfil[,1] = (sqrt((Gxfil[,1]^2) + (Gyfil[,1]^2) + (Gzfil[,1]^2))) + GCP[,1]
  } else if (ii == 7) {    # band pass filtered eucludian norm
    Wc = matrix(0,2,1)
    Wc[1,1] = lb / (sf/2)
    Wc[2,1] = hb / (sf/2)
    bf = signal::butter(n,Wc,type=c("pass"))
    Gxfil[,1] = signal::filter(bf,Gx)
    Gyfil[,1] = signal::filter(bf,Gy)
    Gzfil[,1] = signal::filter(bf,Gz)
    Gfil[,1] = sqrt((Gxfil[,1]^2) + (Gyfil[,1]^2) + (Gzfil[,1]^2))
#   } else if (ii == 8) { #angle
#     winsi = round(sf*5)
#     if (round(winsi/2) == (winsi/2)) winsi = winsi+1
#     Gym = zoo::rollmedian(Gy,k=winsi,na.pad=TRUE)
#     Gym[which(is.na(Gym[1:1000]) ==T)] = Gym[which(is.na(Gym[1:1000]) ==F)[1]]
#     p1 = which(is.na(Gym) ==F)
#     Gym[which(is.na(Gym) ==T)] = Gym[p1[length(p1)]]
#     angle = Gym
#     angle[which(angle < -1)] = -1 #rounding to -1 and 1 as sine cant deal with numbers outside this range
#     angle[which(angle > 1)] = 1
#     angle = (asin(angle) / (2*pi)) * 360# conversion to degrees
#     if (nrow(Gfil) != length(angle)) {
#       warning
#     }
#     Gfil[,1] = angle
  } else if (ii == 9) { #Low pass filtered followed by ENMO  
    bf = signal::butter(n,c(hb/(sf/2)),type=c("low")) #creating filter coefficients
    Gxfil[,1] = signal::filter(bf,Gx)
    Gyfil[,1] = signal::filter(bf,Gy)
    Gzfil[,1] = signal::filter(bf,Gz)
    Gfil[,1] = sqrt((Gxfil[,1]^2) + (Gyfil[,1]^2) + (Gzfil[,1]^2)) - gravity
  } else if (ii == 10) { #filter similar to method by te Lindert 2013
    Wc = matrix(0,2,1)
    Wc[1,1] = 3 / (sf/2)
    Wc[2,1] = 11 / (sf/2)
    bf = signal::butter(n=5,Wc,type=c("pass"))
    Gzfil[,1] = signal::filter(bf,Gz)	#filtering
    Gzfil[,1] = abs(Gzfil[,1]) #rectify
    Gzfil[which(Gzfil[,1]>5),1] = 5 #no values higher than 5 g
    Gzfil[,1] = round(Gzfil[,1] * (128/5)) # convert into numbers between 0 and 128     
    Gfil = zoo::rollmax(Gzfil,k=sf,na.pad=TRUE,fill=0)
  } else if (ii == 11) { 
    winsi = round(sf*5)
    if (round(winsi/2) == (winsi/2)) winsi = winsi+1
    Gxm = zoo::rollmedian(Gx,k=winsi,na.pad=TRUE)
    Gym = zoo::rollmedian(Gy,k=winsi,na.pad=TRUE)
    Gzm = zoo::rollmedian(Gz,k=winsi,na.pad=TRUE)
    Gxm[which(is.na(Gxm[1:1000]) ==T)] = Gxm[which(is.na(Gxm[1:1000]) ==F)[1]]
    Gym[which(is.na(Gym[1:1000]) ==T)] = Gym[which(is.na(Gym[1:1000]) ==F)[1]]
    Gzm[which(is.na(Gzm[1:1000]) ==T)] = Gzm[which(is.na(Gzm[1:1000]) ==F)[1]]
    p1 = which(is.na(Gxm) ==F); Gxm[which(is.na(Gxm) ==T)] = Gxm[p1[length(p1)]]
    p1 = which(is.na(Gym) ==F); Gym[which(is.na(Gym) ==T)] = Gym[p1[length(p1)]]
    p1 = which(is.na(Gzm) ==F); Gzm[which(is.na(Gzm) ==T)] = Gzm[p1[length(p1)]]
    anglex = (atan(Gxm / (sqrt(Gym^2 + Gzm^2)))) / (pi/180)
    angley = (atan(Gym / (sqrt(Gxm^2 + Gzm^2)))) / (pi/180)
    anglez = (atan(Gzm / (sqrt(Gxm^2 + Gym^2)))) / (pi/180)
    Gfil = cbind(anglex,angley,anglez)
    
  } else if (ii == 12) { # vector magnitude minus one and then absolute
    Gfil[,1] = abs(sqrt((Gx^2) + (Gy^2) + (Gz^2)) - gravity)
    
  }
  if (ii != 11) {
    g.metric = Gfil[,1]
  } else {
    g.metric = Gfil
  }
}