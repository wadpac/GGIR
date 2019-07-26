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
  } else if (ii == 3) { # no subtraction, just vector magnitude
    Gfil[,1] = sqrt((Gx^2) + (Gy^2) + (Gz^2))
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
  } else if (ii == 7) {    # band pass filtered eucludian norm
    Wc = matrix(0,2,1)
    Wc[1,1] = lb / (sf/2)
    Wc[2,1] = hb / (sf/2)
    bf = signal::butter(n,Wc,type=c("pass"))
    Gxfil[,1] = signal::filter(bf,Gx)
    Gyfil[,1] = signal::filter(bf,Gy)
    Gzfil[,1] = signal::filter(bf,Gz)
    Gfil[,1] = sqrt((Gxfil[,1]^2) + (Gyfil[,1]^2) + (Gzfil[,1]^2))
  } else if (ii == 9) { #Low pass filtered followed by ENMO
    bf = signal::butter(n,c(hb/(sf/2)),type=c("low")) #creating filter coefficients
    Gxfil[,1] = signal::filter(bf,Gx)
    Gyfil[,1] = signal::filter(bf,Gy)
    Gzfil[,1] = signal::filter(bf,Gz)
    Gfil[,1] = sqrt((Gxfil[,1]^2) + (Gyfil[,1]^2) + (Gzfil[,1]^2)) - gravity
  } else if (ii == 11 | ii == 13 | ii == 14) { # angles or rolling medians
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
    if (ii == 11) { # angles
      anglex = (atan(Gxm / (sqrt(Gym^2 + Gzm^2)))) / (pi/180)
      angley = (atan(Gym / (sqrt(Gxm^2 + Gzm^2)))) / (pi/180)
      anglez = (atan(Gzm / (sqrt(Gxm^2 + Gym^2)))) / (pi/180)
      Gfil = cbind(anglex,angley,anglez)
    }
    if (ii == 13) { # rolling median
      Gfil = cbind(Gxm,Gym,Gzm)
    }
    if (ii == 14) { # direction specific acceleration (experimental)
      Accx = abs(Gx - Gxm)
      Accy = abs(Gy - Gym)
      Accz = abs(Gz - Gzm)
      Gfil = cbind(Accx,Accy,Accz)
    }
  } else if (ii == 12) { # vector magnitude minus one and then absolute
    Gfil[,1] = abs(sqrt((Gx^2) + (Gy^2) + (Gz^2)) - gravity)
  }
  if (ii == 11 | ii == 13 | ii == 14) {
    g.metric = Gfil
  } else {
    g.metric = Gfil[,1]
  }
}
