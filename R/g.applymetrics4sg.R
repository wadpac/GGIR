g.applymetrics4sg = function(acclocal, gvector, 
                                sf,ws3, sg.metrics2do) {
  do.sgAccEN = sg.metrics2do$do.sgAccEN
  do.sgAnglex = sg.metrics2do$do.sgAnglex
  do.sgAngley = sg.metrics2do$do.sgAngley
  do.sgAnglez = sg.metrics2do$do.sgAnglez
  sgmetrics = c()
  averageperws3 = function(x,sf,ws3) {
    x2 =cumsum(c(0,x))
    select = seq(1,length(x2),by=sf*ws3)
    x3 = diff(x2[round(select)]) / abs(diff(round(select)))
  }
  #================================================
  # Functions to aid metric extraction
  process_axes = function(data, filtertype, cut_point, sf=c()) {
    if (length(sf) == 0) warning("sf not found")
    if (filtertype == "rollmedian") {
      rollmed = function(x, sf) {
        winsi = round(sf*5)
        if (round(winsi/2) == (winsi/2)) winsi = winsi+1
        xm = zoo::rollmedian(x,k=winsi,na.pad=TRUE)
        xm[which(is.na(xm[1:1000]) ==T)] = xm[which(is.na(xm[1:1000]) ==F)[1]]
        return(xm)
      }
      data_processed = data
      for (i in 1:3) {
        data_processed[,i] = rollmed(data[,i], sf)
      }
      if (length(which(is.na(data_processed[,1]) == T |
                       is.na(data_processed[,2]) == T |
                       is.na(data_processed[,3]) == T)) > 0) {
        for (j in 1:3) {
          p1 = which(is.na(data_processed[,j]) ==F)
          data_processed[which(is.na( data_processed[,j]) ==T),j] =  data_processed[p1[length(p1)],j]
        }
      }
    }
    return(data_processed)
  }
  anglex = function(xyz) {
    return(atan(xyz[,1] / (sqrt(xyz[,2]^2 + xyz[,3]^2))) / (pi/180))
  }
  angley = function(xyz) {
    return(atan(xyz[,2] / (sqrt(xyz[,1]^2 + xyz[,3]^2))) / (pi/180))
  }
  anglez = function(xyz) {
    return(atan(xyz[,3] / (sqrt(xyz[,1]^2 + xyz[,2]^2))) / (pi/180))
  }
  EuclideanNorm = function(xyz) {
    return(sqrt((xyz[,1]^2) + (xyz[,2]^2) + (xyz[,3]^2)))
  }
  if (do.sgAnglex == TRUE | do.sgAngley == TRUE | do.sgAnglez == TRUE) {
    data_processed = process_axes(gvector, filtertype="rollmedian", cut_point=c(), sf)
    if (do.sgAnglex == TRUE) {
      sgmetrics$sgAnglex = averageperws3(x=anglex(data_processed),sf,ws3)
    }
    if (do.sgAngley == TRUE) {
      sgmetrics$sgAngley = averageperws3(x=angley(data_processed),sf,ws3)
    }
    if (do.sgAnglez == TRUE) {
      sgmetrics$sgAnglez = averageperws3(x=anglez(data_processed),sf,ws3)
    }
  }
  #================================================
  # Filter-free Euclidean norm related metrics:
  if (do.sgAccEN == TRUE) {
    sgmetrics$sgAccEN = averageperws3(EuclideanNorm(acclocal),sf,ws3)
  }
  
  return(sgmetrics)
} 
