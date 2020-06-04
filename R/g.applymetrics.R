g.applymetrics = function(data,n=4,sf,ws3,metrics2do, lb=0.2, hb=15){
  # data is a 3 column matrix with the x, y, and z acceleration
  do.bfen = metrics2do$do.bfen
  do.enmo = metrics2do$do.enmo
  do.lfenmo = metrics2do$do.lfenmo
  do.en = metrics2do$do.en
  do.hfen = metrics2do$do.hfen
  do.hfenplus = metrics2do$do.hfenplus
  do.mad = metrics2do$do.mad
  do.anglex = metrics2do$do.anglex
  do.angley = metrics2do$do.angley
  do.anglez = metrics2do$do.anglez
  do.roll_med_acc_x = metrics2do$do.roll_med_acc_x
  do.roll_med_acc_y = metrics2do$do.roll_med_acc_y
  do.roll_med_acc_z = metrics2do$do.roll_med_acc_z
  do.dev_roll_med_acc_x = metrics2do$do.dev_roll_med_acc_x
  do.dev_roll_med_acc_y = metrics2do$do.dev_roll_med_acc_y
  do.dev_roll_med_acc_z = metrics2do$do.dev_roll_med_acc_z
  do.enmoa = metrics2do$do.enmoa
  do.lfen = metrics2do$do.lfen
  do.hfx = metrics2do$do.hfx
  do.hfy = metrics2do$do.hfy
  do.hfz = metrics2do$do.hfz
  do.lfx = metrics2do$do.lfx
  do.lfy = metrics2do$do.lfy
  do.lfz = metrics2do$do.lfz
  do.bfx = metrics2do$do.bfx
  do.bfy = metrics2do$do.bfy
  do.bfz = metrics2do$do.bfz
  allmetrics = c()
  averageperws3 = function(x,sf,ws3) {
    x2 =cumsum(c(0,x))
    select = seq(1,length(x2),by=sf*ws3)
    x3 = diff(x2[round(select)]) / abs(diff(round(select)))
  }
  if (sf <= (hb *2)) { #avoid having a higher filter boundary higher than sf/2
    hb = round(sf/2) - 1
  }
  gravity = 1
  #================================================
  # Functions to aid metric extraction
  process_axes = function(data, filtertype, cut_point, n=4, sf=c()) {
    if (length(sf) == 0) warning("sf not found")
    if (filtertype == "pass" | filtertype == "high" | filtertype == "low") {
      hf_lf_filter = function(bound, n, sf, filtertype) {
        return(signal::butter(n,c(bound/(sf/2)),type=filtertype))
      }
      bf_filter = function(lb, hb, n, sf) {
        Wc = matrix(0,2,1)
        Wc[1,1] = lb / (sf/2)
        Wc[2,1] = hb / (sf/2)
        return(signal::butter(n,Wc,type=c("pass")))
      }
      if (filtertype == "pass") {
        coef = bf_filter(cut_point[1], cut_point[2], n, sf)
      } else {
        if (filtertype == "low") {
          bound = hb
        } else {
          bound = lb
        }
        coef = hf_lf_filter(bound, n, sf, filtertype)
      }
      data_processed = data
      for (i in 1:3) {
        data_processed[,i] = signal::filter(coef, data[,i])
      }
    } else if (filtertype == "rollmedian") {
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
  #==========================
  # Band-pass filtering related metrics
  if (do.bfen == TRUE | do.bfx == TRUE | do.bfy == TRUE | do.bfz == TRUE) {
    data_processed = abs(process_axes(data, filtertype="pass", cut_point=c(lb,hb), n, sf))
    if (do.bfx == TRUE) {
      allmetrics$BFX = averageperws3(x=data_processed[,1], sf, ws3)
    }
    if (do.bfy == TRUE) {
      allmetrics$BFY = averageperws3(x=data_processed[,2], sf, ws3)
    }
    if (do.bfz == TRUE) {
      allmetrics$BFZ = averageperws3(x=data_processed[,3], sf, ws3)
    }
    allmetrics$BFEN = averageperws3(x=EuclideanNorm(data_processed),sf,ws3)
  }
  #================================================
  # Low-pass filtering related metrics
  if (do.lfenmo == TRUE | do.lfx == TRUE | do.lfy == TRUE | do.lfz == TRUE | do.lfen == TRUE) {
    data_processed = abs(process_axes(data, filtertype="low", cut_point=hb, n, sf))
    if (do.lfx == TRUE) {
      allmetrics$LFX = averageperws3(x=data_processed[,1], sf, ws3)
    }
    if (do.lfy == TRUE) {
      allmetrics$LFY = averageperws3(x=data_processed[,2], sf, ws3)
    }
    if (do.lfz == TRUE) {
      allmetrics$LFZ = averageperws3(x=data_processed[,3], sf, ws3)
    }
    if (do.lfen == TRUE) {
      allmetrics$LFEN = averageperws3(x=EuclideanNorm(data_processed),sf,ws3)
    }
    if (do.lfenmo == TRUE) {
      LFENMO = EuclideanNorm(data_processed)-gravity
      LFENMO[which(LFENMO < 0)] = 0
      allmetrics$LFENMO = averageperws3(x=LFENMO,sf,ws3)
    }
  }
  #================================================
  # High-pass filtering related metrics
  if (do.hfen == TRUE | do.hfx == TRUE | do.hfy == TRUE | do.hfz == TRUE) {
    data_processed = abs(process_axes(data, filtertype="high", cut_point=c(lb), n, sf))
    if (do.hfx == TRUE) {
      allmetrics$HFX = averageperws3(x=data_processed[,1], sf, ws3)
    }
    if (do.hfy == TRUE) {
      allmetrics$HFY = averageperws3(x=data_processed[,2], sf, ws3)
    }
    if (do.hfz == TRUE) {
      allmetrics$HFZ = averageperws3(x=data_processed[,3], sf, ws3)
    }
    allmetrics$HFEN = averageperws3(x=EuclideanNorm(data_processed),sf,ws3)
  }
  #================================================
  # Combined low-pass and high-pass filtering metric:
  if (do.hfenplus == TRUE) { 
    # Note that we are using intentionally the lower boundary for the low pass filter
    data_processed = process_axes(data, filtertype="low", cut_point=lb, n, sf)
    GCP = EuclideanNorm(data_processed) - gravity
    # Note that we are using intentionally the lower boundary for the high pass filter
    data_processed = process_axes(data, filtertype="high", cut_point=lb, n, sf)
    HFENplus = EuclideanNorm(data_processed) + GCP
    HFENplus[which(HFENplus < 0)] = 0
    allmetrics$HFENplus = averageperws3(x=HFENplus,sf,ws3)
  }
  #================================================
  # Rolling median filter related metrics:
  roll_median_done  = FALSE
  if (do.roll_med_acc_x == TRUE | do.roll_med_acc_y == TRUE | do.roll_med_acc_z == TRUE | 
      do.dev_roll_med_acc_x == TRUE | do.dev_roll_med_acc_y == TRUE | do.dev_roll_med_acc_z == TRUE) {
    data_processed = process_axes(data, filtertype="rollmedian", cut_point=c(), n, sf)
    roll_median_done  = TRUE
    if (do.roll_med_acc_x == TRUE | do.roll_med_acc_y == TRUE | do.roll_med_acc_z == TRUE) { #rolling median of acceleration
      allmetrics$roll_med_acc_x = averageperws3(x=data_processed[,1],sf,ws3)
      allmetrics$roll_med_acc_y = averageperws3(x=data_processed[,2],sf,ws3)
      allmetrics$roll_med_acc_z = averageperws3(x=data_processed[,3],sf,ws3)
    }
    if (do.dev_roll_med_acc_x == TRUE | do.dev_roll_med_acc_y == TRUE | do.dev_roll_med_acc_z == TRUE) { #rolling median of acceleration
      allmetrics$dev_roll_med_acc_x = averageperws3(x=abs(data[,1] - data_processed[,1]),sf,ws3)
      allmetrics$dev_roll_med_acc_y = averageperws3(x=abs(data[,2] - data_processed[,2]),sf,ws3)
      allmetrics$dev_roll_med_acc_z = averageperws3(x=abs(data[,3] - data_processed[,3]),sf,ws3)
    }
  }
  if (do.anglex == TRUE | do.angley == TRUE | do.anglez == TRUE) {
    if (roll_median_done == FALSE) { 
      data_processed = process_axes(data, filtertype="rollmedian", cut_point=c(), n, sf)
    }
    if (do.anglex == TRUE) {
      allmetrics$angle_x = averageperws3(x=anglex(data_processed),sf,ws3)
    }
    if (do.angley == TRUE) {
      allmetrics$angle_y = averageperws3(x=angley(data_processed),sf,ws3)
    }
    if (do.anglez == TRUE) {
      allmetrics$angle_z = averageperws3(x=anglez(data_processed),sf,ws3)
    }
  }
  #================================================
  # Filter-free Euclidean norm related metrics:
  EN = EuclideanNorm(data)
  if (do.enmo == TRUE) {
    ENMO = EN - 1
    ENMO[which(ENMO < 0)] = 0 #turning negative values into zero
    allmetrics$ENMO = averageperws3(x=ENMO,sf,ws3)
  }
  if (do.mad == TRUE) { # metric MAD (Mean Amplitude Deviation)
    MEANS = rep(averageperws3(x=EN,sf,ws3), each = sf*ws3)
    MAD = abs(EN - MEANS)
    allmetrics$MAD = averageperws3(x=MAD,sf,ws3)
  }
  if (do.en == TRUE) {
    allmetrics$EN = averageperws3(x=EN,sf,ws3)
  }
  if (do.enmoa == TRUE) {
    ENMOa = abs(EN - gravity)
    ENMOa[which(ENMOa < 0)] = 0
    allmetrics$ENMOa = averageperws3(x=ENMOa,sf,ws3)
  }
  return(allmetrics)
} 
