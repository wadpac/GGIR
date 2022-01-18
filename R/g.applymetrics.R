g.applymetrics = function(data, sf, ws3, metrics2do,
                          n = 4, lb = 0.2, hb = 15){
  epochsize = ws3 #epochsize in seconds
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
  do.zcx = metrics2do$do.zcx
  do.zcy = metrics2do$do.zcy
  do.zcz = metrics2do$do.zcz
  do.brondcounts = metrics2do$do.brondcounts
  allmetrics = c()
  averagePerEpoch = function(x,sf,epochsize) {
    x2 = cumsum(c(0,x))
    select = seq(1,length(x2),by = sf*epochsize)
    x3 = diff(x2[round(select)]) / abs(diff(round(select)))
  }
  sumPerEpoch = function(x, sf, epochsize) {
    x2 = cumsum(c(0,x))
    select = seq(1,length(x2), by = sf*epochsize)
    x3 = diff(x2[round(select)])
  }
  
  if (sf <= (hb * 2)) { #avoid having a higher filter boundary higher than sf/2
    hb = round(sf/2) - 1
  }
  gravity = 1
  #================================================
  # Functions to aid metric extraction
  process_axes = function(data, filtertype, cut_point, n = 4, sf = c()) {
    if (length(sf) == 0) warning("sf not found")
    if (filtertype == "pass" | filtertype == "high" | filtertype == "low") {
      hf_lf_filter = function(bound, n, sf, filtertype) {
        return(signal::butter(n, c(bound/(sf/2)), type = filtertype))
      }
      bf_filter = function(lb, hb, n, sf) {
        Wc = matrix(0,2,1)
        Wc[1,1] = lb / (sf/2)
        Wc[2,1] = hb / (sf/2)
        if (sf/2 < hb | sf/2 < hb) {
          warning("\nSample frequency ", sf, " too low for calculating this metric.")
        }
        return(signal::butter(n, Wc, type = c("pass")))
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
        data_processed[, i] = signal::filter(coef, data[,i])
      }
    } else if (filtertype == "rollmedian") {
      rollmed = function(x, sf) {
        winsi = round(sf * 5)
        if (round(winsi/2) == (winsi/2)) winsi = winsi + 1
        xm = zoo::rollmedian(x, k = winsi, na.pad = TRUE)
        xm[which(is.na(xm[1:1000]) == TRUE)] = xm[which(is.na(xm[1:1000]) == FALSE)[1]]
        return(xm)
      }
      data_processed = data
      for (i in 1:3) {
        data_processed[,i] = rollmed(data[,i], sf)
      }
      if (length(which(is.na(data_processed[,1]) == TRUE |
                       is.na(data_processed[,2]) == TRUE |
                       is.na(data_processed[,3]) == TRUE)) > 0) {
        for (j in 1:3) {
          p1 = which(is.na(data_processed[,j]) == FALSE)
          data_processed[which(is.na( data_processed[,j]) == TRUE),j] =  data_processed[p1[length(p1)],j]
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
    data_processed = abs(process_axes(data, filtertype = "pass", cut_point = c(lb,hb), n, sf))
    if (do.bfx == TRUE) {
      allmetrics$BFX = averagePerEpoch(x = data_processed[,1], sf, epochsize)
    }
    if (do.bfy == TRUE) {
      allmetrics$BFY = averagePerEpoch(x = data_processed[,2], sf, epochsize)
    }
    if (do.bfz == TRUE) {
      allmetrics$BFZ = averagePerEpoch(x = data_processed[,3], sf, epochsize)
    }
    if (do.bfen == TRUE) {
      allmetrics$BFEN = averagePerEpoch(x = EuclideanNorm(data_processed),sf,epochsize)
    }
  }
  if (do.zcx == TRUE | do.zcy == TRUE | do.zcz == TRUE) { # Zero crossing count
    
    # 1) apply band-pass frequency filter to mimic old-sensor
    # 0.25 - 3 Hertz to be in line with Ancoli Isreal's paper from 2003,
    # and online "Motionlogger Users Guide Version 2K1.1" from Ambulatory Monitoring,
    # Inc. Ardsley, New York 10502
    # Be aware that specific boundaries differ between Actigraph brands that copied the
    # Sadeh algorithm.
    # We use a second order filter because if it was an analog filter it was 
    # most likely not very steep filter.
    data_processed = process_axes(data, filtertype = "pass", cut_point = c(0.25, 3), n = 2, sf)
    zil = c()
    
    # 2) Sadeh reported to have used the y-axis but did not specify the orientation of
    # the y-axis in their accelerometer. Therefore, we keep selection of axis flexible for the user
    if (do.zcx == TRUE) zil = 1
    if (do.zcy == TRUE) zil = c(zil, 2)
    if (do.zcz == TRUE) zil = c(zil, 3)
    Ndat = nrow(data_processed)
    for (zi in zil) { # loop over axes
      # 3) apply stop-band to minic sensitivity of 1980s and 1990s accelerometer
      # technology. Using a 0.01g threshold based on book by Tyron 
      # "Activity Measurementy in Psychology And Medicine"
      smallvalues = which(abs(data_processed[,zi]) < 0.01)
      if (length(smallvalues) > 0) {
        data_processed[smallvalues, zi] = 0
      }
      rm(smallvalues)
      # output binary time series zeros and ones with 1 for zero-crossing
      data_processed[,zi] = ifelse(test = data_processed[,zi] >= 0,yes = 1, no = -1)
      # 4) detect zero-crossings
      # The exact algorithm for the original monitor not found, maybe it happened analog
      # we will use: http://rug.mnhn.fr/seewave/HTML/MAN/zcr.html
      zerocross = function(x, Ndat) {
        tmp = abs(sign(x[2:Ndat]) - sign(x[1:(Ndat - 1)])) * 0.5
        tmp = c(tmp[1], tmp) # add value to ensure length aligns
        return(tmp)
      }
      if (zi == 1) {
        allmetrics$ZCX = sumPerEpoch(zerocross(data_processed[,zi], Ndat), sf, epochsize)
      } else if (zi == 2) {
        allmetrics$ZCY = sumPerEpoch(zerocross(data_processed[,zi], Ndat), sf, epochsize)
      } else if (zi == 3) {
        allmetrics$ZCZ = sumPerEpoch(zerocross(data_processed[,zi], Ndat), sf, epochsize)
      }
    }
    # Note that this is per epoch, in GGIR part 3 we aggregate (sum) this per minute
    # to follow Sadeh. In Sadeh 1987 this resulted in values up to 280
    # 280 = 60 x 2 x frequency of movement which would mean near 2.33 Hertz average
    # movement frequencies, which may have reflected walking
  }
  #================================================
  # Low-pass filtering related metrics
  if (do.lfenmo == TRUE | do.lfx == TRUE | do.lfy == TRUE | do.lfz == TRUE | do.lfen == TRUE) {
    data_processed = abs(process_axes(data, filtertype = "low", cut_point = hb, n, sf))
    if (do.lfx == TRUE) {
      allmetrics$LFX = averagePerEpoch(x = data_processed[,1], sf, epochsize)
    }
    if (do.lfy == TRUE) {
      allmetrics$LFY = averagePerEpoch(x = data_processed[,2], sf, epochsize)
    }
    if (do.lfz == TRUE) {
      allmetrics$LFZ = averagePerEpoch(x = data_processed[,3], sf, epochsize)
    }
    if (do.lfen == TRUE) {
      allmetrics$LFEN = averagePerEpoch(x = EuclideanNorm(data_processed), sf, epochsize)
    }
    if (do.lfenmo == TRUE) {
      LFENMO = EuclideanNorm(data_processed) - gravity
      LFENMO[which(LFENMO < 0)] = 0
      allmetrics$LFENMO = averagePerEpoch(x = LFENMO,sf,epochsize)
    }
  }
  #================================================
  # High-pass filtering related metrics
  if (do.hfen == TRUE | do.hfx == TRUE | do.hfy == TRUE | do.hfz == TRUE) {
    data_processed = abs(process_axes(data, filtertype = "high", cut_point = c(lb), n, sf))
    if (do.hfx == TRUE) {
      allmetrics$HFX = averagePerEpoch(x = data_processed[,1], sf, epochsize)
    }
    if (do.hfy == TRUE) {
      allmetrics$HFY = averagePerEpoch(x = data_processed[,2], sf, epochsize)
    }
    if (do.hfz == TRUE) {
      allmetrics$HFZ = averagePerEpoch(x = data_processed[,3], sf, epochsize)
    }
    allmetrics$HFEN = averagePerEpoch(x = EuclideanNorm(data_processed), sf, epochsize)
  }
  #================================================
  # Combined low-pass and high-pass filtering metric:
  if (do.hfenplus == TRUE) { 
    # Note that we are using intentionally the lower boundary for the low pass filter
    data_processed = process_axes(data, filtertype = "low", cut_point = lb, n, sf)
    GCP = EuclideanNorm(data_processed) - gravity
    # Note that we are using intentionally the lower boundary for the high pass filter
    data_processed = process_axes(data, filtertype = "high", cut_point = lb, n, sf)
    HFENplus = EuclideanNorm(data_processed) + GCP
    HFENplus[which(HFENplus < 0)] = 0
    allmetrics$HFENplus = averagePerEpoch(x = HFENplus,sf,epochsize)
  }
  #================================================
  # Rolling median filter related metrics:
  roll_median_done  = FALSE
  if (do.roll_med_acc_x == TRUE | do.roll_med_acc_y == TRUE | do.roll_med_acc_z == TRUE | 
      do.dev_roll_med_acc_x == TRUE | do.dev_roll_med_acc_y == TRUE | do.dev_roll_med_acc_z == TRUE) {
    data_processed = process_axes(data, filtertype = "rollmedian", cut_point = c(), n, sf)
    roll_median_done  = TRUE
    if (do.roll_med_acc_x == TRUE | do.roll_med_acc_y == TRUE | do.roll_med_acc_z == TRUE) { #rolling median of acceleration
      allmetrics$roll_med_acc_x = averagePerEpoch(x = data_processed[,1], sf, epochsize)
      allmetrics$roll_med_acc_y = averagePerEpoch(x = data_processed[,2], sf, epochsize)
      allmetrics$roll_med_acc_z = averagePerEpoch(x = data_processed[,3], sf, epochsize)
    }
    if (do.dev_roll_med_acc_x == TRUE | do.dev_roll_med_acc_y == TRUE | do.dev_roll_med_acc_z == TRUE) { #rolling median of acceleration
      allmetrics$dev_roll_med_acc_x = averagePerEpoch(x = abs(data[,1] - data_processed[,1]), sf, epochsize)
      allmetrics$dev_roll_med_acc_y = averagePerEpoch(x = abs(data[,2] - data_processed[, 2]), sf, epochsize)
      allmetrics$dev_roll_med_acc_z = averagePerEpoch(x = abs(data[,3] - data_processed[, 3]), sf, epochsize)
    }
  }
  if (do.anglex == TRUE | do.angley == TRUE | do.anglez == TRUE) {
    if (roll_median_done == FALSE) { 
      data_processed = process_axes(data, filtertype = "rollmedian", cut_point = c(), n, sf)
    }
    if (do.anglex == TRUE) {
      allmetrics$angle_x = averagePerEpoch(x = anglex(data_processed), sf, epochsize)
    }
    if (do.angley == TRUE) {
      allmetrics$angle_y = averagePerEpoch(x = angley(data_processed), sf, epochsize)
    }
    if (do.anglez == TRUE) {
      allmetrics$angle_z = averagePerEpoch(x = anglez(data_processed), sf, epochsize)
    }
  }
  #================================================
  # Filter-free Euclidean norm related metrics:
  EN = EuclideanNorm(data)
  if (do.enmo == TRUE) {
    ENMO = EN - 1
    ENMO[which(ENMO < 0)] = 0 #turning negative values into zero
    allmetrics$ENMO = averagePerEpoch(x = ENMO, sf, epochsize)
  }
  if (do.mad == TRUE) { # metric MAD (Mean Amplitude Deviation)
    MEANS = rep(averagePerEpoch(x = EN, sf, epochsize), each = sf * epochsize)
    MAD = abs(EN - MEANS)
    allmetrics$MAD = averagePerEpoch(x = MAD, sf, epochsize)
  }
  if (do.en == TRUE) {
    allmetrics$EN = averagePerEpoch(x = EN,sf,epochsize)
  }
  if (do.enmoa == TRUE) {
    ENMOa = abs(EN - gravity)
    allmetrics$ENMOa = averagePerEpoch(x = ENMOa, sf, epochsize)
  }
  #================================================
  # Brond Counts
  if (do.brondcounts == TRUE) {
    if (ncol(data) > 3) data = data[,2:4]
    mycounts = activityCounts::counts(data = data, hertz = sf, 
                                      x_axis = 1, y_axis = 2, z_axis = 3,
                                      start_time = Sys.time()) # ignoring timestamps, because GGIR has its own timestamps
    if (sf < 30) {
      warning("\nNote: activityCounts not designed for handling sample frequencies below 30 Hertz")
    }
    # activityCount output is per second
    # aggregate to our epoch size:
    allmetrics$BrondCount_x = sumPerEpoch(mycounts[, 2], sf = 1, epochsize)
    allmetrics$BrondCount_y = sumPerEpoch(mycounts[, 3], sf = 1, epochsize)
    allmetrics$BrondCount_z = sumPerEpoch(mycounts[, 4], sf = 1, epochsize)
  }
  return(allmetrics)
} 

