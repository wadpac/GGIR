g.applymetrics = function(Gx,Gy,Gz,n,sf,ws3,metrics2do){
  # globalVariables(c("do.enmo","do.lfenmo","do.en","do.bfen","do.hfen",
                    # "do.hfenplus", "do.mad", "do.anglex","do.angley","do.anglez",
                    # "do.roll_med_acc_x","do.roll_med_acc_y","do.roll_med_acc_z",
                    # "do.dev_roll_med_acc_x","do.dev_roll_med_acc_y",
                    # "do.dev_roll_med_acc_z","do.enmoa"))
  # as R check complains about attach, I have just added them to the workspace manually:  
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
  
  # attach(metrics2do,warn.conflicts = FALSE)
  allmetrics = c()
  averageperws3 = function(x,sf,ws3) {
    x2 =cumsum(c(0,x))
    select = seq(1,length(x2),by=sf*ws3)
    x3 = diff(x2[round(select)]) / abs(diff(round(select)))
  }
  #--------------------------------------------------
  # BFEN = band pass filtered signals followed by Euclidean norm
  lb = 0.2; hb = 15; n = 4; TW = 1/lb
  if (sf <= (hb *2)) { #avoid having a higher filter boundary higher than sf/2
    hb = round(sf/2) - 1
  }
  if (do.bfen == TRUE) {
    BFEN = g.metric(Gx,Gy,Gz,n,sf=sf,ii=7,TW=TW,lb=lb,hb=hb) #calling function metric.R to do the calculation
    allmetrics$BFEN3b = averageperws3(x=BFEN,sf,ws3)
  }
  
  #-----------------------------------------------------
  #deriving metric ENMO (Euclidean Norm Minus One)
  if (do.enmo == TRUE) {
    EN = sqrt(Gx^2 + Gy^2 + Gz^2)
    ENMO = EN - 1
    ENMO[which(ENMO < 0)] = 0 #turning negative values into zero
    allmetrics$ENMO3b = averageperws3(x=ENMO,sf,ws3)
  }
  #-----------------------------------------------------
  #deriving metric MAD (Mean Amplitude Deviation)
  if (do.mad == TRUE) {
    MEANS = rep(averageperws3(x=EN,sf,ws3), each = sf*ws3)
    MAD = abs(EN - MEANS)
    allmetrics$MAD3b = averageperws3(x=MAD,sf,ws3)
  }
  #------------------------------------------------------------
  # space for extra metrics
  if (do.lfenmo == TRUE) {
    LFENMO =g.metric(Gx,Gy,Gz,n,sf=sf,ii=9,TW=TW,lb=lb,hb=3.5) #calling function metric.R to do the calculation
    LFENMO[which(LFENMO < 0)] = 0
    allmetrics$LFENMO3b = averageperws3(x=LFENMO,sf,ws3)
  }
  if (do.hfen == TRUE) {
    HFEN =g.metric(Gx,Gy,Gz,n,sf=sf,ii=1,TW=TW,lb=lb,hb=hb) #calling function metric.R to do the calculation
    allmetrics$HFEN3b = averageperws3(x=HFEN,sf,ws3)
  }
  # do this one anyway
  EN = g.metric(Gx,Gy,Gz,n,sf=sf,ii=3,TW=TW,lb=lb,hb=hb) #calling function metric.R to do the calculation
  EN2 =cumsum(c(0,EN))
  select = seq(1,length(EN2),by=sf*ws3)
  allmetrics$EN3b = diff(EN2[round(select)]) / abs(diff(round(select)))
  if (do.hfenplus == TRUE) {
    HFENplus = g.metric(Gx,Gy,Gz,n,sf=sf,ii=5,TW=TW,lb=lb,hb=hb) #calling function metric.R to do the calculation
    HFENplus[which(HFENplus < 0)] = 0
    #averaging HFENplus per ws3 seconds
    allmetrics$HFENplus3b = averageperws3(x=HFENplus,sf,ws3)
  }
  #teLindert code removed because never used, and never verified to be correct
  if (do.anglex == TRUE | do.angley == TRUE | do.anglez == TRUE) {
    angle = g.metric(Gx,Gy,Gz,n,sf=sf,ii=11,TW=TW,lb=lb,hb=hb) #calling function metric.R to do the calculation
    angle_x = angle[,1]; angle_y = angle[,2]; angle_z = angle[,3]
    #averaging per ws3 seconds
    allmetrics$angle_x3b = averageperws3(x=angle_x,sf,ws3)
    allmetrics$angle_y3b = averageperws3(x=angle_y,sf,ws3)
    allmetrics$angle_z3b = averageperws3(x=angle_z,sf,ws3)
  }
  
  if (do.roll_med_acc_x == TRUE | do.roll_med_acc_y == TRUE | do.roll_med_acc_z == TRUE) { #rolling median of acceleration
    roll_med_acc_ = g.metric(Gx,Gy,Gz,n,sf=sf,ii=13,TW=TW,lb=lb,hb=hb) #calling function metric.R to do the calculation
    roll_med_acc_x = roll_med_acc_[,1]; roll_med_acc_y = roll_med_acc_[,2]; roll_med_acc_z = roll_med_acc_[,3]
    allmetrics$roll_med_acc_x3b = averageperws3(x=roll_med_acc_x,sf,ws3)
    allmetrics$roll_med_acc_y3b = averageperws3(x=roll_med_acc_y,sf,ws3)
    allmetrics$roll_med_acc_z3b = averageperws3(x=roll_med_acc_z,sf,ws3)
  }
  
  if (do.dev_roll_med_acc_x == TRUE | do.dev_roll_med_acc_y == TRUE | do.dev_roll_med_acc_z == TRUE) { #rolling median of acceleration
    dev_roll_med_acc = g.metric(Gx,Gy,Gz,n,sf=sf,ii=14,TW=TW,lb=lb,hb=hb) #calling function metric.R to do the calculation
    dev_roll_med_acc_x = dev_roll_med_acc[,1]; dev_roll_med_acc_y = dev_roll_med_acc[,2]; dev_roll_med_acc_z = dev_roll_med_acc[,3]
    allmetrics$dev_roll_med_acc_x3b = averageperws3(x=dev_roll_med_acc_x,sf,ws3)
    allmetrics$dev_roll_med_acc_y3b = averageperws3(x=dev_roll_med_acc_y,sf,ws3)
    allmetrics$dev_roll_med_acc_z3b = averageperws3(x=dev_roll_med_acc_z,sf,ws3)
  }
  if (do.enmoa == TRUE) {
    ENMOa =g.metric(Gx,Gy,Gz,n,sf=sf,ii=12,TW=TW,lb=lb,hb=3.5) #calling function metric.R to do the calculation
    ENMOa[which(ENMOa < 0)] = 0
    allmetrics$ENMOa3b = averageperws3(x=ENMOa,sf,ws3)
  }
  # detach(metrics2do,warn.conflicts = FALSE)
  return(allmetrics)
} 
