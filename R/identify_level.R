identify_levels = function(time,diur,sibdetection,ACC,
                           TRLi,TRMi,TRVi,
                           boutdur.mvpa,boutcriter.mvpa,
                           boutdur.lig,boutcriter.lig,
                           boutdur.in,boutcriter.in,
                           ws3,bout.metric) {
  
  #=======================================================
  # LABEL INSENTITY LEVELS
  LEVELS = rep(0,length(time))
  OLEVELS = rep(0,length(time)) #to capture moderate and vigorous seperately
  LEVELS[which(sibdetection == 1 & diur == 1)] = 0 #nocturnal sleep
  LEVELS[which(sibdetection == 0 & diur == 1)] = 1 #nocturnal waking
  Lnames = c("nightsleep",paste("nightwak_and_IN",TRLi,sep=""))
  # activity during the night
  LEVELS[which(sibdetection == 0 & diur == 1 & ACC > TRLi & ACC <= TRMi)] = 2 #LIGHT
  LEVELS[which(sibdetection == 0 & diur == 1 & ACC > TRMi & ACC <= TRVi)] = 3 #MODERATE
  LEVELS[which(sibdetection == 0 & diur == 1 & ACC > TRVi)] = 4 #VIGOROUS
  Lnames = c(Lnames,paste("nightwak_LIG",TRLi,"_",TRMi,sep=""),
             paste("nightwak_MOD",TRMi,"_",TRVi,sep=""),
             paste("nightwak_VIG",TRVi,sep=""))
  # activity during the day
  LEVELS[which(sibdetection == 1 & diur == 0)] = 5 #daytime sustained inactivity
  #============================================================
  # newly added on 20 may 2015:
  if (length(sibdetection == 1) > 0) ACC[sibdetection == 1] = 0 #turn all acceleration to zero if sustained inactivity bouts are detected
  #======================================
  LEVELS[which(sibdetection == 0 & diur == 0 & ACC <= TRLi)] = 6 #other inactivity
  LEVELS[which(sibdetection == 0 & diur == 0 & ACC > TRLi & ACC <= TRMi)] = 7 #LIGHT
  LEVELS[which(sibdetection == 0 & diur == 0 & ACC > TRMi & ACC <= TRVi)] = 8 #MODERATE
  LEVELS[which(sibdetection == 0 & diur == 0 & ACC > TRVi)] = 9 #VIGOROUS
  Lnames = c(Lnames,"day_SIB",paste("day_OIN",TRLi,sep=""),
             paste("day_LIG",TRLi,"_",TRMi,sep=""),
             paste("day_MOD",TRMi,"_",TRVi,sep=""),
             paste("day_VIG",TRVi,sep=""))
  # store separate copy of moderate and vigorous levels
  OLEVELS[which(LEVELS == 5)] = 1 #SIB
  OLEVELS[which(LEVELS == 6)] = 2 #OIN
  OLEVELS[which(LEVELS == 7)] = 3 #LIGHT
  OLEVELS[which(LEVELS == 8)] = 4 #MOD
  OLEVELS[which(LEVELS == 9)] = 5 #VIG
  #-------------------------------------
  # NEW MVPA BOUTS
  LN = length(time)
  boutdur.mvpa = sort(boutdur.mvpa,decreasing = TRUE)
  boutduration = boutdur.mvpa * (60/ws3) 
  NBL = length(boutduration) #number of bout lengths
  CL = 10 #current level
  refe = rep(0,LN)
  bc.mvpa = c()
  for (BL in 1:NBL) { # needs to be flexible to varibale number of bout lengths
    rr1 = rep(0,LN)
    p = which(sibdetection == 0 & ACC >= TRMi & refe == 0 & diur == 0); rr1[p] = 1
    out1 = g.getbout(x=rr1,boutduration=boutduration[BL],boutcriter=boutcriter.mvpa,
                     closedbout=FALSE,bout.metric=bout.metric,ws3=ws3)
    LEVELS[which(diur == 0 & out1$x == 1)] = CL
    bc.mvpa = rbind(bc.mvpa,out1$boutcount)
    refe = refe + out1$x
    Lnames = c(Lnames,paste0("MVPA_D",boutdur.mvpa[BL],"T",TRMi))
    CL = CL + 1
  }
  #-------------------------------------
  # NEW INACTIVITY BOUTS
  LN = length(time)
  boutdur.in = sort(boutdur.in,decreasing = TRUE)
  boutduration = boutdur.in * (60/ws3)
  NBL = length(boutduration) #number of bout lengths
  # refe = rep(0,LN)
  bc.in = c()
  for (BL in 1:NBL) {
    rr1 = rep(0,LN)
    p = which((sibdetection == 1 | ACC < TRLi) & refe == 0 & diur == 0); rr1[p] = 1
    out1 = g.getbout(x=rr1,boutduration=boutduration[BL],boutcriter=boutcriter.in,
                     closedbout=FALSE,bout.metric=bout.metric,ws3=ws3)
    LEVELS[which(diur == 0 & out1$x == 1)] = CL
    bc.in = rbind(bc.in,out1$boutcount)
    refe = refe + out1$x
    Lnames = c(Lnames,paste0("INB_D",boutdur.in[BL],"T",TRLi))
    CL = CL + 1
  }
  #-------------------------------------
  # NEW LIGHT BOUTS
  LN = length(time)
  boutdur.lig = sort(boutdur.lig,decreasing = TRUE)
  boutduration = boutdur.lig * (60/ws3)
  NBL = length(boutduration) #number of bout lengths
  bc.lig = c()
  for (BL in 1:NBL) {
    rr1 = rep(0,LN)
    p = which(sibdetection == 0 & ACC >= TRLi & refe ==0 & ACC < TRMi & diur == 0); rr1[p] = 1
    out1 = g.getbout(x=rr1,boutduration=boutduration[BL],boutcriter=boutcriter.lig,
                     closedbout=FALSE,bout.metric=bout.metric,ws3=ws3)                    
    LEVELS[which(diur == 0 & out1$x == 1)] = CL
    bc.lig = rbind(bc.lig,out1$boutcount)
    refe = refe + out1$x
    Lnames = c(Lnames,paste0("LIGB_D",boutdur.lig[BL],"T",TRLi,"_",TRMi))
    CL = CL + 1
  }
  invisible(list(LEVELS=LEVELS,OLEVELS=OLEVELS,Lnames=Lnames,bc.mvpa=bc.mvpa,bc.lig =bc.lig ,bc.in=bc.in))
}