g.getM5L5 = function(varnum,ws3,t0_LFMF,t1_LFMF,M5L5res,winhr) {
  #diurnal pattern features extracted only meaningful if more than 16 hours
  V5NIGHT = mean(varnum[((1*60*(60/ws3))+1):(6*60*(60/ws3))]) * 1000#from 1am to 6am
  reso = M5L5res #resolution at 5 minutes
  nwindow_f = (t1_LFMF-winhr) - t0_LFMF #number of windows for L5M5 analyses
  nwindow_f = nwindow_f * (60/reso)
  DAYrunav5 = matrix(0,nwindow_f,1)
  
  for (hri in (t0_LFMF*(60/reso)):(((t1_LFMF-winhr)*(60/reso))-1)) { #e.g.9am-9pm
    e1 = (hri*reso*(60/ws3))+1 #e.g. 9am
    e2 = (hri+(winhr*(60/reso)))*reso*(60/ws3) #e.g. 9am + 5 hrs
    if (e2 <= length(varnum)) {
      einclude = e1:e2
    } else { #allow for analyses beyond end of day 
      einclude = c(1:(e2-length(varnum)),e1:length(varnum))
    }
    DAYrunav5[((hri-(t0_LFMF*(60/reso)))+1),1] = mean(varnum[einclude])
  }
  valid = which(is.na(DAYrunav5) == F)
  DAYL5HOUR = ((which(DAYrunav5 == min(DAYrunav5[valid]) & is.na(DAYrunav5) == F)-1)/(60/reso)) + t0_LFMF #- 1
  DAYL5VALUE = min(DAYrunav5[valid]) * 1000
  DAYM5HOUR = ((which(DAYrunav5 == max(DAYrunav5[valid])  & is.na(DAYrunav5) == F)-1)/(60/reso)) + t0_LFMF #- 1
  DAYM5VALUE = max(DAYrunav5[valid]) * 1000
  if (length(DAYL5VALUE) > 1) { DAYL5VALUE = sort(DAYL5VALUE)[ceiling(length(DAYL5VALUE)/2)] }
  if (length(DAYL5HOUR) > 1) { DAYL5HOUR = sort(DAYL5HOUR)[ceiling(length(DAYL5HOUR)/2)] }
  if (length(DAYM5VALUE) > 1) { DAYM5VALUE = sort(DAYM5VALUE)[ceiling(length(DAYM5VALUE)/2)] }
  if (length(DAYM5HOUR) > 1) { DAYM5HOUR = sort(DAYM5HOUR)[ceiling(length(DAYM5HOUR)/2)] }
  invisible(list(DAYL5HOUR=DAYL5HOUR[1],DAYL5VALUE=DAYL5VALUE,DAYM5HOUR=DAYM5HOUR,DAYM5VALUE=DAYM5VALUE,V5NIGHT=V5NIGHT))
}