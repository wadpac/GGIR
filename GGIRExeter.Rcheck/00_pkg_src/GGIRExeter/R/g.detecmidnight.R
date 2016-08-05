g.detecmidnight = function(ND,time) {
  midnights = midnightsi = matrix(0,(ND+3),1)
  countmidn = 1
  pr = 0
  for (cnt in 1:length(time)) { #need to detect all midnights because some days may be 23 or 25 hours
    temp = unlist(strsplit(time[cnt]," "))[2]
    temp2 = as.numeric(unlist(strsplit(temp,":"))[1])
    temp3 = as.numeric(unlist(strsplit(temp,":"))[2])
    temp4 = as.numeric(unlist(strsplit(temp,":"))[3])
    nr = temp2+temp3+temp4
    if ((pr == 68 & nr == 1 & temp3 == 0 & temp4 == 0) | (nr == 0)) { #68 means a jump from 23:45 to 1:00 #added 9-5-2013
      midnights[countmidn] = as.character(time[cnt])
      midnightsi[countmidn] = cnt
      countmidn = countmidn + 1
    }
    pr = nr
  }
  if (countmidn == 1) {
    tooshort = 1
    lastmidnight = time[length(time)]
    lastmidnighti = length(time)
    firstmidnight = time[1]
    firstmidnighti = 1
  } else {
    midnights = midnights[-c(which(as.numeric(midnightsi) == 0))]
    midnightsi = midnightsi[-c(which(as.numeric(midnightsi) == 0))]  
    lastmidnight = midnights[length(midnights)]
    lastmidnighti = midnightsi[length(midnights)]
    firstmidnight = midnights[1]
    firstmidnighti = midnightsi[1]
  }
  invisible(list(firstmidnight=firstmidnight,firstmidnighti=firstmidnighti,
                 lastmidnight=lastmidnight,lastmidnighti=lastmidnighti,
                 midnights=midnights,midnightsi=midnightsi))
}