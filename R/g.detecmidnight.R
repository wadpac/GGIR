g.detecmidnight = function(ND,time,desiredtz) {
  midnights = midnightsi = matrix(0,(ND+3),1)
  countmidn = 1
  pr = 0
  convert2clock = function(x) {
    return(as.character(format(as.POSIXlt(x,format="%Y-%m-%dT%H:%M:%S%z",tz=desiredtz),"%H:%M:%S")))
  }
  convert2clock_space = function(x) {
    return(unlist(strsplit(x," "))[2])
  }
  checkmidnight = function(x){
    temp1 = unlist(strsplit(x,":"))
    return(sum(as.numeric(temp1)))
  }
  space = ifelse(length(unlist(strsplit(time[1]," "))) > 1,TRUE,FALSE)
  if (space == TRUE) {
    time_clock = sapply(time,FUN=convert2clock_space)
    checkmidnight_out = lapply(time_clock,FUN=checkmidnight)
  } else {
    time_clock = convert2clock(time)
    checkmidnight_out = unlist(lapply(time_clock,FUN=checkmidnight))
  }
  midn = which(checkmidnight_out == 0)
  midnights = as.character(time[midn])
  midnightsi = midn
  countmidn = length(midnightsi)

  # for (cnt in 1:length(time)) { #need to detect all midnights because some days may be 23 or 25 hours
  #   
  #   space = unlist(strsplit(time[cnt]," "))
  #   if (length(space) > 1) {
  #     temp = unlist(strsplit(time[cnt]," "))[2]
  #   } else {
  #     temp = format(as.POSIXlt(time[cnt],format="%Y-%m-%dT%H:%M:%S%z",tz=desiredtz),"%H:%M:%S")
  #   }
  #   # temp = unlist(strsplit(time[cnt]," "))[2]
  #   temp2 = as.numeric(unlist(strsplit(temp,":"))[1])
  #   temp3 = as.numeric(unlist(strsplit(temp,":"))[2])
  #   temp4 = as.numeric(unlist(strsplit(temp,":"))[3])
  #   nr = temp2+temp3+temp4
  #   
  #   
  #   
  #   
  #   if ((pr == 68 & nr == 1 & temp3 == 0 & temp4 == 0) | (nr == 0)) { #68 means a jump from 23:45 to 1:00 #added 9-5-2013
  #     midnights[countmidn] = as.character(time[cnt])
  #     midnightsi[countmidn] = cnt
  #     countmidn = countmidn + 1
  #   }
  #   pr = nr
  # }
  if (countmidn == 1) {
    tooshort = 1
    lastmidnight = time[length(time)]
    lastmidnighti = length(time)
    firstmidnight = time[1]
    firstmidnighti = 1
  } else {
    cut = which(as.numeric(midnightsi) == 0)
    if (length(cut) > 0) { # not sure why I was epecting zero indices, but doesn't seem to harm anything else
      midnights = midnights[-cut]
      midnightsi = midnightsi[-cut]
    }
    lastmidnight = midnights[length(midnights)]
    lastmidnighti = midnightsi[length(midnights)]
    firstmidnight = midnights[1]
    firstmidnighti = midnightsi[1]
  }

  invisible(list(firstmidnight=firstmidnight,firstmidnighti=firstmidnighti,
                 lastmidnight=lastmidnight,lastmidnighti=lastmidnighti,
                 midnights=midnights,midnightsi=midnightsi))
}