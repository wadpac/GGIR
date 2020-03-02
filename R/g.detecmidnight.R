g.detecmidnight = function(time,desiredtz,dayborder) {
  # code in this function is able to deal with two types of timestamp format
  convert2clock = function(x) { #ISO format
    return(as.character(format(as.POSIXlt(x,format="%Y-%m-%dT%H:%M:%S%z",tz=desiredtz),"%H:%M:%S")))
  }
  convert2clock_space = function(x) { #POSIX format
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
  midn = which(checkmidnight_out == dayborder) #replaced "== 0" for "== dayborder" to work in any scenario
  if (length(midn) == 0) { # measurement with no midnights, use last timestamp as dummy midnight for g.analyse() to work
    midnights = time[length(time)]
    midnightsi = length(time)
  } else {
    midnights = as.character(time[midn])
    midnightsi = midn
  }
  if (length(midn) == 0) { # measurement with no midnights, use last timestamp as dummy midnight for g.analyse() to work
    lastmidnight = time[length(time)]
    lastmidnighti = length(time)
    firstmidnight = time[1]
    firstmidnighti = 1
  } else {
    lastmidnight = midnights[length(midnights)]
    lastmidnighti = midnightsi[length(midnights)]
    firstmidnight = midnights[1]
    firstmidnighti = midnightsi[1]
  }
  invisible(list(firstmidnight=firstmidnight,firstmidnighti=firstmidnighti,
                 lastmidnight=lastmidnight,lastmidnighti=lastmidnighti,
                 midnights=midnights,midnightsi=midnightsi))
}
