POSIXtime2iso8601 = function(x,tz){
  POStime = as.POSIXlt(x,tz) #turn to right timezone
  POStime_z = chartime2iso8601(format(POStime),tz) #change format
  return(POStime_z)
}