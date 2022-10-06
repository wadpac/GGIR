POSIXtime2iso8601 = function(x,tz){
  chartime2iso8601 = function(x, tz) {
    POStime = as.POSIXlt(as.numeric(as.POSIXlt(x, tz)), origin = "1970-1-1", tz)
    POStimeISO = strftime(POStime, format = "%Y-%m-%dT%H:%M:%S%z")
    return(POStimeISO)
  }
  POStime = as.POSIXlt(x,tz) #turn to right timezone
  POStime_z = chartime2iso8601(format(POStime),tz) #change format
  return(POStime_z)
}