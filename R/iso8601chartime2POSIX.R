iso8601chartime2POSIX = function(x, tz){
  return(as.POSIXlt(x = x, format = "%Y-%m-%dT%H:%M:%S%z", tz = tz))
}