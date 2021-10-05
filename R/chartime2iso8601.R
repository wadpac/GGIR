chartime2iso8601 = function(x, tz) {
  POStime = as.POSIXlt(as.numeric(as.POSIXlt(x, tz)), origin = "1970-1-1", tz)
  POStimeISO = strftime(POStime, format = "%Y-%m-%dT%H:%M:%S%z")
  return(POStimeISO)
}