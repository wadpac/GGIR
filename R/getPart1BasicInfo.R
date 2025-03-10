getPart1BasicInfo = function(fn, idloc, tz) {
  load(fn)
  if (is.null(M$metashort)) return()
  hvars = g.extractheadervars(I)
  if (exists("Clist")) {
    ID = NA # If Clist exists then ignore this file as it was previously appended
  } else {
    ID = extractID(hvars, idloc, fname = I$filename)
  }
  start = as.POSIXct(x = M$metashort$timestamp[1], format = "%Y-%m-%dT%H:%M:%S%z", tz = tz)
  end = as.POSIXct(x = M$metashort$timestamp[nrow(M$metashort)], format = "%Y-%m-%dT%H:%M:%S%z", tz = tz)
  info = data.frame(ID = ID, start = start, end = end, filename = fn, brand = I$monn)
  return(info)
}