read.myacc.csv = function(file=c(), nrow=c(),  dec=c(),
                                firstraw.acc = 1, firstrow.header=c(),
                                col.acc = 1:3, col.temp = c(), col.time=c(),
                                unit.acc = "g", unit.temp = "C", format.time = "%Y-%m-%d %H:%M:%OS",
                                origin = "1970-01-01",
                                desiredtz = "Europe/London", samplefrequency = 100,
                                headername.samplefrequency = "sample_frequency",
                                headername.deviceserialnumber = "serial_number",
                                headername.recordingid = "ID") { # not included yet, optionally additonal columns
  if (length(firstrow.header) == 0) { # no header block
    if (firstraw.acc == 2) {
      freadheader = TRUE
    } else {
      freadheader = FALSE
    }
    skip = firstraw.acc #- 1
    header = "no header"
  } else {
    # extract header information
    # ...
    header_tmp = as.data.frame(data.table::fread(file,nrow = firstraw.acc-1, skip=firstrow.header-1,
                                                  dec=dec, showProgress = FALSE, header = FALSE))
    colnames(header_tmp) = NULL
    validrows = which(is.na(header_tmp[,1]) == FALSE & header_tmp[,1] != "")
    header_tmp = header_tmp[validrows,1:2]
    header_tmp2 = as.data.frame(header_tmp[,2])
    
    row.names(header_tmp2) = header_tmp[,1]
    colnames(header_tmp2) = NULL
    header = header_tmp2
    skip = firstraw.acc-1
    freadheader = TRUE
  }
  P = as.data.frame(data.table::fread(file,nrow = nrow, skip=skip,
                        dec=dec, showProgress = FALSE, header = freadheader))
  P = P[,c(col.time,col.acc,col.temp)]
  if (length(col.time) > 0 & length(col.temp) > 0) {
    colnames(P) = c("timestamp","accx","accy","accz","temperature")
    P$timestamp = as.POSIXlt(P$timestamp, origin=origin,tz = desiredtz)
  } else if (length(col.time) > 0 & length(col.temp) == 0) {
    colnames(P) = c("timestamp","accx","accy","accz")
    P$timestamp = as.POSIXlt(P$timestamp, origin=origin,tz = desiredtz)
  } else if (length(col.time) == 0 & length(col.temp) > 0) {
    colnames(P) = c("accx","accy","accz","temperature")
  } else if (length(col.time) == 0 & length(col.temp) == 0) {
    colnames(P) = c("accx","accy","accz")
  }
  # Convert acc units
  # ...
  
  # Convert temp units
  # ...
  
  return(list(data=P,header=header))
}