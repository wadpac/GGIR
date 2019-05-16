read.myacc.csv = function(file=c(), nrow=c(),  dec=".",
                          firstraw.acc = 1, firstrow.header=c(),
                          col.acc = 1:3, col.temp = c(), col.time=c(),
                          unit.acc = "g", unit.temp = "C", unit.time = "POSIX", format.time = "%Y-%m-%d %H:%M:%OS",
                          bitrate = c(), dynamic_range = c(), unsignedbit = TRUE,
                          origin = "1970-01-01",
                          desiredtz = "Europe/London", samplefrequency = 100,
                          headername.samplefrequency = "sample_frequency",
                          headername.deviceserialnumber = "serial_number",
                          headername.recordingid = "ID",
                          header.length = c(),
                          header.structure = c()) { # not included yet, optionally additonal columns
  # bitrate should be or header item name as character, or the actual numeric bit rate
  # unit.temp can take C(elsius), F(ahrenheit), and K(elvin) and converts it into Celsius
  if (length(firstrow.header) == 0) { # no header block
    if (firstraw.acc == 2) {
      freadheader = TRUE
    } else {
      freadheader = FALSE
    }
    skip = firstraw.acc #- 1
    header = "no header"
  } else {
    # extract header information:
    if (length(header.length) == 0) {
      header.length = firstraw.acc-1
    }
    options(warn=-1) # fread complains about quote in first row for some file types
    header_tmp = as.data.frame(data.table::fread(file,nrow = header.length, skip=firstrow.header-1,
                                                 dec=dec, showProgress = FALSE, header = FALSE))
    options(warn=0)
    if (length(header.structure) != 0) { # header is stored in 1 column, with strings that need to be split
      if (length(header_tmp) == 1) { # one header item
        header_tmp = as.matrix(unlist(strsplit(as.character(header_tmp[,1]), header.structure)))
      } else { # multiple header items
        if (ncol(header_tmp) > 1) {
          # collapse columns to one
          for (i in 1:length(header_tmp)) { "remove quotes in character"
            header_tmp[i] = gsub(pattern = "\"",replacement = "",x = header_tmp[i])
          }
          header_tmp = do.call(paste0, header_tmp)
        }
        mysplit = function(x){
          tmp = strsplit(as.character(x),header.structure)
          tmp = unlist(tmp)
          return(tmp)
        }
        header_tmp0 = header_tmp
        header_tmp = unlist(lapply(header_tmp, FUN=mysplit))
        if (length(header_tmp) > 2) {
          header_tmp = data.frame(matrix(unlist(header_tmp), nrow=nrow(header_tmp0), byrow=T))
        } else {
          header_tmp = data.frame(matrix(unlist(header_tmp), nrow=1, byrow=T))
          colnames(header_tmp) = NULL
        }
      }
      if (ncol(header_tmp) == 1) header_tmp = t(header_tmp)
      header_tmp2 = data.frame(a = header_tmp[,2])
      colnames(header_tmp2) = NULL
      row.names(header_tmp2) = header_tmp[,1] 
      header = header_tmp2
    } else { # column 1 is header name, column 2 is header value
      colnames(header_tmp) = NULL
      validrows = which(is.na(header_tmp[,1]) == FALSE & header_tmp[,1] != "")
      header_tmp = header_tmp[validrows,1:2]
      header_tmp2 = as.data.frame(header_tmp[,2])
      row.names(header_tmp2) = header_tmp[,1]
      colnames(header_tmp2) = NULL
      header = header_tmp2
    }
    skip = firstraw.acc-1
    freadheader = TRUE
    # assess whether accelerometer data conversion is needed
    if (length(bitrate) > 0 & length(dynamic_range) > 0 & unit.acc == "bit") {
      if (is.character(bitrate[1]) == TRUE) { # extract bitrate if it is in the header
        bitrate = as.numeric(header[which(row.names(header) == bitrate[1]),1])
      }
      if (dynamic_range[1] == "dynamic_range") { # extract dynamic range if it is in the header
        dynamic_range = as.numeric(header[which(row.names(header) == dynamic_range[1]),1])
      } 
    }
  }
  
  # read data from file
  P = as.data.frame(data.table::fread(file,nrow = nrow, skip=skip,
                                      dec=dec, showProgress = FALSE, header = freadheader))
  # select relevant columns, add standard column names
  P = P[,c(col.time,col.acc,col.temp)]
  if (length(col.time) > 0 & length(col.temp) > 0) {
    colnames(P) = c("timestamp","accx","accy","accz","temperature")
  } else if (length(col.time) > 0 & length(col.temp) == 0) {
    colnames(P) = c("timestamp","accx","accy","accz")
  } else if (length(col.time) == 0 & length(col.temp) > 0) {
    colnames(P) = c("accx","accy","accz","temperature")
  } else if (length(col.time) == 0 & length(col.temp) == 0) {
    colnames(P) = c("accx","accy","accz")
  }
  # acceleration and temperature as numeric
  P$accx = as.numeric(P$accx)
  P$accy = as.numeric(P$accy)
  P$accz = as.numeric(P$accz)
  if (length(col.temp) > 0) P$temperature = as.numeric(P$temperature) 
  # Convert timestamps
  if (length(col.time) > 0) {
    if (unit.time == "POSIX") {
      P$timestamp = as.POSIXlt(P$timestamp, origin=origin,tz = desiredtz)
    } else if (unit.time == "UNIXsec") {
      P$timestamp = as.POSIXlt(P$timestamp, origin=origin,tz = desiredtz)
    } else if (unit.time == "ActivPAL") {
      # origin should be specified as: "1899-12-30"
      origin = "1899-12-30"
      datecode = round(P$timestamp) * 3600*24
      tmp2 = P$timestamp - round(P$timestamp)
      timecode = ((tmp2 * 10^10)*8.64) / 1000000
      numerictime = datecode +timecode
      P$timestamp= as.POSIXlt(numerictime,origin=origin,tz=desiredtz)
    }
  }
  # If acceleration is stored in mg units then convert to gravitational units
  if (unit.acc == "mg") {
    P$accx = P$accx * 1000
    P$accy = P$accy * 1000
    P$accz = P$accz * 1000
  }
  # If acceleration is stored in bit values then convert to gravitational unit
  if (length(bitrate) > 0 & length(dynamic_range) > 0 & unit.acc == "bit") {
    if (unsignedbit == TRUE) {
      P$accx = ((P$accx/(2^bitrate)) - 0.5) * 2*dynamic_range
      P$accy = ((P$accy/(2^bitrate)) - 0.5) * 2*dynamic_range
      P$accz = ((P$accz/(2^bitrate)) - 0.5) * 2*dynamic_range
    } else if (unsignedbit == FALSE) { # signed bit
      P$accx = (P$accx/((2^bitrate)/2)) * dynamic_range
      P$accy = (P$accy/((2^bitrate)/2)) * dynamic_range
      P$accz = (P$accz/((2^bitrate)/2)) * dynamic_range
    }
  }
  # Convert temperature units
  if (unit.temp == "K") {
    P$temperature = P$temperature + 272.15 # From Kelvin to Celsius
  } else if (unit.temp == "F") {
    P$temperature = (P$temperature - 32) * (5/9) # From Fahrenheit to Celsius
  }
  
  return(list(data=P,header=header))
}