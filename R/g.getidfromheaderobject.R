g.getidfromheaderobject = function(filename, header, dformat, mon) {
  if (dformat == 1) {
    if (mon == 1) { #reading the binary file
      ID = as.character(header[which(rownames(header) == "Volunteer_Number"),1])
    } else if (mon == 2) { #reading the binary file
      # ID = as.character(header[which(header[,1] == "Subject_Code"),2])
      ID = as.character(header[which(rownames(header) == "Subject_Code"),1])
    } else if (mon == 5) {
      ID = c()
    }
  } else if (dformat == 2) {
    if (mon == 2) {
      ID = as.character(header[which(as.character(header[,1]) == "Subject Code"),2])
    } else if (mon == 3 | mon == 6 | mon == 4) {
      ID = filename #ID not stored in fileheader, but filename instead
    }
  } else if (dformat == 3 | dformat == 4 | dformat == 6) {
    ID = filename # for now use filename as IDentifier
  } else if (dformat == 5) {
    if (length(which(row.names(header) == "recordingID")) > 0) {
      ID = header[which(row.names(header) == "recordingID"),1]
    } else {
      ID = c()
    }
  } 
  return(ID)
}
