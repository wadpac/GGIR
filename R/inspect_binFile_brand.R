inspect_binFile_brand = function(filename) {
  mon = "not_recognised"
  # Extract information from the fileheader
  suppressWarnings({fh = readLines(filename, 69)})
  # look for device type field
  suppressWarnings({deviceGeneactiv = grep("Device Type", fh, ignore.case = T)})
  if (length(deviceGeneactiv) > 0) {
    if (grepl("GENEActiv|GENEAsleep", fh[deviceGeneactiv])) mon = MONITOR$GENEACTIV
  } else {
    # check if it is a Matrix device
    raw = readBin(filename, "raw", file.info(filename)$size)
    # Read the header (bytes 513-516)
    header_raw = raw[513:516]
    header = rawToChar(header_raw[header_raw != 0], multiple = FALSE)
    # if matrix device, header is MDTC, otherwise it is a corrupt file
    if (header == "MDTC") mon = MONITOR$PARMAY_MTX
  }
  return(mon)
}
