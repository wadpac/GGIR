g.extractheadervars = function(I) {
  header = I$header
  mon = I$monn
  hnames = rownames(header)
  hvalues = as.character(as.matrix(header))
  pp = which(hvalues == "")
  hvalues[pp] = c("not stored in header")
  if (mon == "genea") {
    IDd = hvalues[which(hnames == "Volunteer_Number")]    	
    ID = as.character(unlist(IDd))
    iIDd = hvalues[which(hnames == "Investigator_Id")]			
    iID = as.character(unlist(iIDd))
    HN = "not available" #not stored by genea
    sensor.location = hvalues[which(hnames == "Body_Location")]
    SX = "not available" #not stored by genea
    deviceSerialNumber = hvalues[which(hnames == "Serial_Number")] #serial number
  } else if (mon == "geneactive") {
    check_GENEAread = which(hnames == "Subject_Code")
    if (length(check_GENEAread) > 0) { 
      # This if-statement can be deprecated once GENEAread is deprecated as a dependency
      ID = hvalues[which(hnames == "Subject_Code")] #; temp2 = unlist(strsplit(as.character(temp)," "))
      iID = hvalues[which(hnames == "Investigator_ID")] #investigator ID
      HN = hvalues[which(hnames == "Handedness_Code")] #handedness
      sensor.location = as.character(as.matrix(hvalues[which(hnames == "Device_Location_Code")])) #body location
      SX = hvalues[which(hnames == "Sex")] #gender
      deviceSerialNumber = hvalues[which(hnames == "Device_Unique_Serial_Code")] #serial number
    } else {
      ID = ""
      iID = ""
      HN = ""
      sensor.location = ""
      SX = ""
      deviceSerialNumber = hvalues[which(hnames == "serial_number")] #serial number
    }
  } else if (mon == "actigraph" | mon == 'verisense') {
    if (I$dformn == "gt3x") {
      header = I$header
      deviceSerialNumber = as.character(header["Serial.Number",])
      firmwareversion = as.character(header["Firmware",])
    } else { # .csv format
      deviceSerialNumber = as.character(I$header$value[which(row.names(I$header) == "Serial Number:")])
      if (length(deviceSerialNumber) == 0) deviceSerialNumber = "not extracted" #serial number
      # try to extract firmware version and append it to serial number,
      # at a later point in time this could become a separate variable in the reports
      firmwareversion = unlist(strsplit(unlist(strsplit(as.character(I$header$value[1]),"Firmware[.]"))[2],"[.]date"))[1]
    }
    if (length(firmwareversion) == 1) deviceSerialNumber = paste0(deviceSerialNumber,"_firmware_",firmwareversion)
    ID = "not extracted"	# volunteer ID as stored in binary file header
    iID = "not extracted" #investigator ID
    HN = "not extracted" #handedness
    sensor.location = "not extracted" #body location
    SX = "not extracted" #gender
  } else if (mon == "axivity" | mon == "movisens") { #todo: create automatic extraction of information from actigraph fileheader
    if (mon  == "actigraph") {
      deviceSerialNumber = as.character(I$header$value[which(row.names(I$header) == "Serial Number:")])
    }
    ID = "not extracted"	# volunteer ID as stored in binary file header
    iID = "not extracted" #investigator ID
    HN = "not extracted" #handedness
    sensor.location = "not extracted" #body location
    SX = "not extracted" #gender
    deviceSerialNumber = "not extracted" #serial number
    if (mon == "axivity") {
      seriali = which(hnames == "uniqueSerialCode")
      if (length(seriali) > 0) deviceSerialNumber = hvalues[seriali[1]] #serial number			
    }
    if (mon == "movisens") {
      deviceSerialNumber = as.character(I$header$value[which(row.names(I$header) == "serialnumber")])
      ID = as.character(I$header$value[which(row.names(I$header) == "ID")])
    }
  } else if (mon == "unknown") {
    if (length(which(hnames == "recordingID")) > 0) {
      ID = hvalues[which(hnames == "recordingID")]
    } else {
      ID = "not extracted"
    }
    iID = "not extracted" #investigator ID
    HN = "not extracted" #handedness
    sensor.location = "not extracted" #body location
    SX = "not extracted" #gender
    if (length(which(hnames == "device_serial_number")) > 0) {
      deviceSerialNumber = hvalues[which(hnames == "device_serial_number")]			 #serial number
    } else {
      deviceSerialNumber = "not extracted" #gender
    }
  }
  invisible(list(ID = ID, iID = iID, HN = HN, sensor.location = sensor.location,
                 SX = SX, deviceSerialNumber = deviceSerialNumber)) #  wdayname=wdayname,wdaycode=wdaycode,wday=wday,ws3=ws3,ws2=ws2
}
