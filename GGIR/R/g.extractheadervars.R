g.extractheadervars = function(I) {
  header= I$header
  mon = I$monn
  hnames = rownames(header)
  hvalues = as.character(as.matrix(header))
  pp = which(hvalues == "")
  hvalues[pp] = c("not stored in header")
  if (mon == "genea") {
    idd = hvalues[which(hnames == "Volunteer_Number")]    	
    id = as.character(unlist(idd))
    iidd = hvalues[which(hnames == "Investigator_Id")]			
    iid = as.character(unlist(iidd))
    HN = "not available" #not stored by genea
    BL = hvalues[which(hnames == "Body_Location")]
    SX = "not available" #not stored by genea
    SN = hvalues[which(hnames == "Serial_Number")] #serial number
  } else if (mon == "geneactive") {
    id = hvalues[which(hnames == "Subject_Code")] #; temp2 = unlist(strsplit(as.character(temp)," "))
    iid = hvalues[which(hnames == "Investigator_ID")] #investigator id
    HN = hvalues[which(hnames == "Handedness_Code")] #handedness
    BL = as.character(as.matrix(hvalues[which(hnames == "Device_Location_Code")])) #body location
    SX = hvalues[which(hnames == "Sex")] #gender
    SN = hvalues[which(hnames == "Device_Unique_Serial_Code")] #serial number
    if (I$dformn == "csv") { #if it was stored in csv-format then underscores were replaced by spaces (by company)
      id = hvalues[which(hnames == "Subject Code")]# ; temp2 = unlist(strsplit(as.character(temp)," "))
      iid = hvalues[which(hnames == "Investigator ID")] #investigator id
      HN = hvalues[which(hnames == "Handedness Code")] #handedness
      BL = hvalues[which(hnames == "Device Location Code")] #body location
      SX = hvalues[which(hnames == "Sex")] #gender
      SN = hvalues[which(hnames == "Device Unique Serial Code")] #serial number			
    }
  } else if (mon == "actigraph" | mon == "axivity") { #todo: create automatic extraction of information from actigraph fileheader
    id = "not extracted"	# volunteer ID as stored in binary file header
    iid = "not extracted" #investigator id
    HN = "not extracted" #handedness
    BL = "not extracted" #body location
    SX = "not extracted" #gender
    SN = "not extracted" #gender
  }
  invisible(list(id=id,iid=iid,HN=HN,BL=BL,SX=SX,SN=SN)) #  wdayname=wdayname,wdaycode=wdaycode,wday=wday,ws3=ws3,ws2=ws2
}