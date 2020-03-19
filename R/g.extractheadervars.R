g.extractheadervars = function (I) 
{
        header = I$header
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
                HN = "not available"
                BodyLocation = hvalues[which(hnames == "Body_Location")]
                SX = "not available"
                deviceSerialNumber = hvalues[which(hnames == "Serial_Number")]
        }
        else if (mon == "geneactive") {
                id = hvalues[which(hnames == "Subject_Code")]
                iid = hvalues[which(hnames == "Investigator_ID")]
                HN = hvalues[which(hnames == "Handedness_Code")]
                BodyLocation = as.character(as.matrix(hvalues[which(hnames == 
                                                                            "Device_Location_Code")]))
                SX = hvalues[which(hnames == "Sex")]
                deviceSerialNumber = hvalues[which(hnames == "Device_Unique_Serial_Code")]
                if (I$dformn == "csv") {
                        id = hvalues[which(hnames == "Subject Code")]
                        iid = hvalues[which(hnames == "Investigator ID")]
                        HN = hvalues[which(hnames == "Handedness Code")]
                        BodyLocation = hvalues[which(hnames == "Device Location Code")]
                        SX = hvalues[which(hnames == "Sex")]
                        deviceSerialNumber = hvalues[which(hnames == "Device Unique Serial Code")]
                }
        }
        else if (mon == "actigraph" | mon == "axivity" | mon == "movisens") {
                id = "not extracted"
                iid = "not extracted"
                HN = "not extracted"
                BodyLocation = "not extracted"
                SX = "not extracted"
                deviceSerialNumber = "not extracted"
                if (mon == "axivity") {
                        seriali = which(hnames == "uniqueSerialCode")
                        if (length(seriali) > 0) 
                                deviceSerialNumber = hvalues[seriali[1]]
                }
        }
        else if (mon == "unknown") {
                if (length(which(hnames == "recordingID")) > 0) {
                        id = hvalues[which(hnames == "recordingID")]
                }
                else {
                        id = "not extracted"
                }
                iid = "not extracted"
                HN = "not extracted"
                BL = "not extracted"
                SX = "not extracted"
                if (length(which(hnames == "device_serial_number")) > 
                    0) {
                        SN = hvalues[which(hnames == "device_serial_number")]
                }
                else {
                        SN = "not extracted"
                }
        }
        invisible(list(id = id, iid = iid, HN = HN, BodyLocation = BodyLocation, 
                       SX = SX, deviceSerialNumber = deviceSerialNumber))
}
