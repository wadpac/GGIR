g.getstarttime = function(datafile,P,header,mon,dformat,desiredtz,selectdaysfile) {
  if (mon  == 1 & dformat == 1) {
    starttime = P$timestamps2[1]
    lengthheader = nrow(header)
  } else if (mon  == 4 & dformat == 4) {
    starttime = P$data[1,1]
    lengthheader = nrow(header)
    starttime = as.POSIXlt(starttime,tz=desiredtz,origin="1970-01-01")
    starttime = POSIXtime2iso8601(starttime,tz=desiredtz)
  } else if (dformat == 3) { #wav
    starttime = c()
    #It seems that Axivity does not store timestamp in a consistent position
    # therefore, we need to search for it in the data:
    starttime = as.character(header[which(rownames(header) == "ICMTzTime"),1])
    rn = rownames(header)
    vl = header$value
    if (length(starttime) == 0) {
      if (length(which(rn == "Start")) > 0) {
        starttime = as.character(header$value[which(rn == "Start")])
        #in one of the files starttime is hidden in rowname
        if (length(starttime) == 0) starttime = rownames(header)[2]
      }
      #in one of the files start variable name is hidden in the values
      if (length(which(vl == "Start")) > 0) {
        starttime = header$value[2]
      }

    }
    if (length(starttime) == 0) starttime = P$timestamp # initially used, but apparently its is corrupted sometimes, so I am now using ICMTzTime
    if (length(P$timestamp) == 0) starttime = as.character(P$hvalues[which(P$hnames == "Start")])
    lengthheader = nrow(header)
  } else if (mon == 2 & dformat == 1) {
    if (length(desiredtz) > 0) {
      # starttime = as.POSIXlt(P$page.timestamps[1],tz=desiredtz)
      # starttime = POSIXtime2iso8601(P$page.timestamps[1],tz=desiredtz)
      if (length(selectdaysfile) == 0) { # Tested way of getting starttime on GENEACtiv data
        starttime = POSIXtime2iso8601(P$page.timestamps[1],tz=desiredtz)
      } else { # Modified way of getting starttime from Millenium cohort data
        starttime = POSIXtime2iso8601 (getFirstTimestamp(datafile, P$data.out[1,1]), tz = desiredtz)
      }
      if (length(unlist(strsplit(as.character(starttime),":"))) < 2) {
        #needed for MaM study where first timestamp does not have clock time in it
        starttime = POSIXtime2iso8601(P$page.timestamps[2],tz=desiredtz)
      }
    } else {
      starttime = P$page.timestamps[1]
    }
    lengthheader = nrow(header) #length(unlist(H))
  } else if (dformat == 2 & mon == 2) {
    starttime = as.character(P[1,1])
    starttime = as.POSIXlt(starttime)
    lengthheader = 20
  } else if (dformat == 2 & (mon == 3 | mon == 4)) {
    if (mon == 3) {
      tmph = read.csv(datafile,nrow=8,skip=1)
      tmphi = 1
      while (tmphi < 10) {
        if (length(unlist(strsplit(as.character(tmph[tmphi,1]),"Start Time"))) > 1) {
          break
        }
        tmphi = tmphi + 1
      }
      starttime = unlist(strsplit(as.character(tmph[tmphi,1]),"Start Time"))[2]
      #-------------------------------
      tmphi = 1
      while (tmphi < 10) {
        if (length(unlist(strsplit(as.character(tmph[tmphi,1]),"Start Date"))) > 1) {
          break
        }
        tmphi = tmphi + 1
      }
      startdate = unlist(strsplit(as.character(tmph[tmphi,1]),"Start Date"))[2]
      startdate = as.character(unlist(strsplit(as.character(startdate)," ")))
      starttime = as.character(unlist(strsplit(as.character(starttime)," ")))
    }

    if (mon == 4) {
      starttime = P[1,1]
      starttime = as.POSIXlt(starttime,tz=desiredtz,origin="1970-01-01")
      # startdate = unlist(strsplit(as.character(starttime)," "))[1]
    } else {
      #-----------------------------------------
      #remove possible spaces in date or time
      newstarttime = starttime #20-11-2014
      newstartdate = startdate #20-11-2014
      if (length(startdate) > 1) {
        for (rpsi in 1:length(startdate)) {
          if (length(unlist(strsplit(startdate[rpsi],"")))>1) {
            newstartdate = startdate[rpsi]
          }
        }
      }
      if (length(starttime) > 1) {
        for (rpsi in 1:length(starttime)) {
          if (length(unlist(strsplit(starttime[rpsi],"")))>1) {
            newstarttime = starttime[rpsi]
          }
        }
      }
    }
    starttime = newstarttime
    startdate = newstartdate
    #-----------------------------------------
    # flexible four date/time formats
    starttime = paste(startdate," ",starttime,sep="")
    getOption("digits.secs")
    options(digits.secs = 3)
    if (mon == 3) {
      options(warn=-1)
      topline = as.matrix(colnames(as.matrix(read.csv(datafile,nrow=1,skip=0))))
      topline = topline[1]  #To avoid dots
      options(warn=0)
      B1 = length(unlist(strsplit(topline,"MM[.]dd[.]yyyy")))
      B2 = length(unlist(strsplit(topline,"M[.]d[.]yyyy")))
      B6 = length(unlist(strsplit(topline,"M[.]dd[.]yyyy")))
      B7 = length(unlist(strsplit(topline,"MM[.]d[.]yyyy")))

      B3 = length(unlist(strsplit(topline,"dd[.]MM[.]yyyy")))
      B4 = length(unlist(strsplit(topline,"d[.]M[.]yyyy")))
      B5 = length(unlist(strsplit(topline,"d[.]MM[.]yyyy")))
      B8 = length(unlist(strsplit(topline,"dd[.]M[.]yyyy")))

      B9 = length(unlist(strsplit(topline,"yyyy[.]MM[.]dd")))
      B10 = length(unlist(strsplit(topline,"yyyy[.]M[.]d")))
      B11 = length(unlist(strsplit(topline,"yyyy[.]MM[.]d")))
      B12 = length(unlist(strsplit(topline,"yyyy[.]M[.]dd")))

      B13 = length(unlist(strsplit(topline,"yyyy[.]dd[.]MM")))
      B14 = length(unlist(strsplit(topline,"yyyy[.]d[.]M")))
      B15 = length(unlist(strsplit(topline,"yyyy[.]d[.]MM")))
      B16 = length(unlist(strsplit(topline,"yyyy[.]dd[.]M")))

      if (B1 > 1 | B2 > 1 | B6 > 1 | B7 > 1) {
        starttime0 = as.POSIXlt(starttime,format='%m/%d/%Y %H:%M:%S')
        if(is.na(starttime0) == TRUE) {
          starttime0 = as.POSIXlt(starttime,format='%m-%d-%Y %H:%M:%S')
          if(is.na(starttime0) == TRUE) {
            starttime0 = as.POSIXlt(starttime,format='%m.%d.%Y %H:%M:%S')
          }
        }
      } else if (B3 > 1 | B4 > 1 | B5 > 1 | B8 > 1) {
        starttime0 = as.POSIXlt(starttime,format='%d/%m/%Y %H:%M:%S')
        if(is.na(starttime0) == TRUE) {
          starttime0 = as.POSIXlt(starttime,format='%d-%m-%Y %H:%M:%S')
          if(is.na(starttime0) == TRUE) {
            starttime0 = as.POSIXlt(starttime,format='%d.%m.%Y %H:%M:%S')
          }
        }
      } else if (B9 > 1 | B10 > 1 | B11 > 1 | B12 > 1) {
        starttime0 = as.POSIXlt(starttime,format='%Y/%m/%d %H:%M:%S')
        if(is.na(starttime0) == TRUE) {
          starttime0 = as.POSIXlt(starttime,format='%d-%m-%Y %H:%M:%S')
          if(is.na(starttime0) == TRUE) {
            starttime0 = as.POSIXlt(starttime,format='%d.%m.%Y %H:%M:%S')
          }
        }
      } else if (B13 > 1 | B14 > 1 | B15 > 1 | B16 > 1) {
        starttime0 = as.POSIXlt(starttime,format='%Y/%d/%m %H:%M:%S')
        if(is.na(starttime0) == TRUE) {
          starttime0 = as.POSIXlt(starttime,format='%d-%m-%Y %H:%M:%S')
          if(is.na(starttime0) == TRUE) {
            starttime0 = as.POSIXlt(starttime,format='%d.%m.%Y %H:%M:%S')
          }
        }
      }
      rm(starttime)
      starttime = starttime0
      lengthheader = 9
    }
  }
  return(starttime)
}
