g.getstarttime = function(datafile, P, header, mon, dformat, desiredtz, selectdaysfile) { 
  #get input variables (relevant when read.myacc.csv is used)
  #------------------------------------------------------------
  if (mon  == 1 & dformat == 1) {
    starttime = P$timestamps2[1]
  } else if (mon  == 4 & dformat == 4) {
    starttime = P$data[1,1]
    starttime = as.POSIXlt(starttime, tz = desiredtz, origin = "1970-01-01")
    starttime = POSIXtime2iso8601(starttime, tz = desiredtz)
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
  } else if (mon == 2 & dformat == 1) {
    if (length(desiredtz) > 0) {
      if (length(selectdaysfile) == 0) { # Tested way of getting starttime on GENEACtiv data
        starttime = POSIXtime2iso8601(P$page.timestamps[1], tz = desiredtz)
      } else { # Modified way of getting starttime from Millenium cohort data
        starttime = POSIXtime2iso8601(getFirstTimestamp(datafile, P$data.out[1,1]), tz = desiredtz)
      }
      if (length(unlist(strsplit(as.character(starttime),":"))) < 2) {
        #needed for MaM study where first timestamp does not have clock time in it
        starttime = POSIXtime2iso8601(P$page.timestamps[2], tz = desiredtz)
      }
    } else {
      starttime = P$page.timestamps[1]
    }
  } else if (dformat == 2 & mon == 2) {
    starttime = as.character(P[1,1])
    starttime = as.POSIXlt(starttime)
  } else if (dformat == 2 & (mon == 3 | mon == 4 | mon == 6)) {
    if (mon == 3 | mon == 6) {
      tmph = read.csv(datafile, nrow = 8, skip = 1)
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
      startdate = unlist(strsplit(as.character(tmph[tmphi,1]), "Start Date"))[2]
      startdate = as.character(unlist(strsplit(as.character(startdate)," ")))
      starttime = as.character(unlist(strsplit(as.character(starttime)," ")))
    }
    if (mon == 4) {
      starttime = P[1,1]
      starttime = as.POSIXlt(starttime,tz = desiredtz,origin = "1970-01-01")
      startdate = unlist(strsplit(as.character(starttime), " "))[1]
    } else {
      #-----------------------------------------
      #remove possible spaces in date or time
      newstarttime = starttime #20-11-2014
      newstartdate = startdate #20-11-2014
      if (length(startdate) > 1) {
        for (rpsi in 1:length(startdate)) {
          if (length(unlist(strsplit(startdate[rpsi], ""))) > 1) {
            newstartdate = startdate[rpsi]
          }
        }
      }
      if (length(starttime) > 1) {
        for (rpsi in 1:length(starttime)) {
          if (length(unlist(strsplit(starttime[rpsi], ""))) > 1) {
            newstarttime = starttime[rpsi]
          }
        }
      }
      starttime = newstarttime
      startdate = newstartdate
    }
    #-----------------------------------------
    # flexible four date/time formats
    starttime = paste0(startdate," ",starttime)
    getOption("digits.secs")
    options(digits.secs = 3)
    if (mon == 3 | mon == 6) {
      options(warn = -1)
      topline = as.matrix(colnames(as.matrix(read.csv(datafile, nrow = 1, skip = 0))))
      topline = topline[1]  #To avoid dots
      options(warn = 0)
      # Extraction of date format.
      # all formats to consider following R date formatting symbols:
      # Y year including century, y year excluding century, b month as character, m month as a number
      # Note: year in the middle of a date is assumed to not occur.
      allformats = c("mdY", "mdy","bdY", "bdy",
                     "dmY", "dmy", "dbY", "dby",
                     "Ymd", "ymd", "Ybd", "ybd",
                     "Ydm", "ydm", "Ydb", "ydb")
      fc = data.frame(matrix(0,1,length(allformats)), stringsAsFactors = TRUE) # data.frame to keep track of formats that have been checked
      names(fc) = allformats
      # Now systematically go through all possible formats to identify which one it is
      fc$mdY = length(grep("MM[.]dd[.]yyyy|M[.]d[.]yyyy|M[.]dd[.]yyyy|MM[.]d[.]yyyy",topline))
      fc$mdy = length(grep("MM[.]dd[.]yy|M[.]d[.]yy|M[.]dd[.]yy|MM[.]d[.]yy",topline))
      fc$bdY = length(grep("MMM[.]dd[.]yyyy|MMM[.]d[.]yyyy",topline))
      fc$bdy = length(grep("MMM[.]dd[.]yy|MMM[.]d[.]yy",topline))
      if (fc$mdY == 1 & fc$mdy == 1) fc$mdy = 0 # not yy when yyyy is also detected
      if (fc$bdY == 1 & fc$bdy == 1) fc$bdy = 0 # not yy when yyyy is also detected
      if (fc$mdY == 1 & fc$bdY == 1) fc$mdY = 0 # not M(M) when MMM is also detected for yyyy
      if (fc$mdy == 1 & fc$bdy == 1) fc$mdy = 0 # not M(M) when MMM is also detected for yy
      fc$dmY = length(grep("d[.]M[.]yyyy|d[.]MM[.]yyyy",topline))
      fc$dmy = length(grep("d[.]M[.]yy|d[.]MM[.]yy",topline))
      fc$dbY = length(grep("d[.]MMM[.]yyyy",topline))
      fc$dby = length(grep("d[.]MMM[.]yy",topline))
      if (fc$dmY == 1 & fc$dmy == 1) fc$dmy = 0 # not yy when yyyy is also detected
      if (fc$dbY == 1 & fc$dby == 1) fc$dby = 0 # not yy when yyyy is also detected
      if (fc$dmY == 1 & fc$dbY == 1) fc$dmY = 0 # not M(M) when MMM is also detected for yyyy
      if (fc$dmy == 1 & fc$dby == 1) fc$dmy = 0 # not M(M) when MMM is also detected for yy
      fc$Ymd = length(grep("yyyy[.]M[.]d|yyyy[.]MM[.]d",topline))
      fc$ymd = length(grep("yy[.]M[.]d|yy[.]MM[.]d",topline))
      fc$Ybd = length(grep("yyyy[.]MMM[.]d",topline))
      fc$ybd = length(grep("yy[.]MMM[.]d",topline))
      if (fc$Ymd == 1 & fc$ymd == 1) fc$ymd = 0 # not yy when yyyy is also detected
      if (fc$Ybd == 1 & fc$ybd == 1) fc$ybd = 0 # not yy when yyyy is also detected
      if (fc$Ymd == 1 & fc$Ybd == 1) fc$Ymd = 0 # not M(M) when MMM is also detected for yyyy
      if (fc$ymd == 1 & fc$ybd == 1) fc$ymd = 0 # not M(M) when MMM is also detected for yy
      fc$Ydm = length(grep("yyyy[.]dd[.]MM|yyyy[.]d[.]M|yyyy[.]d[.]MM|yyyy[.]dd[.]M",topline))
      fc$ydm = length(grep("yy[.]dd[.]MM|yy[.]d[.]M|yy[.]d[.]MM|yy[.]dd[.]M",topline))
      fc$Ydb = length(grep("yyyy[.]dd[.]MMM|yyyy[.]d[.]MMM",topline))
      fc$ydb = length(grep("yy[.]dd[.]MMM|yy[.]d[.]MMM",topline))
      if (fc$Ydm == 1 & fc$ydm == 1) fc$ydm = 0 # not yy when yyyy is also detected
      if (fc$Ydb == 1 & fc$ydb == 1) fc$ydb = 0 # not yy when yyyy is also detected
      if (fc$Ydm == 1 & fc$Ydb == 1) fc$Ydm = 0 # not M(M) when MMM is also detected for yyyy
      if (fc$ydm == 1 & fc$ydb == 1) fc$ydm = 0 # not M(M) when MMM is also detected for yy
      # Now we can identify which format it is:
      theformat = names(fc)[which(fc == 1)[1]]
      if (length(theformat) == 0) warning("date format not recognised")
      splitformat = unlist(strsplit(theformat,""))
      # identify separater:
      if (length(grep("/",starttime)) > 0) {
        sepa = "/"
      } else {
        if (length(grep("-",starttime)) > 0) {
          sepa = "-"
        } else {
          if (length(grep(".",starttime)) > 0) {
            sepa = "."
          } else {
            warning("separater character for dates not identified")
          }
        }
      }
      expectedformat = paste0('%',splitformat[1],sepa,'%',splitformat[2],sepa,'%',splitformat[3],' %H:%M:%S')
      Sys.setlocale("LC_TIME", "C")  # set language to English because that is what we use elsewhere in GGIR
      starttime = as.POSIXlt(starttime, format = expectedformat)
    }
  } else if (dformat == 5 & mon == 0) {
    starttime = P$data$timestamp[1]
  } else if (mon == 5) {
    starttime = unisensR::readUnisensStartTime(dirname(datafile))
  } else if (dformat == 6 & mon == 3) {
    starttime = as.POSIXlt(as.character(P[1, 1]), tz = desiredtz)
  }
  return(starttime)
}
